# 1. Create a total biomass distribution model (to use for prediction grid)
# 2. Create prediction grid with density and depth
# TODO: get matching depth for use in models and on prediction grids?

library(tidyverse)
library(sdmTMB)
library(ggsidekick)

delta_dens_model <- FALSE
# knot_distance <- 5
# knot_distance <- 12
knot_distance <- 15

# knot_distance <- 20


species_list <- c("Petrale Sole")
# species_list <- c("Canary Rockfish")

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species_list)))

# species_list <- c("Canary Rockfish")

# TODO: extract a new version of survey sets that includes data dropped from the grid
ds <- readRDS("data-raw/survey-sets.rds") %>%
  filter(species_common_name == tolower(species_list)) %>%
  # currently choosing the max series of years without a gap
  # MSA occurs in 2002 and 2003, and SYN QCS starts in 2003
  filter(survey_abbrev %in% c("HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")) %>%
  mutate(
    log_depth = log(depth_m),
    survey_type = as.factor(ifelse(survey_abbrev == "HS MSA", "MSA", "SYN")),
    area_swept = ifelse(is.na(tow_length_m), doorspread_m * duration_min * speed_mpm, doorspread_m * tow_length_m),
    offset = log(area_swept / 10000)
  )

unique(ds$survey_abbrev)

ds <- ds %>% filter(!is.na(depth_m))
ds <- ds %>% filter(!is.na(area_swept))
ds <- ds %>% filter(!is.na(latitude))
ds <- ds %>% filter(!is.na(longitude))

ds$X <- NULL
ds$Y <- NULL

d <- sdmTMB::add_utm_columns(ds, ll_names = c("longitude", "latitude"), utm_crs = 32609)

grid <- replicate_df(gfplot::synoptic_grid, time_name = "year", time_values = unique(d$year)) %>%
  mutate(
    log_depth = log(depth),
    survey_type = as.factor("SYN")
  )

# check that my data and grid are on the same XY scale
range(grid$year)
range(d$X)
range(grid$X)
range(d$Y)
range(grid$Y)

dp <- d %>% filter(catch_weight>1)

hist(log(dp$catch_weight))
range(d$catch_weight)
#
# # Keep only synoptic data
# d <- filter(d, survey_type == "SYN")

mesh <- make_mesh(d, c("X", "Y"), cutoff = knot_distance)

ggplot() +
  inlabru::gg(mesh$mesh) +
  coord_fixed() +
  geom_point(aes(X, Y, colour = catch_weight, size = catch_weight), data = filter(d, catch_weight >0)) +
  facet_wrap(~year) +
  scale_color_viridis_c(trans = "fourth_root_power")

mesh$mesh$n

dens_model_name <- "-w-survey-factor-tw"
# dens_model_name <- "-w-survey-factor-dgm-s2off"
# dens_model_name <- "-w-survey-factor-dgm-nobist"
# dens_model_name <- "-w-survey-factor-1range-dgm"

# dens_model_name <- "-w-survey-factor-1range-dgm-tvyr-s2off"
# dens_model_name <- "-w-survey-factor-1range-dgm-s2off"
#
# dens_model_name <- "-w-survey-factor-1range-dgm-tvyr"
# dens_model_name <- "-w-survey-factor-1range-sigma2off"

dir.create(paste0("data-generated/density-models/"), showWarnings = FALSE)
fm <- paste0("data-generated/density-models/", spp, "-m", dens_model_name, "-", knot_distance, "-km.rds")

if (!file.exists(fm)) {
  m <- sdmTMB(
    # list(
      catch_weight ~ 1 + survey_type + poly(log_depth, 2),
      # catch_weight ~ 1 + survey_type),
    # s(log_depth, k = 3),
  offset = "offset",
  mesh = mesh,
  data = d,
  spatial = "on",
  # spatial = list("on", "off"),
  spatiotemporal = "rw",
  # spatiotemporal = list("off", "rw"),
  share_range = FALSE,
  # share_range = list(FALSE, TRUE),
  silent = FALSE,
  time = "year",
  # reml = TRUE,
  # family = delta_gamma(),
  # family = delta_lognormal(),
  # family = delta_lognormal_mix(),
  # family = delta_gamma_mix(),
  family = tweedie(),
  # control = sdmTMBcontrol(
  #   start = list(logit_p_mix = qlogis(0.01)),
  #   map = list(logit_p_mix = factor(NA)),
  #   newton_loops = 0L
  # ),
  priors = sdmTMBpriors(
    matern_s = pc_matern(range_gt = knot_distance * 1.5, sigma_lt = 2),
    matern_st = pc_matern(range_gt = knot_distance * 1.5, sigma_lt = 2)
  )
  )

  sanity(m)
  m
  saveRDS(m, fm)
} else {
  m <- readRDS(fm)
}

m
m$sd_report
tidy(m, "ran_pars", conf.int = TRUE, model = 1)
# tidy(m, "ran_pars", conf.int = TRUE, model = 2)

p <- predict(m, newdata = grid, return_tmb_object = TRUE)


if(delta_dens_model) {
  p1 <- p$data %>% mutate(density = plogis(est1) * exp(est2))
} else {
  p1 <- p$data %>% mutate(density = exp(est))
}

ggplot(p1, aes(X, Y, colour = density, fill = density)) +
  geom_tile(width = 2, height = 2, alpha = 1) +
  facet_wrap(~year) +
  scale_fill_viridis_c(trans = fourth_root_power_trans()) +
  scale_colour_viridis_c(trans = fourth_root_power_trans())

dir.create(paste0("data-generated/density-predictions/"), showWarnings = FALSE)
saveRDS(p1, paste0("data-generated/density-predictions/", spp, "-p", dens_model_name, "-", knot_distance, "-km.rds"))

ind <- get_index(p, bias_correct = FALSE)
ggplot(ind, aes(year, est)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  xlab("Year") +
  ylab("Biomass estimate (kg)")

dir.create(paste0("data-generated/density-index/"), showWarnings = FALSE)
saveRDS(ind, paste0("data-generated/density-index/", spp, "-p", dens_model_name, "-", knot_distance, "-km.rds"))


g <- ggeffects::ggeffect(m, paste0("log_depth [",
                                   range(d$log_depth)[1], ":", range(d$log_depth)[2], "by=0.05]"))
plot(g)

# dir.create(paste0("data-generated/density-depth-curves/"), showWarnings = FALSE)
# saveRDS(g, paste0("data-generated/density-depth-curves/", spp, "-d", dens_model_name, ".rds"))
