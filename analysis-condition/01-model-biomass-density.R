# 1. Create a total biomass distribution model (to use for prediction grid)
# 2. Create prediction grid with density and depth
# TODO: get matching depth for use in models and on prediction grids?

library(tidyverse)
library(sdmTMB)
library(gfplot)
library(ggsidekick)

theme_set(theme_sleek())

delta_dens_model <- FALSE
# knot_distance <- 5
# knot_distance <- 12
knot_distance <- 15

# knot_distance <- 20
mat_threshold <- 0.5
# just_females <- FALSE
# # just_females <- TRUE
# # mat_class <- "mat"
# mat_class <- "imm"
fig_height <- 4 * 2
fig_width <- 5 * 2

# species_list <- c("Petrale Sole")
species_list <- c("Canary Rockfish")

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species_list)))

# species_list <- c("Canary Rockfish")

# TODO: extract a new version of survey sets that includes data dropped from the grid
# all_set_dat <- readRDS("data-raw/survey-sets-all-species.rds")

dset <- readRDS("data-raw/survey-sets.rds") %>%
  filter(species_common_name == tolower(species_list)) %>%
  filter(!(sample_id == 285491) | is.na(sample_id)) %>% # filter one of the 2 sample ids associated with duplicated event
  # currently choosing the max series of years without a gap
  # MSA occurs in 2002 and 2003, and SYN QCS starts in 2003
  filter(survey_abbrev %in% c("HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI"))
dsamp <- readRDS("data-raw/specimen-data.rds") %>% filter(species_common_name == tolower(species_list)) # %>% rename(trip_month = month)

check_for_duplicates <- dset[duplicated(dset$fishing_event_id), ]

dss <- split_catch_by_sex(dset, dsamp, immatures_pooled = TRUE, p_threshold = mat_threshold)

ds <- dss$data %>%
  mutate(
    log_depth = log(depth_m),
    survey_type = as.factor(ifelse(survey_abbrev == "HS MSA", "MSA", "SYN")),
    area_swept = ifelse(is.na(tow_length_m), doorspread_m * duration_min * speed_mpm, doorspread_m * tow_length_m)
  )

unique(ds$survey_abbrev)
sort(unique(ds$year))

ds <- ds %>% filter(!is.na(catch_weight))
ds <- ds %>% filter(!is.na(depth_m))
ds <- ds %>% filter(!is.na(area_swept))
ds <- ds %>% filter(!is.na(latitude))
ds <- ds %>% filter(!is.na(longitude))
ds$offset <- log(ds$area_swept / 100000)

ggplot(ds) +
  geom_histogram(aes(offset)) +
  facet_wrap(~year)

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

dp <- d %>% filter(catch_weight > 1)

hist(log(dp$catch_weight))
range(d$catch_weight)
#
# # Keep only synoptic data
# d <- filter(d, survey_type == "SYN")


d1 <- d %>% filter(group_name == "Mature females")



mesh <- make_mesh(d1, c("X", "Y"), cutoff = knot_distance)

ggplot() +
  inlabru::gg(mesh$mesh) +
  coord_fixed() +
  geom_point(aes(X, Y, colour = catch_weight, size = catch_weight), data = filter(d1, catch_weight > 0)) +
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
fm <- paste0("data-generated/density-models/", spp, "-total", dens_model_name, "-", knot_distance, "-km.rds")

if (!file.exists(fm)) {
  m <- sdmTMB(
    # list(
    catch_weight ~ 1 + survey_type + poly(log_depth, 2),
    # catch_weight ~ 1 + survey_type),
    # s(log_depth, k = 3),
    offset = "offset",
    mesh = mesh,
    data = d1,
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

  s <- sanity(m)
  if (!all(s)) {
    m <- update(m, share_range = TRUE)
    s <- sanity(m)
    if (!all(s)) {
      m <- update(m, spatial = "off")
    }
  }
  sanity(m)
  saveRDS(m, fm)
} else {
  m <- readRDS(fm)
}

m
m$sd_report
tidy(m, "ran_pars", conf.int = TRUE, model = 1)
# tidy(m, "ran_pars", conf.int = TRUE, model = 2)


p <- predict(m, newdata = grid, return_tmb_object = TRUE)


map_density <- function(dat, type, delta = delta_dens_model) {
  if (delta) {
    p1 <- dat$data %>% mutate(density = plogis(est1) * exp(est2))
  } else {
    p1 <- dat$data %>% mutate(density = exp(est))
  }

  dir.create(paste0("data-generated/density-predictions/"), showWarnings = FALSE)
  saveRDS(p1, paste0("data-generated/density-predictions/", spp, "-p-", type, dens_model_name, "-", knot_distance, "-km.rds"))

  ggplot(p1, aes(X, Y, colour = density, fill = density)) +
    geom_tile(width = 2, height = 2, alpha = 1) +
    facet_wrap(~year) +
    scale_fill_viridis_c(trans = fourth_root_power_trans()) +
    scale_colour_viridis_c(trans = fourth_root_power_trans()) +
    labs(x = "", y = "")
}

map_density(p, "total") + labs(title = paste0(species_list, ": total biomass (", dens_model_name, ")"))
ggsave(paste0("figs/density-map-", spp, "-total", dens_model_name, "-", knot_distance, "-km.png"),
  height = fig_height, width = fig_width
)


plot_index <- function(dat, type) {
  f <- paste0("data-generated/density-index/", spp, "-p-", type, dens_model_name, "-", knot_distance, "-km.rds")
  if (!file.exists(f)) {
    dir.create(paste0("data-generated/density-index/"), showWarnings = FALSE)
    ind <- get_index(dat, bias_correct = FALSE)
    saveRDS(ind, f)
  } else {
    ind <- readRDS(f)
  }
  ggplot(ind, aes(year, est)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
    xlab("Year") +
    ylab("Biomass estimate (kg)")
}

plot_index(p, "total") + ggtitle(paste0(species_list, ": total biomass (", dens_model_name, ")"))
ggsave(paste0("figs/density-index-", spp, "-total", dens_model_name, "-", knot_distance, "-km.png"),
  height = fig_height / 2, width = fig_width / 2
)


# # not working with offset
# g <- ggeffects::ggeffect(m, paste0("log_depth [",
#                                    range(d$log_depth)[1], ":", range(d$log_depth)[2], "by=0.05]"))
# plot(g)


fmf <- paste0("data-generated/density-models/", spp, "-mat-fem", dens_model_name, "-", knot_distance, "-km.rds")

if (!file.exists(fmf)) {
  d2 <- d1 %>% filter(year > 2001)
  mesh2 <- make_mesh(d2, c("X", "Y"), cutoff = knot_distance)

  mf <- update(m, group_catch_est ~ 1 + survey_type + poly(log_depth, 2), mesh = mesh2, data = d2)

  s <- sanity(mf)
  if (!all(s)) {
    mf <- update(mf, share_range = TRUE)
    s <- sanity(mf)
    if (!all(s)) {
      mf <- update(mf, spatial = "off")
    }
  }
  sanity(mf)
  saveRDS(mf, fmf)
} else {
  mf <- readRDS(fmf)
}



pf <- predict(mf, newdata = filter(grid, year > 2001), return_tmb_object = TRUE)

map_density(pf, "mat-fem") + labs(title = paste0(species_list, ": mature female biomass (", dens_model_name, ")"))

ggsave(paste0("figs/density-map-", spp, "-mat-fem", dens_model_name, "-", knot_distance, "-km.png"),
  height = fig_height, width = fig_width
)

plot_index(pf, "mat-fem") + ggtitle(paste0(species_list, ": mature female biomass (", dens_model_name, ")"))
ggsave(paste0("figs/density-index-", spp, "-mat-fem", dens_model_name, "-", knot_distance, "-km.png"),
  height = fig_height / 2, width = fig_width / 2
)


fmm <- paste0("data-generated/density-models/", spp, "-all-mat", dens_model_name, "-", knot_distance, "-km.rds")

if (!file.exists(fmm)) {
  d3 <- ds %>%
    filter(group_name %in% c("Mature males") & year > 2001) %>%
    select(fishing_event_id, group_catch_est_mm = group_catch_est)
  d3 <- left_join(d2, d3) %>% mutate(group_catch_est2 = group_catch_est + group_catch_est_mm)

  mm <- update(mf, group_catch_est2 ~ 1 + survey_type + poly(log_depth, 2), data = d3)

  s <- sanity(mm)
  if (!all(s)) {
    mm <- update(mm, share_range = TRUE)
    s <- sanity(mm)
    if (!all(s)) {
      mm <- update(mm, spatial = "off")
    }
  }
  sanity(mm)
  saveRDS(mm, fmm)
} else {
  mm <- readRDS(fmm)
}
pm <- predict(mm, newdata = filter(grid, year > 2001), return_tmb_object = TRUE)

map_density(pm, "all-mat") + labs(title = paste0(species_list, ": mature biomass (", dens_model_name, ")"))

ggsave(paste0("figs/density-map-", spp, "-all-mat", dens_model_name, "-", knot_distance, "-km.png"),
  height = fig_height, width = fig_width
)

plot_index(pm, "all-mat") + ggtitle(paste0(species_list, ": mature biomass (", dens_model_name, ")"))
ggsave(paste0("figs/density-index-", spp, "-all-mat", dens_model_name, "-", knot_distance, "-km.png"),
  height = fig_height / 2, width = fig_width / 2
)



fmi <- paste0("data-generated/density-models/", spp, "-imm", dens_model_name, "-", knot_distance, "-km.rds")

if (!file.exists(fmi)) {
  d4 <- ds %>%
    filter(group_name %in% c("Immature")) %>%
    filter(year > 2001)

  mi <- update(mf, data = d4)

  s <- sanity(mi)
  if (!all(s)) {
    mi <- update(mi, share_range = TRUE)
    s <- sanity(mi)
    if (!all(s)) {
      mi <- update(mi, spatial = "off")
    }
  }
  sanity(mi)
  saveRDS(mi, fmi)
} else {
  mi <- readRDS(fmi)
}

pi <- predict(mi, newdata = filter(grid, year > 2001), return_tmb_object = TRUE)

map_density(pi, "imm") + labs(title = paste0(species_list, ": immature biomass (", dens_model_name, ")"))

ggsave(paste0("figs/density-map-", spp, "-imm", dens_model_name, "-", knot_distance, "-km.png"),
  height = fig_height, width = fig_width
)

plot_index(pi, "all-mat") + ggtitle(paste0(species_list, ": immature biomass (", dens_model_name, ")"))
ggsave(paste0("figs/density-index-", spp, "-imm", dens_model_name, "-", knot_distance, "-km.png"),
  height = fig_height / 2, width = fig_width / 2
)




# dir.create(paste0("data-generated/density-depth-curves/"), showWarnings = FALSE)
# saveRDS(g, paste0("data-generated/density-depth-curves/", spp, "-d", dens_model_name, ".rds"))
