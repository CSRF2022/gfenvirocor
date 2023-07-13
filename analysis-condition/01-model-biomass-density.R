# 1. Create a total biomass distribution model (to use for prediction grid)
# 2. Create prediction grid with density and depth
# TODO: get matching depth for use in models and on prediction grids?

# knot_distance <- 5
knot_distance <- 10

species_list <- c("Petrale Sole")
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species_list)))

# species_list <- c("Canary Rockfish")
unique(ds$survey_abbrev)

# TODO: extract a new version of survey sets that includes data dropped from the grid
ds <- readRDS("data-raw/survey-sets.rds") %>%
  filter(species_common_name == tolower(species_list)) %>%
  # currently choosing the max series of years without a gap
  # MSA occurs in 2002 and 2003, and SYN QCS starts in 2003
  filter(survey_abbrev %in% c("HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI") & year > 2001) %>%
  mutate(
    log_depth = log(depth_m),
    survey_type = as.factor(ifelse(survey_abbrev == "HS MSA", "MSA", "SYN")),
    area_swept = ifelse(is.na(tow_length_m), doorspread_m * duration_min * speed_mpm, doorspread_m * tow_length_m),
    offset = log(area_swept / 10000)
  )

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

mesh <- make_mesh(d, c("X", "Y"), cutoff = knot_distance)

ggplot() +
  inlabru::gg(mesh$mesh) +
  coord_fixed() +
  geom_point(aes(X, Y), data = d, alpha = 0.2, size = 0.5) +
  geom_point(aes(X, Y, colour = catch_weight, size = catch_weight),
    data = d
  ) +
  facet_wrap(~year) +
  scale_color_viridis_c(trans = "fourth_root_power")

dens_model_name <- "-w-survey-factor"

fm <- paste0("data-generated/density-models/", spp, "-m", dens_model_name, ".rds")

if (!file.exists(fm)) {
  m <- sdmTMB(catch_weight ~ 1 +
    survey_type +
    s(log_depth, k = 3),
  offset = "offset",
  mesh = mesh,
  data = d,
  spatial = "on",
  spatiotemporal = "rw",
  share_range = FALSE,
  silent = FALSE,
  time = "year",
  # reml = TRUE,
  family = delta_gamma(),
  # control = sdmTMBcontrol(newton_loops = 1L),
  priors = sdmTMBpriors(
    matern_s = pc_matern(range_gt = knot_distance * 1.5, sigma_lt = 2),
    matern_st = pc_matern(range_gt = knot_distance * 1.5, sigma_lt = 2)
  )
  )

  sanity(m)

  dir.create(paste0("data-generated/density-models/"), showWarnings = FALSE)
  saveRDS(m, paste0("data-generated/density-models/", spp, "-m", dens_model_name, ".rds"))
} else {
  m <- readRDS(paste0("data-generated/density-models/", spp, "-m", dens_model_name, ".rds"))
}

p <- predict(m, newdata = grid, return_tmb_object = TRUE)

p2 <- p$data %>% mutate(density = plogis(est1) * exp(est2))

ggplot(p2, aes(X, Y, colour = density, fill = density)) +
  geom_tile(width = 2, height = 2, alpha = 1) +
  facet_wrap(~year) +
  scale_fill_viridis_c(trans = "sqrt") +
  scale_colour_viridis_c(trans = "sqrt")

dir.create(paste0("data-generated/density-predictions/"), showWarnings = FALSE)
saveRDS(p2, paste0("data-generated/density-predictions/", spp, "-p", dens_model_name, ".rds"))

ind <- get_index(p, bias_correct = FALSE)
ggplot(ind, aes(year, est)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  xlab("Year") +
  ylab("Biomass estimate (kg)")

## very slow! only run when not in a hurry
# (v1 <- visreg_delta(m, xvar = "log_depth", model = 1, gg = TRUE))
# (v2 <- visreg_delta(m, xvar = "log_depth", model = 2, gg = TRUE))
#
# v <- list(v1, v2)
#
# dir.create(paste0("data-generated/density-depth-curves/"), showWarnings = FALSE)
# saveRDS(v, paste0("data-generated/density-depth-curves/", spp, "-d", dens_model_name, ".rds"))
