# condition models
library(tidyverse)
library(sdmTMB)
library(ggsidekick)
devtools::load_all(".")

theme_set(theme_sleek())

just_females <- FALSE
# just_females <- TRUE
# mat_class <- "mat"
mat_class <- "imm"
mat_threshold <- 0.5
# knot_distance <- 5
# knot_distance <- 10
knot_distance <- 15
# knot_distance <- 20
add_covariates <- FALSE
fig_height <- 4 * 2
fig_width <- 5 * 2

dens_model_name2 <- "-w-survey-factor-tw-15-km"
delta_dens_model <- FALSE

species_list <- c("Petrale Sole")
# species_list <- c("Canary Rockfish")
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species_list)))


# load condition data and attach lagged density estimates
f <- paste0("data-generated/condition-data-w-lag-density/", spp, "-mat-", mat_threshold, "-condition-density.rds")

if (!file.exists(f)) {
  ds <- readRDS(paste0("data-generated/condition-data/", spp, "-mat-", mat_threshold, "-condition.rds")) %>% ungroup()

  # ds$year_smooth <- ds$year
  ds <- ds %>% filter(!is.na(depth_m))
  ds <- ds %>% filter(!is.na(latitude))
  ds <- ds %>% filter(!is.na(longitude))

  ds$X <- NULL
  ds$Y <- NULL
  d <- sdmTMB::add_utm_columns(ds, ll_names = c("longitude", "latitude"), utm_crs = 32609) %>%
    filter(year > 2002) # can't predict prior to first year of density model


  nd <- d %>%
    select(year, survey_abbrev, fishing_event_id, X, Y, log_depth) %>%
    mutate(survey_type = "SYN", year_true = year, year_density = year - 1, year = year_density) %>%
    filter(year > 2001) # can't predict prior to first year of density model

  m <- readRDS(paste0("data-generated/density-models/", spp, "-m", dens_model_name2, ".rds"))

  pd <- predict(m, newdata = nd)

  if (delta_dens_model) {
    pd2 <- pd %>% mutate(density_lag1 = plogis(est1) * exp(est2))
  } else {
    pd2 <- pd %>% mutate(density_lag1 = exp(est))
  }

  pd2 <- pd2 %>%
    mutate(
      year = year_true,
      log_density_lag1 = log(density_lag1)
    ) %>%
    select(
      year, survey_abbrev, fishing_event_id, X, Y, log_depth,
      year_density, density_lag1, log_density_lag1
    ) %>%
    distinct()

  d2 <- left_join(d, pd2)
  dir.create(paste0("data-generated/condition-data-w-lag-density/"), showWarnings = FALSE)
  saveRDS(d2, f)
} else {
  d2 <- readRDS(f)
}


# # load density predictions for full survey grid if going to be used as covariates condition
# gridA <- readRDS(paste0("data-generated/density-predictions/", spp, "-p", dens_model_name2, ".rds")) %>%
#   select(year, X, Y, survey, depth, log_depth, density) %>%
#   mutate(
#     year_density = year,
#     year = year + 1,
#     density_lag1 = density,
#     log_density_lag1 = log(density)
#   ) %>%
#   select(-density) %>%
#   group_by(year) %>% mutate(mean_density_lag1 = mean(density_lag1),
#                             log_mean_density_lag1 = log(mean_density_lag1)) %>%
#   ungroup() %>% mutate(dens_dev = log_density_lag1 - log_mean_density_lag1)
#
# mean_den <- select(gridA, year, mean_density_lag1, log_mean_density_lag1) %>% distinct()


# # get current year density to scale condition index with
# gridB <- readRDS(paste0("data-generated/density-predictions/", spp, "-p", dens_model_name2, ".rds")) %>%
#   select(year, X, Y, survey, depth, log_depth, density) %>%
#   group_by(year) %>%
#   mutate(sum_density = sum(density), prop_density = density / sum_density, log_density = log(density))
#
# grid <- left_join(gridA, gridB) %>%
#   filter(
#     # !(year == 2020) &
#     year <= 2022 & year >= 2003
#   )

# estimate condition model

if (mat_class == "mat") {
  if (just_females) {
    d <- d2 %>%
      filter(group_name == "Mature females") %>%
      left_join(mean_den)
    group_tag <- "mat-fem"
    group_label <- "mature females"

    # get current year density to scale condition index with
    gridA <- readRDS(paste0("data-generated/density-predictions/", spp, "-p-mat-fem", dens_model_name2, ".rds")) %>%
      select(year, X, Y, survey, depth, log_depth, density) %>%
      group_by(year) %>%
      mutate(sum_density = sum(density), prop_density = density / sum_density, log_density = log(density))
  } else {
    d <- d2 %>%
      filter(group_name %in% c("Mature females", "Mature males")) %>%
      left_join(mean_den)
    group_tag <- "mat"
    group_label <- "mature females and males"

    # get current year density to scale condition index with
    gridA <- readRDS(paste0("data-generated/density-predictions/", spp, "-p-all-mat", dens_model_name2, ".rds")) %>%
      select(year, X, Y, survey, depth, log_depth, density) %>%
      group_by(year) %>%
      mutate(sum_density = sum(density), prop_density = density / sum_density, log_density = log(density))
  }
} else {
  if (mat_class == "imm") {
    d <- d2 %>% filter(group_name %in% c("Immature"))
    group_tag <- "imm"
    group_label <- "immatures"

    # get current year density to scale condition index with
    gridA <- readRDS(paste0("data-generated/density-predictions/", spp, "-p-imm", dens_model_name2, ".rds")) %>%
      select(year, X, Y, survey, depth, log_depth, density) %>%
      group_by(year) %>%
      mutate(sum_density = sum(density), prop_density = density / sum_density, log_density = log(density))
  } else {

    # model everything together
    d <- d2 %>% left_join(mean_den)

    # get current year density to scale condition index with
    gridA <- readRDS(paste0("data-generated/density-predictions/", spp, "-p-total", dens_model_name2, ".rds")) %>%
      select(year, X, Y, survey, depth, log_depth, density) %>%
      group_by(year) %>%
      mutate(sum_density = sum(density), prop_density = density / sum_density, log_density = log(density))
  }
}

if (add_covariates) {
  # load density predictions for full survey grid if going to be used as covariates condition
  gridB <- readRDS(paste0("data-generated/density-predictions/", spp, "-p-total", dens_model_name2, ".rds")) %>%
    select(year, X, Y, survey, depth, log_depth, density) %>%
    mutate(
      year_density = year,
      year = year + 1,
      density_lag1 = density,
      log_density_lag1 = log(density)
    ) %>%
    select(-density) %>%
    group_by(year) %>%
    mutate(
      mean_density_lag1 = mean(density_lag1),
      log_mean_density_lag1 = log(mean_density_lag1)
    ) %>%
    ungroup() %>%
    mutate(dens_dev = log_density_lag1 - log_mean_density_lag1)

  mean_den <- select(gridA, year, mean_density_lag1, log_mean_density_lag1) %>% distinct()
  d <- d %>%
    left_join(mean_den) %>%
    mutate(dens_dev = log_density_lag1 - log_mean_density_lag1)

  # see how current total biomass density correlates with last year's total biomass density
  d$density <- d$total_weight / (d$area_swept / 10000)
  hist(d$density)
  hist(log(d$density))
  hist(log(d$density_lag1))
  d$log_density <- log(d$density)
  d$log_mean_density_lag1 <- log(d$mean_density_lag1)
  plot(d$log_density ~ d$log_density_lag1)

  # TODO: I don't think density needs centering, but depth might?
  hist(d$depth_m)
  hist(log(d$depth_m))
  hist((d$mean_density_lag1))
  hist(log(d$mean_density_lag1))

  hist((d$log_density))

  # d$log_depth_c <- d$log_depth - log()
  ## name this model version (knot distance added for file name later)
}

hist(d$cond_fac)
hist(log(d$cond_fac))

mesh <- make_mesh(d, c("X", "Y"), cutoff = knot_distance)

ggplot() +
  inlabru::gg(mesh$mesh) +
  coord_fixed() +
  geom_point(aes(X, Y, size = total_weight), data = d2) +
  geom_point(aes(X, Y, colour = group_catch_weight), data = d) +
  facet_wrap(~year) +
  scale_color_viridis_c(trans = "fourth_root_power")

ggplot() +
  inlabru::gg(mesh$mesh) +
  coord_fixed() +
  geom_point(aes(X, Y, size = group_catch_weight), data = d2) +
  geom_point(aes(X, Y, colour = log(cond_fac)), size = 0.5, data = d) +
  facet_wrap(~year) +
  scale_color_gradient2()

# start with just an intercept model
model_name <- ""

mf <- paste0("data-generated/condition-models-", group_tag, "/", spp, "-c-", group_tag, model_name, "-", knot_distance, "-km.rds")

if (!file.exists(mf)) {
  m1 <- sdmTMB(cond_fac ~ 1,
    weights = d$sample_multiplier,
    mesh = mesh,
    data = d,
    spatial = "on",
    spatiotemporal = "rw",
    # spatiotemporal = "off",
    # spatial_varying = ~ dens_dev,
    # spatial_varying = ~ log_mean_density_lag1,
    extra_time = c(2020),
    share_range = FALSE,
    silent = FALSE,
    time = "year",
    # reml = TRUE,
    family = lognormal(link = "log"),
    # family = student(df = 5),
    # control = sdmTMBcontrol(newton_loops = 1L),
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = knot_distance, sigma_lt = 2),
      matern_st = pc_matern(range_gt = knot_distance, sigma_lt = 2)
    )
  )

  s <- sanity(m1)
  if (!all(s)) {
    m1 <- update(m1, share_range = TRUE)
    s <- sanity(m1)
    if (!all(s)) {
      m1 <- update(m1, spatial = "off")
    }
  }
  sanity(m1)
  dir.create(paste0("data-generated/condition-models-", group_tag, "/"), showWarnings = FALSE)
  saveRDS(m1, mf)
} else {
  # mf <- "data-generated/condition-models-mat-fem/petrale-sole-c-mat-fem-15-km.rds"
  m1 <- readRDS(mf)
}

m1
m1$sd_report
tidy(m1, "ran_pars", conf.int = TRUE)

m <- m1

## don't do this for now, but can be used to explore utility of covariates
if (add_covariates) {
  model_name <- "-pmd-pdev"
  model_name <- "-dlag1"

  mf <- paste0(
    "data-generated/condition-models-", group_tag, "/",
    spp, "-c-", group_tag, model_name, "-", knot_distance, "-km.rds"
  )

  if (!file.exists(mf)) {
    m2 <- update(m1, cond_fac ~ 1 +
      # # log_density +
      # poly(log_density, 2) +
      # log_density_lag1 +
      poly(log_density_lag1, 2) +
      # log_mean_density_lag1 +
      # poly(log_mean_density_lag1, 2) +
      # dens_dev +
      # # poly(dens_dev, 2)  +
      poly(log_depth, 2))
    saveRDS(m2, mf)
  } else {
    m2 <- readRDS(mf)
  }

  sanity(m2)
  m2
  m2$sd_report
  tidy(m2, "ran_pars", conf.int = TRUE)
  m <- m2
}


if (add_covariates) {
  grid <- left_join(gridA, gridB) %>% filter(
    # !(year == 2020) &
    year <= 2022 & year >= 2003
  )
} else {
  grid <- gridA %>% filter(
    # !(year == 2020) &
    year <= 2022 & year >= 2003
  )
}

sort(unique(m$data$year))
sort(unique(grid$year))

pc <- predict(m, newdata = grid, return_tmb_object = TRUE)

p2 <- pc$data %>%
  # filter(!(year == 2020)) %>%
  mutate(cond = exp(est))


# filter to plot only cells representing 99% of mean predicted biomass
# cells must be defined by "X", "Y", time by "year", and biomass/abundance stored as "density"
p2 <- trim_predictions_by_year(p2, 0.01)


ggplot(p2, aes(X, Y, colour = log(cond), fill = log(cond))) +
  geom_tile(width = 2, height = 2, alpha = 1) +
  facet_wrap(~year) +
  scale_fill_gradient2() +
  scale_colour_gradient2() +
  labs(title = paste0(species_list, ": ", group_label, " ", model_name), x = "", y = "")

ggsave(paste0("figs/condition-map-", spp, "-", group_tag, model_name, "-", knot_distance, "-km.png"),
  height = fig_height, width = fig_width
)


ind2 <- get_index(pc, area = grid$prop_density, bias_correct = FALSE)

ggplot(ind2, aes(year, est)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  xlab("Year") +
  ylab("Predicted average condition factor") +
  labs(title = paste0(species_list, ": ", group_label, " ", model_name))

ggsave(paste0("figs/condition-index-", spp, "-", group_tag, model_name, "-", knot_distance, "-km.png"),
  height = fig_height / 2, width = fig_width / 2
)


# dir.create(paste0("data-generated/cond-predictions/"), showWarnings = FALSE)
# dir.create(paste0("data-generated/cond-index/"), showWarnings = FALSE)
# saveRDS(p2, paste0("data-generated/cond-predictions/", spp, "-pc", "-", group_tag, model_name, "-", knot_distance, "-km.rds"))
# saveRDS(ind2, paste0("data-generated/cond-index/", spp, "-pc", "-", group_tag, model_name, "-", knot_distance, "-km.rds"))


## probably don't need these for every model but keeping here for interest
#
# ggplot(p2, aes(X, Y, colour = log_density, fill = log_density)) +
#   geom_tile(width = 2, height = 2, alpha = 1) +
#   # facet_wrap(~year) +
#   scale_fill_viridis_c() +
#   scale_colour_viridis_c()
#
## ggsave(paste0("figs/density-map-", spp, "-", group_tag, model_name, "-", knot_distance, "-km.png"), height = fig_height, width = fig_width)
# ggplot(p2, aes(X, Y,
#                colour = omega_s,
#                fill = omega_s)) +
#   geom_tile(width = 2, height = 2, alpha = 1) +
#   # facet_wrap(~year) +
#   scale_fill_gradient2() +
#   scale_colour_gradient2()
#
## ggsave(paste0("figs/condition-omega-map-", spp, "-", group_tag, model_name, "-", knot_distance, "-km.png"), height = fig_height, width = fig_width)
#
#
# ggplot(p2, aes(X, Y, colour = epsilon_st, fill = epsilon_st)) +
#   geom_tile(width = 2, height = 2, alpha = 1) +
#   facet_wrap(~year) +
#   scale_fill_gradient2() +
#   scale_colour_gradient2()
#
## ggsave(paste0("figs/condition-epsilon-map-", spp, "-", group_tag, model_name, "-", knot_distance, "-km.png"), height = fig_height, width = fig_width)



if (add_covariates) {
  ggplot(p3, aes(X, Y, colour = (est_non_rf), fill = (est_non_rf))) +
    geom_tile(width = 2, height = 2, alpha = 1) +
    facet_wrap(~year) +
    scale_fill_gradient2() +
    scale_colour_gradient2()

  ggsave(paste0("figs/condition-dd-map-", spp, model_name, "-", knot_distance, "-km.png"))

  g <- g2 <- g3 <- g4 <- pd <- NULL

  g <- ggeffects::ggeffect(m, paste0(
    "log_depth [",
    range(d$log_depth)[1], ":", range(d$log_depth)[2], "by=0.05]"
  ))
  plot(g)

  g2 <- ggeffects::ggeffect(m, paste0(
    "log_mean_density_lag1 [",
    range(d$log_mean_density_lag1)[1], ":", range(d$log_mean_density_lag1)[2], "by=0.1]"
  ))
  plot(g2)

  g3 <- ggeffects::ggeffect(m, paste0(
    "dens_dev [",
    range(d$dens_dev)[1], ":", range(d$dens_dev)[2], "by=0.1]"
  ))
  plot(g3)

  g4 <- ggeffects::ggeffect(m, paste0(
    "log_density_lag1 [",
    range(d$log_density_lag1)[1], ":", range(d$log_density_lag1)[2], "by=0.1]"
  ))
  plot(g4)


  ## if time-varying depth included
  nd <- expand.grid(
    log_depth = seq(min(d$log_depth),
      max(d$log_depth),
      length.out = 100
    ),
    year = unique(d$year) # all years
  )

  nd$log_density <- 0
  nd$log_density_lag1 <- 0

  pd <- predict(m2, newdata = nd, se_fit = TRUE, re_form = NA)
  pd <- pd %>%
    group_by(year) %>%
    mutate(max_est = max(est, na.rm = T), xintercept = log_depth[est == max_est])


  ggplot(pd, aes(exp(log_depth), exp(est),
    ymin = exp(est - 1.96 * est_se),
    ymax = exp(est + 1.96 * est_se),
    group = as.factor(year)
  )) +
    geom_vline(aes(
      xintercept = exp(xintercept),
      group = year, colour = year
    ), alpha = 0.4) +
    geom_line(aes(colour = year), lwd = 0.5) +
    geom_ribbon(aes(fill = year), alpha = 0.1) +
    scale_colour_viridis_c() +
    scale_fill_viridis_c() +
    # scale_y_continuous(trans = "sqrt") +
    coord_cartesian(
      expand = F,
      xlim = c(20, 300),
      ylim = c(NA, exp(max(pd$max_est)) + 0.1)
    ) +
    # coord_cartesian(expand = F, ylim = c(NA, 1)) +
    # facet_wrap(~year_bin) +
    geom_rug(data = filter(m2$data, catch_weight > 0), aes(depth_m), inherit.aes = FALSE, alpha = 0.02) +
    labs(x = "Depth (m)", y = "Condition factor") +
    # labs(x = "Depth (m)", y = "Biomass density (kg/km2)") +
    # ylab(paste(group, "biomass density")) +
    gfplot::theme_pbs()


  # gg <- list(g, g2, g3, g4, pd)
  # dir.create(paste0("data-generated/cond-effects/"), showWarnings = FALSE)
  # saveRDS(gg, paste0("data-generated/cond-effects/", spp, "-", group_tag, model_name, "-", knot_distance, "-km.rds"))


  ## SVC plots
  #
  # coefs <- tidy(m2, conf.int = TRUE)
  # log_density_coef <- coefs$estimate[coefs$term == "log_density"]
  #
  # ggplot(p3, aes(X, Y,
  #                colour = log_density_coef + zeta_s_log_density,
  #                fill = log_density_coef + zeta_s_log_density)) +
  #   geom_tile(width = 2, height = 2, alpha = 1) +
  #   # facet_wrap(~year) +
  #   scale_fill_gradient2() +
  #   scale_colour_gradient2()
  #
  #
  # dens_dev_coef <- coefs$estimate[coefs$term == "dens_dev"]
  #
  # ggplot(p3, aes(X, Y,
  #                colour = dens_dev_coef + zeta_s_dens_dev,
  #                fill = dens_dev_coef + zeta_s_dens_dev)) +
  #   geom_tile(width = 2, height = 2, alpha = 1) +
  #   # facet_wrap(~year) +
  #   scale_fill_gradient2() +
  #   scale_colour_gradient2()
  #
  # log_mean_density_lag1_coef <- coefs$estimate[coefs$term == "log_mean_density_lag1"]
  #
  # ggplot(p3, aes(X, Y,
  #                colour = log_mean_density_lag1_coef + zeta_s_log_mean_density_lag1,
  #                fill = log_mean_density_lag1_coef + zeta_s_log_mean_density_lag1)) +
  #   geom_tile(width = 2, height = 2, alpha = 1) +
  #   # facet_wrap(~year) +
  #   scale_fill_gradient2() +
  #   scale_colour_gradient2()
  #
  # ggsave(paste0("figs/condition-zeta-map-", spp, "-", group_tag, model_name, "-", knot_distance, "-km.png"), height = fig_height, width = fig_width)
}
