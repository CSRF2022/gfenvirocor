# condition models
library(tidyverse)
library(sdmTMB)
library(ggsidekick)

theme_set(theme_sleek())

just_females <- TRUE
mat_class <- "mature"
mat_threshold <- 0.5
# mat_class <- "immature"
# knot_distance <- 5
knot_distance <- 10

dens_model_name <- "-w-survey-factor"

species_list <- c("Petrale Sole")
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

  m <- readRDS(paste0("data-generated/density-models/", spp, "-m", dens_model_name, ".rds"))

  pd <- predict(m, newdata = nd)

  pd2 <- pd %>%
    mutate(
      year = year_true,
      density_lag1 = plogis(est1) * exp(est2),
      log_density_lag1 = log(density_lag1)
    ) %>%
    select(year, survey_abbrev, fishing_event_id, X, Y, log_depth, year, year_density, density_lag1, log_density_lag1) %>%
    distinct()

  d2 <- left_join(d, pd2)
  dir.create(paste0("data-generated/condition-data-w-lag-density/"), showWarnings = FALSE)
  saveRDS(d2, f)
} else {
  d2 <- readRDS(f)
}

# estimate condition model

if (mat_class == "mature") {
  if (just_females) {
    d <- d2 %>% filter(group_name == "Mature females")


    hist(d$cond_fac)
    hist(log(d$cond_fac))

    # see how current total biomass density correlates with last year's total biomass density
    d$density <- d$total_weight / (d$area_swept / 10000)
    hist(d$density)
    hist(log(d$density))
    hist(log(d$density_lag1))
    d$log_density <- log(d$density)
    plot(d$log_density ~ d$log_density_lag1)

    # TODO: I don't think density needs centering, but depth might?
    hist(d$depth_m)
    # d$log_depth_c <- d$log_depth - log()
    ## name this model version (knot distance added for file name later)

    # model_name <- "-mat-fem-d0-dlag1" # should current density be included?
    model_name <- "-mat-fem-dlag1"
    # model_name <- "-mat-fem-dlag1-no-extra-time"

    mf <- paste0("data-generated/condition-models-mat-fem/", spp, "-c", model_name, "-", knot_distance, "-km.rds")

    if (!file.exists(mf)) {
      mesh <- make_mesh(d, c("X", "Y"), cutoff = knot_distance)

      ggplot() +
        inlabru::gg(mesh$mesh) +
        coord_fixed() +
        # geom_point(aes(X, Y), data = d, alpha = 0.2, size = 0.5) +
        geom_point(aes(X, Y, colour = group_catch_weight, size = total_weight),
          data = filter(d2, group_catch_weight >= 0)
        ) +
        facet_wrap(~year) +
        scale_color_viridis_c(trans = "fourth_root_power")

      m2 <- sdmTMB(cond_fac ~
        1 +
        poly(log_density_lag1, 2) +
        # poly(log_density, 2) +
        poly(log_depth, 2),
      # s(log_density_lag1, k = 3) +
      # s(log_density, k = 3) +
      # s(log_depth, k = 3),
      weights = ds$sample_multiplier,
      mesh = mesh,
      data = d,
      spatial = "on",
      spatiotemporal = "rw",
      extra_time = c(2020),
      share_range = FALSE,
      silent = FALSE,
      time = "year",
      # reml = TRUE,
      family = lognormal(link = "log"),
      # family = student(df = 5),
      # control = sdmTMBcontrol(newton_loops = 1L),
      priors = sdmTMBpriors(
        matern_s = pc_matern(range_gt = knot_distance * 1.5, sigma_lt = 2),
        matern_st = pc_matern(range_gt = knot_distance * 1.5, sigma_lt = 2)
      )
      )

      sanity(m2)
      m2

      dir.create(paste0("data-generated/condition-models-mat-fem/"), showWarnings = FALSE)
      saveRDS(m2, mf)
    } else {
      m2 <- readRDS(mf)
    }

    # load density predictions for full survey grid
    grid2 <- readRDS(paste0("data-generated/density-predictions/", spp, "-p", dens_model_name, ".rds")) %>%
      select(year, X, Y, survey, depth, log_depth, density) %>%
      mutate(
        year_density = year,
        year = year + 1,
        density_lag1 = density,
        log_density_lag1 = log(density)
      ) %>%
      select(-density) %>%
      filter(
        !(year == 2020) &
          year <= 2022 & year >= 2003
      )

    # get current year density to scale condition index with
    grid3 <- readRDS(paste0("data-generated/density-predictions/", spp, "-p", dens_model_name, ".rds")) %>%
      select(year, X, Y, survey, depth, log_depth, density) %>%
      group_by(year) %>%
      mutate(sum_density = sum(density), prop_density = density / sum_density)

    grid <- left_join(grid2, grid3)

    sort(unique(m2$data$year))
    sort(unique(grid$year))

    pc <- predict(m2, newdata = grid, return_tmb_object = TRUE)

    p3 <- pc$data %>%
      # filter(!(year == 2020)) %>%
      mutate(cond = exp(est))

    ggplot(p3, aes(X, Y, colour = log(cond), fill = log(cond))) +
      geom_tile(width = 2, height = 2, alpha = 1) +
      facet_wrap(~year) +
      scale_fill_gradient2() +
      scale_colour_gradient2()

    dir.create(paste0("data-generated/cond-predictions/"), showWarnings = FALSE)
    saveRDS(p3, paste0("data-generated/cond-predictions/", spp, "-pc", model_name, "-", knot_distance, "-km.rds"))

    ind2 <- get_index(pc, area = grid$prop_density, bias_correct = FALSE)

    ggplot(ind2, aes(year, est)) +
      geom_line() +
      geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
      xlab("Year") +
      ylab("Predicted average condition factor") +
      ggtitle(paste("Mature female", species_list))

    dir.create(paste0("data-generated/cond-index/"), showWarnings = FALSE)
    saveRDS(ind2, paste0("data-generated/cond-index/", spp, "-pc", model_name, "-", knot_distance, "-km.rds"))


    # (vdp <- visreg(m, xvar = "log_depth", gg = TRUE))
    # (vdp <- visreg_delta(m, xvar = "log_density_lag1", gg = TRUE))
    #
    # v <- list(vdp, vdp)
    #
    # dir.create(paste0("data-generated/cond-effects/"), showWarnings = FALSE)
    # saveRDS(v, paste0("data-generated/cond-effects/", spp, "-d", model_name, ".rds"))
    #
  }
}
