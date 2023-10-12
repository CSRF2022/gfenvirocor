# condition models
library(tidyverse)
library(sdmTMB)
library(ggsidekick)
devtools::load_all(".")


# species_list <- c(
#   "Petrale Sole",
#   "Canary Rockfish",
#   "Arrowtooth Flounder",
#   "North Pacific Spiny Dogfish",
#   "Pacific Cod"
# )

species_list <- list(
  "Arrowtooth Flounder", #
  "Petrale Sole", #
  "English Sole", #
  "Dover Sole", #
  "Rex Sole", #
  "Flathead Sole", #
  "Southern Rock Sole", # ,
  "Curlfin Sole" #
  # # "Sand Sole",#
  # # "Slender Sole",#
  # # "Pacific Sanddab",#
  # # "Pacific Halibut",#
  # # "Butter Sole",#
  # # "Starry Flounder"#
  # # # #"C-O Sole", # way too few!
  # # # "Deepsea Sole" # no maturity
)


index_list <- expand.grid(species = species_list, maturity = c("mat", "imm"), males = c(TRUE, FALSE)) %>%
  mutate(
    females = ifelse(males == FALSE & maturity == "mat", TRUE, FALSE),
    males = ifelse(maturity == "imm", FALSE, males)
  ) %>%
  distinct()


# Function for generating condition indices ----

calc_condition_indices <- function(species, maturity, males, females) {
  theme_set(ggsidekick:::theme_sleek())

  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

  mat_class <- maturity
  just_males <- males
  just_females <- females

  # browser()

  print(paste(species, maturity, "(males:", just_males, ")"))

  if (species == "North Pacific Spiny Dogfish") {

    # just_males <- TRUE
    # just_females <- FALSE
    # mat_class <- c("mat", "imm")
    #
    # for (i in seq_len(nrow(index_list))) {
    #   spp <- gsub(" ", "-", gsub("\\/", "-", tolower(index_list$species[i])))
    #
    #   mat_class <- index_list$maturity[i]
    #   just_males <- index_list$males[i]
    #   just_females <- index_list$females[i]
    #   if(index_list$species[i]=="North Pacific Spiny Dogfish") {
    dens_model_name2 <- "-lnm-doy-1-22-xt-offset-15-km"
    # dens_model_name2 <- "-dgm-doy-1-22-xt-offset-15-km"
  } else {
    # dens_model_name2 <- "-w-survey-factor-tw-15-km"
    # dens_model_name2 <- "-dg-doy-1-22-10min-xt-offset-15-km"
    dens_model_name2 <- "-dg-st2000-mssm-03-500m-xt-offset-15-km"
  }

  # mat_class <- "mat"
  # mat_class <- "imm"
  # just_males <- TRUE
  # just_females <- TRUE
  # just_males <- just_females <- FALSE
  # add_density <- FALSE
  add_density <- TRUE
  mat_threshold <- 0.5
  # knot_distance <- 5
  # knot_distance <- 10
  knot_distance <- 15
  # knot_distance <- 20
  fig_height <- 4 * 2
  fig_width <- 5 * 2
  delta_dens_model <- TRUE

  # Load condition data and attach lagged density estimates ----
  f <- paste0("data-generated/condition-data-w-lag-density/", spp, "-mat-", mat_threshold, "-condition-dens-doy.rds")

  if (!file.exists(f)) {
    ds <- readRDS(paste0("data-generated/condition-data/", spp, "-mat-", mat_threshold, "-condition.rds")) %>%
      ungroup() %>%
      mutate(
        log_depth_c = log_depth - 5,
        DOY = as.numeric(strftime(time_deployed, format = "%j")),
        days_to_solstice = DOY - 172
      )

    # ds$year_smooth <- ds$year
    ds <- ds %>% filter(!is.na(depth_m))
    ds <- ds %>% filter(!is.na(days_to_solstice))
    ds <- ds %>% filter(!is.na(latitude))
    ds <- ds %>% filter(!is.na(longitude))

    ds$X <- NULL
    ds$Y <- NULL
    ds2 <- sdmTMB::add_utm_columns(ds, ll_names = c("longitude", "latitude"), utm_crs = 32609) %>%
      filter(year > 2001) # can't predict prior to first year of density model

    nd0 <- ds2 %>%
      select(year, survey_abbrev, fishing_event_id, X, Y, log_depth) %>%
      mutate(
        survey_group = "TRAWL",
        survey_type = "SYN",
        days_to_solstice = 0,
        log_depth_c = log_depth - 5
      ) # can't predict prior to first year of density model


    nd1 <- ds2 %>%
      select(year, survey_abbrev, fishing_event_id, X, Y, log_depth) %>%
      mutate(
        survey_group = "TRAWL",
        survey_type = "SYN",
        days_to_solstice = 0,
        log_depth_c = log_depth - 5,
        year_true = year,
        year_density = year - 1,
        year = year_density
      ) %>%
      filter(year > 2000) # can't predict prior to first year of density model

    md <- readRDS(paste0("data-generated/density-models/", spp, "-total", dens_model_name2, ".rds"))

    pd0 <- predict(md, newdata = nd0)
    pd1 <- predict(md, newdata = nd1)

    if (delta_dens_model) {
      pd0 <- pd0 %>% mutate(density = plogis(est1) * exp(est2))
      pd1 <- pd1 %>% mutate(density_lag1 = plogis(est1) * exp(est2))
    } else {
      pd0 <- pd0 %>% mutate(density = exp(est))
      pd1 <- pd1 %>% mutate(density_lag1 = exp(est))
    }

    pd0 <- pd0 %>%
      mutate(
        log_density = log(density),
        model = dens_model_name2
      ) %>%
      select(
        year, survey_abbrev, fishing_event_id, X, Y, log_depth,
        density, log_density, model
      ) %>%
      distinct()

    pd1 <- pd1 %>%
      mutate(
        year = year_true,
        log_density_lag1 = log(density_lag1),
        model = dens_model_name2
      ) %>%
      select(
        year, survey_abbrev, fishing_event_id, X, Y, log_depth,
        year_density, density_lag1, log_density_lag1, model
      ) %>%
      distinct()

    d2 <- left_join(ds2, pd0) %>% left_join(pd1)
    dir.create(paste0("data-generated/condition-data-w-lag-density/"), showWarnings = FALSE)
    saveRDS(d2, f)
  } else {
    d2 <- readRDS(f)
  }



  # Select relevant data and grid ----

  # browser()

  if (mat_class == "mat") {
    if (just_males) {
      pf <- paste0(
        "data-generated/density-predictions/p-", spp,
        "-mat-m", dens_model_name2, ".rds"
      )
      if (file.exists(pf)) {
        d <- d2 %>% filter(group_name == "Mature males")
        group_tag <- "mat-m"
        group_label <- "mature males"

        # get current year density to scale condition index with
        gridA <- readRDS(pf) %>%
          select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
          group_by(year) %>%
          mutate(
            sum_density = sum(density),
            prop_density = density / sum_density,
            log_density = log(density)
          ) %>%
          ungroup() %>%
          group_by(year, survey) %>%
          mutate(
            survey_density = sum(density),
            prop_density_by_survey = density / survey_density
          )
      } else {
        print(paste("No density predictions for mature male ", species, "densities."))
        return(NA)
      }
    } else {
      if (just_females) {
        pf <- paste0(
          "data-generated/density-predictions/p-", spp,
          "-mat-fem", dens_model_name2, ".rds"
        )
        if (file.exists(pf)) {
          d <- d2 %>% filter(group_name == "Mature females")
          group_tag <- "mat-fem"
          group_label <- "mature females"

          # get current year density to scale condition index with
          gridA <- readRDS(pf) %>%
            select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
            group_by(year) %>%
            mutate(
              sum_density = sum(density),
              prop_density = density / sum_density,
              log_density = log(density)
            ) %>%
            ungroup() %>%
            group_by(year, survey) %>%
            mutate(
              survey_density = sum(density),
              prop_density_by_survey = density / survey_density
            )
        } else {
          print(paste("No density predictions for mature female ", species, "densities."))
          return(NA)
        }
      } else {
        pf <- paste0(
          "data-generated/density-predictions/p-", spp,
          "-all-mat", dens_model_name2, ".rds"
        )
        if (file.exists(pf)) {
          d <- d2 %>%
            filter(group_name %in% c("Mature females", "Mature males")) %>%
            rename(group_catch_weight_split = group_catch_weight)
          d3 <- d %>%
            group_by(fishing_event_id, group_name) %>%
            select(fishing_event_id, group_name, group_catch_weight_split) %>%
            distinct() %>%
            ungroup() %>%
            group_by(fishing_event_id) %>%
            summarise(group_catch_weight = sum(group_catch_weight_split))
          d <- left_join(d, d3)
          group_tag <- "mat"
          group_label <- "mature (females and males)"

          # get current year density to scale condition index with
          gridA <- readRDS(pf) %>%
            select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
            group_by(year) %>%
            mutate(sum_density = sum(density), prop_density = density / sum_density, log_density = log(density)) %>%
            ungroup() %>%
            group_by(year, survey) %>%
            mutate(survey_density = sum(density), prop_density_by_survey = density / survey_density)
        } else {
          print(paste("No density predictions for all mature ", species, "densities."))
          return(NA)
        }
      }
    }
  } else {
    if (mat_class == "imm") {
      pf <- paste0(
        "data-generated/density-predictions/p-", spp,
        "-imm", dens_model_name2, ".rds"
      )
      if (file.exists(pf)) {
        d <- d2 %>% filter(group_name %in% c("Immature"))
        group_tag <- "imm"
        group_label <- "immatures"

        # get current year density to scale condition index with
        gridA <- readRDS(pf) %>%
          select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
          group_by(year) %>%
          mutate(
            sum_density = sum(density),
            prop_density = density / sum_density,
            log_density = log(density)
          ) %>%
          ungroup() %>%
          group_by(year, survey) %>%
          mutate(
            survey_density = sum(density),
            prop_density_by_survey = density / survey_density
          )

        # browser()
      } else {
        print(paste("No density predictions for immature ", species, "densities."))
        return(NA)
      }
    } else {
      pf <- paste0(
        "data-generated/density-predictions/p-", spp,
        "-total", dens_model_name2, ".rds"
      )
      if (file.exists(pf)) {
        # model everything together
        d <- d2

        # get current year density to scale condition index with
        gridA <- readRDS(pf) %>%
          select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
          group_by(year)
        mutate(sum_density = sum(density), prop_density = density / sum_density, log_density = log(density)) %>%
          ungroup() %>%
          group_by(year, survey) %>%
          mutate(survey_density = sum(density), prop_density_by_survey = density / survey_density)
      } else {
        print(paste("No density predictions for total ", species, "densities."))
        return(NA)
      }
    }
  }

  #

  if (add_density) {
    # load density predictions for full survey grid if going to be used as covariates condition
    gridB <- readRDS(paste0(
      "data-generated/density-predictions/p-", spp,
      "-total", dens_model_name2, ".rds"
    )) %>%
      select(year, X, Y, survey, depth, days_to_solstice, log_depth, density) %>%
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

    # mean_den <- select(gridA, year, mean_density_lag1, log_mean_density_lag1) %>% distinct()
    # d <- d %>%
    #   left_join(mean_den) %>%
    #   mutate(dens_dev = log_density_lag1 - log_mean_density_lag1)
    #
    # # see how current total biomass density correlates with last year's total biomass density
    # d$density <- d$total_weight / (d$area_swept / 10000)
    # hist(d$density)
    # hist(log(d$density))
    # hist(log(d$density_lag1))
    # d$log_density <- log(d$density)
    # d$log_mean_density_lag1 <- log(d$mean_density_lag1)
    # plot(d$log_density ~ d$log_density_lag1)
    #
    # # TODO: I don't think density needs centering, but depth might?
    # hist(d$depth_m)
    # hist(log(d$depth_m))
    # hist((d$mean_density_lag1))
    # hist(log(d$mean_density_lag1))
    #
    # hist((d$log_density))
    #
    # # d$log_depth_c <- d$log_depth - log()
    # ## name this model version (knot distance added for file name later)
  }



  # if(add_density){
  #   # load density predictions for full survey grid if going to be used as covariates condition
  #   gridA <- readRDS(paste0("data-generated/density-predictions/p-", spp, "", dens_model_name2, ".rds")) %>%
  #     select(year, X, Y, survey, depth, log_depth, density) %>%
  #     mutate(
  #       year_density = year,
  #       year = year + 1,
  #       density_lag1 = density,
  #       log_density_lag1 = log(density)
  #     ) %>%
  #     select(-density) %>%
  #     group_by(year) %>% mutate(mean_density_lag1 = mean(density_lag1),
  #                               log_mean_density_lag1 = log(mean_density_lag1)) %>%
  #     ungroup() %>% mutate(dens_dev = log_density_lag1 - log_mean_density_lag1)
  #
  #   mean_den <- select(gridA, year, mean_density_lag1, log_mean_density_lag1) %>% distinct()
  #
  #
  #   # get current year density to scale condition index with
  #   gridB <- readRDS(paste0("data-generated/density-predictions/p-", spp, "", dens_model_name2, ".rds")) %>%
  #     select(year, X, Y, survey, depth, log_depth, density) %>%
  #     group_by(year) %>%
  #     mutate(sum_density = sum(density), prop_density = density / sum_density, log_density = log(density))
  #
  #   grid <- left_join(gridA, gridB) %>%
  #     filter(
  #       # !(year == 2020) &
  #       year <= 2022 & year >= 2000
  #     )
  # }



  # Make mesh ----
  hist(d$cond_fac)
  hist(log(d$cond_fac))

  mesh <- make_mesh(d, c("X", "Y"), cutoff = knot_distance)

  ggplot() +
    inlabru::gg(mesh$mesh) +
    coord_fixed() +
    geom_point(aes(X, Y, size = total_weight), data = d2) +
    geom_point(aes(X, Y, colour = group_catch_weight), data = d) +
    facet_wrap(~year) +
    scale_color_viridis_c(trans = ggsidekick::fourth_root_power_trans())

  ggplot() +
    inlabru::gg(mesh$mesh) +
    coord_fixed() +
    geom_point(aes(X, Y, size = group_catch_weight), data = d2) +
    geom_point(aes(X, Y, colour = log(cond_fac)), size = 0.5, data = d) +
    facet_wrap(~year) +
    scale_color_gradient2()

  # load refine function
  refine_cond_model <- function(m, set_formula = cond_formula, dist = knot_distance) {
    s <- sanity(m)
    t <- tidy(m, "ran_pars", conf.int = TRUE)
    if (!all(s) & !m$call$share_range) {
      # browser()
      if (abs(diff(t$estimate[t$term == "range"])) < dist) {
        m <- update(m,
          formula = set_formula,
          # formula = as.list(m[["formula"]]),
          share_range = TRUE,
          weights = m$data$sample_multiplier,
          # spatial = as.list(m[["spatial"]]),
          # spatiotemporal = as.list(m[["spatiotemporal"]]),
          # extra_time = m$extra_time,
          # family = m$family,
          data = m$data, mesh = m$spde
        )
        s <- sanity(m)
        t <- tidy(m, "ran_pars", conf.int = TRUE)
      }
    }
    if (!all(s)) {
      if (t$estimate[t$term == "sigma_O"] < 0.001) {
        m <- update(m,
          formula = set_formula,
          weights = m$data$sample_multiplier,
          spatial = "off",
          data = m$data, mesh = m$spde
        )
      }
    }
    sanity(m)
    return(m)
  }

  # Estimate condition model ----
  ## start with just an intercept model ----


  d %>%
    group_by(survey_group) %>%
    summarise(n = n())

  # browser()

  if (length(unique(d$survey_group)) == 1) {
    cond_formula <- cond_fac ~ 1 + poly(days_to_solstice, 2)
    model_name <- "-doy"
    mf <- paste0(
      "data-generated/condition-models-", group_tag, "/", spp, "-c-",
      group_tag, model_name, "-", knot_distance, "-km.rds"
    )
  } else {
    cond_formula <- cond_fac ~ as.factor(survey_group) +
      poly(days_to_solstice, 2)
    model_name <- "-all-st2002-doy"
    mf <- paste0(
      "data-generated/condition-models-", group_tag, "/", spp, "-c-",
      group_tag, model_name, "-", knot_distance, "-km.rds"
    )
  }

  if (file.exists(mf)) {
    try(m1 <- readRDS(mf))
  }

  if (!exists("m1")) {

    # TODO: Add DOY with cyclic smoother?
    # require(mgcv)
    # set.seed(6)
    # x <- sort(runif(200)*10)
    # z <- runif(200)
    # f <- sin(x*2*pi/10)+.5
    # y <- rpois(exp(f),exp(f))
    #
    # ## finished simulating data, now fit model...
    # b <- gam(y ~ s(x,bs="cc",k=12) + s(z),family=poisson,
    #          knots=list(x=seq(0,10,length=12)))
    # ## or more simply
    # b <- gam(y ~ s(x,bs="cc",k=12) + s(z),family=poisson,
    #          knots=list(x=c(0,10)))
    #
    # ## plot results...
    # par(mfrow=c(2,2))
    # plot(x,y);plot(b,select=1,shade=TRUE);lines(x,f-mean(f),col=2)
    # plot(b,select=2,shade=TRUE);plot(fitted(b),residuals(b))



    sort(unique(d$year))

    m1 <- sdmTMB(cond_formula,
      weights = d$sample_multiplier,
      mesh = mesh,
      data = d,
      spatial = "on",
      spatiotemporal = "rw",
      # spatiotemporal = "off",
      # spatial_varying = ~ dens_dev,
      # spatial_varying = ~ log_mean_density_lag1,
      extra_time = c(2002, 2006, 2007, 2010, 2016, 2020),
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

    m1 <- refine_cond_model(m1, set_formula = cond_formula, dist = knot_distance)
    saveRDS(m1, mf)
  } else {
    # make sure if converged last time
    m1 <- refine_cond_model(m1, set_formula = cond_formula, dist = knot_distance)
    saveRDS(m1, mf)
  }

  m1
  m1$sd_report
  tidy(m1, "ran_pars", conf.int = TRUE)

  m <- m1

  # Add density dependence ----
  ## don't do this for now, but can be used to explore utility of covariates
  if (add_density) {
    d$log_density_c <- d$log_density - mean(d$log_density, na.rm = TRUE)
    d$log_density_lag1_c <- d$log_density_lag1 - mean(d$log_density_lag1, na.rm = TRUE)


    if (length(unique(d$survey_group)) == 1) {

      # model_name <- "1-st2002-doy-dlag1"
      model_name <- "1-st2002-doy-d0c"

      cond_formula2 <- cond_fac ~ 1 + poly(days_to_solstice, 2) +
        # poly(log_density_lag1_c, 2)
        poly(log_density_c, 2)

      mf <- paste0(
        "data-generated/condition-models-", group_tag, "/", spp, "-c-",
        group_tag, model_name, "-", knot_distance, "-km.rds"
      )
    } else {

      # model_name <- "-all-st2002-doy-dlag1"
      model_name <- "-all-st2002-doy-d0c"

      cond_formula2 <- cond_fac ~ as.factor(survey_group) +
        poly(days_to_solstice, 2) +
        # poly(log_density_lag1_c, 2)
        poly(log_density_c, 2)

      # # log_mean_density_lag1 +
      # # poly(log_mean_density_lag1, 2) +
      # # dens_dev +
      # # # poly(dens_dev, 2)  +
      # poly(log_depth, 2)

      mf <- paste0(
        "data-generated/condition-models-", group_tag, "/",
        spp, "-c-", group_tag, model_name, "-", knot_distance, "-km.rds"
      )
    }


    if (file.exists(mf)) {
      try(m2 <- readRDS(mf))
    }

    if (!exists("m2")) {
      mesh2 <- make_mesh(d, c("X", "Y"), cutoff = knot_distance)

      m2 <- update(m1, cond_formula2,
        weights = d$sample_multiplier,
        mesh = mesh2, data = d
      )
      saveRDS(m2, mf)
      m2 <- refine_cond_model(m2, set_formula = cond_formula2, dist = knot_distance)
      saveRDS(m2, mf)
    }

    sanity(m2)
    m2
    m2$sd_report
    tidy(m2, "ran_pars", conf.int = TRUE)
    m <- m2
  }

  # Filter grid ----
  if (add_density) {
    grid <- left_join(gridA, gridB) %>% filter(
      # !(year == 2020) &
      year <= 2022 & year >= 2001
    )

    grid$log_density_c <- 0
    grid$log_density_lag1_c <- 0
  } else {
    grid <- gridA %>% filter(
      # !(year == 2020) &
      year <= 2022 & year >= 2001
    )
  }

  grid$survey_group <- "TRAWL"

  sort(unique(m$data$year))
  sort(unique(grid$year))

  # might be redundant
  grid <- filter(grid, year %in% unique(m$data$year))

  pc <- predict(m, newdata = grid, return_tmb_object = TRUE)


  p2 <- pc$data %>%
    # filter(!(year == 2020)) %>%
    mutate(cond = exp(est))


  # filter to plot only cells representing 99% of mean predicted biomass
  # cells must be defined by "X", "Y", time by "year", and biomass/abundance stored as "density"
  p2 <- trim_predictions_by_year(p2, 0.001)


  ggplot(p2, aes(X, Y, colour = (cond), fill = (cond))) +
    geom_tile(width = 2, height = 2, alpha = 1) +
    facet_wrap(~year) +
    scale_fill_viridis_c() +
    scale_colour_viridis_c() +
    # scale_fill_gradient2() +
    # scale_colour_gradient2() +
    labs(title = paste0(species, ": ", group_label, " ", model_name), x = "", y = "")

  # ggsave(paste0("figs/condition-map-", spp, "-", group_tag,
  #        model_name, "-", knot_distance, "-km.png"),
  #        height = fig_height, width = fig_width
  # )


  # Map model predictions ----

  # browser()

  dset <- readRDS("data-generated/set-data-used.rds") %>%
    filter(species_common_name == tolower(species)) %>%
    sdmTMB::add_utm_columns(., ll_names = c("longitude", "latitude"), utm_crs = 32609) %>%
    # filter(year > 2000)
    filter(year >= min(m$data$year), year <= max(m$data$year))


  unique(dset$survey_abbrev)
  # all sets
  set_list <- dset %>%
    select(fishing_event_id, longitude, latitude, X, Y, year, catch_weight, catch_count) %>%
    distinct() %>%
    mutate(lon = longitude, lat = latitude)

  # # just sampled sets
  # set_list <- d2 %>% select(fishing_event_id, longitude, latitude, X, Y) %>% distinct() %>%
  #   mutate(lon = longitude, lat = latitude)

  model_dat <- d %>%
    group_by(fishing_event_id) %>%
    mutate(
      count = n()
    )

  model_dat <- left_join(set_list, model_dat, multiple = "all") %>% mutate(
    density = group_catch_weight,
    caught = ifelse(catch_count > 0 | catch_weight > 0, 1, 0),
    count = ifelse(is.na(count), 0, count),
    present = ifelse(count > 0, 1, ifelse(caught == 1, 0, NA_integer_))
  )

  # model_dat %>% group_by(present, caught) %>% summarise(n = n()) %>% View()
  p2$log_cond <- log(p2$cond)
  p2 <- p2 %>% mutate(cond_trim = ifelse(cond > quantile(p2$cond, 0.99),
    quantile(p2$cond, 0.99), cond
  ))
  g <- gfenvirocor::plot_predictions(p2, model_dat, # extrapolate_depth = FALSE,
    # fill_column = "log_cond",
    fill_column = "cond_trim",
    fill_label = "Condition \nfactor",
    pt_column = "count",
    pt_label = "Fish \nsampled",
    pt_size_range = c(0.5, 4),
    pos_pt_fill = NA,
    bin_pt_col = "black",
    pos_pt_col = "red",
    # x_buffer = c(-0, 0),
    # y_buffer = c(-0, 0),
    fill_scale =
      ggplot2::scale_fill_viridis_c(),
    # ggplot2::scale_fill_viridis_c(trans = "log10"),
    bounds = grid,
    rotation_angle = 30, show_raw_data = TRUE
  )

  g <- g + facet_wrap(~year, ncol = 7) + ggtitle(paste0(species, ": ", group_label, " ", model_name))

  ggsave(paste0("figs/condition-map-", spp, "-", group_tag, model_name, "-", knot_distance, "-km.png"),
    height = fig_height * 1.5, width = fig_width
  )

  # # experiment with using gfplot function
  # library(gfplot)
  # fit <- fit_survey_sets(dset,
  #                        years = 2015,
  #                        survey = "SYN QCS")
  # # names(fit)
  # plot_survey_sets(fit$predictions, fit$data, fill_column = "combined", rotation_angle = 40, show_raw_data = FALSE)


  # Get coastwide index ----
  i1 <- paste0(
    "data-generated/cond-index/cond-index-", group_tag, "-", spp,
    model_name, "-", knot_distance, "-km.rds"
  )

  if (!file.exists(i1)) {
    ind2 <- get_index(pc, area = grid$prop_density, bias_correct = TRUE)

    saveRDS(ind2, i1)
  } else {
    ind2 <- readRDS(i1)
  }

  ggplot(ind2, aes(year, est)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
    xlab("Year") +
    ylab("Predicted average condition factor") +
    labs(title = paste0(species, ": ", group_label, " ", model_name))

  ggsave(paste0("figs/condition-index-", spp, "-", group_tag, model_name, "-", knot_distance, "-km.png"),
    height = fig_height / 2, width = fig_width / 2
  )



  # # Get survey indices ----
  # i2 <- paste0("data-generated/cond-index/survey-cond-indices-", spp, "-", group_tag, model_name, "-", knot_distance, "-km.rds")
  #
  # if(!file.exists(i2)) {
  #
  # ## TODO: if we want to split by stock area, will need to edit grid first to add species specific stock areas
  # preds <- grid %>%
  #   split(.$survey) %>%
  #   lapply(function(x) predict(m, newdata = x, return_tmb_object = TRUE))
  #
  # inds <- purrr::map_dfr(preds, function(.x)
  #   get_index(.x, area = .x$data$prop_density_by_survey, bias_correct = TRUE), .id = "region")
  #
  # survey_years <- m$data %>% select(survey_abbrev, year) %>% distinct() %>%
  #   mutate(region = ifelse(survey_abbrev == "HS MSA", "SYN HS",
  #                          ifelse(survey_abbrev == "MSSM QCS", "SYN QCS",
  #                                 ifelse(survey_abbrev == "MSSM WCVI", "SYN WCVI",
  #                                        survey_abbrev))))
  #
  # ind3 <- left_join(survey_years, inds, multiple = "all") %>%
  #   filter(!is.na(est))
  #
  # saveRDS(ind3, i2)
  #
  # } else {
  # ind3 <- readRDS(i2)
  # }
  #
  # ggplot(ind3, aes(year, est, fill = region)) +
  #   geom_line(aes(colour = region)) +
  #   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.1) +
  #   xlab("Year") +
  #   ylab("Predicted average condition factor") +
  #   labs(title = paste0(species, ": ", group_label, " ", model_name))
  #
  # ggsave(paste0("figs/condition-index-", spp, "-split-", group_tag, model_name, "-", knot_distance, "-km.png"),
  #        height = fig_height / 2, width = fig_width/1.5
  # )


  #   map(get_index(., area = .$data$prop_density, bias_correct = FALSE)) %>%
  #   bind_rows(.id = "survey")


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
  plot_covariates <- FALSE
  if (plot_covariates) {

    # TODO: not been tested with recent model configurations

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
}

# Run with pmap -----

index_list2 <- index_list[2, ]
pmap(index_list2, calc_condition_indices)

# Run with furrr ----

library(gfenvirocor)
library(future)

is_rstudio <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
is_unix <- .Platform$OS.type == "unix"
cores <- round(parallel::detectCores() / 2)
(cores <- parallel::detectCores() - 6L)
if (!is_rstudio && is_unix) plan(multicore, workers = cores) else plan(multisession, workers = cores)


furrr::future_pmap(index_list, calc_condition_indices)
