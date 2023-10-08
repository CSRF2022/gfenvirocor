# 1. Create a total biomass distribution model (to use for prediction grid)
# 2. Create prediction grid with density and depth
# TODO: get matching depth for use in models and on prediction grids?

devtools::load_all()
options(scipen = 100, digits = 4)

library(future)
library(tidyverse)
library(sdmTMB)
library(gfplot)
library(ggsidekick)
library(tidyverse)
# library(gfenvirocor)


species_list <- list(
  # "Arrowtooth Flounder", #
  # "Petrale Sole", #
  # "English Sole",#
  # "Dover Sole",#
  # "Flathead Sole",#
  # "Southern Rock Sole",#
  # "Rex Sole", #
  # "Curlfin Sole",#
  # "Sand Sole",#
  # "Slender Sole",#
  # "Pacific Sanddab",#
  "Pacific Halibut",#
  "Butter Sole",#
  "Starry Flounder"#
  # #"C-O Sole", # way too few!
  # "Deepsea Sole" # no maturity
)
species_list <- list(species = species_list)
#
# species <- "Curlfin Sole"
# species <- "Pacific Halibut"
# species <- "Petrale Sole"
# species <- "Arrowtooth Flounder"




# ##### function for running species in parallel --------
#
# is_rstudio <- !is.na(Sys.getenv("RSTUDIO", unset = NA))
# is_unix <- .Platform$OS.type == "unix"
# cores <- round(parallel::detectCores() / 2)
# (cores <- parallel::detectCores() - 6L)
# if (!is_rstudio && is_unix) plan(multicore, workers = cores) else plan(multisession, workers = cores)


fit_all_distribution_models <- function(species) {

  options(scipen = 100, digits = 4)
  theme_set(theme_sleek())
  mat_threshold <- 0.5
  knot_distance <<- 15

  # only including years using when splitting by maturity
  extra_years <- c(2001, 2020)

  # # full timeframe for MSA
  # extra_years <- c(1985, 1986, 1988, 1990,
  #                  1992, 1994, 1997, 1999, 2001, 2020)
  # extra_years <- NULL


  set_family <- delta_gamma()
  # dens_model_name <- "-dg-st2000-all-10min-xt-offset"
  dens_model_name <- "-dg-st2000-all-500m-xt-offset"
  # # dens_model_name <- "-dg-doy-1-22-10min-xt-offset"
  # # dens_model_name <- "-dg-all-yrs-mssm-1-22-10min-xt-offset"
  # # dens_model_name <- "-dg-all-yrs-other-1-22-10min-xt-offset"
  # dens_model_name <- "-dg-mssm-1-22-10min-xt-offset"
  #
  # set_family <- delta_poisson_link_gamma()
  # dens_model_name <- "-dplg-st2000-all-10min-xt-offset"
  # dens_model_name <- "-dplg-mssm-1-22-10min-xt-offset"

  # set_family <- tweedie()
  # dens_model_name <- "-tw-doy-1-22-10min-xt-offset"
  dens_model_name_long <- "depth, DOY, and survey type"

  # set_family2 <- tweedie()
  set_family2 <- delta_gamma()

  fig_height <- 4 * 2
  fig_width <- 5 * 2


  surveys_included <- c(
    "HS MSA",
    "SYN HS", "SYN QCS",
    "SYN WCHG", "SYN WCVI",
    "MSSM QCS", "MSSM WCVI",
    "OTHER"
  )

  if (length(set_family$family) > 1) {
    set_spatial <- "on"
    # set_spatial <- list("on", "off")
    set_spatiotemporal <- list("rw", "rw")
  } else {
    set_spatial <- "on"
    set_spatiotemporal <- "rw"
  }

  if (species == "North Pacific Spiny Dogfish") {
    custom_maturity_code <- c(NA, 55)
    custom_length_threshold <- NULL
    # custom_length_threshold <- c(70.9, 86.2)
    # set_family <- delta_lognormal_mix()
    # set_family2 <- delta_lognormal()
    set_family <- delta_gamma_mix()
    set_family2 <- delta_gamma()
    # set_spatial <- list("on", "off")
    # set_spatiotemporal <- list("rw", "off")
    dens_model_name <- "-dgm-doy-1-22-xt-offset"
  } else {
    custom_maturity_code <- NULL
    custom_length_threshold <- NULL
  }

  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

  # set naming conventions ----
  m0 <- paste0(spp, "-total", dens_model_name, "-", knot_distance, "-km")
  m1 <- paste0(spp, "-mat-fem", dens_model_name, "-", knot_distance, "-km")
  m2 <- paste0(spp, "-mat-m", dens_model_name, "-", knot_distance, "-km")
  m3 <- paste0(spp, "-imm", dens_model_name, "-", knot_distance, "-km")

  ### filenames for all density models
  dir.create(paste0("data-generated/density-models/"), showWarnings = FALSE)

  fm <- paste0("data-generated/density-models/", m0, ".rds")
  fmf <- paste0("data-generated/density-models/", m1, ".rds")
  fmm <- paste0("data-generated/density-models/", m2, ".rds")
  fmi <- paste0("data-generated/density-models/", m3, ".rds")

  ### filenames for all density models
  dir.create(paste0("data-generated/density-predictions/"), showWarnings = FALSE)

  pfn <- paste0("data-generated/density-predictions/p-", m0, ".rds")
  pmfn <- paste0("data-generated/density-predictions/p-", m1, ".rds")
  pmfn2 <- paste0("data-generated/density-predictions/p-", m2, ".rds")
  pifn <- paste0("data-generated/density-predictions/p-", m3, ".rds")

  ### filenames for all generated indices
  dir.create(paste0("data-generated/density-index/"), showWarnings = FALSE)

  i0 <- paste0("data-generated/density-index/i-", m0, ".rds")
  i1 <- paste0("data-generated/density-index/i-", m1, ".rds")
  i2 <- paste0("data-generated/density-index/i-", m2, ".rds")
  i3 <- paste0("data-generated/density-index/i-", m3, ".rds")


  # load data ----
  # dset <- readRDS("data-raw/survey-sets-all.rds") %>%
  dset1 <- readRDS("data-raw/survey-sets-flatfish.rds") %>%
    filter(
      survey_abbrev != "SABLE",
      # some MSSM sets are in both as QCS and WCVI
      !(survey_series_id == 6 & latitude < 50),
      !(survey_series_id == 7 & latitude > 50),
      # 11 useful for deeper species, 9 from 1996 probably too limited
      !(survey_abbrev == "OTHER" & !(survey_series_id %in% c(9, 11))),
      species_common_name == tolower(species)
    ) %>%
    mutate(survey_abbrev = ifelse(survey_abbrev == "MSSM" & latitude < 50, "MSSM WCVI",
      ifelse(survey_abbrev == "MSSM" & latitude > 50, "MSSM QCS", survey_abbrev)
    )) %>%
    distinct()

  # dsamp <- readRDS("data-raw/survey-samples-all.rds") %>%
  dsamp <- readRDS("data-raw/survey-samples-flatfish.rds") %>%
    filter(survey_abbrev != "SABLE") %>%
    filter(species_common_name == tolower(species)) %>%
    distinct()


  # # data exploration and processing ----
  dsum <- dset1 %>%
    filter(catch_weight > 0) %>%
    # filter(survey_abbrev == "OTHER") %>%
    group_by(survey_abbrev, survey_series_desc, survey_series_id) %>%
    summarise(
      sum = sum(catch_weight, na.rm = T),
      count = sum(catch_count, na.rm = T),
      n = n(),
      min_year = min(year), max_year = max(year)
    )
  # write.csv(dsum, "all-surveys.csv")
  #
  # dset1 %>% filter(year == 1999) %>% View() # one unusable set has wrong year
  #
  # dset1 %>% filter(survey_series_id %in% c(1, 2, 3, 4, 6, 7, 9, 11,16)) %>%
  #   filter(usability_code %in% c(1, 22, 16, 6)) %>% ggplot() +
  #   facet_wrap(~year) +
  #   geom_point(aes(longitude, latitude, colour = as.factor(survey_series_id)), alpha = 0.1)

  dsum2 <- dsamp %>%
    filter(!is.na(length), sex %in% c(1, 2)) %>%
    group_by(survey_abbrev, survey_series_desc, survey_series_id, year) %>%
    summarise(
      n = n()
    )
  # while there are lengths going back to 1984, no weight or sex before 2002
  ## check against original function call
  # dsamp1 <- readRDS("data-raw/survey-samples-all-original.rds") %>%
  #   filter(survey_abbrev != "SABLE OFF") %>%
  #   filter(species_common_name == tolower(species)) %>% distinct()

  dset <- dset1 %>% filter(survey_abbrev %in% surveys_included) %>%
    # keep those surveys with this species + all synoptic surveys
    filter(survey_abbrev %in% dsum$survey_abbrev |
      survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI"))

  check_for_duplicates <- dset[duplicated(dset$fishing_event_id), ]
  # I'm not using sample is so shouldn't be any unless something else can cause this
  # test_event <- dset[ dset$fishing_event_id == 329270, ]

  # unique(dset$survey_abbrev)
  # unique(dsamp$survey_abbrev)
  # unique(dsamp$maturity_code)


  dss <- gfplot::split_catch_by_sex(dset, dsamp,
    survey = surveys_included,
    immatures_pooled = TRUE,
    custom_maturity_at = custom_maturity_code,
    custom_length_thresholds = custom_length_threshold,
    p_threshold = mat_threshold,
    plot = TRUE
  )

  # dss$maturity_plot
  # dss$weight_plot

  meandoors <- dss$data %>%
    filter(group_name == "Mature females" &
      usability_code == 1 &
      doorspread_m != 0) %>%
    group_by(survey_id) %>%
    summarise(
      mean_doorspread = mean(doorspread_m, na.rm = TRUE)
    )

  ds <- dss$data %>%
    left_join(meandoors) %>%
    # filter(!(usability_code %in% c(5,9,13))) %>%
    # filter(usability_code %in% c(1, 22)) %>%
    filter(usability_code %in% c(1, 22, 16, 6)) %>%
    mutate(
      DOY = as.numeric(strftime(time_deployed, format = "%j")),
      days_to_solstice = DOY - 172,
      fishing_event_id = as.factor(fishing_event_id),
      vessel_cap_combo = factor(paste0(vessel_id, "-", captain_id)),
      vessel = as.factor(vessel_id),
      captain = as.factor(ifelse(is.na(captain_id),
        paste0(vessel_id, "-", year), captain_id
      )),
      usability_name = paste(usability_code, "-", usability_desc),
      usability_name = fct_reorder(usability_name, usability_code),
      doorspread_m = ifelse(doorspread_m == 0, mean_doorspread, doorspread_m),
      log_depth = log(depth_m),
      log_depth_c = log_depth - 5, # mean and median for whole data set
      survey_type = as.factor(ifelse(survey_abbrev == "HS MSA", "MSA",
        ifelse(survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") &
          year > 2002 & year <= 2005, "MSSM 2003-05",
        ifelse(survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") &
          year > 2005, "MSSM post-2005",
        ifelse(survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") &
          year <= 2002, "MSSM pre-2003",
        ifelse(survey_abbrev == "OTHER", "OTHER", "SYN")
        )
        )
        )
      )),
      area_swept = ifelse(is.na(tow_length_m),
        doorspread_m * duration_min * speed_mpm,
        doorspread_m * tow_length_m
      )
    ) %>%
    # if speed recorded, it isn't too slow
    filter(!is.na(speed_mpm) & speed_mpm != 0 & speed_mpm >= 50) %>%
    # if time recorded, it was at least 10 min
    filter(!is.na(duration_min) & duration_min != 0 & duration_min >= 10) %>%
    # if tow length available it's at least 200m
    filter(is.na(tow_length_m) | tow_length_m > 500) %>%
    filter(area_swept > 0) %>%
    # filter(year > 1983) # too slow to fit using only MSSM from 1975 to 1983
    # early MSSM not reliable for small fish
    filter(survey_type != "MSSM pre-2003" & year >= 2000)

  ds <- ds %>% filter(!is.na(catch_weight))
  ds <- ds %>% filter(!is.na(depth_m))
  ds <- ds %>% filter(!is.na(area_swept))
  ds <- ds %>% filter(!is.na(latitude))
  ds <- ds %>% filter(!is.na(longitude))
  ds$offset <- log(ds$area_swept / 100000)

  # ds %>% group_by(usability_code, usability_desc, survey_type) %>% summarise(
  #   mean_catch = mean(catch_weight, na.rm = TRUE),
  #   sum_catch = sum(catch_weight, na.rm = TRUE),
  #   n = n()
  # )

  ds %>%
    filter(group_name == "Mature females") %>%
    distinct() %>%
    ggplot() +
    # geom_violin(aes(area_swept, usability_desc))+
    geom_point(aes(area_swept, usability_name, size = log(catch_weight + 1)), alpha = 0.3) +
    scale_y_discrete(limits = rev) +
    facet_grid(~survey_abbrev, scales = "free") +
    theme(axis.title.y = element_blank())
  #
  # ggsave("figs/all-usabilities-petrale.png", width = 17, height = 3.5)

  unique(ds$survey_abbrev)
  unique(ds$survey_type)
  sort(unique(ds$year))
  # ds %>% select(survey_abbrev, year,
  #               vessel_id, captain_id, vessel_cap_combo) %>% distinct() %>% View()

  # check that offset doesn't contain NAs of Inf
  range(ds$offset)
  range(ds$area_swept)
  mean(ds$offset)

  ggplot(ds) +
    geom_histogram(aes(offset)) +
    facet_wrap(~year)


  # make grid ----
  ds$X <- NULL
  ds$Y <- NULL

  d <- sdmTMB::add_utm_columns(ds, ll_names = c("longitude", "latitude"), utm_crs = 32609)

  if (is.null(extra_years)) {
    grid <- replicate_df(gfplot::synoptic_grid,
      time_name = "year",
      time_values = unique(d$year)
    ) %>%
      mutate(
        fishing_event_id = as.factor(paste(ds$fishing_event_id[1])),
        days_to_solstice = 0,
        log_depth = log(depth),
        log_depth_c = log_depth - 5, # mean and median for whole data set
        survey_type = as.factor("SYN"),
        vessel = as.factor("584"),
        captain = as.factor("408"),
        vessel_cap_combo = as.factor("584-408") # has greatest spatial coverage
      )
  } else {
    # if extra time
    grid <- replicate_df(gfplot::synoptic_grid,
      time_name = "year",
      time_values = min(d$year):max(d$year)
    ) %>%
      mutate(
        fishing_event_id = as.factor(paste(ds$fishing_event_id[1])),
        days_to_solstice = 0,
        log_depth = log(depth),
        log_depth_c = log_depth - 5, # mean and median for whole data set
        survey_type = as.factor("SYN"),
        captain = as.factor("408"),
        vessel = as.factor("584"),
        vessel_cap_combo = as.factor("584-408") # has greatest spatial coverage
      )
  }


  # check that my data and grid are on the same XY scale
  range(grid$year)
  range(d$year)
  range(d$X)
  range(grid$X)
  range(d$Y)
  range(grid$Y)

  dp <- d %>% filter(catch_weight > 1)

  hist(log(dp$catch_weight))
  range(d$catch_weight)

  # # Keep only synoptic data
  # d <- filter(d, survey_type == "SYN")

  # make mesh for total density
  d1 <- d %>% filter(group_name == "Mature females")

  mesh <- make_mesh(d1, c("X", "Y"), cutoff = knot_distance)

  ggplot() +
    inlabru::gg(mesh$mesh) +
    coord_fixed() +
    geom_point(aes(X, Y, colour = catch_weight, size = catch_weight),
      data = filter(d1, catch_weight > 0)
    ) +
    facet_wrap(~year) +
    scale_color_viridis_c(trans = "fourth_root_power")

  mesh$mesh$n

  browser()

  # start modeling ----

  if (!file.exists(fm)) {
    m <- sdmTMB(
      catch_weight ~ 1 + survey_type +
        poly(log_depth_c, 2) +
        poly(days_to_solstice, 2), # +
      # (1|captain),
      # (1|vessel_cap_combo),
      offset = "offset",
      mesh = mesh,
      data = d1,
      spatial = set_spatial,
      spatiotemporal = set_spatiotemporal,
      share_range = FALSE,
      silent = FALSE,
      time = "year",
      extra_time = extra_years,
      # reml = TRUE,
      family = set_family,
      control = sdmTMBcontrol(
        start = list(logit_p_mix = qlogis(0.01)),
        map = list(logit_p_mix = factor(NA)),
        newton_loops = 0L
      ),
      priors = sdmTMBpriors(
        matern_s = pc_matern(
          range_gt = knot_distance * 1.5,
          sigma_lt = 2
        ),
        matern_st = pc_matern(
          range_gt = knot_distance * 1.5,
          sigma_lt = 2
        )
      )
    )

    saveRDS(m, fm)

    if (length(m$family) == 6) {
      m <- refine_delta_model(m, alternate_family = set_family2)
    } else {
      m <- refine_model(m)
    }

    saveRDS(m, fm)
  } else {
    m <- readRDS(fm)

    # browser()
    if (!all(sanity(m))) {
      if (length(m$family) == 6) {
        m <- refine_delta_model(m, alternate_family = set_family2)
      } else {
        m <- refine_model(m)
      }
      saveRDS(m, fm)
    }
  }


  # browser()

  m
  m$sd_report
  tidy(m, "ran_pars", conf.int = TRUE, model = 1)
  try(tidy(m, "ran_pars", conf.int = TRUE, model = 2))

  # m0 <- sdmTMB(
  #   catch_weight ~ 1,
  #   # mesh = mesh,
  #   data = m$data,
  #   spatial = "off",
  #   spatiotemporal = "off",
  #   silent = FALSE,
  #   family = m$family
  # )
  #
  # de <- 1 - (logLik(m)/logLik(m0))
  # de[1]

  grid <- filter(grid, year %in% c(sort(unique(m$data$year))))

  if (file.exists(pfn) & file.exists(i0)) {
    p <- readRDS(pfn)
  } else {
    p <- predict(m, re_form_iid = NA, newdata = grid, return_tmb_object = TRUE)

    map_density(p, pfn, variable = "density_trimmed") +
      labs(
        title = paste0(species, ": total biomass (", dens_model_name, ")"),
        # subtitle = paste0("Deviance explained:", round(de[1], 2))
      )

    ggsave(paste0("figs/density-map-", m0, ".png"),
      height = fig_height, width = fig_width
    )

    plot_index(p, extra_years = NULL, i0) +
      ggtitle(paste0(species, ": total biomass (", dens_model_name, ")"))

    ggsave(paste0("figs/density-index-", m0, ".png"),
      height = fig_height / 2, width = fig_width / 1.5
    )
  }

  # # not working with offset
  # g <- ggeffects::ggeffect(m, paste0("log_depth [",
  #   range(d$log_depth)[1], ":", range(d$log_depth)[2], "by=0.05]"))
  # plot(g)

  ## mature females
  d2 <- d1 %>%
    filter(year > 2001) %>%
    filter(!is.na(group_catch_est))

  which_surv <- d2 %>%
    filter(group_catch_est > 0) %>%
    group_by(survey_type) %>%
    summarise(n = n())

  d2 <- filter(d2, survey_type %in% which_surv$survey_type)

  mesh2 <- make_mesh(d2, c("X", "Y"), cutoff = knot_distance)

  if (!file.exists(fmf)) {
    mf <- sdmTMB(group_catch_est ~ 1 + survey_type +
      poly(log_depth_c, 2) +
      poly(days_to_solstice, 2),
    offset = "offset",
    spatial = as.list(m[["spatial"]]),
    spatiotemporal = as.list(m[["spatiotemporal"]]),
    time = "year",
    family = m$family,
    extra_time = c(2020),
    silent = FALSE,
    mesh = mesh2, data = d2
    )
    saveRDS(mf, fmf)
    if (length(mf$family) == 6) {
      mf <- refine_delta_model(mf, alternate_family = set_family2)
    } else {
      mf <- refine_model(mf)
    }
    saveRDS(mf, fmf)
  } else {
    mf <- readRDS(fmf)
    if (!all(sanity(mf))) {
      if (length(mf$family) == 6) {
        mf <- refine_delta_model(mf, alternate_family = set_family2)
      } else {
        mf <- refine_model(mf)
      }
      saveRDS(mf, fmf)
    }
  }

  # check that model updated properly
  sort(unique(m$data$year))
  sort(unique(mf$data$year))


  if (file.exists(pmfn) & file.exists(i1)) {
    pf <- readRDS(pmfn)
  } else {
    # pf <- predict(mf, re_form_iid = NA,
    #               newdata = filter(grid, year > 2001),
    #               return_tmb_object = TRUE)

    pf <- predict(mf,
      re_form_iid = NA,
      newdata = filter(grid, year %in% c(sort(unique(mf$data$year)))),
      return_tmb_object = TRUE
    )

    map_density(pf, pmfn) +
      labs(title = paste0(species, ": mature female biomass (", dens_model_name, ")"))

    ggsave(paste0("figs/density-map-", m1, ".png"),
      height = fig_height, width = fig_width
    )
  }


  ## mature males

  if (!file.exists(fmm)) {
    d2b <- d %>%
      filter(group_name == "Mature males") %>%
      filter(year > 2001) %>%
      filter(!is.na(group_catch_est))

    which_surv <- d2b %>%
      filter(group_catch_est > 0) %>%
      group_by(survey_type) %>%
      summarise(n = n())

    d2b <- filter(d2b, survey_type %in% which_surv$survey_type)

    mesh2b <- make_mesh(d2b, c("X", "Y"), cutoff = knot_distance)

    mm <- update(mf,
      mesh = mesh2b,
      data = d2b
    )
    saveRDS(mm, fmm)

    if (length(mm$family) == 6) {
      mm <- refine_delta_model(mm, alternate_family = set_family2)
    } else {
      mm <- refine_model(mm)
    }
    saveRDS(mm, fmm)
  } else {
    mm <- readRDS(fmm)
    if (!all(sanity(mm))) {
      if (length(mm$family) == 6) {
        mm <- refine_delta_model(mm, alternate_family = set_family2)
      } else {
        mm <- refine_model(mm)
      }
      saveRDS(mm, fmm)
    }
  }



  if (file.exists(pmfn2) & file.exists(i2)) {
    pm <- readRDS(pmfn2)
  } else {
    pm <- predict(mm,
      re_form_iid = NA,
      newdata = filter(grid, year %in% c(sort(unique(mm$data$year)))),
      return_tmb_object = TRUE
    )

    map_density(pm, pmfn2) +
      labs(title = paste0(species, ": mature male biomass (", dens_model_name, ")"))

    ggsave(paste0("figs/density-map-", m2, ".png"),
      height = fig_height, width = fig_width
    )
  }

  if (!file.exists(fmi)) {

    # browser()

    d3 <- d %>%
      filter(group_name %in% c("Immature")) %>%
      filter(year > 2001) %>%
      filter(!is.na(group_catch_est))

    which_surv <- d3 %>%
      filter(group_catch_est > 0) %>%
      group_by(survey_type) %>%
      summarise(n = n())


    d3 <- filter(d3, survey_type %in% which_surv$survey_type)

    # if(length(mm$family$family)>1) {
    #   simpspatial <- list("on", "off")
    # } else{
    #   simpspatial <- "on"
    # }

    mesh3 <- make_mesh(d3, c("X", "Y"), cutoff = knot_distance)

    mi <- update(mm,
      # spatial = simpspatial,
      mesh = mesh3,
      data = d3
    )

    saveRDS(mi, fmi)

    if (length(mi$family) == 6) {
      mi <- refine_delta_model(mi, alternate_family = set_family2)
    } else {
      mi <- refine_model(mi)
    }
    saveRDS(mi, fmi)
  } else {
    mi <- readRDS(fmi)
    if (!all(sanity(mi))) {
      if (length(mi$family) == 6) {
        mi <- refine_delta_model(mi, alternate_family = set_family2)
      } else {
        mi <- refine_model(mi)
      }
      saveRDS(mi, fmi)
    }
  }

  s <- sanity(mi)

  if (all(s)) {
    if (file.exists(pifn) & file.exists(i3)) {
      pi <- readRDS(pifn)
    } else {
      pi <- predict(mi,
        re_form_iid = NA,
        newdata = filter(grid, year %in% c(sort(unique(mi$data$year)))),
        return_tmb_object = TRUE
      )


      map_density(pi, pifn) +
        labs(title = paste0(species, ": immature biomass (", dens_model_name, ")"))

      ggsave(paste0("figs/density-map-", m3, ".png"),
        height = fig_height, width = fig_width
      )
    }
    # browser()
  }

  if (!file.exists(i0)) {
    # Generate coastwide indices

    plot_index(p, extra_years = NULL, i0) +
      ggtitle(paste0(species, ": total biomass (", dens_model_name, ")"))

    # ggsave(paste0("figs/density-index-", m0, ".png"),
    # height = fig_height / 2, width = fig_width /1.5
    # )
  }
  if (!file.exists(i1)) {
    plot_index(pf, extra_years = NULL, i1) +
      ggtitle(paste0(species, ": mature female biomass (", dens_model_name, ")"))

    # ggsave(paste0("figs/density-index-", m1, ".png"),
    # height = fig_height / 2, width = fig_width / 1.5
    # )
  }
  if (!file.exists(i2)) {
    plot_index(pm, extra_years = NULL, i2) +
      ggtitle(paste0(species, ": mature male biomass (", dens_model_name, ")"))

    # ggsave(paste0("figs/density-index-", m2, ".png"),
    # height = fig_height / 2, width = fig_width / 1.5
    # )
  }

  if (!file.exists(i3)) {
    try(plot_index(pi, extra_years = NULL, i3) +
      ggtitle(paste0(species, ": immature biomass (", dens_model_name, ")")))

    # ggsave(paste0("figs/density-index-", m3, ".png"),
    # height = fig_height / 2, width = fig_width / 1.5
    # )
  }

  ind0 <- readRDS(i0) %>% mutate(index = "Total")
  ind1 <- readRDS(i1) %>% mutate(index = "Mature female")
  ind2 <- readRDS(i2) %>% mutate(index = "Mature male")
  try(ind3 <- readRDS(i3) %>% mutate(index = "Immature"))

  bc_inds <- bind_rows(ind0, ind1, ind2)
  try(bc_inds <- bind_rows(ind0, ind1, ind2, ind3))

  # browser()

  p1 <- bc_inds %>%
    mutate(index = fct_relevel(index, rev)) %>%
    ggplot(aes(year, est)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = index), alpha = 0.3) +
    geom_line(aes(colour = index), linewidth = 0.7) +
    scale_colour_viridis_d(direction = 1, end = 0.8, option = "A") +
    scale_fill_viridis_d(direction = 1, end = 0.8, option = "A") +
    labs(colour = "Biomass Index", fill = "Biomass Index") +
    xlab("Year") +
    ylab("Biomass estimate (kg)") +
    ggtitle(paste0(species), subtitle = paste0(
      "Model: ",
      ifelse(length(m$family) == 6, m$family[6], paste0(m$family[1], "(link = 'log')")),
      ", spatial (", m[["spatial"]][1], ", ", m[["spatial"]][2],
      ") with st RW and ", dens_model_name_long,
      " (", paste(unique(m$data$survey_type), collapse = ", "), ")"
    ))

  p1

  # ggsave(paste0("figs/density-index-", spp, "-all",
  # dens_model_name, "-", knot_distance, "-km.png"),
  # height = fig_height / 2, width = fig_width / 1.5
  # )

  survey_years <- d1 %>%
    select(survey_abbrev, year) %>%
    distinct() %>%
    mutate(survey = ifelse(survey_abbrev == "HS MSA", "SYN HS",
      ifelse(survey_abbrev == "MSSM QCS", "SYN QCS",
        ifelse(survey_abbrev %in% c("OTHER", "MSSM WCVI"), "SYN WCVI",
          survey_abbrev
        )
      )
    ))
  #
  # grid2 <- grid %>%
  #   # filter(year > 2001)
  #   filter(year %in% c(sort(unique(mm$data$year))))

  fsi <- paste0(
    "data-generated/density-index/", spp, "-split-",
    dens_model_name, "-", knot_distance, "-km.rds"
  )

  # browser()

  if (!file.exists(fsi)) {
    inds <- split_index_by_survey(m, grid, "Total")
    inds2 <- split_index_by_survey(mf, grid, "Mature female")
    inds3 <- split_index_by_survey(mm, grid, "Mature male")

    all_split_inds <- bind_rows(inds, inds2, inds3)

    if (file.exists(f3)) {
      inds4 <- split_index_by_survey(mi, grid, "Immature")
      all_split_inds <- bind_rows(inds, inds2, inds3, inds4)
    }

    saveRDS(all_split_inds, fsi)
  } else {
    all_split_inds <- readRDS(fsi)
  }

  # browser()

  p2 <- all_split_inds %>%
    left_join(survey_years, ., multiple = "all") %>%
    select(-survey_abbrev) %>%
    filter(!is.na(est)) %>%
    distinct() %>%
    filter(index == "Total") %>%
    mutate(index = fct_relevel(index, rev)) %>%
    ggplot(aes(year, est)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = survey), alpha = 0.3) +
    geom_line(aes(colour = survey)) +
    # facet_wrap(~index, scales = "free") +
    # facet_wrap(~index, scales = "free_x") +
    scale_colour_viridis_d(direction = -1, end = 0.9) +
    scale_fill_viridis_d(direction = -1, end = 0.9) +
    labs(colour = "Area", fill = "Area") +
    coord_cartesian(ylim = c(0, max(all_split_inds$est) * 1.5)) +
    ggtitle("Total") +
    xlab("Year") +
    ylab("Biomass estimate (kg)")

  p2

  p3dat <- filter(all_split_inds, index != "Total") %>%
    left_join(survey_years, ., multiple = "all") %>%
    select(-survey_abbrev) %>%
    filter(!is.na(est)) %>%
    distinct()

  p3 <- p3dat %>%
    mutate(index = fct_relevel(index, rev)) %>%
    ggplot(aes(year, est)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = survey), alpha = 0.3) +
    geom_line(aes(colour = survey)) +
    # facet_wrap(~index, scales = "free") +
    facet_wrap(~index, scales = "free_x") +
    scale_colour_viridis_d(direction = -1, end = 0.9) +
    scale_fill_viridis_d(direction = -1, end = 0.9) +
    labs(colour = "Area", fill = "Area") +
    coord_cartesian(ylim = c(0, max(p3dat$est) * 1.25)) +
    # ggtitle(paste0(species, " (", dens_model_name, ")"))+
    xlab("Year") +
    ylab("Biomass estimate (kg)")

  if (length(unique(all_split_inds$model)) > 1) {
    p3 <- p3 + geom_text(aes(label = model),
      x = 2003, y = max(p3dat$est) * 1.1, size = 2.5, hjust = 0
    )
  }

  library(patchwork)
  p1a <- p1 + theme(axis.text.x = element_blank(), axis.title = element_blank())
  p2a <- p2 + theme(axis.title.x = element_blank())
  p3a <- p3 + theme(axis.title.y = element_blank(), legend.position = "none")

  p1a + p2a + p3a + plot_layout(ncol = 1)

  ggsave(paste0(
    "figs/density-index-", spp, "-all2",
    dens_model_name, "-", knot_distance, "-km.png"
  ),
  height = fig_height, width = fig_width
  )
}


# fit_all_distribution_models(species_list)

## test runs
pmap(species_list, fit_all_distribution_models)

# furrr::future_pmap(species_list, fit_all_distribution_models)



# dir.create(paste0("data-generated/density-depth-curves/"), showWarnings = FALSE)
# saveRDS(g, paste0("data-generated/density-depth-curves/", spp, "-d", dens_model_name, ".rds"))





# ## both mature males and females combined
# fmm2 <- paste0("data-generated/density-models/", spp, "-all-mat", dens_model_name, "-", knot_distance, "-km.rds")
#
# if (!file.exists(fmm2)) {
#   d4 <- ds %>%
#     filter(group_name %in% c("Mature males") & year > 2001) %>%
#     select(fishing_event_id, group_catch_est_mm = group_catch_est)
#   d4 <- left_join(d2, d4) %>%
#     mutate(group_catch_est2 = group_catch_est + group_catch_est_mm)
#
#   mm2 <- update(mf, group_catch_est2 ~ 1 + survey_type + poly(log_depth, 2), data = d4)
#
#   mm2 <-  refine_model(mm2)
#   saveRDS(mm2, fmm2)
# } else {
#   mm2 <- readRDS(fmm2)
# }
#
# pm2 <- predict(mm2, re_form_iid = NA, newdata =filter(grid, year > 2001), return_tmb_object = TRUE)
#
# map_density(pm2, "all-mat") +
#   labs(title = paste0(species, ": mature biomass (", dens_model_name, ")"))
#
# ggsave(paste0("figs/density-map-", spp, "-all-mat", dens_model_name, "-", knot_distance, "-km.png"),
#   height = fig_height, width = fig_width
# )
#
# plot_index(pm2, "all-mat") + ggtitle(paste0(species, ": mature biomass (", dens_model_name, ")"))
# ggsave(paste0("figs/density-index-", spp, "-all-mat", dens_model_name, "-", knot_distance, "-km.png"),
#   height = fig_height / 2, width = fig_width / 2
# )
#
