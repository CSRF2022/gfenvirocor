# wrangle all assessments together
devtools::load_all(".")
library(tidyverse)
library(sf)


gf <- readxl::read_xlsx("data/GF_assessments.xlsx") %>% filter(Outputs == "Y")

unique(gf$Species)

d <- list()
penv <- list()
rT <- list()
rO2 <- list()
lT <- list()
l2 <- list()

# add_enviro_vars <- FALSE
add_enviro_vars <- TRUE
climate_model <- "roms"

# for (i in 1:3) { # test with just first 3 stocks
# for (i in 5:8) { # test with just 3 POP stocks + boccacio
for (i in 1:nrow(gf)) {
  # i <- 1 # test with just one stock
  model <- case_when(
    gf$model[i] == "iSCAM" ~ "iscam",
    gf$model[i] == "landmark" ~ "landmark",
    gf$model[i] == "SS3" ~ "ss3",
    .default = "rowans"
  )

  if (model == "rowans") {
    catchdata <- paste0(gf$folder[i], gf$fileprefix[i], "-MPD(C", gf$filesuffix[i])
    prefix <- paste0(gf$folder[i], gf$fileprefix[i], "-MCMC(")
    suffix <- gf$filesuffix[i]
  } else {
    catchdata <- paste0(gf$folder[i], gf$catchfile[i])
    prefix <- paste0(gf$folder[i], gf$fileprefix[i])
    suffix <- gf$filesuffix[i]
  }


  print(paste(
    gf$Species[i],
    gf$Stock[i], model
  ))
  # browser()
  # if(gf$age_recruited[i] == 1){ age_recruits <- 1L}

  d[[i]] <- calc_prod(
    catchfile = catchdata,
    mcmcprefix = prefix,
    mcmcsuffix = suffix,
    gf$Species[i],
    gf$Stock[i],
    model_type = model,
    start_year = gf$start[i],
    end_year = gf$end[i],
    proportion_catch = gf$prop_catch[i],
    recruitment_age = gf$age_recruited[i],
    maturity_age = round(gf$age50mature[i])
  )

  d[[i]]$group <- gf$Group[i]

  if(add_enviro_vars){
  # currently calculating using 3 methods but might focus on different ones for each time period and loop over variable types instead
  methods <- c("mean", "min", "max")

  variable <- "tob"

  areas <- unlist(strsplit(as.character(gf$Areas[i]), ","))
  spawn_areas <- unlist(strsplit(as.character(gf$Spawn_areas[i]), ","))

  if (gf$Stock[i] == "Coastwide") {
    area_polygons <- NULL
  } else {
    area_polygons <- sf::st_read("../BC_map/Shapes/majorOutline.shp") %>%
      filter(Name %in% areas)
  }


  months <- as.numeric(unlist(strsplit(as.character(gf$R_T_months[i]), ",")))
  month_string <- paste0(months[1], "to", max(months))


  months2 <- as.numeric(unlist(strsplit(as.character(gf$R_O_months[i]), ",")))
  month_string2 <- paste0(months2[1], "to", max(months2))

  # browser()

  for (j in 1:3) {

    ann_variable_p <- paste0(variable, "_1to12_", climate_model, "_", methods[j])
    pgrid <- readRDS(paste0("data/grid_", ann_variable_p, ".rds")) %>%
      mutate(depth = posdepth)

    penv[[i]] <- get_stock_enviro_var(
      temporal_grid = pgrid,
      variable_name = ann_variable_p,
      species = gf$Species[i],
      stock = gf$Stock[i],
      stage = "Adult",
      # get for all lags up to the summer before spawn/birth would have occurred
      recruitment_age = gf$age_recruited[i] + 1,
      depth_range = c(gf$depth_0.025[i], gf$depth_0.975[i]),
      polygon = area_polygons
    )

    d[[i]] <- left_join(d[[i]], penv[[i]])
# browser()

    if(is.na(spawn_areas)) {
       # will default to coastwide like for adults in coastwide stocks
       spawn_polygons <- NULL
    } else {
    if(spawn_areas == "custom"){
      warning("Custom spawn areas not coded in yet.")
    } else {
    spawn_polygons <- sf::st_read("../BC_map/Shapes/majorOutline.shp") %>%
        filter(Name %in% spawn_areas)
    }
    }

    if(!is.na(gf$R_T_variable[i])){
    ann_variable_r <- paste0(gf$R_T_variable[i], "_", month_string, "_", climate_model, "_", methods[j])
    rgrid <- readRDS(paste0("data/grid_", ann_variable_r, ".rds")) %>%
      mutate(depth = posdepth)

    rT[[i]] <- get_stock_enviro_var(
      temporal_grid = rgrid,
      variable_name = ann_variable_r,
      species = gf$Species[i],
      stock = gf$Stock[i],
      stage = "Eggs/gestation",
      recruitment_age = gf$age_recruited[i],
      depth_range = c(gf$R_depth_min[i], gf$R_depth_max[i]),
      polygon = spawn_polygons
    )

    d[[i]] <- left_join(d[[i]], rT[[i]])
}

    if(!is.na(gf$R_O_variable[i])){
    ann_variable_r2 <- paste0(gf$R_O_variable[i], "_", month_string2, "_", climate_model, "_", methods[j])
    rgrid2 <- readRDS(paste0("data/grid_", ann_variable_r2, ".rds")) %>%
      mutate(depth = posdepth)

    rO2[[i]] <- get_stock_enviro_var(
      temporal_grid = rgrid2,
      variable_name = ann_variable_r2,
      species = gf$Species[i],
      stock = gf$Stock[i],
      stage = "Eggs/gestation",
      recruitment_age = gf$age_recruited[i],
      depth_range = c(gf$R_depth_min[i], gf$R_depth_max[i]),
      polygon = spawn_polygons
    )

    d[[i]] <- left_join(d[[i]], rO2[[i]])
    }


    # pelagic larval stage and/or early juvenile stages
    # assume similar geographic distribution to the stock, but distinct depth range

    if(!is.na(gf$Larval_T_variable[i])){
    ann_variable_l <- paste0(gf$Larval_T_variable[i], "_", month_string, "_", climate_model, "_", methods[j])
    lgrid <- readRDS(paste0("data/grid_", ann_variable_l, ".rds")) %>%
      mutate(depth = posdepth)

    lT[[i]] <- get_stock_enviro_var(
      temporal_grid = lgrid,
      variable_name = ann_variable_l,
      species = gf$Species[i],
      stock = gf$Stock[i],
      stage = "Larval",
      recruitment_age = gf$age_recruited[i],
      depth_range = c(gf$Larval_depth_min[i], gf$Larval_depth_max[i]),
      polygon = area_polygons
    )

    d[[i]] <- left_join(d[[i]], lT[[i]])
}

    if(!is.na(gf$Larval_2_variable[i])){
    # currently the second variable is always O2, but could be other things like currents or productivity
    ann_variable_l2 <- paste0(gf$Larval_2_variable[i], "_", month_string2, "_", climate_model, "_", methods[j])
    lgrid2 <- readRDS(paste0("data/grid_", ann_variable_l2, ".rds")) %>%
      mutate(depth = posdepth)

    l2[[i]] <- get_stock_enviro_var(
      temporal_grid = lgrid2,
      variable_name = ann_variable_l2,
      species = gf$Species[i],
      stock = gf$Stock[i],
      stage = "Larval",
      recruitment_age = gf$age_recruited[i],
      depth_range = c(gf$Larval_depth_min[i], gf$Larval_depth_max[i]),
      polygon = area_polygons
    )

    d[[i]] <- left_join(d[[i]], l2[[i]])
}
  }
  }
  d
}

dat <- do.call(bind_rows, d)
date_stamp <- Sys.Date()

if(add_enviro_vars){
# saveRDS(dat, paste0("data/all-productivity-and-covars-3spp.rds"))
  saveRDS(dat, paste0("data/all-productivity-and-covars-", date_stamp, ".rds"))
# saveRDS(dat, paste0("data/all-productivity-and-covars-", date_stamp, ".rds"))

# match("group",names(dat))

dat2 <- dat %>% pivot_longer((match("group",names(dat))+1):ncol(dat), values_to = "value", names_to = "variable")


out <- strsplit(as.character(dat2$variable),'_')
out <- do.call(rbind, out)
dat2$stage <- out[,1]
dat2$variable_type <- out[,2]
dat2$months <- out[,3]
dat2$climate_model <- out[,4]
dat2$agg_type <- out[,5]
dat2$lag <- out[,6]
dat2$lag <- as.numeric(gsub("lag", "", dat2$lag))

dat2[is.na(dat2["lag"]),]$lag <- 0

dat3 <- dat2[!is.na(dat2["value"]),]

# saveRDS(dat3, paste0("data/all-productivity-longer-3spp.rds"))
saveRDS(dat3, paste0("data/all-productivity-longer-", date_stamp, ".rds"))
} else {
  saveRDS(dat, paste0("data/all-productivity-", date_stamp, ".rds"))
}

