# get all climate vars
devtools::load_all(".")
library(ncdf4) # package for netcdf manipulation
library(tidyverse)
library(sf)
library(terra)


# temperature vars
# climate_model <- "bcc"
# variable <- "tob"


climate_model <- "roms"
# variable <- "TOB"
# variable <- "SST"
variable <- "O2"
# variable <- "salinity"


# for temperature variables the time frames of interest are stored in variables named:
# R_T_months and Larval_months
if (variable == "SST" | variable == "TOB") {

# get vector of unique R_T_months
rmonths <- readxl::read_xlsx(
  "data/GF_assessments.xlsx" # , col_type = "list"
) %>%
  filter(Outputs == "Y" & R_T_variable == variable) %>%
  select(R_T_months) %>%
  distinct() %>%
  na.omit()

# get vector of unique Larval_months
lmonths <- readxl::read_xlsx(
  "data/GF_assessments.xlsx" # , col_type = "list"
) %>%
  filter(Outputs == "Y" & Larval_T_variable == variable) %>%
  select(Larval_months) %>%
  distinct() %>%
  na.omit()

# add full year two above vectors
all_month_sets <- unique(c("1,2,3,4,5,6,7,8,9,10,11,12", rmonths$R_T_months, lmonths$Larval_months))

# for now generate mean, min, and max for all of these time periods
all_layers <- expand.grid(method = c("mean", "min", "max"),
                          months = all_month_sets)
}


# for O2 variables the time frames of interest are stored in variables named:
# R_O_months and Larval_months
if (variable == "O2" | variable == "salinity") {

rmonths <- readxl::read_xlsx(
  "data/GF_assessments.xlsx" # , col_type = "list"
) %>%
  filter(Outputs == "Y" & R_O_variable == "O2") %>%
  select(R_O_months) %>%
  distinct() %>%
  na.omit()

lmonths <- readxl::read_xlsx(
  "data/GF_assessments.xlsx" # , col_type = "list"
) %>%
  filter(Outputs == "Y" & Larval_2_variable == "O2") %>%
  select(Larval_months) %>%
  distinct() %>%
  na.omit()

# add full year two above vectors
all_month_sets <- unique(c("1,2,3,4,5,6,7,8,9,10,11,12", rmonths$R_O_months, lmonths$Larval_months))

# for now generate mean, min, and max for all of these time periods
all_layers <- expand.grid(method = c("mean", "min", "max"),
                          months = all_month_sets)
}

# to run full annual only
# all_layers <- expand.grid(method = c("mean", "min", "max"),
#                           months = c("1,2,3,4,5,6,7,8,9,10,11,12"))


# unlist(strsplit(as.character(all_layers$months[1]), ","))

for (i in 1:nrow(all_layers)) {
  months <- as.numeric(unlist(strsplit(as.character(all_layers$months[i]), ",")))

# can manually set a range of months not included in that vector, but right now will only get included for a species if it's in that R_months vector or the full year
  # months <- c(1:12)
  method <- all_layers$method[i]

  month_string <- paste0(months[1], "to", max(months))
  # if (month_string == "1to12") {
  #   month_string <- "ann"
  # }
# browser()
  ann_variable <- paste0(variable, "_", month_string, "_", climate_model, "_", method)
  gridfile <- paste0("data/grid_", ann_variable, ".rds")

  if (!file.exists(gridfile)) {

    ncdatafile <- paste0("data/", variable, "-", month_string, "-", climate_model, "-", method, ".rds")

    if (!file.exists(ncdatafile)) {

      if (variable == "TOB" & climate_model == "bcc") {
      df1 <- extract_netcdf_values(
        "data/bottom-temp/tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_195001-197912.nc",
        # variable_name = variable,
        nc_variable_name = "tob",
        out_variable_name = "tob",
        whichtimes = c(months),
        agg_method = method,
        model_start_time = 1950,
        model_end_time = 1979
      )

      df2 <- extract_netcdf_values(
        "data/bottom-temp/tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_198001-201412.nc",
        # variable_name = variable,
        nc_variable_name = "tob",
        out_variable_name = "tob",
        whichtimes = c(months),
        agg_method = method,
        model_start_time = 1980,
        model_end_time = 2014
      )

      df <- left_join(df1, df2)
      }

      if (variable == "TOB" & climate_model == "roms") {

        file1 <- 'data/roms-hindcast/bcc42_era5b37r1_mon1981to2018_botTSO'
        df <- extract_netcdf_values(paste0(file1,'.nc'),
                                    model_start_time = 1981,
                                    model_end_time = 2018,
                                    whichtimes = c(months),
                                    agg_method = method,
                                    xvar_name = "lon_rho",
                                    yvar_name = "lat_rho",
                                    time_name = "ocean_time",
                                    nc_variable_name = "temp",
                                    out_variable_name = "tob"
        )
      }

      if (variable == "SST" & climate_model == "roms") {

        file2 <- 'data/roms-hindcast/bcc42_era5b37r1_mon1981to2018_surTSO'

        df <- extract_netcdf_values(paste0(file2,'.nc'),
                                    model_start_time = 1981,
                                    model_end_time = 2018,
                                    whichtimes = c(months),
                                    agg_method = method,
                                    xvar_name = "lon_rho",
                                    yvar_name = "lat_rho",
                                    time_name = "ocean_time",
                                    nc_variable_name = "temp",
                                    out_variable_name = "SST"
        )
      }

      if (variable == "O2" & climate_model == "roms") {

        file1 <- 'data/roms-hindcast/bcc42_era5b37r1_mon1981to2018_botTSO'

        df <- extract_netcdf_values(paste0(file1,'.nc'),
                                    model_start_time = 1981,
                                    model_end_time = 2018,
                                    whichtimes = c(months),
                                    agg_method = method,
                                    xvar_name = "lon_rho",
                                    yvar_name = "lat_rho",
                                    time_name = "ocean_time",
                                    nc_variable_name = "Oxygen",
                                    out_variable_name = "O2"
        )
      }

      # # not using yet
      if (variable == "salinity" & climate_model == "roms") {

        file1 <- 'data/roms-hindcast/bcc42_era5b37r1_mon1981to2018_botTSO'

        df <- extract_netcdf_values(paste0(file1,'.nc'),
                                    model_start_time = 1981,
                                    model_end_time = 2018,
                                    whichtimes = c(months),
                                    agg_method = method,
                                    xvar_name = "lon_rho",
                                    yvar_name = "lat_rho",
                                    time_name = "ocean_time",
                                    nc_variable_name = "salt",
                                    out_variable_name = "salinity"
        )
      }

      # not sure why, but extract_netcdf_values is returning Inf
      df[sapply(df, is.infinite)] <- NA

      saveRDS(df, ncdatafile)
    }

    # using lindsay's grid but could be updated to include inside waters
    bccoast <- readRDS("data/predictiongrid_bccoast_scaled.rds")
    sf::st_geometry(bccoast) <- NULL
    bccoast <- bccoast %>%
      dplyr::select(-year, -geartype, -value, -offset) %>%
      distinct()

    # browser() # if we need to see what's happening?
    project_netcdf_values(
      ncdatafile,
      variable_name = ann_variable,
      set_resolution = 10,
      print_test_plot = TRUE,
      coord_df = bccoast,
      radius = 1000,
      grid_xvar_name = "UTM.lon",
      grid_yvar_name = "UTM.lat",
      grid_crs = "+proj=utm +zone=9 +datum=WGS84 +units=km"
    )

    grid <- project_netcdf_values(
      ncdatafile,
      variable_name = ann_variable,
      set_resolution = 10,
      # print_test_plot = TRUE,
      coord_df = bccoast,
      radius = 1000,
      grid_xvar_name = "UTM.lon",
      grid_yvar_name = "UTM.lat",
      grid_crs = "+proj=utm +zone=9 +datum=WGS84 +units=km"
    )
    ## could do this here; currently doing it within get_stock_envrio_var()
    # grid <- grid[!is.na(grid[ann_variable]),]
    saveRDS(grid, gridfile)
  }
}

