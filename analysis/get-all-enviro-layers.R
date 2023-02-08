# get all climate vars
devtools::load_all(".")
library(ncdf4) # package for netcdf manipulation
library(tidyverse)
library(sf)
library(terra)

rmonths <- readxl::read_xlsx(
  "data/GF_assessments.xlsx" # , col_type = "list"
) %>%
  filter(Outputs == "Y") %>%
  select(R_months) %>%
  distinct() %>%
  na.omit()

all_layers <- expand.grid(method = c("mean", "min", "max"), months = rmonths$R_months)
variable <- "tob"




# unlist(strsplit(as.character(all_layers$months[1]), ","))

for (i in 1:nrow(all_layers)) {
  months <- as.numeric(unlist(strsplit(as.character(all_layers$months[i]), ",")))
# can manually set a range of months not included in that vector, but right now will only get included for a species if it's in that R_months vector or the full year
  # months <- c(1:12)
  method <- all_layers$method[i]

  month_string <- paste0(months[1], "to", max(months))
  if (month_string == "1to12") {
    month_string <- "ann"
  }
  ann_variable <- paste0(variable, "_", month_string, "_", method)


  gridfile <- paste0("data/grid_", ann_variable, ".rds")

  if (!file.exists(gridfile)) {

    ncdatafile <- paste0("data/", variable, "-", month_string, "-", method, ".rds")

    if (!file.exists(ncdatafile)) {

      if (variable == "tob") {
      df1 <- extract_netcdf_values(
        "data/bottom-temp/tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_195001-197912.nc",
        variable_name = variable,
        whichtimes = months,
        agg_method = method,
        model_start_time = 1950,
        model_end_time = 1979
      )

      df2 <- extract_netcdf_values(
        "data/bottom-temp/tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_198001-201412.nc",
        variable_name = variable,
        whichtimes = months,
        agg_method = method,
        model_start_time = 1980,
        model_end_time = 2014
      )

      df <- left_join(df1, df2)
      }


      if (variable == "SST") {

        stop( "TODO: Need to get this layer still." )
        # df1 <- extract_netcdf_values(
        #   ####,
        #   variable_name = variable,
        #   whichtimes = months,
        #   agg_method = method,
        #   model_start_time = ###,
        #   model_end_time = ###
        # )
        #
        # df2 <- extract_netcdf_values(
        #   ####,
        #   variable_name = variable,
        #   whichtimes = months,
        #   agg_method = method,
        #   model_start_time = ###,
        #   model_end_time = ###
        # )
        #
        # df <- left_join(df1, df2)
      }


      saveRDS(df, ncdatafile)
    }

    # using lindsay's grid but could be updated to include inside waters
    bccoast <- readRDS("data/predictiongrid_bccoast_scaled.rds")
    sf::st_geometry(bccoast) <- NULL
    bccoast <- bccoast %>%
      dplyr::select(-year, -geartype, -value, -offset) %>%
      distinct()

    # browser() # if we need to see what's happening?
    # project_netcdf_values(
    #   ncdatafile,
    #   variable_name = ann_variable,
    #   set_resolution = 10,
    #   print_test_plot = TRUE,
    #   coord_df = bccoast,
    #   radius = 1000,
    #   grid_xvar_name = "UTM.lon",
    #   grid_yvar_name = "UTM.lat",
    #   grid_crs = "+proj=utm +zone=9 +datum=WGS84 +units=km"
    # )

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


    saveRDS(grid, gridfile)
  }
}
