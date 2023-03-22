# draft wrangle of projections
devtools::load_all(".")
library(tidyverse)
# library(raster) # package for raster manipulation
# library(rgdal) # package for geospatial analysis
library(sf)
# library(terra)
library(ncdf4)

# Project netcdf data to a set of coordinates -------------------------

bccoast <- readRDS("data/predictiongrid_bccoast_scaled.rds")
sf::st_geometry(bccoast) <- NULL
bccoast <- bccoast %>% dplyr::select(-year, -geartype, -value, -offset) %>% distinct()

file1 <- 'data/roms-scenarios/bcc42_bioNew_mon1981to2010_botTSO'
file2 <- 'data/roms-scenarios/bcc42_bioNew_can85_mon2041to2070_botTSO'
# file2 <- 'data/roms-scenarios/bcc42_bioNew_can85_mon2041to2070_surTSO'

calc_rcp_diff <- function(
  file1,
  file2,
  grid = bccoast,
  agg_type = "min",
  months = c(1:12),
  nc_var = "Oxygen",
  out_var_name = "O2-RCP8.5",
  var_label = "Monthly\nmin \nO2",
  viridis_option = "D"
  # nc_var = "temp",
  # out_var_name = "SST-RCP8.5",
  # var_label = "Monthly\nmax \nSST"
  # viridis_option = "B"
) {

df_past <- extract_netcdf_values(paste0(file1,'.nc'),
                                model_start_time = 1981,
                                model_end_time = 1981,
                                xvar_name = "lon_rho",
                                yvar_name = "lat_rho",
                                time_name = "ocean_time",
                                whichtimes = months,
                                # agg_method = "max",
                                agg_method =agg_type,
                                # nc_variable_name = "temp",
                                # # out_variable_name = "sst"
                                # out_variable_name = "SST-RCP8.5"
                                nc_variable_name = nc_var,
                                out_variable_name = out_var_name
)


df_rcp <- extract_netcdf_values(paste0(file2,'.nc'),
                                model_start_time = 2041,
                                model_end_time = 2041,
                                xvar_name = "lon_rho",
                                yvar_name = "lat_rho",
                                time_name = "ocean_time",
                                whichtimes = months,
                                # agg_method = "max",
                                agg_method = agg_type,
                                # nc_variable_name = "temp",
                                # # out_variable_name = "sst"
                                # out_variable_name = "SST-RCP8.5"
                                nc_variable_name = nc_var,
                                out_variable_name = out_var_name
)


df_rcp <- left_join(df_past, df_rcp)

saveRDS(df_rcp, "data/RCP-temp.rds")

df_grid <- project_netcdf_values("data/RCP-temp.rds",
                              variable_name = out_var_name,
                              set_resolution = 10,
                              # print_test_plot = TRUE,
                              coord_df = bccoast,
                              radius = 1000,
                              grid_xvar_name = "UTM.lon",
                              grid_yvar_name = "UTM.lat",
                              grid_crs = '+proj=utm +zone=9 +datum=WGS84 +units=km')

# browser()
df_grid  %>% filter(!is.na(!!sym(out_var_name))) %>%
  mutate(year = ifelse(year == "1981", "1981-2010", ifelse(year == "2041", "2041-2070", year))) %>%
  ggplot() + geom_tile(aes(X, Y, fill = !!sym(out_var_name))) +
  scale_fill_viridis_c(name= var_label, option = viridis_option) +
  facet_wrap(~year) +
  coord_fixed() +
  gfplot::theme_pbs() +
  theme(
    legend.position = c(0.1,0.3),
    # strip.text.x = element_blank(),
    axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())


}

calc_rcp_diff(file1, file2, months = c(1:4)) + ggtitle("RCP 8.5")
# ggsave("figs/map-O2-RCP8.5-monthly-min-1-12.png", width = 8, height = 5)
ggsave("figs/map-O2-RCP8.5-monthly-min-1-4.png", width = 6, height = 4)

calc_rcp_diff(file1, file2,
              nc_var = "temp",
              out_var_name = "SST-RCP8.5",
              var_label = "Monthly\nmax \nSST",
              viridis_option = "B"
              ) + ggtitle("RCP 8.5")

ggsave("figs/map-SST-RCP8.5-monthly-max-1-12.png", width = 6, height = 4)
