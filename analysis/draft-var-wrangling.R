# global bottom temp --------------------------------------

# library(ncdf4) # package for netcdf manipulation
library(tidyverse)
# library(raster) # package for raster manipulation
# library(rgdal) # package for geospatial analysis
library(sf)
# library(terra)

# two files downloaded from: https://aims2.llnl.gov/search
# though maybe also available here: https://esgf-data.dkrz.de/search/cmip6-dkrz/
# Identifier DOI: http://doi.org/10.22033/ESGF/CMIP6.2921
# Creators: Jie, Weihua; Zhang, Jie; Wu, Tongwen; et al.
# Titles: BCC BCC-CSM2HR model output prepared for CMIP6 HighResMIP hist-1950
# Publisher: Earth System Grid Federation
# Publication Year: 2020
# License: Creative Commons Attribution 4.0 International License (CC BY 4.0)

# File 1:
#   id: CMIP6.HighResMIP.BCC.BCC-CSM2-HR.hist-1950.r1i1p1f1.Omon.tob.gn.v20200922.tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_195001-197912.nc|cmip.bcc.cma.cn
# version: 1
# cf_standard_name: sea_water_potential_temperature_at_sea_floor
# checksum_type: SHA256
# dataset_id: CMIP6.HighResMIP.BCC.BCC-CSM2-HR.hist-1950.r1i1p1f1.Omon.tob.gn.v20200922|cmip.bcc.cma.cn
# instance_id: CMIP6.HighResMIP.BCC.BCC-CSM2-HR.hist-1950.r1i1p1f1.Omon.tob.gn.v20200922.tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_195001-197912.nc
# master_id: CMIP6.HighResMIP.BCC.BCC-CSM2-HR.hist-1950.r1i1p1f1.Omon.tob.gn.tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_195001-197912.nc
# timestamp: 2020-09-22T10:25:56Z
# variable: tob
# variable_id: tob
# variable_long_name: Sea Water Potential Temperature at Sea Floor
# variable_units: degC
#
#
# File 2:
#   id: CMIP6.HighResMIP.BCC.BCC-CSM2-HR.hist-1950.r1i1p1f1.Omon.tob.gn.v20200922.tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_198001-201412.nc|cmip.bcc.cma.cn
# version: 1
# cf_standard_name: sea_water_potential_temperature_at_sea_floor
# checksum_type: SHA256
# dataset_id: CMIP6.HighResMIP.BCC.BCC-CSM2-HR.hist-1950.r1i1p1f1.Omon.tob.gn.v20200922|cmip.bcc.cma.cn
# instance_id: CMIP6.HighResMIP.BCC.BCC-CSM2-HR.hist-1950.r1i1p1f1.Omon.tob.gn.v20200922.tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_198001-201412.nc
# master_id: CMIP6.HighResMIP.BCC.BCC-CSM2-HR.hist-1950.r1i1p1f1.Omon.tob.gn.tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_198001-201412.nc
# timestamp: 2020-09-22T14:58:24Z
# variable: tob
# variable_id: tob
# variable_long_name: Sea Water Potential Temperature at Sea Floor
# variable_units: degC


nc_data <- nc_open('data/bottom-temp/tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_195001-197912.nc')
# # Save the print(nc) dump to a text file
{
  sink('data/bottom-temp/tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_195001-197912.txt')
  print(nc_data)
  sink()
}

# look at text file produced to see what variables are in this file

# note that "lon" will return a short dim vector of unique values and "longitude" will return the full matrix
lon1 <- ncvar_get(nc_data, "lon")
lon2 <- ncvar_get(nc_data, "longitude") # store the data in a 3-dimensional array
lat1 <- ncvar_get(nc_data, "lat", verbose = T)
time <- ncvar_get(nc_data, "time", verbose = T)
head(lon) # look at the first few entries in the longitude vector
var.array <- ncvar_get(nc_data, "tob") # store the data in a 3-dimensional array
dim(var.array)

# All done reading in the data. We can close the netCDF file.
nc_close(nc_data)

# # Ok, everything checks out, so we can go ahead and save this data in a raster. Note that we provide the coordinate reference system “CRS” in the standard well-known text format. For this data set, it is the common WGS84 system.
#
# r <- raster(t(nc.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# # We will need to transpose and flip to orient the data correctly. The best way to figure this out is through trial and error, but remember that most netCDF files record spatial data from the bottom left corner.
#
# r <- flip(r, direction='y')
#
# plot(r)
# # If this looks good, we could save it to a GeoTIFF file.
# # writeRaster(r, "tob1950.tif", "GTiff", overwrite=TRUE)

# # get bounds of BC coast
library(tidyverse)
library(sf)

bccoast <- readRDS("data/predictiongrid_bccoast_scaled.rds") %>%
  mutate(UTM.lon = UTM.lon * 1000, UTM.lat = UTM.lat * 1000)

#Conversion of data frame to sf object
bc_utm <- st_as_sf(x = bccoast,
                  coords = c("UTM.lon", "UTM.lat"),
                  crs = '+proj=utm +zone=9 +datum=WGS84')

st_crs(bc_utm) <- '+proj=utm +zone=9 +datum=WGS84'

bc_ll <- st_transform(bc_utm, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")

st_bbox(bc_ll)



## rbrick code - may not be needed if I use Lindsay's code
# # Maybe you want to get a timeseries at a study location.
# # First, we will need to convert the entire 3d array of data to a raster brick. Note, this step may take several minutes.
#
# r_brick <- brick(nc.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# # note that you may have to play around with the transpose (the t() function) and flip() before the data are oriented correctly. In this example, the netcdf file recorded latitude on the X and longitude on the Y, so both a transpose and a flip in the y direction were required.
# r_brick <- flip(t(r_brick), direction='y')
#
# res(r_brick) <- 4000
#
# # Extract timeseries of data from the raster brick using the ‘extract()’ function.
#
# toolik_lon <- -149.5975
# toolik_lat <- 68.6275
# toolik_series <- extract(r_brick, SpatialPoints(cbind(toolik_lon,toolik_lat)), method='simple')
# # This timeseries is in a simple vector indexed only by the raster layer ID, so let’s put it in an easier-to-use dataframe form and then plot the timeseries.
#
# toolik_df <- data.frame(year= seq(from=1982, to=2012, by=1), NDVI=t(toolik_series))
# ggplot(data=toolik_df, aes(x=year, y=NDVI, group=1)) +
#   geom_line() + # make this a line plot
#   ggtitle("Growing season NDVI at Toolik Lake Station") +     # Set title
#   theme_bw() # use the black and white theme
#


#default is annual mean "tob" (temp on bottom)
df1 <- extract_netcdf_values('data/bottom-temp/tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_195001-197912.nc',
                            model_start_time = 1950,
                            model_end_time = 1979)

df2 <- extract_netcdf_values('data/bottom-temp/tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_198001-201412.nc',
                            model_start_time = 1980,
                            model_end_time = 2014)

df <- left_join(df1, df2)

saveRDS(df, "data/annual_mean_tob_BC.rds")


# saved overall mean for whole world, but function now saved smaller file for just bc coast
# df <- readRDS("data/annual_mean_tob.rds")
df <- readRDS("data/annual_mean_tob_bc.rds")

# Project netcdf data to a set of coordinates -------------------------

bccoast <- readRDS("data/predictiongrid_bccoast_scaled.rds") %>%
  mutate(UTM.lon = UTM.lon * 1000, UTM.lat = UTM.lat * 1000)
sf::st_geometry(bccoast) <- NULL
bccoast <- bccoast %>% dplyr::select(-year, -geartype, -value, -offset) %>% distinct()


# rds.file <- "data/tob-monthly-1950-1979-max-112.rds"
# var_crs <- "+proj=longlat +datum=WGS84"
# grid_crs <- '+proj=utm +zone=9 +datum=WGS84'
# set_resolution <- 10000
rds.file <- "data/annual_mean_tob_bc.rds"

# test plot with radius set
grid <- project_netcdf_values(rds.file,
                      variable_name = "ann_mean",
                      # print_test_plot = TRUE,
                      coord_df = bccoast,
                      radius = 1000,
                      grid_xvar_name = "UTM.lon",
                      grid_yvar_name = "UTM.lat",
                      grid_crs = '+proj=utm +zone=9 +datum=WGS84')


saveRDS(grid, "data/grid_w_annual_mean_tob_bc.rds")


# # test that is works with only coords
# bc <- bccoast %>% dplyr::select("UTM.lon", "UTM.lat") %>% distinct()
#
# grid <- project_netcdf_values(rds.file,
#                               variable_name = "ann_mean",
#                               coord_df = bc,
#                               grid_xvar_name = "UTM.lon",
#                               grid_yvar_name = "UTM.lat",
#                               grid_crs = '+proj=utm +zone=9 +datum=WGS84')
#



grid <- readRDS("data/grid_w_annual_mean_tob_bc.rds")

grid <- grid %>% mutate(depth = posdepth)


df <- get_stock_enviro_vars(  temporal_grid = grid,
                              variable_name = "ann_max",
                              species = "NA",
                              stock = "NA",
                              recruitment_age = 1,
                              depth_range = c(0, 200),
                              # lon_range = NULL,
                              # lat_range = c(48,51.2),
                              lon_var_name = "UTM.lon",
                              lat_var_name = "UTM.lat",
                              polygon = NULL,
                              bbox = NULL)



grid <- grid %>% mutate(depth = posdepth)


# # Test with a shape file
shape <- sf::st_read("../../ye-wcvi/grids/HBLL-N-S/PHMA_S_GRID.shp")
#
# # type <- st_geometry_type(shape, by_geometry = FALSE)
# # type[1]=="POLYGON"
#
d <-  project_netcdf_values(rds.file,
                            variable_name = "ann_mean",
                            # print_test_plot = TRUE,
                            coord_df = shape,
                            grid_xvar_name = NULL,
                            grid_yvar_name = NULL,
                            grid_crs = '+proj=utm +zone=9 +datum=WGS84')

grid <- d %>% mutate(depth = -DEPTH_M)
# st_geometry(grid) <- NULL
df <- get_stock_enviro_vars(  temporal_grid = grid,
                              variable_name = "ann_mean",
                              species = "NA",
                              stock = "NA",
                              recruitment_age = 1,
                              depth_range = c(0, 200),
                              lon_range = NULL,
                              lat_range = c(48,51.2),
                              lon_var_name = "LONGITUDE",
                              lat_var_name = "LATITUDE"
)
