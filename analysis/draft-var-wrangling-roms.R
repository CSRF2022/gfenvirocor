# global bottom temp --------------------------------------
devtools::load_all(".")
library(tidyverse)
# library(raster) # package for raster manipulation
# library(rgdal) # package for geospatial analysis
library(sf)
# library(terra)

file <- 'data/roms-hindcast/bcc42_era5b37r1_mon1981to2018_surTSO'

file <- 'data/roms-hindcast/bcc42_era5b37r1_mon1981to2018_botTSO'



library(ncdf4) # package for netcdf manipulation
nc_data <- nc_open(paste0(file,'.nc'))
# # Save the print(nc) dump to a text file
{
  sink(paste0(file,'.txt'))
  print(nc_data)
  sink()
}

# look at text file produced to see what variables are in this file
# File data/roms-hindcast/bcc42_era5b37r1_mon1981to2018_botTSO.nc (NC_FORMAT_CLASSIC):
#
#   6 variables (excluding dimension variables):
# double lon_rho[xi_rho,eta_rho]
# double lat_rho[xi_rho,eta_rho]
# double mask_rho[xi_rho,eta_rho]
# double temp[xi_rho,eta_rho,ocean_time]
# double salt[xi_rho,eta_rho,ocean_time]
# double Oxygen[xi_rho,eta_rho,ocean_time]
#
# 3 dimensions:
# ocean_time  Size:456
# xi_rho  Size:236 (no dimvar)
# eta_rho  Size:410 (no dimvar)
# All files have 3 variables: temp (temperature in oC), salt (salinity) and Oxygen (dissolved oxygen in mmol-oxygen m-3).


# note that "lon" will return a short dim vector of unique values and "longitude" will return the full matrix
lon1 <- ncvar_get(nc_data, "lon_rho", verbose = T) # stored in a large matrix
lat1 <- ncvar_get(nc_data, "lat_rho", verbose = T)
mask_rho <- ncvar_get(nc_data, "mask_rho", verbose = T) # mask_rho (a mask for model grids that are on land=0 and ocean=1)

time <- ncvar_get(nc_data, "ocean_time", verbose = T) #vector of seconds from midnight 1st Jan 1970 in Andy's data

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

# bccoast <- readRDS("data/predictiongrid_bccoast_scaled.rds")
#
# #Conversion of data frame to sf object
# bc_utm <- st_as_sf(x = bccoast,
#                   coords = c("UTM.lon", "UTM.lat"),
#                   crs = '+proj=utm +zone=9 +datum=WGS84 +units=km')
#
# st_crs(bc_utm) <- '+proj=utm +zone=9 +datum=WGS84 +units=km'
#
# # this next step is currently broken and not sure why
# bc_ll <- st_transform(bc_utm, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+  towgs84=0,0,0")
#
# st_bbox(bc_ll)
#


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

df <- extract_netcdf_values(paste0(file,'.nc'),
                            model_start_time = 1981,
                            model_end_time = 2018,
                            xvar_name = "lon_rho",
                            yvar_name = "lat_rho",
                            time_name = "ocean_time",
                            nc_variable_name = "temp",
                            # out_variable_name = "sst"
                            out_variable_name = "roms-tob"
                            )

# saveRDS(df, "data/annual_mean_sst_BC.rds")

# df <- readRDS("data/annual_mean_sst_BC.rds")

# file <- 'data/roms-scenarios/bcc42_bioNew_mon1981to2010_botTSO'
# file <- 'data/roms-scenarios/bcc42_bioNew_can85_mon2041to2070_botTSO'
file <- 'data/roms-scenarios/bcc42_bioNew_can85_mon2041to2070_surTSO'

df_rcp <- extract_netcdf_values(paste0(file,'.nc'),
                            model_start_time = 2041,
                            model_end_time = 2041,
                            xvar_name = "lon_rho",
                            yvar_name = "lat_rho",
                            time_name = "ocean_time",
                            # whichtimes = c(months),
                            # agg_method = "max",
                            agg_method = "min",
                            # nc_variable_name = "temp",
                            # # out_variable_name = "sst"
                            # out_variable_name = "SST-RCP8.5"
                            nc_variable_name = "Oxygen",
                            out_variable_name = "O2-RCP8.5"
)


rds.file <- "data/SST-RCP8.5-monthly-2041-2041-mean-1-12.rds"
rds.file <- "data/SST-RCP8.5-monthly-2041-2041-max-1-12.rds"


rds.file <- "data/O2-RCP8.5-monthly-2041-2041-min-1-12.rds"
saveRDS(df_rcp, rds.file)


# Project netcdf data to a set of coordinates -------------------------

bccoast <- readRDS("data/predictiongrid_bccoast_scaled.rds")
sf::st_geometry(bccoast) <- NULL
bccoast <- bccoast %>% dplyr::select(-year, -geartype, -value, -offset) %>% distinct()


# bccoast <- readRDS("data/prediction_grid_coastaltl_new.rds") %>% filter(survey_name == "syn bc")
# ggplot(bccoast ) + geom_point(aes(longitude, latitude)) +
#   gfplot::theme_pbs()
# # unique(bccoast$survey_name)
# sf::st_geometry(bccoast) <- NULL
# bccoast <- bccoast %>% dplyr::select(-event_id, -survey_name) %>% distinct()

# rds.file <- "data/tob-monthly-1950-1979-max-112.rds"
# var_crs <- "+proj=longlat +datum=WGS84"
# grid_crs <- '+proj=utm +zone=9 +datum=WGS84'
# set_resolution <- 10000
# rds.file <- "data/annual_mean_tob_bc.rds"

rds.file <- "data/sst-monthly-1981-2018-mean-1-12.rds"
rds.file <- "data/roms-tob-monthly-1981-2018-mean-1-12.rds"

#simple example
project_netcdf_values(
  rds.file,
  variable_name = "ann_mean",
  print_test_plot = TRUE,
  coord_df = sdmTMB::qcs_grid,
  grid_xvar_name = "X",
  grid_yvar_name = "Y"
)


# test plot with radius set
grid <- project_netcdf_values(rds.file,
                      variable_name = "roms_ann_mean",
                      set_resolution = 10,
                      # print_test_plot = TRUE,
                      coord_df = bccoast,
                      radius = 1000,
                      grid_xvar_name = "UTM.lon",
                      grid_yvar_name = "UTM.lat",
                      grid_crs = '+proj=utm +zone=9 +datum=WGS84 +units=km')


saveRDS(grid, "data/grid_w_annual_mean_roms_tob_bc.rds")
saveRDS(grid, "data/grid_w_annual_mean_sst_bc.rds")

glimpse(grid)


grid<- readRDS("data/grid_w_annual_mean_sst_bc.rds") %>%
  filter(year %in% c(1981, 1990, 2000,2010), !is.na(roms_ann_mean))

grid  %>% filter(!is.na(roms_ann_mean)) %>%
ggplot() + geom_tile(aes(X, Y, fill = roms_ann_mean)) +
  scale_fill_viridis_c(name= "Monthly\nmax \nSST", option = "B") +
  # scale_fill_viridis_c(name= "Monthly\nmin \nO2") +
  facet_wrap(~year) +
  coord_fixed() +
  ggtitle("RCP 8.5 2041-2070") +
  gfplot::theme_pbs() +
  theme(
    legend.position = c(0.2,0.2),
    strip.text.x = element_blank(),
    axis.title = element_blank(), axis.text = element_blank())

ggsave("figs/map-SST-RCP8.5-monthly-2041-2041-max-1-12.png", width = 6, height = 6)
ggsave("figs/map-O2-RCP8.5-monthly-2041-2041-min-1-12.png", width = 6, height = 6)



grid1<- readRDS("data/grid_tob_ann_mean.rds") %>%
  filter(year %in% c(1981, 1990, 2000,2010), !is.na(tob_ann_mean))

grid1  %>%
  ggplot() + geom_tile(aes(X, Y, colour = tob_ann_mean)) +
  scale_colour_viridis_c() +
  facet_wrap(~year) + gfplot::theme_pbs()

grid2 <- left_join(grid, grid1)

ggplot(grid2) + geom_point(aes(roms_ann_mean, tob_ann_mean, colour = posdepth)) +
  facet_wrap(~year)

# simple example


load(file = "data/wcvi_grid.rda")

wcvi_mean_tob <- project_netcdf_values(
  rds.file,
  variable_name = "ann_mean",
  # print_test_plot = TRUE,
  coord_df = wcvi_grid,
  grid_xvar_name = "X",
  grid_yvar_name = "Y"
)

df <- get_stock_enviro_var(
  temporal_grid = wcvi_mean_tob,
  variable_name = "ann_mean",
  species = "yelloweye rockfish",
  stock = "WCVI",
  recruitment_age = 1,
  depth_range = c(0, 200),
  lon_range = NULL,
  lat_range = c(5300, 5600),
  lon_var_name = "X",
  lat_var_name = "Y"
)


# # test that is works with only coords
# bc <- bccoast %>% dplyr::select("UTM.lon", "UTM.lat") %>% distinct()
#
# grid <- project_netcdf_values(rds.file,
#                               variable_name = "ann_mean",
#                               coord_df = bc,
#                               grid_xvar_name = "UTM.lon",
#                               grid_yvar_name = "UTM.lat",
#                               grid_crs = '+proj=utm +zone=9 +datum=WGS84')


grid <- readRDS("data/grid_w_annual_mean_tob_bc.rds")

grid <- grid %>% mutate(depth = posdepth)





df <- get_stock_enviro_var(  temporal_grid = grid,
                              variable_name = "ann_mean",
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
df <- get_stock_enviro_var(  temporal_grid = grid,
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






# ## get major area from PBSmapping
#
# library(gfplot)
library(PBSmapping) # needs this for some reason
#
load_boundaries <- function(utm_zone) {
  data("major", package = "PBSdata", envir = environment())
  gfplot:::ll2utm(major, utm_zone = utm_zone)
}
# major_labels <- gfplot:::boundary_labels(9, xmin = 122)
# majorbound <- load_boundaries(9)
#
# majorbounds <- fortify(majorbound)
#
# ggplot() + geom_polygon(
#   data = majorbounds,
#   aes(X , Y , group = PID, fill = as.factor(PID)),
#   lty = 1
# ) + geom_text(data = major_labels,
#               aes(X , Y, label = label), colour = "grey97"
# )
#


#
# library(sp)
Area_3cd <- majorbound %>% filter(PID %in% c(3,4))

Area_3cd <- fortify(Area_3cd)
Area_3cd <- cbind(Area_3cd$X, Area_3cd$Y)
Area_3cd <- Polygon(Area_3cd)
Area_3cd <- Polygons(list(Area_3cd),1)
Area_3cd <- SpatialPolygons(list(Area_3cd))
# plot(Area_3cd)
# proj4string(Area_3cd) <- CRS('+proj=utm +zone=9 +datum=WGS84 +units=km')
#
# ## might also be:
# # "+proj=utm +zone=9 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0  +units=m +no_defs" # i believe this is 3156
# # crsnum <- 3156
#
#
data <- data.frame(f=99.9)
Area_3cd <- SpatialPolygonsDataFrame(Area_3cd,data)
# Area_3cd
#
# spplot(Area_3cd)


# # Test with a shape file
Area_3cd <- sf::st_read("../BC_map/Shapes/majorOutline.shp") %>% filter(Name %in% c("3C", "3D"))


df6 <- get_stock_enviro_var(  temporal_grid = grid,
                              variable_name = "ann_mean",
                              species = "NA",
                              stock = "NA",
                              lat_range = c(48, 51.2),
                              lat_var_name = "LATITUDE",
                              recruitment_age = 3,
                              depth_range = c(0, 200),
                              polygon = Area_3cd
)



# test run code used in the above
grid <- grid %>% mutate(x = UTM.lon/1000, y = UTM.lat/1000) #%>% filter(year== "1950")

# grid1 <- grid %>% mutate(x = UTM.lon/1000, y = UTM.lat/1000) %>% filter(year== "1950")
gridsf <- st_as_sf(grid, coords = c("x", "y"), crs = '+proj=utm +zone=9 +datum=WGS84 +units=km' )
# plot(gridsf)


Area_3cd2 <- st_as_sfc(Area_3cd)
Area_3cd2 <- st_as_sfc(Area_3cd, crs = '+proj=utm +zone=9 +datum=WGS84 +units=km')

plot(Area_3cd)

## none of this is working yet!
# keep <- st_intersects(Area_3cd, gridsf)
# grid1 <- gridsf[unlist(keep)]
#
# new_grid <- st_as_sf(grid1)

cells_in_area <- st_intersection(gridsf, Area_3cd)
# area_in_grid <- st_intersection(Area_3cd, gridsf)
#
# #not sure why, but st_union required otherwise df becomes huge!
# new_grid1 <- st_difference(gridsf, st_union(area_in_grid))
# new_grid2 <- st_difference(st_union(area_in_grid), gridsf)
# new_grid3 <- new_grid1 %>% filter(year == "1950")
# ggplot(new_grid3) + geom_sf()
ggplot(cells_in_area) + geom_sf()
