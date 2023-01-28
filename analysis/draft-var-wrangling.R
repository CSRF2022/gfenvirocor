# bottom temp

library(ncdf4) # package for netcdf manipulation
library(tidyverse)
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(sf)
library(terra)

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



# Get averages from certain months within the netcdf -------------------------------------------------------

extract_annual_values <- function(
  nc.file = 'data/bottom-temp/tob_Omon_BCC-CSM2-HR_hist-1950_r1i1p1f1_gn_195001-197912.nc',
  output.dir = "data",
  xvar_name = "longitude",
  yvar_name = "latitude",
  time_name = "time",
  time_resolution = "monthly",
  variable_name = "tob",
  model_start_year = 1950,
  model_end_year = 1979,
  xbounds = c(-133.85254, -123.21362), # default all BC coast
  ybounds = c(48.21968, 55.68620), # default all BC coast
  whichmonths = c(1:12), # default all for annual mean
  type = "mean"
  ) {

  nc_data <- nc_open(nc.file)
  lon <- ncvar_get(nc_data, xvar_name)
  lat <- ncvar_get(nc_data, yvar_name)

  # browser()

  t <- ncvar_get(nc_data, time_name)
  nc.array <- ncvar_get(nc_data, variable_name) # store the data in a 3-dimensional array
  # dim(nc.array)

  # see what fill value was used for missing data.
  fillvalue <- ncatt_get(nc_data, variable_name, "_FillValue")
  # fillvalue # The fill value is 1e+20.

  # All done reading in the data. We can close the netCDF file.
  nc_close(nc_data)

  # Let’s replace all those pesky fill values with the R-standard ‘NA’.
  nc.array[nc.array == fillvalue$value] <- NA


  if(time_resolution == "monthly") {
  # create df of months and years to go with the sequential time variable in the array
  month <- rep(1:12, (model_end_year - model_start_year + 1))
  year <- rep(model_start_year:model_end_year, each = 12)
  timedf <- as.data.frame(cbind(t, months = month, year = year))
  timedf$layer_seq <- seq(1, nrow(timedf), 1)
  selected_months <- dplyr::filter(timedf, month %in% whichmonths)

  years <- as.numeric(seq(model_start_year, model_end_year, 1))

  df <- data.frame(longitude=c(lon), latitude=c(lat))

  for (i in 1:length(years) ){

  yearsel <- years[i]
  xx <- selected_months %>% dplyr::filter(year == yearsel) %>% dplyr::select(layer_seq)
  selection <- nc.array[,,xx[1:5,]]

  if(type == "mean") {
    value <- apply(selection, MARGIN=c(1, 2), mean) #margin, rows and columns
  }

  if(type == "max") {
    value <- apply(selection, MARGIN=c(1, 2), max) #margin, rows and columns
  }

  if(type == "min") {
    value <- apply(selection, MARGIN=c(1, 2), min) #margin, rows and columns
  }

  df[[as.character(yearsel)]] <- c(value[,]) # most be character to name columns
  }
  } else{
    stop("Time resolutions other than monthly are not supported yet.")
  }

  if(!is.null(xbounds)){
  df <- df %>%
    filter(
      .data[[xvar_name]] > xbounds[1],
      .data[[xvar_name]] < xbounds[2]
    )
  }

  if(!is.null(ybounds)){
    df <- df %>%
      filter(
        .data[[yvar_name]] > ybounds[1],
        .data[[yvar_name]] < ybounds[2]
      )
  }

  saveRDS(df, paste0(output.dir, "/",
                     variable_name, "-",
                     time_resolution, "-",
                     model_start_year, "-", model_end_year, "-",
                     type, "-",
                     whichmonths[1],
                     whichmonths[max(whichmonths)], ".rds"))
  return(df)
}

df <- extract_annual_values()
df <- extract_annual_values(type = "max")


# saved overall mean for whole world, but function now saved smaller file for just bc coast
# df <- readRDS("data/annual_mean_tob.rds")


# Create raster from ROMs-------------------------


bccoast <- readRDS("data/predictiongrid_bccoast_scaled.rds") %>%
  mutate(UTM.lon = UTM.lon * 1000, UTM.lat = UTM.lat * 1000)
sf::st_geometry(bccoast) <- NULL
bccoast <- bccoast %>% dplyr::select(-year, -geartype, -value, -offset) %>% distinct()


# set_resolution <- 10000
# model_start_year <- 1950
# model_end_year <- 1979
#
# rds.file <- "data/tob-monthly-1950-1979-max-112.rds"
#   var_xvar_name <- "longitude"
# var_yvar_name <- "latitude"
# var_crs <- "+proj=longlat +datum=WGS84"
# grid_crs <- '+proj=utm +zone=9 +datum=WGS84'


project_annual_values <- function (
  rds.file = "data/tob-monthly-1950-1979-max-112.rds",
  variable_name = "ann_max",
  var_xvar_name = "longitude",
  var_yvar_name = "latitude",
  var_crs = "+proj=longlat +datum=WGS84",
  set_resolution = 10000,
  spatial_grid = bccoast,
  grid_xvar_name = "UTM.lon",
  grid_yvar_name = "UTM.lat",
  grid_crs = '+proj=utm +zone=9 +datum=WGS84'
) {

# browser()
#get rid of the nas
#df <- df %>% drop_na()

spdf <- readRDS(rds.file) %>%  mutate(longitude = .data[[var_xvar_name]], latitude = .data[[var_yvar_name]])

# min(unique(spdf$longitude))
# df <- df[,!names(df) %in% c("longitude","latitude")]
coordinates(spdf) <- ~longitude+latitude
proj4string(spdf) <- CRS(var_crs)
# Create a base raster with with ROMS lat/lon extent
b <- rast(xmin=min(unique(spdf$longitude)), xmax=max(unique(spdf$longitude)),
          ymin=min(unique(spdf$latitude)), ymax=max(unique(spdf$latitude)),
          crs=var_crs)
class(b)

# project
r <- terra::project(b, grid_crs)
spdf <- spTransform(spdf, grid_crs)

# # set resolution to 3 km (the resolution of the ROMs grid), make this 4km so I have less NAs however this should be 3 km and interpolate data
res(r) <- set_resolution
# plot(r)

#Project values onto a 10km raster (values are a mean of surrounding cells)
# xx <- rasterize(x=vect(spdf), y=r, field=names(spdf)[1], na.rm=F) # will loose a couple periperal cells
xx <- rasterize(x=vect(spdf), y=r, field=names(spdf)[1], na.rm=T)
# plot(xx)
mx <- matrix(1, nc=3, nr=3)
# mx[2,1] <- NA  # will loose a couple periperal cells
xf <- terra::focal(xx, w=mx, fun=mean, na.policy="only", na.rm=T)
plot(xf)

for (i in (2:length(names(spdf)))){
  xx <- rasterize(x=vect(spdf), y=r, field=names(spdf)[i], na.rm=T)
  xf2 <- terra::focal(xx, w=mx, fun=mean, na.policy="only", na.rm=T)
  xf <- c(xf, xf2)
}

names(xf) <- names(spdf)

# Overlay rasters with prediction points ----------------------------------
r <- xf


## test output
# xy <- raster::extract(r[[1]], df_utm, weights = FALSE, fun = mean, na.rm = T)
# df <- data.frame(cbind(z = xy[,2], df_utm))
# ggplot(df) + geom_point( aes(longitude, latitude, col = z)) + scale_colour_viridis_c()
# browser()

# grid <- spatial_grid
# spatial_grid$longitude <- spatial_grid[grid_xvar_name]
# spatial_grid$latitude <- spatial_grid[grid_yvar_name]

grid <- spatial_grid %>% mutate(longitude = .data[[grid_xvar_name]], latitude = .data[[grid_yvar_name]])

#get just coordinates
coords <- grid %>% dplyr::select(longitude, latitude) %>% distinct()

# coords to append to output
out <- coords

for (i in seq_along(1:length(names(spdf)))){
  xy <- raster::extract(r[[i]], coords, weights = FALSE, fun = mean)
  df <- data.frame(cbind(z = xy[,2], coords))
  colnames(df)[1] <- names(r[[i]])
  out <- df %>% dplyr::select(colnames(df)[1]) %>% bind_cols(out, .)
  out
}
#
# browser()
# glimpse(out)

out <- pivot_longer(out, 3:ncol(out), names_to = "year", values_to = variable_name)
grid <- left_join(grid, out)
grid
}

grid <- project_annual_values()

saveRDS(grid, "data/ann_max_grid.rds")


# # st_crs(grid) <- CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
#
# wcvi <- gfplot::synoptic_grid %>% filter(survey == "SYN WCVI") #%>% View()
#
# bbox <- as(raster::extent((min(wcvi$X*1000)-10000), max(wcvi$X*1000) + 10000, min(wcvi$Y*1000)-10000, max(wcvi$Y*1000) + 10000 ), "SpatialLines")
# proj4string(bbox) <- CRS("+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
# bboxsf <- st_as_sf(bbox)
#

library(tidyverse)

grid <- readRDS( "data/ann_max_grid.rds")

grid <- grid %>% mutate(depth = posdepth)

stock_specific_values <- function(
  temporal_grid = grid,
  variable_name = "ann_max",
  species = "NA",
  recruitment_age = 1,
  # bbox = NULL,
  # lon_range = NULL,
  # lat_range = NULL,
  depth_range = c(0, 200)
  ){

  dat <-  dplyr::filter(temporal_grid, depth >= depth_range[1] & depth >= depth_range[2])
  dat <- dat %>% dplyr::group_by(year) %>%
    dplyr::summarise(var = mean(.data[[variable_name]], na.rm = T)
    )

  dat <- dat %>% dplyr::arrange(year)


  if (recruitment_age == 1) {
    dat <- dat %>% mutate(var_lag1 = lag(var, 1),
                          var_lag2 = lag(var, 2))
  }

  if (recruitment_age == 2) {
    dat <- dat %>% mutate(var_lag1 = lag(var, 1),
                          var_lag2 = lag(var, 2),
                          var_lag3 = lag(var, 3))
  }

  if (recruitment_age == 3) {
    dat <- dat %>% mutate(var_lag1 = lag(var, 1),
                          var_lag2 = lag(var, 2),
                          var_lag3 = lag(var, 3),
                          var_lag4 = lag(var, 4))
  }
names(dat) <- gsub("var", variable_name, names(dat))
dat$species <- species
dat
}


df <- stock_specific_values()
