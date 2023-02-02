#' Get aggregated netcdf values for specific locations or grid cells
#'
#' @param rds.file
#' @param variable_name
#' @param var_xvar_name
#' @param var_yvar_name
#' @param var_crs
#' @param set_resolution
#' @param spatial_grid
#' @param grid_xvar_name
#' @param grid_yvar_name
#' @param grid_crs
#'
#' @return
#' @export
#'
#' @examples
#'
project_netcdf_values <- function (
  rds.file,
  variable_name,
  var_xvar_name = "X",
  var_yvar_name = "Y",
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

  spdf <- readRDS(rds.file) %>%  mutate(X = .data[[var_xvar_name]], Y = .data[[var_yvar_name]])

  # min(unique(spdf$longitude))
  # df <- df[,!names(df) %in% c("X","Y")]
  coordinates(spdf) <- ~X+Y
  proj4string(spdf) <- CRS(var_crs)
  # Create a base raster with with ROMS lat/lon extent
  b <- rast(xmin=min(unique(spdf$X)), xmax=max(unique(spdf$X)),
            ymin=min(unique(spdf$Y)), ymax=max(unique(spdf$Y)),
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
  # ggplot(df) + geom_point( aes(X, Y, col = z)) + scale_colour_viridis_c()
  # browser()

  # grid <- spatial_grid
  # spatial_grid$X <- spatial_grid[grid_xvar_name]
  # spatial_grid$Y <- spatial_grid[grid_yvar_name]


  # this works for spatial points, should probably add buffer though and ultimately an option to use polygons
  grid <- spatial_grid %>% mutate(X = .data[[grid_xvar_name]], Y = .data[[grid_yvar_name]])

  #get just coordinates
  coords <- grid %>% dplyr::select(X, Y) %>% distinct()

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
