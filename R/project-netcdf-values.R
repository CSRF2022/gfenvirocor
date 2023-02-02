#' Get aggregated netcdf values for specific locations or grid cells
#'
#' @param rds.file File containing output from `extract-netcdf()` function
#' @param variable_name What variable does the above file contain
#' @param set_resolution Choose a resolution in m for the projected raster
#' @param print_test_plot If `TRUE` will return a plot of how the resolution looks
#' @param var_xvar_name What is the x variable from the rds called?
#' @param var_yvar_name What is the Y variable from the rds called?
#' @param var_crs What CRS projection was the netcdf file in?
#' @param spatial_grid The grid to extract values for. Can be spatial object, or have x and y columns.
#' @param grid_xvar_name If a spatial object, use `NULL`, otherwise give name of x variable in the grid.
#' @param grid_yvar_name If a spatial object, use `NULL`, otherwise give name of y variable in the grid.
#' @param grid_crs What CRS projection is the grid in?
#'
#' @return
#' The grid expanded to include all years and the netcdf derived variables.
#'
#' @export
#'
#' @examples
#' project_netcdf_values(rds.file,
#'   variable_name = "ann_mean",
#'   print_test_plot = TRUE,
#'   spatial_grid = bccoast,
#'   grid_xvar_name = "UTM.lon",
#'   grid_yvar_name = "UTM.lat"
#' )
#'
project_netcdf_values <- function(rds.file,
                                  variable_name,
                                  set_resolution = 10000,
                                  print_test_plot = FALSE,
                                  var_xvar_name = "X",
                                  var_yvar_name = "Y",
                                  var_crs = "+proj=longlat +datum=WGS84",
                                  spatial_grid = bccoast,
                                  grid_xvar_name = "UTM.lon",
                                  grid_yvar_name = "UTM.lat",
                                  grid_crs = "+proj=utm +zone=9 +datum=WGS84") {
  spdf <- readRDS(rds.file) %>% mutate(X = .data[[var_xvar_name]], Y = .data[[var_yvar_name]])

  coordinates(spdf) <- ~ X + Y
  proj4string(spdf) <- CRS(var_crs)

  # Create a base raster with with ROMS lat/lon extent
  b <- rast(
    xmin = min(unique(spdf$X)), xmax = max(unique(spdf$X)),
    ymin = min(unique(spdf$Y)), ymax = max(unique(spdf$Y)),
    crs = var_crs
  )
  class(b)

  # project
  r <- terra::project(b, grid_crs)
  spdf <- spTransform(spdf, grid_crs)

  # # set resolution to 3 km (the resolution of the ROMs grid), make this 4km so I have less NAs however this should be 3 km and interpolate data
  res(r) <- set_resolution
  # plot(r)

  # Project values onto a 10km raster (values are a mean of surrounding cells)
  # xx <- rasterize(x=vect(spdf), y=r, field=names(spdf)[1], na.rm=F) # will loose a couple periperal cells
  xx <- rasterize(x = vect(spdf), y = r, field = names(spdf)[1], na.rm = T)

  # if(print_test_plot) raster::plot(xx)
  # raster::plot(xx)
  mx <- matrix(1, nc = 3, nr = 3)
  # mx[2,1] <- NA  # will loose a couple periperal cells
  xf <- terra::focal(xx, w = mx, fun = mean, na.policy = "only", na.rm = T)
  # raster::plot(xf)

  for (i in (2:length(names(spdf)))) {
    xx <- rasterize(x = vect(spdf), y = r, field = names(spdf)[i], na.rm = T)
    xf2 <- terra::focal(xx, w = mx, fun = mean, na.policy = "only", na.rm = T)
    xf <- c(xf, xf2)
  }

  names(xf) <- names(spdf)

  # Overlay rasters with prediction points ----------------------------------
  r <- xf

  # browser()
  # grid <- spatial_grid
  # spatial_grid$X <- spatial_grid[grid_xvar_name]
  # spatial_grid$Y <- spatial_grid[grid_yvar_name]

  # this works for spatial points, should probably add buffer though and ultimately an option to use polygons

  if (!is.null(grid_xvar_name)) {
    grid <- spatial_grid %>% mutate(X = .data[[grid_xvar_name]], Y = .data[[grid_yvar_name]])

    # get just coordinates
    coords <- grid %>%
      dplyr::select(X, Y) %>%
      distinct()

    if (print_test_plot) {

      ## test output
      xy <- raster::extract(r[[1]], coords, weights = FALSE, fun = mean, na.rm = T)
      df <- data.frame(cbind(z = xy[, 2], coords))
      # browser()

      g <- ggplot2::ggplot(df) +
        geom_point(aes(X, Y, colour = z)) +
        scale_colour_viridis_c()
      g
    } else {

      # coords to append to output
      out <- coords

      for (i in seq_along(1:length(names(spdf)))) {
        xy <- raster::extract(r[[i]], coords, weights = FALSE, fun = mean)
        df <- data.frame(cbind(z = xy[, 2], coords))
        colnames(df)[1] <- names(r[[i]])
        out <- df %>%
          dplyr::select(colnames(df)[1]) %>%
          bind_cols(out, .)
        out
      }
      out <- pivot_longer(out, 3:ncol(out), names_to = "year", values_to = variable_name)
      out <- left_join(grid, out) %>% dplyr::select(-X, -Y)
    }
  } else {
    if (print_test_plot) {

      ## test output
      xy <- raster::extract(r[[1]], spatial_grid, weights = FALSE, fun = mean, na.rm = T)
      df <- data.frame(cbind(z = xy[, 2], spatial_grid))
      df <- st_as_sf(df)
      g <- ggplot2::ggplot(df) +
        geom_sf(aes(colour = z)) +
        scale_colour_viridis_c()
      g
    } else {

      # coords to append to output
      out <- spatial_grid

      for (i in seq_along(1:length(names(spdf)))) {
        xy <- raster::extract(r[[i]], spatial_grid, weights = FALSE, fun = mean)
        df <- data.frame(cbind(z = xy[, 2], spatial_grid))
        colnames(df)[1] <- names(r[[i]])
        out <- df %>%
          dplyr::select(colnames(df)[1]) %>%
          bind_cols(out, .)
        out
      }
      out <- pivot_longer(out, ncol(spatial_grid):ncol(out) - 1,
        names_to = "year", values_to = variable_name
      )
    }
  }
  out
}
