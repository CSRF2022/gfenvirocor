#' Get aggregated netcdf values for specific locations or grid cells
#'
#' @param rds.file File containing output from `extract_netcdf_values()` function
#' @param variable_name What variable does the above file contain
#' @param print_test_plot If `TRUE` will return a plot of the first time step, showing how the resolution and extraction is working
#' @param var_xvar_name What is the x variable from the rds called? Default matches output from `extract_netcdf_values()` function
#' @param var_yvar_name What is the Y variable from the rds called? Default matches output from `extract_netcdf_values()` function
#' @param var_crs What CRS projection was the netcdf file in?
#' @param coord_df Coordinates data frame. Can be spatial object, or any data frame with x and y columns
#' @param grid_xvar_name Provide name of x variable in the grid, if not using a spatial object
#' @param grid_yvar_name Provide name of y variable in the grid, if not using a spatial object
#' @param grid_crs What CRS projection is the grid in?
#' @param set_resolution Choose a resolution in same units as grid_crs
#' @param radius Choose radius around points (only applies if not using a polygon). Default is half the resolution.
#'
#' @return
#' The grid expanded to include all years and the netcdf derived variables.
#'
#' @export
#'
#' @examples
#' rds.file <- "data/annual_mean_tob_bc.rds"
#' project_netcdf_values(
#'   rds.file,
#'   variable_name = "ann_mean",
#'   print_test_plot = TRUE,
#'   coord_df = sdmTMB::qcs_grid,
#'   grid_xvar_name = "X",
#'   grid_yvar_name = "Y"
#' )
project_netcdf_values <- function(nc.rds.file,
                                  variable_name,
                                  print_test_plot = FALSE,
                                  var_xvar_name = "X",
                                  var_yvar_name = "Y",
                                  var_crs = "+proj=longlat +datum=WGS84",
                                  coord_df = bccoast,
                                  grid_xvar_name = "UTM.lon",
                                  grid_yvar_name = "UTM.lat",
                                  grid_crs = "+proj=utm +zone=9 +datum=WGS84 +units=km",
                                  set_resolution = 10,
                                  radius = NULL) {
  spdf <- readRDS(nc.rds.file) %>% dplyr::mutate(X = .data[[var_xvar_name]], Y = .data[[var_yvar_name]])

  sp::coordinates(spdf) <- ~ X + Y
  sp::proj4string(spdf) <- sp::CRS(var_crs)

  # Create a base raster ----------------------------------
  b <- terra::rast(
    xmin = min(unique(spdf$X)), xmax = max(unique(spdf$X)),
    ymin = min(unique(spdf$Y)), ymax = max(unique(spdf$Y)),
    crs = var_crs
  )
  class(b)

  # project
  r <- terra::project(b, grid_crs)
  spdf <- sp::spTransform(spdf, grid_crs)

  # # set resolution
  terra::res(r) <- set_resolution
  # plot(r)

  # Project values onto a 10km raster (values are a mean of surrounding cells)
  # xx <- rasterize(x=vect(spdf), y=r, field=names(spdf)[1], na.rm=F) # will loose a couple periperal cells
  xx <- terra::rasterize(x = terra::vect(spdf), y = r, field = names(spdf)[1], na.rm = T)

  # if(print_test_plot) raster::plot(xx)
  # raster::plot(xx)
  mx <- matrix(1, nc = 3, nr = 3)

  # mx[2,1] <- NA  # will loose a couple peripheral cells
  xf <- terra::focal(xx, w = mx, fun = mean, na.policy = "only", na.rm = T)
  # raster::plot(xf)
  if(length(names(spdf))>=2){
  for (i in (2:length(names(spdf)))) {
    xx <- terra::rasterize(x = terra::vect(spdf), y = r, field = names(spdf)[i], na.rm = T)
    xf2 <- terra::focal(xx, w = mx, fun = mean, na.policy = "only", na.rm = T)
    xf <- c(xf, xf2)
  }
  }
  # browser()
  names(xf) <- names(spdf)

  # Overlay rasters with prediction points ----------------------------------
  r <- xf

  if (is.null(radius)) {
    radius <- set_resolution / 2
  }

  is_sf <- attr(coord_df, "sf_column")

  if (!is.null(is_sf)) {
    # there is a geometry ----------------------------------
    what_geometry <- st_geometry_type(coord_df, by_geometry = FALSE)

    if (what_geometry[1] == "POLYGON") {
      # geometry is a polygon ----------------------------------
      if (print_test_plot) {

        ## test output
        xy <- raster::extract(r[[1]], coord_df, weights = FALSE, fun = mean, na.rm = T)
        df <- data.frame(cbind(z = xy[, 2], coord_df))
        df <- sf::st_as_sf(df)
        g <- ggplot(df) +
          geom_sf(aes(colour = z)) +
          scale_colour_viridis_c() +
          labs(colour = variable_name)
        print(g)
      } else {

        # coords to append to output
        out <- coord_df

        for (i in seq_along(1:length(names(spdf)))) {
          xy <- raster::extract(r[[i]], coord_df, weights = FALSE, fun = mean, na.rm = T)
          df <- data.frame(cbind(z = xy[, 2], coord_df))
          colnames(df)[1] <- names(r[[i]])
          out <- df %>%
            dplyr::select(colnames(df)[1]) %>%
            dplyr::bind_cols(out, .)
          out
        }
        out
      }
    } else {
      # geometry is not a polygon ----------------------------------
      if (print_test_plot) {

        ## test output
        xy <- raster::extract(r[[1]], coord_df, buffer = radius, weights = FALSE, fun = mean, na.rm = T)
        df <- data.frame(cbind(z = xy[, 2], coord_df))
        df <- st_as_sf(df)
        browser()
        g <- ggplot(df) +
          geom_sf(aes(colour = z)) +
          scale_colour_viridis_c() +
          labs(colour = variable_name)
        print(g)
      } else {

        # coords to append to output
        out <- coord_df

        for (i in seq_along(1:length(names(spdf)))) {
          xy <- raster::extract(r[[i]], coord_df, buffer = radius, weights = FALSE, fun = mean, na.rm = T)
          df <- data.frame(cbind(z = xy[, 2], coord_df))
          colnames(df)[1] <- names(r[[i]])
          out <- df %>%
            dplyr::select(colnames(df)[1]) %>%
            dplyr::bind_cols(out, .)
          out
        }
      }
    }
    if (!print_test_plot) {
      # browser()
      out2 <- tidyr::pivot_longer(out, ncol(coord_df):(ncol(out) - 1),
        names_to = "year", values_to = variable_name
      )
      out2
    } else {
      g
    }
  } else {
    # no geometry at all ----------------------------------
    grid <- coord_df %>% mutate(X = .data[[grid_xvar_name]], Y = .data[[grid_yvar_name]])

    # get just coordinates
    coords <- grid %>%
      dplyr::select(X, Y) %>%
      dplyr::distinct()

    if (print_test_plot) {

      ## test output
      xy <- raster::extract(r[[1]], coords, buffer = radius, weights = FALSE, fun = mean, na.rm = T)
      df <- data.frame(cbind(z = xy[, 2], coords))
      g <- ggplot(df) +
        geom_point(aes(X, Y, colour = z)) +
        scale_colour_viridis_c() +
        labs(colour = variable_name)
      g
    } else {

      # coords to append to output
      out <- coords

      for (i in seq_along(1:length(names(spdf)))) {
        xy <- raster::extract(r[[i]], coords, buffer = radius, weights = FALSE, fun = mean, na.rm = T)
        df <- data.frame(cbind(z = xy[, 2], coords))
        colnames(df)[1] <- names(r[[i]])
        out <- df %>%
          dplyr::select(colnames(df)[1]) %>%
          dplyr::bind_cols(out, .)
        out
      }

      if(ncol(out)>3){
        out <- tidyr::pivot_longer(out, 3:ncol(out), names_to = "year", values_to = variable_name)
      } else {
        out[variable_name] <- out[, 3]
        out$year <- colnames(out)[3]
      }

      out2 <- dplyr::left_join(grid, out) #%>% dplyr::select(-X, -Y)
      out2
    }
  }
}
