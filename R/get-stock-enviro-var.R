#' Get stock specific environmental variables from any larger scale spatiotemporal grid
#'
#' @param temporal_grid Spatiotemporal grid spanning the time frame and spatial extents relevant to a stock
#' @param variable_name Which variable to extract and summarize
#' @param species Optional argument to provide a variable indicating the species. Default is "NA"
#' @param stock Optional argument to provide a variable indicating the stock. Default is "NA"
#' @param time_var Which variable contains the time steps of interest. Must be evenly spaced integers
#' @param recruitment_age The age at recruitment used in the stock assessments in same units as time_var
#'   This controls which lags (up to age + 1) are included.
#' @param depth_range Vector length 2 defining the range of depths of interest.
#'   Can/should depend on the variable, species, and stock. Can be `NULL` when all depths should be aggregated.
#' @param lon_range Vector length 2 defining the longitude range. Can be `NULL` when all should be aggregated.
#' @param lat_range Vector length 2 defining the latitude range. Can be `NULL` when all should be aggregated.
#' @param lon_var_name Provide name of longitude variable in the grid, if not using a spatial object
#' @param lat_var_name Provide name of latitude variable in the grid, if not using a spatial object
#' @param polygon Polygons are not yet supported
#' @param bbox A bbox is not yet supported
#'
#' @return
#' A data frame of time steps and variable values summarized for a specific geographic area and depth range.
#'
#' @export
#'
#' @examples
#' rds.file <- "data/annual_mean_tob_bc.rds"
#' load(file = "data/wcvi_grid.rda")
#'
#' grid <- project_netcdf_values(
#'   rds.file,
#'   variable_name = "ann_mean",
#'   print_test_plot = TRUE,
#'   coord_df = wcvi_grid,
#'   grid_xvar_name = "X",
#'   grid_yvar_name = "Y"
#' )
#'
#' df <- get_stock_enviro_var(
#'   temporal_grid = grid,
#'   variable_name = "ann_mean",
#'   species = "yelloweye rockfish",
#'   stock = "WCVI",
#'   recruitment_age = 1,
#'   depth_range = c(0, 200),
#'   lon_range = NULL,
#'   lat_range = c(48, 51.2),
#'   lon_var_name = "LONGITUDE",
#'   lat_var_name = "LATITUDE"
#' )
#'
get_stock_enviro_var <- function(temporal_grid = grid,
                                 variable_name = "ann_mean",
                                 species = "NA",
                                 stock = "NA",
                                 time_var = "year",
                                 recruitment_age = 1,
                                 depth_range = c(0, 200),
                                 lon_range = NULL,
                                 lat_range = NULL,
                                 lon_var_name = "UTM.lon",
                                 lat_var_name = "UTM.lat",
                                 polygon = NULL,
                                 grid_crs = "+proj=utm +zone=9 +datum=WGS84 +units=km +no_defs",
                                 polygon_crs = "+proj=utm +zone=9 +datum=WGS84 +units=km",
                                 bbox = NULL) {

  dat <- temporal_grid %>% dplyr::mutate(
    time = as.numeric(.data[[time_var]])
  )

  dat <- dat[!is.na(dat[variable_name]),]

  if (!is.null(lon_range)) {
    dat <- dat %>% dplyr::mutate(
      lon = .data[[lon_var_name]]
    )
    dat <- dplyr::filter(dat, lon >= lon_range[1] & lon <= lon_range[2])
  }
  if (!is.null(lat_range)) {
    dat <- dat %>% dplyr::mutate(
      lat = .data[[lat_var_name]]
    )
    dat <- dplyr::filter(dat, lat >= lat_range[1] & lat <= lat_range[2])
  }

  if (!is.null(polygon)) {
    # "TODO: Polygons are not yet supported."
    # browser()
    grid_crs2 <- st_crs(dat)$proj4string

    if (is.na(grid_crs2)) {
      if (is.null(grid_crs)) {
        stop("Your grid does not have a CRS, so you need to provide it to the arguement 'grid_crs'.")
      } else {
        # make grid into sf object
        dat <- dat %>% dplyr::mutate(
          lon = .data[[lon_var_name]],
          lat = .data[[lat_var_name]]
        )
        grid <- dat %>% mutate(x = lon, y = lat)
        gridsf <- st_as_sf(grid, coords = c("x", "y"), crs = grid_crs)
      }
    } else {
      print(paste("The grid provided had a CRS of", grid_crs2, ". Is this what you expected? If you provided one, it will be replaced with this one."))
      grid_crs <- grid_crs2
      gridsf <- dat
    }

    # check if polygon is already an sf object?
    type <- attr(polygon, "class")
    # browser()
    if (type[1] == "sf") {
      type <- attr(polygon$geometry, "class")
    }

    if (type[1] %in% c("sfg_POLYGON", "sfc_POLYGON", "sfc_MULTIPOLYGON", "sfg_MULTIPOLYGON")) {
      polygon_crs2 <- st_crs(polygon)$proj4string

      if (is.na(polygon_crs2)) {
        if (is.null(polygon_crs)) {
          stop("Your polygon is missing a CRS, so you need to provide it to the arguement 'polygon_crs'.")
        }

        st_crs(polygon) <- polygon_crs # check how to define the crs?
      } else {
        print(paste("The polygon provided had a CRS of", polygon_crs2, ". Is this what you expected? It will be tranformed to match the grid crs."))
      }
    } else {
      if (type == "SpatialPolygonsDataFrame") {
        # there isn't a geometry, so make into an sf object
        polygon <- st_as_sfc(polygon, crs = polygon_crs)
      } else {
        "Object input to the polygon arguement needs to be a spatial polygon."
      }
    }

    # transform into the same projection as your temporal grid
    polygon <- st_transform(polygon, crs = grid_crs)

    # gridsf %>% filter(time == min(time, na.rm = TRUE)) %>%
    #   ggplot() + geom_sf(aes(colour = depth))

    g <- gridsf %>% filter(time == min(time, na.rm = TRUE)) %>%
    ggplot() + geom_sf(aes(colour = .data[[variable_name]])) +
      scale_colour_viridis_c()

    print(g)
    # browser()

    dat <- st_intersection(gridsf, polygon)
  }

  if (!is.null(bbox)) {
    "TODO: A bbox is not yet supported."
  }


  if (!is.null(depth_range)) {
    dat <- dplyr::filter(dat, depth >= depth_range[1] & depth <= depth_range[2])
  }


    g <- dat %>% filter(time == min(time, na.rm = TRUE)) %>%
    ggplot() +
      geom_point(aes(.data[[lon_var_name]], .data[[lat_var_name]],
        colour = .data[[variable_name]]
      )) +
      scale_colour_viridis_c()

    print(g)

    # remove geometry because next step is to summarize by time step
    dat$geometry <- NULL

  # for now only using mean to aggregate environmental conditions within the spatial extent of interest
  dat <- dat %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(var = mean(.data[[variable_name]], na.rm = T))

  dat <- dat %>% dplyr::arrange(time)

  if (recruitment_age == 0) {
    dat <- dat %>% mutate(
      var_lag1 = lag(var, 1)
    )
  }

  if (recruitment_age == 1) {
    dat <- dat %>% mutate(
      var_lag1 = lag(var, 1),
      var_lag2 = lag(var, 2)
    )
  }

  if (recruitment_age == 2) {
    dat <- dat %>% mutate(
      var_lag1 = lag(var, 1),
      var_lag2 = lag(var, 2),
      var_lag3 = lag(var, 3)
    )
  }

  if (recruitment_age == 3) {
    dat <- dat %>% mutate(
      var_lag1 = lag(var, 1),
      var_lag2 = lag(var, 2),
      var_lag3 = lag(var, 3),
      var_lag4 = lag(var, 4)
    )
  }

  dat[[time_var]] <- as.numeric(dat$time)
  dat$time <- NULL

  names(dat) <- gsub("var", variable_name, names(dat))
  dat$species <- species
  dat$stock <- stock
  dat
}
