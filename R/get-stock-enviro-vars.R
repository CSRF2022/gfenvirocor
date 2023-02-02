#' Get stock specific environmental variables from any larger scale spatiotemporal grid
#'
#' @param temporal_grid Spatiotemporal grid spanning the time frame and spatial extents relevant to a stock
#' @param variable_name Which variable to extract and summarize
#' @param species Optional argument to provide a variable indicating the species. Default is "NA"
#' @param stock Optional argument to provide a variable indicating the stock. Default is "NA"
#' @param recruitment_age The age at recruitment used in the stock assessments.
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
#' df <- get_stock_enviro_vars(  temporal_grid = grid,
#' variable_name = "ann_mean",
#' species = "yelloweye rockfish",
#' stock = "WCVI",
#' recruitment_age = 1,
#' depth_range = c(0, 200),
#' lon_range = NULL,
#' lat_range = c(48,51.2),
#' lon_var_name = "LONGITUDE",
#' lat_var_name = "LATITUDE"
#' )
#'
get_stock_enviro_vars <- function(
  temporal_grid = grid,
  variable_name = "ann_mean",
  species = "NA",
  stock = "NA",
  recruitment_age = 1,
  depth_range = c(0, 200),
  lon_range = NULL,
  lat_range = NULL,
  lon_var_name = "UTM.lon",
  lat_var_name = "UTM.lat",
  polygon = NULL,
  bbox = NULL
){


  dat <- temporal_grid %>% dplyr::mutate(lon = .data[[lon_var_name]], lat = .data[[lat_var_name]])

  if(!is.null(lon_range)){
    dat <-  dplyr::filter(dat, lon >= lon_range[1] & lon >= lon_range[2])
  }
  if(!is.null(lat_range)){
    dat <-  dplyr::filter(dat, lat >= lat_range[1] & lat >= lat_range[2])
  }

  if(!is.null(polygon)) { "TODO: Polygons are not yet supported."}

  if(!is.null(bbox)) { "TODO: A bbox is not yet supported."}


  if(!is.null(depth_range)){
    dat <-  dplyr::filter(dat, depth >= depth_range[1] & depth >= depth_range[2])
  }

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
