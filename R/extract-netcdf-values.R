#' Get annual averages from certain times within the netcdf
#'
#' @param nc.file Path and file name for netcdf
#' @param output.dir Directory to save output into
#' @param xvar_name What is the x variable called when stored as an array, not a vector? Default "longitude"
#' @param yvar_name What is the y variable called when stored as an array, not a vector? Default "latitude"
#' @param time_name What is the time variable called? Default "time"
#' @param time_resolution_input Currently only working for monthly data
#' @param time_resolution_output Currently only working for annual values
#' @param variable_name What is the variable of interest called? Default "tob"
#' @param model_start_time Starting time in the output units. Currently on working for years
#' @param model_end_time Ending time in the output units. Currently on working for years
#' @param xbounds A vector of length 2 containing min and max values in same units as netcdf dimensions.
#'   Default is longitude for all BC coast
#' @param ybounds A vector of length 2 containing min and max values in same units as netcdf dimensions.
#'   Default is latitude for all BC coast
#' @param whichtimes A vector of integers representing times of interest. Default is `c(1:12)` for annual mean from monthly data
#' @param agg_method Method for aggregating data. Default "mean", other options include "min" and "max"
#'
#' @return
#' A data frame with columns X, Y, and for each time step output.
#'
#' @export
#'
#' @examples
#' df <- extract_netcdf_values(filename,
#'   variable_name = "tob",
#'   model_start_year = 1950,
#'   model_end_year = 1979,
#'   agg_method = "max")
#'
extract_netcdf_values <- function(
  nc.file,
  output.dir = "data",
  xvar_name = "longitude",
  yvar_name = "latitude",
  time_name = "time",
  time_resolution_input = "monthly",
  time_resolution_output = "annual",
  variable_name = "tob",
  model_start_time = 1950,
  model_end_time = 1979,
  xbounds = c(-133.85254, -123.21362), # default all BC coast
  ybounds = c(48.21968, 55.68620), # default all BC coast
  whichtimes = c(1:12), # default all for annual mean
  agg_method = "mean"
) {

  nc_data <- nc_open(nc.file)
  x <- ncvar_get(nc_data, xvar_name)
  y <- ncvar_get(nc_data, yvar_name)

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


  if(time_resolution_input == "monthly" & time_resolution_output == "annual") {
    # create df of months and years to go with the sequential time variable in the array
    month <- rep(1:12, (model_end_time - model_start_time + 1))
    year <- rep(model_start_time:model_end_time, each = 12)
    timedf <- as.data.frame(cbind(t, months = month, year = year))
    timedf$layer_seq <- seq(1, nrow(timedf), 1)
    selected_months <- dplyr::filter(timedf, month %in% whichtimes)

    years <- as.numeric(seq(model_start_time, model_end_time, 1))

    df <- data.frame(X=c(x), Y=c(y))

    for (i in 1:length(years) ){

      yearsel <- years[i]
      xx <- selected_months %>% dplyr::filter(year == yearsel) %>% dplyr::select(layer_seq)
      # selection <- nc.array[,,xx[1:length(xx$layer_seq),]]
      selection <- nc.array[,,xx$layer_seq]

      if(agg_method == "mean") {
        value <- apply(selection, MARGIN=c(1, 2), mean, na.rm=TRUE) #margin, rows and columns
      }

      if(agg_method == "max") {
        value <- apply(selection, MARGIN=c(1, 2), max, na.rm=TRUE) #margin, rows and columns
      }

      if(agg_method == "min") {
        value <- apply(selection, MARGIN=c(1, 2), min, na.rm=TRUE) #margin, rows and columns
      }

      df[[as.character(yearsel)]] <- c(value[,]) # most be character to use to name columns
    }
  } else{
    stop("Time resolutions other than monthly input with annual output are not yet supported.")
  }

  if(!is.null(xbounds)){
    df <- df %>%
      filter(
        .data[["X"]] > xbounds[1],
        .data[["X"]] < xbounds[2]
      )
  }

  if(!is.null(ybounds)){
    df <- df %>%
      filter(
        .data[["Y"]] > ybounds[1],
        .data[["Y"]] < ybounds[2]
      )
  }

  # not sure why, but extract_netcdf_values is returning Inf
  df[sapply(df, is.infinite)] <- NA

  saveRDS(df, paste0(output.dir, "/",
                     variable_name, "-",
                     time_resolution_input, "-",
                     model_start_time, "-", model_end_time, "-",
                     agg_method, "-",
                     whichtimes[1], "-",
                     max(whichtimes), ".rds"))
  return(df)
}
