#' Convert iscam mcmc output into dataframe of annual medians
#'
#' @param csv csv file location and name
#' @param species species label string
#' @param stock stock label string
#' @param start_year first year as integer
#' @param end_year last year as integer
#' @param var_name what to call parameter output
#'
#' @return dataframe of parameter values by year
#' @export
#'
#' @examples
#' d <- iscam_mcmc(
#'   csv = "data/PCOD/iscam_sbt_mcmc_HS_QCS.csv",
#'   species = "pacific cod",
#'   stock = "SYN HS & QCS",
#'   start_year = 1956,
#'   end_year = 2021,
#'   var_name = "biomass"
#' )
#'
iscam_mcmc <- function(csv, species, stock, start_year, end_year, var_name = "biomass") {
  .d <- readr::read_csv(csv)
  .d <- .d[1001:2000, ] %>% dplyr::summarise_all(median)
  .d <- t(.d)
  year <- start_year:end_year
  .d <- as.data.frame(.d)
  .d$year <- as.numeric(gsub(".*\\_", "", rownames(.d)))

  if(var_name %in% c("harvest_rate","vbiomass")){
    browser()
    .d$fleet <- gsub("vbt_fleet", "", rownames(.d))
    .d$fleet <- as.numeric(gsub("_.*", "", .d$fleet))
    # .d <- .d[.d$fleet == 1,] %>% dplyr::rename(!!var_name := V1)
  } else {
  # .d <- .d[1:length(year),] %>% dplyr::rename(!!var_name := V1)
  }

  .d <- .d %>% dplyr::rename(!!var_name := V1)
  # browser()
  .d$species <- species
  .d$stock <- stock
  .d
}
