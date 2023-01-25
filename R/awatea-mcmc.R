#' Convert Awatea mcmc output into dataframe of annual medians
#'
#' @param csv csv file location and name
#' @param species species label string
#' @param stock stock label string
#' @param var_name what to call parameter output
#'
#' @examples
#' d <- awatea_mcmc(
#'   csv = "data/BOR-CST-2021-Awatea-output-forRobyn-230119/BOR-CST-2021-MCMC(B)-forRobyn.csv",
#'   species = "bocaccio",
#'   stock = "Coastwide",
#'   var_name = "biomass"
#' )
#'
awatea_mcmc <- function(csv, species, stock, var_name = "biomass") {
  .d <- readr::read_csv(csv)
  .d <- .d %>%
    dplyr::select(-Run.Sample) %>%
    dplyr::summarise_all(median)
  .d <- t(.d)
  .d <- as.data.frame(.d)
  .d$year <- as.numeric(rownames(.d))
  rownames(.d) <- NULL
  .d <- .d %>% dplyr::rename(!!var_name := V1)
  .d$species <- species
  .d$stock <- stock
  .d
}
