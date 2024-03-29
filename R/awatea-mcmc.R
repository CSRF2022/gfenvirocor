#' Convert mcmc outputs provided by Rowan into dataframe of annual medians
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
  .d <- .d[,2:ncol(.d)]
  .d <- .d %>% dplyr::summarise_all(median)
  .d <- t(.d)
  .d <- as.data.frame(.d)

  if(var_name %in% c("harvest_rate","vbiomass")){
  .d$fleet <- as.numeric(gsub(".*_", "", rownames(.d)))
  }
  .d$year <- as.numeric(gsub("_.*", "", rownames(.d)))
  rownames(.d) <- NULL
  .d <- .d %>% dplyr::rename(!!var_name := V1)
  .d$species <- species
  .d$stock <- stock
  .d
}
