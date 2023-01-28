#' Calculate stock productivity, recruitment, and lag variables from mcmc outputs
#'
#' @param catchfile location of catch data csv summarized by year and summed across fleets
#' @param mcmcprefix mcmc csv file location and name prefix (everything before the variable label)
#' @param mcmcsuffix mcmc csv file location and name suffix (everything after the variable label)
#' @param species species label string
#' @param stock stock label string
#' @param model_type which stock assessment platform was used to produce mcmc outputs
#' @param start_year default `NULL`; integer value required for iscam outputs
#' @param end_year default `NULL`; integer value required for iscam outputs
#' @param recruitment_age default `1`; integer value
#'
#' @return dataframe
#' @export
#'
#' @examples
#' calc_prod(
#'   catchfile = "data/arrowtooth-mcmc/arrowtooth-total-catch.csv",
#'   mcmcprefix = "data/arrowtooth-mcmc/iscam_",
#'   mcmcsuffix = "_mcmc.csv",
#'   species = "arrowtooth flounder",
#'   stock = "Coastwide",
#'   model_type = "iscam",
#'   start_year = 1996,
#'   end_year = 2021,
#'   recruitment_age = 1
#' )
#'
calc_prod <- function(catchfile,
                      mcmcprefix,
                      mcmcsuffix,
                      species,
                      stock,
                      model_type = "awatea",
                      start_year = NULL,
                      end_year = NULL,
                      recruitment_age = 1) {

  c <- readr::read_csv(catchfile)

  if (model_type == "iscam") {
    b <- iscam_mcmc(
      csv = paste0(mcmcprefix, "sbt", mcmcsuffix),
      species,
      stock,
      start_year = start_year,
      end_year = end_year + 1,
      var_name = "biomass"
    )

    r <- iscam_mcmc(
      csv = paste0(mcmcprefix, "rt", mcmcsuffix),
      species,
      stock,
      start_year = start_year + recruitment_age,
      end_year = end_year,
      var_name = "recruits"
    )

    d <- iscam_mcmc(
      csv = paste0(mcmcprefix, "rdev", mcmcsuffix),
      species,
      stock,
      start_year = start_year + recruitment_age,
      end_year = end_year,
      var_name = "rdev"
    )
  }

  if (model_type == "awatea") {
    b <- awatea_mcmc(
      csv = paste0(mcmcprefix, "B", mcmcsuffix),
      species,
      stock,
      var_name = "biomass"
    )

    r <- awatea_mcmc(
      csv = paste0(mcmcprefix, "R", mcmcsuffix),
      species,
      stock,
      var_name = "recruits"
    )

    d <- awatea_mcmc(
      csv = paste0(mcmcprefix, "Rdev", mcmcsuffix),
      species,
      stock,
      var_name = "rdev"
    )

    c <- c %>% rename(year = `...1`) %>%
      pivot_longer(2:ncol(c), names_to = "fleet", values_to = "value") %>%
      group_by(year) %>% summarise(catch = sum(value, na.rm = TRUE))
  }


  df <- left_join(c, b) %>%
    left_join(r) %>%
    left_join(d)

  df <- df %>% select(species, stock, year, catch, biomass, recruits, rdev)


  if (recruitment_age == 1) {
    df <- df %>% mutate(
      catch_lag1 = lag(catch),
      biomass_lag1 = lag(biomass),
      biomass_lag2 = lag(biomass, 2),
      production = (biomass + catch_lag1 - biomass_lag1),
      production1 = (biomass + catch_lag1 - biomass_lag1) / biomass_lag1,
      production2 = (biomass + catch_lag1 - biomass_lag1) / biomass_lag2,
    )
  }

  if (recruitment_age == 2) {
    df <- df %>% mutate(
      catch_lag1 = lag(catch),
      biomass_lag1 = lag(biomass),
      biomass_lag2 = lag(biomass, 2),
      biomass_lag3 = lag(biomass, 3),
      production = (biomass + catch_lag1 - biomass_lag1),
      production1 = (biomass + catch_lag1 - biomass_lag1) / biomass_lag1,
      production2 = (biomass + catch_lag1 - biomass_lag1) / biomass_lag2,
      production3 = (biomass + catch_lag1 - biomass_lag1) / biomass_lag3
    )
  }

  if (recruitment_age == 3) {
    df <- df %>% mutate(
      catch_lag1 = lag(catch),
      biomass_lag1 = lag(biomass),
      biomass_lag2 = lag(biomass, 2),
      biomass_lag3 = lag(biomass, 3),
      biomass_lag4 = lag(biomass, 4),
      production = (biomass + catch_lag1 - biomass_lag1),
      production1 = (biomass + catch_lag1 - biomass_lag1) / biomass_lag1,
      production2 = (biomass + catch_lag1 - biomass_lag1) / biomass_lag2,
      production3 = (biomass + catch_lag1 - biomass_lag1) / biomass_lag3,
      production4 = (biomass + catch_lag1 - biomass_lag1) / biomass_lag4
    )
  }
  df
}

