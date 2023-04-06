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
#' @param proportion_female default to `0.5`; used to scale catch to match female biomass
#' @param recruitment_age default `1`; integer value
#' @param maturity_age default `1`; integer value
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
                      model_type = "rowans",
                      start_year = NULL,
                      end_year = NULL,
                      proportion_female = 0.5,
                      recruitment_age = 1,
                      maturity_age = 1
                      ){

  c <- readr::read_csv(catchfile)

  # browser()
  if (model_type == "iscam") {

    b <- iscam_mcmc(
      csv = paste0(mcmcprefix, "sbt", mcmcsuffix),
      species,
      stock,
      start_year = start_year,
      end_year = end_year + 1,
      var_name = "biomass"
    )

   try(v <- iscam_mcmc(
      csv = paste0(mcmcprefix, "vbt", mcmcsuffix),
      species,
      stock,
      start_year = start_year,
      end_year = end_year + 1,
      var_name = "vbiomass"
    ))

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

  if (model_type == "rowans") {
    b <- awatea_mcmc(
      csv = paste0(mcmcprefix, "B", mcmcsuffix),
      species,
      stock,
      var_name = "biomass"
    )

    try(v <- awatea_mcmc(
      csv = paste0(mcmcprefix, "VB", ")-forPhilina.csv"),
      species,
      stock,
      var_name = "vbiomass"
    ))

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
      group_by(year) %>% summarise(catch = sum(value, na.rm = TRUE)*proportion_female)
  }

  if (model_type == "landmark") {
    df <- c %>% mutate(species = species,
                       stock = stock,
                       year = Year,
                       catch = Landings*proportion_female, # tonnes
                       biomass = SSBt_p50*1000, # tonnes
                       vbiomass = NA,
                       recruits = Rt_p50*1000, # 1000 fish
                       rdev = Rdev_p50
                       )

  } else {
  if(exists("v")){
    # for now this code only extracts vulnerable biomass for the first fleet
    v <- na.omit(v) # additional fleets will have NA for year, so this removes them
    df <- left_join(c, b) %>%
      left_join(r) %>%
      left_join(d) %>%
      left_join(v)
  } else {
  df <- left_join(c, b) %>%
    left_join(r) %>%
    left_join(d)
  df$vbiomass <- NA
  }
  }

  df <- df %>% select(species, stock, year, catch, biomass, vbiomass, recruits, rdev) %>%
    mutate(model_type = model_type,
           proportion_female = proportion_female,
           recruitment_age = recruitment_age,
           maturity_age = maturity_age
    )

  recruitment_lag <- as.integer(round(maturity_age-recruitment_age))

  if (recruitment_age == 0) {
    df <- df %>% mutate(
      biomass_lead1 = lead(biomass),
      production = (biomass_lead1 + catch - biomass),
      p_by_biomass = production / biomass,
      recruits_lag = lag(recruits, recruitment_lag[1]),
      biomass_for_recruits = lag(biomass, 0),
      biomass_lag1 = lag(biomass, 1)
      # Prod_by_biomass1 = production / biomass_lag1,
    )
  }

  if (recruitment_age == 1) {
    df <- df %>% mutate(
      biomass_lead1 = lead(biomass),
      production = (biomass_lead1 + catch - biomass),
      p_by_biomass = production / biomass,
      recruits_lag = lag(recruits, recruitment_lag[1]),
      biomass_for_recruits = lag(biomass, 1),
      biomass_lag1 = lag(biomass, 1),
      biomass_lag2 = lag(biomass, 2)
      # Prod_by_biomass1 = production / biomass_lag1,
    )
  }

  if (recruitment_age == 2) {
    df <- df %>% mutate(
      biomass_lead1 = lead(biomass),
      production = (biomass_lead1 + catch - biomass),
      p_by_biomass = production / biomass,
      recruits_lag = lag(recruits, recruitment_lag[1]),
      biomass_for_recruits = lag(biomass, 2),
      biomass_lag1 = lag(biomass, 1),
      biomass_lag2 = lag(biomass, 2),
      biomass_lag4 = lag(biomass, 3)
      # Prod_by_biomass1 = production / biomass_lag1,
    )
  }

  if (recruitment_age == 3) {
    df <- df %>% mutate(
      biomass_lead1 = lead(biomass),
      production = (biomass_lead1 + catch - biomass),
      p_by_biomass = production / biomass,
      recruits_lag = lag(recruits, recruitment_lag[1]),
      biomass_for_recruits = lag(biomass, 3),
      biomass_lag1 = lag(biomass, 1),
      biomass_lag2 = lag(biomass, 2),
      biomass_lag4 = lag(biomass, 3),
      biomass_lag4 = lag(biomass, 4)
    )
  }

  if(exists("v")){
    df <- df %>% mutate(
      vbiomass_lead1 = lead(vbiomass),
      v_production = (vbiomass_lead1 + (catch/proportion_female) - vbiomass),
      vp_by_biomass = v_production / vbiomass
    )
  }

  df
}

