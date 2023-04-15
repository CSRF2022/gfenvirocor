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
#' @param proportion_catch default to `0.5`; used to scale catch to match female biomass.
#'   Use `1` for two sex biomass estimates.
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
                      proportion_catch = 0.5,
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

    c <- c %>% rename(year = `...1`)
    c$catch <- rowSums(c[,2:ncol(c)])


    u <- awatea_mcmc(
      csv = paste0(mcmcprefix, "U", mcmcsuffix),
      species,
      stock,
      var_name = "harvest_rate"
    )

    # browser()
    if(exists("u")) {
      if(max(u$fleet, na.rm = TRUE) == 1 | min(u$fleet, na.rm = TRUE)>1){
      u$fleet <- NA

     if(!exists("v")) {
      v <- left_join(c, u)
      v$vbiomass <- v$catch/v$harvest_rate
     }
     } else {
       if(!exists("v")) {
        # warning("More than one fleet, so vulnerable biomass not calculated from u.")
         # browser()
         c2 <- c
         names(c2)[2] <- "fleet_1_catch"
         v <- filter(u, fleet == 1)
         c2 <- select(c2, year, fleet_1_catch)
         v <- left_join(v, c2)
         v$vbiomass <- v$fleet_1_catch/v$harvest_rate
         v <- select(v, year, fleet, vbiomass)

       }
      }
    }
  }

  if (model_type == "landmark") {
    df <- c %>% mutate(species = species,
                       stock = stock,
                       year = Year,
                       catch = Landings*proportion_catch, # tonnes
                       biomass = SSBt_p50*1000, # tonnes
                       vbiomass = NA,
                       vbiomass2 = NA,
                       harvest_rate = NA,
                       harvest_rate2 = NA,
                       recruits = Rt_p50*1000, # 1000 fish
                       rdev = Rdev_p50
                       )

  } else {

    df <- left_join(c, b) %>%
      left_join(r) %>%
      left_join(d)

  if(exists("v")){

    if(max(v$fleet, na.rm = TRUE) == 1 | min(v$fleet, na.rm = TRUE)>1){
      v <- v %>% select(year, vbiomass)
      df <- df %>% left_join(v)
      df$vbiomass2 <- NA
    } else {
    # for now this code only keeps vulnerable biomass for the first 2 fleets
    v2 <- filter(v, fleet == 2) %>% rename(vbiomass2 = vbiomass) %>% select(-fleet)
    v <- filter(v, fleet == 1) %>% select(-fleet)
    df <- df %>% left_join(v) %>% left_join(v2)
    }

  } else {
  df$vbiomass <- NA
  df$vbiomass2 <- NA
  }

  if(exists("u")){
    if(max(u$fleet, na.rm = TRUE)>1) {
      u2 <- filter(u, fleet == 2) %>% rename(harvest_rate2 = harvest_rate) %>% select(-fleet)
      u <- filter(u, fleet == 1) %>% select(-fleet)
      df <- df %>% left_join(u)%>% left_join(u2)
    } else {

    u <- u %>% select(year, harvest_rate)
    df <- df %>% left_join(u)
    df$harvest_rate2 <- NA
    }
  } else {
    df$harvest_rate <- NA
    df$harvest_rate2 <- NA
  }
  }

  df <- df %>%
    select(species, stock, year, biomass, vbiomass, vbiomass2,
           catch, harvest_rate, harvest_rate2, recruits, rdev) %>%
    mutate(model_type = model_type,
           prop_catch = proportion_catch,
           recruitment_age = recruitment_age,
           maturity_age = maturity_age
    )

  recruitment_lag <- as.integer(round(maturity_age-recruitment_age))

  if (recruitment_age == 0) {
    df <- df %>% mutate(
      biomass_lead1 = lead(biomass),
      # production = (biomass_lead1 + catch - biomass),
      # p_by_biomass = production / biomass,
      recruits_lag = lag(recruits, recruitment_lag[1]),
      biomass_for_recruits = lag(biomass, 0),
      biomass_lag1 = lag(biomass, 1)
      # Prod_by_biomass1 = production / biomass_lag1,
    )
  }

  if (recruitment_age == 1) {
    df <- df %>% mutate(
      biomass_lead1 = lead(biomass),
      # production = (biomass_lead1 + catch - biomass),
      # p_by_biomass = production / biomass,
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
      # production = (biomass_lead1 + catch - biomass),
      # p_by_biomass = production / biomass,
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
      # production = (biomass_lead1 + catch - biomass),
      # p_by_biomass = production / biomass,
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
      production = (vbiomass_lead1 + catch - vbiomass),
      p_by_biomass = production / vbiomass
    )
  }

  df
}

