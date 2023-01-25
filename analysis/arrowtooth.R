# arrowtooth

# # save catch dataframe
# base <- readRDS("data/arrowtooth-mcmc/base.rds")
# c <- as.data.frame(base$dat$catch) %>% group_by(year) %>% summarise(catch = sum(value))
# readr::write_csv(c, "data/arrowtooth-mcmc/arrowtooth-total-catch.csv")



df <- calc_prod(
  catchfile = "data/arrowtooth-mcmc/arrowtooth-total-catch.csv",
  mcmcprefix = "data/arrowtooth-mcmc/iscam_",
  mcmcsuffix = "_mcmc.csv",
  species = "arrowtooth flounder",
  stock = "Coastwide",
  model_type = "iscam",
  start_year = 1996,
  end_year = 2021,
  recruitment_age = 1
)




plot(biomass~year, data = df)
plot(recruits~year, data = df)
plot(rdev~year, data = df)
plot(production1~year, data = df)



# catchfile <- "data/arrowtooth-mcmc/arrowtooth-total-catch.csv"
# mcmcprefix <- "data/arrowtooth-mcmc/iscam_"
# mcmcsuffix <- "_mcmc.csv"
# species <- "arrowtooth flounder"
# stock <- "Coastwide"
# start_year <- 1996
# end_year <- 2021
# recruitment_age <- 1
#
# b <- iscam_mcmc(
#    csv = paste0(mcmcprefix, "sbt", mcmcsuffix),
#    species,
#    stock,
#    start_year = start_year,
#    end_year = end_year + 1,
#    var_name = "biomass"
# )
#
# r <- iscam_mcmc(
#   csv = paste0(mcmcprefix, "rt", mcmcsuffix),
#   species,
#   stock,
#   start_year = start_year + recruitment_age,
#   end_year = end_year,
#   var_name = "recruits"
# )
#
# d <- iscam_mcmc(
#   csv = paste0(mcmcprefix, "rdev", mcmcsuffix),
#   species,
#   stock,
#   start_year = start_year + recruitment_age,
#   end_year = end_year,
#   var_name = "rdev"
# )
#
# df <- left_join(b, r) %>% left_join(d)
#
#
#
# c <- readr::read_csv(catchfile)
#
# df <- left_join(df, c) %>% select(species, stock, year, biomass, catch, recruits, rdev)
#
# df <- df %>% mutate(
#   catch_lag1 = lag(catch),
#   biomass_lag1 = lag(biomass),
#   biomass_lag2 = lag(biomass, 2),
#   production = (biomass + catch_lag1 - biomass_lag1),
#   production1 = (biomass + catch_lag1 - biomass_lag1)/biomass_lag1,
#   production2 = (biomass + catch_lag1 - biomass_lag1)/biomass_lag2
# )


