# pacific cod

df <- calc_prod(
  catchfile = "data/PCOD/pcod-wcvi-catch.csv",
  mcmcprefix = "data/PCOD/iscam_",
  mcmcsuffix = "_mcmc_WCVI.csv",
  species = "pacific cod",
  stock = "WCVI",
  model_type = "iscam",
  start_year = 1956,
  end_year = 2020,
  recruitment_age = 2
)

plot(biomass~year, data = df)
plot(recruits~year, data = df)
plot(rdev~year, data = df)
plot(production3~year, data = df)


# catchfile <- "data/PCOD/pcod-catch-WCVI.csv"
# mcmcprefix <- "data/PCOD/iscam_"
# mcmcsuffix <- "_mcmc_WCVI.csv"
# species <- "pacific cod"
# stock <- "WCVI"
# start_year <- 1956
# end_year <- 2020
# recruitment_age <- 2
#
# b <- iscam_mcmc(
#   csv = paste0(mcmcprefix, "sbt", mcmcsuffix),
#   species,
#   stock,
#   start_year = start_year,
#   end_year = end_year + 1,
#   var_name = "biomass"
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
# c <- readr::read_csv(catchfile)
#
# df <- left_join(df, c) %>% select(species, stock, year, biomass, catch, recruits, rdev)
#
# df <- df %>% mutate(
#   catch_lag1 = lag(catch),
#   biomass_lag1 = lag(biomass),
#   biomass_lag2 = lag(biomass, 2),
#   biomass_lag3 = lag(biomass, 3),
#   production = (biomass + catch_lag1 - biomass_lag1),
#   production1 = (biomass + catch_lag1 - biomass_lag1)/biomass_lag1,
#   production2 = (biomass + catch_lag1 - biomass_lag1)/biomass_lag2,
#   production3 = (biomass + catch_lag1 - biomass_lag1)/biomass_lag3
# )

