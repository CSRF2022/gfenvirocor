library(tidyverse)

df <- calc_prod(
  catchfile = "../rowan/BOR-CST-2021-Awatea-output-forRobyn-230119/BOR-CST-2021-MPD(C)-forRobyn.csv",
  mcmcprefix = "../rowan/BOR-CST-2021-Awatea-output-forRobyn-230119/BOR-CST-2021-MCMC(",
  mcmcsuffix = ")-forRobyn.csv",
  species = "bocaccio",
  stock = "Coastwide",
  model_type = "awatea",
  recruitment_age = 1
)

plot(biomass~year, data = df)
plot(recruits~year, data = df)
plot(rdev~year, data = df)
plot(production~year, data = filter(df, production < 2000))
plot(recruits~year, data = filter(df, recruits < 3000))


# catchfile <- "data/BOR-CST-2021-Awatea-output-forRobyn-230119/BOR-CST-2021-MPD(C)-forRobyn.csv"
# mcmcprefix <- "data/BOR-CST-2021-Awatea-output-forRobyn-230119/BOR-CST-2021-MCMC("
# mcmcsuffix <- ")-forRobyn.csv"
# species <- "bocaccio"
# stock <- "Coastwide"
#
#
# b <- awatea_mcmc(
#   csv = paste0(mcmcprefix, "B", mcmcsuffix),
#   species,
#   stock,
#   var_name = "biomass"
# )
#
# r <- awatea_mcmc(
#   csv = paste0(mcmcprefix, "R", mcmcsuffix),
#   species,
#   stock,
#   var_name = "recruits"
# )
#
# d <- awatea_mcmc(
#   csv = paste0(mcmcprefix, "Rdev", mcmcsuffix),
#   species,
#   stock,
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
#        catch_lag1 = lag(catch),
#        biomass_lag1 = lag(biomass),
#        biomass_lag2 = lag(biomass, 2),
#        biomass_lag3 = lag(biomass, 3),
#        production = (biomass + catch_lag1 - biomass_lag1),
#        production1 = (biomass + catch_lag1 - biomass_lag1)/biomass_lag1,
#        production2 = (biomass + catch_lag1 - biomass_lag1)/biomass_lag2,
#        production3 = (biomass + catch_lag1 - biomass_lag1)/biomass_lag3
# )
#

