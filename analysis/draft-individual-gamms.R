# test run analyses
library(tidyverse)
library(mgcv)
library(gratia)

dat <- readRDS(paste0("data/all-productivity-longer-3spp.rds"))


dat4 <- filter(dat3, months == "ann" &
               agg_type == "max"  &
               lag == 0) %>%
  mutate(variable = paste(agg_type, months, variable_type, "lag =", lag))



dat4 <- filter(dat3, months == "ann" &
                 agg_type == "max" ) %>%
  group_by(species,stock,year,variable_type, months, agg_type) %>% summarise_all(mean) %>%
  mutate(variable = paste(agg_type, months, variable_type))


ggplot(dat4, aes(value, p_by_biomass)) + geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~paste(species, stock), scales = "free")


ggplot(dat4, aes(year, value, colour = paste(species, stock))) +
  geom_point()



# choose a species and stock
Species <- "Southern Rock Sole"
# Stock <- "5CD"
Stock <- "5AB"

# Species <- "Arrowtooth Flounder"
# Stock <- "Coastwide"


# choose aggregation type and lag for production
prod_agg_type <- "max"
# prod_lag <- c(0)
# prod_lag <- c(0,1)
# prod_lag <- c(0,1,2)
prod_lag <- c(0,1,2,3)
prod_lag_string <- paste0(prod_lag, collapse = "+")

# choose aggregation type for recruitment
R_agg_type <- "mean"
# R lag is currently just for spawning conditions and adult envirnment the year before spawn so determined solely by age at recruitment
# we could add rearing conditions?


d <- dat %>% filter(species == Species & stock == Stock)

d1 <- filter(d, months == "ann" & agg_type == prod_agg_type & lag %in% prod_lag) %>%
  group_by(species,stock,year,variable_type, months, agg_type) %>% summarise_all(mean) %>%
  mutate(variable = paste(agg_type, months, variable_type, "lag =", prod_lag_string))

# hist(d1$production)
m1 <- gamm(production ~
             s(value, k = 3),
           correlation=corAR1(form=~year),
           data = d1
)
summary(m1$gam)
draw(m1)
gam.check(m1$gam)


d2 <- filter(d, months != "ann" &
               agg_type == R_agg_type &
               lag == max(d$lag, na.rm = TRUE)-1) %>%
  mutate(variable = paste(agg_type, months, variable_type, "lag =", lag))
d2 <- d2[!is.na(d2["recruits"]),]


d3 <- filter(d, months == "ann" &
               agg_type == prod_agg_type &
               lag == max(d$lag, na.rm = TRUE)) %>%
  mutate(cond_variable = paste(agg_type, months, variable_type, "lag =", lag)) %>%
  dplyr::select(year, cond_value = value, cond_variable)

d2 <- left_join(d2, d3)

m2 <- gamm(rdev ~
            s(value, k = 3) +
            s(cond_value, k = 3),
          correlation=corAR1(form=~year),
          data = d2)
summary(m2$gam)
draw(m2)
gam.check(m2$gam)

