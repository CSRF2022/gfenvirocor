# test run analyses
library(tidyverse)
library(mgcv)
library(gratia)

dat <- readRDS(paste0("data/all-productivity-longer-2023-03-21.rds"))

# just plot biomass ~ time w catch, P ~ time, P/B ~ time, P/B ~ B
# no lag
dat0 <- filter(dat, months == "ann" &
                 agg_type == "max"  &
                 lag == 0) %>%
  group_by(species, stock) %>%
  mutate(max_biomass = max(biomass, na.rm = TRUE),
         max_production = max(production, na.rm = TRUE)
  ) %>%
  arrange(species, stock, year)

glimpse(dat0)

# add variable name
dat0 <- dat0 %>% mutate(variable = paste(agg_type, months, variable_type, "lag =", lag))



# # mean of all lags
# dat1 <- filter(dat, months == "ann" &
#                  agg_type == "max" ) %>%
#   drop_na() %>%
#   group_by(species, stock, year, variable, variable_type, months, agg_type) %>%
#
#   summarise_all(mean) %>%
#   mutate(variable = paste(agg_type, months, variable_type))


ggplot(dat0, aes(value, p_by_biomass,
                 colour = paste(species, stock), fill = paste(species, stock))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~paste(species, stock), scales = "free_y", nrow = 3) +
  ylab("Production by biomass") +
  xlab("ROMs annual maximum bottom temperature") +
  gfplot::theme_pbs()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )

ggsave("figs/Flatfish-production-by-temp.png", width = 4, height = 6)


ggplot(dat0, aes(year, value, colour = paste(species, stock))) +
  geom_line() +
  labs(colour = "Stock") +
  xlab("Year") +
  ylab("ROMs annual maximum bottom temperature") +
  gfplot::theme_pbs() +
  theme(legend.position = c(0.2, 0.2))

ggsave("figs/Flatfish-ann-temp-through-time.png", width = 5.5, height = 4)




dat1 <- filter(dat, months != "ann" &
                 variable_type == "O2" &
                 agg_type == "min") %>%
  group_by(species, stock) %>%
  filter(lag == recruitment_age) %>%
  mutate(max_biomass = max(biomass, na.rm = TRUE),
         max_production = max(production, na.rm = TRUE)
  ) %>%
  arrange(species, stock, year)


# add variable name
dat1 <- dat1 %>% mutate(variable = paste(agg_type, months, variable_type, "lag =", lag))


ggplot(dat1, aes(value, recruits/biomass_for_recruits,
                 colour = paste(species, stock), fill = paste(species, stock))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~paste(species, stock), scales = "free_y", nrow = 3) +
  ylab("Recruits per unit biomass") +
  xlab("Minimum oxygen during spawning") +
  gfplot::theme_pbs()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

ggsave("figs/Flatfish-recruits-by-02.png", width = 4, height = 6)


ggplot(dat1, aes(year, value, colour = paste(species, stock))) +
  geom_line() +
  labs(colour = "Stock") +
  xlab("Year") +
  ylab("Minimum oxygen during spawning") +
  gfplot::theme_pbs() +
  theme(legend.position = c(0.2, 0.2))

ggsave("figs/Flatfish-spawing-O2-through-time.png", width = 5.5, height = 4)


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
  # drop_na() %>%
  select(-variable) %>%
  group_by(species, stock, year, variable_type, months, agg_type) %>%
  summarise_all(mean) %>%
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

