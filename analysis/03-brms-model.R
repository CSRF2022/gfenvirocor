# test run analyses
library(tidyverse)
library(brms)

library(tidybayes)
library(bayesplot)

# library(mgcv)
# library(gratia)

dat <- readRDS(paste0("data/all-productivity-longer-2023-04-14.rds"))  %>%
  # temporary error, next run of get production will fix it
  filter(!(species == "Arrowtooth Flounder")) %>%
  filter(!(species == "Pacific Cod" & recruitment_age == 2))

# just plot biomass ~ time w catch, P ~ time, P/B ~ time, P/B ~ B
# no lag
dat0 <- filter(dat, months == "1to12" &
                 agg_type == "max"  &
                 variable_type == "TOB" &
                 stage == "Adult" &
                 climate_model == "roms",
                 lag == 0) %>%
  group_by(species, stock) %>%
  mutate(max_biomass = max(biomass, na.rm = TRUE),
         max_production = max(production, na.rm = TRUE)
  ) %>%
  arrange(species, stock, year)

glimpse(dat0)

# add variable name
dat0 <- dat0 %>%
  filter(!is.na(production))  %>%
  group_by(species, stock) %>%
  mutate(
  value_sd = sd(value, na.rm = TRUE),
  p_by_biomass2 = p_by_biomass * 100,
  value_c = value - mean(value, na.rm = TRUE),
  variable = paste(agg_type, months, variable_type, "lag =", lag),
  spp_stock = as.factor(paste(species, stock))
                        ) %>% ungroup()



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
  facet_wrap(~paste(species), scales = "free_y", nrow = 3) +
  ylab("Production by biomass") +
  xlab("ROMs annual maximum bottom temperature") +
  gfplot::theme_pbs()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )


ggplot(dat0, aes(value, production,
                 colour = paste(species, stock), fill = paste(species, stock))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~paste(species), scales = "free_y", nrow = 3) +
  ylab("Production") +
  xlab("ROMs annual maximum bottom temperature") +
  gfplot::theme_pbs()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )



ggplot(dat0, aes(year, p_by_biomass,
                 colour = paste(species, stock), fill = paste(species, stock))) +
  geom_line() +
  # geom_smooth(method = "lm") +
  facet_wrap(~paste(species), scales = "free_y", nrow = 3) +
  ylab("Production by biomass") +
  # xlab("ROMs annual maximum bottom temperature") +
  gfplot::theme_pbs()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )


d <- dat0 %>%
  filter(!is.na(value_c)) %>%
  select(-vbiomass,-vbiomass2, -harvest_rate, -harvest_rate2)

priors <-
  prior(normal(0, 2), class = "b") +
  prior(normal(0, 20), class = "Intercept") +
  prior(student_t(3, 0, 2.5), class = "sd") +
  prior(lkj_corr_cholesky(1), class = "L") +
  prior(student_t(3, 0, 2.5), class = "sigma")

m1 <- brm(p_by_biomass2 ~
                value_c + I(value_c^2) + (value_c + I(value_c^2)|spp_stock),
              autocor = cor_ar(form = ~ year|spp_stock, p = 1),
          chains = 2, #4
          iter = 1000, #2000
          prior = priors,
          data = d
)

m1

# brms::prior_summary(m1)

m1_lp <- brms::posterior_linpred(m1)


#
# tidybayes::spread_draws(m1, r_taxonomic_group[taxa, param])
# brms::as_draws_df


