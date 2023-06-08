# test run analyses
library(tidyverse)
library(brms)
library(modelr)
library(tidybayes)
library(bayesplot)

date_stamp <- Sys.Date()

# dat <- readRDS(paste0("data/all-productivity-longer-2023-04-24.rds"))  %>%
dat <- readRDS(paste0("data/all-productivity-longer-2023-06-07.rds"))  %>%
  filter(!(species == "Pacific Cod" & recruitment_age == 3))

# select hypothesis
climate_model <- "ROMS"

response <- "Production"
# response <- "Recruitment"


# # climate_component <- c("TOB")
# # climate_component <- c("SST")
# climate_component <- c("temp")
# summary_type <- "max"

# climate_component <- c("O2")
climate_component <- c("salinity")

summary_type <- "min"
# summary_type <- "mean"
# summary_type <- "max"

if(response == "Recruitment"){
  life_stage <- "Eggs/gestation"
  life_stage2 <- "eggs/gestation"
  life_stage3 <- "eggs"
  # life_stage <- "Larval"
  # life_stage2 <- life_stage3 <- "larval"
} else {
  life_stage <- "Adult"
  life_stage2 <- life_stage3 <- "adult"
  # life_stage <- "Eggs/gestation"
  # life_stage2 <- "eggs/gestation"
  # life_stage3 <- "eggs"
  # life_stage <- "Larval"
  # life_stage2 <- life_stage3 <- "larval"
}

if (climate_component == "O2" & response == "Recruitment"){
  life_stage <- "Eggs/gestation"
  life_stage2 <- "eggs/gestation"
  life_stage3 <- "eggs"
}

if (climate_component == "salinity" & response == "Recruitment"){
  life_stage <- "Eggs/gestation"
  life_stage2 <- "eggs/gestation"
  life_stage3 <- "eggs"
}

climate_component_name <- climate_component

if(climate_component == "TOB" & summary_type == "max" & climate_model == "ROMS") {
  if(life_stage == "Adult"){
    x_label <- "ROMS annual maximum bottom temperature deviation"
  } else {
    x_label <- paste("ROMS maximum bottom temperature deviation at", life_stage2, "stage")
  }
}

if(climate_component == "SST" & summary_type == "max" & climate_model == "ROMS") {
  if(life_stage == "Adult"){
    x_label <- "ROMS annual maximum sea surface temperature deviation"
  } else {
    x_label <- paste("ROMS maximum sea surface temperature deviation at", life_stage2, "stage")
  }
}

if(climate_component == "temp" & summary_type == "max" & climate_model == "ROMS") {
  climate_component <- c("TOB", "SST")
  if(life_stage == "Adult"){
    x_label <- "ROMS annual maximum temperature deviation"
  } else {
    x_label <- paste("ROMS maximum temperature deviation at", life_stage2, "stage")
  }
}

if(climate_component == "O2" & summary_type == "min" & climate_model == "ROMS") {
  if(life_stage == "Adult"){
    x_label <- "ROMS annual minimum seafloor O2 deviation in mmol/m3"
  } else {
    x_label <- paste("ROMS minimum seafloor O2 deviation in mmol/m3 at", life_stage2, "stage")
  }
}

if(climate_component == "O2" & summary_type == "mean" & climate_model == "ROMS") {
  if(life_stage == "Adult"){
    x_label <- "ROMS annual minimum seafloor O2 deviation in mmol/m3"
  } else {
    x_label <- paste("ROMS mean seafloor O2 in deviation mmol/m3 at", life_stage2, "stage")
  }
}

if(climate_component == "O2" & summary_type == "max" & climate_model == "ROMS") {
  if(life_stage == "Adult"){
    x_label <- "ROMS annual minimum seafloor O2 deviation in mmol/m3"
  } else {
    x_label <- paste("ROMS max seafloor O2 in deviation mmol/m3 at", life_stage2, "stage")
  }
}

if(climate_component == "salinity" & summary_type == "min" & climate_model == "ROMS") {
  if(life_stage == "Adult"){
    x_label <- "ROMS annual minimum seafloor salinity deviation"
  } else {
    x_label <- paste("ROMS minimum seafloor salinity deviation", life_stage2, "stage")
  }
}

if(climate_component == "salinity" & summary_type == "max" & climate_model == "ROMS") {
  if(life_stage == "Adult"){
    x_label <- "ROMS annual maximum seafloor salinity deviation"
  } else {
    x_label <- paste("ROMS maximum seafloor salinity deviation", life_stage2, "stage")
  }
}

if(life_stage == "Adult") {

  # # could use a mean of all lags, but for now just just using current year
  # dat0 <- filter(dat, months == "ann" &
  #                  agg_type == "max" ) %>%
  #   drop_na() %>%
  #   group_by(species, stock, year, variable, variable_type, months, agg_type) %>%
  #   summarise_all(mean) %>%
  #   mutate(variable = paste(agg_type, months, variable_type))


  dat0 <- filter(dat,
                 agg_type == summary_type &
                 # variable_type %in% climate_component &
                 stage == life_stage &
                 climate_model == "roms",
                 lag == 0)

} else {
  dat0 <- filter(dat,
                 agg_type == summary_type &
                 variable_type %in% climate_component &
                 stage == life_stage &
                 climate_model == "roms",
                 lag == recruitment_age)
}

if(response == "Production"){
  dat0$response <- dat0$p_by_biomass * 100
  dat0$response_raw <- dat0$production
  dat0$biomass <- dat0$vbiomass
  y_label <- "% change in surplus production per unit biomass"
  y_label_raw <- "Surplus production"
}

if(response == "Recruitment"){
  dat0$response <- log(dat0$recruits/dat0$biomass_for_recruits)
  dat0$response_raw <- dat0$recruits
  dat0$biomass <- dat0$biomass_for_recruits
  y_label_raw <- "Total recruits"
  y_label <- "Recruits per unit spawning biomass (log space)"
}

hist(dat0$response, breaks = 60)

dat0 <- dat0 %>%
  group_by(species, stock) %>%
  mutate(
    max_biomass = max(biomass, na.rm = TRUE),
    max_production = max(production, na.rm = TRUE)
  ) %>%
  arrange(species, stock, year)%>%
  filter(!is.na(response))  %>%
  group_by(species, stock) %>%
  mutate(
    value_mean = mean(value, na.rm = TRUE),
    value_sd = sd(value, na.rm = TRUE),
    value_c = value - value_mean,
    variable = paste(agg_type, months, variable_type, "lag =", lag),
    spp_stock = as.factor(paste(species, stock)),
    Stock = paste0(gsub(" ", ".", species), ".", stock)
  ) %>% ungroup()

# if(climate_component == "O2") {
#   dat0 <- dat0 %>%
#     mutate(
#       value_sd = sd(value, na.rm = TRUE),
#       value_c = value - mean(value, na.rm = TRUE)
#     )
# }


ggplot(dat0, aes(value, response_raw,
                 colour = paste(species, stock), fill = paste(species, stock))) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~paste(species), scales = "free_y", nrow = 3) +
  ylab(y_label_raw) +
  xlab(x_label) +
  gfplot::theme_pbs()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
        )

ggplot(dat0, aes(value, response,
                 colour = paste(species, stock), fill = paste(species, stock))) +
  geom_point() +
  # geom_smooth(method = "lm") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  facet_wrap(~paste(species), scales = "free", nrow = 3) +
  ylab(y_label) +
  xlab(x_label) +
  gfplot::theme_pbs()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )
ggsave(paste0("figs/", response, "-rate-", life_stage3, "-", climate_component_name,
              "-", summary_type, "-corplot.png"),
       width = 8, height = 5)

ggplot(dat0, aes(value_c, response,
                 colour = paste(species, stock), fill = paste(species, stock))) +
  geom_point() +
  geom_smooth(method = "lm") +
  # geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
  facet_wrap(~paste(species), scales = "free_y", nrow = 3) +
  ylab(y_label) +
  xlab(x_label) +
  # coord_cartesian(expand = FALSE) +
  gfplot::theme_pbs()+
  theme(legend.position = "none",
        # axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
  )

ggsave(paste0("figs/", response, "-rate-", life_stage3, "-", climate_component_name, "-", summary_type, "-deviations-corplot.png"),
       width = 9, height = 5)


d <- dat0 %>%
  filter(!is.na(value_c)) %>%
  filter(!is.na(response)) %>%
  select(-vbiomass,-vbiomass2, -harvest_rate, -harvest_rate2)


if(length(unique(d$variable_type)) == 1) {
# if(unique(d$variable_type) == "O2") {
#   priors1 <-
#     prior(normal(0, 2), class = "b") +
#     prior(normal(0, 0.1), coef = "Ivalue_cE2") +
#     prior(normal(0, 20), class = "Intercept") +
#     prior(student_t(3, 0, 2.5), class = "sd") +
#     prior(student_t(3, 0, 0.1), class = "sd", group = "spp_stock", coef = "Ivalue_cE2") +
#     prior(lkj_corr_cholesky(1), class = "L") +
#     prior(student_t(3, 0, 2.5), class = "sigma")
# } else {
  priors1 <-
    prior(normal(0, 2), class = "b") +
    prior(normal(-0.15, 0.15), coef = "Ivalue_cE2") +
    prior(normal(0, 20), class = "Intercept") +
    prior(student_t(3, 0, 2.5), class = "sd") +
    prior(lkj_corr_cholesky(1), class = "L") +
    prior(student_t(3, 0, 2.5), class = "sigma")
# }

m1 <- brm(
  response ~
    # value_mean*
    value_c + I(value_c^2) +
    # (value_c + I(value_c^2)|group) +
    (value_c + I(value_c^2)|spp_stock) ,
              autocor = cor_ar(form = ~ year|spp_stock, p = 1),
          chains = 3, #4
          iter = 2000, #2000
          prior = priors1,
          # sample_prior = "only",
          data = d
)

} else {

#   priors1 <-
#     prior(normal(0, 2), class = "b") +
#     prior(normal(0, 20), class = "Intercept") +
#     prior(student_t(3, 0, 2.5), class = "sd") +
#     prior(lkj_corr_cholesky(1), class = "L") +
#     prior(student_t(3, 0, 2.5), class = "sigma")
#
#
#   m1 <- brm(
#   response ~
#     value_c +
#     # (value_c|group) +
#     (value_c|spp_stock) ,
#   autocor = cor_ar(form = ~ year|spp_stock, p = 1),
#   chains = 2, #4
#   iter = 1000, #2000
#   prior = priors1,
#   data = d
# )

  priors1 <-
    prior(normal(0, 2), class = "b") +
    prior(normal(-0.15, 0.15), coef = "Ivalue_cE2") +
    prior(normal(0, 20), class = "Intercept") +
    prior(student_t(3, 0, 2.5), class = "sd") +
    prior(lkj_corr_cholesky(1), class = "L") +
    prior(student_t(3, 0, 2.5), class = "sigma")

  # d$value_c <- d$value
  m1 <- brm(
    response ~ variable_type +
      value_c + I(value_c^2) +
      # (value_c + I(value_c^2)|group) +
      (value_c + I(value_c^2)|spp_stock) ,
    autocor = cor_ar(form = ~ year|spp_stock, p = 1),
    chains = 3, #4
    iter = 2000, #2000
    prior = priors1,
    # sample_prior = "only",
    data = d
  )

}

saveRDS(m1, paste0("models/", response, "-rate-", life_stage3, "-",
                   climate_component_name, "-",summary_type, "-", date_stamp, ".rds"))


# m2 <- brm(
#   response_raw ~ #log(biomass) +
#     value_c + I(value_c^2) +
#     # (value_c + I(value_c^2)|group) +
#     (log(biomass) + value_c + I(value_c^2)|spp_stock),
#   autocor = cor_ar(form = ~ year|spp_stock, p = 1),
#   chains = 2, #4
#   iter = 1000, #2000
#   prior = priors1,
#   data = d
# )
#
# saveRDS(m2, paste0("models/", response, "-raw-", life_stage3, "-", climate_component_name, "-",summary_type, "-", date_stamp, ".rds"))

prior_summary(m1)

m <- m1
# m <- m2
# get_variables(m)

data_labels <- select(d, species, stock, spp_stock, Stock, group, value_c) %>%
  group_by(spp_stock) %>%
  mutate(group = ifelse(group %in% c("Rockfish-Slope", "Rockfish-Shelf"), "Rockfish", group),
         group = ifelse(group %in% c("Round"), "Cods and allies", group),
         min_value = min(value_c, na.rm = TRUE),
         max_value = max(value_c, na.rm = TRUE)
         ) %>% select(-value_c) %>% distinct()



plot_quadratic_variable_effect <- function(m, labels = data_labels, x_lab = x_label, y_lab = y_label) {
b_j <- as_draws_matrix(m, variable = "r_spp_stock")
b1_j <- b_j[,grepl("value_c]$", colnames(b_j))]
b1 <- as_draws_matrix(m, variable = "b_value_c")

# if(climate_component != "O2"){
b2_j <- b_j[,grepl("value_cE2]$", colnames(b_j))]
b2 <- as_draws_matrix(m, variable = "b_Ivalue_cE2")
# }


stocks <- gsub(",value_c]", "", colnames(b1_j))
stocks <- gsub("r_spp_stock\\[", "", stocks)

# could add in intercepts too
x <- seq(min(d$value_c), max(d$value_c), length.out = 50)
post <- list()
for (j in 1:ncol(b1_j)) {
  post[[j]] <- matrix(NA_real_, nrow = length(x), ncol = nrow(b1))
  for (i in 1:nrow(b1)) {
    # if(climate_component != "O2"){
    post[[j]][,i] <- (as.numeric(b1[i,]) + as.numeric(b1_j[i,j])) * x + (as.numeric(b2[i,]) + as.numeric(b2_j[i,j])) * x^2
    # } else {
      # post[[j]][,i] <- (as.numeric(b1[i,]) + as.numeric(b1_j[i,j])) * x
    # }
  }
}
names(post) <- stocks
out <- purrr::map_dfr(post, function(.x) {
  lwr <- apply(.x, 1, quantile, probs = 0.1)
  med <- apply(.x, 1, quantile, probs = 0.5)
  upr <- apply(.x, 1, quantile, probs = 0.9)
  data.frame(x = x, lwr, med, upr)
}, .id = "Stock")

out <- out |> left_join(labels) |> group_by(spp_stock) |> filter(x >= min_value & x <= max_value) |> ungroup()
# browser()
ymin <- min(out$med, na.rm = TRUE)
ymax <- max(out$med, na.rm = TRUE)

out |>
  # filter(group == "Flatfish") %>%
  ggplot(aes(x, y = med,
             ymin = lwr, ymax = upr,
             )) +
  # geom_line(aes(colour = spp_stock)) +
  geom_ribbon(aes(fill = spp_stock), alpha = 0.03) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), aes(colour = spp_stock), se = FALSE) +
  facet_wrap(~group,
             scales = "free_x",
             ncol = 3) +
  coord_cartesian(ylim = c(ymin + ymin*0.01, ymax + ymax*0.01)) +
  labs(y = y_lab, x = x_lab, colour = "", fill = ""
      ) +
  gfplot::theme_pbs() + theme(
    strip.text = element_text(size = 12),
    legend.position = "bottom")
#
#
# p2 <- out |> left_join(labels) |>
#   filter(group == "Rockfish") %>%
#   ggplot(aes(x, y = med,
#              # ymin = lwr, ymax = upr,
#   )) +
#   geom_line(aes(colour = Stock)) +
#   # geom_smooth(method = "lm", aes(colour = spp_stock), se = FALSE) +
#   # geom_ribbon(aes(fill = spp_stock), alpha = 0.01) +
#   # facet_wrap(~group, ncol = 1) +
#   labs(y = y_lab, x = x_lab, colour = ""
#   ) +
#   gfplot::theme_pbs() + theme(legend.position = c(0.3, 0.75))
#
# p3 <- out |> left_join(labels) |>
#   filter(group == "Cods and allies") %>%
#   ggplot(aes(x, y = med,
#              # ymin = lwr, ymax = upr,
#   )) +
#   geom_line(aes(colour = Stock)) +
#   # geom_smooth(method = "gam", aes(colour = spp_stock), se = FALSE) +
#   # geom_ribbon(aes(fill = spp_stock), alpha = 0.01) +
#   # facet_wrap(~group, ncol = 1) +
#   labs(y = y_lab, x = x_lab, colour = ""
#   ) +
#   gfplot::theme_pbs() + theme(legend.position = c(0.3, 0.75))
#
# p1 + p2 + p3 + patchwork::plot_layout()
# p1
# theme(legend.position = c(0.3, 0.35))
}

plot_quadratic_variable_effect(m)

ggsave(paste0("figs/", response, "-rate-", life_stage3, "-", climate_component_name, "-",summary_type, "-", date_stamp, ".png"),
       width = 8, height = 5.5)

