library(ggplot2)
library(dplyr)
library(bayesdfa)
library(patchwork)

theme_set(ggsidekick::theme_sleek())
options(mc.cores = parallel::detectCores())
fig_height <- 4 * 2
fig_width <- 5 * 2

## Choose model ----
## DFA settings

ctrl <- list(adapt_delta = 0.98, max_treedepth = 12)
set_iter <- 1000
set_chains <- 4



#
# ## choose response variable
model_name <- "all-st2002-doy-d0c"

# set_group <- "immatures"
# y_label <- "Immature condition indices (excluding density dependence)"
# trend_count <- 2

#
# set_group <- "mature males"
# y_label <- "Mature male condition indices (excluding density dependence)"
# trend_count <- 3

# y_label <- "Mature male condition indices (excluding density dependence)"
#
set_group <- "mature females"
y_label <- "mature female condition indices (excluding density dependence)"
trend_count <- 2

# model_name <- "all-st2002-doy"
# set_group <- "immatures"
# y_label <- "Immature condition indices (not controlling for density dependence)"

# set_group <- "mature males"
# y_label <- "Mature male condition indices (not controlling for density dependence)"
#
# set_group <- "mature females"
# y_label <- "Mature female condition indices (not controlling for density dependence)"


if (trend_count == 3) trend_label <- "3 trends"
if (trend_count == 2) trend_label <- "2 trends"
if (trend_count == 1) trend_label <- "1 trend"

## Load and structure data ----
f <- list.files(paste0("data-generated/cond-index/",
                       model_name), pattern = ".rds", full.names = TRUE)

d <- purrr::map_dfr(f, readRDS)
dg <- filter(d, group == set_group)

ggplot(d, aes(year, est)) + facet_wrap(~paste(species, group)) +
  geom_line()

ggplot(d, aes(year, est, colour = species)) + facet_wrap(~group) +
  geom_line()

ggplot(dg, aes(year, log_est)) +
  facet_wrap(~paste(species, group)) +
  geom_line()

## wide dataframe structure
# dw <- tidyr::pivot_wider(dg, id_cols = c(year), names_from = species, values_from = est) |>
#   select(-year) |> t()
# dw

## long dataframe structure
yrs <- sort(unique(dg$year))
spp <- unique(dg$species)
yr_lu <- data.frame(year = sort(unique(dg$year)), time = seq_along(yrs))
dg <- left_join(dg, yr_lu)

dd <- tibble(
  obs = dg$log_est,
  # obs = dg$est,
  year = dg$year,
  time = dg$time,
  ts = dg$species,
  se = dg$se
)

## Weights ----
range(dg$se)
range(1/dg$se^2)

# SD/weight not SD^2/weight in model code, so try
# dd$weights <- 1 / (dd$se^2)
dd$weights <- 1 / (dd$se)
dd$weights_scaled <- dd$weights / mean(dd$weights)
hist(dd$weights)
hist(dd$weights_scaled)
mean(dd$weights_scaled)

plot((sort(dd$weights_scaled)))

# DFA without a covariate ----

m <- fit_dfa(
  y = dd,
  iter = set_iter,
  chains = set_chains,
  num_trends = trend_count,
  weights = "weights_scaled",
  estimate_process_sigma = FALSE,
  estimate_nu = FALSE,
  scale = "zscore",
  data_shape = "long",
  seed = 298191,
  control = ctrl
)
# m

range(m$monitor$Bulk_ESS)
range(m$monitor$Rhat)
# row.names(m$monitor)

checks <- m$monitor |> filter(Rhat >1)
checks |> View()

r <- rotate_trends(m)

label_yrs <- function(x) {
  x + yrs[1] - 1
}

plot_trends(r) + scale_x_continuous(label = label_yrs )

plot_loadings(r, names = spp) +
  ggtitle(paste0("DFA for ", set_group, " with no covariates"))

flip_trend <- function(rotated_modelfit, trend = 1L) {
  rflip <- rotated_modelfit
  rflip$trends_mean[trend,] <- -1 * rotated_modelfit$trends_mean[trend,]
  rflip$trends_lower[trend,] <- -1 * rotated_modelfit$trends_lower[trend,]
  rflip$trends_upper[trend,] <- -1 * rotated_modelfit$trends_upper[trend,]
  for (i in seq_len(dim(rotated_modelfit$Z_rot)[1])) {
    rflip$Z_rot[i,,trend] <- -1 * rotated_modelfit$Z_rot[i,,trend]
    rflip$trends[i,,trend] <- -1 * rotated_modelfit$trends[i,,trend]
  }
  rflip
}


which_flip <- 1L
which_flip <- 2L

rflip <- flip_trend(r, which_flip)

# to flip both
rflip <- flip_trend(rflip, which_flip)


plot_trends(r, years = yrs)
plot_trends(rflip, years = yrs)
# plot_loadings(r, names = spp)
plot_loadings(rflip, names = spp)



## Get coastwide covariates ----

## choose 1 covariate
# set_lag <- 1
# agg_var <- "mean"
# set_lag <- 0
# # agg_var <- "max" # not good fit for immature
# agg_var <- "mean"
# agg_var <- "min"
#
# if (set_lag == 0) lag_label <- ""
# if (set_lag == 1) lag_label <- "previous year's "


load("data-raw/npi_monthly.rda")
npi0 <- npi_monthly |>
  filter(month %in% c(1,2,3,4,5,6,7)) |>
  group_by(year) |> summarise(value = mean(value)) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "NPI")
hist(npi0$value)

load("data-raw/soi.rda")
soi0 <- soi |>
  filter(month %in% c(1,2,3,4,5,6)) |>
  group_by(year) |> summarise(value = mean(anomaly)) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "SOI")
hist(soi0$value)

soi1 <- soi  |> group_by(year) |> summarise(value = mean(anomaly)) |>
  mutate(year = year + 1) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "SOI (previous year)")
hist(soi1$value)


## ONI - not correlated
load("data-raw/oni.rda")
oni0 <- oni |>
  filter(month %in% c(1,2,3,4,5,6)) |>
  group_by(year) |> summarise(value = mean(anomaly)) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "ENSO (ONI)")
hist(oni0$value)

# if(var_type == "ONI" & set_lag == 0){
#   agg_var <- "mean"
#   covar <- oni0
#   var_label <- " Jan-Jun ONI (red line)"
# }

oni1 <- oni |> group_by(year) |> summarise(value = mean(anomaly)) |>
  mutate(year = year + 1) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "ONI (previous year)")
hist(oni1$value)

# if(var_type == "ONI" & set_lag == 1){
#   agg_var <- "mean"
#   var_type <- "ONI"
#   covar <- oni1
#   var_label <- " ONI (red line)"
# }
#

load("data-raw/pdo.rda")
pdo0 <- pdo |>
  filter(month %in% c(1,2,3,4,5,6)) |>
  group_by(year) |> summarise(value = mean(anomaly)) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "PDO")
hist(pdo0$value)

pdo1 <- pdo |> group_by(year) |> summarise(value = mean(anomaly)) |>
  mutate(year = year + 1) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "PDO (previous year)")
hist(pdo1$value)

# if(var_type == "PDO" & set_lag == 1){
#   agg_var <- "mean"
#   var_type <- "PDO"
#   covar <- pdo1
#   var_label <- " PDO (red line)"
# }
#
# if(var_type == "PDO" & set_lag == 0){
#   agg_var <- "mean"
#   var_type <- "PDO"
#   covar <- pdo0
#   var_label <- " Jan-Jun PDO (red line)"
# }

## primary -- positively correlated (more so w trend 2)
pp_monthly <- readRDS("data-raw/cw_primary_production.rds")
pp0 <- pp_monthly |>
  filter(month %in% c(1,2,3,4,5,6)) |>
  group_by(year) |> summarise(value = mean(value)) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "Primary production")
hist(pp0$value)

# if(var_type == "production" & set_lag == 0){
#   agg_var <- "mean"
#   var_label <- " primary production (red line)"
#   covar <- pp0
# }

## phytoplankton -- positively correlated (more so w trend 2)
pt_monthly <- readRDS("data-raw/cw_phytoplankton.rds")
pt0 <- pt_monthly |>
  filter(month %in% c(1,2,3,4,5,6)) |>
  group_by(year) |> summarise(value = mean(value)) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "Phytoplankton  ")

hist(pt0$value)

# if(var_type == "phytoplankton" & set_lag == 0){
#   agg_var <- "mean"
#   var_label <- " phytoplankton (red line)"
#   covar <- pt0
#   set_lag <- 0
# }

## SST - not correlated
sst_monthly <- readRDS("data-raw/cw_surface_temperature.rds")
sst0 <- sst_monthly |>
  filter(month %in% c(1,2,3,4,5,6)) |>
  group_by(year) |> summarise(value = mean(value)) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "SST")
hist(sst0$value)
#
# if(var_type == "SST" & set_lag == 0){
#   agg_var <- "mean"
#   var_type <- "SST"
#   var_label <- " Jan-Jun SST (red line)"
#   covar <- sst0
# }

sst1 <- sst_monthly |>
  group_by(year) |> summarise(value = mean(value)) |>
  mutate(year = year + 1) |>
  filter(year %in% yrs) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value))/ sd(value),
    type = "SST (previous year)")
hist(sst1$value)

## TOB - not correlated
tob_monthly <- readRDS("data-raw/cw_bottom_temperature.rds")
tob0 <- tob_monthly |>
  filter(month %in% c(1,2,3,4,5,6)) |>
  group_by(year) |> summarise(value = mean(value)) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "TOB")
hist(tob0$value)

tob1 <- tob_monthly |> group_by(year) |> summarise(value = mean(value)) |>
  mutate(year = year + 1) |>
  filter(year %in% yrs) |>
  mutate(
    time = seq_along(year),
    value_raw = value,
    value = (value - mean(value))/ sd(value),
    type = "TOB (previous year)")
hist(tob1$value)

## BO2 - negatively correlated with 2
do_monthly <- readRDS("data-raw/cw_bottom_oxygen.rds")
o20 <- do_monthly |>
  filter(month %in% c(1,2,3,4,5,6)) |>
  group_by(year) |> summarise(value = mean(value)) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         # type = "Bottom O2"
         value = -value,
         type = "Upwelling (O2 depletion)"
         )
hist(o20$value)

## BO2 - negatively correlated with 2
so2_monthly <- readRDS("data-raw/cw_surface_oxygen.rds")
so20 <- so2_monthly |>
  filter(month %in% c(1,2,3,4,5,6)) |>
  group_by(year) |> summarise(value = mean(value)) |>
  filter(year %in% yrs) |>
  mutate(time = seq_along(year),
         value_raw = value,
         value = (value - mean(value))/ sd(value),
         type = "Surface O2")
hist(so20$value)


# if(var_type == "TOB" & set_lag == 0){
#   agg_var <- "mean"
#   var_type <- "TOB"
#   var_label <- " Jan-Jun TOB (red line)"
#   covar <- tob0
#   set_lag <- 0
# }
# if(var_type == "TOB" & set_lag == 1){
#   agg_var <- "mean"
#   var_type <- "TOB"
#   var_label <- " previous year's TOB (red line)"
#   covar <- tob1
#   set_lag <- 1
# }
#
# arrow_tob1 <- readRDS("data/all-productivity-longer-2023-04-14-2.rds") |>
#   filter(species == "Arrowtooth Flounder",
#          variable_type == "TOB",
#          months == "1to12", climate_model == "roms")|>
#   select(year, agg_type, value, lag) |>
#   # make sure first year represented and no missing years in sequence
#   filter(year %in% yrs) |>
#   filter(agg_type == agg_var) |>
#   # filter(agg_type == "max") |>
#   # filter(lag == 0) |>
#   filter(lag == 1) |>
#   mutate(time = seq_along(year),
#          trend = 1,
#          covariate = 1,
#          # var_mean = mean(value),
#          # var_sd = sd(value),
#          # value = (value - mean(value))/sd(value)
#          ) |>
#   select(-year, -agg_type, -lag) |>
#   as.data.frame(stringsAsFactors = FALSE)
#
# # check_tob <- pro_tob1 |> rename(value1 = value) |> left_join(pro_covar)
# # plot(value~value1, data = check_tob)
# tob1$covariate <- "arrowtooth TOB"
# arrow_tob1$covariate <- "BCCM TOB"
# check_tob <- bind_rows(tob1, arrow_tob1)
# ggplot(check_tob) + geom_line(aes(time, value, colour = covariate))
#
# ## explore
# plot(pp0$value, pt0$value)
# plot(pt0$year,pt0$value)
#
# plot(pp0$year,pp0$value)
# points(pt0$value~pt0$year, col = "red")

# all_vars <- bind_rows(pdo0, pdo1, oni0, oni1)
# all_vars <- bind_rows(pdo0, oni0, soi0)
# all_vars <- bind_rows(pdo1, oni1, soi1)
# plot_trends(r, years = pro_covar$time) +
#   geom_line(data = all_vars, aes_string(x = "time", y = "value", colour = "type")) +
#   scale_color_viridis_d()
## formate an annual var

# One covariate ----
# # posthoc correlation: (simple, propagates uncertainty via 'trend_samples')
# set.seed(1)
# fake_dat <- rnorm(length(yrs), 0, 1)
# correlation <- trend_cor(r, fake_dat, trend_samples = 100)
# hist(correlation)

## alternate set up for just one at a time
# pro_covar <- covar |>
#   mutate(trend = 1,
#          value = (value - mean(value))/ sd(value),
#          covariate = 1
#   ) |>
#   select(-year, -type)|>
#   as.data.frame(stringsAsFactors = FALSE)


# Explore covariates ----

all_covs <- bind_rows(pdo0, pdo1,
                      # npi0, #soi0,
                      oni0, oni1)


all_covs <- bind_rows(#pdo0,  oni0,
                      npi0, soi0,
                      sst0,  tob0,
                      # sst1, tob1,
                      # pp0,pt0,
                      so20, o20
)

ggplot(all_covs) + geom_line(aes(time, value, colour = type))




# Select 2 covariates ----

  # pro_covar1 <- pt0 |>
  # pro_covar1 <- pp0 |> #*
  # pro_covar1 <- pdo1 |>
  pro_covar1 <- pdo0 |> #*
  # pro_covar1 <- npi0 |> #~
  # pro_covar1 <- oni0 |>
  # pro_covar1 <- oni1 |>
  # pro_covar1 <- soi0 |>
  # pro_covar1 <- sst0 |> #*
  # pro_covar1 <- tob0 |>
  # pro_covar1 <- o20 |>
  # pro_covar1 <- so20 |> #*
  mutate(trend_number = "Trend 1",
         # value = (value - mean(value))/ sd(value),
         Variable = type
  ) |>
  select(-year, -type)|>
  as.data.frame(stringsAsFactors = FALSE)

correlation <- trend_cor(r, pro_covar1$value,
                         time_window = seq_len(max(pro_covar1$time)),
                         trend = 1, trend_samples = 1000)
hist(correlation)
cor1 <- as.data.frame(correlation)

## flip didn't work here
# correlation_f <- trend_cor(rflip, pro_covar1$value,
#                          time_window = seq_len(max(pro_covar1$time)),
#                          trend = 1, trend_samples = 100)
# hist(correlation_f)


  pro_covar1b <- oni0 |>
  # pro_covar1b <- oni1 |>
  # pro_covar1b <- soi0 |>
  # pro_covar1b <- sst0 |>
  # pro_covar1b <- so20 |>
  # pro_covar1b <- pp0 |>
  # pro_covar1b <- tob0 |>
  # pro_covar1b <- o20 |>
# pro_covar1b <- so20 |> #*
    mutate(trend_number = "Trend 1",
         # value = (value - mean(value))/ sd(value),
         Variable = type
  ) |>
  select(-year, -type)|>
  as.data.frame(stringsAsFactors = FALSE)

correlation <- trend_cor(r, pro_covar1b$value,
                         time_window = seq_len(max(pro_covar1b$time)),
                         trend = 1, trend_samples = 1000)
hist(correlation)
cor1b <- as.data.frame(correlation)

covars <- pro_covar1
covarsb <- pro_covar1b

if(trend_count>1){

  # pro_covar2 <- pt0 |>
  # pro_covar2 <- pp0 |> #~
  pro_covar2 <- sst0 |>
  # pro_covar2 <- tob0 |> #~
  # pro_covar2 <- o20 |>
  # pro_covar2 <- so20 |>
  mutate(trend_number = "Trend 2",
         # value = (value - mean(value))/ sd(value),
         Variable = type
  ) |>
  select(-year, -type)|>
  as.data.frame(stringsAsFactors = FALSE)

  correlation <- trend_cor(r, pro_covar2$value,
                           time_window = seq_len(max(pro_covar2$time)),
                           trend = 2, trend_samples = 1000)
  hist(correlation)
  # (mpp <- mean(correlation))
  cor2 <- as.data.frame(correlation)


  # pro_covar2b <- pt0 |>
  # pro_covar2b <- pp0 |>
  # pro_covar2b <- sst0 |>
  # pro_covar2b <- tob0 |>
  pro_covar2b <- so20 |>
    mutate(trend_number = "Trend 2",
           # # value = (value - mean(value))/ sd(value),
           Variable = type
    ) |>
    select(-year, -type)|>
    as.data.frame(stringsAsFactors = FALSE)

  correlation <- trend_cor(r, pro_covar2b$value,
                           time_window = seq_len(max(pro_covar2b$time)),
                           trend = 2, trend_samples = 1000)
  hist(correlation)
  # (mpp <- mean(correlation))
  cor2b <- as.data.frame(correlation)

  covars <- bind_rows(pro_covar1, pro_covar2, pro_covar1b, pro_covar2b)

  covars$Variable <- factor(covars$Variable, levels = c(
    pro_covar1b$Variable[1],
    pro_covar1$Variable[1],
    pro_covar2b$Variable[1],
    pro_covar2$Variable[1]
  ))

}

if(trend_count>2){

  # pro_covar3 <- tob0 |>
  # pro_covar3 <- sst0 |>
    # pro_covar3 <- pp0 |> #~
    # pro_covar3 <- o20 |>
    pro_covar3 <- so20 |>
    mutate(trend_number = "Trend 3",
           # value = (value - mean(value))/ sd(value),
           Variable = type
    ) |>
    select(-year, -type)|>
    as.data.frame(stringsAsFactors = FALSE)

  correlation <- trend_cor(r, pro_covar2$value,
                           time_window = seq_len(max(pro_covar2$time)),
                           trend = 3, trend_samples = 1000)
  hist(correlation)
  # (mpp <- mean(correlation))
  cor3 <- as.data.frame(correlation)


  covars <- bind_rows(pro_covar1, pro_covar2, pro_covar1b,
                      pro_covar2b#, pro_covar3
                      )

  covars$Variable <- factor(covars$Variable, levels = c(
    pro_covar1b$Variable[1],
    pro_covar1$Variable[1],
    pro_covar2b$Variable[1],
    pro_covar2$Variable[1]#,
    # pro_covar3$Variable[1]
  ))

}


if(which_flip == 1L) {
  cor1$correlation <- -cor1$correlation
  cor1b$correlation <- -cor1b$correlation
}

cor1$var <- pro_covar1$Variable[1]
cor1b$var <- pro_covar1b$Variable[1]

cor1$Label <- cor1b$Label <- paste0("Trend 1 ~ ", pro_covar1$Variable[1], " / ", pro_covar1b$Variable[1])
correlations <- bind_rows(cor1, cor1b)

# correlations$var <- factor(correlations$var, levels = c(
#   pro_covar1b$Variable[1],
#   pro_covar1$Variable[1]
#   ))

if(trend_count>1){
  if(which_flip == 2L) {
    cor2$correlation <- -cor2$correlation
    cor2b$correlation <- -cor2b$correlation
  }

  cor2$var <- pro_covar2$Variable[1]
  cor2b$var <- pro_covar2b$Variable[1]
  cor2$Label <- cor2b$Label <- paste0("Trend 2 ~ ", pro_covar2$Variable[1], " / ", pro_covar2b$Variable[1])
  correlations <- bind_rows(cor1, cor2, cor1b, cor2b)

  correlations$var <- factor(correlations$var, levels = c(
    pro_covar1b$Variable[1],
    pro_covar1$Variable[1],
    pro_covar2b$Variable[1],
    pro_covar2$Variable[1]
  ))
}


# DFA summary plots ----

# (p1 <- plot_loadings(r, names = spp) +
#   ggtitle(paste0("DFA for ", set_group, " with no covariates")))


(p1 <- plot_trends(rflip) +
  scale_x_continuous(label = label_yrs ) +
    geom_line(data = covars, aes_string(x = "time", y = "value", colour = "Variable"),
            alpha = 0.8,
            linewidth = 1.5) +
    # scale_colour_brewer(palette = "Paired") +
    labs(colour = "", ylab = "Standardized value" ) +
    # theme(legend.position = c(0.2,0.85), axis.title.y = element_text())+
    # theme(legend.position = "none") +
    theme(legend.position='top', legend.justification='left', legend.direction='horizontal',
          legend.text=element_text(size=rel(0.8))) +
    # ggtitle(paste0("DFA of ", set_group, " with no covariates"))
    ggtitle(paste0("DFA of ", y_label, ""))
  )

if(trend_count>1){
  if(trend_count>2){
  (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 5, name = "Paired")[c(1,2,3,4)]))
  } else{
    if(pro_covar2$Variable[1]=="SST"){
      (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(1,2,5,6)]) )
    }else{
      (p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")) )
    }
  }
} else {
  p1 <- p1 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")[c(2,4)])
}



(p2 <- ggplot(correlations) +
  geom_histogram(aes(correlation, fill = var), alpha = 0.7, position="identity") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "darkgrey") +
  xlab("Post-hoc correlation coefficients") + ylab("Count") +
  facet_wrap(~Label#, scales = "free"
             ) +
  theme( axis.title.y = element_blank(),
         axis.ticks.y = element_blank(), axis.text.y = element_blank(),
         legend.position = "none"))

if(trend_count>1){
  if(pro_covar2$Variable[1]=="SST"){
  (p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(1,2,5,6)]) )
}else{
  (p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")) )
}
} else {
  p2 <- p2 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")[c(2,4)])
}

(p3 <- plot_loadings(rflip, names = spp)+
    theme(axis.title.y = element_blank(), legend.position = "none") )
if(trend_count>2){
  (p3 <- p3 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(2,4,5)]))
} else {
  (p3 <- p3 + scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = "Paired")[c(2,6)]))
}


wrap_elements(p1 / p2 + plot_layout(nrow = 2, widths= c(1), heights = c(1,0.5))) + wrap_elements(p3) + plot_layout(nrow = 1, widths= c(1.75, 1))
# & theme(axis.title.y = element_blank())


if(trend_count>1){
ggsave(paste0("figs/DFA-", trend_count, "trends-no-cov-",
              gsub(" ", "-", set_group), "-", model_name, "-",
              gsub(" ", "-", pro_covar1$Variable[1]), "-",
              gsub(" ", "-", pro_covar2$Variable[1]), "-2.png"),
       height = fig_height/1.65, width = fig_width*1.2)
} else {
ggsave(paste0("figs/DFA-", trend_count, "trends-no-cov-",
              gsub(" ", "-", set_group), "-", model_name, "-",
              gsub(" ", "-", pro_covar1$Variable[1]), "-2.png"),
       height = fig_height/1.65, width = fig_width*0.8)
}

# tall version
# wrap_elements(p1 / p2 + plot_layout(nrow = 2, widths= c(1), heights = c(1,0.5))) / wrap_elements(p3) + plot_layout(nrow = 2, widths= c(1), heights = c(1.2,1))
# # & theme(axis.title.y = element_blank())
#
# ggsave(paste0("figs/DFA-sum-",  trend_count, "trends-no-cov-lag", set_lag, "-", agg_var, var_type, ".png"),
#        height = fig_height, width = fig_width/1.5)


# DFA fit plot ----

# plot_fitted(m2, names = spp)

source("R/dfa_fitted.R")
df1 <- dfa_fitted(m, conf_level = 0.95, names = spp)
# cols <- viridis::viridis(length(unique((df$ID))), end = 0.8)

(p4 <- ggplot(df1) +
    geom_ribbon(aes_string(x = "time",
                           ymin = "lower", ymax = "upper"
                           #, fill = "ID"
                           ),
                alpha = 0.4) +
    geom_line(aes_string(x = "time", y = "estimate"#, colour = "ID"
                         )) +
    # # geom_line(data = pro_covar, aes_string(x = "time", y = "value"), colour = "red") +
    # geom_line(data = covars, aes_string(x = "time", y = "value", colour = "Variable"),
    #           alpha = 0.7,
    #           linewidth = 0.5) +
    # # scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")[c(2,4)]
    # # ) +
    geom_point(aes_string(x = "time", y = "y"),
               #col = "blue",
               size = 0.5, alpha = 0.7) +
    # scale_color_manual(values = cols) +
    # scale_fill_manual(values = cols) +
    facet_wrap("ID"#, scales = "free_y"
               ) +
    xlab("Time") + ylab(y_label) +
    scale_x_continuous(label = label_yrs ) +
    # theme(legend.position = c(.87,.1)) +
    # theme(legend.position = "none") +
    # labs(colour= "Species", fill = "Species")+
    ggtitle(paste0("DFA ", y_label, " with ", trend_label))
)

if(trend_count>1){
  (p4 <- p4 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")) )
} else {
  p4 <- p4 + scale_colour_manual(values = RColorBrewer::brewer.pal(n = 4, name = "Paired")[c(2,4)])
}


ggsave(paste0("figs/DFA-fits-",  trend_count, "trends-no-cov-",
              gsub(" ", "-", set_group), "-", model_name, ".png"),
       height = fig_height*0.7, width = fig_width)
# ggsave(paste0("figs/DFA-",  trend_count, "trends-no-cov-lag", set_lag, "-", agg_var, var_type, ".png"),
#        height = fig_height, width = fig_width)



# # # spgehetti
# (p5 <- ggplot(df) +
#   geom_line(aes_string(x = "time", y = "y", group = "ID"),
#             color = "grey50", linewidth = 0.5) +
#   geom_line(aes_string(x = "time", y = "estimate",
#                        group = "ID", color = "ID"), linewidth = 1.2) +
#   scale_color_manual(values = cols) +
#   xlab("Time") # + theme(legend.position = "none")
#   )
#



# DFA with covariate ----
# with process covariate when fitting:
# https://cran.r-project.org/web/packages/bayesdfa/vignettes/a3_covariates.html

# pro_covar <- data.frame(
#   time = seq_along(yrs),
#   trend = 1,
#   covariate = 1,
#   value = rnorm(length(yrs)), # make sure this is standardized!
#   stringsAsFactors = FALSE
# )
#
# mcov <- fit_dfa(
#   y = dw,
#   iter = 1000,
#   chains = 4,
#   num_trends = 1,
#   pro_covar = pro_covar,
#   scale = "zscore",
#   control = ctrl
# )

#
# mcov <- fit_dfa(
#   y = dd2,
#   iter = set_iter,
#   chains = set_chains,
#   num_trends = trend_count,
#   weights = "weights_scaled",
#   estimate_process_sigma = TRUE,
#   estimate_nu = TRUE,
#   scale = "zscore",
#   pro_covar = pro_covar,
#   data_shape = "long",
#   seed = 298191,
#   control = ctrl
# )
#
# range(mcov$monitor$Bulk_ESS)
# range(mcov$monitor$Rhat)
# names(mcov$samples_permuted)
# rc <- rotate_trends(mcov)
#
# hist(mcov$samples_permuted$b_pro[,1,1])
#
# plot_trends(rc, years = yrs) +
#   geom_line(data = pdo0, aes_string(x = "year", y = "value"), colour = "red")


plot_loadings(rc, names = spp) +
  ggtitle(paste0("DFA for ", set_group, " with ", lag_label, agg_var, var_type))

# plot_fitted(m2, names = spp)
df2 <- dfa_fitted(mcov, conf_level = 0.95, names = spp)
cols <- viridis::viridis(length(unique((df$ID))), end = 0.8)

# # # spgehetti
# (p1 <- ggplot(df) +
#     geom_line(aes_string(x = "time", y = "y", group = "ID"),
#               color = "grey50", linewidth = 0.5) +
#     geom_line(aes_string(x = "time", y = "estimate",
#                          group = "ID", color = "ID"), linewidth = 1.2) +
#     scale_color_manual(values = cols) +
#     xlab("Time") # + theme(legend.position = "none")
# )

(p2 <- ggplot(df2) +
    geom_ribbon(aes_string(x = "time",
                           ymin = "lower", ymax = "upper", fill = "ID"),
                alpha = 0.4) +
    geom_line(aes_string(x = "time", y = "estimate", colour = "ID")) +
    geom_line(data = pro_covar, aes_string(x = "time", y = "value"), colour = "red") +
    geom_point(aes_string(x = "time", y = "y"),
               #col = "blue",
               size = 0.5, alpha = 0.7) +
    scale_color_manual(values = cols) +
    scale_fill_manual(values = cols) +
    facet_wrap("ID" #, scales = "free_y"
               ) +
    xlab("Time") + ylab(y_label) +
    theme(legend.position = "none") +
    # labs(colour= "Species", fill = "Species")+
    ggtitle(paste0("DFA with ",  trend_label, " and ", lag_label, agg_var, var_label))
)

ggsave(paste0("figs/DFA-",  trend_count, "trends-lag", set_lag, "-", agg_var, var_type, ".png"),
       height = fig_height, width = fig_width)




# # plot_fitted(m2, names = spp)
# df <- dfa_fitted(m2, conf_level = 0.95, names = spp)
# cols <- viridis::viridis(length(unique((df$ID))), end = 0.8)
#
# (p <- ggplot(df) +
#     geom_ribbon(aes_string(x = "time",
#                            ymin = "lower", ymax = "upper"),
#                 alpha = 0.4) +
#   geom_line(aes_string(x = "time", y = "estimate")) +
#   geom_point(aes_string(x = "time", y = "y"), col = "red",
#              size = 0.5, alpha = 0.4) +
#   facet_wrap("ID", scales = "free_y") +
#   xlab("Time") + ylab("")
#   )

## model selection not working
# m3 <- find_dfa_trends(
#   y = dd,
#   iter = 1000,
#   chains = 6,
#   weights = "weights_scaled",
#   estimate_process_sigma = TRUE,
#   scale = "zscore",
#   data_shape = "long",
#   seed = 298191,
#   kmin = 1, kmax = 3,
#   # compare_normal = TRUE,
#   # variance = c("equal", "unequal"),
#   control = ctrl
# )
# m3


