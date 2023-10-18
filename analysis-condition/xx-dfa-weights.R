library(ggplot2)
library(dplyr)
library(bayesdfa)
theme_set(theme_light())
options(mc.cores = parallel::detectCores())

# fix path:
f <- list.files("~/Downloads/cond-no-dens 2/", pattern = ".rds", full.names = TRUE)

d <- purrr::map_dfr(f, readRDS)

ggplot(d, aes(year, est)) + facet_wrap(~paste(species, group)) +
  geom_line()

ggplot(d, aes(year, est, colour = species)) + facet_wrap(~group) +
  geom_line()

dg <- filter(d, group == "mature females")

ggplot(dg, aes(year, log_est)) + facet_wrap(~paste(species, group)) +
  geom_line()
#
# dw <- tidyr::pivot_wider(dg, id_cols = c(year), names_from = species, values_from = est) |>
#   select(-year) |> t()
# dw
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
dd$weights <- 1 / (dd$se^2)
dd$weights_scaled <- dd$weights / mean(dd$weights)
hist(dd$weights_scaled)
hist(dd$weights_scaled)
mean(dd$weights_scaled)

ctrl <- list(adapt_delta = 0.98, max_treedepth = 12)

m <- fit_dfa(
  y = dd,
  iter = 500,
  chains = 1,
  num_trends = 1,
  weights = "weights_scaled",
  scale = "zscore",
  data_shape = "long",
  seed = 298191,
  control = ctrl
)
m

r <- rotate_trends(m)
plot_trends(r, years = yrs)
plot_loadings(r, names = spp)
# plot_fitted(m, names = spp) # bug with weights or long format!?

# > 1 trend are not fitting well:
# m2 <- fit_dfa(
#   y = dw,
#   iter = 1000,
#   chains = 1,
#   num_trends = 2,
#   control = ctrl
# )
# m2
#
# r <- rotate_trends(m2)
# plot_trends(r, years = yrs)
# plot_loadings(r, names = spp)
# plot_fitted(m2)

# posthoc correlation: (simple, propagates uncertainty via 'trend_samples')
set.seed(1)
fake_dat <- rnorm(length(yrs), 0, 1)
correlation <- trend_cor(r, fake_dat, trend_samples = 100)
hist(correlation)

# with process covariate when fitting:

# envdat <- readRDS("~/Downloads/TOB-for-arrowtooth-depths.rds")
# envdat <- filter(envdat, year %in% yrs)
# filter(envdat, agg_type == "mean") |> as.data.frame()

# https://cran.r-project.org/web/packages/bayesdfa/vignettes/a3_covariates.html

pro_covar <- data.frame(
  time = seq_along(yrs),
  trend = 1,
  covariate = 1,
  value = rnorm(length(yrs)), # make sure this is standardized!
  stringsAsFactors = FALSE
)

mcov <- fit_dfa(
  y = dw,
  iter = 1000,
  chains = 4,
  num_trends = 1,
  pro_covar = pro_covar,
  scale = "zscore",
  control = ctrl
)

names(mcov$samples_permuted)
r <- rotate_trends(mcov)

hist(mcov$samples_permuted$b_pro[,1,1])
