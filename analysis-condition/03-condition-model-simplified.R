# condition models
library(tidyverse)
library(sdmTMB)
library(ggsidekick)

theme_set(theme_sleek())

just_females <- TRUE
mat_class <- "mature"
mat_threshold <- 0.5
knot_distance <- 20

dens_model_name <- "-w-survey-factor"

species_list <- c("Petrale Sole")
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species_list)))


# load condition data and attach lagged density estimates

f <- paste0("data-generated/condition-data-w-lag-density/", spp, "-mat-", mat_threshold, "-condition-density.rds")
d2 <- readRDS(f)


# estimate condition model

d <- d2 %>% filter(group_name == "Mature females")
mesh <- make_mesh(d, c("X", "Y"), cutoff = knot_distance)

ggplot() +
  inlabru::gg(mesh$mesh) +
  coord_fixed() +
  geom_point(aes(X, Y, size = total_weight), data = d2) +
  geom_point(aes(X, Y, colour = group_catch_weight),data = d) +
  facet_wrap(~year) +
  scale_color_viridis_c(trans = "fourth_root_power")



# model_name <- "-mat-fem-dlag1"
model_name <- "-mat-fem-dlag1-1range"
# model_name <- "-mat-fem-dlag1-1range-no-weights"
mf <- paste0("data-generated/condition-models-mat-fem/", spp, "-c", model_name, "-", knot_distance, "-km.rds")

m <- sdmTMB(cond_fac ~
  1 +
  poly(log_density_lag1, 2) +
  poly(log_depth, 2),
weights = d$sample_multiplier,
mesh = mesh,
data = d,
spatial = "on",
spatiotemporal = "rw",
extra_time = c(2020),
# share_range = FALSE,
silent = FALSE,
time = "year",
family = lognormal(link = "log"),
priors = sdmTMBpriors(
  matern_s = pc_matern(range_gt = knot_distance * 1.5, sigma_lt = 2),
  matern_st = pc_matern(range_gt = knot_distance * 1.5, sigma_lt = 2)
)
)

sanity(m)
m

m$sd_report
tidy(m, "ran_pars", conf.int = TRUE)

dir.create(paste0("data-generated/condition-models-mat-fem/"), showWarnings = FALSE)
saveRDS(m, mf)

model_name2 <- "-mat-fem-dlag1-1range-no-extra-time"
# model_name2 <- "-mat-fem-dlag1-1range-no-extra-time-no-weights"

mf2 <- paste0("data-generated/condition-models-mat-fem/", spp, "-c", model_name2, "-", knot_distance, "-km.rds")

m2 <- sdmTMB(cond_fac ~
  1 +
  poly(log_density_lag1, 2) +
  poly(log_depth, 2),
weights = d$sample_multiplier,
mesh = mesh,
data = d,
spatial = "on",
spatiotemporal = "rw",
# extra_time = c(2020),
# share_range = FALSE,
silent = FALSE,
time = "year",
family = lognormal(link = "log"),
priors = sdmTMBpriors(
  matern_s = pc_matern(range_gt = knot_distance * 1.5, sigma_lt = 2),
  matern_st = pc_matern(range_gt = knot_distance * 1.5, sigma_lt = 2)
)
)

# m2 <- run_extra_optimization(m2)

sanity(m2)
m2
m2$sd_report
tidy(m2, "ran_pars", conf.int = TRUE)

saveRDS(m2, mf2)
# m2 <- readRDS(mf2)

# load density predictions for full survey grid
gridA <- readRDS(paste0("data-generated/density-predictions/", spp, "-p", dens_model_name, ".rds")) %>%
  select(year, X, Y, survey, depth, log_depth, density) %>%
  mutate(
    year_density = year,
    year = year + 1,
    density_lag1 = density,
    log_density_lag1 = log(density)
  ) %>%
  select(-density) %>%
  filter(year <= 2022 & year >= 2003)

# get current year density to scale condition index with
gridB <- readRDS(paste0("data-generated/density-predictions/", spp, "-p", dens_model_name, ".rds")) %>%
  select(year, X, Y, survey, depth, log_depth, density) %>%
  group_by(year) %>%
  mutate(sum_density = sum(density), prop_density = density / sum_density)

grid <- left_join(gridA, gridB)

sort(unique(m$data$year))
sort(unique(grid$year))



# results of model 1

pc <- predict(m, newdata = grid, return_tmb_object = TRUE)

p <- pc$data %>% mutate(cond = exp(est)) %>% filter(!(year == 2020))

# filter to plot only cells representing 99% of predicted biomass

proportion_of_biomass <- function(data, threshold = 0.01) {
  bio5perc <- sum(data$density, na.rm = TRUE) * threshold
  s <- sort(data$density)
  bio_sum <- cumsum(s)
  lower_density_threshold <- s[which(bio_sum >= bio5perc)[1]]
  data <- filter(data, density > lower_density_threshold)
  data
}

p <- proportion_of_biomass(p, 0.01)

ggplot(p, aes(X, Y, colour = log(cond), fill = log(cond))) +
  geom_tile(width = 2, height = 2, alpha = 1) +
  facet_wrap(~year) +
  scale_fill_gradient2() +
  scale_colour_gradient2()+
  ggtitle(paste(spp, model_name, knot_distance, "km"))

ggsave(paste0("figs/condition-map-", spp, model_name, "-", knot_distance, "-km.png"))
dir.create(paste0("data-generated/cond-predictions/"), showWarnings = FALSE)
saveRDS(p, paste0("data-generated/cond-predictions/", spp, "-pc", model_name, "-", knot_distance, "-km.rds"))

ind <- get_index(pc, area = grid$prop_density, bias_correct = FALSE)

ggplot(ind, aes(year, est)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  xlab("Year") +
  ylab("Predicted average condition factor") +
  ggtitle(paste("Mature female", species_list))

ggsave(paste0("figs/condition-index-", spp, model_name, "-", knot_distance, "-km.png"))
dir.create(paste0("data-generated/cond-index/"), showWarnings = FALSE)
saveRDS(ind, paste0("data-generated/cond-index/", spp, "-pc", model_name, "-", knot_distance, "-km.rds"))



# results of model 2

grid2 <- filter(grid, !(year == 2020))
pc2 <- predict(m2, newdata = grid2, return_tmb_object = TRUE)

p2 <- pc2$data %>% mutate(cond = exp(est))

p2 <- proportion_of_biomass(p2, 0.01)

ggplot(p2, aes(X, Y, colour = log(cond), fill = log(cond))) +
  geom_tile(width = 2, height = 2, alpha = 1) +
  facet_wrap(~year) +
  scale_fill_gradient2() +
  scale_colour_gradient2() +
  ggtitle(paste(spp, model_name2, knot_distance, "km"))
ggsave(paste0("figs/condition-map-", spp, model_name2, "-", knot_distance, "-km.png"))
saveRDS(p2, paste0("data-generated/cond-predictions/", spp, "-pc", model_name2, "-", knot_distance, "-km.rds"))

ind2 <- get_index(pc2, area = grid2$prop_density, bias_correct = FALSE)

ggplot(ind2, aes(year, est)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  xlab("Year") +
  ylab("Predicted average condition factor") +
  ggtitle(paste("Mature female", species_list))

ggsave(paste0("figs/condition-index-", spp, model_name2, "-", knot_distance, "-km.png"))
saveRDS(ind2, paste0("data-generated/cond-index/", spp, "-pc", model_name2, "-", knot_distance, "-km.rds"))
