# investigate maturity data for yellowtail because mating and gestiation time and depth not in literature
library(reldist)
library(tidyverse)
library(patchwork)
options(scipen=999)
year_cutoff <- 1995 # greater than
species <- "yellowtail"

readRDS("data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 1) # ROCKFISH
readRDS("data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 11) # PORTSAMPLES
readRDS("data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 13) # AMR


# get all catch for overall depth profiles by month

d_all <- readRDS("data/yellowtail-get-catch.rds") %>%
  filter(year > year_cutoff) %>%
  mutate(month = lubridate::month(best_date),
         total_catch = landed_kg + discarded_kg)

# get minor area coordinates
get_minor_coords <- function(){
  # library(PBSmapping)
  data("minor", package = "PBSdata", envir = environment())
  labels <- attributes(minor)$PolyData
  class(labels) <- "data.frame" # this seems to prevent needing to library(PBSmapping)
  labels
}

minor_areas <- get_minor_coords() %>%
  select(minor_stat_area_code = minor, longitude2 = X, latitude2 = Y) %>%
  mutate(minor_stat_area_code = as.character(minor_stat_area_code))


d_all <- left_join(d_all, minor_areas) %>% mutate(longitude = ifelse(is.na(lon), longitude2, lon),
                                              latitude = ifelse(is.na(lat), latitude2, lat))


# d <- readRDS("../../gfsynopsis/report/data-cache/yellowtail-rockfish.rds")
# dat <- d$commercial_samples
# dat1 <- gfdata::get_commercial_samples("yellowtail rockfish", unsorted_only = FALSE)
# saveRDS(dat1, "data/yellowtail-comm-samples.rds")
dat1 <- readRDS("data/yellowtail-comm-samples.rds") # cached data is missing lat, lon, and depth
dat2 <- readRDS("../../gfsynopsis/report/data-cache/yellowtail-rockfish.rds")

# dat2$survey_samples %>% View()
# range(dat2$survey_sets$catch_weight)
# range(dat$catch_weight, na.rm = T)
# dat2$survey_samples$fishing_event_id
# # glimpse(dat)
# # glimpse(dat2$survey_samples)
# # unique(dat2$survey_samples$maturity_code)
# unique(dat2$survey_samples$maturity_name)
# unique(dat2$survey_samples$catch_weight)
# dat2$survey_samples %>% filter(maturity_code %in% c(9, 37)) %>% View()
# dat2$survey_samples %>% filter(month %in% c(7)) %>% View()

.d <- dat2$survey_sets %>%
  select(fishing_event_id, latitude, longitude, catch_weight, catch_count, depth_m) %>%
  rename(best_depth = depth_m)

plot(catch_weight~catch_count, data = filter(.d, catch_count > 0, catch_weight >0))

# first remove all false 0s in dataframe
.d$catch_count <- ifelse(.d$catch_weight > 0 & .d$catch_count == 0, NA, .d$catch_count)
.d$catch_weight <- ifelse(.d$catch_count > 0 & .d$catch_weight == 0, NA, .d$catch_weight)

# generate simple linear model formula
f <- lm(catch_weight~catch_count, data = .d)

# fill in missing biomass densities
.d$catch_weight <- ifelse(is.na(.d$catch_weight),
                            .d$catch_count*f$coefficients[2] + f$coefficients[1],
                          .d$catch_weight)


dat3 <- left_join(dat2$survey_samples, .d, multiple = "all")

dat <- bind_rows(dat1, dat3)

dat <- left_join(dat, minor_areas) %>%
  mutate(longitude = ifelse(is.na(longitude), longitude2, longitude),
         latitude = ifelse(is.na(latitude), latitude2, latitude))


# prep maturity data into better format for plotting that contains "Maturity" and an associated "prop_catch_weight"

d <- dat %>%
  filter(year > year_cutoff) %>%
  mutate(Maturity = case_when(maturity_code %in% c(NA, 0, 9, 37) ~ NA,
                              maturity_code %in% c(1, 2) ~ "1/2 = immature",
                              # maturity_code == 2 ~ "2 = maturing",
                              maturity_code == 3 ~ "3/4M = mature",
                              maturity_code == 4 & sex == 1 ~ "3/4M = mature",
                              maturity_code %in% c(4, 5) & sex == 2 ~ "4F/5F = fertilized/embryos",
                              maturity_code == 6 ~ "6 = spent",
                              maturity_code == 7 ~ "7 = resting",
  )) %>%
  filter(catch_weight > 0) %>%
  filter(best_depth > 0)

# dat %>% filter(!is.na(maturity_code)) %>%
#   group_by(year, maturity_code) %>% summarise(n = n()) %>% View()
# d %>% filter(!is.na(Maturity)) %>% group_by(year, maturity_code) %>%
#   summarise(n = n()) %>% View()

dp <- d %>%
  filter(!is.na(Maturity)) %>%
  filter(sex %in% c(1,2)) %>% group_by(fishing_event_id) %>%
  mutate(N = n()) %>%
  group_by(sex, Maturity, fishing_event_id, N) %>%
  summarise(n = n(), prop = n/N) %>% distinct()

d <- left_join(d, dp) %>% mutate(prop_catch_weight = prop*catch_weight/N)

range(dp$prop)
range(d$n, na.rm = TRUE)
range(d$prop_catch_weight, na.rm = TRUE)



# months and depths by maturity ----
d %>%
  filter(!is.na(Maturity) & prop_catch_weight > 0) %>%
  group_by(sex, Maturity) %>%
  summarise(n = n(),
            mean_depth = weighted.mean(best_depth, prop_catch_weight, na.rm = TRUE),
            depth_2.5 = wtd.quantile(best_depth, 0.025, na.rm = TRUE, weight = prop_catch_weight),
            depth_97.5 = wtd.quantile(best_depth, 0.975, na.rm = TRUE, weight = prop_catch_weight),
            mean_month = weighted.mean(month, prop_catch_weight, na.rm = TRUE),
            month_25 = wtd.quantile(month, 0.25, na.rm = TRUE, weight = prop_catch_weight),
            month_75 = wtd.quantile(month, 0.75, na.rm = TRUE, weight = prop_catch_weight)
            )

d %>%
  filter(sex %in% c(1)) %>% group_by(fishing_event_id) %>%
  mutate(N = n()) %>% ungroup() %>%
  group_by(sex, fishing_event_id) %>%
  mutate(n = n(), prop = n/N, prop_catch_weight = prop*catch_weight/N) %>% ungroup() %>%
  group_by(sex, month) %>%
  summarise(n = n(),
            mean_depth = weighted.mean(best_depth, prop_catch_weight, na.rm = TRUE),
            depth_2.5 = wtd.quantile(best_depth, 0.025, na.rm = TRUE, weight = prop_catch_weight),
            depth_97.5 = wtd.quantile(best_depth, 0.975, na.rm = TRUE, weight = prop_catch_weight)
  )

d %>%
  filter(sex %in% c(2)) %>% group_by(fishing_event_id) %>%
  mutate(N = n()) %>% ungroup() %>%
  group_by(sex, fishing_event_id) %>%
  mutate(n = n(), prop = n/N, prop_catch_weight = prop*catch_weight/n) %>% ungroup() %>%
  group_by(sex, month) %>%
  summarise(n = n(),
            mean_depth = weighted.mean(best_depth, prop_catch_weight, na.rm = TRUE),
            depth_2.5 = wtd.quantile(best_depth, 0.025, na.rm = TRUE, weight = prop_catch_weight),
            depth_97.5 = wtd.quantile(best_depth, 0.975, na.rm = TRUE, weight = prop_catch_weight)
            )


# plotting function ----
plot_maturity_by_month <- function(
  mat_data,
  all_catch,
  which_sex = c("Male", "Female"),
  months = c(1:12),
  maturity_codes = c(1, 2, 3, 4, 5, 6,7),
  bin_width = 20,
  scale_weights = 100,
  ylimits = NULL,
  text_placement_x = 60
  ){

  d1 <- mat_data %>%
    filter(month %in% months) %>%
    filter(sex %in% c(1,2))

  d1$sex <- factor(d1$sex, labels = c("Male", "Female"))

  m <- d1 %>% filter(sex %in% which_sex) %>%
    filter(catch_weight > 0) %>% # not clear why this is needed
    filter(month %in% months) %>% group_by(fishing_event_id) %>%
    mutate(N = n()) %>% ungroup() %>%
    group_by(sex, fishing_event_id) %>%
    mutate(n = n(), prop = n/N, prop_catch_weight = prop*catch_weight/N) %>% ungroup() %>%
    group_by(sex, month) %>%
    summarise(n = n(),
              mean_depth = weighted.mean(best_depth, prop_catch_weight, na.rm = TRUE),
              depth_2.5 = wtd.quantile(best_depth, 0.025, na.rm = TRUE, weight = prop_catch_weight),
              depth_97.5 = wtd.quantile(best_depth, 0.975, na.rm = TRUE, weight = prop_catch_weight)
    )

  m2 <- d1 %>% filter(sex %in% which_sex) %>%
    filter(maturity_code %in% maturity_codes) %>%
    filter(month %in% months) %>%
    group_by(sex, month) %>%
    summarise(n = n())

  d1 %>% filter(maturity_code %in% maturity_codes) %>%
    filter(sex %in% which_sex) %>%
    ggplot() +
    geom_histogram(data = d1 %>% filter(sex %in% which_sex),
                   aes(best_depth,
                       weight = catch_weight/N/scale_weights
                   ), binwidth = bin_width,
                   alpha = 0.25
    ) +
    geom_histogram(aes(best_depth,
                       weight = prop_catch_weight/scale_weights,
                       fill = Maturity),
                   binwidth = bin_width,
                   alpha = 0.9
    ) +
    geom_vline(data = m, aes(xintercept = mean_depth), colour = "grey", linetype = "solid") +
    geom_vline(data = m, aes(xintercept = depth_2.5), colour = "grey", linetype = "dotted") +
    geom_vline(data = m, aes(xintercept = depth_97.5), colour = "grey", linetype = "dotted") +
    geom_text(data = m, aes(x = text_placement_x, y = Inf,
                            label = paste0("N = ", n)), vjust   = 3.75, colour = "grey") +
    geom_text(data = m2, aes(x = text_placement_x, y = Inf,
                             label = paste0("n = ", n)
                             ), vjust   = 1.75, colour = "grey30") +
    geom_density(
      data = d_all %>% filter(month %in% months),
      aes(best_depth, after_stat(count), weight = total_catch)
    ) +
    facet_grid(month~sex, switch = "y") +
    coord_cartesian(xlim = c(0, quantile(mat_data$best_depth, 0.995)),
                    ylim = ylimits,
                    expand = FALSE) +
    scale_fill_viridis_d() +
    labs(y = "Weighted commercial catch (solid line) and samples weighted by catch (grey bars = all sexed samples)",
         x = "Depth (m)") +
    gfplot::theme_pbs() + theme(axis.ticks.y = element_blank(),
                                axis.text.y = element_blank())
}


# default test
plot_maturity_by_month(d, d_all)
ggsave(paste0("figs/", species, "-maturity-by-month-depth-long.png"), width = 8, height = 14)

# # split by month sequence but feels a bit akward
# p1 <- plot_maturity_by_month(d, d_all, months = c(1:6))
# p2 <- plot_maturity_by_month(d, d_all, months = c(7:12),
#                              ylimits = layer_scales(p1)$y$range$range) +
#   theme(axis.title.y = element_blank())
#
# p1 + p2 + plot_layout(guides = "collect")
#
# ggsave(paste0("figs/", species, "-maturity-by-month-depth.png"), width = 8, height = 14)


# split by sex first
p1 <- plot_maturity_by_month(d, d_all, which_sex = c("Male")) + theme(legend.position = "none") +
  facet_wrap(~month, ncol = 2, strip.position = "left",dir="v") + ggtitle("A. Male")
p2 <- plot_maturity_by_month(d, d_all, which_sex = c("Female"), ylimits = layer_scales(p1)$y$range$range) +
  facet_wrap(~month, ncol = 2, strip.position = "left",dir="v")  + ggtitle("B. Female")+
  theme(axis.title.y = element_blank())

p1 + p2 + plot_layout()

ggsave(paste0("figs/", species, "-maturity-by-month-depth.png"), width = 12, height = 10)


# see how these samples are distributed spatially ----
d2 <- d %>%
  filter(!is.na(Maturity)) %>%
  filter(sex %in% c(1,2)) %>% group_by(latitude, longitude, month, catch_weight) %>%
  mutate(N = n()) %>%
  group_by(sex, Maturity, latitude, longitude, month, catch_weight) %>%
  summarise(n = n(), prop = n/N) %>% distinct()

# d2 %>% View()
d2$sex <- factor(d2$sex, labels = c("Male", "Female"))

plot_maturity_map <- function(mat_data, d_all, months = c(1:12)){
  mat_data %>%
    filter(month %in% months) %>%
  ggplot() +
    geom_jitter(data = d_all %>%
                  filter(month %in% months), aes(longitude, latitude), alpha = 0.1, size = 0.1) +
    geom_jitter(aes(longitude, latitude, colour = Maturity, size = prop*catch_weight),
                height = 0.2, width = 0.2, alpha = 0.8) +
    scale_colour_viridis_d() +
    scale_size_continuous(name = "Sampled catches", limits = c(0, max(d2$catch_weight))) +
    facet_grid(month~sex) +
    coord_cartesian(xlim = c(min(d2$longitude, na.rm = TRUE), max(d2$longitude, na.rm = TRUE)),
                    ylim = c(min(d2$latitude, na.rm = TRUE), max(d2$latitude, na.rm = TRUE))) +
    labs(x = "Longitude", y = "Latitude") +
    gfplot::theme_pbs()
}


p1 <- plot_maturity_map(d2, d_all, months = c(1:6)) +
  theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
p2 <- plot_maturity_map(d2, d_all, months = c(7:12))

p2 + p1 + plot_layout(guides = "collect")

ggsave(paste0("figs/", species, "-maturity-mapped.png"), width = 10, height = 8)

