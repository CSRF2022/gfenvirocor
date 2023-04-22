# investigate maturity data for yellowtail because mating and gestiation time and depth not in literature
library(reldist)
library(tidyverse)
library(patchwork)
options(scipen=999)
year_cutoff <- 1995 # greater than
# species <- "yellowtail"
# species <- "silvergray"
# species <- "pcod"
# species <- "yellowmouth"
# species <- "redstripe"
# species <- "rougheye"
# species <- "bocaccio"
# species <- "widow"

species <- "canary"

# get all catch for overall depth profiles by month

d_all <- readRDS(paste0("sample-data/", species, "-get-catch.rds")) %>%
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


d_all <- left_join(d_all, minor_areas) %>% mutate(
  longitude = ifelse(is.na(lon), longitude2, lon),
  latitude = ifelse(is.na(lat), latitude2, lat)
  )


# d <- readRDS("../../gfsynopsis/report/data-cache/yellowtail-rockfish.rds")
# dat <- d$commercial_samples
# dat1 <- gfdata::get_commercial_samples("yellowtail rockfish", unsorted_only = FALSE)
# saveRDS(dat1, "data/yellowtail-comm-samples.rds")
dat1 <- readRDS(paste0("sample-data/", species, "-comm-samples.rds")) # cached data is missing lat, lon, and depth

dat1 %>% group_by(maturity_convention_code, maturity_convention_desc) %>% summarize(n = n()) %>% distinct()

if (species == "pcod") {
  dat2 <- readRDS("../../gfsynopsis/report/data-cache/pacific-cod.rds")

  readRDS("sample-data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 6) # PACIFIC COD (1973-75)
  readRDS("sample-data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 7) # PCOD
  readRDS("sample-data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 11) # PORTSAMPLES
  readRDS("sample-data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 22) # old
} else{

  readRDS("sample-data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 1) # ROCKFISH
  readRDS("sample-data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 11) # PORTSAMPLES
  readRDS("sample-data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 13) # AMR

  if (species == "rougheye") {
    dat2 <- readRDS(paste0("../../gfsynopsis/report/data-cache/rougheye-blackspotted-rockfish-complex.rds"))
  } else{
    if (species == "bocaccio") {
      dat2 <- readRDS(paste0("../../gfsynopsis/report/data-cache/bocaccio.rds"))
    } else{
      dat2 <- readRDS(paste0("../../gfsynopsis/report/data-cache/", species, "-rockfish.rds"))
  }
}
}





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

d0 <- dat %>%
  filter(year > year_cutoff) %>%
  filter(catch_weight > 0) %>%
  filter(best_depth > 0) %>%
  distinct() # needed because about 300 records are duplicated

if(species == "pcod"){
d1 <- d0 %>%
  mutate(Maturity = case_when(
     maturity_code %in% c(NA, 0, 9, 37) ~ NA,
    maturity_convention_code != 6 & maturity_code %in% c(1, 2) ~ "1/2 = immature",
    maturity_convention_code != 6 & maturity_code == 3 ~ "3/4 = mature",
    maturity_convention_code != 6 & maturity_code == 4 ~ "3/4 = mature",
    maturity_convention_code != 6 & maturity_code %in% c(5) ~ "5 = ripe",
    maturity_convention_code != 6 & maturity_code == 6 ~ "6 = spent",
    maturity_convention_code != 6 & maturity_code == 7 ~ "7 = resting",
    maturity_convention_code == 6 & maturity_code %in% c(1) ~ "1/2 = immature",
    maturity_convention_code == 6 & maturity_code == 2 ~ "3/4 = mature",
    maturity_convention_code == 6 & maturity_code %in% c(3) ~ "5 = ripe",
    maturity_convention_code == 6 & maturity_code == 4 ~ "7 = resting",
    maturity_convention_code == 22 & maturity_code %in% c(1) ~ "1/2 = immature",
    maturity_convention_code == 22 & maturity_code == 2 ~ "3/4 = mature",
    maturity_convention_code == 22 & maturity_code %in% c(3) ~ "7 = resting",
  ))

} else {
  # for rockfish
  d1 <- d0 %>%
    mutate(Maturity = case_when(maturity_code %in% c(NA, 0, 9, 37) ~ NA,
                                maturity_code %in% c(1, 2) ~ "1/2 = immature",
                                # maturity_code == 2 ~ "2 = maturing",
                                maturity_code == 3 ~ "3/4M = mature",
                                maturity_code == 4 & sex == 1 ~ "3/4M = mature",
                                maturity_code %in% c(4, 5) & sex == 2 ~ "4F/5F = fertilized/embryos",
                                maturity_code == 6 ~ "6 = spent",
                                maturity_code == 7 ~ "7 = resting",
    ))
}


# dat %>% filter(!is.na(maturity_code)) %>%
#   group_by(year, maturity_code) %>% summarise(n = n()) %>% View()
# d %>% filter(!is.na(Maturity)) %>% group_by(year, maturity_code) %>%
#   summarise(n = n()) %>% View()

# half of samples are missing weights, so need a way to scale from counts to weight
# .d$catch_count*f$coefficients[2] + f$coefficients[1]

# generate simple linear model formula

dp <- d1 %>%
  filter(!is.na(Maturity)) %>%
  filter(sex %in% c(1,2)) %>%
  group_by(sex, Maturity) %>%
  mutate(weight_est = mean(weight, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(fishing_event_id)%>%
  mutate(N = n(),
         fe_sampled_weight_true = sum(weight/1000),
         fe_sampled_weight = ifelse(is.na(fe_sampled_weight_true), sum(weight_est/1000), fe_sampled_weight_true)
  ) %>%
  ungroup() %>%
  group_by(fishing_event_id, sex, Maturity) %>%
  mutate(n = n(),
         sm_sampled_weight_true = sum(weight/1000),
         sm_sampled_weight = ifelse(is.na(sm_sampled_weight_true), n*weight_est/1000, sm_sampled_weight_true),
         prop_sampled_weight = ifelse(sm_sampled_weight>0, sm_sampled_weight/fe_sampled_weight, NA),
         prop_catch_weight = ifelse(n>0 & !is.na(prop_sampled_weight), prop_sampled_weight*catch_weight/n, NA)
         ) %>%
  ungroup()

d_fe <- dp %>% select(fishing_event_id, N, fe_sampled_weight_true, fe_sampled_weight) %>%
  distinct()

dp <- dp %>% select(fishing_event_id, sex, Maturity, n,
                    sm_sampled_weight_true, sm_sampled_weight,
                    prop_sampled_weight, prop_catch_weight) %>%
  distinct()

d <- left_join(d1, d_fe) %>% left_join(., dp)

# range(dp$prop)
# range(d$n, na.rm = TRUE)
range(d$prop_catch_weight, na.rm = TRUE)



# months and depths by maturity ----
d %>%
  filter(!is.na(Maturity) & prop_catch_weight > 0) %>%
  group_by(sex, Maturity) %>%
  summarise(n = n(),
            mean_depth = weighted.mean(best_depth, prop_catch_weight, na.rm = TRUE),
            depth_2.5 = wtd.quantile(best_depth, 0.025, na.rm = TRUE, weight = prop_catch_weight),
            depth_97.5 = wtd.quantile(best_depth, 0.975, na.rm = TRUE, weight = prop_catch_weight),
            depth_25 = wtd.quantile(best_depth, 0.25, na.rm = TRUE, weight = prop_catch_weight),
            depth_75 = wtd.quantile(best_depth, 0.75, na.rm = TRUE, weight = prop_catch_weight),
            mean_month = weighted.mean(month, prop_catch_weight, na.rm = TRUE),
            month_25 = wtd.quantile(month, 0.25, na.rm = TRUE, weight = prop_catch_weight),
            month_75 = wtd.quantile(month, 0.75, na.rm = TRUE, weight = prop_catch_weight)
            )



# plotting function ----
plot_maturity_by_month <- function(
  mat_data,
  all_catch,
  which_sex = c("Male", "Female"),
  months = c(1:12),
  maturity_codes = c(1, 2, 3, 4, 5, 6,7),
  bin_width = 20,
  scale_weights = 1000,
  ylimits = NULL,
  text_placement_x = 60
  ){

  d1 <- mat_data %>%
    filter(month %in% months) %>%
    filter(sex %in% c(1,2))  %>%
    # droplevels() %>%
    group_by(sex, month) %>%
    mutate(N = n())

  d1$sex <- factor(d1$sex, labels = c("Male", "Female"))

  m <- d1 %>% filter(sex %in% which_sex) %>%
    filter(prop_catch_weight > 0) %>% # not clear why this is needed
    filter(month %in% months) %>%
    filter(maturity_code %in% maturity_codes) %>%
    droplevels() %>%
    # group_by(fishing_event_id) %>% mutate(N = n()) %>% ungroup() %>%
    # group_by(sex, fishing_event_id) %>%
    # mutate(n = n(), prop = n/N, prop_catch_weight = prop*catch_weight/N) %>% ungroup() %>%
    group_by(sex, month) %>%
    summarise(N = mean(N),
              mean_depth = weighted.mean(best_depth, prop_catch_weight, na.rm = TRUE),
              depth_2.5 = wtd.quantile(best_depth, 0.025, na.rm = TRUE, weight = prop_catch_weight),
              depth_97.5 = wtd.quantile(best_depth, 0.975, na.rm = TRUE, weight = prop_catch_weight)
    ) %>% distinct()

  m2 <- d1 %>% filter(sex %in% which_sex) %>%
    filter(maturity_code %in% maturity_codes) %>%
    filter(month %in% months) %>%
    droplevels() %>%
    group_by(sex, month) %>%
    summarise(n = n()) %>% distinct()

  # all2 <- all_catch %>% filter(month %in% months)

# browser()
  d1 %>% filter(maturity_code %in% maturity_codes) %>%
    filter(!is.na(Maturity)) %>%
    filter(sex %in% which_sex) %>%
    droplevels() %>%
    ggplot() +
    # # geom_histogram(data = d1 %>% filter(sex %in% which_sex),
    # #                aes(best_depth,
    # #                    weight = catch_weight/N/scale_weights
    # #                ), binwidth = bin_width,
    # #                alpha = 0.25
    # # ) +
    # geom_histogram(
    #   data = all_catch %>% filter(month %in% months),
    #   aes(best_depth, after_stat(scaled), weight = total_catch)
    # ) +

    geom_histogram(aes(best_depth,
                       # after_stat(count),
                       weight = prop_catch_weight/scale_weights,
                       fill = Maturity),
                   binwidth = bin_width,
                   alpha = 0.9
    ) +

    # geom_density(aes(best_depth, after_stat(count),
    #                  weight = prop_catch_weight/scale_weights,
    #                  fill = Maturity), position = "stack") +

    geom_vline(data = m, aes(xintercept = mean_depth), colour = "grey", linetype = "solid") +
    geom_vline(data = m, aes(xintercept = depth_2.5), colour = "grey", linetype = "dotted") +
    geom_vline(data = m, aes(xintercept = depth_97.5), colour = "grey", linetype = "dotted") +
    geom_text(data = m, aes(x = text_placement_x, y = Inf,
                            label = paste0("N = ", N)), vjust   = 3.75, colour = "grey") +
    geom_text(data = m2, aes(x = text_placement_x, y = Inf,
                             label = paste0("n = ", n)
                             ), vjust   = 1.75, colour = "grey30") +
    geom_density(
      data = all_catch %>% filter(month %in% months),
      aes(best_depth,
          after_stat(count),
          # after_stat(scaled),
          weight = total_catch)
    ) +
    facet_grid(month~sex, switch = "y", scales = "free_y"
               ) +
    coord_cartesian(
      # expand = FALSE,
      xlim = c(0, quantile(mat_data$best_depth, 0.998)),
                    ylim = ylimits) +
    scale_fill_viridis_d() +
    labs(y = "Weighted commercial catch (solid line) and samples weighted by catch
         (grey text and lines = all sexed samples)",
         x = "Depth (m)") +
    gfplot::theme_pbs() #+ theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
}


# default test
plot_maturity_by_month(d, d_all, scale_weights = 100)
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
p1 <- plot_maturity_by_month(d, d_all, scale_weights = 1, which_sex = c("Male")) + theme(legend.position = "none") +
  facet_wrap(~month, ncol = 2, strip.position = "left",
             scales = "free_y",
             dir="v") + ggtitle("A. Male")
# p2 <- plot_maturity_by_month(d, d_all, which_sex = c("Female"), ylimits = layer_scales(p1)$y$range$range) +

p2 <- plot_maturity_by_month(d, d_all, scale_weights = 1, which_sex = c("Female")) +
  facet_wrap(~month, ncol = 2,
             scales = "free_y",
             strip.position = "left", dir="v")  + ggtitle("B. Female")+
  theme(axis.title.y = element_blank())

p1 + p2 + plot_layout()

ggsave(paste0("figs/", species, "-maturity-by-month-depth.png"), width = 12, height = 10)


# see how these samples are distributed spatially ----
d2 <- d %>%
  filter(!is.na(Maturity)) %>%
  filter(sex %in% c(1,2)) %>%
  select(fishing_event_id, sex, Maturity, latitude, longitude, month,
         catch_weight, n, N, prop_sampled_weight) %>%
  distinct() %>%
  mutate(catch_by_mat = prop_sampled_weight*catch_weight/1000)


# d2 %>% View()
d2$sex <- factor(d2$sex, labels = c("Male", "Female"))

plot_maturity_map <- function(mat_data, d_all, months = c(1:12)){
  mat_data %>% filter(month %in% months) %>%
  ggplot() +
    geom_point(data = d_all %>% filter(month %in% months),
                aes(longitude, latitude), alpha = 0.1, size = 0.1) +
    geom_jitter(aes(longitude, latitude, colour = Maturity, size = catch_by_mat),
                height = 0.3, width = 0.3, alpha = 0.8) +
    scale_colour_viridis_d() +
    scale_size_continuous(name = "Sampled catches",
                          trans = "sqrt",
                          breaks = c(0,5,10,20,40),
                          range = c(0.75,4),
                          limits = c(0, max(d2$catch_by_mat))) +
    facet_grid(month~sex) +
    coord_cartesian(xlim = c(min(d2$longitude, na.rm = TRUE),
                             max(d2$longitude, na.rm = TRUE)),
                    ylim = c(min(d2$latitude, na.rm = TRUE),
                             max(d2$latitude, na.rm = TRUE))) +
    labs(x = "Longitude", y = "Latitude") +
    gfplot::theme_pbs()
}


p1 <- plot_maturity_map(d2, d_all, months = c(1:6)) +
  theme(axis.title.y = element_blank(),axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
p2 <- plot_maturity_map(d2, d_all, months = c(7:12))

p2 + p1 + plot_layout(guides = "collect")

ggsave(paste0("figs/", species, "-maturity-mapped.png"), width = 11, height = 9)


d %>%
  filter(sex %in% c(1)) %>%
  filter(prop_catch_weight > 0) %>%
  group_by(sex, month) %>%
  summarise(n = n(),
            mean_depth = weighted.mean(best_depth, prop_catch_weight, na.rm = TRUE),
            depth_2.5 = wtd.quantile(best_depth, 0.025, na.rm = TRUE, weight = prop_catch_weight),
            depth_97.5 = wtd.quantile(best_depth, 0.975, na.rm = TRUE, weight = prop_catch_weight)
  )

d %>%
  filter(sex %in% c(2)) %>%
  filter(prop_catch_weight > 0) %>%
  group_by(sex, month) %>%
  summarise(n = n(),
            mean_depth = weighted.mean(best_depth, prop_catch_weight, na.rm = TRUE),
            depth_2.5 = wtd.quantile(best_depth, 0.025, na.rm = TRUE, weight = prop_catch_weight),
            depth_97.5 = wtd.quantile(best_depth, 0.975, na.rm = TRUE, weight = prop_catch_weight)
  )
