# explore seasonality in commercial catch of rockfish

library(tidyverse)
# library(lubridate)
# library(gfdata)


species <- "yellowtail"
# d <- get_catch("yellowtail rockfish") # run march 6th 2023
# saveRDS(d, "data/yellowtail-get-catch.rds")
d <- readRDS("data-raw/yellowtail-get-catch.rds")

# species <- "silvergray"
# # d <- get_catch("silvergray rockfish") # run march 6th 2023
# # saveRDS(d, "data/silvergray-get-catch.rds")
# d <- readRDS("data/silvergray-get-catch.rds")


# species <- "pop"
# # d <- get_catch("pacific ocean perch") # run march 6th 2023
# # saveRDS(d, "data/pop-get-catch.rds")
# d <- readRDS("data/pop-get-catch.rds")

# if using get_catch
d <- d %>% mutate(month = lubridate::month(best_date))

glimpse(d)

# ggplot(d) + geom_histogram(aes(month))

unique(d$fishery_sector)
# d %>% group_by(year, fishery_sector) %>%
#   summarise(total_catch = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE),
#             n = n()
#   ) %>% View()


d2 <- d %>% group_by(year, month) %>%
  summarise(total_catch = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE),
            n = n()
            )


rainbow1 <- rainbow(14)
rainbow2 <- c(rainbow1[10:14],rainbow1[1:4],rainbow1[7:9])

d2 %>% filter(year > 1986 & year < 2023) %>%
ggplot() + geom_col(aes(as.factor(month), total_catch, fill = as.factor(month))) +
  scale_fill_manual(values = rainbow2)  +
  facet_wrap(~year) + gfplot::theme_pbs()

ggsave(paste0("figs/", species,"-catch-seasonality.png"))


d2 %>% filter(year > 1986 & year < 2023) %>%
  ggplot() + geom_col(aes(as.factor(month), total_catch, fill = as.factor(month)), alpha = 0.8) +
  geom_col(aes(as.factor(month), n*200), alpha = 0.3) +
  scale_fill_manual(values = rainbow2)  +
  facet_wrap(~year) + gfplot::theme_pbs()

ggsave(paste0("figs/", species,"-catch-w-trips-seasonality.png"))


unique(d$major_stat_area_name)

d_catch <- d %>%
  filter(fishery_sector == "GROUNDFISH TRAWL") %>%
  filter(year == "2021") %>%
  filter(
    !(major_stat_area_name %in% c(
      "4B: STRAIT OF GEORGIA",
      "UNKNOWN: NO POSITION INFORMATION",
      "ALASKA",  "3A:CAPE FALCON TO CAPE ELIZABETH (45 46' TO 47 20')",
      "4A:PUGET SOUND", "3B: CAPE FLATTERY (47 20' to 220 T)"
    ))
  ) %>%
  select(fishing_event_id, year, month, fishery_sector, landed_kg, discarded_kg)



d3 <- d %>%
  filter(
    !(major_stat_area_name %in% c(
      # "4B: STRAIT OF GEORGIA",
      # "UNKNOWN: NO POSITION INFORMATION",
      "ALASKA",  "3A:CAPE FALCON TO CAPE ELIZABETH (45 46' TO 47 20')",
      "4A:PUGET SOUND", "3B: CAPE FLATTERY (47 20' to 220 T)"
      ))
    ) %>%
  select(trip_id, fishing_event_id, year, month, fishery_sector, landed_kg, discarded_kg) %>%
  distinct() %>% group_by(year, month, fishery_sector) %>%
  summarise(
    landed_catch = sum(landed_kg, na.rm = TRUE),
    total_catch = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE))

d3 %>% filter(year > 1986 & year < 2023) %>%
  filter(fishery_sector == "GROUNDFISH TRAWL") %>%
  ggplot() + geom_col(aes(as.factor(month), landed_catch, fill = as.factor(month))) +
  scale_fill_manual(values = rainbow2)  +
  facet_wrap(~year) + gfplot::theme_pbs()



d3 %>% filter(year > 1986 & year < 2023) %>%
  filter(fishery_sector == "GROUNDFISH TRAWL") %>%
  ggplot() + geom_col(aes(as.factor(month), total_catch, fill = as.factor(month))) +
  scale_fill_manual(values = rainbow2)  +
  facet_wrap(~year) + gfplot::theme_pbs()

ggsave(paste0("figs/", species,"-catch-seasonality-just-trawl.png"))


# check if the output from cpue historical (d0) is similar?
# first check particular trip with problems
d %>% filter(trip_id == 82252) %>% View() # 3 good events

d0 <- readRDS("data-raw/yellowtail-cpue-historical.rds")
d0 %>% filter(trip_id == 82252) %>% View() # duplications

dx <- readRDS("data-raw/yellowtail-cpue-spatial.rds")
dx %>% filter(trip_id == 82252) %>% View() # 3 good events



d0 <- readRDS("data-raw/yellowtail-cpue-historical.rds") %>%
  select(-target_species) %>% distinct()


d_cpue <- d0 %>%
  filter(year == "2021") %>%
  select(fishing_event_id, year, month, landed_kg, discarded_kg)

d_test <- anti_join(d_catch, d_cpue)


d4 <- d0 %>% group_by(year, month) %>%
  summarise(
    landed_catch = sum(landed_kg, na.rm = TRUE),
    total_catch = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE),
            n = n()
  )

d4 %>% filter(year > 1986 & year < 2023) %>% ggplot() +
  geom_col(aes(as.factor(month), landed_catch, fill = as.factor(month))) +
  scale_fill_manual(values = rainbow2)  +
  facet_wrap(~year) + gfplot::theme_pbs()

d4 %>% filter(year > 1986 & year < 2023) %>% ggplot() +
  geom_col(aes(as.factor(month), total_catch, fill = as.factor(month))) +
  scale_fill_manual(values = rainbow2)  +
  facet_wrap(~year) + gfplot::theme_pbs()

d5 <- d0 %>% select(trip_id, fishing_event_id, latitude, longitude, best_date, best_depth_m, landed_kg, discarded_kg, major_stat_area_code, minor_stat_area_code, database_name2 = database_name, vessel_registration_number = vessel)

# try binding
d6 <- left_join(d, d5) # no warning, so 1:1, and d appears complete, only d5 is missing some

# get annual totals from cpue historical
d9 <- d0 %>% filter(year > 1980) %>% group_by(year) %>% summarise(total_catch = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE))

# get annual totals for excluded events
d7 <- anti_join(d, d5)
d8 <- d7 %>% filter(year > 1980) %>% group_by(year) %>% summarise(total_catch = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE))

# by area
d8x <- d7 %>% filter(year > 2000) %>% group_by(year, major_stat_area_name) %>% summarise(total_catch = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE))

ggplot(d8x) + geom_path(aes(year, total_catch, colour = major_stat_area_name)) +
  # geom_path(data = d8, aes(year, total_catch), colour = "red") +
  gfplot::theme_pbs()

# by sector, mostly groundfish trawl (largely midwater, but some of bottom too)
d8xx <- d7 %>% filter(year > 2000) %>% group_by(year, gear) %>% summarise(total_catch = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE))

ggplot(d8xx) + geom_path(aes(year, total_catch, colour = gear)) +
  # geom_path(data = d8, aes(year, total_catch), colour = "red") +
  # scale_y_sqrt() +
  gfplot::theme_pbs()

# by database
d8xx <- d7 %>% filter(year > 2000) %>% group_by(year, database_name) %>% summarise(total_catch = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE))

ggplot(d8xx) + geom_path(aes(year, total_catch, colour = database_name)) +
  # geom_path(data = d8, aes(year, total_catch), colour = "red") +
  # scale_y_sqrt() +
  gfplot::theme_pbs()

plot(d8$total_catch~d8$year)
plot(d9$total_catch~d9$year)

ggplot(d9) + geom_path(aes(year, total_catch)) +
  geom_path(data = d8, aes(year, total_catch), colour = "red") +
  gfplot::theme_pbs()

d6 %>% filter(!is.na(database_name2) & year > 1980) %>% View()

d0 %>% filter(trip_id == 82252) %>% View()






(d6$fishing_event_id == d6$fishing_event_id2)

# plot(d6$fishing_event_id~d6$fishing_event_id2)
# plot(d6$trip_id~d6$trip_id2)
