# compile dataset from gfsynopsis cache
# could also be downloaded directly with gfdata

library(tidyverse)
dir.create("data-raw", showWarnings = FALSE)


# # list of species of interest
# species_list <- c("Petrale Sole", "Canary Rockfish", "Arrowtooth Flounder",
#                   "North Pacific Spiny Dogfish",
#                   "Pacific Cod")

species_list <- c(
  "Petrale Sole", #
  "Arrowtooth Flounder",#
  "English Sole",#
  "Dover Sole",#
  "Flathead Sole",#
  "Southern Rock Sole",#
  "Rex Sole", #
  "Curlfin Sole",#
  "Sand Sole",#
  "Slender Sole",#
  "Pacific Sanddab",#
  "Pacific Halibut",#
  "Butter Sole",#
  "Starry Flounder",#
  "C-O Sole", #
  "Deepsea Sole" #
)


if (Sys.info()[["user"]] == "dfomac") {

  # update for where you have gfsynopsis data cache stored
  dr <- "../../gfsynopsis-data-cache/data-cache"

  # to get everything?
  f <- list.files(dr, full.names = TRUE)
  f <- f[!grepl("cpue", f)]
  f <- f[!grepl("iphc", f)]

  all_set_dat <- purrr::map_dfr(seq_along(f), function(i) {
    cat(f[i], "\n")
    d <- readRDS(f[i])$survey_sets
  })


  # saveRDS(all_set_dat, file = "data-raw/survey-sets-all-species.rds")

  # subset to species of interest
  sets <- filter(all_set_dat, species_common_name %in% tolower(species_list))
  saveRDS(sets, file = "data-raw/survey-sets.rds")


  all_specimen_dat <- purrr::map_dfr(seq_along(f), function(i) {
    cat(f[i], "\n")
    d <- readRDS(f[i])$survey_samples
  })
  # saveRDS(all_specimen_dat, file = "data-raw/specimen-data-all-species.rds")

  specimens <- filter(all_specimen_dat, species_common_name %in% tolower(species_list))
  saveRDS(specimens, file = "data-raw/specimen-data.rds")
} else {
  sets <- readRDS("data-raw/survey-sets.rds")
  specimens <- readRDS("data-raw/specimen-data.rds")
}


all_set_dat <- readRDS("data-raw/survey-sets-all-species.rds")

all_specimen_dat <- readRDS("data-raw/specimen-data-all-species.rds")
### investigate duplicated fishing event for petrale sole
id <- 886323

### investigate duplicated fishing event for arrowtooth
id <- 308793
id <- 328882


all_set_dat[all_set_dat$fishing_event_id == id, ] %>% View()


event_samples_list <-  all_specimen_dat[all_specimen_dat$fishing_event_id == id, ] %>%
  group_by(sample_id, species_common_name) %>%
  summarise(sum_weight = sum(weight, na.rm = T)/1000,
            n = n())

event_catch_list <-all_set_dat[all_set_dat$fishing_event_id == id, ] %>% filter(catch_weight >0)  %>%
  select(species_common_name, catch_weight, catch_count, density_kgpm2) %>% distinct()

left_join(event_catch_list, event_samples_list)


# check if it's possible that both samples were petrale by estimating missing weights
dat <- readRDS("data-raw/specimen-data.rds") %>% filter(species_common_name == tolower(species_list))
mm <- gfplot::fit_length_weight(dat, sex = "male")
df <- dplyr::filter(dat, sex == 2, !is.na(weight), !is.na(length))
dm <- dplyr::filter(dat, sex == 1, !is.na(weight), !is.na(length))

est_weights$weight <- exp(mm$pars$log_a) * est_weights$length^mm$pars$b * 1000
est_weights <- filter(dat, is.na(weight) & fishing_event_id == 886323)
sum(est_weights$weight)/1000 + 6.23
# yes, looks like they are!
