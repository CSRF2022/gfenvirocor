# compile dataset from gfsynopsis cache
# could also be downloaded directly with gfdata
library(tidyverse)
dir.create("data-raw", showWarnings = FALSE)


# list of species of interest
# species_common_name <- c("Petrale Sole")
# species_list <- as.data.frame(species_common_name)

species_list <- c("Petrale Sole", "Canary Rockfish")

# species <- species_list[i]
#
# spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))
# d <- readRDS(paste0(dr, spp, ".rds"))


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

  # subset to species of interest
  sets <- filter(all_set_dat, species_common_name %in% tolower(species_list))
  saveRDS(sets, file = "data-raw/survey-sets.rds")


  all_specimen_dat <- purrr::map_dfr(seq_along(f), function(i) {
    cat(f[i], "\n")
    d <- readRDS(f[i])$survey_samples
  })

  specimens <- filter(all_specimen_dat, species_common_name %in% tolower(species_list))
  saveRDS(specimens, file = "data-raw/specimen-data.rds")
} else {
  sets <- readRDS("data-raw/survey-sets.rds")
  specimens <- readRDS("data-raw/specimen-data.rds")
}
