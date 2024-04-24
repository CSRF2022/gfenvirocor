## add gfdata pulls here
# remotes::install_github("pbs-assess/gfdata", ref = "trials")
# library(gfdata)
# # all flatfish
# species_list <- c(
#   "Petrale Sole", #
#   "Arrowtooth Flounder",#
#   "English Sole",#
#   "Dover Sole",#
#   "Flathead Sole",#
#   "Southern Rock Sole",#
#   "Rex Sole", #
#   "Curlfin Sole",#
#   "Sand Sole",#
#   "Slender Sole",#
#   "Pacific Sanddab",#
#   "Pacific Halibut",#
#   "Butter Sole",#
#   "Starry Flounder",#
#   "C-O Sole", #
#   "Deepsea Sole" #
# )
#
# # other species
# species_list <- c(
#   "North Pacific Spiny Dogfish",
#   "Lingcod",
#   "Pacific Cod",
#   "Walleye Pollock",
#   "Sablefish",
#   "Bocaccio",
#   "Canary Rockfish",
#   "Pacific Ocean Perch",
#   "Redstripe Rockfish",
#   "Silvergray Rockfish",
#   "Widow Rockfish",
#   "Yellowmouth Rockfish",
#   "Yellowtail Rockfish",
#   "Yellowmouth Rockfish",
#   "Yelloweye Rockfish",
#   "Shortspine Thornyhead"
#   "Pacific Hake",
#   "Quillback Rockfish",
#   "Redbanded Rockfish",
##  "Copper Rockfish",
#   "Shortraker Rockfish",
#   "Rosethorn Rockfish",
#   "Harlequin Rockfish",
#   "Pygmy Rockfish",
#   "Sharpchin Rockfish",
#   "Darkblotched Rockfish",
#   "Greenstriped Rockfish",
#   "Spotted Ratfish",
#   "Sandpaper Skate",
#   "Pacific Tomcod",
#   "Big Skate",
#   "Longnose Skate"
# )
#
# # # rougheye/blackspotted species complex
# species_list <- c(
#  394
# )
# tictoc::tic()
# dd <- get_survey_sets2(species_list, ssid = NULL,
#                        remove_false_zeros = TRUE, usability = NULL)
# tictoc::toc()
#
# saveRDS(dd, "data-raw/survey-sets-all.rds")
# saveRDS(dd, "data-raw/survey-sets-all-2.rds")
# saveRDS(dd, "data-raw/survey-sets-rougheye.rds")
#
# tictoc::tic()
# ds <- get_survey_samples2(species_list,
#                           include_event_info = TRUE,
#                           unsorted_only = FALSE)
# tictoc::toc()
#
# saveRDS(ds, "data-raw/survey-samples-all-1.rds")
# saveRDS(ds, "data-raw/survey-samples-all-2.rds")
# saveRDS(ds, "data-raw/survey-samples-all-3.rds")
# saveRDS(ds, "data-raw/survey-samples-rougheye.rds")

library(tidyverse)
# for condition model (inclusive of those for density models)
surveys_included <- c("HBLL OUT N", "HBLL OUT S",
                      "IPHC FISS",
                      # maybe remove because at different time of year than all the others
                      # for now retaining for length at maturity calculation, but removed from condition analysis
                      "SABLE", # only have weights for certain species anyway?
                      "MSSM QCS", "MSSM WCVI",
                      "OTHER", # filtered to two older bottom trawl surveys + hake
                      "HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")


dset <- readRDS("data-raw/survey-sets-all.rds") %>%
  bind_rows(., readRDS("data-raw/survey-sets-all-2.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-sets-rougheye.rds")) %>%
  # this removes duplications and non-Canadian data
  filter(
    survey_abbrev %in% surveys_included,
    ## remove the combined version of the sablefish survey which is only one retained above
    ## because this survey is at different time of year than all the others
    !(survey_abbrev %in% c("SABLE")),
    # !(survey_abbrev %in% c("SABLE INLET", "SABLE OFF", "SABLE RAND")),
    ## some MSSM sets are in both as QCS and WCVI
    !(survey_series_id == 6 & latitude < 50),
    !(survey_series_id == 7 & latitude > 50),
    # 11 useful for deeper species, 9 from 1996 probably too limited, 68 = HAKE
    !(survey_abbrev == "OTHER" & !(survey_series_id %in% c(9, 11, 68)))
  ) %>%
  # remove non-Canadian Hake samples
  filter(!(survey_series_id == 68 & latitude > 55.4),
         !(survey_series_id == 68 & latitude < 48)) %>%
  mutate(
    survey_abbrev = ifelse(survey_abbrev == "MSSM" & latitude < 50, "MSSM WCVI",
                           ifelse(survey_abbrev == "MSSM" & latitude > 50, "MSSM QCS", survey_abbrev)
    ),
    survey_abbrev = ifelse(survey_series_id == 68, "HAKE", survey_abbrev),
    # survey_area = ifelse(survey_abbrev == "HS MSA", "SYN HS",
    #                 ifelse(survey_abbrev == "MSSM QCS", "SYN QCS",
    #                 ifelse(survey_abbrev == "MSSM WCVI", "SYN WCVI",
    #                 survey_abbrev))),
    survey_type = as.factor(
      case_when(
        survey_abbrev == "HS MSA"~"MSA",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2002 & year<=2005~"MSSM<=05",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2005~"MSSM>05",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year <= 2002~"MSSM <03",
        survey_series_id == 68~"HAKE",
        survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")~"SYN",
        survey_abbrev %in% c("EUL N", "EUL S")~"EUL",
        TRUE~survey_abbrev
      ))
  ) %>% distinct()



dsamp <- readRDS("data-raw/survey-samples-all-1.rds") %>%
  bind_rows(., readRDS("data-raw/survey-samples-all-2.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-samples-all-3.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-samples-rougheye.rds")) %>%
  filter(survey_abbrev %in% surveys_included,
         !(survey_abbrev == "OTHER" & !(survey_series_id %in% c(9, 11, 68))),
         !(survey_series_id == 68 & latitude > 55.4),
         !(survey_series_id == 68 & latitude < 48)) %>%
  mutate(
    survey_abbrev = ifelse(survey_series_id == 68, "HAKE", survey_abbrev),
    survey_abbrev = ifelse(survey_abbrev == "MSSM" & latitude < 50, "MSSM WCVI",
                           ifelse(survey_abbrev == "MSSM" & latitude > 50, "MSSM QCS", survey_abbrev)
    ),
    survey_type = as.factor(
      case_when(
        survey_abbrev == "HS MSA"~"MSA",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2002 & year<=2005~"MSSM<=05",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year>2005~"MSSM>05",
        survey_abbrev %in% c("MSSM WCVI", "MSSM QCS") & year <= 2002~"MSSM <03",
        survey_series_id == 68~"HAKE",
        survey_abbrev %in% c("SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")~"SYN",
        survey_abbrev %in% c("EUL N", "EUL S")~"EUL",
        TRUE~survey_abbrev
      ))) %>%
  # causes duplication for some species with multiple samples collected per individual
  select(-dna_container_id, -dna_sample_type) %>%
  distinct()

# remove misidentified sandpaper skates from set data
dset <- dset %>% filter(!(species_common_name == tolower("Sandpaper Skate")
    & fishing_event_id %in% c(filter(dsamp, species_common_name == tolower("Sandpaper Skate")&length > 70)$fishing_event_id)))

# and then remove misidentified sandpaper skates from sample data
dsamp <- dsamp %>% filter(!(species_common_name == tolower("Sandpaper Skate")&length > 70))

# save combined processed data
saveRDS(dset, "data-generated/all-sets-used.rds")
saveRDS(dsamp, "data-generated/all-samples-used.rds")


check_for_duplicates <- dsamp[duplicated(dsamp$specimen_id), ]
unique(check_for_duplicates$species_common_name)
## will need to rerun these due to duplication of specimens
# "bocaccio"
# "yelloweye rockfish"
# "rougheye/blackspotted rockfish complex"
# test_id <- dsamp[dsamp$specimen_id == 15764008,]

weight_discrepencies <- dsamp %>% group_by(species_common_name, species_code, year, survey_abbrev, fishing_event_id) %>%
  filter(year >= 2002) %>%
  summarise(
    n = n(),
    num_ids = length(unique(specimen_id)),
    sample_sum_kg = sum(weight)/1000,
    catch_weight = mean(catch_weight),
    # missing_CW = max(catch_weight)-min(catch_weight),
    catch_count = mean(catch_count),
    # missing_CC = max(catch_count)-min(catch_count),
    count_diff = ifelse(catch_weight > 0, NA, catch_count - n),
    excess_weight = sample_sum_kg - catch_weight,
    excess_weight_per_fish = round((sample_sum_kg - catch_weight)/n, 3),
    percent_error = round(excess_weight/(mean(weight)/1000), 2)
    )

# filter(weight_discrepencies, excess_weight_per_fish > 0.1) %>% View()

single_fish <- filter(weight_discrepencies, n == 1)

hist(single_fish$excess_weight_per_fish, breaks = 50)



test_id <- dsamp |> filter(fishing_event_id == 5099787, species_code == "056")
test_id <- dsamp |> filter(fishing_event_id == 4363329, species_code == "614")


# # # check data for a specific species
# # species <- "Big Skate"
# species <- "Longnose Skate"

bad_skates <- dsamp2 %>% filter((weight > 2*1000 & length < 15) |
                                  (length < 5 & weight > 100)) #%>% View()

## get just petrale data
# species <- "Petrale Sole"
# dset <- readRDS("data-raw/survey-sets-all.rds")
# dsamp <- readRDS("data-raw/survey-samples-all-1.rds") %>%
#   bind_rows(., readRDS("data-raw/survey-samples-all-2.rds"))
# dset <- dset %>% filter(species_common_name == tolower(species))
# dsamp <- dsamp %>% filter(species_common_name == tolower(species))
#
# saveRDS(dset, "data-generated/all-sets-petrale.rds")
# saveRDS(dsamp, "data-generated/all-samples-petrale.rds")
#

species <- "Sandpaper Skate"
dsamp2 <- dsamp %>% filter(species_common_name == tolower(species))
# bad_skates <- dsamp %>%

# dset <- dset %>% filter(!(species_common_name == tolower("Sandpaper Skate") & fishing_event_id == filter(dsamp, species_common_name == tolower("Sandpaper Skate")&length > 75)$fishing_event_id))
#
# dsamp <- dsamp %>% filter(!(species_common_name == tolower("Sandpaper Skate")&length > 75))


filter(weight_discrepencies, (fishing_event_id %in% c(unique(bad_skates$fishing_event_id)))) %>%
    filter(species_common_name == tolower(species)) %>%
  #   # group_by(sample_id) %>%
    View()


# bad_skates$specimen_id
# bad_skates$survey_abbrev
#
#
# filter(weight_discrepencies, !(fishing_event_id %in% c(unique(bad_skates$sample_id)))) %>%
#   filter(species_common_name == tolower(species)) %>%
#   # group_by(sample_id) %>%
#   View()
# #
# test_id <- dsamp[dsamp$sample_id == 252056,]
#
# test_id <- dsamp[dsamp$fishing_event_id == 328913,]
# test_id <- weight_discrepencies[weight_discrepencies$fishing_event_id == 328913,]
#
# library(ggrepel)
# filter(dsamp2, !is.na(weight), (sample_id %in% c(unique(bad_skates$sample_id)))) %>%
#   ggplot() + geom_point(aes(length, weight, colour = as.factor(sample_id))) +
#   geom_text_repel(data = bad_skates, aes(length, weight, label = specimen_id),
#                    segment.color = 'grey80') +
#   geom_point(data = bad_skates, aes(length*10, weight), shape = 1, inherit.aes = FALSE) +
#
#   theme_bw()
# ggsave("longnose-skate-problem-specimens.png", height=4, width = 6)
