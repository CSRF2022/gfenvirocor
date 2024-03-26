## add gfdata pulls here
#
#
#
#
#
#
#
#


library(tidyverse)
# for condition model (inclusive of those for density models)
surveys_included <- c("HBLL OUT N", "HBLL OUT S",
                      "IPHC FISS",
                      # maybe remove because at different time of year than all the others
                      "SABLE", # only have weights for certain species anyway?
                      "MSSM QCS", "MSSM WCVI",
                      "OTHER", # filtered to two older bottom trawl surveys + hake
                      "HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")


dset <- readRDS("data-raw/survey-sets-flatfish.rds") %>%
  bind_rows(., readRDS("data-raw/survey-sets-part2.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-sets-part3.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-sets-shortspine.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-sets-rougheye.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-sets-hake.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-sets-quillback.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-sets-skates.rds")) %>%
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

saveRDS(dset, "data-generated/all-sets-used.rds")


dsamp <- readRDS("data-raw/survey-samples-part2.rds") %>%
  bind_rows(., readRDS("data-raw/survey-samples-part3.rds")) %>%
  # bind_rows(., readRDS("data-raw/survey-samples-part4.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-samples-flatfish.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-samples-rougheye.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-samples-shortspine.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-samples-hake.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-samples-quillback.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-samples-skates.rds")) %>%
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
  distinct()

saveRDS(dsamp, "data-generated/all-samples-used.rds")


