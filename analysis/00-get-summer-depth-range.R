# get depths
library(tidyverse)
library(reldist)

gf <- readxl::read_xlsx("data/GF_assessments.xlsx")# %>% filter(Outputs == "Y")

# gf <- read_csv("data/GF_assessments.csv")

for (i in 1:length(gf$Species)){
  browser()
  spp <- gsub(" ", "-", gsub("\\/", "-", tolower(gf$Species[i])))

  survey_sets <- readRDS(paste0("../../gfsynopsis/report/data-cache/", spp, ".rds"))$survey_sets

  survey_sets$weights <- as.integer(round(survey_sets$density_kgpm2 * 100000))
  survey_sets <- survey_sets[!is.na(survey_sets$depth_m), ]
  survey_sets <- survey_sets[!is.na(survey_sets$weights), ]

  gf$depth_0.025[i] <- round(reldist::wtd.quantile(survey_sets$depth_m, 0.025,
                                                   weight = survey_sets$weights,
                                                   na.rm = TRUE
  ))

  gf$depth_0.975[i] <- round(reldist::wtd.quantile(survey_sets$depth_m, 0.975,
                                                   weight = survey_sets$weights,
                                                   na.rm = TRUE
  ))
  # gf$depth_max[i] <- round(quantile(survey_sets$depth_m, 0.999, na.rm = TRUE))
}

write_csv(gf, "data/GF_assessments.csv") # I then copied the depth columns over to the xlsx version
