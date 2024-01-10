# check which surveys to include for which species
library(tidyverse)
options(scipen = 100, digits = 4)
theme_set(ggsidekick::theme_sleek())

f1 <- list.files(paste0("data-generated/surv-summary/"), pattern = ".rds", full.names = TRUE)
d1 <- purrr::map_dfr(f1, readRDS)

# saveRDS(d1, "data-generated/surv-summary-pre-adding-unsampled-surveys.rds")
# d0 <- readRDS("data-generated/surv-summary-pre-adding-unsampled-surveys.rds")
# saveRDS(d0, "data-generated/surv-summary-split-w-all-weights.rds")
# d0x <- readRDS("data-generated/surv-summary-split-w-min-6.rds")

d2 <- filter(d1,
                # round(prop_pos, 2) >= 0.01 &
                prop_pos >= 0.01 &
                # (max_pos_by_year < 5 & change_prop > -0.75) &
                # pos_n >= 9 &
                max_pos_by_year >= 3 &
                !(prop_years_w_0 > 0.5 & max_pos_by_year < 5) &
                round(max_prop_pos, 2) >= 0.05
             )

d3 <- anti_join(d0, d2)

d4 <- filter(d1, species == species_list[[1]][[2]])

d2 <- d2 %>% mutate(group_name = case_when(group_name=="Males"~"Mature males",
                                           group_name=="Females"~"Mature females",
                                           TRUE ~ group_name))

# d2 %>% ggplot() + geom_point(aes(mean_ratio_w_0, mean_ratio_filled, size = total_samples))
d2 %>% ggplot() + geom_point(aes(mean_ratio_true, mean_ratio_filled,
                                 shape = group_name,
                                 alpha = min_samples,
                                 size = max_samples, colour = survey_type)) +
  facet_wrap(~species) +
  coord_fixed()

# saveRDS(d1, "data-generated/surv-summary-pre-adding-unsampled-surveys.rds")

# d3 <- filter(d1, prop_pos < 0.01|
#                max_pos_by_year < 3 |
#                pos_n < 9 |
#                round(max_prop_pos, 2) < 0.05)


# rerun with other classes


