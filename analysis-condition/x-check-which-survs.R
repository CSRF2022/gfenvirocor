# check which surveys to include for which species
library(dplyr)

f1 <- list.files(paste0("data-generated/surv-summary/"), pattern = ".rds", full.names = TRUE)

d1 <- purrr::map_dfr(f1, readRDS)
# saveRDS(d1, "data-generated/surv-summary-pre-adding-unsampled-surveys.rds")
# d0 <- readRDS("data-generated/surv-summary-pre-adding-unsampled-surveys.rds")

d2 <- filter(d1,
                round(prop_pos, 2) >= 0.01 &
                # (max_pos_by_year < 5 & change_prop > -0.75) &
                # pos_n >= 9 &
                max_pos_by_year >= 3 &
                !(prop_years_w_0 > 0.5 & max_pos_by_year < 5) &
                round(max_prop_pos, 2) >= 0.05
             )

d3 <- anti_join(d1, d2)

# saveRDS(d1, "data-generated/surv-summary-pre-adding-unsampled-surveys.rds")

# d3 <- filter(d1, prop_pos < 0.01|
#                max_pos_by_year < 3 |
#                pos_n < 9 |
#                round(max_prop_pos, 2) < 0.05)


# rerun with other classes



mf$data %>% #filter(usability_code != 0) %>%
  # filter(year %in% c(2003, 2004)) %>%
  ggplot() + geom_histogram(aes(depth_m)) +
  geom_histogram(aes(depth_m), fill = "red", data = filter(m$data, catch_weight > 0)) +
  facet_wrap(~survey_type, scales = "free_y")

