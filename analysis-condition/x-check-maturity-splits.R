### check proportions for full data set
library(tidyverse)
theme_set(ggsidekick:::theme_sleek())


f <- list.files(paste0("data-generated/split-catch-data/"),
                pattern = ".rds", full.names = TRUE)

d <- purrr::map(f, readRDS)

d2 <- do.call(bind_rows, lapply(d, `[[`, 1))

d2 <- d2 %>% mutate(group_name = case_when(group_name=="Males"~"Mature males",
                                           group_name=="Females"~"Mature females",
                                           TRUE ~ group_name))

d2 %>% filter(group_name == "Mature females" & year >= 2000) %>%
  select(species_common_name, year, survey_abbrev,
         # median_prop_ann, mean_prop_ann, proportion,
         n_events_sampled) %>%
  group_by(species_common_name, year, survey_abbrev) %>%
  summarise_all(mean) %>%
  distinct() %>%
  filter( n_events_sampled > 0) %>%
  ggplot() +
  geom_histogram(aes( n_events_sampled, fill = survey_abbrev),
                 binwidth = 3,
                 boundary = 0,
                 colour = NA,
                 alpha = 0.5) +
  geom_vline(xintercept = 6, linetype = "dashed", colour = "grey40") +
  facet_wrap(~species_common_name, scales = "free")

ggsave("figs/all-sample-counts-by-survey-yr-cutoff-6.png", height = 12, width = 14)

# d0 <- d2 %>% filter(group_name == "Immature" & species_common_name == "southern rock sole") %>%
#   select(species_common_name, year, survey_abbrev, median_prop_ann, proportion,  n_events_sampled, catch_weight) %>%
#   filter(catch_weight > 0) %>%
#   filter( n_events_sampled < 10)

select(d2, species_common_name, use_within_yr_prop) %>% distinct() %>% View()

dy <- filter(d2, species_common_name == "yelloweye rockfish") %>%
  select(fishing_event_id, group_name, survey_abbrev, year,
         n_fish_sampled, group_n, proportion, mean_prop_ann,
         n_events_sampled, group_catch_est) #%>% filter( n_fish_sampled >0)

# dy %>% filter( n_fish_sampled > 0) %>% ggplot() + geom_histogram(aes(proportion)) + facet_wrap(~group_name)

plot_mat_proportions <- function(
    dat,
    group = "Immature",
    agg_function = "mean",
    min_sample_number = 6
){

  group_low <- tolower(group)
  if(agg_function == "mean") {
    agg_prop <- "mean_prop_ann"
  } else {
    agg_prop <- "median_prop_ann"
  }

  dsamp <- dat %>% filter(group_name == group & year >= 2000) %>% mutate(
    icon = as.character(ifelse(use_within_yr_prop, "*", " ")),
    species_label = paste(species_common_name, icon)) %>%
    select(species_common_name, species_label, use_within_yr_prop, icon, year, survey_abbrev,
           mean_prop_ann, median_prop_ann, proportion,
           n_events_sampled,  n_fish_sampled, catch_weight) %>%
    filter( n_fish_sampled > 0) %>%
    filter(catch_weight > 0) %>%
    group_by(species_common_name, species_label, use_within_yr_prop, icon, year, survey_abbrev) %>%
    summarise_all(agg_function) %>%
    distinct()


  dat %>% filter(group_name == group & year >= 2000) %>%  mutate(
    icon = ifelse(use_within_yr_prop, "*"," "),
    species_label = paste(species_common_name, icon)) %>%
    select(species_common_name, species_label, use_within_yr_prop, year, survey_abbrev,
           mean_prop_ann, median_prop_ann, proportion,
           n_events_sampled,  n_fish_sampled, catch_weight) %>%
    filter(catch_weight > 0) %>%
    group_by(species_common_name, species_label, use_within_yr_prop, year, survey_abbrev) %>%
    dplyr::add_tally() %>%
    summarise_all(agg_function) %>%
    distinct() %>%
    filter( n_events_sampled < min_sample_number) %>%
    ggplot() +
    geom_point(aes(year, proportion,
                   size = (catch_weight * n)/100,
                   colour = survey_abbrev),
               # colour = "black",
               shape = 4,
               alpha = 0.5) +
    geom_point(data = dsamp,
               aes(year, .data[[agg_prop]], colour = survey_abbrev,
                   size = (n_events_sampled * n_fish_sampled)/1
               ), alpha = 0.5) +
    geom_line(data = dsamp %>% filter( n_events_sampled >= min_sample_number),
              aes(year, .data[[agg_prop]], colour = survey_abbrev)) +
    labs(size = "N", colour = "Survey") +
    facet_wrap(~species_label) +
    ggtitle(paste("Proportion", group_low,
                  "(o =", agg_function, "of samples, x = estimated from the mean within the more variable category—years(*) or surveys)"))
}



plot_mat_proportions(d2, "Mature females", min_sample_number = 6)
ggsave("figs/all-proportion-fem-means-sd-6-true-weights-2.png", height = 12, width = 14)

# not actually run using the median values to generate the xs
# but useful to see how things might change if it was
plot_mat_proportions(d2, "Mature females", "median", min_sample_number = 6)
ggsave("figs/all-proportion-fem-medians-sd-6-true-weights-2.png", height = 12, width = 14)


plot_mat_proportions(d2, "Mature males", min_sample_number = 6)
ggsave("figs/all-proportion-mal-means-sd-6-true-weights-2.png", height = 12, width = 14)


plot_mat_proportions(d2, "Immature", min_sample_number = 6)
ggsave("figs/all-proportion-imm-means-sd-6-true-weights-2.png", height = 12, width = 14)

# not actually run using the median values to generate the xs
# but useful to see how things might change if it was
plot_mat_proportions(d2, "Immature", "median", min_sample_number = 6)
ggsave("figs/all-proportion-imm-medians-sd-6-true-weights-2.png", height = 12, width = 14)



