# Make boxplots of input density data
library(tidyverse)
theme_set(ggsidekick:::theme_sleek())


flatfish<- tolower(c(
  "Curlfin Sole",#
  "Butter Sole",
  "Sand Sole",#
  "Petrale Sole", #
  "Arrowtooth Flounder", #
  "English Sole",#
  "Dover Sole",#
  "Rex Sole", #
  "Flathead Sole",#
  "Southern Rock Sole",#
  "Slender Sole",#
  "Pacific Sanddab",#
  "Pacific Halibut"#
))

rockfish<- tolower(c(
  "Pacific Ocean Perch",
  "Bocaccio",
  "Canary Rockfish",
  "Redstripe Rockfish", # MSA added with mean > 4
  "Rougheye/Blackspotted Rockfish Complex", # WILL NEED UPDATE FOR ALL MAT CLASSES
  "Silvergray Rockfish", # MSA added with mean > 5
  "Shortspine Thornyhead",
  "Widow Rockfish", # hake would need mean > 1, mssm1 > 4
  "Yelloweye Rockfish",
  "Yellowmouth Rockfish", #
  "Yellowtail Rockfish"
))

f1 <- list.files(paste0("data-generated/density-data/"), pattern = ".rds", full.names = TRUE)
d1 <- purrr::map_dfr(f1, readRDS)

d1$density_kgha <- d1$catch_weight/(d1$area_swept_km2*100)
d1$group_density_kgha <- d1$group_catch_est/(d1$area_swept_km2*100)
# d1$survey_type <- relevel(d1$survey_type, "SYN")
d1$survey_abbrev <- ifelse(d1$survey_abbrev %in% c("OTHER", "SYN WCVI"), "OTHER/SYN WCVI", d1$survey_abbrev)
d1$survey_abbrev <- ifelse(d1$survey_abbrev %in% c("HS MSA", "SYN HS"),"MSA/SYN HS", d1$survey_abbrev)

d1 <- d1 %>% mutate(group_name = case_when(group_name=="Males"~"Mature males",
                                           group_name=="Females"~"Mature females",
                                           TRUE ~ group_name),
                    icon = ifelse(use_within_yr_prop, "*"," "),
                    species_label = paste(
                      ifelse(species_common_name== "rougheye/blackspotted rockfish complex",
                             "rougheye complex", species_common_name), icon))


# d1 %>%
#   filter(group_name %in% c("Mature", "Females", "Mature females")) %>% #
#   ggplot() +
#   geom_boxplot(aes(as.factor(year), log(density_kgha +1), colour = survey_abbrev)) +
#   facet_wrap(~survey_abbrev, ncol = 2, scales = "free_y")


split_survey_boxplots <- function(data){
  data %>%
    filter(group_name %in% c("Mature males", "Mature females", "Immature")) %>%
    ggplot() +
    geom_boxplot(aes(as.factor(year), log(group_density_kgha + 1),
                     fill = survey_abbrev,
                     colour = survey_abbrev), alpha = 0.2) +
    facet_grid(species_common_name~group_name, scales = "free_y")
  # facet_grid(survey_abbrev~species_common_name, scales = "free_y")
}

split_boxplots <- function(data){
  data <- data %>%
    filter(group_name %in% c("Mature males", "Mature females", "Immature")) %>%
    filter(density_kgha > 0 & group_density_kgha > 0) #%>%
  ggplot(data) +
    geom_violin(data = data[data$group_name == "Mature females",],
                aes(as.factor(year),
                    log(density_kgha + 1)),
                # (density_kgha)),
                # log(density_kgha)),
                # scale = "width",
                scale = "count",
                fill = "lightgrey",
                colour = "lightgrey") +
    geom_boxplot(aes(as.factor(year),
                     log(group_density_kgha + 1),
                     # (group_density_kgha),
                     # log(group_density_kgha),
                     fill = group_name,
                     colour = group_name),
                 outlier.size = 0.1,
                 position = position_dodge(preserve = "single", width = 1),
                 # outlier.shape =".",
                 alpha = 0.5)
}

d1 %>%
  mutate(year = year - 2000) %>%
  filter(survey_abbrev %in% c("MSSM WCVI")) %>%
  split_boxplots()+
  facet_wrap(~species_common_name, ncol = 3, scales = "free_y") +
  ggtitle("MSSM WCVI when present")

ggsave("figs/boxplots-mssm-WCVI-split.png", height = 10, width = 16)


d1 %>%
  mutate(year = year - 2000) %>%
  filter(survey_abbrev %in% c("MSSM QCS")) %>%
  split_boxplots()+
  facet_wrap(~species_common_name, ncol = 3, scales = "free_y") +
  ggtitle("MSSM QCS when present")

ggsave("figs/boxplots-mssm-QCS-split.png", height = 10, width = 16)

d1 %>%
  mutate(year = year - 2000,
         group_name = case_when(group_name=="Males"~"Mature males",
                                group_name=="Females"~"Mature females",
                                TRUE ~ group_name)
  ) %>%
  filter(survey_type %in% c("SYN")) %>%
  split_boxplots()+
  facet_wrap(~species_common_name, ncol = 3, scales = "free_y") +
  ggtitle("SYN when present")

ggsave("figs/boxplots-SYN-split.png", height = 14, width = 16)

# d1 %>%
#   mutate(year = year - 2000) %>%
#   filter(survey_type %in% c("HAKE")) %>%
#   split_boxplots()+
#   facet_wrap(~species_common_name, ncol = 3, scales = "free_y") +
#   ggtitle("HAKE when present")
#
# ggsave("figs/boxplots-HAKE-split.png", height = 6, width = 10)


#   d1 %>% filter(year == 2005, survey_abbrev %in% c("MSSM WCVI"), species_common_name == "pacific halibut") %>%
#   group_by(group_name) %>%
#   # summarise_all(mean)
#   summarise(total = mean(density_kgha),
#             group_n = mean(group_n),
#             group_weight = mean(group_weight),
#             ratio = mean(proportion),
#             group = mean(group_catch_est))
# ggplot() + geom_histogram(aes(group_n)) + facet_wrap(~group_name)


d1 %>% filter(species_common_name %in% flatfish) %>%
  filter(!(species_common_name %in% c(
    "curlfin sole", "butter sole", "sand sole"))) %>%
  mutate(year = year - 2000) %>%
  filter(survey_type %in% c("SYN")) %>%
  split_boxplots ()+
  # facet_wrap(~species_common_name, ncol = 3, scales = "free_y") +
  facet_grid(species_common_name~survey_abbrev, scales = "free_y") +
  ggtitle("SYN Flatfish  when present")

ggsave("figs/boxplots-flatfish-syn-split.png", height = 12, width = 14)

d1 %>% filter((species_common_name %in% rockfish)) %>%
  mutate(year = year - 2000) %>%
  filter(survey_type %in% c("SYN")) %>%
  split_boxplots ()+
  facet_grid(species_common_name~survey_abbrev, scales = "free_y") +
  ggtitle("SYN Rockfish when present")

ggsave("figs/boxplots-rockfish-syn-split.png", height = 10, width = 14)

d1 %>% filter(!(species_common_name %in% c(rockfish,flatfish))) %>%
  mutate(year = year - 2000) %>%
  filter(survey_type %in% c("SYN")) %>%
  split_boxplots ()+
  facet_grid(species_common_name~survey_abbrev, scales = "free_y") +
  ggtitle("SYN Other when present")

ggsave("figs/boxplots-other-syn-split.png", height = 6, width = 14)





density_boxplots <- function(data){
  data %>%
    filter(group_name %in% c("Mature", "Females", "Mature females")) %>%
    ggplot() +
    geom_boxplot(aes(as.factor(year), log(density_kgha + 1), colour = survey_abbrev),
                 # width=0.5/length(unique(data$survey_abbrev)),
                 position = position_dodge2(preserve = "single"),
                 outlier.size = 0.5,
                 alpha = 0.3,
                 fill = "white") +
    theme(legend.position = "top") +
    facet_wrap(~species_common_name, ncol = 2, scales = "free_y")
  # facet_grid(rows = vars(fct_rev(species_common_name)), scales = "free_y")
  # facet_grid(survey_abbrev~species_common_name, scales = "free_y")
}

d1 %>% filter(species_common_name %in% flatfish) %>%
  filter(!(species_common_name %in% c(
    "curlfin sole", "butter sole", "sand sole"))) %>%
  filter(survey_type %in% c("MSSM<=05", "MSSM>05")) %>%
  density_boxplots ()+
  ggtitle("Flatfish")

ggsave("figs/boxplots-flatfish-mssm-total.png", height = 10, width = 12)
# ggsave("figs/boxplots-flatfish-mssm-total.png", height = 14, width = 8)

d1 %>% filter(species_common_name %in% rockfish) %>%
  filter(survey_type %in% c("MSSM<=05", "MSSM>05")) %>%
  density_boxplots ()+
  ggtitle("Rockfish")

ggsave("figs/boxplots-rockfish-mssm-total.png", height = 9, width = 12)

d1 %>% filter(!(species_common_name %in% c(rockfish,flatfish))) %>%
  filter(survey_type %in% c("MSSM<=05", "MSSM>05")) %>%
  density_boxplots () +
  ggtitle("Other")

ggsave("figs/boxplots-other-mssm-total.png", height = 6, width = 12)


# d1 %>% filter(!(species_common_name %in% c(flatfish))) %>%
#   filter(survey_type %in% c("MSSM<=05", "MSSM>05")) %>%
#   density_boxplots ()
#
# ggsave("figs/boxplots-nonflat-mssm-total.png", height = 10, width = 12)


d1 %>% filter(species_common_name %in% flatfish) %>%
  filter(!(species_common_name %in% c(
    "curlfin sole", "butter sole", "sand sole"))) %>%
  filter(survey_type %in% c("SYN")) %>%
  density_boxplots ()+
  ggtitle("Flatfish") +
  scale_color_brewer(palette = "Set1")
#
ggsave("figs/boxplots-flatfish-syn.png", height = 14, width = 14)
# #
d1 %>% filter((species_common_name %in% rockfish)) %>%
  filter(survey_type %in% c("SYN")) %>%
  density_boxplots ()+
  ggtitle("Rockfish")+
  scale_color_brewer(palette = "Set1")

ggsave("figs/boxplots-rockfish-syn.png", height = 14, width = 14)

#
d1 %>% filter(!(species_common_name %in% c(rockfish,flatfish))) %>%
  filter(survey_type %in% c("SYN")) %>%
  density_boxplots ()+
  scale_color_brewer(palette = "Set1")+
  ggtitle("Other")

ggsave("figs/boxplots-other-syn.png", height = 6, width = 14)



all_density_boxplots <- function(data){
  data %>%
    filter(group_name %in% c("Mature", "Females", "Mature females")) %>%
    ggplot() +
    geom_boxplot(aes(as.factor(year), log(density_kgha + 1), colour = species_common_name),
                 position = position_dodge(preserve = "single", width = 1.01),
                 outlier.size = 0.5,
                 alpha = 0.3,
                 fill = "white") +
    theme(legend.position = "top") +
    facet_grid(rows = vars(fct_rev(survey_abbrev)), scales = "free_y")
  # facet_grid(survey_abbrev~species_common_name, scales = "free_y")
}


d1 %>% filter(!(species_common_name %in% c(rockfish,flatfish))) %>%
  all_density_boxplots ()

ggsave("figs/boxplots-other-all-total.png", height = 8.5, width = 12)


d1 %>% filter((species_common_name %in% c(rockfish))) %>%
  all_density_boxplots () + guides(colour=guide_legend(nrow=2))

ggsave("figs/boxplots-rockfish-all-total.png", height = 9, width = 16)



d1 %>% filter((species_common_name %in% c(flatfish))) %>%
  filter(!(species_common_name %in% c(
    "curlfin sole", "butter sole", "sand sole"))) %>%
  all_density_boxplots () + guides(colour=guide_legend(nrow=2))

ggsave("figs/boxplots-flatfish-all-total.png", height = 9, width = 14)



dd <- d1 %>% group_by(species_common_name, fishing_event_id) %>% mutate(
  sum_group_ests = round(sum(group_catch_est), 2),
  diff = sum_group_ests - catch_weight,
  perc_diff = (diff/catch_weight)
)

dd2 <- dd %>% filter(perc_diff > 0.005) #%>% View()
dd %>% filter(diff != 0) %>% #View()
  ggplot() + geom_histogram(aes(perc_diff)) + facet_wrap(~species_common_name, scales = "free")

ds <- d1 %>% filter(group_name == "Mature females",
                    n_fish_sampled > 0, #n_fish_sampled < 6,
                    n_events_sampled >= 6#, n_events_sampled < 10
) %>%
  select(species_common_name, year, survey_series_id) %>% distinct()

ds2 <- left_join(ds, d1) %>%
  group_by(species_common_name, group_name, year, survey_abbrev) %>%
  summarise(total_fish = sum(n_fish_sampled, na.rm = TRUE),
            total_events = mean(n_events_sampled),
            ratio = mean(proportion)
  ) #%>%


min_sample_number <- 6
# min_sample_number <- 9


ds2 <- d1 %>% filter(#group_name == "Mature females",
  n_fish_sampled > 0, #n_fish_sampled < 6,
  # n_events_sampled >= min_sample_number #, n_events_sampled < 10
) %>%
  group_by(species_label, group_name, year, survey_type, survey_series_id) %>%
  summarise(#total_fish = sum(n_fish_sampled, na.rm = TRUE),
    total_fish = mean(n_fish_by_surv_yr, na.rm = TRUE),
    total_events = mean(n_events_sampled, na.rm = TRUE),
    total_catch = sum(catch_weight),
    ratio = mean(proportion)
  ) #%>%


ds2 %>% filter(total_events > 0) %>%
  ggplot(aes(total_events, ratio, size = total_catch,
             alpha = year,
             shape = survey_type,
             fill = group_name, colour = group_name)) +
  geom_rect(aes(xmin = 1, xmax = 6 - 0.5,
                ymin = -0.05, ymax = 1.05),
            fill = "lightgrey", colour = NA) +
  geom_hline(yintercept = 0.33, colour = "darkgrey") +
  geom_vline(xintercept = 6 - 0.5, colour = "darkgrey", linetype = "dashed") +
  geom_vline(xintercept = min_sample_number - 0.5, colour = "darkgrey", linetype = "dashed") +
  geom_vline(xintercept = 9 - 0.5, colour = "darkgrey", linetype = "dashed") +
  # geom_vline(xintercept = 10 - 0.5, colour = "darkgrey", linetype = "dashed") +
  # geom_smooth(method = "gam") +
  geom_point() +
  coord_cartesian(expand = FALSE) +
  scale_x_log10() +
  facet_wrap(~species_label, scales = "free_x") +
  xlab("Annual number of locations sampled") +
  ylab("Proportion in each maturity class") +
  ggtitle("Ratios relative to number of locations (sets) sampled each year on each survey",
          subtitle = "Points in grey area use higher level ratios, any dashed verticle lines are proposed thresholds for this behaviour")

ggsave("figs/ratios-by-locations-sampled-2.png", width = 12, height = 8)

ds2 %>%
  filter(#total_fish < 200,
    total_events >= min_sample_number) %>%
  ggplot(aes(total_fish, ratio, size = total_catch,
             alpha = year,
             shape = survey_type,
             fill = group_name, colour = group_name)) +
  geom_hline(yintercept = 0.33, colour = "darkgrey") +
  geom_rect(aes(xmin = 1, xmax = min_sample_number*3 - 0.5,
                ymin = -0.05, ymax = 1.05),
            fill = "lightgrey", colour = NA) +
  geom_vline(xintercept = min_sample_number - 0.5, colour = "darkgrey", linetype = "dashed") +
  geom_vline(xintercept = min_sample_number*2- 0.5, colour = "darkgrey", linetype = "dashed") +
  geom_vline(xintercept = min_sample_number*3- 0.5, colour = "darkgrey", linetype = "dashed") +
  scale_x_log10() +
  coord_cartesian(expand = FALSE,
                  ylim = c( -0.05, 1.05),
                  xlim = c(min_sample_number - 0.5,NA)) +
  # geom_smooth(method = "gam") +
  geom_point(data = filter(ds2, total_events < min_sample_number), colour = "black") +
  geom_point() +
  facet_wrap(~species_label, scales = "free_x")+
  xlab("Annual number of fish sampled") +
  ylab("Proportion in each maturity class") +
  ggtitle("Ratios relative to number of fish measured each year on each survey",
          subtitle = "Black points and those in grey area use higher level ratios. Dashed verticle lines represent 2 or 3 times as many fish as the min number of sets.")

ggsave("figs/ratios-by-fish-sampled-2.png", width = 12, height = 8)

# x <- d1 %>%
#   filter(survey_abbrev == "MSSM QCS" & species_common_name == "sablefish" & year == 2008)
