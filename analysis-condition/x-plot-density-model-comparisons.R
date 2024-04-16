# compare models with different split data
library(tidyverse)
theme_set(ggsidekick:::theme_sleek())

# these files are built while running the condition models
# only species used there will be included here
f1 <- list.files(paste0("data-generated/compare-models/"),
                 pattern = ".rds", full.names = TRUE)
d <- purrr::map_dfr(f1, readRDS)

d <- filter(d, !(species %in% species_to_remove))

d <- d %>% mutate(within_ci = ifelse(too_low == 0 & too_high == 0, TRUE, FALSE),
                  species = ifelse(
                    species == "Rougheye/Blackspotted Rockfish Complex",
                               "Rougheye/Blackspotted Complex", species)
                    )

ggplot(d) + geom_boxplot(aes(model_string, prop_diff))
d1 <- filter(d, prop_ci_error < 0.05)
d2 <- filter(d, prop_ci_error > 0.05)


# absolute error
ggplot(d, aes(forcats::fct_rev(species), prop_diff, colour = model_string)) +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_point(size = 3) +
  geom_point(data = d1, aes(alpha = prop_ci_error), colour = "black", size = 2) +
  geom_point(data = d2, colour = "black", size = 2) +
  scale_alpha_continuous(range = c(0,0.5)) +
  guides(alpha = "none", colour=guide_legend(nrow=2)) +
  scale_color_discrete(
    labels = c("All surveys with postive catches on >= 3 years & > 1% overall",
    "Only survey-years with morphological data from >= 18 fish & >= 6 sets")
    ) +
  labs(x = "",
       y = "Absolute proportional deviation in sum of split indices from overall index",
       colour = "",
       title = "Comparison of maturity-specific indices of abundance",
       subtitle = "Inner circle shade indicates when the sum exceeds the CI on the overall index (solid black = > 5% of total biomass)."
       # subtitle = "Dark shading indicates a sum that exceeded the CI on the overall index in some years."
  ) +
  coord_flip() +
  theme(legend.position = "top", legend.justification = "left",
        plot.title = element_text(hjust=0.1),
        plot.subtitle = element_text(hjust=0.4),
        plot.title.position = "plot")

ggsave("figs/model-comparison-abs-diff.png", height = 7, width = 9)


# average direction of error?
ggplot(d, aes(forcats::fct_rev(species), mean_diff, colour = model_string)) +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_point(size = 3) +
  geom_point(data = d1, aes(alpha = prop_ci_error), colour = "black", size = 2) +
  geom_point(data = d2, colour = "black", size = 2) +
  scale_alpha_continuous(range = c(0,0.5)) +
  guides(alpha = "none", colour=guide_legend(nrow=2)) +
  scale_color_discrete(
    labels = c(
               "All surveys with postive catches on >= 3 years & > 1% overall",
               "Only survey-years with morphological data from >= 18 fish & >= 6 sets"
    )) +
  labs(x = "",
       y = "Mean proportional deviation in sum of split indices from overall index",
       colour = "",
       title = "Comparison of maturity-specific indices of abundance ",
       # subtitle = "Shading indicates that CI on the overall index was exceeded (solid black = > 5% of total biomass)."
       subtitle = "Inner circle shade indicates when the sum exceeds the CI on the overall index (solid black = > 5% of total biomass)."
  ) +
  coord_flip() +
  theme(legend.position = "top", legend.justification = "left",
        plot.title = element_text(hjust=0.1),
        plot.subtitle = element_text(hjust=0.5),
        plot.title.position = "plot",
        plot.caption.position = "plot")

ggsave("figs/model-comparison-mean-diff.png", height = 7, width = 9)

