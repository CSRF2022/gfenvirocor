# compare models with different split data
library(tidyverse)
theme_set(ggsidekick:::theme_sleek())

f1 <- list.files(paste0("data-generated/compare-models/"),
                 pattern = ".rds", full.names = TRUE)
d <- purrr::map_dfr(f1, readRDS)

d <- d %>% mutate(within_ci = ifelse(lwr_diff > 0 & upr_diff < 0, TRUE, FALSE),
                  species = ifelse(
                    species == "Rougheye/Blackspotted Rockfish Complex",
                               "Rougheye/Blackspotted Complex", species)
                    )

ggplot(d) + geom_boxplot(aes(model_string, prop_diff))

d2 <- filter(d, !within_ci)

ggplot(d, aes(forcats::fct_rev(species), mean_diff, colour = model_string)) +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_point(size = 2) +
  geom_point(data = d2, colour = "black") +
  scale_color_discrete(
    labels = c("Only survey-years with morphological data from >= 6 sets",
               "All surveys with postive catches on >= 3 years & > 1% overall"
               )) +
  labs(x = "",
       y = "Mean proportional deviation in sum of split indices from overall index",
       colour = "",
       title = "Comparison of maturity-specific indices of abundance ",
       subtitle = "Black indicates a sum that exceeded the CI on the overall index."
  ) +
  coord_flip() +
  theme(legend.position = "top",
        plot.title = element_text(hjust=-0.5),
        plot.subtitle = element_text(hjust=-0.5))

ggsave("figs/model-comparison-mean-diff.png", height = 7, width = 9)



ggplot(d, aes(forcats::fct_rev(species), prop_diff, colour = model_string)) +
  geom_point(size = 2) +
  geom_point(data = d2, colour = "black") +
  scale_color_discrete(
    labels = c("Only survey-years with morphological data from >= 6 sets",
               "All surveys with postive catches on >= 3 years & > 1% overall")
    ) +
  labs(x = "",
       y = "Absolute proportional deviation in sum of split indices from overall index",
       colour = "",
       title = "Comparison of maturity-specific indices of abundance ",
       subtitle = "Black indicates a sum that exceeded the CI on the overall index."
  ) +
  coord_flip() +
  theme(legend.position = "top",
        plot.title = element_text(hjust=-0.5),
        plot.subtitle = element_text(hjust=-0.5))

ggsave("figs/model-comparison-abs-diff.png", height = 7, width = 9)
