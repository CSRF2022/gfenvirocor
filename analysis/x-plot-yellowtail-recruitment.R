# compare yellowtail recruitment estimates and deviations from the 2014 and 2024 assessments
# recruitment uses the file made by 02b-format-tesa-assess.R
devtools::load_all(".")
library(tidyverse)

r <- read_csv("TESA_assess_data/outputs/pacific_groundfish/data/major_stock.csv",
  name_repair = "minimal", show_col_types = FALSE
) %>%
  select(assessment_id, year, total_recruits, total_recruits_units,
    lower = total_recruits_uncertainty_lower,
    upper = total_recruits_uncertainty_upper
  ) %>%
  filter(assessment_id %in% c("YTR-CST-2014", "YTR-CST-2024"), !is.na(total_recruits)) %>%
  mutate(
    assessment = if_else(assessment_id == "YTR-CST-2014",
      "2014 assessment (Awatea, age 1)", "2024 assessment (SS3, age 0)"
    ),
    # millions of fish
    across(c(total_recruits, lower, upper), ~ .x / 1000)
  )

cols <- c(
  "2014 assessment (Awatea, age 1)" = "#2a78d6",
  "2024 assessment (SS3, age 0)" = "#eb6834"
)

# the 2014 estimates are recruits at age 1 and the 2024 estimates are at age 0,
# so the same cohort appears one year apart
panels <- c("As reported in each assessment", "Aligned by birth year")

r2 <- bind_rows(
  mutate(r, panel = panels[1]),
  mutate(r, panel = panels[2], year = if_else(assessment_id == "YTR-CST-2014", year - 1, year))
) %>%
  mutate(panel = factor(panel, levels = panels))

ggplot(r2, aes(year, total_recruits, colour = assessment, fill = assessment)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~panel, ncol = 1) +
  scale_colour_manual(values = cols, name = NULL) +
  scale_fill_manual(values = cols, name = NULL) +
  scale_x_continuous(breaks = seq(1940, 2020, 20), expand = expansion(mult = 0.01)) +
  scale_y_log10(labels = scales::label_comma(drop0trailing = TRUE)) +
  labs(
    title = "Yellowtail Rockfish recruitment estimates",
    subtitle = "Posterior medians with 95% credible intervals; log scale",
    x = NULL, y = "Recruits (millions of fish)"
  ) +
  ggsidekick::theme_sleek() +
  theme(legend.position = "top", legend.justification = "left")

ggsave("figs/yellowtail-recruitment-2014-vs-2024.png", width = 8, height = 7.5, dpi = 200)


# recruitment deviations --------------------------------------------------

assess_dir <- "../Assess_outputs/rowan/"

rdev <- bind_rows(
  summarise_mcmc(paste0(
    assess_dir, "YTR-CST-2014-Awatea-output-forRobyn-230124/",
    "YTR-CST-2014-MCMC(Rdev)-forRobyn.csv"
  )) %>% mutate(assessment = names(cols)[1], birth_year = year - 1),
  summarise_mcmc(paste0(
    assess_dir, "YTR-BC-2024-forPhilina-(261109.v1)/",
    "YTR-BC-2024-MCMC(Rdev)-forPhilina.csv"
  )) %>% mutate(assessment = names(cols)[2], birth_year = year)
)

rdev2 <- bind_rows(
  mutate(rdev, panel = panels[1]),
  mutate(rdev, panel = panels[2], year = birth_year)
) %>%
  mutate(panel = factor(panel, levels = panels))

ggplot(rdev2, aes(year, value, colour = assessment, fill = assessment)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, colour = NA) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~panel, ncol = 1) +
  scale_colour_manual(values = cols, name = NULL) +
  scale_fill_manual(values = cols, name = NULL) +
  scale_x_continuous(breaks = seq(1940, 2020, 20), expand = expansion(mult = 0.01)) +
  labs(
    title = "Yellowtail Rockfish recruitment deviations",
    subtitle = "Posterior medians with 95% credible intervals; log space",
    x = NULL, y = "Recruitment deviation"
  ) +
  ggsidekick::theme_sleek() +
  theme(legend.position = "top", legend.justification = "left")

ggsave("figs/yellowtail-rdev-2014-vs-2024.png", width = 8, height = 7.5, dpi = 200)
