# test plot assess outputs
library(tidyverse)
library(gratia)
library(patchwork)

dat <- readRDS(paste0("data/all-productivity-2023-03-01.rds"))
# just plot biomass ~ time w catch, P ~ time, P/B ~ time, P/B ~ B
# no lag

unique(dat$group)

# groups <- "Flatfish"
# groups <- "Round"
# groups <- "Rockfish-Shelf"
groups <- "Rockfish-Slope"


dat0 <- filter(dat) %>%
  filter(group %in% groups) %>%
  group_by(species, stock, model_type, recruitment_age) %>%
  mutate(max_biomass = max(biomass, na.rm = TRUE),
         max_production = max(production, na.rm = TRUE),
         model_name = paste0(species, " \n",
                            stock, " \n",
                            "(recruit age = ", recruitment_age, ")"
  )
  ) %>%
  arrange(species, stock, recruitment_age, year)

glimpse(dat0)

# ggplot(dat0) + geom_path(aes(year, production/max_production)) +
#   facet_wrap(~ species + stock, scales = "free_y") +
#   ggsidekick::theme_sleek()
#
# ggplot(dat0) + geom_path(aes(year, p_by_biomass)) +
#   facet_wrap(~ species + stock, scales = "free_x") +
#   ggsidekick::theme_sleek()

(p1 <- ggplot(dat0) +
    geom_path(aes(year, biomass),
              # size = 1,
              alpha = 0.75) +
    geom_col(aes(year, catch), alpha = 0.5) +
    geom_path(aes(year, production), alpha = 0.75,
              # size = 1,
              # lty = "dashed",
              colour = "blue") +
    ylab("Biomass (black), Production (blue) and Catch (bars) \n ") +
    xlab("Year") +
    facet_wrap(~model_name,
               scales = "free_y",
               ncol = 1,
               strip.position = "right") +
    ggsidekick::theme_sleek() +
    theme(strip.text.y = element_blank(),
          # axis.title.x = element_blank(),
          # axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()
    ))

(p2 <- ggplot(dat0) +
    geom_path(aes(year, p_by_biomass)) +
    geom_point(aes(year, p_by_biomass, colour = year)) +
    scale_colour_viridis_c() +
    # geom_path(aes(year, production/max_production), lty = "dotted") +
    # ylab("Production/Biomass (solid line) \n \nProduction (scaled to max, dotted line) \n ") +
    ylab("Production/Biomass") +
    facet_wrap(~ model_name,
               scales = "free_y",
               ncol = 1,  strip.position = "right") + #, scales = "free_x"
    xlab("Year") +
    ggsidekick::theme_sleek() + theme(
      legend.position = "none",
      strip.text.y = element_blank())
)

(p3 <- ggplot(dat0) +
    geom_path(aes(biomass/max_biomass, p_by_biomass)) +
    geom_point(aes(biomass/max_biomass, p_by_biomass, colour = year)) +
    ylab("Production/Biomass") + xlab("Biomass") +
    scale_colour_viridis_c() +
    facet_wrap(~ model_name,
               scales = "free", ncol = 1, strip.position = "right") +
    ggsidekick::theme_sleek()+ theme(
      legend.position = "none",
      strip.text.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank())
)

p1 + p2 + p3 + plot_layout(ncol = 3)

length(unique(dat0$model_name))

ggsave(paste0(groups, "-stock-assess-output-fig-draft.png"),
       width = 8, height = length(unique(dat0$model_name))*1.5)
