# test plot assess outputs
library(tidyverse)
# library(gratia)
library(patchwork)

# dat <- readRDS(paste0("data/all-productivity-2023-03-21.rds"))
# dat <- readRDS(paste0("data/all-productivity-2023-04-14.rds"))

dat <- readRDS(paste0("data/all-productivity-2023-04-20.rds"))


# dat <- readRDS(paste0("data/all-productivity-and-covars-2023-04-14.rds"))
# just plot biomass ~ time w catch, P ~ time, P/B ~ time, P/B ~ B
# no lag

unique(dat$group)

unique(dat$species)



groups <- "Flatfish"
# groups <- "Round"
# groups <- "Rockfish-Shelf"
# groups <- "Rockfish-Slope"
year_range <- "all"
# year_range <- "pre-1985"
# year_range <- "post-1985"


# dat <- readRDS(paste0("data/all-productivity-and-covars-2023-04-20.rds"))
dat <- filter(dat) %>%
  filter(!(species == "Pacific Cod" & recruitment_age == 2)) %>%
  # filter(species != "Walleye Pollock") %>%
  # filter(species == "Pacific Ocean Perch") %>%
  # filter(species %in% c("Canary Rockfish", "Widow Rockfish", "Yellowtail Rockfish")) %>%
  group_by(species, stock, model_type, recruitment_age) %>%
  mutate(max_biomass = max(biomass, na.rm = TRUE),
         max_production = max(production, na.rm = TRUE),
         # recruitment_lag = lag(recruits, 8),
         model_name = paste0(species, " \n", stock)
  ) %>%
  arrange(species, stock, recruitment_age, year)



dat0 <- dat %>% filter(group %in% groups)  %>%
  mutate(model_name = paste0(species, " \n",
                             stock, " \n",
                             "(recruit age = ", recruitment_age, ")"
         )
  )

glimpse(dat0)

# ggplot(dat0) + geom_path(aes(year, production/max_production)) +
#   facet_wrap(~ species + stock, scales = "free_y") +
#   ggsidekick::theme_sleek()
#
# ggplot(dat0) + geom_path(aes(year, p_by_biomass)) +
#   facet_wrap(~ species + stock, scales = "free_x") +
#   ggsidekick::theme_sleek()
if(year_range == "all") {
  dat1 <- dat0
}

if(year_range == "pre-1985") {
dat1 <- dat0 %>%
  filter(year > 1950 & year < 1986)
}

if(year_range == "post-1985") {
  dat1 <- dat0 %>%
    # filter(!(species == "Bocaccio" & year > 2019)) %>%
    filter(year >= 1986)
}




(p1 <- ggplot(dat1) +
    geom_path(aes(year, vbiomass),
              # size = 1,
              alpha = 0.75) +
    geom_path(aes(year, biomass),
              # size = 1,
              linetype = "dotted",
              alpha = 0.75) +
    geom_col(aes(year, catch), alpha = 0.5) +
    geom_path(aes(year, production), alpha = 0.75,
              # size = 1,
              # lty = "dashed",
              colour = "blue") +
    # geom_path(aes(year, production), alpha = 0.75,
    #           # size = 1,
    #           linetype = "dotted",
    #           colour = "blue") +
    geom_path(aes(year, recruits), alpha = 0.75,
              # size = 1,
              # lty = "dashed",
              colour = "red") +
    ylab("Biomass (black), Production (blue), Recruits (red), Catch (bars) \n
         Total or vulnerable = solid, Spawning = dotted") +
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

(p2 <- ggplot(dat1) +
    geom_path(aes(year, p_by_biomass)) +
    geom_point(aes(year, p_by_biomass, colour = year)) +
    # geom_path(aes(year, p_by_biomass), linetype = "dotted") +
    # geom_point(aes(year, vp_by_biomass, colour = year)) +
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

(p3 <- ggplot(dat1) +
    geom_path(aes(biomass/max_biomass, p_by_biomass, colour = year),
              size = 0.75,
              arrow=arrow(angle=30,length=unit(0.1,"inches"),type="open")) +
    # geom_path(aes(biomass/max_biomass, p_by_biomass),
    #           size = 0.25,
    #           linetype = "dotted",
    #           arrow=arrow(angle=30,length=unit(0.1,"inches"),type="open")) +
    # geom_point(aes(biomass/max_biomass, p_by_biomass, colour = year)) +
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

(p4 <- ggplot(dat1) +
    geom_point(aes(biomass_for_recruits,
                   # recruits,
                   (recruits/biomass_for_recruits),
                   colour = year),
              # size = 1,
              alpha = 0.75) +
    geom_hline(yintercept = 1, colour = "grey", linetype = "dashed") +
    scale_colour_viridis_c() +
    ylab("Recruits per unit of spawning biomass (dashed line at 1:1)") +
    xlab("Biomass") +
    facet_wrap(~model_name,
               scales = "free",
               ncol = 1,
               strip.position = "right") +

    # coord_cartesian(expand = FALSE) +
    ggsidekick::theme_sleek() +
    theme(legend.position = "none",
          # axis.title = element_blank(),
          axis.text.x = element_blank(),
          # axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          strip.text.y = element_blank()
    ))

p1 + p4 + p2 + p3 + plot_layout(ncol = 4, widths = c(1,0.5,1,0.5))

length(unique(dat1$model_name))
# groups <- paste0(groups, "-trimmed")
groups2 <- paste0(groups, "-vb")
ggsave(paste0("figs/", groups2, "-stock-assess-output-fig-all.png"),
       width = 12, height = length(unique(dat0$model_name))*1.65)


### all together plots

(p0 <- ggplot(dat) +
    geom_path(aes(year, vbiomass),
              # size = 1,
              alpha = 0.75) +
    geom_path(aes(year, biomass),
              # size = 1,
              linetype = "dotted",
              alpha = 0.75) +
    geom_col(aes(year, catch), alpha = 0.5) +
    geom_path(aes(year, production), alpha = 0.75,
              # size = 1,
              # lty = "dashed",
              colour = "blue") +
    # geom_path(aes(year, production), alpha = 0.75,
    #           # size = 1,
    #           linetype = "dotted",
    #           colour = "blue") +
    geom_path(aes(year, recruits), alpha = 0.75,
              # size = 1,
              # lty = "dashed",
              colour = "red") +
    ylab("Biomass (    ), Production (    ), Recruits (    ), Catch (    bars) \n
Total or vulnerable biomass (        ), Spawning biomass (       )") +
    xlab("Year") +
    facet_wrap(~model_name,
               scales = "free_y",
               nrow = 4,
               # ncol = 1,
               strip.position = "top") +
    ggsidekick::theme_sleek() +
    theme(#strip.text.y = element_blank(),
      # axis.title.x = element_blank(),
      # axis.text.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    ))
length(unique(dat1$model_name))
# groups <- paste0(groups, "-trimmed")
# groups2 <- paste0(groups, "-vb")
ggsave(paste0("figs/all-stock-assess-output-fig-all.png"),
       width = 12, height = 7)




# (p2 <- ggplot(dat1) +
#     geom_path(aes(year, p_by_biomass)) +
#     geom_point(aes(year, p_by_biomass, colour = year)) +
#     # geom_path(aes(year, p_by_biomass), linetype = "dotted") +
#     # geom_point(aes(year, vp_by_biomass, colour = year)) +
#     scale_colour_viridis_c() +
#     # geom_path(aes(year, production/max_production), lty = "dotted") +
#     # ylab("Production/Biomass (solid line) \n \nProduction (scaled to max, dotted line) \n ") +
#     ylab("Production/Biomass") +
#     facet_wrap(~ model_name,
#                scales = "free_y",
#                ncol = 1,  strip.position = "right") + #, scales = "free_x"
#     xlab("Year") +
#     ggsidekick::theme_sleek() + theme(
#       legend.position = "none",
#       strip.text.y = element_blank())
# )

(p3 <-
    dat %>%
    # filter(!(species %in% c("Pacific Cod", "Walleye Pollock", "Bocaccio"))) %>%
    ggplot() +
    geom_rect(aes(xmin = -Inf, xmax = 1, ymin = -0.1, ymax = 0.1), fill = "grey95") +
    geom_rect(aes(xmin = -Inf, xmax = 1, ymin = -0.05, ymax = 0.05), fill = "grey89") +
    # geom_hline(yintercept = 0.1, colour = "grey", linetype = "solid") +
    # geom_hline(yintercept = -0.1, colour = "grey", linetype = "solid") +
    geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
    geom_path(aes(biomass/max_biomass, p_by_biomass, colour = year),
              size = 0.75,
              arrow=arrow(angle=30,length=unit(0.1,"inches"),type="open")) +
    # geom_path(aes(biomass/max_biomass, p_by_biomass),
    #           size = 0.25,
    #           linetype = "dotted",
    #           arrow=arrow(angle=30,length=unit(0.1,"inches"),type="open")) +
    # geom_point(aes(biomass/max_biomass, p_by_biomass, colour = year)) +
    labs(y = "Production rate", x = "Vulnerable biomass (proportion of maximun)", colour = "Year") +
    scale_colour_viridis_c() +
    facet_wrap(~ model_name,
               scales = "free",
               nrow = 3,
               strip.position = "top") +
    # scale_x_continuous(breaks = c(0,0.5, 1)) +
    ggsidekick::theme_sleek() + theme(
      legend.position = c(0.8,0.13),
      # strip.text.x = element_blank(),
      # axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
      )
)

ggsave(paste0("figs/all-stock-assess-production-by-biomass-free-x.png"),
       width = 11.6, height = 5.5)

(p4 <- ggplot(dat) +
    # geom_line(aes(biomass_for_recruits,
    #                                # recruits,
    #                                recruits,
    #                                colour = year),
    #                            # size = 1,
    #                            alpha = 0.75) +
    geom_point(aes(biomass_for_recruits,
                   # recruits,
                   log(recruits/biomass_for_recruits),
                   colour = year),
               # size = 1,
               alpha = 0.75) +
    # geom_hline(yintercept = 1, colour = "grey", linetype = "dashed") +
    scale_colour_viridis_c() +
    labs(y="Recruits per spawner (log scale)",x="Spawning biomass", colour = "Year") + #per unit of spawning biomass
    scale_colour_viridis_c() +
    facet_wrap(~ model_name,
               scales = "free",
               nrow = 4,
               strip.position = "top") +
    ggsidekick::theme_sleek() + theme(
      legend.position = c(0.75,0.12),
      # strip.text.x = element_blank(),
      # axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    ))

ggsave(paste0("figs/all-stock-assess-recruit-by-biomass-fig-all.png"),
       width = 10, height = 7)


dat3 <- readRDS(paste0("data/all-productivity-and-covars-2023-04-21.rds"))

# dat <- filter(dat) %>%
#   filter(species == "Pacific Cod" )

dat3 <- filter(dat3) %>%
  filter(!(species == "Pacific Cod" & recruitment_age == 2)) %>%
  group_by(species, stock, model_type, recruitment_age) %>%
  mutate(max_biomass = max(biomass, na.rm = TRUE),
         max_production = max(production, na.rm = TRUE),
         # recruitment_lag = lag(recruits, 8),
         model_name = paste0(species, " \n", stock)
  ) %>% group_by(species, stock) %>%
  mutate(
    # value_sd = sd(value, na.rm = TRUE),
    p_by_biomass2 = p_by_biomass * 100,
    value_c = Adult_TOB_1to12_roms_max - mean(Adult_TOB_1to12_roms_max, na.rm = TRUE)
    ) %>% arrange(species, stock, recruitment_age, year)



(p2 <- ggplot(dat3) +
    geom_hline(yintercept = 0, colour = "black", linetype = "dashed") +
    geom_path(aes(year, p_by_biomass)) +
    geom_point(aes(year, p_by_biomass, colour = value_c)) +
    scale_colour_viridis_c() +
    labs(y="Production rate", x="Year", colour = "ROMS bottom\ntemperature\ndeviations") + #per unit of spawning biomass
    scale_colour_viridis_c(option = "B") +
    facet_wrap(~ model_name,
               scales = "free",
               nrow = 4,
               strip.position = "top") +
    ggsidekick::theme_sleek() + theme(
      # legend.position = "none",
      legend.position = c(0.75,0.12),
      # strip.text.x = element_blank(),
      # axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    ))

ggsave(paste0("figs/all-stock-assess-production-TOB-deviations.png"),
       width = 12, height = 7)




# (p2 <- ggplot(dat) +
#     geom_path(aes(year, recruits/biomass_for_recruits)) +
#     geom_point(aes(year, recruits/biomass_for_recruits, colour = `Larval_SST_1to12_roms_max_lag1`)) +
#     scale_colour_viridis_c() +
#     labs(y="Recruitment rate", x="Year", colour = "Year") + #per unit of spawning biomass
#     scale_colour_viridis_c(option = "B") +
#     facet_wrap(~ model_name,
#                scales = "free",
#                nrow = 4,
#                strip.position = "top") +
#     ggsidekick::theme_sleek() + theme(
#       # legend.position = "none",
#       legend.position = c(0.75,0.12),
#       # strip.text.x = element_blank(),
#       # axis.title.y = element_blank(),
#       axis.ticks.y = element_blank(),
#       axis.text.y = element_blank()
#     ))
#
# ggsave(paste0("figs/all-stock-assess-recruitment-time-fig-all.png"),
#        width = 12, height = 7)



dat1 <- readRDS(paste0("data/all-productivity-longer-2023-04-21.rds")) %>%
  filter(
      # variable_type == "O2" & agg_type == "min" &
      variable_type == "O2" & agg_type == "min" &
      stage == "Eggs/gestation",
      # stage == "Larval",
      lag == recruitment_age
      )  %>%
  filter(!(species == "Pacific Cod" & recruitment_age == 2)) %>%
  select (species, stock, recruitment_age, year, value) %>%
  group_by(species, stock) %>%
  mutate(
    # value_sd = sd(value, na.rm = TRUE),
    # p_by_biomass2 = p_by_biomass * 100,
    value_c = value - mean(value, na.rm = TRUE)
  )

dat2 <- left_join(dat, dat1)

(p2 <- ggplot(dat2) +
    geom_path(aes(year, recruits/biomass_for_recruits)) +
    geom_point(aes(year, recruits/biomass_for_recruits,
                   colour = value
                   # colour = value_c
                   )) +
    scale_colour_viridis_c() +
    labs(y="Recruitment rate", x="Year",
         colour = "ROMS\nO2 mmol/m3"
         # colour = "ROMS\nO2 mmol/m3\ndeviations"
         ) + #per unit of spawning biomass
    scale_colour_viridis_c(option = "D") +
    facet_wrap(~ model_name,
               scales = "free",
               nrow = 4,
               strip.position = "top") +
    ggsidekick::theme_sleek() + theme(
      # legend.position = "none",
      legend.position = c(0.75,0.1),
      # strip.text.x = element_blank(),
      # axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    ))

ggsave(paste0("figs/all-stock-assess-recruitment-DO-fig-all.png"),
       width = 12, height = 7)

ggsave(paste0("figs/all-stock-assess-recruitment-DO-deviations.png"),
       width = 12, height = 7)
