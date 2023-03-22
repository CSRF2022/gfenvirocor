# Map environmental layers
library(tidyverse)

# grid <- readRDS("data/grid_SST_3to8_roms_max.rds")
# grid <- readRDS("data/grid_SST_3to8_roms_mean.rds")

# glimpse(grid)
# max(grid[, ncol(grid)], na.rm = TRUE)
# min(grid[, ncol(grid)], na.rm = TRUE)

plot_enviro_layers <- function(
  grid,
  var_type = "SST",
  viridis_option = "B",
  start_year = 1981,
  end_year = 2018
){
var <- colnames(grid)[ncol(grid)]

grid$year <- as.numeric(grid$year)
# browser()
grid %>%
  filter(!is.na(!!sym(var))) %>%
  filter(year >= start_year) %>%
  filter(year <= end_year) %>%
  # filter(year %in% c(1985, 1990, 1995, 2000, 2005, 2010, 2015)) %>%
  ggplot() + geom_tile(aes(X, Y, colour = !!sym(var))) +
  scale_colour_viridis_c(name = var_type, option = viridis_option) +
  facet_wrap(~year, ncol = 10) +
  ggtitle(var) +
  gfplot::theme_pbs() +
  theme(axis.title = element_blank(), axis.text = element_blank())
}

plot_enviro_layers(readRDS("data/grid_SST_ann_roms_max.rds"), "Max\nSST")
ggsave("figs/map_SST_ann_roms_max.png", width = 12, height = 6)

plot_enviro_layers(readRDS("data/grid_SST_ann_roms_min.rds"), "Min\nSST")
ggsave("figs/map_SST_ann_roms_min.png", width = 12, height = 6)


plot_enviro_layers(readRDS("data/grid_tob_ann_roms_max.rds"), "Max\nbottom\ntemp")
ggsave("figs/map_tob_ann_roms_max.png", width = 12, height = 6)

plot_enviro_layers(readRDS("data/grid_tob_ann_roms_min.rds"), "Min\nbottom\ntemp")
ggsave("figs/map_tob_ann_roms_min.png", width = 12, height = 6)


plot_enviro_layers(readRDS("data/grid_O2_ann_roms_max.rds"), "Max\noxygen", "D")
plot_enviro_layers(readRDS("data/grid_O2_ann_roms_min.rds"), "Min\noxygen", "D")




plot_enviro_layers(readRDS("data/grid_O2_1to2_roms_max.rds"),
                   "Max\nwinter\noxygen", "D")
plot_enviro_layers(readRDS("data/grid_O2_1to2_roms_min.rds"),
                   "Min\nwinter\noxygen", "D")
ggsave("figs/map_O2_1to2_roms_min.png", width = 12, height = 6)

# plot_enviro_layers(readRDS("data/grid_SST_3to8_roms_max.rds"), "SST")
# plot_enviro_layers(readRDS("data/grid_SST_3to8_roms_mean.rds"), "SST")

plot_enviro_layers(readRDS("data/grid_tob_ann_bcc_max.rds"), "Max\nbottom\ntemp",
                   start_year = 1950,
                   end_year = 1995)
ggsave("figs/map_tob_ann_bcc_max.png", width = 12, height = 6)
