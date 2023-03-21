# Map environmental layers
library(tidyverse)

# grid <- readRDS("data/grid_SST_3to8_roms_max.rds")
# grid <- readRDS("data/grid_SST_3to8_roms_mean.rds")

# glimpse(grid)
# max(grid[, ncol(grid)], na.rm = TRUE)
# min(grid[, ncol(grid)], na.rm = TRUE)

plot_enviro_layers <- function(
  grid,
  var_type = "SST"
){
var <-colnames(grid)[ncol(grid)]

grid %>%
  filter(!is.na(!!sym(var))) %>%
  # filter(year %in% c(1985, 1990, 1995, 2000, 2005, 2010, 2015)) %>%
  ggplot() + geom_tile(aes(X, Y, colour = !!sym(var))) +
  scale_colour_viridis_c(name = var_type) +
  facet_wrap(~year) +
  ggtitle(var) +
  gfplot::theme_pbs() +
  theme(axis.title = element_blank(), axis.text = element_blank())
}

plot_enviro_layers(readRDS("data/grid_SST_ann_roms_max.rds"), "SST")
plot_enviro_layers(readRDS("data/grid_SST_ann_roms_min.rds"), "SST")

# plot_enviro_layers(readRDS("data/grid_SST_3to8_roms_max.rds"), "SST")
# plot_enviro_layers(readRDS("data/grid_SST_3to8_roms_mean.rds"), "SST")

