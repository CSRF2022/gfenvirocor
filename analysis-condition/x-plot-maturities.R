## maturity plots
library(tidyverse)
# library(sdmTMB)
library(ggsidekick)
library(aplot)
# devtools::load_all(".")
theme_set(ggsidekick:::theme_sleek())

# fig_height <- 4 * 2
# fig_width <- 5 * 2


f <- list.files(paste0("data-generated/maturity-ogives/"), pattern = ".rds", full.names = TRUE)

m <- purrr::map(f, readRDS)

p <- list()

for (i in seq_along(m)){
p[[i]] <- gfplot::plot_mat_ogive(m[[i]]) +
  ggtitle(paste0(m[[i]]$data$species_common_name, " (",
                length(unique(m[[i]]$data$specimen_id)),
                " fish, ", length(unique(m[[i]]$data$sample_id)),
                " tows)"
                )) #+
  # theme(axis.title = element_blank())
}
# p
# p[[1]]

(g <- plot_list(gglist = p, ncol = 3)+ patchwork::plot_layout(guides = "collect"))

ggsave("figs/all-maturities.png", height = 12, width = 14)

# gridExtra::grid.arrange(g, left = "Probablity Mature", bottom = "Length (cm)")
