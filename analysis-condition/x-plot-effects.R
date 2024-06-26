
## effect plots
library(tidyverse)
library(sdmTMB)
library(ggsidekick)
library(aplot)
library(patchwork)
# devtools::load_all(".")
theme_set(ggsidekick:::theme_sleek())


source("analysis-condition/00-species-list.R")

# fig_height <- 4 * 2
# fig_width <- 5 * 2
variable <- "days_to_solstice"
# variable <- "log_density_c"
# xlabel <- "Local log density of maturity class (centered on mean)"

# model_name <- "apr-2024" # might not need this because density removed from ones lacking a negative effect.
model_name <- "apr-2024-density"

group_tag <- "mat-m"
# group_tag <- "mat-fem"
# group_tag <- "imm"

f <- list.files(paste0("data-generated/condition-models-", group_tag, "/", model_name, "/"),
                pattern = ".rds", full.names = TRUE)

m <- purrr::map(f, readRDS)



p <- list()
p2 <- list() #for filtered version

for (i in seq_along(m)){

  if (variable == "log_density_c") {
    nd <- data.frame(
      days_to_solstice = 0,
      survey_group = "TRAWL",
      log_density_c = seq(min(m[[i]]$data$log_density_c),
                          max(m[[i]]$data$log_density_c),
                          length.out = 50
      ),
      ann_log_density_c = 0,
      dens_dev = 0,
      year = max(m[[i]]$data$year) # a chosen year
    )
    xlabel <- "Local log density of maturity class (centered on mean)"
  }

  if(variable == "days_to_solstice") {
    nd <- data.frame(
      days_to_solstice = seq(min(m[[i]]$data$days_to_solstice),
                             max(m[[i]]$data$days_to_solstice),
                             length.out = 50
      ),
      survey_group = "TRAWL",
      log_density_c = 0,
      ann_log_density_c = 0,
      dens_dev = 0,
      year = max(m[[i]]$data$year) # a chosen year
    )
    xlabel <- "Days from solstice"
  }

pd <- predict(m[[i]], newdata = nd, se_fit = TRUE, re_form = NA)

t <- tidy(m[[i]], conf.int = TRUE)

set_alpha <- 0.8

p[[i]] <- ggplot(pd, aes(.data[[variable]], exp(est),
               ymin = exp(est - 1.96 * est_se),
               ymax = exp(est + 1.96 * est_se)
)) +
  geom_rug(data = m[[i]]$data, aes(.data[[variable]], y = 0.9),
             sides = "b", alpha = 0.1, inherit.aes = FALSE) +
  geom_line(alpha = set_alpha) +
  geom_ribbon(alpha = set_alpha/2) +
  scale_x_continuous() +
  # coord_cartesian(expand = F, ylim = c(NA, max(exp(pd$est))+0.01)) +
  coord_cartesian(expand = F, ylim = c(NA, 1.2)) +
  labs(x = xlabel,
       y = "condition") +
theme(axis.title = element_blank())

if (variable == "log_density_c") {

  if(t$estimate[t$term == "log_density_c"] < -t$std.error[t$term == "log_density_c"]){
    set_alpha <- 1
  } else {
    set_alpha <- 0.4
  }

  pd$density <- exp(pd$log_density_c + mean(m[[i]]$data$log_density, na.rm = TRUE))

  # p[[i]] <- ggplot(pd, aes(log_density_c, exp(est))) +
  #   geom_rug(data = m[[i]]$data, aes(log_density_c, y = 0.95),
  #          sides = "b", alpha = 0.1, inherit.aes = FALSE) +
  p[[i]] <- ggplot(pd, aes(density, exp(est))) +
    geom_rug(data = m[[i]]$data, aes(density, y = 0.95),
             sides = "b", alpha = 0.1, inherit.aes = FALSE) +
    geom_line(alpha = set_alpha) +
    geom_ribbon(aes(ymin = exp(est - 1.96 * est_se),
                    ymax = exp(est + 1.96 * est_se)
    ), alpha = set_alpha/2) +
    scale_x_continuous() +
    # geom_vline(xintercept = exp(mean(m[[i]]$data$log_density_c, na.rm = TRUE)), linetype = "dashed") +
    # ggridges::geom_density_ridges(data = m[[i]]$data,
    #    aes(log_density_c, y = 0.95
    #    height = after_stat(count)),
    #    rel_min_height = 0.01, scale = 0.25, alpha = 0) +
    # coord_cartesian(ylim = c(0.95, 1.15),  xlim = c(0, NA), expand = F) +
    coord_cartesian(expand = F) +
    labs(x = xlabel,
         y = "condition") +
    theme(axis.title = element_blank())



  xlabel <- "Local biomass density of maturity class (slopes are in log space)"

p[[i]] <- p[[i]] + ggtitle(paste0(stringr::str_to_title(m[[i]]$data$species_common_name),
               " (slope: ", round(t$estimate[t$term == variable], 3),
               ", SE: ", round(t$std.error[t$term == variable], 3), ")"
))


} else {
  p[[i]] <- p[[i]] + ggtitle(paste0(stringr::str_to_title(m[[i]]$data$species_common_name)))
}

p2[[i]] <- p[[i]]
if(m[[i]]$data$species_common_name[1] %in% tolower(c(species_to_remove))) { p2[[i]] <- NULL }

}

p2 <- p2 %>% discard(is.null)


if(group_tag == "mat-m") {group_label <- "Mature male"}
if(group_tag == "mat-fem") {group_label <- "Mature female"}
if(group_tag == "imm") {group_label <- "Immature"}

y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 5,
           label = paste0(group_label, " condition factor"), angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()

x_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 5,
           label = paste("", xlabel)) +
  coord_cartesian(clip = "off")+
  theme_void()

# length(p)/3

design = "
AAAAAA
#BBBBB
"

if(variable == "log_density_c") {set_font <- 9} else{
  set_font <- 12
}


(g <- ((y_lab_big |
          wrap_plots(gglist = p, ncol = 6) &
          theme(text = element_text(size = set_font))) +
          plot_layout(widths = c(0.05, 1)))
  /x_lab_big + plot_layout(heights = c(1,0.05), design = design)
  )

ggsave(paste0("figs/cond-effects-", variable, "-trans-",
              model_name, "-", group_tag, ".png"),
       height = 14, width = 22)

(g2 <- ((y_lab_big |
          wrap_plots(gglist = p2, ncol = 4) &
          theme(text = element_text(size = set_font))) +
         plot_layout(widths = c(0.05, 1)))
  /x_lab_big + plot_layout(heights = c(1,0.05), design = design)
)


ggsave(paste0("figs/cond-effects-", variable, "-trans-",
              model_name, "-", group_tag, "-filtered.png"),
       height = 18, width = 14)



## would work if layout stayed the same
# p_axis <- ggplot() + labs(x = xlabel, y = "Condition factor")
# x_lab_big <- cowplot::get_plot_component(p_axis, "xlab-b")
# y_lab_big <- cowplot::get_plot_component(p_axis, "ylab-l")
# design = "
# FAB
# FCD
# #EE
# "
#
# c(gglist = p, list(x_lab_big, y_lab_big)) |>
#   wrap_plots() +
#   plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25), design = design)

# (g <- (y_lab_big | wrap_plots(gglist = p, ncol = 2)/x_lab_big ) +
#     plot_layout(widths = c(0.1, 1), heights = c(1,0.1)))

