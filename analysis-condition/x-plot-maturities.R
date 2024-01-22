## maturity plots
library(tidyverse)
# library(sdmTMB)
library(ggsidekick)
library(aplot)
library(patchwork)
# devtools::load_all(".")
theme_set(ggsidekick:::theme_sleek())

f <- list.files(paste0("data-generated/maturity-ogives/"), pattern = ".rds", full.names = TRUE)

m <- purrr::map(f, readRDS)

p <- list()

for (i in seq_along(m)){

  m[[i]]$data$species_common_name <- ifelse(m[[i]]$data$species_common_name == "rougheye/blackspotted rockfish complex", "Rougheye/Blackspotted", m[[i]]$data$species_common_name)

p[[i]] <- gfplot::plot_mat_ogive(m[[i]]) +
  ggtitle(paste0(toupper(m[[i]]$data$species_common_name), " (",
                length(unique(m[[i]]$data$specimen_id)),
                " fish, ", length(unique(m[[i]]$data$sample_id)),
                " tows)"
                )) +
  theme(axis.title = element_blank())
}
# p
# p[[1]]

# (g <- plot_list(gglist = p, ncol = 5)+ patchwork::plot_layout(guides = "collect"))

y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 5,
           label = "Probability Mature", angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()

x_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 5,
           label = "Length (cm)") +
  coord_cartesian(clip = "off")+
  theme_void()

design = "
AAAAAA
#BBBBB
"

(g <- ((y_lab_big |
          wrap_plots(gglist = p, ncol = 5) &
          theme(text = element_text(size = 9),
                # plot.title = element_blank(),
                axis.title = element_blank())) +
         plot_layout(widths = c(0.05, 1)))
  /x_lab_big + plot_layout(heights = c(1,0.05), design = design,guides = "collect")
)

ggsave("figs/all-maturities.png", height = 11, width = 18)

# gridExtra::grid.arrange(g, left = "Probablity Mature", bottom = "Length (cm)")


## while the maturity data is loaded, might as well produce all the Le Cren's plots together

dat <- readRDS("data-raw/survey-samples-flatfish.rds") %>%
  bind_rows(., readRDS("data-raw/survey-samples-part2.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-samples-part3.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-samples-shortspine.rds")) %>%
  bind_rows(., readRDS("data-raw/survey-samples-rougheye.rds")) %>%
  mutate(
    survey_abbrev = ifelse(
      survey_abbrev %in% c("MSSM", "MSSM QCS", "MSSM WCVI") & latitude < 50,
      "MSSM WCVI", ifelse(
        survey_abbrev %in% c("MSSM", "MSSM QCS", "MSSM WCVI") & latitude > 50,
        "MSSM QCS", survey_abbrev
      )
    ),
    survey_group = ifelse(
      survey_abbrev %in% c("HBLL OUT N", "HBLL OUT S"),
      "HBLL", ifelse(
        survey_abbrev %in% c("OTHER", "HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI"),
        "TRAWL", ifelse(
          survey_abbrev %in% c("MSSM QCS", "MSSM WCVI"), "MSSM", survey_abbrev
        )
      )
    )
  ) %>%
  distinct()

mat_threshold <- 0.5
f1 <- list.files(paste0("data-generated/condition-data-black-swan/"), pattern = ".rds", full.names = TRUE)

d1 <- purrr::map_dfr(f1, readRDS)

p2 <- list()

for (i in seq_along(m)){

  min_LC <- min(d1$cond_fac, na.rm = TRUE)
  max_LC <- max(d1$cond_fac, na.rm = TRUE)

  .ds <- d1 |> filter(species_common_name == m[[i]]$data$species_common_name[1])|>
    mutate(sex_label = as.factor(ifelse(sex == 1, "Male", ifelse(sex == 2, "Female", "Unknown"))))

  p2[[i]] <- dat %>% filter(species_common_name == m[[i]]$data$species_common_name[1]) |>
    mutate(weight = weight/1000,
           sex_label = as.factor(ifelse(sex == 1, "Male", ifelse(sex == 2, "Female", "Unknown")))
           ) |>
    filter(
     sex %in% c(1,2,3), # are ones that couldn't tell--hopefully only tiny ones?
     weight < 50) |>
   ggplot(aes(length, weight, shape = sex_label)) +
     geom_point(colour = "red") +
     geom_point(colour = "white", data = .ds) +
     geom_point(aes(colour = cond_fac), data = .ds, alpha = 0.4) +
     geom_vline(xintercept = m[[i]]$mat_perc$f.p0.5, col = "#fde725") +
     geom_vline(xintercept = m[[i]]$mat_perc$mean$f.mean.p0.5, col = "#fde725") +
     geom_vline(xintercept = m[[i]]$mat_perc$m.p0.5, col = "#440154", alpha = 0.5) +
     geom_vline(xintercept = m[[i]]$mat_perc$mean$m.mean.p0.5, col = "#440154", alpha = 0.5) +
     labs(
       colour = "Le Cren's",
       shape = "Sex",
       x = "Length (cm)", y = "Weight (kg)") +
     scale_colour_viridis_c(limits= c(min_LC, max_LC)) +
     ggtitle(paste0(toupper(m[[i]]$data$species_common_name[1]))) +
     ggsidekick::theme_sleek() + theme(legend.position = c(0.2,0.8))
}

y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 5,
           label = "Weight (kg)", angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()

x_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 5,
           label = "Length (cm)") +
  coord_cartesian(clip = "off")+
  theme_void()


design = "
AAAAAA
#BBBBB
"

(g <- ((y_lab_big |
          wrap_plots(gglist = p2, ncol = 5) &
          theme(text = element_text(size = 9),
                # plot.title = element_blank(),
                axis.title = element_blank())) +
         plot_layout(widths = c(0.05, 1)))
  /x_lab_big + plot_layout(heights = c(1,0.05), design = design,guides = "collect")
)


# (g <- plot_list(gglist = p2, ncol = 3)+ patchwork::plot_layout())

ggsave("figs/all-Le-Crens.png", height = 12, width = 16)
