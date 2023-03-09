# investigate maturity data for yellowtail because mating and gestiation time and depth not in literature
library(reldist)
library(tidyverse)
library(patchwork)
options(scipen=999)

species <- "yellowtail"

readRDS("data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 1) # ROCKFISH
readRDS("data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 11) # PORTSAMPLES
readRDS("data/maturities.rds") %>% filter(MATURITY_CONVENTION_CODE == 13) #AMR

# d <- readRDS("../../gfsynopsis/report/data-cache/yellowtail-rockfish.rds")
# dat <- d$commercial_samples
dat <- readRDS("data/yellowtail-comm-samples.rds")
dat2 <- readRDS("../../gfsynopsis/report/data-cache/yellowtail-rockfish.rds")

dat2$survey_samples %>% View()

# range(dat2$survey_sets$catch_weight)
# range(dat$catch_weight, na.rm = T)
#
# dat2$survey_samples$fishing_event_id
# # glimpse(dat)
# # glimpse(dat2$survey_samples)
# #
# # unique(dat2$survey_samples$maturity_code)
# unique(dat2$survey_samples$maturity_name)
#
# unique(dat2$survey_samples$catch_weight)
#
# dat2$survey_samples %>% filter(maturity_code %in% c(9, 37)) %>% View()
# dat2$survey_samples %>% filter(month %in% c(7)) %>% View()

cw <- dat2$survey_sets %>% select(fishing_event_id, catch_weight, depth_m) %>%
  rename(best_depth = depth_m)

dat3 <- left_join(dat2$survey_samples, cw, multiple = "all")

dat <- bind_rows(dat, dat3)



d <- dat %>%
  filter(year > 1995) %>%
  mutate(Maturity = case_when(maturity_code %in% c(NA, 0, 9, 37) ~ NA,
                              maturity_code %in% c(1, 2) ~ "1/2 = immature",
                              # maturity_code == 2 ~ "2 = maturing",
                              maturity_code == 3 ~ "3/4M = mature",
                              maturity_code == 4 & sex == 1 ~ "3/4M = mature",
                              maturity_code %in% c(4, 5) & sex == 2 ~ "4F/5F = fertilized/embryos",
                              maturity_code == 6 ~ "6 = spent",
                              maturity_code == 7 ~ "7 = resting",
  )) %>%
  filter(!is.na(catch_weight)) %>%
  filter(best_depth > 0)

dat %>% filter(!is.na(maturity_code)) %>% group_by(year, maturity_code) %>% summarise(n = n()) %>% View()
d %>% filter(!is.na(Maturity)) %>% group_by(year, maturity_code) %>% summarise(n = n()) %>% View()


d %>%
  filter(!is.na(Maturity)) %>%
  group_by(sex, Maturity) %>%
  summarise(n = n(),
            mean_depth = weighted.mean(best_depth, catch_weight, na.rm = TRUE),
            depth_25 = wtd.quantile(best_depth, 0.25, na.rm = TRUE, weight = catch_weight),
            depth_75 = wtd.quantile(best_depth, 0.75, na.rm = TRUE, weight = catch_weight),
            mean_month = weighted.mean(month, catch_weight, na.rm = TRUE),
            month_25 = wtd.quantile(month, 0.25, na.rm = TRUE, weight = catch_weight),
            month_75 = wtd.quantile(month, 0.75, na.rm = TRUE, weight = catch_weight)
            )

d_all <- readRDS("data/yellowtail-get-catch.rds") %>%
  filter(year > 1995) %>%
  mutate(month = lubridate::month(best_date),
         total_catch = landed_kg + discarded_kg)

plot_maturity_by_month <- function(
  mat_data,
  all_catch,
  which_sex = c("Male", "Female"),
  months = c(1:12),
  maturity_codes = c(1, 2, 3, 4, 5, 6,7),
  scale_weights = 10000,
  ylimits = NULL,
  bin_width = 25
  ){


  d1 <- mat_data %>%
    filter(month %in% months) %>%
    filter(sex %in% c(1,2))

  d1$sex <- factor(d1$sex, labels = c("Male", "Female"))


  d1 %>% filter(maturity_code %in% maturity_codes) %>%
    filter(sex %in% which_sex) %>%
    ggplot() +
    geom_histogram(data = d1 %>% filter(sex %in% which_sex),
                   aes(best_depth,
                       weight = catch_weight/scale_weights
                   ), binwidth = bin_width,
                   alpha = 0.25
    ) +
    geom_histogram(aes(best_depth,
                       weight = catch_weight/scale_weights,
                       fill = Maturity),
                   binwidth = bin_width,
                   alpha = 0.9
    ) +
    geom_density(
      data = d_all %>% filter(month %in% months),
      aes(best_depth, after_stat(count), weight = total_catch)
    ) +
    facet_grid(month~sex, switch = "y") +
    coord_cartesian(xlim = c(0, quantile(mat_data$best_depth, 0.995)),
                    ylim = ylimits,
                    expand = FALSE) +
    scale_fill_viridis_d() +
    labs(y = "Weighted commercial catch (solid line) and sample counts (bars)",
         x = "Depth (m)") +
    gfplot::theme_pbs() + theme(axis.ticks.y = element_blank(),
                                axis.text.y = element_blank())
}


# default test
plot_maturity_by_month(d, d_all)
ggsave("figs/", species, "-maturity-by-month-depth-long.png", width = 8, height = 14)

# # split by month sequence but feels a bit akward
# p1 <- plot_maturity_by_month(d, d_all, months = c(1:6))
# p2 <- plot_maturity_by_month(d, d_all, months = c(7:12), ylimits = layer_scales(p1)$y$range$range) + theme(axis.title.y = element_blank())
#
# p1 + p2 + plot_layout(guides = "collect")
#
# ggsave("figs/", species, "-maturity-by-month-depth.png", width = 8, height = 14)


# split by sex first
p1 <- plot_maturity_by_month(d, d_all, which_sex = c("Male")) + theme(legend.position = "none") +
  facet_wrap(~month, ncol = 2, strip.position = "left",dir="v") + ggtitle("A. Male")
p2 <- plot_maturity_by_month(d, d_all, which_sex = c("Female"), ylimits = layer_scales(p1)$y$range$range) +
  facet_wrap(~month, ncol = 2, strip.position = "left",dir="v")  + ggtitle("B. Female")+
  theme(axis.title.y = element_blank())

p1 + p2 + plot_layout()

ggsave("figs/", species, "-maturity-by-month-depth.png", width = 10, height = 7)


