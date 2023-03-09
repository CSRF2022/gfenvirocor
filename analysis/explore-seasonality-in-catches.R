# explore seasonality in commercial catch of rockfish

library(tidyverse)
library(lubridate)
# library(gfdata)


species <- "yellowtail"
# d <- get_catch("yellowtail rockfish") # run march 6th 2023
# saveRDS(d, "data/yellowtail-get-catch.rds")
d <- readRDS("data/yellowtail-get-catch.rds")


# species <- "silvergray"
# # d <- get_catch("silvergray rockfish") # run march 6th 2023
# # saveRDS(d, "data/silvergray-get-catch.rds")
# d <- readRDS("data/silvergray-get-catch.rds")


# species <- "pop"
# # d <- get_catch("pacific ocean perch") # run march 6th 2023
# # saveRDS(d, "data/pop-get-catch.rds")
# d <- readRDS("data/pop-get-catch.rds")

d <- d %>% mutate(month = lubridate::month(best_date))

glimpse(d)

ggplot(d) + geom_histogram(aes(month))

unique(d$fishery_sector)

d2 <- d %>% group_by(year, month) %>%
  summarise(total_catch = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE),
            n = n()
            )


rainbow1 <- rainbow(14)
rainbow2 <- c(rainbow1[10:14],rainbow1[1:4],rainbow1[7:9])

d2 %>% filter(year > 1986 & year < 2023) %>%
ggplot() + geom_col(aes(as.factor(month), total_catch, fill = as.factor(month))) +
  scale_fill_manual(values = rainbow2)  +
  facet_wrap(~year) + gfplot::theme_pbs()

ggsave(paste0("figs/", species,"-catch-seasonality.png"))

d2 %>% filter(year > 1986 & year < 2023) %>%
  ggplot() + geom_col(aes(as.factor(month), total_catch, fill = as.factor(month)), alpha = 0.8) +
  geom_col(aes(as.factor(month), n*200), alpha = 0.3) +
  scale_fill_manual(values = rainbow2)  +
  facet_wrap(~year) + gfplot::theme_pbs()

ggsave(paste0("figs/", species,"-catch-w-trips-seasonality.png"))


d3 <- d %>% group_by(year, month, fishery_sector) %>%
  summarise(total_catch = sum(landed_kg, na.rm = TRUE) + sum(discarded_kg, na.rm = TRUE))

d3 %>% filter(year > 1986 & year < 2023) %>%
  filter(fishery_sector == "GROUNDFISH TRAWL") %>%
  ggplot() + geom_col(aes(as.factor(month), total_catch, fill = as.factor(month))) +
  scale_fill_manual(values = rainbow2)  +
  facet_wrap(~year) + gfplot::theme_pbs()

ggsave(paste0("figs/", species,"-catch-seasonality-just-trawl.png"))
