# plots combining species
library(tidyverse)
library(sdmTMB)
library(ggsidekick)
devtools::load_all(".")
theme_set(ggsidekick:::theme_sleek())

fig_height <- 4 * 2
fig_width <- 5 * 2


species_list <- list(
  "Arrowtooth Flounder", #
  "Petrale Sole", #
  "English Sole", #
  "Dover Sole", #
  "Rex Sole",#
  "Flathead Sole", #
  "Southern Rock Sole", # ,
  "Curlfin Sole", #
  "Sand Sole",#
  "Slender Sole",#
  "Pacific Sanddab",#
  "Pacific Halibut",
  "Butter Sole"#
  ## "Starry Flounder"# too few males!
  ## "C-O Sole", # way too few!
  ## "Deepsea Sole" # no maturity
)


index_list <- expand.grid(species = species_list, maturity = c("mat", "imm"), males = c(TRUE, FALSE)) %>%
  mutate(
    females = ifelse(males == FALSE & maturity == "mat", TRUE, FALSE),
    males = ifelse(maturity == "imm", FALSE, males)
  ) %>%
  distinct()


combine_indices <- function(species, maturity, males, females,
                            model_type = "density",
                            file_prefix = "data-generated/density-index/i-",
                            model_string = "-dg-st2000-mssm-03-500m-xt-offset-15-km"
                            ) {
# browser()

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

mat_class <- maturity
just_males <- males
just_females <- females


if (mat_class == "mat") {
  if (just_males) {
      group_tag <- "mat-m"
      group_label <- "mature males"
  } else {
    if (just_females) {
        group_tag <- "mat-fem"
        group_label <- "mature females"
    } else {
        group_tag <- "mat"
        group_label <- "mature (females and males)"
    }
  }
} else {
  if (mat_class == "imm") {
      group_tag <- "imm"
      group_label <- "immatures"
  } else {
    group_label <- group_tag <- "total"
  }
}

if(model_type == "density") {
f <- paste0(file_prefix, spp, "-", group_tag, "-", model_string, ".rds")
} else {
f <- paste0(file_prefix, group_tag, "-", spp, "-", model_string, ".rds")
}

if(file.exists(f)) {
  i <- readRDS(f)
i$species <- species
i$group <- group_label
i$model_string <- model_string
return(i)
}
return(NULL)
}


# density model
model_string <- "-dg-st2000-mssm-03-500m-xt-offset-15-km"
d <- purrr::pmap_dfr(index_list, combine_indices,
                    model_type = "density",
                    file_prefix = "data-generated/density-index/i-",
                    model_string = "dg-st2000-mssm-03-500m-xt-offset-15-km",
                    .id = "model")

# saveRDS(d, "data-generated/all-density-indices.rds")

# model_string <- "-dg-st2000-mssm-03-500m-xt-offset-15-km"
# f <- list.files(paste0("data-generated/density-index/i-",
#                        model_string), pattern = ".rds", full.names = TRUE)
#
# d2 <- purrr::map_dfr(f, readRDS)




ggplot(d, aes(year, est, fill = species)) +
  geom_line(aes(colour = species)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
  facet_wrap(~group) +
  xlab("Year") +
  ylab("Biomass index")

d |> mutate(
  # group = forcats::fct_relevel(group, "immatures", "mature males", "mature females"),

  group = factor(group, levels = c( "mature females","mature males", "immatures"),
                 labels = c("Mature females", "Mature males", "Immatures" ))
  ) |>
ggplot(aes(year, est/1000, fill = group)) +
  geom_line(aes(colour = group)) +
  geom_ribbon(aes(ymin = lwr/1000,
                  ymax = upr/1000, alpha = group)) +
  facet_wrap(~species, scales = "free_y", ncol = 5) +
  scale_alpha_discrete(range = c(0.5, 0.3)) +
  scale_color_viridis_d(
    # end = 0.99,
    direction =-1,
    option = "D") +
  scale_fill_viridis_d(option = "D",
                       # end = 0.99,
                       direction =-1) +
  # scale_color_viridis_d(option = "C", direction =-1, end = 0.9) +
  # scale_fill_viridis_d(option = "C", direction =-1, end = 0.9) +
  xlab("Year") +
  ylab("Relative biomass")


ggsave(paste0("figs/all-density-indices-fixed",
              model_string, ".png"),
       # height = fig_height*.5, width = fig_width*1.1
       height = fig_height*.65, width = fig_width*1.1
)




# condition model ----
# model_name <- "all-st2002-doy-d0c"
# model_name <- "all-st2002-doy"

# d2 <- purrr::pmap_dfr(index_list, combine_indices,
#                      model_type = "condition",
#                      file_prefix = paste0("data-generated/cond-index/",
#                                           model_name,"/cond-index-"),
#                      model_string = paste0(model_name,"-15-km"),
#                      .id = "model")
#



# model_name <- "all-st2002-doy"
model_name <- "all-st2002-doy-unweighted"
# model_name <- "all-st2002-doy-within-yr-weights"
# model_name <- "all-st2002-doy-small-weights"

f1 <- list.files(paste0("data-generated/cond-index/",
                       model_name), pattern = ".rds", full.names = TRUE)

d1 <- purrr::map_dfr(f1, readRDS)

min_est <- min(d1$est)
max_est <- max(d1$est)


# model_name <- "all-st2002-doy-d0c"
# model_name <- "all-st2002-doy-d0c-unweighted"
# model_name <- "all-st2002-doy-d0c-within-yr-weights"
# model_name <- "all-st2002-doy-d0c-small-weights"
#

model_name <- "all-st2002-doy-ld0c-unweighted"

f2 <- list.files(paste0("data-generated/cond-index/",
                       model_name), pattern = ".rds", full.names = TRUE)

d2 <- purrr::map_dfr(f2, readRDS)

# d1 %>% mutate(
d2 %>% mutate(
              # group = forcats::fct_relevel(group, "immatures", "mature males", "mature females"),
              group = factor(group, levels = c("immatures", "mature males", "mature females"),
                             labels = c("Immatures", "Mature males", "Mature females")),
              upr_trimmed = ifelse(upr > 1.4, 1.4, upr)
              ) %>%
ggplot( aes(year, est, fill = group)) +
  geom_line(aes(colour = group)#, linewidth = 1
            ) +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr_trimmed, alpha = group)) +
  geom_hline(yintercept = 1, linetype = "dotted") +

  # scale_y_log10() +
  facet_wrap(~species,
             scales = "free_y",
             ncol = 5) +
  # scale_color_brewer(palette = "Set3") +
  # scale_fill_brewer(palette = "Set3") +
  coord_cartesian( ylim = c(min_est, max_est)) +
  scale_alpha_discrete(range = c(0.1, 0.3)) +
  scale_color_viridis_d(
                        # end = 0.99,
                        direction =1,
                        option = "D") +
  scale_fill_viridis_d(option = "D",
                       # end = 0.99,
                       direction =1) +
  # ggtitle(paste(model_name))+
  theme(legend.position = "none") +
  labs(
    x = "", y = "",
       # x = "Year", y = "Condition factor",
       alpha = "",
       fill = "",
       colour = "")

ggsave(paste0("figs/all-condition-indices-fixed-",
# ggsave(paste0("figs/all-condition-indices-free-",
              model_name, ".png"),
       height = fig_height*.5, width = fig_width*0.9
)

