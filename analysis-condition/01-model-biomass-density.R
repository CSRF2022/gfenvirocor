# 1. Create a total biomass distribution model (to use for prediction grid)
# 2. Create prediction grid with density and depth
# TODO: get matching depth for use in models and on prediction grids?
devtools::load_all()
library(tidyverse)
library(sdmTMB)
library(gfplot)
library(ggsidekick)
options(scipen = 100, digits = 4)

theme_set(theme_sleek())

species_list <- c(
  "Petrale Sole"
  # "Canary Rockfish"
  # "Arrowtooth Flounder"
  # "North Pacific Spiny Dogfish"
  # "Pacific Cod"
  )



# knot_distance <- 5
# knot_distance <- 12
knot_distance <- 15
# knot_distance <- 20

mat_threshold <- 0.5

fig_height <- 4 * 2
fig_width <- 5 * 2

extra_years <- c(1985, 1986, 1988, 1990,
                 1992, 1994, 1997, 1999, 2001)

surveys_included <- c("HS MSA", "SYN HS", "SYN QCS", "SYN WCHG", "SYN WCVI")

dens_model_name_long <- "survey type, depth, and DOY"

i = 1
# for(i in seq_along(species_list)){

# extra_years <- NULL
# dens_model_name <- "-w-survey-factor-tw-1-22"
# # try adding vessel captain combo
# dens_model_name <- "-w-survey-factor-tw-1-22-10min-vcc"
# # try just vessel # didn't converge for petrale
# dens_model_name <- "-w-survey-factor-tw-1-22-10min-v"

# dens_model_name <- "-w-survey-factor-tw-1-22-10min-xt-vcc"
# dens_model_name <- "-w-survey-factor-tw-1-22-10min-xt-vc"
# dens_model_name <- "-w-survey-factor-tw-1-22-10min-xt-c"
# dens_model_name <- "-w-survey-factor-dg-1-22-10min-xt-c"
# dens_model_name <- "-w-survey-factor-tw-1-22-10min-xt-i"

# dens_model_name <- "-w-survey-factor-dg-doy-1-22-10min-xt"

set_family <- delta_gamma()
dens_model_name <- "-dg-doy-1-22-10min-xt-offset"

# set_family <- tweedie()
# dens_model_name <- "-tw-doy-1-22-10min-xt-offset"

set_family2 <- tweedie()

# # try just vessel # didn't converge for petrale
# dens_model_name <- "-w-survey-factor-tw-1-22-10min-xt-v"


if(length(set_family)== 6){
  set_spatial <- "on"
  set_spatiotemporal <- list("rw", "rw")
} else {
  set_spatial <- "on"
  set_spatiotemporal <- "rw"
}


if(species_list[i]=="North Pacific Spiny Dogfish") {
 custom_maturity_code <- c(NA, 55)
 custom_length_threshold <- NULL
 # # custom_length_threshold <- c(70.9, 86.2)
 set_family <- delta_lognormal_mix()
 set_family2 <- delta_lognormal()
 # set_spatiotemporal <- list("rw", "off")
 # # dens_model_name <- "-w-survey-factor-tw-exp"
 # dens_model_name <- "-w-survey-factor-lnm-nosp-1-22-xt"
 dens_model_name <- "-lnm-doy-1-22-xt-offset"
} else{
  custom_maturity_code <- NULL
  custom_length_threshold <- NULL
}

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species_list[i])))

# TODO: extract a new version of survey sets that includes data dropped from the grid
# all_set_dat <- readRDS("data-raw/survey-sets-all-species.rds")

dset <- readRDS("data-raw/survey-sets-all.rds") %>%
  filter(species_common_name == tolower(species_list[i])) %>%
  # select(-sample_id) %>% # it seems multiple sample ids from the same set were frequently used when some fish were weighed and some not?
  # filter(!(sample_id == 285491) | is.na(sample_id)) %>% # filter one of the 2 sample ids associated with duplicated event
  # currently choosing the max series of years without a gap
  # MSA occurs in 2002 and 2003, and SYN QCS starts in 2003
  filter(survey_abbrev %in% surveys_included) %>%
  distinct()

check_for_duplicates <- dset[duplicated(dset$fishing_event_id), ] # shouldn't be any unless something other than sample_ids can cause them

dsamp <- readRDS("data-raw/survey-samples-all.rds") %>%
  filter(survey_abbrev != "SABLE OFF") %>%
  filter(species_common_name == tolower(species_list[i])) %>% distinct()
## check against original function call
# dsamp1 <- readRDS("data-raw/survey-samples-all-original.rds") %>%
#   filter(survey_abbrev != "SABLE OFF") %>%
#   filter(species_common_name == tolower(species_list[i])) %>% distinct()

# note that month in this data frame is inaccurate because it is derived from the start of the trip not the true sample date

unique(dsamp$survey_abbrev)

unique(dsamp$maturity_code)

# dsamp2 <- filter(dsamp, survey_abbrev == "SYN WCVI")

dss <- split_catch_by_sex(dset, dsamp,
                          survey = surveys_included,
                          immatures_pooled = TRUE,
                          custom_maturity_at = custom_maturity_code,
                          custom_length_thresholds = custom_length_threshold,
                          p_threshold = mat_threshold,
                          plot = TRUE)

# dss$maturity_plot
# dss$weight_plot

meandoors <- dss$data %>%
  filter(group_name == "Mature females" &
           usability_code == 1 &
           doorspread_m != 0) %>%
  group_by(survey_id) %>% summarise(
    mean_doorspread = mean(doorspread_m, na.rm = TRUE)
    )

ds <- dss$data %>% left_join(meandoors) %>%
  # filter(!(usability_code %in% c(5,9,13))) %>%
  # filter(usability_code %in% c(1, 22)) %>%
  filter(usability_code %in% c(1, 22, 16, 6)) %>%
  filter(duration_min >= 10) %>%
  mutate(
    DOY = as.numeric(strftime(time_deployed, format = "%j")),
    days_to_solstice = DOY - 172,
    fishing_event_id = as.factor(fishing_event_id),
    vessel_cap_combo = factor(paste0(vessel_id, "-", captain_id)),
    vessel = as.factor(vessel_id),
    captain = as.factor(ifelse(is.na(captain_id),
                               paste0(vessel_id, "-", year), captain_id)),
    usability_name = paste(usability_code, "-", usability_desc),
    usability_name = fct_reorder(usability_name, usability_code),
    doorspread_m = ifelse(doorspread_m == 0, mean_doorspread, doorspread_m),
    log_depth = log(depth_m),
    log_depth_c = log_depth - 5, # mean and median for whole data set
    survey_type = as.factor(ifelse(survey_abbrev == "HS MSA", "MSA", "SYN")),
    area_swept = ifelse(is.na(tow_length_m), doorspread_m * duration_min * speed_mpm, doorspread_m * tow_length_m)
  ) #%>% filter(area_swept > 0)


ds <- ds %>% filter(!is.na(catch_weight))
ds <- ds %>% filter(!is.na(depth_m))
ds <- ds %>% filter(!is.na(area_swept))
ds <- ds %>% filter(!is.na(latitude))
ds <- ds %>% filter(!is.na(longitude))
ds$offset <- log(ds$area_swept / 100000)

# ds %>% group_by(usability_code, usability_desc, survey_type) %>% summarise(
#   mean_catch = mean(catch_weight, na.rm = TRUE),
#   sum_catch = sum(catch_weight, na.rm = TRUE),
#   n = n()
# )

ds %>% filter(group_name == "Mature females") %>% distinct() %>%
  ggplot() +
  # geom_violin(aes(area_swept, usability_desc))+
  geom_point(aes(area_swept, usability_name, size = log(catch_weight+1)), alpha=0.3) +
  scale_y_discrete(limits=rev) +
  facet_grid(~survey_abbrev, scales = "free") +
  theme(axis.title.y = element_blank())
#
# ggsave("figs/all-usabilities-petrale.png", width = 17, height = 3.5)

unique(ds$survey_abbrev)
unique(ds$vessel_cap_combo)

sort(unique(ds$year))
# ds %>% select(survey_abbrev, year,
#               vessel_id, captain_id, vessel_cap_combo) %>% distinct() %>% View()

# check that offset doesn't contain NAs of Inf
range(ds$offset)
range(ds$area_swept)
mean(ds$offset)

ggplot(ds) +
  geom_histogram(aes(offset)) +
  facet_wrap(~year)

ds$X <- NULL
ds$Y <- NULL

d <- sdmTMB::add_utm_columns(ds, ll_names = c("longitude", "latitude"), utm_crs = 32609)

if(is.null(extra_years)) {
grid <- replicate_df(gfplot::synoptic_grid, time_name = "year", time_values = unique(d$year)) %>%
  mutate(
    fishing_event_id = as.factor(paste(ds$fishing_event_id[1])),
    days_to_solstice = 0,
    log_depth = log(depth),
    log_depth_c = log_depth - 5, # mean and median for whole data set
    survey_type = as.factor("SYN"),
    vessel = as.factor("584"),
    captain = as.factor("408"),
    vessel_cap_combo = as.factor("584-408") # has greatest spatial coverage
  )
} else {
#if extra time
grid <- replicate_df(gfplot::synoptic_grid, time_name = "year",
                     time_values = min(d$year):max(d$year)) %>%
  mutate(
    fishing_event_id = as.factor(paste(ds$fishing_event_id[1])),
    days_to_solstice = 0,
    log_depth = log(depth),
    log_depth_c = log_depth - 5, # mean and median for whole data set
    survey_type = as.factor("SYN"),
    captain = as.factor("408"),
    vessel = as.factor("584"),
    vessel_cap_combo = as.factor("584-408") # has greatest spatial coverage
  )
}


# check that my data and grid are on the same XY scale
range(grid$year)
range(d$X)
range(grid$X)
range(d$Y)
range(grid$Y)

dp <- d %>% filter(catch_weight > 1)

hist(log(dp$catch_weight))
range(d$catch_weight)

# # Keep only synoptic data
# d <- filter(d, survey_type == "SYN")

d1 <- d %>% filter(group_name == "Mature females")

mesh <- make_mesh(d1, c("X", "Y"), cutoff = knot_distance)

ggplot() +
  inlabru::gg(mesh$mesh) +
  coord_fixed() +
  geom_point(aes(X, Y, colour = catch_weight, size = catch_weight),
             data = filter(d1, catch_weight > 0)) +
  facet_wrap(~year) +
  scale_color_viridis_c(trans = "fourth_root_power")

mesh$mesh$n
# exp data
# dens_model_name <- "-w-survey-factor-dgm-s2off"
# dens_model_name <- "-w-survey-factor-dgm-nobist"
# dens_model_name <- "-w-survey-factor-1range-dgm"

# dens_model_name <- "-w-survey-factor-1range-dgm-tvyr-s2off"
# dens_model_name <- "-w-survey-factor-1range-dgm-s2off"
#
# dens_model_name <- "-w-survey-factor-1range-dgm-tvyr"
# dens_model_name <- "-w-survey-factor-1range-sigma2off"



dir.create(paste0("data-generated/density-models/"), showWarnings = FALSE)
fm <- paste0("data-generated/density-models/", spp, "-total", dens_model_name, "-", knot_distance, "-km.rds")

if (!file.exists(fm)) {

  m <- sdmTMB(
    # list(
    catch_weight ~ 1 + survey_type +
      poly(log_depth_c, 2) +
      poly(days_to_solstice, 2),# +
      # (1|vessel),
      # (1|fishing_event_id),
      # (1|captain),
      # (1|vessel_cap_combo),
    offset = "offset",
    mesh = mesh,
    data = d1,
    spatial = set_spatial,
    spatiotemporal = set_spatiotemporal,
    share_range = FALSE,
    silent = FALSE,
    time = "year",
    extra_time = extra_years,
    # reml = TRUE,
    family = set_family,
    control = sdmTMBcontrol(
      start = list(logit_p_mix = qlogis(0.01)),
      map = list(logit_p_mix = factor(NA)),
      newton_loops = 0L
    ),
    priors = sdmTMBpriors(
      matern_s = pc_matern(range_gt = knot_distance * 1.5,
                           sigma_lt = 2),
      matern_st = pc_matern(range_gt = knot_distance * 1.5,
                            sigma_lt = 2)
    )
  )

if(length(set_family) == 6) {
  m <- refine_delta_model(m)
} else {
  m <- refine_model(m)
}

saveRDS(m, fm)

} else {
  m <- readRDS(fm)
}

m
m$sd_report
tidy(m, "ran_pars", conf.int = TRUE, model = 1)
tidy(m, "ran_pars", conf.int = TRUE, model = 2)

m0 <- sdmTMB(
  catch_weight ~ 1,
  # mesh = mesh,
  data = m$data,
  spatial = "off",
  spatiotemporal = "off",
  silent = FALSE,
  family = m$family
)

de <- 1 - (logLik(m)/logLik(m0))
de[1]

p <- predict(m, re_form_iid = NA, newdata = grid, return_tmb_object = TRUE)


map_density(p, "total", col_trans = "log10") +
  labs(title = paste0(species_list[i], ": total biomass (", dens_model_name, ")"),
       subtitle = paste0("Deviance explained:", round(de[1], 2)))

ggsave(paste0("figs/density-map-", spp, "-total", dens_model_name, "-",
              knot_distance, "-km.png"),
       height = fig_height, width = fig_width
)


# # not working with offset
# g <- ggeffects::ggeffect(m, paste0("log_depth [",
#                                    range(d$log_depth)[1], ":", range(d$log_depth)[2], "by=0.05]"))
# plot(g)

## mature females
fmf <- paste0("data-generated/density-models/", spp, "-mat-fem", dens_model_name, "-", knot_distance, "-km.rds")
d2 <- d1 %>% filter(year > 2001)
mesh2 <- make_mesh(d2, c("X", "Y"), cutoff = knot_distance)

grid2 <- grid %>% filter(year > 2001)


if (!file.exists(fmf)) {

  mf <- sdmTMB(group_catch_est ~ 1 + survey_type +
                 poly(log_depth_c, 2) +
                 poly(days_to_solstice, 2),# +
               # (1|captain),
               # (1|vessel_cap_combo),
               offset = "offset",
               spatial = as.list(m[["spatial"]]),
               spatiotemporal = as.list(m[["spatiotemporal"]]),
               time = "year",
               family = m$family,
               extra_time = NULL,
               silent = FALSE,
               mesh = mesh2, data = d2)

  # mf <- update(m,
  #              group_catch_est ~ 1 + survey_type + poly(log_depth, 2), #+
  #              # # (1|captain),
  #              # # (1|vessel_cap_combo),
  #              # spatial = as.list(m[["spatial"]]),
  #              # spatiotemporal = as.list(m[["spatiotemporal"]]),
  #              # time = "year",
  #              # family = m$family,
  #              extra_time = NULL,
  #              # silent = FALSE,
  #              mesh = mesh2, data = d2)

  if (length(mf$family) == 6) {
    mf <- refine_delta_model(mf)
  } else {
    mf <- refine_model(mf)
  }

  saveRDS(mf, fmf)

} else {
  mf <- readRDS(fmf)
}

# check that model updated properly
sort(unique(m$data$year))
sort(unique(mf$data$year))

pf <- predict(mf, re_form_iid = NA, newdata =filter(grid, year > 2001), return_tmb_object = TRUE)

map_density(pf, "mat-fem") +
  labs(title = paste0(species_list[i], ": mature female biomass (", dens_model_name, ")"))

ggsave(paste0("figs/density-map-", spp, "-mat-fem", dens_model_name, "-", knot_distance, "-km.png"),
       height = fig_height, width = fig_width
)


## mature males
fmm <- paste0("data-generated/density-models/", spp, "-mat-m", dens_model_name, "-", knot_distance, "-km.rds")

if (!file.exists(fmm)) {

  d2b <- d %>% filter(group_name == "Mature males") %>% filter(year > 2001)

  mm <- update(mf, mesh = mesh2, data = d2b)

  if (length(mm$family) == 6) {
    mm <- refine_delta_model(mm)
  } else {
    mm <- refine_model(mm)
  }
  saveRDS(mm, fmm)

} else {
  mm <- readRDS(fmm)
}

pm <- predict(mm, re_form_iid = NA, newdata =filter(grid, year > 2001), return_tmb_object = TRUE)

map_density(pm, "mat-m") +
  labs(title = paste0(species_list[i], ": mature male biomass (", dens_model_name, ")"))

ggsave(paste0("figs/density-map-", spp, "-mat-m", dens_model_name, "-", knot_distance, "-km.png"),
       height = fig_height, width = fig_width
)


fmi <- paste0("data-generated/density-models/", spp, "-imm", dens_model_name, "-", knot_distance, "-km.rds")

if (!file.exists(fmi)) {
  d4 <- d %>%
    filter(group_name %in% c("Immature")) %>%
    filter(year > 2001)

  mi <- update(mm,
               extra_time = NULL,
               data = d4)

  if (length(mi$family) == 6) {
    mi <- refine_delta_model(mi)
  } else {
    mi <- refine_model(mi)
  }
  saveRDS(mi, fmi)
} else {
  mi <- readRDS(fmi)
}

pi <- predict(mi, re_form_iid = NA, newdata =filter(grid, year > 2001), return_tmb_object = TRUE)

map_density(pi, "imm") +
  labs(title = paste0(species_list[i], ": immature biomass (", dens_model_name, ")"))

ggsave(paste0("figs/density-map-", spp, "-imm", dens_model_name, "-", knot_distance, "-km.png"),
  height = fig_height, width = fig_width
)


# Generate coastwide indices

plot_index(p, "total") + ggtitle(paste0(species_list[i], ": total biomass (", dens_model_name, ")"))

# ggsave(paste0("figs/density-index-", spp, "-total", dens_model_name, "-", knot_distance, "-km.png"),
#   height = fig_height / 2, width = fig_width /1.5
# )

plot_index(pf, "mat-fem") +
  ggtitle(paste0(species_list[i], ": mature female biomass (", dens_model_name, ")"))

# ggsave(paste0("figs/density-index-", spp, "-mat-fem", dens_model_name, "-", knot_distance, "-km.png"),
#   height = fig_height / 2, width = fig_width / 1.5
# )

plot_index(pm, "mat-m") +
  ggtitle(paste0(species_list[i], ": mature male biomass (", dens_model_name, ")"))

# ggsave(paste0("figs/density-index-", spp, "-mat-m", dens_model_name, "-", knot_distance, "-km.png"),
#        height = fig_height / 2, width = fig_width / 1.5
# )

plot_index(pi, "imm") +
  ggtitle(paste0(species_list[i], ": immature biomass (", dens_model_name, ")"))

# ggsave(paste0("figs/density-index-", spp, "-imm", dens_model_name, "-", knot_distance, "-km.png"),
#        height = fig_height / 2, width = fig_width / 1.5
# )



f0 <- paste0("data-generated/density-index/", spp, "-p-total", dens_model_name, "-", knot_distance, "-km.rds")
f1 <- paste0("data-generated/density-index/", spp, "-p-mat-fem", dens_model_name, "-", knot_distance, "-km.rds")
f2 <- paste0("data-generated/density-index/", spp, "-p-mat-m", dens_model_name, "-", knot_distance, "-km.rds")
f3 <- paste0("data-generated/density-index/", spp, "-p-imm", dens_model_name, "-", knot_distance, "-km.rds")

ind0 <- readRDS(f0) %>% mutate(index = "Total")
ind1 <- readRDS(f1) %>% mutate(index = "Mature female")
ind2 <- readRDS(f2) %>% mutate(index = "Mature male")
ind3 <- readRDS(f3) %>% mutate(index = "Immature")

bc_inds <- bind_rows(ind0, ind1, ind2, ind3)

p1 <- bc_inds %>%  mutate(index = fct_relevel(index, rev)) %>%
  ggplot(aes(year, est)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = index), alpha = 0.3) +
  geom_line(aes(colour = index), linewidth = 0.7) +
  scale_colour_viridis_d(direction = 1, end = 0.8, option = "A") +
  scale_fill_viridis_d(direction = 1, end = 0.8, option = "A") +
  labs(colour = "Biomass Index", fill = "Biomass Index") +
  xlab("Year") +
  ylab("Biomass estimate (kg)")+
  ggtitle(paste0(species_list[i]),
          subtitle = paste0("Model: ", ifelse(
            length(m$family)==6, m$family[6], paste0(m$family[1],"(link = 'log')")
            ), ", spatial (", m[["spatial"]][1], ", ", m[["spatial"]][2],
            ") with st RW and ", dens_model_name_long))

p1
# ggsave(paste0("figs/density-index-", spp, "-all", dens_model_name, "-", knot_distance, "-km.png"),
#        height = fig_height / 2, width = fig_width / 1.5
# )


survey_years <- d1 %>% select(survey_abbrev, year) %>% distinct() %>%
  mutate(survey = ifelse(survey_abbrev == "HS MSA", "SYN HS", survey_abbrev))

grid2 <- grid %>% filter(year > 2001)

fsi <- paste0("data-generated/density-index/", spp, "-all-split-", dens_model_name, "-", knot_distance, "-km.rds")

if (!file.exists(fsi)) {
preds <- grid |>
  split(grid$survey) |>
  lapply(function(x) predict(m, re_form_iid = NA, newdata =x, return_tmb_object = TRUE))
inds <- purrr::map_dfr(preds, get_index, area = 4, .id = "survey")
inds$index <- "Total"
inds$model <- paste0(ifelse(
  length(m$family)==6, m$family[6], paste0(m$family[1],"(link = 'log')")
), "\nspatial (", m[["spatial"]][1], ", ", m[["spatial"]][2], ")")

pfs <- grid2 |>
  split(grid2$survey) |>
  lapply(function(x) predict(mf, re_form_iid = NA, newdata = x, return_tmb_object = TRUE))
inds2 <- purrr::map_dfr(pfs, get_index, area = 4, .id = "survey")
inds2$index <- "Mature female"
inds2$model <- paste0(ifelse(
  length(mf$family)==6, mf$family[6], paste0(mf$family[1],"(link = 'log')")
), "\nspatial (", mf[["spatial"]][1], ", ", mf[["spatial"]][2], ")")

pms <- grid2 |>
  split(grid2$survey) |>
  lapply(function(x) predict(mm, re_form_iid = NA, newdata = x, return_tmb_object = TRUE))
inds3 <- purrr::map_dfr(pms, get_index, area = 4, .id = "survey")
inds3$index <- "Mature male"
inds3$model <- paste0(ifelse(
  length(mm$family)==6, mm$family[6], paste0(mm$family[1],"(link = 'log')")
), "\nspatial (", mm[["spatial"]][1], ", ", mm[["spatial"]][2], ")")

pis <- grid2 |>
  split(grid2$survey) |>
  lapply(function(x) predict(mi, re_form_iid = NA, newdata = x, return_tmb_object = TRUE))
inds4 <- purrr::map_dfr(pis, get_index, area = 4, .id = "survey")
inds4$index <- "Immature"
inds4$model <- paste0(ifelse(
  length(mi$family)==6, mi$family[6], paste0(mi$family[1],"(link = 'log')")
), "\nspatial (", mi[["spatial"]][1], ", ", mi[["spatial"]][2], ")")

all_split_inds <- bind_rows(inds, inds2, inds3, inds4) %>%
  left_join(survey_years, ., multiple = "all") %>%
  filter(!is.na(est))

saveRDS(all_split_inds, fsi)
} else {
all_split_inds <- readRDS(fsi)
}


p2 <- all_split_inds %>%
  filter(index == "Total") %>%
  mutate(index = fct_relevel(index, rev)) %>%
  ggplot(aes(year, est)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = survey), alpha = 0.3) +
  geom_line(aes(colour = survey)) +
  # facet_wrap(~index, scales = "free") +
  # facet_wrap(~index, scales = "free_x") +
  scale_colour_viridis_d(direction = -1, end = 0.9) +
  scale_fill_viridis_d(direction = -1, end = 0.9) +
  labs(colour = "Area", fill = "Area") +
  coord_cartesian(ylim = c(0, max(all_split_inds$est)*1.5)) +
  ggtitle("Total")+
  xlab("Year") +
  ylab("Biomass estimate (kg)")

p2

p3dat <- filter(all_split_inds, index != "Total")

p3 <- p3dat %>%
  mutate(index = fct_relevel(index, rev)) %>%
  ggplot(aes(year, est)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = survey), alpha = 0.3) +
    geom_line(aes(colour = survey)) +
    # facet_wrap(~index, scales = "free") +
    facet_wrap(~index, scales = "free_x") +
    scale_colour_viridis_d(direction = -1, end = 0.9) +
    scale_fill_viridis_d(direction = -1, end = 0.9) +
  labs(colour = "Area", fill = "Area") +
  coord_cartesian(ylim = c(0, max(p3dat$est)*1.25)) +
  # ggtitle(paste0(species_list[i], " (", dens_model_name, ")"))+
    xlab("Year") +
    ylab("Biomass estimate (kg)")

if(length(unique(all_split_inds$model))>1){
  p3 <- p3 + geom_text(aes(label = model),
      x = 2003, y = max(p3dat$upr)*0.9, size = 2.5, hjust = 0)
  }

library(patchwork)
p1a <- p1 + theme(axis.text.x = element_blank(), axis.title = element_blank())
p2a <- p2 + theme(axis.title.x = element_blank())
p3a <- p3 + theme(axis.title.y = element_blank(), legend.position = "none")

p1a + p2a + p3a + plot_layout(ncol = 1)

ggsave(paste0("figs/density-index-", spp, "-all", dens_model_name, "-", knot_distance, "-km.png"),
       height = fig_height, width = fig_width
)


# dir.create(paste0("data-generated/density-depth-curves/"), showWarnings = FALSE)
# saveRDS(g, paste0("data-generated/density-depth-curves/", spp, "-d", dens_model_name, ".rds"))





# ## both mature males and females combined
# fmm2 <- paste0("data-generated/density-models/", spp, "-all-mat", dens_model_name, "-", knot_distance, "-km.rds")
#
# if (!file.exists(fmm2)) {
#   d3 <- ds %>%
#     filter(group_name %in% c("Mature males") & year > 2001) %>%
#     select(fishing_event_id, group_catch_est_mm = group_catch_est)
#   d3 <- left_join(d2, d3) %>%
#     mutate(group_catch_est2 = group_catch_est + group_catch_est_mm)
#
#   mm2 <- update(mf, group_catch_est2 ~ 1 + survey_type + poly(log_depth, 2), data = d3)
#
#   mm2 <- refine_model(mm2)
#   saveRDS(mm2, fmm2)
# } else {
#   mm2 <- readRDS(fmm2)
# }
#
# pm2 <- predict(mm2, re_form_iid = NA, newdata =filter(grid, year > 2001), return_tmb_object = TRUE)
#
# map_density(pm2, "all-mat") +
#   labs(title = paste0(species_list[i], ": mature biomass (", dens_model_name, ")"))
#
# ggsave(paste0("figs/density-map-", spp, "-all-mat", dens_model_name, "-", knot_distance, "-km.png"),
#   height = fig_height, width = fig_width
# )
#
# plot_index(pm2, "all-mat") + ggtitle(paste0(species_list[i], ": mature biomass (", dens_model_name, ")"))
# ggsave(paste0("figs/density-index-", spp, "-all-mat", dens_model_name, "-", knot_distance, "-km.png"),
#   height = fig_height / 2, width = fig_width / 2
# )
#

