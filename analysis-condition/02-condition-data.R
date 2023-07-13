# 1. Calculate Le Cren’s relative condition factor for each fish sampled
# 2. Remove the most extreme outliers that are likely errors
# 3. Split samples into immature and mature and into length bins
# 4. Calculate ‘weights' (sample multiplier for each fish sampled)
library(tidyverse)
library(gfplot)


# mat_threshold  <-  0.05
mat_threshold <- 0.5
# mat_threshold <- 0.95


species_list <- c("Petrale Sole")
# species_list <- c("Canary Rockfish")

# TODO: there appear to be a lot of samples from sets that were ultimately deemed un-usable that might be retrieved if I can get an updated dataframe that includes those sets

dset <- readRDS("data-raw/survey-sets.rds") %>% filter(species_common_name == tolower(species_list))
dat1 <- readRDS("data-raw/specimen-data.rds") %>% filter(species_common_name == tolower(species_list))

dat <- left_join(
  # dat1,
  select(dat1, -sample_id, -grouping_code),
  # select(dat1, species_common_name, fishing_event_id, year, length, sex, age, weight, usability_code, specimen_id),
  dset,
  multiple = "all"
)


# TODO: decide which surveys to include, for now including all data available
# Have there been any measurement changes or are there differences between HBLL ans synoptic in lengths or maturity keys?

# dat <- filter(dat, survey_abbrev %in% c("SYN HS"   ,"SYN QCS",  "SYN WCHG", "SYN WCVI"))
# dat <- filter(dat, !is.na(depth_m))


# 1. Le Cren’s relative condition factor

mf <- gfplot::fit_length_weight(dat, sex = "female")
mm <- gfplot::fit_length_weight(dat, sex = "male")

plot_length_weight(object_female = mf, object_male = mm)

# mf$data$predicted_weight <- exp(mf$pars$log_a + mf$pars$b * log(mf$data$length))
# mf$data$residuals <- (mf$data$weight - mf$data$predicted_weight)/mf$data$weight
# plot(residuals~length, data = mf$data)


df <- dplyr::filter(dat, sex == 2, !is.na(weight), !is.na(length))
dm <- dplyr::filter(dat, sex == 1, !is.na(weight), !is.na(length))

df$wbar <- exp(mf$pars$log_a) * df$length^mf$pars$b * 1000
dm$wbar <- exp(mm$pars$log_a) * dm$length^mm$pars$b * 1000

# include unknown sex individuals for now, because immature individuals can be difficult to sex and differences in growth rate may be slim
du <- dplyr::filter(dat, sex %in% c(0, 3), !is.na(weight), !is.na(length))

# Apply an intermediate slope and intercept to these individuals
du$wbar <- exp((mm$pars$log_a + mf$pars$log_a) / 2) * du$length^((mm$pars$b + mf$pars$b) / 2) * 1000

dd <- bind_rows(df, dm, du)
dd$cond_fac <- dd$weight / dd$wbar

# plot(cond_fac~length, data = dd)


# 2. Remove outliers
# dd <- filter(dd, cond_fac < quantile(dd$cond_fac, probs = 0.995))
# dd <- filter(dd, cond_fac > quantile(dd$cond_fac, probs = 0.005))

# ds <- select(dd, fishing_event_id, sex, age, length, weight, maturity_code, wbar, cond_fac, specimen_id, year, survey_abbrev)

plot(cond_fac ~ length, data = dd)

# remove extreme outliers from a sample where the scale or board were clearly calibrated wrong.
dd2 <- filter(dd, cond_fac < 1000)
plot(cond_fac ~ length, data = dd2)

# remove the most extreme outliers that are likely errors
dd2 <- filter(dd2, cond_fac < quantile(dd$cond_fac, probs = 0.9975))
plot(cond_fac ~ length, data = dd2)

# TODO: could trim lower, but looking more balance without trimming for petrale
dd2 <- filter(dd2, cond_fac > quantile(dd$cond_fac, probs = 0.0025))
plot(cond_fac ~ length, data = dd2)



group_by(dd2, year) %>%
  summarise(mcond = mean(cond_fac)) %>%
  ggplot(aes(year, mcond)) +
  geom_line()

fish <- dd2

ggplot(
  filter(fish, !is.na(latitude) & !is.na(longitude)),
  aes(longitude, latitude, colour = log(cond_fac))
) +
  geom_point() +
  facet_wrap(~year) +
  scale_colour_gradient2()

#
# ds %>% select(fishing_event_id, year, length, sex, age, weight, specimen_id,
#               survey_abbrev, wbar, cond_fac) %>% View()

plot(weight ~ length, data = dd, col = "red")
points(weight ~ length, data = ds)
abline(v = m$mat_perc$f.p0.95, col = "green")
abline(v = m$mat_perc$m.p0.95, col = "blue")

# 3. Split samples into immature and mature and into length bins
# Starting with code lifted from split by maturity function in case we also want to functionalize this
# arguements required:
custom_maturity_at <- NULL
sample_id_re <- TRUE
year_re <- TRUE
p_threshold <- mat_threshold
split_by_maturity <- TRUE
split_by_sex <- TRUE
immatures_pooled <- TRUE


# -----
# does maturity data exist at all for this species?




if (split_by_maturity) {
  maturity_codes <- unique(fish$maturity_code)

  if (length(maturity_codes) < 3) {
    warning("Fewer than 3 maturity codes; returning NULL data.", call. = FALSE)
    return(list(data = survey_sets, maturity = NULL, weight_model = NULL))
  }

  # Check if only some years without maturity data, and set year_re = FALSE in that case
  years_w_maturity <- fish %>%
    group_by(year) %>%
    mutate(maturity_levels = length(unique(maturity_code)))

  levels_per_year <- unique(years_w_maturity$maturity_levels)


  if (max(levels_per_year) < 3) { # NA plus only one recorded maturity level is max ever recorded
    warning("Maturity data not recorded, so catch not split.", call. = FALSE)
    return(list(data = survey_sets, model = NULL))
  }

  if (min(levels_per_year) < 3) { # some years lack maturity data

    years_w_maturity %>%
      select(year, maturity_levels) %>%
      distinct() %>%
      arrange(year)
    fish <- filter(fish, year >= 2002)
    years_w_maturity <- fish %>%
      group_by(year) %>%
      mutate(maturity_levels = length(unique(maturity_code)))

    levels_per_year <- unique(years_w_maturity$maturity_levels)
  }

  f_fish <- fish %>%
    filter(sex == 2) %>%
    mutate(year_f = as.character(year))

  m_fish <- fish %>%
    filter(sex == 1) %>%
    mutate(year_f = as.character(year))




  if (min(levels_per_year) < 3) { # some years lack maturity data

    if (length(levels_per_year) < 1) { # TODO: check if this threshold should actually be 2
      warning("Maturity data not recorded, so catch not split.", call. = FALSE)
      return(list(data = survey_sets, model = NULL))
    } else {
      warning("Some years lack maturity data, but catch still split.", call. = FALSE)

      m <- fit_mat_ogive(fish,
        type = "length",
        sample_id_re = sample_id_re,
        custom_maturity_at = custom_maturity_at
      )

      if (p_threshold == 0.5) {
        f_fish$threshold <- m$mat_perc$f.p0.5
        m_fish$threshold <- m$mat_perc$m.p0.5
      }
      if (p_threshold == 0.05) {
        f_fish$threshold <- m$mat_perc$f.p0.05
        m_fish$threshold <- m$mat_perc$m.p0.05
      }
      if (p_threshold == 0.95) {
        f_fish$threshold <- m$mat_perc$f.p0.95
        m_fish$threshold <- m$mat_perc$m.p0.95
      }
    }
  } else {
    if (year_re) {
      #
      # sample_id_re <- FALSE
      #       # p_threshold <- 0.5 # Probability of maturity to split at. Default = 0.5. Alternatives are 0.05 or 0.95.
      #       year_re <- FALSE
      #

      m <- fit_mat_ogive(fish,
        type = "length",
        sample_id_re = sample_id_re,
        year_re = TRUE,
        custom_maturity_at = custom_maturity_at
      )
      if (p_threshold == 0.5) {
        f_fish$threshold <- lapply(f_fish$year_f, function(x) m$mat_perc[[x]]$f.p0.5)
        m_fish$threshold <- lapply(m_fish$year_f, function(x) m$mat_perc[[x]]$m.p0.5)
      }
      if (p_threshold == 0.05) {
        f_fish$threshold <- lapply(f_fish$year_f, function(x) m$mat_perc[[x]]$f.p0.05)
        m_fish$threshold <- lapply(m_fish$year_f, function(x) m$mat_perc[[x]]$m.p0.05)
      }
      if (p_threshold == 0.95) {
        f_fish$threshold <- lapply(f_fish$year_f, function(x) m$mat_perc[[x]]$f.p0.95)
        m_fish$threshold <- lapply(m_fish$year_f, function(x) m$mat_perc[[x]]$m.p0.95)
      }
    } else {
      m <- fit_mat_ogive(fish,
        type = "length",
        sample_id_re = sample_id_re,
        custom_maturity_at = custom_maturity_at
      )

      # apply global estimates to all catches
      if (p_threshold == 0.5) {
        f_fish$threshold <- m$mat_perc$f.p0.5
        m_fish$threshold <- m$mat_perc$m.p0.5
      }
      if (p_threshold == 0.05) {
        f_fish$threshold <- m$mat_perc$f.p0.05
        m_fish$threshold <- m$mat_perc$m.p0.05
      }
      if (p_threshold == 0.95) {
        f_fish$threshold <- m$mat_perc$f.p0.95
        m_fish$threshold <- m$mat_perc$m.p0.95
      }
      # # or could choose to save sample_id estimates and apply them like this
      # if(p_threshold == 0.5) {
      #   f_fish$threshold <- lapply(f_fish$sample_id, function(x) m$mat_perc[[x]]$f.p0.5)
      #   m_fish$threshold <- lapply(m_fish$sample_id, function(x) m$mat_perc[[x]]$m.p0.5)
      # }
      # if(p_threshold == 0.05) {
      #   f_fish$threshold <- lapply(f_fish$sample_id, function(x) m$mat_perc[[x]]$f.p0.05)
      #   m_fish$threshold <- lapply(m_fish$sample_id, function(x) m$mat_perc[[x]]$m.p0.05)
      # }
      # if(p_threshold == 0.95) {
      #   f_fish$threshold <- lapply(f_fish$sample_id, function(x) m$mat_perc[[x]]$f.p0.95)
      #   m_fish$threshold <- lapply(m_fish$sample_id, function(x) m$mat_perc[[x]]$m.p0.95)
      # }
    }
  }

  # browser()
  # classify each fish as immature or mature based on above thresholds
  f_fish <- mutate(f_fish, mature = if_else(length >= threshold, 1, 0, missing = NULL))
  m_fish <- mutate(m_fish, mature = if_else(length >= threshold, 1, 0, missing = NULL))

  # get unsexed immature fish
  imm_fish <- fish %>%
    filter(!(sex %in% c(1, 2)) &
      length < min(c(
        min(unique(unlist(f_fish$threshold)), na.rm = TRUE),
        min(unique(unlist(m_fish$threshold)), na.rm = TRUE)
      ), na.rm = TRUE)) %>%
    mutate(
      mature = 0,
      year_f = as.character(year)
    )

  # create groups
  if (split_by_sex) {
    if (immatures_pooled) {
      # since not splitting by sex for immatures, the unsexed imm can be added on
      fish_groups <- bind_rows(f_fish, m_fish, imm_fish) %>%
        mutate(group_name = ifelse(mature == 1,
          paste("Mature", ifelse(sex == 1, "males", "females")),
          "Immature"
        ))
    } else {
      fish_groups <- rbind(f_fish, m_fish) %>%
        mutate(group_name = ifelse(mature == 1,
          paste("Mature", ifelse(sex == 1, "males", "females")),
          paste("Immature", ifelse(sex == 1, "males", "females"))
        ))
    }
  } else {
    fish_groups <- rbind(f_fish, m_fish, imm_fish) %>%
      mutate(group_name = ifelse(mature == 1, "Mature", "Immature"))
  }
} else {
  # just split by sex
  fish_groups <- rbind(f_fish, m_fish) %>%
    mutate(group_name = ifelse(sex == 1, "Males", "Females"))
}

# 4. Calculate ‘weights' (sample multiplier for each fish sampled)
ds <- fish_groups %>%
  group_by(fishing_event_id, group_name) %>%
  mutate(
    log_depth = log(depth_m),
    group_sampled_weight = sum(weight, na.rm = T) / 1000,
    group_num_sampled = n()
  ) %>%
  ungroup() %>%
  group_by(fishing_event_id) %>%
  mutate(
    sampled_weight = sum(weight, na.rm = T) / 1000,
    num_sampled = n(),
    mean_weight = mean(weight, na.rm = T) / 1000,
    area_swept = ifelse(is.na(tow_length_m), doorspread_m * duration_min * speed_mpm, doorspread_m * tow_length_m),
    total_weight = ifelse(catch_weight > sampled_weight, catch_weight, sampled_weight),
    est_count = round(total_weight / mean_weight),
    est_count = ifelse(num_sampled > est_count, num_sampled, est_count),
    # catch_weight = mean(catch_weight),
    sample_multiplier_by_weight = 1 / (sampled_weight / total_weight), # this is total sampled
    # prop_n_not_sampled = 1 - (num_sampled/est_count), # this is est proportion by count not sampled
    prop_n_in_group = (group_num_sampled / num_sampled), # this is proportion by count
    prop_w_in_group = (group_sampled_weight / sampled_weight), # this is proportion by weight
    group_catch_weight = total_weight * prop_w_in_group, # this is est group catch weight
    est_num_unsampled_group_members = (est_count - num_sampled) * prop_n_in_group,
    sample_multiplier = 1 + (est_num_unsampled_group_members / group_num_sampled)
  ) %>%
  unique()

# investigate results
ds %>%
  select(
    year, survey_abbrev, fishing_event_id,
    latitude, latitude_end,
    longitude, longitude_end,
    depth_m, log_depth,
    usability_code, area_swept, density_kgpm2,
    group_name, length, sex, age, weight, specimen_id,
    wbar, cond_fac,
    catch_weight, total_weight,
    sampled_weight, num_sampled, group_num_sampled,
    prop_w_in_group, group_catch_weight,
    prop_n_in_group, est_count, est_num_unsampled_group_members, sample_multiplier
  ) %>%
  View()

hist(ds$sample_multiplier)
plot(log(ds$sample_multiplier_by_weight) ~ log(ds$sample_multiplier))

# save data
spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species_list)))

dir.create(paste0("data-generated/condition-data/"), showWarnings = FALSE)
saveRDS(ds, paste0("data-generated/condition-data/", spp, "mat-", mat_threshold, "-condition.rds"))