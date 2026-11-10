# format stock assessment outputs to match the TESA draft_assess template
# uses the same assessment list and output locations as 02-get-all-productivity
devtools::load_all(".")
library(tidyverse)

template_dir <- "TESA_assess_data/templates/draft_assess/"
output_dir <- "TESA_assess_data/outputs/pacific_groundfish/"
assess_dir <- "../Assess_outputs/"

contact_name <- "Philina English"
contact_email <- "philina.english@dfo-mpo.gc.ca"

# TRUE drops stocks that are not in stock_species_region_list.csv
listed_stocks_only <- FALSE

# both versions of Pacific Cod have the same stock labels and only one can be
# reported per stock, so choose by age at recruitment
pcod_age_recruited <- 2


# stock information not in GF_assessments.xlsx ----------------------------

# major_stock_name matches stock_species_region_list.csv where the stock is
# listed. stock_code is a placeholder used to build major_stock_id and
# assessment_id, because the list does not contain ids yet.
# rp_source is described in get_ref_points()
stocks <- tribble(
  ~Species, ~Stock, ~major_stock_name, ~stock_code, ~scientific_name, ~rp_source,
  "Arrowtooth Flounder", "Coastwide", "Arrowtooth flounder", "ARF-CST", "Atheresthes stomias", "sbo",
  "Southern Rock Sole", "5AB", "Southern rock sole 5AB", "ROL-5AB", "Lepidopsetta bilineata", NA,
  "Southern Rock Sole", "5CD", "Southern rock sole 5CD", "ROL-5CD", "Lepidopsetta bilineata", NA,
  "Canary Rockfish", "Coastwide", "Canary rockfish", "CAR-CST", "Sebastes pinniger", "RP",
  "Pacific Ocean Perch", "3CD", "Pacific ocean perch West Coast of Vancouver Island", "POP-3CD", "Sebastes alutus", NA,
  "Pacific Ocean Perch", "5ABC", "Pacific ocean perch Queen Charlotte Sound", "POP-5ABC", "Sebastes alutus", NA,
  "Pacific Ocean Perch", "5DE", "Pacific ocean perch North and West Coasts of Haida Gwaii", "POP-5DE", "Sebastes alutus", NA,
  "Bocaccio", "Coastwide", "Bocaccio rockfish", "BOR-CST", "Sebastes paucispinis", NA,
  "Redstripe Rockfish", "5DE", "Redstripe rockfish 5DE", "RSR-BCN", "Sebastes proriger", NA,
  "Redstripe Rockfish", "3CD5ABC", "Redstripe rockfish 3CD5ABC", "RSR-BCS", "Sebastes proriger", NA,
  "Rougheye/Blackspotted", "5DE", "Rougheye/Blackspotted rockfish 5DE", "BSR-CST", "Sebastes aleutianus/melanostictus", NA,
  "Rougheye/Blackspotted", "3CD5AB", "Rougheye/Blackspotted rockfish 3CD5AB", "RER-CST", "Sebastes aleutianus/melanostictus", NA,
  "Shortspine Thornyhead", "Coastwide", "Shortspine thornyhead", "SST-CST", "Sebastolobus alascanus", "bmsy",
  "Silvergray Rockfish", "Coastwide", "Silvergray rockfish", "SGR-CST", "Sebastes brevispinis", NA,
  "Widow Rockfish", "Coastwide", "Widow rockfish", "WWR-CST", "Sebastes entomelas", NA,
  "Yellowmouth Rockfish", "Coastwide", "Yellowmouth rockfish", "YMR-CST", "Sebastes reedi", "RP",
  "Yellowtail Rockfish", "Coastwide", "Yellowtail rockfish", "YTR-CST", "Sebastes flavidus", NA,
  "Sablefish", "Coastwide", "Sablefish", "SBF-CST", "Anoplopoma fimbria", NA,
  "Pacific Cod", "3CD", "Pacific cod 3CD", "PCOD-3CD", "Gadus macrocephalus", NA,
  "Pacific Cod", "5ABCD", "Pacific cod 5ABCD", "PCOD-5ABCD", "Gadus macrocephalus", NA,
  "Walleye Pollock", "5CDE", "Walleye pollock 5CDE", "WAP-North", "Gadus chalcogrammus", "HRP",
  "Walleye Pollock", "3CD5AB", "Walleye pollock 3CD5AB", "WAP-South", "Gadus chalcogrammus", "HRP"
)

rp_notes <- c(
  RP = "LRP and USR are from the MCMC(RP) output; USR is entered as trp.",
  HRP = "LRP is Bmin and USR is Bavg from the MCMC(HRP) output; USR is entered as trp.",
  bmsy = "LRP is 0.4 Bmsy and USR is 0.8 Bmsy, calculated from posterior Bmsy; USR is entered as trp.",
  sbo = "LRP is 0.2 SB0 and USR is 0.4 SB0, calculated from posterior SB0; USR is entered as trp."
)


# set up stock table ------------------------------------------------------

gf <- readxl::read_xlsx("data/GF_assessments.xlsx") %>%
  filter(Outputs == "Y") %>%
  filter(Species != "Pacific Cod" | age_recruited == pcod_age_recruited) %>%
  mutate(folder = paste0(assess_dir, gsub("^\\.\\./", "", folder))) %>%
  left_join(stocks, by = c("Species", "Stock")) %>%
  # the 2024 yellowtail assessment supplied reference points but the 2014 one did not
  mutate(rp_source = if_else(fileprefix %in% "YTR-BC-2024", "RP", rp_source))

stopifnot(!any(is.na(gf$stock_code)))

stock_list <- read_csv(paste0(template_dir, "stock_species_region_list.csv"),
  col_types = cols(major_stock_id = "c", assessment_year = "i", .default = "c")
)

gf <- gf %>% mutate(
  listed = major_stock_name %in% stock_list$major_stock_name,
  model_type = case_when(
    model == "iSCAM" ~ "iscam",
    model == "landmark" ~ "landmark",
    .default = "rowans"
  ),
  model_label = gsub("\\(RH\\)|\\?", "", model),
  # arrowtooth biomass and catch are in kt and recruits are in millions
  biomass_scale = if_else(Species == "Arrowtooth Flounder", 1000, 1)
)

if (listed_stocks_only) {
  gf <- filter(gf, listed)
}


# retrieve outputs --------------------------------------------------------

major_stock <- list()
assessments <- list()

for (i in 1:nrow(gf)) {
  print(paste(gf$Species[i], gf$Stock[i], gf$model_type[i]))

  if (gf$model_type[i] == "rowans") {
    catchdata <- paste0(gf$folder[i], gf$fileprefix[i], "-MPD(C", gf$filesuffix[i])
    catch_names <- if (file.exists(catchdata)) {
      names(read_csv(catchdata, n_max = 0, show_col_types = FALSE))
    } else {
      NA_character_
    }
  } else {
    catchdata <- paste0(gf$folder[i], gf$catchfile[i])
    catch_names <- NA_character_
  }

  d <- get_assess_outputs(
    model_type = gf$model_type[i],
    folder = gf$folder[i],
    fileprefix = gf$fileprefix[i],
    filesuffix = gf$filesuffix[i],
    catchfile = catchdata,
    biomass_scale = gf$biomass_scale[i]
  )

  rp <- get_ref_points(
    gf$rp_source[i], gf$folder[i], gf$fileprefix[i], gf$filesuffix[i],
    gf$biomass_scale[i]
  )

  if (!is.null(rp)) {
    d <- cbind(d, rp) %>% mutate(
      lrp_units = "tonnes",
      trp_units = "tonnes",
      lrp_uncertainty_interval_level = 95L,
      trp_uncertainty_interval_level = 95L,
      lrp_uncertainty_interval_type = "credible interval",
      trp_uncertainty_interval_type = "credible interval"
    )
  }

  # Rowan's file names carry the year of the assessment that produced them.
  # Otherwise use the latest of the listed assessment and the outputs, because
  # some listed years predate the outputs supplied
  file_year <- if (gf$model_type[i] == "rowans") {
    as.integer(str_extract(gf$fileprefix[i], "[0-9]{4}$"))
  } else {
    max(gf$Assessed[i], d$year[!is.na(d$total_catch)])
  }

  uses_harvest_rate <- gf$model_type[i] == "rowans"
  has_f <- "fishing_mortality_rate" %in% names(d)
  has_catch <- any(!is.na(d$total_catch))

  notes <- c(
    if (gf$model_type[i] == "landmark") {
      "Values are medians; intervals were not available."
    } else {
      "Values are posterior medians with 2.5 and 97.5 percent credible limits."
    },
    if (uses_harvest_rate) {
      "fishing_mortality_rate is annual harvest rate U, summed across fleets within each draw."
    },
    if (gf$model_type[i] == "iscam" && has_f) {
      "fishing_mortality_rate is instantaneous F, summed across fleets within each draw."
    },
    if (gf$prop_catch[i] == 0.5) {
      "total_spawning_abundance is female spawning biomass."
    },
    if ("female" %in% catch_names) {
      "total_catch is female catch as supplied to the model."
    },
    if (!has_catch) "No catch file was supplied with these outputs.",
    if (!is.na(gf$rp_source[i])) rp_notes[[gf$rp_source[i]]]
  )

  major_stock[[i]] <- d %>% mutate(
    assessment_id = paste0(gf$stock_code[i], "-", file_year),
    major_stock_id = gf$stock_code[i],
    major_stock_name = gf$major_stock_name[i],
    species_common_name = gf$Species[i],
    species_scientific_name = gf$scientific_name[i],
    dfo_region = "Pacific",
    assessment_year = file_year,
    total_catch_units = if (has_catch) "tonnes" else NA_character_,
    total_spawning_abundance_units = "tonnes",
    total_recruits_units = paste0("thousands of age ", gf$age_recruited[i], " fish"),
    contact = paste0(contact_name, " <", contact_email, ">"),
    comments = paste(notes, collapse = " ")
  )

  assessments[[i]] <- tibble(
    assessment_id = paste0(gf$stock_code[i], "-", file_year),
    major_stock_id = gf$stock_code[i],
    major_stock_name = gf$major_stock_name[i],
    assessment_year = file_year,
    assessment_type = gf$model_label[i],
    # same stock, so the 2024 assessment replaces the 2014 one in the template
    supersedes_assessment_id = if (gf$fileprefix[i] %in% "YTR-BC-2024") "YTR-CST-2014" else NA_character_,
    submitter_email = contact_email,
    status_notes = paste0(
      "Listed in GF_assessments.xlsx as assessed ", gf$Assessed[i],
      ", SAR ", gf$SAR[i], ", Res Doc ", gf$`Res Doc`[i], "."
    )
  )
}


# match template columns --------------------------------------------------

# read header as text because major_stock.csv has an unnamed column before rr
read_template_columns <- function(file) {
  header <- gsub("^\ufeff", "", readLines(file, n = 1, warn = FALSE, encoding = "UTF-8"))
  strsplit(header, ",", fixed = TRUE)[[1]]
}

match_template <- function(d, columns) {
  stopifnot(all(names(d) %in% columns))
  out <- lapply(columns, function(x) {
    if (x %in% names(d)) d[[x]] else rep(NA, nrow(d))
  })
  out <- as.data.frame(out, optional = TRUE)
  names(out) <- columns
  out
}

major_stock_out <- bind_rows(major_stock) %>%
  mutate(across(where(is.double), ~ signif(.x, 6))) %>%
  match_template(read_template_columns(paste0(template_dir, "data/major_stock.csv")))

assessments_out <- bind_rows(assessments) %>%
  match_template(read_template_columns(paste0(template_dir, "metadata/assessments.csv")))

stock_list_out <- stock_list %>%
  rows_update(
    bind_rows(assessments) %>%
      filter(major_stock_name %in% stock_list$major_stock_name) %>%
      select(major_stock_name, major_stock_id, assessment_year),
    by = "major_stock_name"
  )


# checks ------------------------------------------------------------------

required <- c(
  "assessment_id", "major_stock_id", "major_stock_name", "species_common_name",
  "species_scientific_name", "dfo_region", "assessment_year", "year", "contact"
)
stopifnot(!anyNA(major_stock_out[required]))
stopifnot(!anyDuplicated(major_stock_out[c("assessment_id", "year")]))
stopifnot(!anyDuplicated(assessments_out$assessment_id))

for (v in c("total_spawning_abundance", "total_recruits", "fishing_mortality_rate", "lrp", "trp")) {
  estimate <- major_stock_out[[v]]
  stopifnot(
    all(estimate <= major_stock_out[[paste0(v, "_uncertainty_upper")]], na.rm = TRUE),
    all(estimate >= major_stock_out[[paste0(v, "_uncertainty_lower")]], na.rm = TRUE)
  )
}


# save --------------------------------------------------------------------

# start from a copy of the template so the metadata and sub_population files
# are included; sub_population stays empty as no stocks have subpopulation data
files <- list.files(template_dir, recursive = TRUE)
dir.create(paste0(output_dir, "data"), recursive = TRUE, showWarnings = FALSE)
dir.create(paste0(output_dir, "metadata"), recursive = TRUE, showWarnings = FALSE)
file.copy(paste0(template_dir, files), paste0(output_dir, files), overwrite = TRUE)

write_csv(major_stock_out, paste0(output_dir, "data/major_stock.csv"), na = "")
write_csv(assessments_out, paste0(output_dir, "metadata/assessments.csv"), na = "")
write_csv(stock_list_out, paste0(output_dir, "stock_species_region_list.csv"), na = "")

# review which stocks are missing from the template list or from each variable
bind_rows(major_stock) %>%
  group_by(major_stock_name, assessment_id) %>%
  summarise(
    years = paste(min(year), max(year), sep = "-"),
    catch = sum(!is.na(total_catch)),
    spawning = sum(!is.na(total_spawning_abundance)),
    recruits = sum(!is.na(total_recruits)),
    mortality = sum(!is.na(fishing_mortality_rate)),
    lrp = any(!is.na(lrp)),
    .groups = "drop"
  ) %>%
  mutate(listed = major_stock_name %in% stock_list$major_stock_name) %>%
  print(n = Inf, width = Inf)
