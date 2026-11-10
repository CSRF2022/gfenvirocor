#' Summarise mcmc draws by year as a posterior median and 95% credible interval
#'
#' Unlike [awatea_mcmc()] and [iscam_mcmc()], which return medians only, this
#' keeps interval bounds. Fleet-specific columns for the same year are summed
#' within each draw before summarising.
#'
#' @param csv csv file location and name
#' @param format `"rowans"` for files with a draw label in the first column and
#'   column names like `1935` or `1935_1`; `"iscam"` for files with no draw
#'   label and column names like `sbt_1996` or `ft_fleet1_1996`
#' @param scale multiplier applied to all values, to convert units
#'
#' @return dataframe with columns year, value, upper, lower
#'
summarise_mcmc <- function(csv, format = c("rowans", "iscam"), scale = 1) {
  format <- match.arg(format)
  .d <- as.data.frame(readr::read_csv(
    csv,
    name_repair = "minimal", show_col_types = FALSE
  ))

  if (format == "rowans") {
    .d <- .d[, -1, drop = FALSE]
    year <- gsub("_.*", "", names(.d))
  } else {
    # same draws as iscam_mcmc()
    .d <- .d[1001:2000, , drop = FALSE]
    year <- gsub(".*_", "", names(.d))
  }

  .y <- rowsum(t(as.matrix(.d)), group = as.integer(year))
  .q <- apply(.y, 1, stats::quantile, probs = c(0.5, 0.975, 0.025), na.rm = TRUE)

  data.frame(
    year = as.integer(rownames(.y)),
    value = .q[1, ] * scale,
    upper = .q[2, ] * scale,
    lower = .q[3, ] * scale
  )
}

#' Summarise a reference point from mcmc draws
#'
#' @param x vector of draws
#' @param name prefix for the returned columns, `"lrp"` or `"trp"`
#'
#' @return one row dataframe
#'
summarise_ref_point <- function(x, name) {
  .q <- stats::quantile(x, probs = c(0.5, 0.975, 0.025), na.rm = TRUE)
  .d <- data.frame(.q[1], .q[2], .q[3], row.names = NULL)
  names(.d) <- paste0(name, c("", "_uncertainty_upper", "_uncertainty_lower"))
  .d
}

#' Get limit and upper reference points supplied with assessment outputs
#'
#' @param rp_source where the reference points come from: `"RP"` for `LRP` and
#'   `USR` in an MCMC(RP) file, `"HRP"` for `Bmin` and `Bavg` in an MCMC(HRP)
#'   file, `"bmsy"` for 0.4 and 0.8 Bmsy from an MCMC(P) file, `"sbo"` for 0.2
#'   and 0.4 SB0 from an iscam_mcmc.csv file, and `NA` when none are available
#' @param folder folder containing the outputs
#' @param fileprefix stock prefix for Rowan's files
#' @param filesuffix file name suffix for Rowan's files
#' @param scale multiplier applied to all values, to convert units
#'
#' @return one row dataframe, or `NULL` when `rp_source` is `NA`
#'
get_ref_points <- function(rp_source, folder, fileprefix, filesuffix, scale = 1) {
  if (is.na(rp_source)) {
    return(NULL)
  }

  read <- function(f) {
    readr::read_csv(f, name_repair = "minimal", show_col_types = FALSE)
  }
  rowan_file <- function(x) {
    paste0(folder, fileprefix, "-MCMC(", x, filesuffix)
  }

  if (rp_source == "RP") {
    .d <- read(rowan_file("RP"))
    lrp <- .d$LRP
    trp <- .d$USR
  }
  if (rp_source == "HRP") {
    .d <- read(rowan_file("HRP"))
    lrp <- .d$Bmin
    trp <- .d$Bavg
  }
  if (rp_source == "bmsy") {
    .d <- read(rowan_file("P"))
    lrp <- 0.4 * .d$bmsy
    trp <- 0.8 * .d$bmsy
  }
  if (rp_source == "sbo") {
    .d <- read(paste0(folder, "iscam_mcmc.csv"))[1001:2000, ]
    lrp <- 0.2 * .d$sbo
    trp <- 0.4 * .d$sbo
  }

  cbind(
    summarise_ref_point(lrp * scale, "lrp"),
    summarise_ref_point(trp * scale, "trp")
  )
}

#' Add interval columns for one variable using the assess template column names
#'
#' @param d dataframe with columns value, upper, and lower
#' @param name template variable name
#'
#' @return dataframe with year and the template columns for this variable
#'
name_assess_var <- function(d, name) {
  has_ci <- !is.na(d$upper)
  .d <- data.frame(
    year = d$year,
    value = d$value,
    upper = d$upper,
    lower = d$lower,
    level = ifelse(has_ci, 95L, NA_integer_),
    type = ifelse(has_ci, "credible interval", NA_character_)
  )
  names(.d) <- c(
    "year", name,
    paste0(name, "_uncertainty_", c(
      "upper", "lower", "interval_level", "interval_type"
    ))
  )
  .d
}

#' Get annual stock assessment outputs in tonnes and thousands of recruits
#'
#' Unlike [calc_prod()], this keeps credible intervals, includes all years with
#' estimates, sums harvest rates across fleets, and does not scale catch by
#' `prop_catch`.
#'
#' @param model_type which stock assessment platform was used to produce mcmc
#'   outputs: `"rowans"`, `"iscam"`, or `"landmark"`
#' @param folder folder containing the outputs
#' @param fileprefix file name prefix
#' @param filesuffix file name suffix
#' @param catchfile catch file location and name
#' @param biomass_scale multiplier converting biomass and catch to tonnes and
#'   recruits to thousands of fish
#'
#' @return dataframe of template columns, one row per year
#'
get_assess_outputs <- function(model_type, folder, fileprefix, filesuffix,
                               catchfile, biomass_scale = 1) {
  if (model_type == "landmark") {
    # only annual medians were provided; SSB in kt and recruits in millions
    .d <- readr::read_csv(catchfile, show_col_types = FALSE)
    na_col <- rep(NA_real_, nrow(.d))
    b <- data.frame(year = .d$Year, value = .d$SSBt_p50 * 1000, upper = na_col, lower = na_col)
    r <- data.frame(year = .d$Year, value = .d$Rt_p50 * 1000, upper = na_col, lower = na_col)
    u <- NULL
    catch <- data.frame(year = .d$Year, total_catch = .d$Landings)
  }

  if (model_type == "rowans") {
    f <- function(x) paste0(folder, fileprefix, "-MCMC(", x, filesuffix)
    b <- summarise_mcmc(f("B"), "rowans", biomass_scale)
    r <- summarise_mcmc(f("R"), "rowans", biomass_scale)
    u <- summarise_mcmc(f("U"), "rowans")

    if (file.exists(catchfile)) {
      .c <- as.data.frame(readr::read_csv(
        catchfile,
        name_repair = "minimal", show_col_types = FALSE
      ))
      catch <- data.frame(
        year = as.integer(.c[[1]]),
        total_catch = rowSums(.c[, -1, drop = FALSE]) * biomass_scale
      )
    } else {
      catch <- data.frame(year = b$year, total_catch = NA_real_)
    }
  }

  if (model_type == "iscam") {
    f <- function(x) paste0(folder, fileprefix, x, filesuffix)
    b <- summarise_mcmc(f("sbt"), "iscam", biomass_scale)
    r <- summarise_mcmc(f("rt"), "iscam", biomass_scale)
    # not all iscam output folders include fishing mortality
    u <- if (file.exists(f("ft"))) summarise_mcmc(f("ft"), "iscam") else NULL

    .c <- readr::read_csv(catchfile, show_col_types = FALSE)
    catch <- data.frame(year = .c$year, total_catch = .c$catch * biomass_scale)
  }

  out <- name_assess_var(b, "total_spawning_abundance") %>%
    dplyr::full_join(name_assess_var(r, "total_recruits"), by = "year") %>%
    dplyr::full_join(catch, by = "year")

  if (!is.null(u)) {
    out <- dplyr::full_join(out, name_assess_var(u, "fishing_mortality_rate"), by = "year")
  }

  dplyr::arrange(out, year)
}
