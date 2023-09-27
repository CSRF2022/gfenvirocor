#' helper functions
#' refine delta model
#' @export
refine_delta_model <- function(m){
  s <- sanity(m)
  # browser()
  if (!s$range_ok) {
    m <- update(m, share_range = TRUE)
    s <- sanity(m)
  }
  if (!s$hessian_ok & !s$nlminb_ok) {
    m <- update(m, family = set_family2)
    s <- sanity(m)
  }
  if (!s$se_magnitude_ok|!s$se_na_ok|!s$sigmas_ok) {
    m <- update(m, spatial = list("on", "off"))
    s <- sanity(m)
  }
  if (!s$se_magnitude_ok|!s$se_na_ok|!s$sigmas_ok) {
    m <- update(m, spatial = list("off", "off"))
    s <- sanity(m)
  }

  if (!s$se_magnitude_ok|!s$se_na_ok|!s$sigmas_ok) {
    m <- update(m, spatial = list("on", "off"), spatiotemporal = c("off", "rw"))
    s <- sanity(m)
  }

  if (!s$se_magnitude_ok|!s$se_na_ok) {
    m <- update(m,
                spatial = "on",
                spatiotemporal = "rw",
                family = set_family2,
                share_range = FALSE)
    s <- sanity(m)
  }
  if(!s$range_ok){
    m <- update(m, share_range = TRUE)
    s <- sanity(m)
  }
  if (!s$se_magnitude_ok|!s$se_na_ok) {
    m <- update(m, spatial = "off")
    s <- sanity(m)
  }
  if(!s$gradients_ok){
    m <- run_extra_optimization(m)
    s <- sanity(m)
  }
  sanity(m)
  return(m)
}

#' refine regular model
#' @export
#'
refine_model <- function(m){
  s <- sanity(m)
  # browser()
  # if(!s$gradients_ok){
  #   m <- run_extra_optimization(m)
  #   s <- sanity(m)
  # }
  if(!s$se_magnitude_ok|!s$se_na_ok){
    m <- update(m, share_range = TRUE)
    s <- sanity(m)
  }
  if (!s$se_magnitude_ok|!s$se_na_ok) {
    m <- update(m, spatial = "off")
    s <- sanity(m)
  }
  if(!s$gradients_ok){
    m <- run_extra_optimization(m)
    s <- sanity(m)
  }
  sanity(m)
  return(m)
}


#'
#' @export
#'
plot_index <- function(dat, type) {
  f <- paste0("data-generated/density-index/", spp, "-p-", type, dens_model_name, "-", knot_distance, "-km.rds")
  if (!file.exists(f)) {
    dir.create(paste0("data-generated/density-index/"), showWarnings = FALSE)
    ind <- get_index(dat, bias_correct = FALSE)
    saveRDS(ind, f)
  } else {
    ind <- readRDS(f)
  }

  if(!is.null(extra_years)) {
    ind <- filter(ind, !(year %in% extra_years))
  }

  ggplot(ind, aes(year, est)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
    xlab("Year") +
    ylab("Biomass estimate (kg)")
}


#'
#' @export
#'
map_density <- function(dat, type, col_trans = fourth_root_power_trans()
) {

  if (length(dat$fit_obj$family) == 6) {
    p1 <- dat$data %>% mutate(density = plogis(est1) * exp(est2))
  } else {
    p1 <- dat$data %>% mutate(density = exp(est))
  }

  dir.create(paste0("data-generated/density-predictions/"), showWarnings = FALSE)
  saveRDS(p1, paste0("data-generated/density-predictions/", spp, "-p-", type,
                     dens_model_name, "-", knot_distance, "-km.rds"))

  ggplot(p1, aes(X, Y, colour = density, fill = density)) +
    geom_tile(width = 2, height = 2, alpha = 1) +
    facet_wrap(~year) +
    scale_fill_viridis_c(trans = col_trans) +
    scale_colour_viridis_c(trans = col_trans) +
    labs(x = "", y = "")
}


get_wraper <- function(x) {
  lapply(strwrap(x, width = 30, simplify = FALSE), paste, collapse="\n")
}


## old version of delta function
# refine_dg_model <- function(m){
#   s <- sanity(m)
#   # browser()
#   if(!s$gradients_ok){
#     m <- run_extra_optimization(m)
#     s <- sanity(m)}
#   if (!s$se_magnitude_ok) {
#     m <- update(m,
#                 spatial = list("on", "off"),
#                 share_range = FALSE)
#     s <- sanity(m)
#     if(!s$gradients_ok){
#       m <- run_extra_optimization(m)
#       s <- sanity(m)}
#     if (!s$se_magnitude_ok) {
#       m <- update(m,
#                   spatial = "on",
#                   spatiotemporal = "rw",
#                   family = tweedie(),
#                   share_range = FALSE)
#       s <- sanity(m)
#       if(!s$se_magnitude_ok){
#         m <- update(m, share_range = TRUE)
#         s <- sanity(m)
#       }
#       if (!s$se_magnitude_ok) {
#         m <- update(m, spatial = "off")
#         s <- sanity(m)
#       }
#       if(!s$gradients_ok){
#         m <- run_extra_optimization(m)
#         s <- sanity(m)}
#     }
#   }
#   sanity(m)
#   return(m)
# }
