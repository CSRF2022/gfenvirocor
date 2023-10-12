#' helper functions
#' refine delta model
#' @export
refine_delta_model <- function(m, alternate_family = set_family2){
  s <- sanity(m)
  # browser()
  if (!s$range_ok) {
    m <- update(m, share_range = TRUE,
                spatial = as.list(m[["spatial"]]),
                spatiotemporal = as.list(m[["spatiotemporal"]]),
                extra_time = m$extra_time,
                data = m$data, family = m$family, mesh = m$spde)
    s <- sanity(m)
  }
  if (!s$hessian_ok & !s$nlminb_ok) {
    m <- update(m, family = alternate_family,
                spatial = as.list(m[["spatial"]]),
                spatiotemporal = as.list(m[["spatiotemporal"]]),
                extra_time = m$extra_time,
                data = m$data, mesh = m$spde)
    s <- sanity(m)
  }
  if (!s$se_magnitude_ok|!s$se_na_ok|!s$sigmas_ok) {
    m <- update(m, spatial = list("on", "off"),
                spatiotemporal = as.list(m[["spatiotemporal"]]),
                extra_time = m$extra_time,
                data = m$data, family = m$family, mesh = m$spde)
    s <- sanity(m)
  }
  if (!s$hessian_ok) {
    m <- update(m, family = alternate_family,
                spatial = as.list(m[["spatial"]]),
                spatiotemporal = as.list(m[["spatiotemporal"]]),
                extra_time = m$extra_time,
                data = m$data, mesh = m$spde)
    s <- sanity(m)
  } else {
    if (!s$se_magnitude_ok|!s$se_na_ok|!s$sigmas_ok) {
      m <- update(m, spatial = list("off", "off"),
                  spatiotemporal = as.list(m[["spatiotemporal"]]),
                  extra_time = m$extra_time,
                  data = m$data, family = m$family, mesh = m$spde)
      s <- sanity(m)
    }

    if (!s$se_magnitude_ok|!s$se_na_ok|!s$sigmas_ok) {
      m <- update(m, spatial = list("on", "off"),
                  spatiotemporal = list("off", "rw"),
                  extra_time = m$extra_time,
                  data = m$data, family = m$family, mesh = m$spde)
      s <- sanity(m)
    }

    if (!s$se_magnitude_ok|!s$se_na_ok) {
      m <- update(m,
                  spatial = "on",
                  spatiotemporal = "rw",
                  family = alternate_family,
                  share_range = FALSE,
                  extra_time = m$extra_time,
                  data = m$data, mesh = m$spde)
      s <- sanity(m)
    }
  }
  if(!all(s)){
    m <- update(m, share_range = TRUE,
                spatial = as.list(m[["spatial"]]),
                spatiotemporal = as.list(m[["spatiotemporal"]]),
                extra_time = m$extra_time,
                data = m$data, family = m$family, mesh = m$spde)
    s <- sanity(m)
  }
  if (!s$se_magnitude_ok|!s$se_na_ok) {
    m <- update(m, spatial = "off",
                spatiotemporal = as.list(m[["spatiotemporal"]]),
                extra_time = m$extra_time,
                data = m$data, family = m$family, mesh = m$spde)
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
    m <- update(m, share_range = TRUE,
                spatial = as.list(m[["spatial"]]),
                spatiotemporal = as.list(m[["spatiotemporal"]]),
                extra_time = m$extra_time,
                data = m$data, family = m$family, mesh = m$spde)
    s <- sanity(m)
  }
  if (!s$se_magnitude_ok|!s$se_na_ok) {
    m <- update(m, spatial = "off",
                spatiotemporal = as.list(m[["spatiotemporal"]]),
                extra_time = m$extra_time,
                data = m$data, family = m$family, mesh = m$spde)
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
plot_index <- function(dat, extra_years = NULL, filename){
  if (!file.exists(filename)) {
    ind <- get_index(dat, bias_correct = TRUE)
    saveRDS(ind, filename)
  } else {
    ind <- readRDS(filename)
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
split_index_by_survey <- function(model, grid, species, model_name){

  grid <- filter(grid, year %in% c(sort(unique(model$data$year))))
# browser()
  p <- grid |>
    split(grid$survey) |>
    lapply(function(x) predict(model, re_form_iid = NA, newdata = x,
                               return_tmb_object = TRUE))
  i <- purrr::map_dfr(p, get_index, area = 4, .id = "survey")
  i$surveys <- paste0(unique(model$data$survey_type), collapse=", ")
  i$species <- species
  i$group <- model_name
  i$index <- paste0(i$group, "\n(", i$surveys, ")")
  i$model <- paste0(ifelse(length(model$family)==6, model$family[6],
    paste0(model$family[1],"(link = 'log')")), "\nspatial (",
    model[["spatial"]][1], ", ", model[["spatial"]][2], ")")

  saveRDS(i, paste0("data-generated/density-split-ind/temp-index-split-",
                    gsub(" ", "-", gsub("\\/", "-", tolower(species))), "-",
                    gsub(" ", "-", model_name), ".rds"))

  return(i)
}



#'
#' @export
#'
map_density <- function(dat, filename, variable = "density_trimmed",
                        col_trans = fourth_root_power_trans()
) {

 if(!is(dat, "data.frame")){
  if (length(dat$fit_obj$family$family)>1) {
    p1 <- dat$data %>% mutate(
      density = dat$fit_obj$family[[1]]$linkinv(est1) * dat$fit_obj$family[[2]]$linkinv(est2),
      density_trimmed = ifelse(density > quantile(density, 0.995),
                               quantile(density, 0.995),
                               density))
  } else {
    p1 <- dat$data %>% mutate(density = dat$fit_obj$family$linkinv(est),
                              density_trimmed = ifelse(density > quantile(density, 0.995),
                                                       quantile(density, 0.995),
                                                       density))
  }
   saveRDS(p1, filename)

 } else {
   p1 <- dat
 }
  ggplot(p1, aes_string("X", "Y", colour = variable, fill = variable)) +
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
