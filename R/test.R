#' Testing
#'
#' @param x
#'
#' @return x
#' @export
#'
#' @examples testing()
testing <- function(x = "test"){
  print(x)
}


# iscam_out
# QCS <- read_csv("data/iscam_sbt_mcmc_HS_QCS.csv")
# sbt1 <- QCS[1001:2000, ] %>% summarise_all(median)
# sbt <- t(sbt1)
# year <- 1956:2021
# d1 <- as.data.frame(sbt)
# d1 <- cbind(year, d1)
#
#
# d1 <- d1 %>% rename(biomass = V1)
