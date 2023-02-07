# wrangle all assessments together
devtools::load_all(".")
library(tidyverse)

gf <- readxl::read_xlsx("data/GF_assessments.xlsx") %>% filter(Outputs == "Y")

d <- list()

for (i in 1:nrow(gf)){

model <- case_when(gf$Model[i] == "iSCAM" ~ "iscam",
                   gf$Model[i] == "SS3" ~ "ss3",
                   .default = "rowans")

if (model == "rowans") {
  catchdata <- paste0(gf$folder[i], gf$fileprefix[i], "-MPD(C", gf$filesuffix[i])
  prefix <- paste0(gf$folder[i], gf$fileprefix[i], "-MCMC(")
  suffix <- gf$filesuffix[i]

} else{
  catchdata <- paste0(gf$folder[i], gf$catchfile[i])
  prefix <- paste0(gf$folder[i], gf$fileprefix[i])
  suffix <- gf$filesuffix[i]
}

print(paste(gf$Species[i],
      gf$Stock[i], model))

d[[i]] <- calc_prod(
  catchfile = catchdata,
  mcmcprefix = prefix,
  mcmcsuffix = suffix,
  gf$Species[i],
  gf$Stock[i],
  model_type = model,
  start_year = gf$Start[i],
  end_year = gf$End[i],
  recruitment_age = gf$AgeRecuits[i]
)

}

dat <- do.call(bind_rows, d)
