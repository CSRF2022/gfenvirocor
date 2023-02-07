# # rowans other spp
# I have additional model outputs that I've not processed yet, but will get to them at some point:
# Canary Rockfish in 2022 using SS3
# Yellowmouth Rockfish in 2021 using SS3
# Walleye Pollock (WAP) in 2017 using iSCAM
# Shortspine Thornyhead (SST) in 2015 using iSCAM

library(tidyverse)

# should work for most outputs provided by Rowan
model_type <- "rowans"
mcmcsuffix <- ")-forRobyn.csv"
recruitment_age <- 1

# Pacific Ocean Perch (POP) 3CD and 5DE in 2012
folder <- "../rowan/POP-3CD+5DE-2012-Awatea-output-forRobyn-230124/"
fileprefix <- "POP-3CD-2012"
species <- "Pacific Ocean Perch"
stock <- "3CD"

folder <- "../rowan/POP-3CD+5DE-2012-Awatea-output-forRobyn-230124/"
fileprefix <- "POP-5DE-2012"
species <- "Pacific Ocean Perch"
stock <- "5DE"

# Pacific Ocean Perch (POP) 5ABC in 2017
folder <- "../rowan/POP-5ABC-2017-Awatea-output-forRobyn-230123/"
fileprefix <- "POP-5ABC-2017"
species <- "Pacific Ocean Perch"
stock <- "5ABC"

# Yellowtail Rockfish (YTR) in 2014
folder <- "../rowan/YTR-CST-2014-Awatea-output-forRobyn-230124/"
fileprefix <- "YTR-CST-2014"
species <- "Yellowtail Rockfish"
stock <- "Coastwide"

# Silvergray (SGR) in 2013
folder <- "../rowan/SGR-CST-2013-Awatea-output-forRobyn-230124/"
fileprefix <- "SGR-CST-2013"
species <- "Silvergray Rockfish"
stock <- "Coastwide"

# Redstripe Rockfish (RSR)in 2018 (not a composite, one run each)
# lag between recruitment and production interesting
folder <- "../rowan/RSR-BCN-BCS-2018-Awatea-output-forRobyn-230120/"
fileprefix <- "RSR-BCN-2018"
species <- "Redstripe Rockfish"
stock <- "BC North"

folder <- "../rowan/RSR-BCN-BCS-2018-Awatea-output-forRobyn-230120/"
fileprefix <- "RSR-BCS-2018"
species <- "Redstripe Rockfish"
stock <- "BC South"

# ROUGHEYE/BLACKSPOTTED ROCKFISH (REBS, BSR, RER)
folder <- "../rowan/REBS-North-2020-Awatea-output-forRobyn-230120/"
fileprefix <- "BSR-CST-2020"
species <- "Rougheye/Blackspotted Complex"
stock <- "BC North"

folder <- "../rowan/REBS-South-2020-Awatea-output-forRobyn-230120/"
fileprefix <- "RER-CST-2020"
species <- "Rougheye/Blackspotted Complex"
stock <- "BC South"

# Widow Rockfish
folder <- "../rowan/WWR-CST-2019-Awatea-output-forRobyn-230120/"
fileprefix <- "WWR-CST-2019"
species <- "Widow Rockfish"
stock <- "Coastwide"

# # Rock Sole (ROL) 5AB and 5CD in 2013
folder <- "../rowan/ROL-5AB+5CD-2013-Awatea-output-forRobyn-230124/"
fileprefix <- "ROL-5AB-2013"
species <- "Southern Rock Sole"
stock <- "5AB"

folder <- "../rowan/ROL-5AB+5CD-2013-Awatea-output-forRobyn-230124/"
fileprefix <- "ROL-5CD-2013"
species <- "Southern Rock Sole"
stock <- "5CD"

## Shortspine Thornyhead
folder <- "../rowan/SST-CST-2015-iSCAM-output-forRobyn-230203/"
fileprefix <- "SST-CST-2015"
species <- "Shortspine Thornyhead"
stock <- "Coastwide"

## Yellowmouth Rockfish
folder <- "../rowan/YMR-CST-2021-SS3-output-forRobyn-230202/"
fileprefix <- "YMR-CST-2021"
species <- "Yellowmouth Rockfish"
stock <- "Coastwide"

## Bocaccio
folder <- "../rowan/BOR-CST-2021-Awatea-output-forRobyn-230119/"
fileprefix <- "BOR-CST-2021"
species <- "Bocaccio"
stock <- "Coastwide"

## Canary Rockfish
folder <- "../rowan/CAR-CST-2022-SS3-output-forRobyn-230202/"
fileprefix <- "CAR-CST-2022"
species <- "Canary Rockfish"
stock <- "Coastwide"


df <- calc_prod(
  catchfile = paste0(folder, fileprefix, "-MPD(C", mcmcsuffix),
  mcmcprefix = paste0(folder, fileprefix, "-MCMC("),
  mcmcsuffix,
  species,
  stock,
  model_type = model_type,
  recruitment_age = recruitment_age
)

range(df$year)

plot(biomass~year, data = df)
plot(recruits~year, data = df)
plot(rdev~year, data = df)
plot(production~year, data = df)
plot(production1~year, data = df)

