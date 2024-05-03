# renv::init #initialize renv
# renv::activate() #activate renv
# synchronise packages from lock file if needed
# renv::restore()

# system("R")

options(scipken = 999) # turns of scientific notations

## set working directory
dir <- "~/workspaces/geiranger-emissions-analysis/output"
setwd(dir)

# load all dependencies
source("../dependencies.R")

## Input
# load data
load("../input-data/input2020_2023") # input

# adjust time (e.g. years to be included)
firstYear <- 2015
lastYear <- 2019
nYears <- lastYear - firstYear

## create constants
source("../src/constants.R")

## create functions
source("../src/services.R")

## run data preperation scripts
source("../src/data-preperation.R")
source("../src/get-time-in-port.R")
source("../src/aggregate-hourly-data.R")

# export <- input_daily[, -which(names(input_daily) %in% ship_names)]
# export$ferry <- input_daily$Ferry
# export$date.1 <- NULL
# fwrite(export, file = "../input-data/input_daily.csv.gz", compress = c("gzip"))
