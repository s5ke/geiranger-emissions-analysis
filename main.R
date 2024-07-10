# renv::init #initialize renv
# renv::activate() #activate renv
# renv::install() # install packages
# renv::remove('') # remove packages
# synchronise packages from lock file if needed
# renv::restore()
# renv::snapshot() # update the lockfile.

 system("R")

options(scipken = 999) # turns of scientific notations

## set working directory
dir <- "~/Promotion/Research/Papers/01_PM_2015_2019/02STOTEN/R_Svenja_github/geiranger-emissions-analysis/output"
setwd(dir)

# load all dependencies
source("../dependencies.R")

## Input
# load data
load("../input-data/input2015_2023") # input

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
