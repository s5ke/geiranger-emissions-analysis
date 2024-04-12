# renv::init #initialize renv
# renv::activate() #activate renv
# synchronise packages from lock file if needed
# renv::restore()

# system("R")

options(scipen = 999) # turns of scientific notations

## set working directory
dir <- "~/workspaces/geiranger-emissions-analysis/output" # set directory for output
setwd(dir)

# load all dependencies
source("../dependencies.R")

## Input
# load data
load("/home/svenja/workspaces/geiranger-emissions-analysis/input-data/input2020_2023") # input

# adjust time (e.g. years to be included)
firstYear <- 2015
lastYear <- 2020
nYears <- lastYear - firstYear

## create constants
source("../src/constants.R")

## create functions
source("../src/services.R")

## run data preperation scripts
source("../src/data-preperation.R")
