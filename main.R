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



## run data preperation scripts
