## Script for exploration and visualization of calorimetry results ----

# Written by:
# Chelsea L Faber
# Mirzadeh Lab at Barrow Neurological Institute

# Use this script to visually inspect cleaned calorimety data prior to inputting
# into the automated plotting and statistical analysis pipeline

## Load required packages ----------------

library(plyr, include.only = 'mapvalues')
library(tidyverse)
library(lubridate)

## User-defined parameters -----------------

cohort          <- "cal017"
rundate         <- "2023-01-11"
fpath           <- "C:/Users/cfaber/Dropbox (Barrow Neurological Institute)/Mirzadeh Lab Dropbox MAIN/Data/Calorimetry/macro_processed/r_cleaned"

## Load data -------------

fname           <- paste(rundate,cohort,sep = "_")

# Prompt user which .Rda should be loaded if a Copy exists
if (file.exists(paste0(fpath,"/",fname,"_Clean.Rda")) 
    & file.exists(paste0(fpath,"/",fname,"_Clean_COPY.Rda"))) {
  
  tmp <- menu(c("Original","Copy"), 
                title = "Two .Rda files found for this run. Which would you like to inspect?")
  if (tmp == 1) {
    f <- paste0(fpath,"/",fname,"_Clean.Rda")
  } else if (tmp == 2) {
    f <- paste0(fpath,"/",fname,"_Clean_COPY.Rda")
  }
}

# Load Clean.Rda

load(f)

## Summary --------------------------

# Desired metrics:
# - count unique subjects
# - number within each Treatment, Diet, Group, Sex
# - whether any intervention, diet, or light schedule change occurred
# - length of recording
# - 

## Quick time-series plots



## Identify outliers

## 