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
fpath           <- "C:/Users/kaspe/Dropbox (Barrow Neurological Institute)/Mirzadeh Lab Dropbox MAIN/Data/Calorimetry/macro_processed/r_cleaned"

## Load data -------------

fname           <- paste(rundate,cohort,sep = "_")
code            <- paste(rundate,cohort,"KEY",sep = "_")

# Load csv with run metadata
key             <- read_csv(paste(fpath,paste(code,".csv",sep = ""),sep="/"),show_col_types=F)

if (file.exists(savename)) {
  go <- menu(c("Re-run and overwrite existing Clean.Rda file","Re-run and save a new copy of Clean.Rda","Abort! Abort!"), title = "A .Rda file for this file already exists. How would you like to proceed?")
}

# Load cal.csv and merge metadata
df              <- read_csv(paste(fpath,paste(fname,".csv",sep = ""),sep="/"),show_col_types = F) %>%
  left_join(key, by = "Animal") %>%                 #unblind by merging with decoding df 
  rename(Cage = Animal,
         Animal = ID_Code)%>%
  select(!(starts_with("Enviro") 
           | starts_with("Ped") 
           | all_of(cols2excl))) %>%
  mutate(across(c(Animal,Sex,Cage,Group,Cohort,Treatment), factor))

## Summary --------------------------



## Quick time-series plots



## Identify outliers

## 