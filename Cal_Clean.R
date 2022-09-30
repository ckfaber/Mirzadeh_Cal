## Script for Tidying Sable Calorimetry Data ----------------------------------------

# Written by Chelsea Faber
# Mirzadeh Lab, Barrow Neurological Institute

# kasper.chelsea@gmail.com

# REQS:
# - .exp data files processed with MI v2.46 in 1-minute bins - all should be processed with the SAME MACRO
# - TimeSeries sheet as a .csv 
# - Group decoding sheet with same prefix as TimeSeries sheet (e.g., expID_date_code.csv), saved in same directory

# Known bugs: 
# - EnviroLightlux_M sometimes has 0s and 1s, sometimes has 1s and 3s, sometimes 3s and 7s
# - therefore photoperiod assignment is done based on ZT/clock time NOT from sensor
# - will need to be updated for DD or other light schedule experiments

# Next steps: 

# - improve hardcoding of file names/directories: back-slashes (/) are needed instead of (\)
#         like Windows provides when copy/pasting 
# - Convert this to function to call from another pre-processing script
# - read in appropriately named .csvs from directory without having to hard-code the file
# - improve experimental log for metadata read-in and association with cal data

## Load required packages ------------------------------------------------------

library(plyr)
library(tidyverse)
library(magrittr)
library(lubridate)
library(here)

## Directory hard-coding ------------------------------------------------------ 

#load_dir  <- "C:/Users/cfabe/Dropbox (Barrow Neurological Institute)/Mirzadeh Lab Dropbox MAIN/CLF/Data_Raw/Calorimetry/" 
ext       <- ".csv"
cohort    <- "mon001"
rundate   <- "2021-10-18"  

## Animals to remove -----------------------------------------------------------

remove_animals <- 274 
cols2excl <- c('Age','Diet','Cage','Stilltime_M',
               'Sleeptime_M','XBreak_R','YBreak_R',
               'month','day','Mass_g','AllMeters_M')

## Load data ------------------------------------------------------------------

filename  <- paste(rundate,cohort,sep = "_")
code      <- paste(rundate,cohort,"DECODED",sep = "_")

# Load csv with run metadata
df_code   <- read_csv(here::here(paste(code,".csv",sep = ""))) 

# Load cal.csv and merge metadata
df        <- read_csv(here::here(paste(filename,".csv",sep = ""))) %>%
  left_join(df_code, by = "Animal") %>%                 #unblind by merging with decoding df 
  mutate(across(.cols = everything()),na_if(.,".")) %>% #replace "." with "NA"
  rename(Cage = Animal)

## Tidy data -------------------------------------------------------------------

# Remove dead/sick animals
df %<>%
  filter(ID_Code != remove_animals)

# Parse date-times and sort by animal ID (ID_Code) and date-time
df %<>% 
  relocate(c(DateTime,Sex,Group,Treatment,Cohort,ID_Code)) %>%
  mutate(DateTime = round_date(ymd_hms(df$DateTime),unit = "minute")) %>%
  mutate(month = as.numeric(month(DateTime)),
         day = as.numeric(day(DateTime)),
         hour = as.numeric(hour(DateTime)),
         minute = as.numeric(minute(DateTime))) 

df <- df[order(df$DateTime),] %>%
  arrange(ID_Code)

## Transform data  ------------------------------------------------------------

# Compute experimental day and ZT time, rename some variables
df %<>%
  mutate(ZT = mapvalues(hour, from = (0:23), to = c(18:23,0:17)),.before = Sex) %>%     
  mutate(Photoperiod = as.factor(mapvalues(ZT, from = c(0:23), to = c(rep(1,12),rep(0,12)))),.before = ZT) %>%
  mutate(exp_day = mapvalues(day, from = unique(day), to = 1:length(unique(day))),.after = DateTime) %>%
  rename(VO2 = VO2_M, 
         Animal = ID_Code, 
         VCO2 = VCO2_M, 
         EE = kcal_hr_M, 
         RER = RER_M, 
         FoodIn.cum = FoodInA_M, 
         WaterIn.cum = WaterInA_M, 
         AllMeters = AllMeters_R,
         BodyMass = BodyMass_M,
         VH2O = VH2O_M)

# Compute hours from recording start for plots
start_time <- df$DateTime[1]
df %<>%
  mutate(Time = as.numeric(difftime(DateTime,start_time),units = "hours"),.after = DateTime)

# Compute binned measures, energy balance, deselect some extraneous columns
# TO DO: compute EE columns as kcal/bin

int <- df$Time[2] - df$Time[1]

df %<>%
  group_by(Animal) %>%
  mutate(FoodIn.g = c(diff(FoodIn.cum),0),.before = FoodIn.cum) %>%
  mutate(WaterIn.g = c(diff(WaterIn.cum),0),.before = WaterIn.cum) %>%
  mutate(FoodIn.kcal = FoodIn.g * 4.2, .before = FoodIn.g) %>%
  mutate(EE.kcal.bin = EE * int, .before = EE) %>%
  mutate(EBalance = FoodIn.kcal - EE.kcal.bin, .before = VO2) %>%
  ungroup() %>%
  select(!(starts_with("Enviro") 
           | starts_with("Ped") 
           | all_of(cols2excl)))

# Remove any rows/columns with only NAs
df <- df[rowSums(is.na(df)) != ncol(df), ]
df <- df[, colSums(is.na(df)) != nrow(df)]
View(df)

## Bin to hourly ---------------------------------------------------
cols2sum <- c('FoodIn.g','WaterIn.g','EBalance')
cols2avg <- c('VO2','VCO2','VH2O','EE','RER','BodyMass')
cols4cum <- c('AllMeters.cum','FoodIn.cum','WaterIn.cum','FoodIn.kcal')
cols2keep <- c('DateTime','Time','minute')

df.hourly <- df %>%
  group_by(Animal,exp_day,hour) %>%
  select(!EE.kcal.bin) %>%
  mutate(
    across(all_of(cols2keep),median)) %>% 
  mutate(across(
    all_of(cols2avg), mean)) %>%
  mutate(across(
    all_of(cols2sum),sum)) %>%
  mutate(across(
    all_of(cols4cum),max)) %>%
  ungroup() %>%
  distinct() %>%
  select(!c(hour,minute)) %>%
  mutate(Animal = as.factor(Animal)) %>%
  group_by(Animal) %>%
  slice(2:(n()-1)) %>% # trim incomplete hours at start and end
  mutate(Time = as.numeric(
    difftime(DateTime,DateTime[1]),units = "hours"),.after = DateTime) %>%
  ungroup()

## Overall photoperiod means ------------------------------------------------

se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

pp.averaged.total <- df %>%
  group_by(Photoperiod,Treatment,Animal) %>%
  summarize(
    across(all_of(cols2sum),sum,.names = "tot.{.col}"),
    across(all_of(cols2avg),mean,.names = "mean.{.col}"),
    across(all_of(cols4cum),max,.names = "tot.{.col}")) 

pp.averaged.daily <- df %>%
  group_by(exp_day,Photoperiod,Treatment,Animal) %>%
  summarize(
    across(all_of(cols2sum),sum,.names = "tot.{.col}"),
    across(all_of(cols2avg),mean,.names = "mean.{.col}"),
    across(all_of(cols4cum),max,.names = "tot.{.col}"))  

## Export to (optional) .csv, .Rda--------------------------------------------

setwd("C:/Users/cfaber/Dropbox (Barrow Neurological Institute)/Mirzadeh Lab Dropbox MAIN/CLF/Projects/mon/Outputs")
#write.csv(df, glue(cohort,"Clean.csv",.sep="_"),row.names = FALSE)
savename <- glue(cohort,rundate,"Clean.Rda",.sep = "_")
save(df, df.hourly, pp.averaged.total, pp.averaged.daily, file = savename)
