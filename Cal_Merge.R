## Script for merging independent calorimetry runs -----------------------------
# Written by Chelsea Faber

# Known bugs: 

#     - matching date-times only works when hour/minute align - need to either modify
#       macro to always begin bins at same time (e.g., first 18:00 that occurs) or
#       will have to round to 3" bins and slightly misrepresent data in one df

#     - plan to use 1" macro-binned data in future

# To Do: 

#       - can semi_join() be used to combine the datasets with fewer steps? It appears 
#         to result in a different result, doesn't include all the animals

#       - Convert this to function to call from another pre-processing script
#           - inputs to function:
#                 - load_dir,load_file1, load_file2, code_file1, code_file2
#           - instead of having same code duplicated for df1 and df2, loop it

# Reqs: - ...._Clean.Rda file from Cal_Clean.R script

## Load required packages ------------------------------------------------------

library(plyr)
library(tidyverse)
library(ViewPipeSteps)
library(lubridate)
library(glue)

## Directory hard-coding ------------------------------------------------------ 

setwd("C:/Users/chels/OneDrive - Barrow Neurological Institute/Project 4 - Maternal Overnutrition/Outputs")
load_dir <- "C:/Users/chels/OneDrive - Barrow Neurological Institute/Project 4 - Maternal Overnutrition/Outputs" 

# First df to be merged
cohort1 <- "mon001"   ; rundate1 <- "_2021_10_18" 
data1 <- glue(cohort1,rundate1,"_Clean.Rda")

# Second df to be merged
cohort2 <- "mon002"   ; rundate2 <- "_2021_11_03"
data2 <- glue(cohort2,rundate2,"_Clean.Rda")

## Read in data ----------------------------------------------------------------
df1 <- load(data1)
df2 <- load(data2)

## Aligning & merging data frames ----------------------------------------------

start1 <- date(head(df1$DateTime,1))
end1 <- date(tail(df1$DateTime,1))
length1 <- end1-start1

start2 <- date(head(df2$DateTime,1))
end2 <- date(tail(df2$DateTime,1))
length2 <- end2-start2

rec_lag <- as.integer(start2-start1)

# Shift df2 to align with df1 by date: 
if(setequal(df1$DateTime,df1$DateTime) & length1 == length2){
  print("DateTimes align, merging data frames into Merged_Data data frame")
  Merged_Data <- bind_rows(df1,df2,.id = "Df_ID")
  View(df_Merge)
} else {
  print("DateTimes do not align. Computing lag, shifting df1 onto df2, and trimming to match lengths")
  glue("Recording lag is {rec_lag} days. Subtracting {rec_lag} from df2 datetimes and creating new df2_shifted df")
  df2_shifted <- df2 
  df2_shifted %<>%
    mutate(DateTime = DateTime - days(rec_lag+1))
  View(df2_shifted)
}

# Find indices of matching date/times & create new data-frames with matched times
df1_match_idx <- match(df1$DateTime,df2_shifted$DateTime)
df2_match_idx <- match(df2_shifted$DateTime,df1$DateTime)

df1_matched <- df1[!is.na(df1_match_idx),]
df2_matched <- df2_shifted[!is.na(df2_match_idx),]

# BE JOINED IN HOLY MATRIMONY
if(length(unique(df1_matched$DateTime)) == length(unique(df2_matched$DateTime))) {
  print("DateTimes are properly matched, merging data-frames as df_Merge")
  Merged_Data <- bind_rows(df1_matched,df2_matched,.id = "OG_Df")
  View(Merged_Data)
} else {
  print("Shit, it's still not right. Ask Chelsea for help.")
  glue("df1 has {length(unique(df1_matched$DateTime))} unique datetimes, while df2 has {length(unique(df2_matched$DateTime))} unique datetimes.")
}

## Export to .csv, .Rda---------------------------------------------------------

setwd("C:/Users/chels/OneDrive - Barrow Neurological Institute/Project 4 - Maternal Overnutrition/Outputs")
merged_filename <- glue(data1,data2,"Combined_Runs",.sep = "_")
#write.csv(df_Merge, glue(merged_filename,"Combined_Runs".csv"),row.names = FALSE)
save(df_Merge,file = glue(merged_filename,".Rda"))
