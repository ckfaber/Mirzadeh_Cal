# Mirzadeh_Cal
R scripts and functions for tidying, visualizing, and analyzing indirect calorimetry data from Sable Promethion. 

## Requirements: 
- Expedata (.exp) data files processed with MI v2.46 in 1-minute bins - all should be processed with the SAME MACRO
- TimeSeries sheet as a .csv 
- Group decoding sheet with same prefix as TimeSeries sheet (e.g., expID_date_code.csv), saved in same directory

## Workflow:
#### 1) Cal_Clean
Tidies and computes hourly bins (for time-series plots) and photoperiod-averaged data frames (for summary boxplots).
- Required Inputs: 
``` R
# Assumes file-naming convention of: yyyy-mm-dd_run.csv
cohort          <- "run001"
rundate         <- "yyyy-mm-dd"
fpath           <- "C:/path/to/filefolder"
```
- Optional Inputs: 
```R
NCDgcal         <- 3.35 # default conversion of kcal per gram for normal chow 
HFDgcal         <- 5.47 # default conversion of kcal per gram for high-fat diet
segment.run     <- FALSE # for conditional execution of Cal_Segment 
cols2excl       <- c('Still_pct_M','Sleep_pct_M',
                     'XBreak_R','YBreak_R','Mass_g','AllMeters_M') # default columns to exclude
```
- Outputs:
    * df
: Cleaned data frame at original sampling interval as performed by the macro. 
    * df.hourly
: Cleaned data frame re-sampled into hourly bins.
    * df.daily
: Cleaned data frame re-sampled into daily photoperiod bins.
    * df.daily.avg
: WIP!!
    * df.total
: WIP!!!
    * key
: Data frame containing run metadata for unblinding investigator.
#### 2) Cal_Inspect
- Inputs:
- Outputs:
#### 3) (Optional) Cal_Segment
#### 4) (Optional) Cal_Merge
#### 5) Cal_Plot

Please acknowledge use of this repo in any publications.

For questions, issues, suggestions, please submit an issue or contact:

**Chelsea Faber**<br>
Mirzadeh Laboratory at Barrow Neurological Institute<br>
kasper DOT chelsea AT gmail DOT com<br>
02-Aug-2022
