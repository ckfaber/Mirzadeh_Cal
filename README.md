# Mirzadeh_Cal
R scripts and functions for tidying, visualizing, and analyzing indirect calorimetry data from Sable Promethion. 

## Requirements: 
- Expedata (.exp) data files processed with MI >v2.46 in 1-minute or 3-minute bins - all files must be processed with the SAME MACRO to ensure that the column names and binnings are consistent.
- .csv containing the TimeSeries sheet from the macro-processed .xml. 
- This code will work best if you implement the following naming convention: <"yyyy-mm-dd_runid_timeseries_m.csv">. If you name your raw .exp files with the yyyy-mm-dd_runID convention, the One-Click-Macro (OCM) can be modified to automatically export the TimeSeries as a .csv with this naming convention applied. Contact Sable for details on implementing this in your OCM.
- .csv containing any grouping/identifiers/metadata supplied to Sable in the 'Animal Data Entry' form. This file should have the same filename as the Group decoding sheet with same naming convention as TimeSeries sheet (e.g., 2023-06-08_cal021_KEY.csv), saved in same directory. A template is included in this repository. Until such time as Sable enables automatic export of this metadata from the OCMs, users of this code must manually enter their information into the template, and save it with the appropriate filename in the same folder as the TimeSeries.csv file.

## Workflow:
#### 1) Cal_Clean
Tidies and computes hourly bins (for time-series plots) and photoperiod-averaged data frames (for summary boxplots).
- Required Inputs: 
``` R
# Assumes file-naming convention of: yyyy-mm-dd_runID.csv
filename        <- '2023-06-08_cal021_timeseries_m.csv'
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
- Outputs one .Rda file containing the following:
    * df 
: Cleaned data frame at original sampling interval as performed by the macro. 
    * df.hourly
: Cleaned data frame re-sampled into hourly bins.
    * df.env
: Data frame containing ESA data
    * df.daily
: Cleaned data frame re-sampled into daily photoperiod bins.
    * df.avg.total
: Tidy data frame containing data averaged across the entire experiment (mean/total per day)
    * df.cum.total
: Somewhat redundant with df.avg.total, but frames the experimental averages as mean/total across experiment, instead of per day.
    * key
: Data frame containing run metadata for unblinding investigator.

#### 2) Cal_Inspect
Work in progress: Will be built up as a way to interactively view and assess quality of data.

#### 3) (Optional) Cal_Segment
Work in progress: This was a script requested for splitting large recordings into distinct segments. 
- 
#### 4) (Optional) Cal_Merge
Work in progress: This script was requested for merging separate recordings into one data frame. Please contact Chelsea before using.

#### 5) Cal_Plot
Takes the Clean.Rda file as input, and automatically generates time-series and photoperiod-averaged boxplots for the requested variables. 

Please acknowledge use of this repo in any publications.

For questions, issues, suggestions, please submit an issue or contact:

**Chelsea Faber**<br>
Mirzadeh Laboratory at Barrow Neurological Institute<br>
kasper DOT chelsea AT gmail DOT com<br>
02-Aug-2022
