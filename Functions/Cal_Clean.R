Cal_Clean <- function(cohort,
                      rundate,
                      fpath,
                      remove_animals = NA,
                      NCDgcal = 3.35,
                      HFDgcal = 5.47,
                      cols2excl = c('Still_pct_M','Sleep_pct_M',
                                    'XBreak_R','YBreak_R','Mass_g','AllMeters_M'))
{
  
  ## Load required packages -----------------------------------------------------
  
  library(plyr, include.only = 'mapvalues')
  library(tidyverse)
  library(lubridate)
  
  # Load data

  fname           <- paste(rundate,cohort,sep = "_")
  code            <- paste(rundate,cohort,"KEY",sep = "_")
  savename <- paste(rundate,cohort,"Clean.Rda",sep = "_")
  savename <- paste(fpath,"/r_cleaned/",savename,sep="")
  
  if (file.exists(savename)) {
    go <- menu(c("Re-run and overwrite existing Clean.Rda file","Re-run and save a new copy of Clean.Rda","Abort! Abort!"), title = "A .Rda file for this file already exists. How would you like to proceed?")
  }
  
  if (go == 1) {
    print(paste0("Re-running Cal_Clean script and over-writing previous file at ", savename))
    break
  }else if (go == 2) {
    savename <- paste(rundate,cohort,"Clean_COPY.Rda",sep = "_")
    savename <- paste0(fpath,"/r_cleaned/",savename)
    print(paste0("A new Clean.Rda file will be generated at ", savename))
    print("Be sure to revise filename inputs to subsequent scripts to reflect this new naming convention.")
  } else if (go == 3) {
    print("Aborting script, Clean.Rda file for this run already exists.")
    rm(list = ls())
  }
  
  # Load csv with run metadata
  key             <- read_csv(paste(fpath,paste(code,".csv",sep = ""),sep="/"),show_col_types=F) 
  
  # Load cal.csv and merge metadata
  df              <- read_csv(paste(fpath,paste(fname,".csv",sep = ""),sep="/"),show_col_types = F) %>%
    left_join(key, by = "Animal") %>%                 #unblind by merging with decoding df 
    rename(Cage = Animal,
           Animal = ID_Code)%>%
    select(!(starts_with("Enviro") 
             | starts_with("Ped") 
             | all_of(cols2excl))) %>%
    mutate(across(c(Animal,Sex,Cage,Group,Cohort,Treatment), factor))
  
  # Remove dead/sick animals; if NA, don't run
  if (!is.na(remove_animals)) {
    df <- df %>%
      filter(Animal != remove_animals)
  }
  
  # Parse date-times and sort by animal ID (ID_Code) and date-time
  df <- df %>% 
    relocate(c(DateTime,Sex,Group,Treatment,Cohort,Animal)) %>%
    mutate(DateTime = round_date(ymd_hms(df$DateTime),unit = "minute"),
           month = as.numeric(month(DateTime)),
           day = as.numeric(day(DateTime)),
           hour = as.numeric(hour(DateTime)),
           minute = as.numeric(minute(DateTime))) %>%
    mutate(DietChangeDateTime = round_date(mdy_hm(df$DietChangeDateTime)))
  
  df <- df[order(df$DateTime),] %>%
    arrange(Animal)
  
  # Remove any rows/columns with only NAs
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  df <- df[, colSums(is.na(df)) != nrow(df)]
  
  
  # Extract original date-time information
  og.start         <- head(df$DateTime, n=1)
  og.end           <- last(df$DateTime)
  og.dur           <- og.end - og.start
  
  print(paste("Trimmming df to start and end at ZT18. Original recording length =", round(og.dur,2), "days."))
  
  # Find index of first lights out (18:00) for each animal
  df <- df %>% group_by(Animal)
  idx.start        <- which(hour(df$DateTime) == 18)[1]-1
  time.start       <- as.POSIXct(df$DateTime[idx.start])
  
  # Trim everything before first lights out
  df <- df %>%
    filter(DateTime > time.start)
  
  # Find index of last lights out
  idx.end          <- tail(which(hour(df$DateTime) == 17), n=1) + 1
  time.end         <- as.POSIXct(df$DateTime[idx.end])
  
  # Trim everything after last lights out
  df <- df %>%
    filter(DateTime < time.end)
  
  trim.dur         <- last(df$DateTime) - head(df$DateTime,n=1)
  print(paste(round(og.dur - trim.dur,2), "days trimmed. Trimmed df length =", round(trim.dur,2), "days."))
  
  # Compute ZT time, rename some variables
  df <- df %>%
    mutate(ZT = plyr::mapvalues(hour, 
                                from = (0:23), 
                                to = c(18:23,0:17)),.before = Sex) %>% 
    mutate(Photoperiod = as.factor(
      plyr::mapvalues(ZT, 
                      from = c(0:23), 
                      to = c(rep("Light",12),rep("Dark",12)))),.before = ZT) %>%
    rename(VO2 = VO2_M,
           VCO2 = VCO2_M,
           EE = kcal_hr_M,
           RER = RER_M,
           FoodIn.cum = FoodInA_M,
           WaterIn.cum = WaterInA_M,
           AllMeters = AllMeters_R,
           BodyMass = BodyMass_Mnz,
           VH2O = VH2O_M)
  
  # Compute experimental day (ExpDay) and elapsed time (Time) in hours
  df <- df %>%
    mutate(ExpDay = as.integer(ceiling(difftime(DateTime,time.start, units = "days"))),
           Time = as.numeric(difftime(DateTime,time.start), units = "hours"),
           .after = DateTime) %>%
    ungroup()
  
  # Calculate rolling Age, from the starting Age entered in the "DECODED" metadata file.
  if ("Age" %in% colnames(df)) {
    df <- df %>%
      group_by(ExpDay,Animal) %>% 
      mutate(Age = ExpDay-1 + first(Age)) %>% 
      ungroup()
  } else {
    print("No Age values found in DECODED. If this is in error, please revised DECODED sheet accordingly, save, and rerun script, otherwise, continue.")
  }
  
  # Recompute cumulative variables to begin at new start time
  df <- df %>%
    group_by(Animal) %>% 
    mutate(FoodIn.cum = FoodIn.cum - first(FoodIn.cum),
           WaterIn.cum = WaterIn.cum - first(WaterIn.cum)) %>% 
    ungroup()
  
  # Compute time interval (in hours) for estimating binned energy expenditure
  int <- df$Time[2] - df$Time[1]
  
  # Compute daily average for body mass
  df <- df %>%
    group_by(Animal, ExpDay) %>%
    mutate(meanBodyMass = mean(BodyMass),.after = BodyMass) %>%
    ungroup()
  
  # Detect diet from key and establish conversion
  if (sum(is.na(key$Diet1))==length(key$Diet1) & sum(is.na(key$Diet2)) ==length(key$Diet2)) {
    
    diet <- menu(c("NCD","HFD"), title = "No diet type specified in DECODED file. Which diet was provided?")
    
    df <- df %>%
      mutate(Diet = case_when(
        diet == 1 ~ "NCD",
        diet == 2 ~ "HFD"))
  } else {
    df <- df %>% 
      group_by(Animal) %>%
      mutate(Diet = case_when(
        DateTime <= DietChangeDateTime ~ Diet1,
        DateTime > DietChangeDateTime ~ Diet2)) %>%
      ungroup()
  }
  
  # Compute new columns
  df <- df %>%
    group_by(Animal) %>%
    mutate(FoodIn.g = c(diff(FoodIn.cum),0),.before = FoodIn.cum) %>% # convert cumulative to binned food intake
    mutate(WaterIn.g = c(diff(WaterIn.cum),0),.before = WaterIn.cum) %>% # convert cumulative to binned water intake
    mutate(FoodIn.kcal = case_when(
      Diet == "NCD" ~ FoodIn.g * NCDgcal,
      Diet == "HFD" ~ FoodIn.g * HFDgcal),
      .before = FoodIn.g) %>% # convert g to kcal
    mutate(FoodIn.cum.kcal = cumsum(FoodIn.kcal),.after = FoodIn.cum) %>%
    mutate(EE.kcal.bin = EE * int, .before = EE) %>% # EE is kcal/hr, multiply by int (x hours between samples) to get kcal/bin
    mutate(EE.cum = cumsum(EE.kcal.bin), .before = EE.kcal.bin) %>%
    mutate(EBalance = FoodIn.kcal - EE.kcal.bin, .before = VO2) %>% # compute energy balance per bin
    mutate(EB.cum = cumsum(EBalance), .after = EBalance) %>%
    mutate(AllMeters.cum = cumsum(AllMeters),.before = AllMeters) %>% # compute cumulative distance traveled
    ungroup()
  
  # Compute measures normalized to body weight
  cols2norm <- c('FoodIn.kcal','FoodIn.cum.kcal','EE','EE.cum','EBalance',
                 'EB.cum','EE.kcal.bin')
  
  df <- df %>% 
    group_by(Animal) %>% 
    mutate(across(all_of(cols2norm),
                  ~ . / meanBodyMass,
                  .names = "norm.{.col}")) %>% 
    ungroup()
  
  # Trim last row with mostly NAs, created during some transformations above
  df <- head(df,-1)
  
  # Bin to hourly Here, we compute hourly bins for each variable. Depending on
  # variable, this is done by either summing all values within the hour
  # (cols2sum: binned, non-cumulative measures), taking the mean of all values
  # within the hour (cols2avg: rates), taking the maximum value within the hour
  # (cols4cum: binned, cumulative measures), and taking the median value for
  # assigning the new bin time (cols2med: dates and times only).
  #
  
  # DO NOT CHANGE WITHOUT EXPRESS PERMISSION FROM CHELSEA!!!!!!!!!!
  cols2sum <- sort(c('norm.FoodIn.kcal','FoodIn.g','FoodIn.kcal','WaterIn.g','EBalance','norm.EBalance','AllMeters','EE.kcal.bin','norm.EE.kcal.bin'))
  cols2avg <- sort(c('VO2','VCO2','VH2O','EE','RER','BodyMass','meanBodyMass','norm.EE'))
  cols4cum <- sort(c('AllMeters.cum','FoodIn.cum','WaterIn.cum','EE.cum','norm.EE.cum','EB.cum','norm.EB.cum','FoodIn.cum.kcal','norm.FoodIn.cum.kcal'))
  cols2med <- c('DateTime','Time','minute')
  
  # Bin to hourly! 
  df.hourly <- df %>%
    group_by(Animal,ExpDay,hour) %>%
    mutate(
      across(all_of(cols2med),median)) %>% # assign middle of time bin to new bin
    mutate(across(
      all_of(cols2avg), mean)) %>% # rates get averaged
    mutate(across(
      all_of(cols2sum),sum)) %>% # intake, distances get summed
    mutate(across(
      all_of(cols4cum),last)) %>% # cumulative values just keep the maximum (total for the hour)
    ungroup() %>%
    distinct() %>% # squashes down to one observation per hour
    select(!c(hour,minute,month,day)) %>% 
    mutate(Animal = as.factor(Animal)) %>%
    group_by(Animal) %>%
    #slice(2:(n()-1)) %>% # trim incomplete hours at start and end
    mutate(DateTime = round_date(DateTime, "30 minutes")) %>% 
    mutate(Time = as.numeric(
      difftime(DateTime,DateTime[1]),units = "hours"),.after = DateTime) %>% # starts clock from 0 at start of recording
    ungroup()
  
  # Compute averages/totals for each day
  df.dailytot <- df %>%
    group_by(Animal,ExpDay) %>% 
    summarize(
      across(all_of(cols2sum),sum),
      across(all_of(cols2avg),mean)) %>%
    mutate(Photoperiod = "Total") %>%
    ungroup() 
  
  # Compute average within each photoperiod, append to total.avg.daily
  df.daily <- df %>%
    group_by(Animal,ExpDay,Photoperiod) %>%
    summarize(
      across(all_of(cols2sum),sum),
      across(all_of(cols2avg),mean)) %>% 
    ungroup() %>%
    bind_rows(.,df.dailytot) %>% 
    left_join(key, by = c("Animal" = "ID_Code")) %>% 
    select(-Animal.y) %>%
    arrange(Animal,ExpDay,Photoperiod)
  rm(df.dailytot)
  
  # Finally, we compute cumulative totals across the entire experiment.
  
  df.avg.total <- df.daily %>%
    group_by(Animal, Photoperiod) %>%
    summarize(
      across(all_of(c(cols2sum,cols2avg)),mean)) %>%
    ungroup() %>%
    left_join(key, by = c("Animal" = "ID_Code")) %>%
    select(-c(Animal.y, Diet1,Diet2,DietChangeDateTime)) 
  
  # Compute each animal's total per cumulative variable for entire experiment
  df.cum.total <- df.daily %>%
    group_by(Animal, Photoperiod) %>%
    summarize(
      across(all_of(cols2sum),sum)) %>% 
    ungroup()  %>%
    left_join(key, by = c("Animal" = "ID_Code")) %>% 
    select(-c(Animal.y, Diet1,Diet2,DietChangeDateTime)) %>%
    rename(EE.cum = EE.kcal.bin,
           norm.EE.cum = norm.EE.kcal.bin)
  
  #Save data
  
  save(df, df.hourly, df.daily, df.avg.total, df.cum.total,key, file = savename)
  #rm(list = ls())
  
}

  
