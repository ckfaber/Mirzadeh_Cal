## Cal_Actogram: Double-plotted actogram -------------------------------------

library(tidyverse)
library(ggnewscale)

## Define filename/path ------------

cohort          <- "cal018"
rundate         <- "2023-02-15"
fpath            <- "C:/Users/cfaber/Dropbox (Barrow Neurological Institute)/Mirzadeh Lab Dropbox MAIN/Data/Calorimetry/macro_processed/r_cleaned"

## Load data --------------

fname           <- paste(rundate,cohort,sep = "_")

# Prompt user which .Rda should be loaded if a Copy exists
if (file.exists(paste0(fpath,"/",fname,"_Clean.Rda")) 
    & file.exists(paste0(fpath,"/",fname,"_Clean_COPY.Rda"))) {
  
  tmp <- menu(c("Original","Copy"), 
              title = "Two .Rda files found for this run. Which would you like to plot?")
  if (tmp == 1) {
    f <- paste0(fpath,"/",fname,"_Clean.Rda")
  } else if (tmp == 2) {
    f <- paste0(fpath,"/",fname,"_Clean_COPY.Rda")
  }
}

load(f)
unitkeys         <- read_csv(paste(fpath,"Cal_Units.csv",sep="/"))

## ------
# generate toy dataset

acto <- df %>% select(DateTime,Time,ExpDay,hour,minute,Photoperiod,Animal,XBreak_R,YBreak_R)
pp   <- df %>% distinct(Time,Photoperiod)

## -------------

ggplot(filter(acto,Animal == "dtx007"), 
       aes(x = Time, 
           y = ExpDay, 
           fill = YBreak_R)) + 
  geom_tile() + 
  scale_fill_continuous(low = "white", high = "red4") +
  new_scale_fill() +
  geom_tile(data = pp, 
          mapping = aes(x = Time, fill = Photoperiod,y=0),
          linewidth = 0,
          alpha = 0.1,
          linetype = 0,
          height = Inf, # tiles will go all the way up and down
          show.legend = NA,
          inherit.aes = FALSE) + 
  scale_fill_manual(values = c("Dark" = "gray45",
                               "Light" = "white"),guide = "none") + 
  theme_classic()

## Modify from actogrammr::plot_actogram() ----------
function (data, animal, activity_count = "YBreak_R") {
  
  data %>% 
    ggplot(aes(x = Time, y = interaction(date, file_name), fill = bin_act)) + 
    ggplot2::geom_tile() + 
    ggplot2::scale_fill_continuous(low = "white",
                                   high = "darkblue")
}
