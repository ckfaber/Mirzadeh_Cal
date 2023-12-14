## Cal_Actogram: Double-plotted actogram -------------------------------------

# TO DO:
# - fix naming convention issue
# - figure out why df.long from merged file doesn't work
# - 

library(tidyverse)
library(ggnewscale)

## Define filename/path ------------

filename        <- "2023-10-31_cal023_Clean.Rda"
fpath           <- "C:/Users/kaspe/Barrow Neurological Institute Dropbox/Chelsea Faber/Mirzadeh Lab Dropbox MAIN/Data/Calorimetry/macro_processed/r_cleaned/"

groupvar        <- "Treatment"
activity        <- "AllMeters" # default to AllMeters
plt             <- "Dark2"
export          <- F
ftype           <- ".pdf" # default

## Load data and extract activity into separate df --------------

load(paste0(fpath,filename))
unitkeys         <- read_csv(paste(fpath,"Cal_Units.csv",sep="/"))

if (export) {
  repo <- paste(rundate,cohort,"plots",sep="_")
  repo <- paste0(fpath,"/",repo,"/actograms")
  
  if (!dir.exists(repo)) {
    dir.create(repo)
  }
}

## Extract activity df -----------------------------------------------------

acto <- df %>% 
  select(DateTime:Cage,XBreak_R,YBreak_R,AllMeters,EE,LightCycle) %>%
  group_by(ExpDay) %>%
  mutate(Time_daily = as.numeric(
    difftime(DateTime,first(DateTime)), units = "hours"),.after = DateTime) %>%
  ungroup() %>%
  mutate(facet_x = 1,
         facet_y = ExpDay) %>%
  mutate(Photoperiod_actual = case_when(
    LightCycle == "LD" ~ Photoperiod,
    LightCycle == "DD" ~ "Dark"))

acto_dups <- acto %>%
  filter(ExpDay != 1) %>%
  mutate(
    facet_x = 2,
    facet_y = facet_y - 1
  )

acto <- bind_rows(acto,acto_dups)
rm(acto_dups)

# Extract photoperiod df
pp   <- acto %>% distinct(Time_daily,ZT,Photoperiod,Photoperiod_actual)

## Heat map style actogram -------------

ggplot(filter(acto,Animal == "dtx024"), 
       aes(x = Time_daily, 
           y = ExpDay, 
           fill = .data[[activity]])) + 
  geom_raster(hjust = 0, vjust = 0.5) + 
  scale_fill_continuous(low = "white", high = "red4") +
  #new_scale_fill() +
  #geom_tile(data = pp, 
          # mapping = aes(x = Time_daily, fill = Photoperiod,y=1),
          # linewidth = 0,
          # alpha = 0.3,
          # linetype = 0,
          # height = Inf, # tiles will go all the way up and down
          # show.legend = NA,
          # inherit.aes = FALSE) + 
  #scale_fill_manual(values = c("Dark" = "gray45",
  #                             "Light" = "white"),guide = "none") + 
  theme_classic() + 
  scale_x_continuous(limits = c(0,24),
                     name = "Zeitgeber Time",
                     expand = expansion(0)) +
  scale_y_reverse() +
  scale_y_continuous(breaks = seq(length(unique(acto$ExpDay)),1),
                     expand = expansion(0,0)) 


## Double-plotted actogram -------------

# Loop through all animals
actograms <- vector(mode = "list", length = n_distinct(acto$Animal))
for (ani in 1:n_distinct(acto$Animal)) {
    
    animal <- as.character(unique(acto$Animal)[ani])
    names(actograms)[ani] <- animal
    
    actograms[[ani]] <- acto %>% filter(Animal == animal) %>%
      ggplot() +
      geom_tile(aes(x = Time_daily, y = 1, fill = Photoperiod_actual),
                linewidth = 1,
                alpha = 0.3,
                color = "black",
                linetype = 0,
                height = Inf, # tiles will go all the way up and down
                show.legend = NA,
                inherit.aes = FALSE) +
      scale_fill_manual(values = c("Dark" = "gray45",
                                   "Light" = "white"),guide = "none") +
      
      geom_col(mapping = aes(x = Time_daily,y = .data[[activity]]),
               fill = "black",
               show.legend = FALSE) +
      
      geom_hline(
        aes(yintercept = 0),
        color = "black") +
      
      # Dark cycle box
      geom_rect(
        aes(xmin = 0, xmax = 12, ymin = ymin, ymax = ymax),
        color = "black",
        fill = "black",
        inherit.aes = FALSE,
        data = . %>% 
          filter(ExpDay == 1) %>%
          summarize(ymin = max(.data[[activity]]) + .25*max(.data[[activity]]), 
                    ymax = ymin + .25*max(.data[[activity]]),
                    facet_y = 1)) +
      
      # Light cycle box
      geom_rect(
        aes(xmin = 12, xmax = 24, ymin = ymin, ymax = ymax),
        color = "black",
        fill = "white",
        inherit.aes = FALSE,
        data = . %>% 
          filter(ExpDay == 1) %>%
          summarize(ymin = max(.data[[activity]]) + .25*max(.data[[activity]]), 
                    ymax = ymin + .25*max(.data[[activity]]),
                    facet_y = 1)) +
      
      facet_grid(
        facet_y ~ facet_x,
        switch = "y",
        labeller = labeller(facet_y = ~ paste("Day", 1 : max(acto$ExpDay)))) +
      
      # customization
      scale_x_continuous(expand = expansion(0)) +
      theme_void() +
      theme(
        plot.margin = unit(rep(.5, 4), "cm"),
        strip.text.y = element_text(
          vjust = 0.2, size = 12, face = "bold",
          margin = margin(r = 15)
        ),
        strip.text.x = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.spacing.x = unit(0,"lines")) +
      ggtitle(animal, subtitle = "Distance Traveled (m)") 
      
      if(export){
          fname <- animal
        ggsave(paste(rundate,cohort,paste(fname,ftype,sep=""),sep= "_"), width=5,height=6,units="in",path = repo)
      }
}

