## Script for Generating Plots of Pre-processed Calorimetry Data

# Written by Chelsea Faber
# Mirzadeh Lab, Barrow Neurological Institute

# kasper.chelsea@gmail.com

# REQS:

# - 'filename_Clean.Rda' file from Cal_Clean.R script

# KNOWN ISSUES/BUGS
# - no flag for recording days with fewer than 24h - may misrepresent cumulative data

# To do 

# 1) total daily average together with dark/light box plots 
# 2) overlay individual values as geom_point? for small ns 
# 3) annotate number within each group on top of each box/bar plot 
# 4) SEM bars on bar plots 
# 5) stats!!! 
# 6) cumulative sum of FI within each photoperiod per animal <- partially there,
# but issues detailed below
# 7) write function to generate multiple plots and group into figure

## Load packages -------------------------------------------------------------

library(plyr, include.only = 'mapvalues')
library(tidyverse)
library(patchwork)
library(here)

## Where be your data? ---------------------------------------------------------

cohort <- "mon001"
rundate <- "2021-10-18"

# Specify grouping & variables for time-series plotting
group     <- 'Treatment'
vars2plot <- c('EE','EBalance','RER','AllMeters.cum','FoodIn.cum','WaterIn.cum','FoodIn.kcal')

## Load data -----------------------------------------------------------------

load(here::here(paste(cohort,rundate,"Clean.Rda",sep = "_")))
unitkeys <- read_csv(here::here("Cal_Units.csv"))

## (Optional) Smoothing via moving mean ---------------------------------------

cols2smooth <- c('VO2','VCO2','VH2O','EE','RER','BodyMass')

smooth_win <- 3 # in hours

df.hourly %<>%
  group_by(Animal) %>% 
  mutate(across(
    all_of(cols2smooth) , ~ zoo::rollmean(., smooth_win, fill = NA), .names = "smooth{smooth_win}_{.col}" )) %>%
  ungroup()

## Create function to generate hourly summaries --------------------------------

group_summarize <- function(group,var) {
  
  df.hourly %>%
    group_by( get(group) ,Time) %>%
    summarize(value = mean( get(var) ),
              sd = sd( get(var) ),
              n = n(),
              sem = sd(get(var)) / sqrt(n()),
              .groups = "drop") %>%
    rename( {{group}} := "get(group)")
}

# Loop through hard-coded variables of vars2plot
grp.summaries <- vector(mode = "list", length = length(vars2plot)) # initialize empty list
for (i in 1:length(vars2plot)) {
  
  var <- vars2plot[i]
  names(grp.summaries)[i] <- var
  grp.summaries[[i]] <- group_summarize(group,var)

}

## Timeseries plots -----------------------------------------------------------

# Extract time-series as small df for plotting
pp_data <- df.hourly %>%
  distinct(Time,Photoperiod)

group_tsplot <- function(data,group,title,ylab) {
  
  plot <- ggplot(data) + 
    
    # Shaded light/dark boxes
    geom_tile(data = pp_data,
              mapping = aes(fill = Photoperiod,y=0),
              alpha = 0.2,
              height = Inf,
              show.legend = NA) + ## tiles will go all the way up and down
    aes(x = Time, y = value) +
    geom_line(aes(color = get(group))) + 
    
    # Set color scheme
    scale_color_manual(group,values = c("turquoise4", "darkorange3")) +   ## colors for the group
    
    # Smooth SEM ribbon 
    geom_ribbon(aes(
      ymin = value-sem, 
      ymax = value+sem,
      fill = get(group)),
      linetype = 0,
      alpha = 0.3)+
    
    # Set color scheme for filled areas: light/dark boxes and SEM ribbon
    scale_fill_manual(values = c("0" = "gray45","1" = "white","chABC" = "turquoise4","HIchABC" = "darkorange3"),guide = "none") +   
    labs(x = "Time (hours)", y = ylab)+ 
    scale_x_continuous(expand = expansion(0, 0)) +   ## no padding on the x-axis
    theme_classic() + 
    ggtitle(title)
  
}

## Loop through ggplot generation ----------------------------------------------

ts.plots <- vector(mode = "list",length = length(vars2plot))

for (i in 1:length(vars2plot)) {
  
  data <- grp.summaries[[i]]
  var <- names(grp.summaries)[i]
  
  title <- mapvalues(var, from = unitkeys$Renamed_Var, to = unitkeys$Title, warn_missing = FALSE)
  unit <- mapvalues(var, from = unitkeys$Renamed_Var, to = unitkeys$Unit, warn_missing = FALSE)
  
  if (is.na(unit)) ylab <- title else ylab <- paste(title,unit)
  
  ts.plots[[i]] <- group_tsplot(data,group,title,ylab)
  names(ts.plots)[i] <- var
  #ts.plots[[i]]
  
}

# Visualize plots with the following syntax: 
ts.plots$RER

# TO DO: 
# - improve plot scaling
# - use geom_segment to create vertical line to mark important events

## Box Plots (D/L/total) -----------------------------------------------------

group_boxplot <- function(data,group,var,title,ylab) {
  
  ggplot(data,
         aes(x = Photoperiod,
             y = get(var))) +
    geom_boxplot(aes(fill = stage(get(group), after_scale = alpha(fill, 0.5)))) +
    scale_color_manual(group,
      values = c("turquoise4", "darkorange3"),
      aesthetics = c("color", "fill")
    ) +
    labs(x = NULL, y = ylab) +
    scale_x_discrete(labels = c("Dark", "Light", "Total")) +
    theme_classic() + 
    ggtitle(title)
  
}

## Loop through all plots ------------------------------------------------------

# BUG HERE: 
# For some reason, all of the plots in box.plots list end up as the last
# variable named in the loop. If you remove the call to save the ggplot to
# box.plots, and instead just print to the Plot window, they look correct. I
# cannot for the life of me figure out what is causing this problem, especially
# because the same code worked for the ts.plots above.

box.plots <- vector(mode = "list",length = length(vars2plot))

for (i in 1:length(vars2plot)) {
  
  var <- vars2plot[i]
  
  title <- mapvalues(var, from = unitkeys$Renamed_Var, to = unitkeys$Title, warn_missing = FALSE)
  unit <- mapvalues(var, from = unitkeys$Renamed_Var, to = unitkeys$Unit, warn_missing = FALSE)
  
  if (is.na(unit)) ylab <- title else ylab <- paste(title,unit)
  
  print(group_boxplot(data = pp.avg.total,group = group,var = var,title = title,ylab = ylab))
  
  #box.plots[[i]] <- plot
  names(box.plots)[i] <- var
  
}

## Assemble figures -----------------------------------------------------------

# TO DO: 
# - move to separate script

# busy, but example of what patchwork can do: 
Figure1 <- EE / locomotion

Figure1 +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    title = "Figure 1. Neonatal digestion of PNNs does stuff!",
    caption = "from cohort mon001",
    tag_levels = "a"
  )


Figure2 <- pp_EE | pp_VO2 | pp_RER

Figure2 +
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Figure 2. Photoperiod-averaged data is cool",
                  tag_levels = "a")


pp_EE / pp_VO2


## Save your pretty plots! ----------------------------------------------------

# TO DO: 
# - export box.plots & ts.plots

# Modify to save vector graphics (for better scaling at high-res)

# Save png to disk
# ggsave("plotname.png", [plotname here], width = 5, height = 5)

#save cached copy of plot object to disk
# saveRDS(p, "plot.rds")
# q <- readRDS("plot.rds")
