## ---------------

# Issues: 
# - when exporting time-series plots as a vector graphics file type, the
# photoperiod shading shows 12 rectangles per photoperiod, resulting in dark
# vertical lines at each hourly bin

# Solution from Nicola Rennie, needs to be validated: 

# pp_data <- data.frame(Time = 0:71, 
#                       Photoperiod = as.factor(rep(rep(c(0,1),each = 12),3)))
# 
# maxheight <- 20
# plot_df <- purrr::map_dfr(seq_len(maxheight), ~pp_data)
# plot_df$y <- rep(1:maxheight, each = nrow(pp_data))
# 
# ggplot(plot_df)+
#   geom_raster(mapping = aes(x = Time, fill = Photoperiod, y = y),
#               alpha = 0.1,
#               show.legend = NA) + 
#   scale_fill_manual(values = c("0" = "gray45",
#                                "1" = "white"),
#                     guide = "none")+
#   theme_classic() +
#   scale_x_continuous()


# To do: 
# - Add shaded rectangle to dark period of box plots? 
# - Add statistics!!!

## Load required packages ----------------------------------------------------

library(tidyverse)
library(ggnewscale)
library(zoo, include.only = 'rollmean')
library(here)
library(broom)
library(ggpubr)
library(rstatix)

## Define inputs to script for analysis ---------------------------------------

cohort           <- "mon001"
rundate          <- "2021-10-18"

# Specify grouping & list of variables for smoothing via moving mean
groupvar         <- "Treatment"
facetvar         <- "Sex" # set to NA (no quotes!) if no faceting desired
plt              <- "Dark2"
export           <- T
fpath            <- "C:/Users/cfaber/Dropbox (Barrow Neurological Institute)/Mirzadeh Lab Dropbox MAIN/Data/OLD_Calorimetry/r_cleaned/2021-10-18_mon001"
ftype            <- ".pdf" # default to export pdfs

## Defaults -------------------------------------------------------------------
smooth           <- T
norm             <- T
tsvars           <- c('VO2','VCO2','VH2O','EE','EE.cum','EBalance','RER',
                      'AllMeters.cum','FoodIn.cum','WaterIn.cum',
                      'FoodIn.kcal','BodyMass')
boxvars          <- c('VO2','EE','EBalance','RER','AllMeters','WaterIn.g',
                  'FoodIn.kcal')

if (norm) {
  boxvars        <- c(boxvars,'norm.EB.cum','norm.EE.cum','norm.EE','norm.FoodIn.cum.kcal')
}

if (smooth) {
  swin           <- suppressWarnings(as.integer(readline(prompt = "Enter window size (in integer hours) for smoothing via moving mean:")))
  while (is.na(swin)) {
    message("You entered a non-numeric value for smoothing window size. Try again.")
    swin  <- suppressWarnings(as.integer(readline(prompt = "Enter window size (in integer hours) for smoothing via moving mean:")))
    }
} 

## Load data ------------------------------------------------------------------

load(here::here(paste(rundate,cohort,"Clean.Rda",sep = "_")))
unitkeys         <- read_csv(here::here("Cal_Units.csv"))

# Statistics -----------------------------------------------------------------

# From Longitudinal_Phenotyping:
# Needs to be revised to run ANOVA, post hoc comparisons between/within groups

plotvar <- "RER"
RER_ttest <- exp.pp.avg %>%
  group_by(.data[[facetvar]]) %>% # group by variable that will be used to facet. .data[[]] subsets variable name within string (see https://ggplot2.tidyverse.org/reference/tidyeval.html)
  t_test(as.formula(paste(plotvar, "~", groupvar))) %>% # 
  adjust_pvalue(method = "bonferroni") %>%
  add_significance() %>%
  add_xy_position()

# Line below needs to be added to call to bxplot:
stat_pvalue_manual(RER_ttest,
                   bracket.nudge.y = -0.5,
                   label = "{p.adj.signif}")

## Plot functions -------------------------------------------------------------
# Create ggplot function for time-series plots with SEM ribbon
tsplot <- function(data,var,groupvar,facetvar,ylab) {
  
  plot <- ggplot(data, aes(x = Time, 
               y = .data[[var]],
               color = .data[[groupvar]],
               group = .data[[groupvar]],
               fill = .data[[groupvar]])) + 
  # Conditionally facet if facetvar is not NA
  {if(!is.na(facetvar) & facetvar %in% colnames(data))facet_grid(~ .data[[facetvar]])} +
  stat_summary(fun = "mean", geom = "line", linewidth = 1) + 
  stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.5, linetype = 0) + 
  scale_color_brewer(palette = plt) + 
  scale_fill_brewer(palette = plt) + 
  theme_classic() + 
  
# Shaded tiles for photoperiod
  new_scale_fill() +
  geom_tile(data = pp_data, 
            mapping = aes(x = Time, fill = Photoperiod,y=0),
            linewidth = 0,
            alpha = 0.1,
            linetype = 0,
            height = Inf, # tiles will go all the way up and down
            show.legend = NA,
            inherit.aes = FALSE) + 
  scale_fill_manual(values = c("0" = "gray45",
                               "1" = "white"),guide = "none") +
  
  # Plot annotations and formatting
  labs(x = "Time (hours)", y = ylab) + 
  scale_x_continuous(expand = expansion(0, 0)) +   # no padding on the x-axis
  theme_classic() 
}

# Create function to generate boxplots
bxplot <- function(var,groupvar,facetvar,ylab) {
  
  ggplot(exp.pp.avg,
         aes(x = Photoperiod,
             y = .data[[var]])) +
    facet_grid(~.data[[facetvar]]) +
    geom_boxplot(aes(fill = .data[[groupvar]]), alpha = 0.5) +
    scale_fill_brewer(palette = plt) +
    geom_point(position = position_dodge(width = 0.75),
               aes(color = .data[[groupvar]], 
                   fill = .data[[groupvar]], 
                   group = .data[[groupvar]]), 
               color = "black", shape = 21, show.legend = FALSE) + 
    labs(x = NULL, y = ylab) +
    scale_x_discrete(labels = c("Dark", "Light", "Total")) +
    theme_classic() + 
    ggtitle(title)
  
}

## Time-Series Plots ----------------------------------------------------------
# Conditionally generate smoothed vs. non-smoothed df.hourly
if (smooth) {
  df.hourly <- df.hourly %>%
    group_by(Animal) %>%
    mutate(across(all_of(tsvars), ~ rollmean(.x,swin,fill = NA),.names = "{.col}")) %>%
    ungroup()
  # Extract time-series and photoperiod as small df for plotting
  pp_data <- df.hourly %>%
    distinct(Time,Photoperiod)
} else {
  # Extract time-series and photoperiod as small df for plotting
  pp_data <- df.hourly %>%
    distinct(Time,Photoperiod)
}

ts.plots <- vector(mode = "list",length = length(tsvars)) # Initialize empty list
for (i in 1:length(tsvars)) {
  
  var <- tsvars[i]
  
  ylab <- filter(unitkeys,Renamed_Var == {{var}}) %>% 
    select(Title,Unit) %>% 
    summarize(ylab = paste(Title,Unit)) %>% 
    pull()
  
  title <- filter(unitkeys,Renamed_Var == {{var}}) %>% 
    select(Title) %>%
    pull()

  if (grepl("NA",ylab)) ylab <- title else ylab <- ylab

  ts.plots[[i]] <- tsplot(df.hourly,var,groupvar,facetvar,ylab)
  names(ts.plots)[i] <- var
  #print(ts.plots[[i]]) # commented out - tends to crash R
  if(export){
    if (smooth) {
      fname <- paste(var,"_smooth",as.character(swin),sep="")
    } else {
      fname <- var
    }
    ggsave(paste(rundate,cohort,paste(fname,ftype,sep=""),sep= "_"), path = fpath)
  }
}

## Box plots ------------------------------------------------------------------

# Loop through all variables for overall experiment plots
box.plots <- vector(mode = "list",length = length(boxvars))
for (i in 1:length(boxvars)) {
  
  var <- boxvars[i]
  fname <- paste(var,"_exp_boxplot")
  
  ylab <- filter(unitkeys,Renamed_Var == {{var}}) %>% 
    select(Title,Unit) %>% 
    summarize(ylab = paste(Title,Unit)) %>% 
    pull()
  
  title <- filter(unitkeys,Renamed_Var == {{var}}) %>% 
    select(Title) %>%
    pull()
  
  if (grepl("NA",ylab)) ylab <- title else ylab <- ylab
  
  names(box.plots)[i] <- var
  box.plots[[i]] <- bxplot(var,groupvar,facetvar,ylab)
  #print(plot)
  if (export) {
    ggsave(paste(rundate,cohort,paste(fname,ftype,sep=""),sep= "_"), path = fpath)
  }
}
