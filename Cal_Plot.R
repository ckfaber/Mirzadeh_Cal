## ---------------

# Issues: 
# - when exporting time-series plots as a vector graphics file type, the
# photoperiod shading shows 12 rectangles per photoperiod, resulting in dark
# vertical lines at each hourly bin

# - If custom sizing needed, will need to change width and height specifications within calls to ggsave (lines 209 and 237)

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
# - Adjust x-axis numbering to go by 24-h intervals (unless really long recordings, then 48?)
# - add custom plot dimensions, e.g.: 
#      - ggh4x::force_panelsizes(rows = unit(4,'cm'), cols = unit(4,'cm))
# - Add statistics!!!
# - add ability to customize colors: 
#      - custom_scale <- c('grey','red','darkred')
#      - plot1 + scale_color_manual(values = custom_scale) 
#      - needs to add conditional checks for proper length depending on n groups, whether or not palette has been specified, etc

## Load required packages ----------------------------------------------------

library(tidyverse)
library(ggnewscale)
library(zoo, include.only = 'rollmean')
library(scales)
#library(broom)
#library(ggpubr)
#library(rstatix)

## Define inputs to script for analysis ---------------------------------------
fname           <- '2023-12-11_cal023_full-run.Rda'
fpath           <- "C:/Users/kaspe/Barrow Neurological Institute Dropbox/Chelsea Faber/Mirzadeh Lab Dropbox MAIN/Data/Calorimetry/macro_processed/r_cleaned"

# Specify grouping & list of variables for smoothing via moving mean
groupvar         <- "Treatment"
facetvar         <- NA # set to NA (no quotes!) if no faceting desired
plt              <- "Dark2"
export           <- T
ftype            <- ".pdf" # default to export pdfs
segment          <- F

## -----------------------------------------------

fileparts       <- unlist(strsplit(fname,'[_.]+'))
rundate  <- fileparts[1]
runid    <- fileparts[2]
rm(fileparts)
  
## Defaults -------------------------------------------------------------------
smooth           <- T
tsvars           <- sort(c('AllMeters','AllMeters.cum','BodyMass','EBalance',
                           'EB.cum','EE','EE.cum','FoodIn.kcal',
                           'FoodIn.cum.kcal','RER','VO2','VCO2','VH2O',
                           'WaterIn.cum'))
                          
boxvars.avg      <- sort(c('AllMeters','EBalance','EE','FoodIn.kcal','RER',
                           'VO2','WaterIn.g'))


if (smooth) {
  swin           <- 3
}

if (export) {
  repo <- paste(rundate,runid,"plots",sep="_")
  repo <- paste0(fpath,"/",repo)
  
  if (!dir.exists(repo)) {
    dir.create(repo)
  }
}

if (segment) {
  trimtime           <- suppressWarnings(as.integer(readline(prompt = "Enter time (in hours) from recording start to segment plots:")))
  while (is.na(trimtime)) {
    message("You entered a non-numeric value for time to filter. Try again.")
    trimtime  <- suppressWarnings(as.integer(readline(prompt = "Enter time (in hours) from recording start to segment plots:")))
  }
}

## Load data ------------------------------------------------------------------

# Prompt user which .Rda should be loaded if a Copy exists
if (file.exists(paste0(fpath,"/",fname)) 
    & file.exists(paste0(fpath,"/",rundate,"_",runid,"_Clean_COPY.Rda"))) {
  
  tmp <- menu(c("Original","Copy"), 
              title = "Two .Rda files found for this run. Which would you like to plot?")
  if (tmp == 1) {
    f <- paste0(fpath,"/",fname,"_Clean.Rda")
  } else if (tmp == 2) {
    f <- paste0(fpath,"/",fname,"_Clean_COPY.Rda")
  }
} else if (file.exists(paste0(fpath,"/",fname))) {
  f <- paste0(fpath,"/",fname)
}

load(f)
unitkeys         <- read_csv(paste(fpath,"Cal_Units.csv",sep="/"))

# Statistics -----------------------------------------------------------------

# From Longitudinal_Phenotyping:
# Needs to be revised to run ANOVA, post hoc comparisons between/within groups

# plotvar <- "RER"
# RER_ttest <- df.exp.summary %>%
#   group_by(.data[[facetvar]]) %>% # group by variable that will be used to facet. .data[[]] subsets variable name within string (see https://ggplot2.tidyverse.org/reference/tidyeval.html)
#   t_test(as.formula(paste(plotvar, "~", groupvar))) %>% # 
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance() %>%
#   add_xy_position()
# 
# # Line below needs to be added to call to bxplot:
# stat_pvalue_manual(RER_ttest,
#                    bracket.nudge.y = -0.5,
#                    label = "{p.adj.signif}")

## Plot functions -------------------------------------------------------------
# Create ggplot function for time-series plots with SEM ribbon
tsplot <- function(data,var,groupvar,facetvar,ylab) {
  ts <- data
  plot <- ggplot() +
    # Conditionally facet if facetvar is not NA
    {if(!is.na(facetvar) & facetvar %in% colnames(data))facet_grid(~ .data[[facetvar]])} +  
    
    # Shaded tiles for photoperiod
    geom_tile(data = pp_data, 
              mapping = aes(x = Time, fill = Photoperiod,y=0),
              linewidth = 0,
              alpha = 0.3,
              linetype = 0,
              height = Inf, # tiles will go all the way up and down
              show.legend = NA,
              inherit.aes = FALSE) + 
    scale_fill_manual(values = c("Dark" = "gray65",
                                 "Light" = "white",
                                 "Subjective Light" = "gray75",
                                 "Subjective Dark" = "lightgoldenrod3"),guide = "none") + 
    new_scale_fill() +
    stat_summary(data = ts,
                 aes(x = Time, 
                     y = .data[[var]],
                     color = .data[[groupvar]],
                     group = .data[[groupvar]]),
                 fun = "mean", geom = "line", # use fun to return single value
                 linewidth = 1) + 
    stat_summary(data = ts,
                 aes(x = Time, 
                     y = .data[[var]],
                     color = .data[[groupvar]],
                     group = .data[[groupvar]],
                     fill = .data[[groupvar]]),
                 fun.data = mean_se, # use fun.data to return multiple values per data point (+/- sem)
                 geom = "ribbon", 
                 alpha = 0.5, linetype = 0) + 
    scale_color_brewer(palette = plt) + 
    scale_fill_brewer(palette = plt) + 
    theme_classic() + 
    
    # Plot annotations and formatting
    labs(x = "Time (hours)", y = ylab) + 
    scale_x_continuous(expand = expansion(0, 0)) +   # no padding on the x-axis
    theme_classic() + 
    theme(text = element_text(size = 12))
}

# Create function to generate boxplots
bxplot <- function(data,var,groupvar,facetvar,ylab) {
  
  ggplot(data,
         aes(x = Photoperiod,
             y = .data[[var]])) +
    {if(!is.na(facetvar) & facetvar %in% colnames(data))facet_grid(~ .data[[facetvar]])} +
    geom_boxplot(aes(fill = .data[[groupvar]]), alpha = 0.5) +
    scale_fill_brewer(palette = plt) +
    geom_point(position = position_dodge(width = 0.75),
               aes(color = .data[[groupvar]], 
                   fill = .data[[groupvar]], 
                   group = .data[[groupvar]]), 
               color = "black", shape = 21, show.legend = FALSE) + 
    labs(x = NULL, y = ylab) +
    #scale_x_discrete(labels = c("Dark", "Light", "Total")) +
    theme_classic() + 
    ggtitle(title) +
    theme(text = element_text(size = 12))
  
}

## Time-Series Plots ----------------------------------------------------------
# Conditionally generate smoothed vs. non-smoothed df.hourly
if (smooth) {
  df.hourly <- df.hourly %>%
    group_by(Animal) %>%
    mutate(across(all_of(tsvars), ~ rollmean(.x,swin,fill = NA),.names = "{.col}")) %>%
    ungroup()
}

# Extract time-series and photoperiod as small df for plotting
pp_data <- df.hourly %>%
  distinct(Time,Photoperiod,LightCycle) %>%
  mutate(Photoperiod = case_when(
    LightCycle == "LD" & Photoperiod == "Light" ~ "Light",
    LightCycle == "LD" & Photoperiod == "Dark" ~ "Dark",
    LightCycle == "DD" & Photoperiod == "Light" ~ "Subjective Light",
    LightCycle == "DD" & Photoperiod == "Dark" ~ "Dark",
    LightCycle == "LL" & Photoperiod == "Light" ~ "Light",
    LightCycle == "LL" & Photoperiod == "Dark" ~ "Subjective Dark"))

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
  
  tmplt <- tsplot(df.hourly,var,groupvar,facetvar,ylab)
  
  if (var == "RER") {
    tmplt <- tmplt + coord_cartesian(ylim = c(0.7,1.0))
  }
  
  if (nrow(pp_data) > 100) {
    xbreaks <- seq(from = 0, to = nrow(pp_data), by = 24)
    xlabels <- seq(from = 0, to = length(xbreaks)-1, by = 1)
    xlabel <- "Time (Days)"
    
    tmplt <- tmplt + scale_x_continuous(breaks = xbreaks,
                                        labels = xlabels,
                                        expand = expansion(0,0)) +
      xlab(xlabel)
  }
  
  ts.plots[[i]] <- tmplt
  names(ts.plots)[i] <- var
  #print(ts.plots[[i]]) # commented out - tends to crash R
  if(export){
    if (smooth) {
      fname <- paste(var,"_smooth",as.character(swin),sep="")
    } else {
      fname <- var
    }
    if (!is.na(facetvar)){
      fname <- paste(fname,"~",facetvar,sep="")
    }
    ggsave(paste(rundate,runid,paste(fname,ftype,sep=""),sep= "_"), width=5,height=3,units="in",path = repo)
  }
}

## Box plots ------------------------------------------------------------------

# Loop through all variables for overall experiment plots
boxplots.avg <- vector(mode = "list",length = length(boxvars.avg))
for (i in 1:length(boxvars.avg)) {
  
  var <- boxvars.avg[i]
  fname <- paste0(var,"_boxplot_avg")
  if (!is.na(facetvar)){
    fname <- paste(fname,"~",facetvar,sep="")
  }
  
  ylab <- filter(unitkeys,Renamed_Var == {{var}}) %>% 
    select(Title,Unit) %>% 
    summarize(ylab = paste(Title,Unit)) %>% 
    pull()
  
  title <- filter(unitkeys,Renamed_Var == {{var}}) %>% 
    select(Title) %>%
    pull()
  
  if (grepl("NA",ylab)) ylab <- title else ylab <- ylab
  
  names(boxplots.avg)[i] <- var

  tmplt <- bxplot(df.avg.total,var,groupvar,facetvar,ylab)
  if (var == "RER") {
    tmplt <- tmplt + coord_cartesian(ylim = c(0.7,1.0))
  }
  
  boxplots.avg[[i]] <- tmplt
  #print(plot)
  if (export) {
    ggsave(paste(rundate,runid,paste(fname,ftype,sep=""),sep= "_"),width=5,height=3,units="in",path = repo)
  }
}
