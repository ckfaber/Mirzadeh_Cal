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
# - Add shaded rectangle to dark period of box plots? 
# - GET RID OF ALL BODY MASS NORMALIZATIONS
# - Adjust x-axis numbering to go by 24-h intervals (unless really long recordings, then 48?)
# - Add statistics!!!

## Load required packages ----------------------------------------------------

library(tidyverse)
library(ggnewscale)
library(zoo, include.only = 'rollmean')
library(scales)
#library(broom)
#library(ggpubr)
#library(rstatix)

## Define inputs to script for analysis ---------------------------------------
fname           <- '2023-09-07_cal023_Clean.Rda'
fpath           <- "C:/Users/kaspe/Barrow Neurological Institute Dropbox/Chelsea Faber/Mirzadeh Lab Dropbox MAIN/Data/Calorimetry/macro_processed/r_cleaned"

# Specify grouping & list of variables for smoothing via moving mean
groupvar         <- "Treatment"
facetvar         <- "Sex" # set to NA (no quotes!) if no faceting desired
plt              <- "Dark2"
export           <- F
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

boxvars.cum      <- sort(c('AllMeters','EBalance','EE.cum','FoodIn.kcal',
                           'norm.FoodIn.kcal','WaterIn.g'))

if (smooth) {
  swin           <- suppressWarnings(as.integer(readline(prompt = "Enter window size (in integer hours) for smoothing via moving mean:")))
  while (is.na(swin)) {
    message("You entered a non-numeric value for smoothing window size. Try again.")
    swin  <- suppressWarnings(as.integer(readline(prompt = "Enter window size (in integer hours) for smoothing via moving mean:")))
    }
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
    swin  <- suppressWarnings(as.integer(readline(prompt = "Enter time (in hours) from recording start to segment plots:")))
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
  
  if(segment){
    data <- data %>% filter(Time < trimtime)
    photoperiods <- pp_data %>% filter(Time < trimtime)
  } else {
    photoperiods <- pp_data
  }
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
# For color options see http://sape.inf.usi.ch/quick-reference/ggplot2/colour
  new_scale_fill() +
  geom_tile(data = photoperiods, 
            mapping = aes(x = Time, fill = Photoperiod,y=0),
            linewidth = 0,
            alpha = 0.3,
            linetype = 0,
            height = Inf, # tiles will go all the way up and down
            show.legend = NA,
            inherit.aes = FALSE) + 
  scale_fill_manual(values = c("Dark" = "gray65",
                               "Light" = "white",
                               "Subjective Light" = "gray75"),guide = "none") +
  
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
      LightCycle == "DD" & Photoperiod == "Dark" ~ "Dark"))

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
    ggsave(paste(rundate,runid,paste(fname,ftype,sep=""),sep= "_"), width=5,height=3,units="in",path = repo)
  }
}

## Box plots ------------------------------------------------------------------

# Loop through all variables for overall experiment plots
boxplots.avg <- vector(mode = "list",length = length(boxvars.avg))
for (i in 1:length(boxvars.avg)) {
  
  var <- boxvars.avg[i]
  fname <- paste0(var,"_boxplot_avg")
  
  ylab <- filter(unitkeys,Renamed_Var == {{var}}) %>% 
    select(Title,Unit) %>% 
    summarize(ylab = paste(Title,Unit)) %>% 
    pull()
  
  title <- filter(unitkeys,Renamed_Var == {{var}}) %>% 
    select(Title) %>%
    pull()
  
  if (grepl("NA",ylab)) ylab <- title else ylab <- ylab
  
  names(boxplots.avg)[i] <- var
  boxplots.avg[[i]] <- bxplot(df.avg.total,var,groupvar,facetvar,ylab)
  #print(plot)
  if (export) {
    ggsave(paste(rundate,runid,paste(fname,ftype,sep=""),sep= "_"),width=5,height=3,units="in",path = repo)
  }
}

boxplots.cum <- vector(mode = "list",length = length(boxvars.cum))
for (i in 1:length(boxvars.cum)) {
  
  var <- boxvars.cum[i]
  fname <- paste0(var,"_boxplot_cum")
  
  ylab <- filter(unitkeys,Renamed_Var == {{var}}) %>% 
    select(Title,Unit) %>% 
    summarize(ylab = paste(Title,Unit)) %>% 
    pull()
  
  title <- filter(unitkeys,Renamed_Var == {{var}}) %>% 
    select(Title) %>%
    pull()
  
  title <- paste0("Cumulative ", title)
  
  if (grepl("NA",ylab)) ylab <- title else ylab <- ylab
  
  names(boxplots.cum)[i] <- var
  boxplots.cum[[i]] <- bxplot(df.cum.total,var,groupvar,facetvar,ylab)
  #print(plot)
  if (export) {
    ggsave(paste(rundate,runid,paste(fname,ftype,sep=""),sep= "_"),width=5,height=3,units="in",path = repo)
  }
}
