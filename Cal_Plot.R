## Script for Generating Plots and Summary Tables of Pre-processed Calorimetry Data

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
# 7) summary tables without having to hard-code each individual parameter...gt breaks when you try to do multiple
# 8) write function to generate multiple plots and group into figure

## Load packages -------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(glue)
library(here)

## Where be your data? ---------------------------------------------------------

cohort <- "mon001"
rundate <- "2021-10-18"

## Load data -----------------------------------------------------------------

load(here::here(paste(cohort,rundate,"Clean.Rda",sep = "_")))
#exp_date <- format(ymd(rundate),'%b %d, %Y')

## (Optional) Smoothing via moving mean ---------------------------------------

cols2smooth <- c('VO2','VCO2','VH2O','EE','RER','BodyMass')

smooth_win <- 3 # in hours

df.hourly %<>%
  group_by(Animal) %>% 
  mutate(across(
    all_of(cols2smooth) , ~ zoo::rollmean(., smooth_win, fill = NA), .names = "smooth{smooth_win}_{.col}" )) %>%
  ungroup()

## Compute summary statistics -----------------------------------------------

# Specify grouping & variables for time-series plotting
group <- 'Treatment'
vars2plot <- c('EE','EBalance','RER','AllMeters.cum','FoodIn.cum','WaterIn.cum','FoodIn.kcal','WaterIn.g')

## Create function to generate hourly summaries for plotting with geom_line()---
summarize_groups <- function(group,var) {
  
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
  grp.summaries[[i]] <- summarize_groups(group,var)

}

## Timeseries plots -----------------------------------------------------------

# Extract time-series as small df for plotting
pp_data <- df.hourly %>%
  distinct(Time,Photoperiod)

# Temporary fix to avoid copy/pasting tons of code blocks: hard-code variable of
# interest here
df <- grp.summaries[[1]]
xlab <- "Time (Hours)"
ylab <- "kcal"

EBplot <- ggplot(data = df) + 
  
  # Shaded light/dark boxes
  geom_tile(data = pp_data,
            mapping = aes(fill = Photoperiod,y=0),
            alpha = 0.2,
            height = Inf,
            show.legend = NA) + ## tiles will go all the way up and down
  aes(x = Time, y = value) +
  geom_line(aes(color = Treatment)) +
  
  # Set color scheme
  scale_color_manual(values = c("turquoise4", "darkorange3")) +   ## colors for the group
  
  # Smooth SEM ribbon 
  geom_ribbon(aes(
    ymin = value-sem, 
    ymax = value+sem,
    fill = Treatment),
    linetype = 0,
    alpha = 0.3)+
  
  # Set color scheme for filled areas: light/dark boxes and SEM ribbon
  scale_fill_manual(values = c("0" = "gray45","1" = "white","chABC" = "turquoise4","HIchABC" = "darkorange3"),guide = "none") +   
  labs(x = xlab, y = ylab)+ 
  scale_x_continuous(expand = expansion(0, 0)) +   ## no padding on the x-axis
  theme_classic() + 
  ggtitle("Energy Balance")

# use geom_segment to create vertical line to mark important events

## Box Plots (D/L/total) -----------------------------------------------------

ylab <- "Energy Expenditure (kcal/hr)"

pp.EE <- ggplot(pp.averaged.total,
                aes(x = Photoperiod,
                    y = mean.EE)) + 
  geom_boxplot(aes(fill = stage(Treatment,after_scale = alpha(fill,0.5)))) +
  scale_color_manual(values = c("turquoise4","darkorange3"),aesthetics = c("color","fill")) +
  labs(x = NULL, y = ylab) +
  scale_x_discrete(labels = c("Dark","Light")) +
  theme_classic()


pp.EE.daily <- ggplot(pp.averaged.daily,
                      aes(x = Photoperiod,
                          y = mean.EE)) + 
  geom_boxplot(aes(fill = stage(Treatment,after_scale = alpha(fill,0.5)))) +
  scale_color_manual(values = c("turquoise4","darkorange3"),aesthetics = c("color","fill")) +
  labs(x = NULL, y = ylab) +
  scale_x_discrete(labels = c("Dark","Light")) +
  theme_classic() +
  facet_grid(cols = vars(exp_day))

## Assemble figures -----------------------------------------------------------

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

# Modify to save vector graphics (for better scaling at high-res)

# Save png to disk
# ggsave("plotname.png", [plotname here], width = 5, height = 5)

#save cached copy of plot object to disk
# saveRDS(p, "plot.rds")
# q <- readRDS("plot.rds")


## Summary parameters ----------------------------------------------------------

# Remind yourself of the variable names
# colnames(df)
# 
# # Select which ones you'd like to see summary statistics for
# cols_to_eval <- c("VO2","RER","EE")
# 
# # How do you want to group the data?
# #groups_for_eval <- c("Sex","Group","Treatment")
# 
# # Quick experimental overview 
# df %>%
#   summarize(
#     across(Sex:Animal,n_distinct)
#   ) %>%
#   gt() %>%
#   tab_header(
#     title = md(glue("Summary of cohort **{cohort}** calorimetry study beginning {exp_date}")),
#     subtitle = md("*Counts* per column")
#   ) %>%
#   opt_align_table_header(align = "left")
# 
# # Summary statistics by group for variables selected above 
# 
# # Mean
# df %>%
#   group_by(Treatment) %>%
#   summarize(
#     across(all_of(cols_to_eval),mean) # this is where gt breaks currently - can only put one function as input to across
#   ) %>%
#   gt(rowname_col = "Treatment") %>%
#   tab_header(
#     title = md(glue("Column mean of **{cohort}** grouped by *Treatment*")),
#     subtitle = md(glue("From calorimetry study beginning {exp_date}"))
#   ) %>%
#   opt_align_table_header(align = "left")
# 
# # Median
# 
# # Min
# df %>%
#   group_by(Treatment) %>%
#   summarize(
#     across(cols_to_eval,min) # this is where gt breaks currently - can only put one function as input to across
#   ) %>%
#   gt(rowname_col = "Treatment") %>%
#   tab_header(
#     title = md(glue("Column min values of **{cohort}** grouped by *Treatment*")),
#     subtitle = md(glue("From calorimetry study beginning {exp_date}"))
#   ) %>%
#   opt_align_table_header(align = "left")
# 
# # Max
# df %>%
#   group_by(Treatment) %>%
#   summarize(
#     across(cols_to_eval,max) # this is where gt breaks currently - can only put one function as input to across
#   ) %>%
#   gt(rowname_col = "Treatment") %>%
#   tab_header(
#     title = md(glue("Column max values of **{cohort}** grouped by *Treatment*")),
#     subtitle = md(glue("From calorimetry study beginning {exp_date}"))
#   ) %>%
#   opt_align_table_header(align = "left")
# 
# # SD
# df %>%
#   group_by(Treatment) %>%
#   summarize(
#     across(cols_to_eval,sd) # this is where gt breaks currently - can only put one function as input to across
#   ) %>%
#   gt(rowname_col = "Treatment") %>%
#   tab_header(
#     title = md(glue("Column sd values of **{cohort}** grouped by *Treatment*")),
#     subtitle = md(glue("From calorimetry study beginning {exp_date}"))
#   ) %>%
#   opt_align_table_header(align = "left")
