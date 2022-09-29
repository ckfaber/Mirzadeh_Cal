## Script for Generating Summary Tables of Pre-processed Calorimetry Data

# Written by Chelsea Faber
# Mirzadeh Lab, Barrow Neurological Institute

# kasper.chelsea@gmail.com

# REQS:

# - 'filename_Clean.Rda' file from Cal_Clean.R script

## Load packages -------------------------------------------------------------

library(tidyverse)
library(here)

## Summary parameters ----------------------------------------------------------

# Remind yourself of the variable names
colnames(df)

# Select which ones you'd like to see summary statistics for
cols_to_eval <- c("VO2","RER","EE")

# How do you want to group the data?
#groups_for_eval <- c("Sex","Group","Treatment")

# Quick experimental overview
df %>%
  summarize(
    across(Sex:Animal,n_distinct)
  ) %>%
  gt() %>%
  tab_header(
    title = md(glue("Summary of cohort **{cohort}** calorimetry study beginning {exp_date}")),
    subtitle = md("*Counts* per column")
  ) %>%
  opt_align_table_header(align = "left")

# Summary statistics by group for variables selected above

# Mean
df %>%
  group_by(Treatment) %>%
  summarize(
    across(all_of(cols_to_eval),mean) # this is where gt breaks currently - can only put one function as input to across
  ) %>%
  gt(rowname_col = "Treatment") %>%
  tab_header(
    title = md(glue("Column mean of **{cohort}** grouped by *Treatment*")),
    subtitle = md(glue("From calorimetry study beginning {exp_date}"))
  ) %>%
  opt_align_table_header(align = "left")

# Median

# Min
df %>%
  group_by(Treatment) %>%
  summarize(
    across(cols_to_eval,min) # this is where gt breaks currently - can only put one function as input to across
  ) %>%
  gt(rowname_col = "Treatment") %>%
  tab_header(
    title = md(glue("Column min values of **{cohort}** grouped by *Treatment*")),
    subtitle = md(glue("From calorimetry study beginning {exp_date}"))
  ) %>%
  opt_align_table_header(align = "left")

# Max
df %>%
  group_by(Treatment) %>%
  summarize(
    across(cols_to_eval,max) # this is where gt breaks currently - can only put one function as input to across
  ) %>%
  gt(rowname_col = "Treatment") %>%
  tab_header(
    title = md(glue("Column max values of **{cohort}** grouped by *Treatment*")),
    subtitle = md(glue("From calorimetry study beginning {exp_date}"))
  ) %>%
  opt_align_table_header(align = "left")

# SD
df %>%
  group_by(Treatment) %>%
  summarize(
    across(cols_to_eval,sd) # this is where gt breaks currently - can only put one function as input to across
  ) %>%
  gt(rowname_col = "Treatment") %>%
  tab_header(
    title = md(glue("Column sd values of **{cohort}** grouped by *Treatment*")),
    subtitle = md(glue("From calorimetry study beginning {exp_date}"))
  ) %>%
  opt_align_table_header(align = "left")