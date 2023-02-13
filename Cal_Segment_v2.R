# Required user-input
segment   <- T
daysplits <- c(10,20) # total number of segments will be length(Daysplits)+1 (0 - int1, int1-int2, int2-end)

## WIP - Prompt user for segmentation information ------------------------------
if (segment) {
  nseg           <- suppressWarnings(as.integer(readline(prompt = "Enter number of segments desired:")))
  while (is.na(nseg)) {
    message("You entered a non-numeric value for number of segments. Try again.")
    nseg  <- suppressWarnings(as.integer(readline(prompt = "Enter number of segments desired:")))
  }
  # Initialize empty list for segmented data frames
  df.segments <- vector(mode = "list",length = length(nseg))
  daysplits   <- vector(mode = "integer", length = 1 - length(nseg))

}

## "Manually" ----------------------------------------------------------------- 
segment   <- T
daysplits <- c(10,20) # total number of segments will be length(Daysplits)+1 (0 - int1, int1-int2, int2-end)

# Create new column with Segments
df.hourly <- df.hourly %>% 
                      group_by(Animal) %>%
                      mutate(Segment = factor(findInterval(ExpDay, daysplits) + 1,ordered = TRUE))

# Create list of dataframes by Segments
df.segments <- split(df.hourly,df.hourly$Segment)

# Within each new segmented dataframe, recompute clock time, cumulative values, 
for (i in 1:length(df.segments)) {
  
  df.segments[[i]] <- df.segments[[i]] %>%
    group_by(Animal) %>%
    mutate(ExpDay = as.integer(ceiling(difftime(DateTime,time.start, units = "days"))),
           Time = as.numeric(difftime(DateTime,time.start), units = "hours"),
           .after = DateTime) %>%
    ungroup()
  
  # (OPTIONAL) Trim new segments to start and stop at ZT12
  
  # Recompute cumulative values
  
  
}

# Use bind_rows() to recombine all into one - facet by "Segment"? OR save each segment individually to run through Cal_Plot? 