tsploteach <- function(data,var,groupvar,facetvar,ylab) {
  
  if(segment){
    data <- data %>% filter(Time < trimtime)
    photoperiods <- pp_data %>% filter(Time < trimtime)
  } else {
    photoperiods <- pp_data
  }
  plot <- ggplot(data, aes(x = Time, 
                           y = .data[[var]])) + 
    # Conditionally facet if facetvar is not NA
    {if(!is.na(facetvar) & facetvar %in% colnames(data))facet_grid(~ .data[[facetvar]])} +
    geom_line(aes(color = Animal),linewidth = 2) +
    theme_classic() + 
    
    # Shaded tiles for photoperiod
    geom_tile(data = photoperiods, 
              mapping = aes(x = Time, fill = Photoperiod,y=0),
              linewidth = 0,
              alpha = 0.1,
              linetype = 0,
              height = Inf, # tiles will go all the way up and down
              show.legend = NA,
              inherit.aes = FALSE) + 
    scale_fill_manual(values = c("Dark" = "gray45",
                                 "Light" = "white"),guide = "none") +
    
    # Plot annotations and formatting
    labs(x = "Time (hours)", y = ylab) + 
    scale_x_continuous(expand = expansion(0, 0)) +   # no padding on the x-axis
    theme_classic() + 
    theme(text = element_text(size = 12))
  return(plot)
}
