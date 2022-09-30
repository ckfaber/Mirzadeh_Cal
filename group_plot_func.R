## Write ggplot function for timeseries plots ----------------------------------

group_plot <- function(data,group,title,ylab) {
  
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

group <- 'Treatment'

ts.plots <- vector(mode = "list",length = length(vars2plot))

for (i in 1:length(vars2plot)) {
  
  data <- grp.summaries[[i]]
  title <- names(grp.summaries)[i]
  names(ts.plots)[i] <- title
  ylab <- "UNITS"
  ts.plots[[i]] <- group_plot(data,group,title,ylab)
  
}



