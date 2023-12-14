p <- ggplot(df.hourly,
                 mapping = aes(x = Time, 
                               y = AllMeters,
                               color = .data[[groupvar]],
                               group = Animal,
                               fill = .data[[groupvar]]))

p + geom_line(mapping = aes(linetype = Animal),stat = "identity")
