# Faceting approach to double-plotted actogram: courtesy of June Choe in R4DS Slack

data <- tibble(
  Day = rep(1:4, each = 50),
  x = rep(1:50, times = 4),
  y = c(replicate(4, sample(20, 50, replace = TRUE))),
  facet_x = 1,
  facet_y = Day
)

data_dups <- data %>%
  filter(Day != 1) %>%
  mutate(
    facet_x = 2,
    facet_y = facet_y - 1
  )

data <- bind_rows(data,data_dups)

data %>%
  ggplot(aes(x, y)) +
  geom_col(
    fill = "black",
    show.legend = FALSE
  ) +
  geom_hline(
    aes(yintercept = 0),
    color = "black"
  ) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
        fill = after_scale(ifelse(xmin == 0, "black", "white"))),
    inherit.aes = FALSE,
    color = "black",
    data = . %>%
      filter(Day == 1) %>%
      summarize(
        xmin = c(-max(x), 0), xmax = c(0, max(x)),
        ymin = max(y) + 5, ymax = ymin + 5,
        facet_y = 1
      )
  ) +
  facet_grid(
    facet_y ~ facet_x,
    switch = "y",
    labeller = labeller(facet_y =  ~ paste("Day", 1:4))
  ) +
  # customization
  scale_x_continuous(expand = expansion(0)) +
  theme_void() +
  theme(
    plot.margin = unit(rep(.5, 4), "cm"),
    strip.text.y = element_text(
      vjust = 0.2, size = 12, face = "bold",
      margin = margin(r = 15)
    ),
    strip.text.x = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.spacing.x = unit(0,"lines")
  )
  