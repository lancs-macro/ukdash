# Create map --------------------------------------------------------------

ggplot(nuts1_regions, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "#222d32", colour = "white") +
  theme_void()

ggsave("ukmap-dark.png", width = 4)