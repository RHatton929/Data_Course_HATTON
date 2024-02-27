library(tidyverse)
library(palmerpenguins)

df <- penguins
names(penguins)

ugly <- ggplot(df, aes(x = bill_length_mm, y = flipper_length_mm)) +
  geom_line(aes(color = "nah")) + 
  geom_smooth(color = "purple", se = FALSE) +
  facet_wrap(~year, scale = "free") +
  labs(x =" yes ", y =" maybe ") +
  theme(axis.title.x = element_text(face = 'bold',
                                    color = "blue",
                                    size = 3,
                                    hjust = 0,
                                    vjust = 0,
                                    angle=30),
        axis.text = element_blank(),
        axis.title.y = element_text(face = 'bold',
                                    color = "blue",
                                    size = 2,
                                    hjust = 0,
                                    vjust = 0,
                                    angle=30),
        legend.text = element_text(size = 60,
                                   family = "sans"),
        legend.title = element_text(color = "springgreen2",
                                    angle = -45,
                                    size = 5),
        strip.background = element_rect(fill = "lawngreen"),
        strip.text = element_text(color = "purple1", 
                                  face = "bold",
                                  vjust = 6, 
                                  hjust = 1),
        panel.grid.major = element_line(linewidth = 10, color = "lawngreen"),
        panel.background = element_rect(fill = "red"),
        plot.background = element_rect(fill = "lawngreen"))

ggsave("uglyplot.png", ugly, dpi = 42)
