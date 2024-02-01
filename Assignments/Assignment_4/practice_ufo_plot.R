library(tidyverse)
ufo <- read_csv("./ufo_data.csv")

state_count <- 
  ufo %>% 
  mutate(state = toupper(state)) %>% 
  filter(country == "us" & state!=is.na(state)) %>% 
  group_by(state) %>% 
  summarize(N = n())

ggplot(state_count, mapping = aes(x = state, #x = fct_reorder(state, -N) for ordered by sightings
                                  y = N)) +
  geom_col(aes(fill = state)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.4, 
                                   hjust = 1),
        plot.title = element_text(face = "bold", size = (15))) +
    labs(title = "Number of UFO Sightings by US State/Territory",
       x = "State",
       y = "Number of UFO Sightings") +
  guides(fill = "none")