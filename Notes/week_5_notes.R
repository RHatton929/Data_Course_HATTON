library(gapminder)
library(tidyverse)
library(ggimage)
library(gganimate)
library(patchwork)

#geom_grob takes images, moves them around
#get ggalignment for uglyplot
#look at the r extensions available

names(gapminder)
df <- gapminder

Adf <- df %>% 
  filter(grepl("^A", country))

PlotA1 <- Adf %>% 
  ggplot(mapping = aes(x = year,
                       y = lifeExp,
                       color = country)) +
  geom_point(aes(size = pop)) +
  facet_wrap(~continent) +
  theme_bw()

PlotA2 <- Adf %>% 
  ggplot(mapping = aes(x = year,
                               y = lifeExp,
                               color = country)) +
  geom_point(aes(size = pop)) +
  theme_bw()

PlotA2/PlotA1 + plot_annotation("Comparing with/without Facets")

p.dark <- PlotA +
  theme_dark()

#Patchwork####

PlotA + p.dark
PlotA / p.dark
(PlotA + p.dark)/p.dark + 
  plot_annotation("Main Title") +
  plot_layout(guides = 'collect')
#^ this works due to patchwork

my_countries <- c("Venezuela", "Rwanda", "Nepal")

df <- df %>% 
  mutate(my_countries = case_when(country %in% my_countries ~ country))

p3 <- 
  ggplot(df, aes(x=gdpPercap, y=lifeExp, color=continent)) +
  geom_point(aes(size = pop))+
  geom_text(aes(label = my_countries))

#GGAnimate####

p3 +
  transition_time(time = year) +
  labs(title = "Year: {frame_time}")

#Saving Plots####

anim_save("./Notes/gapminder_animation.gif")
ggsave("./Notes/plot_example.png", plot = p3)
#mess with height, width, dpi in ggsave
#