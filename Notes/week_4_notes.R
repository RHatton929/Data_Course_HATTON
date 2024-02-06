#practice: piping####
#convert examples to pipe format
library(tidyverse)

iris %>% 
  pluck("Species")
  stringr::str_to_title() %>% 
  unique()

iris %>% 
  pluck("Sepal.Length") %>% 
  round(0) %>% 
  max()

rnorm(100, 0, 5) %>% 
  abs() %>% 
  mean()

seq(1, 100, 0.01) %>% 
  round(1) %>% 
  median()

#Reminder: Exam 1 is available####
library(palmerpenguins)
x <- 
  penguins %>% 
  mutate(fat_status = case_when(body_mass_g > 5000 ~ "fatty",
                                body_mass_g <= 5000 ~ "skinny"))

#Plot####
#make a plot! layer with +
x %>%
  filter(!is.na(sex)) %>% 
  ggplot(mapping = aes(x=body_mass_g, y=bill_length_mm, color = fat_status, shape = fat_status)) +
  geom_point() +
  geom_smooth() +
  scale_color_manual(values = c('violet', 'salmon')) + #make it ugly!!
  theme_gray()
#theme(axis.text = element_text(angle=180, face = 'italic')) +
#scale_color_viridis_d(option = 'plasma') #viridis is colorblind friendly

names(penguins)
ggplot(penguins, mapping = aes(x = flipper_length_mm,
                               y = body_mass_g, color=species, alpha=bill_depth_mm)) +
  #geom_line(aes(group=species))
  geom_path(aes(group=species)) +
  stat_ellipse() +
  geom_point(aes(color=sex))+
  geom_polygon()+
  geom_hex()+
  geom_bin_2d()+
  geom_boxplot()+
  geom_hline(yintercept=4500,linewidth=25, color ='magenta', linetype='1121', alpha=.25)+
  geom_point(color='yellow', aes(alpha=bill_depth_mm))+
  theme(axis.title = element_text(face='italic', size=12, angle=30), legend.background = element_rect(fill='hotpink', color='tan'))
#alpha is transparency
#geom_col is a bar chart (position='dodge') puts them overlapping

library(ggimage)
#geom_image(image=)


library(tidyverse)
library(penguins)

names(penguins)

peng_filtered <- penguins %>% 
  filter(sex!=is.na(sex))

ggplot(peng_filtered, mapping = aes(x = factor(year),
                               y = bill_length_mm,
                               color = species)) +
  geom_boxplot() +
  geom_jitter(height = 0, width = 0.1, alpha = 0.3) +
  facet_wrap(~year, scale = "free") +
  theme_bw()

ggplot(peng_filtered, aes(x=flipper_length_mm,
           fill=species)) +
  geom_density(alpha=0.3)
#same as a hist, really

#ask: what am I trying to show/discover!
#never hide data
#plot data before running stats! you need to visualize before you math it

#show and tell: leaflet!! Use this for final project.  

#READ_DELIM for TSV files
library(GGally)

GGally::ggpairs(penguins)
  
peng_filtered %>% 
  ggplot(aes(x = bill_depth_mm,
             y = body_mass_g)) +
  facet_wrap(~species) +
  geom_point(aes(color=sex), size = 4, alpha = .75) +
  scale_color_viridis_d(end=0.8)+
  theme_bw()+
  theme(strip.background= element_blank())

library(gapminder)
df <- gapminder
