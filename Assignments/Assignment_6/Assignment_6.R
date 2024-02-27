#Rachel Hatton
#Assignment 6

library(tidyverse)
library(janitor)
library(gganimate)

#Task 1: Clean Data into Tidy (Long) Form####

df <- read_csv("../../Data/Biolog_Plate_Data.csv") %>% 
  clean_names()

names(df)

df <- 
  df %>% 
  pivot_longer(starts_with("hr"),
               names_to="hr",
               values_to="absorb",
               names_prefix = "hr_",
               names_transform = as.numeric)


#Task 2: Create New Column Specifying Soil or Water Sample####

unique(df$sample_id)

df <- 
  df %>% 
  mutate(sample_type = case_when(sample_id == "Clear_Creek" | sample_id == "Waste_Water" ~ "Water",
                                 sample_id == "Soil_1" | sample_id == "Soil_2" ~ "Soil"))

#Task 3: Generate Matching Plot####

df %>% 
  filter(dilution == .1) %>% 
  ggplot(aes(x = hr,
             y = absorb,
             color = sample_type)) + 
  geom_smooth(se = FALSE) +
  facet_wrap(~substrate) +
  theme_minimal() +
  labs(title = "Just dillution 0.1",
       x = "Time",
       y = "Absorbance",
       color = "Type")


#Task 4: Generate Matching Animated Plot####

df %>% 
  group_by(hr, sample_id, dilution, substrate) %>% 
  summarize(mean_absorb = mean(absorb)) %>% 
  filter(substrate == "Itaconic Acid") %>% 
  ggplot(aes(x = hr,
             y = mean_absorb,
             color = sample_id)) +
  geom_line() +
  facet_wrap(~dilution) +
  theme_minimal() +
  labs(x = "Time",
       y = "Mean_absorbance",
       color = "Sample ID") +
  transition_reveal(hr)

