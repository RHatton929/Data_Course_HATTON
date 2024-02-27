library(tidyverse)
library(janitor)

# Task 1: Read in UNICEF data####

df <- read_csv("unicef-u5mr.csv") 


# Task 2: Tidy UNICEF data####

clean_u5mr <- 
  df %>% 
  clean_names() %>% 
  pivot_longer(starts_with("u5mr_"),
               names_to = "year",
               values_to = "u5mr",
               names_prefix = "u5mr_",
               names_transform = as.numeric)


# Task 3: Plot each country's U5MR over time####

clean_u5mr %>% 
  ggplot(aes(x = year,
             y = u5mr,
             fill = country_name)) +
  geom_line() +
  facet_wrap(~continent) +
  theme_bw() +
  labs(x = "Year",
       y = "U5MR")


# Task 4: Save this plot as LASTNAME_Plot_1.png####

ggsave("HATTON_Plot_1.png")


# Task 5: Plot the mean U5MR for all countries within a given continent each year####

clean_u5mr %>% 
  group_by(continent, year) %>% 
  summarize(mean_u5mr = mean(u5mr, na.rm = TRUE)) %>% 
  ggplot(aes(x = year,
             y = mean_u5mr,
             color = continent)) +
  geom_line(linewidth = 2) +
  theme_bw() +
  labs(x = "Year",
       y = "Mean_U5MR",
       color = "Continent")


# Task 6: Save this plot as LASTNAME_Plot_2.png####

ggsave("HATTON_Plot_2.png")


# Task 7: Create 3 models of U5MR####
## Model 1: Account for only Year####



## Model 2: Account for Year and Continent####



## Model 3: Account for Year, Continent, and their interaction term####




# Task 8: Compare the three models with respect to their performance####



# Task 9: Plot the 3 models' predictions as indicated####



# BONUS - Task 10: Using prefered model, predict what the U5MR would be for Ecuador in the year 2020####