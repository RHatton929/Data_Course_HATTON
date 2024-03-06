#Rachel Hatton
#Assignment 7

library(tidyverse)
library(janitor)

#Task 1: Import data set

df <- read_csv("./Utah_Religions_by_County.csv")

#Task 2: Tidy data

clean_df <- df %>% 
  clean_names() %>% 
  pivot_longer(-c("county", "pop_2010", "religious"),
               names_to = "religion",
               values_to = "prop_religion") %>% 
  select(-religious)

clean_df$county <- 
  clean_df$county %>% 
  str_replace(" County", "")

#Task 3: Address the Questions
## Does population of a county correlate with the proportion of any specific religion in that county?####

clean_df %>% 
  ggplot(aes(x = pop_2010,
             y = prop_religion,
             color = county)) +
  geom_smooth(se = FALSE,
              color = 'coral3',
              method = 'lm') +
  geom_smooth(se = FALSE,
              color = 'cornflowerblue') +
  geom_point(size = 2) +
  facet_wrap(~religion, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5,
                                   hjust=1))

ggsave('pop_corr_religion.png')

# I had to free the scales because otherwise only LDS, Catholic, and Non-Religious would show any variation
# I used geom_smooth twice to see the linear model and the loess. I tried to coord_flip this one but it was ugly, so I just fixed the text instead
# It really looks like there is no correlation in the population of a county and the proportion of people who practice a religion

## Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?####

clean_df %>% 
  ggplot(aes(x = religion,
             y = prop_religion,
             fill = religion)) +
  geom_col() +
  facet_wrap(~county) +
  coord_flip()

ggsave('non_religion_corr.png')

# It looks like the higher the proportion of the population is LDS, the less non_religious people are in the county
# With the exception of Grand, San Juan, and Summit counties, where non-religious outperforms LDS
# Non-religious never breaks 50% of the population, with San Juan being closest at 0.49667
# I'd probably have to have more data and/or do more tests to determine what's different about those counties