library(tidyverse)
library(janitor)
library(easystats)

# Task 1: Read in UNICEF data####

df <- read_csv("unicef-u5mr.csv")


# Task 2: Tidy UNICEF data####

clean_u5mr <- 
  df %>% 
  janitor::clean_names() %>% 
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

model_1 <- glm(data = clean_u5mr,
               u5mr ~ year)

## Model 2: Account for Year and Continent####

model_2 <- glm(data = clean_u5mr,
               u5mr ~ year + continent)

## Model 3: Account for Year, Continent, and their interaction term####

model_3 <- glm(data = clean_u5mr,
               u5mr ~ year * continent)


# Task 8: Compare the three models with respect to their performance####

compare_performance(model_1, model_2, model_3) %>% 
  plot()

# Model 3 outperforms the other two models in all areas, so it's likely our 'best'


# Task 9: Plot the 3 models' predictions as indicated####

clean_u5mr$mod1 <- predict(model_1, clean_u5mr)
clean_u5mr$mod2 <- predict(model_2, clean_u5mr)
clean_u5mr$mod3 <- predict(model_3, clean_u5mr)

clean_u5mr %>% 
  pivot_longer(starts_with("mod"),
               names_to = "model",
               values_to = "pred") %>% 
  ggplot(aes(x = year,
             y = pred,
             color = continent)) +
  geom_line(linewidth = 1) +
  facet_wrap(~model) +
  theme_bw() +
  labs(x = "Year",
       y = "Predicted U5MR",
       color = "Continent",
       title = "Model Predictions")


# BONUS - Task 10: Using preferred model, predict what the U5MR would be for Ecuador in the year 2020####

Ec_2020 <- data.frame(country_name = "Ecuador",
                      continent = "Americas",
                      region = "South America",
                      year = 2020)

predict(model_3, Ec_2020)


# The real value is 13, but model_3 puts the value at -10.58.
# How incredible that 10.58 babies are being spawned into existence per 1000 live births.

# Now, I'll attempt to make a better model:

model_4 <- glm(data = clean_u5mr,
               u5mr ~ poly(year, 2) * region)

compare_performance(model_3, model_4) %>% 
  plot()

# This comparison says it's better, so let's test it!

prediction <- predict(model_4, Ec_2020)
actual <- 13
differ <- actual - prediction

Ec_results <- data.frame(Model = "model_4",
                         Prediction = prediction,
                         Reality = actual,
                         Difference = differ)

# Admittedly, it doesn't make sense to me to make year a polynomial
# But it produced 13.65, which isn't too far off from the real value of 13
# It's certainly the closest I've been able to manage

# Let's try another country or 2 with model_4!

Chad_2020 <- data.frame(country_name = "Chad",
                        continent = "Africa",
                        region = "Middle Africa",
                        year = 2020)

predict(model_4, Chad_2020)

# Real = 110.5, Predicted = 99.53

Bangladesh_2020 <- data.frame(country_name = "Bangladesh",
                              continent = "Asia",
                              region = "Southern Asia",
                              year = 2020)

predict(model_4, Bangladesh_2020)

# Real = 28.8, Predicted = 8.069

# Basically, this model isn't good, but it isn't popping out negative death tolls, so ¯\_(ツ)_/¯
