# Rachel Hatton
# Assignment 8

# Load Data####
library(tidyverse)
library(easystats)
library(modelr)

df <- read_csv('../../Data/mushroom_growth.csv')

# Explore Data, Create Plots####

df %>% 
  ggplot(aes(x = as.factor(Nitrogen),
             y = GrowthRate,
             color = Species)) +
  geom_point()
# Growth rate is not highly dependent on Nitrogen

df %>% 
  ggplot(aes(x = as.factor(Light),
             y = GrowthRate,
             color = Species)) +
  geom_point()
# P. cornucopiae does seem to have a higher growth rate dependent on Light

df %>% 
  ggplot(aes(x = as.factor(Light),
             y = GrowthRate,
             color = Species)) +
  geom_point() +
  facet_wrap(~Humidity)
# Light has a higher impact when paired with high humidity

df %>% 
  ggplot(aes(x = Humidity,
             y = GrowthRate,
             color = Species)) +
  geom_point() +
  facet_wrap(~Temperature)
# The growth rate of P. cornucopiae is higher at 20 degrees with high humidity than at 25

# Define min(4) Models Explaining "GrowthRate"####
mod1 <- glm(data = df, formula = GrowthRate ~ Species)

mod2 <- glm(data = df, formula = GrowthRate ~ Species + Light + Humidity + Temperature + Nitrogen)

mod3 <- glm(data = df, formula = GrowthRate ~ Species * Light * Humidity * Temperature * Nitrogen)

mod4 <- glm(data = df, formula = GrowthRate ~ Species * Light * Humidity * Temperature + Nitrogen)

compare_performance(mod1, mod2, mod3, mod4) %>% plot()

# Calculates Mean Sq Error of Each Model####

mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2) # mod 3 has the lowest mean-squared
mean(mod4$residuals^2)

# Adds Predictions Based on New Hypothetical Values for the Ind. Vars Model####

df <- df %>% 
  add_predictions(mod3)

df %>% dplyr::select("GrowthRate", "pred")

# Making a new data frame with predictor values
newdf <- data.frame(Species = c("P.ostreotus","P.ostreotus","P.ostreotus","P.cornucopiae","P.cornucopiae","P.cornucopiae","P.ostreotus","P.ostreotus","P.cornucopiae","P.cornucopiae"),
                    Light = c(0,20,0,10,10,0,20,10,20,10),
                    Nitrogen = c(25,25,45,5,10,0,20,35,0,20),
                    Humidity = c("High","Low","Low","Low","High","High","Low","Low","High","Low"),
                    Temperature = c(25,20,20,25,20,25,20,25,20,20))

pred <- predict(mod3, newdf)
hyp_preds <- data.frame(Species = newdf$Species,
                        Light = newdf$Light,
                        Nitrogen = newdf$Nitrogen,
                        Humidity = newdf$Humidity,
                        Temperature = newdf$Temperature,
                        pred = pred)

df$PredType <- "Real"
hyp_preds$PredType <- "Hypothetical"

fullpreds <- full_join(df, hyp_preds)

# Plots Predictions####

fullpreds %>% 
  ggplot(aes(x = Light,
             y = pred, 
             color = PredType)) +
  geom_point() +
  geom_point(aes(y = GrowthRate), alpha = .75, color = "Black") +
  geom_smooth(aes(y = GrowthRate), color = "Black") +
  geom_smooth() +
  theme_minimal()

fullpreds %>% 
  ggplot(aes(x = Temperature,
             y = pred, 
             color = PredType)) +
  geom_point() +
  geom_point(aes(y = GrowthRate), alpha = .75, color = "Black") +
  theme_minimal()

fullpreds %>% 
  ggplot(aes(x = Nitrogen,
             y = pred, 
             color = PredType)) +
  geom_point() +
  geom_point(aes(y = GrowthRate), alpha = .75, color = "Black") +
  geom_smooth(aes(y = GrowthRate), alpha = .75, color = "Black") +
  geom_smooth() +
  theme_minimal()

fullpreds %>% 
  ggplot(aes(x = Humidity,
             y = pred, 
             color = PredType)) +
  geom_point() +
  geom_point(aes(y = GrowthRate), alpha = .75, color = "Black") +
  theme_minimal()


# Questions####
# Are any of your predicted response values from your best model scientifically meaningless? Explain.

# None of the predicted values fall outside of the possible range,
# meaning that there are no predicted values that are negative or completely out of the question.
# But my 'best' model was certainly not the best out there,
# so I'm sure some of the values are less meaningful than they could be

# In your plots, did you find any non-linear relationships? Do a bit of research online and give a link to at least one resource explaining how to deal with modeling non-linear relationships in R.

# The worst offender of being non-linear looked like GrowthRate ~ Nitrogen.
# Rather than moving in one direction and sticking to it, it formed a nice curve
# Starting low and ending low, with a high point in the middle.
# https://www.r-bloggers.com/2016/02/first-steps-with-non-linear-regression-in-r/

# Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” with a linear model (there are a few ways of doing this)

df2 <- read_csv("../../Data/non_linear_relationship.csv")

# compare the data points to a linear model
df2 %>% 
  ggplot(aes(x = predictor,
             y = response)) +
  geom_point() + 
  geom_smooth(method = "lm")
# it looks exponential, so I'm going to take the easy way out and transform it by log
# (I got a lot of practice with this in Gen Chem 2 Lab)

# transform data using log
df2$log_response <- log(df2$response)

linear_mod <- glm(data = df2, formula = log_response ~ predictor)

df2 %>% 
  ggplot(aes(x = predictor,
             y = log_response)) +
  geom_point() +
  geom_smooth(method = "lm")
# and our newly transformed data fits our linear model much better now

