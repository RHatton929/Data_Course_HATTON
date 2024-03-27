library(tidyverse)
library(GGally)
library(easystats)
library(MASS)
library(modelr)

df <- read_csv("../../Data/GradSchool_Admissions.csv") %>% 
  mutate(admit = as.logical(admit))

glimpse(df)

ggpairs(df)

df %>% 
  ggplot(aes(x = admit,
             y = gpa, 
             fill = admit)) +
  geom_violin(alpha = 0.5) +
  facet_wrap(~rank) +
  theme_minimal()

df %>% 
  ggplot(aes(x = admit,
             y = gre, 
             fill = admit)) +
  geom_violin(alpha = 0.5) +
  facet_wrap(~rank) +
  theme_minimal()

names(df)

mod1 <- glm(data = df, 
            formula = admit ~ gre + gpa,
            family = "binomial")

mod2 <- glm(data = df,
            formula = admit ~ gre * gpa,
            family = "binomial")

mod3 <- glm(data = df, 
            formula = admit ~ gre + gpa + rank,
            family = "binomial")

mod4 <- glm(data = df,
            formula = admit ~ gre * gpa + rank,
            family = "binomial")

mod5 <- glm(data = df,
            formula = admit ~ gre * gpa * rank,
            family = "binomial")

step <- stepAIC(mod5)

step$formula

mod6 <- glm(data = df,
            formula = step$formula,
            family = "binomial")

comps <- compare_performance(mod1, mod2, mod3, mod4, mod5, mod6, rank = TRUE)

comps

comps %>% plot()

df %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6, type = "response") %>%
  ggplot(aes(x = admit,
             y = pred,
             fill = admit)) +
  geom_violin(alpha = 0.5) +
  facet_wrap(~model) +
  theme_minimal()