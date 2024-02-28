# SETUP ####
library(tidyverse)
library(readxl)
library(measurements)

# DATA ####
path <- "./human_heights.xlsx"
dat <- read_xlsx(path)

# CLEAN ####
dat <- 
dat %>% 
  pivot_longer(everything(),
               names_to = "sex",
               values_to = "height") %>% 
  separate(height, into = c("feet","inches"),convert = TRUE) %>% 
  mutate(inches = (feet*12) + inches) %>% 
  mutate(cm=conv_unit(inches, from='in',to='cm'))

dat %>% 
  ggplot(aes(x=cm,fill=sex)) +
  geom_density(alpha=.5)

# HYPOTHESIS TESTING ####
# T-Test: Determine if the means of exactly two groups are different
# ~ means "as a function of"
# left-hand side of formula is for outcome variable (y)

t.test(dat$cm ~ factor(dat$sex))

#Reject null hypothesis

# p-value: assuming null hyp is true, the likelihood of seeing this result
# the smaller the number, the more 'shocked' we are to see this result (<0.05)

mod <- 
  glm(data = dat,
    formula = cm ~ sex)

summary(mod)

mod$coefficients

# generalized linear model: y = mx + b
# cm = 7.466(sexmale) + 168.256 

# PRACTICE ####

names(mpg)

mpg %>% 
  ggplot(aes(x = displ,
             y = cty)) +
  geom_point() +
  geom_smooth(method ="glm")

mod <- 
  glm(data = mpg,
    formula = cty ~ displ)

# mileage in the city = -2.6305(displacement) + 25.9915
# reject the null hypothesis, there is an effect of displacement on the mileage one gets in the city
# the point of modeling is to predict

## All Models are Wrong, Some are Useful ####

library(easystats)

report(mod)
performance(mod)
performance::check_model(mod)

data.frame(A = rnorm(50000, mean = 0, sd = 1),
           B = rnorm(50000, mean = 5, sd = 1)) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density()


