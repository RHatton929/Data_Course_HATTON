library(tidyverse)
library(easystats)
library(palmerpenguins)

# Does body_mass_g vary significantly between species?

unique(penguins$species)

peng <- glm(data = penguins, 
            body_mass_g ~ species)
summary(peng)

# Yes. Adelie is the comparator bc alphabetical, so it is at intercept.
# At least one of these species is different by body mass

# So far every model we've done has had a continuous outcome variable
# Let's learn binary outcome variable

names(penguins)

#Let's predict whether a given penguin is Gentoo based on bill_l/d, flipper, and body_m

mod <- penguins %>% 
  mutate(gentoo = case_when(species == "Gentoo" ~ TRUE,
                            TRUE ~ FALSE)) %>% 
  glm(data = .,
      formula = gentoo ~ bill_depth_mm + bill_length_mm + body_mass_g + flipper_length_mm,
      family = 'binomial') #logistic regression

check_model(mod)
summary(mod)
penguins$pred <- predict(mod, penguins, type = 'response') #remember to type response

penguins %>% 
  ggplot(aes(x = body_mass_g, y = pred, color = species)) +
  geom_point()

preds <- 
  penguins %>% 
  mutate(outcome = case_when(pred < 0.01 ~ 'not gentoo',
                            pred > 0.75 ~ 'gentoo')) %>% 
  select(species, outcome) %>% 
  mutate(correct = case_when(species == "Gentoo" & outcome == 'gentoo' ~ TRUE,
                             species != "Gentoo" & outcome == 'not gentoo' ~ TRUE,
                             TRUE ~ FALSE))

# How accurate is my model?
preds %>% 
  pluck("correct") %>% 
  sum()/nrow(preds)

# New one
dat <- read_csv("../Data/GradSchool_Admissions.csv")
str(dat)

mod2 <- glm(data = dat,
            formula = as.logical(admit) ~ (gre + gpa) * rank,
            family = 'binomial')

dat$pred <- predict(mod2, dat, type = 'response')

dat %>% 
  ggplot(aes(x = gpa, y = pred, color = factor(rank))) +
  geom_point() +
  geom_smooth()

