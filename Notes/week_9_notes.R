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


# show and tell
library(pdftools)

#in order to avoid adobe products
pdftools::pdf_combine(pdf1, pdf2, pdf3, output = "~Desktop.pdf")

#Day 2
library(tidyverse)
library(caret)
library(broom)
library(modelr)
library(kableExtra)

mod1 <- mpg %>% 
  glm(data = .,
      formula = cty ~ displ + drv)
broom::tidy(mod1) %>% #turns summary into a data.frame
  kableExtra::kable() %>% 
  kableExtra::kable_classic(lightable_options = 'hover') #kable makes an interactive table

add_predictions(mpg, mod1) %>% #does the predicting step and adds it for you
  ggplot(aes(x = pred, 
             y = cty)) +
  geom_point()

add_residuals(mpg, mod1) %>% #tells us how far off mod1 is
  ggplot(aes(x = resid, 
             y = cty)) +
  geom_point()

# Cross Validation: Test Model on New Data with Actual Answers####
# Take a random subset of the data and put it aside as a testing set
# It does not improve the model, but gives the reported value of error

mpg$drv %>% table
id <- caret::createDataPartition(mpg$cty, p = 0.8, list = FALSE)
train <- mpg[id, ]
test <- mpg[-id, ]


#Train Model on training set

mod2 <- glm(data = train,
            formula = mod1$formula)

#Real Test: Gives the True Value of Error
add_predictions(test, mod2) %>% 
  mutate(error = abs(pred - cty)) %>% 
  pluck('error') %>% 
  summary()

#Bad Test
add_predictions(mpg, mod1) %>% 
  mutate(error = abs(pred - cty)) %>% 
  pluck('error') %>% 
  summary()

iris
library(vegan)
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Petal.Length,
             color = Species)) +
  geom_point() +
  stat_ellipse()

mat <- 
  iris %>% 
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) %>% 
  as.matrix()

#Permutational ANOVA

adonis2(mat ~ iris$Species)

mds <- metaMDS(mat)
data.frame(species = iris$Species,
           mds1 = mds$points[,1],
           mds2 = mds$points[,2]) %>% 
  ggplot(aes(x = mds1,
             y = mds2,
             color = species)) +
  geom_point() +
  stat_ellipse()

kmeans()

#look into tidy_clust for this type of thing, handles the merging step for you