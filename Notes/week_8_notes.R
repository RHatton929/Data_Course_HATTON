library(tidyverse)
library(easystats)
library(MASS)

# use mpg data
# build a model that predicts cty as a function of displ

mod1 <- glm(data = mpg, cty ~ poly(displ, 2))

mod1$coefficients # how much the line moves per one unit of x
mod1$residuals # the distance of each point from the predictor line
mod1$formula

names(mpg)

mod2 <- glm(data = mpg, cty ~ displ + cyl)
mod3 <- glm(data = mpg, cty ~ displ * cyl)

#mod1
mpg %>% ggplot(aes(x = displ, y = cty)) +
  geom_smooth(method = "glm")

#mod3: displacement * cyl AKA the model Depends on cyl
mpg %>% ggplot(aes(x = displ, y = cty, color = factor(cyl))) +
  geom_smooth(method = "glm")

compare_models(mod1, mod2, mod3)
compare_performance(mod1, mod2, mod3, mod4) %>% plot()

#predict variables
predict(mod3, mpg)

predict(mod1, data.frame(displ = 1:100)) #cannot predict outside of training data

mpg$pred <- predict(mod1, mpg)
mpg$pred2 <- predict(mod2, mpg)
mpg$pred3 <- predict(mod3, mpg)
mpg$pred4 <- predict(mod4, mpg)

mpg %>% 
  ggplot(aes(x = cty, y = pred)) +
  geom_point()

mpg %>% 
  ggplot(aes(x = cty, y = pred2)) +
  geom_point()

mpg %>% 
  ggplot(aes(x = cty, y = pred3)) +
  geom_point()

mpg %>% 
  pivot_longer(starts_with("pred")) %>% 
  ggplot(aes(x = displ,
             y = cty,
             color = factor(cyl))) +
  geom_point() +
  geom_point(aes(y = value),
             color = 'black') +
  facet_wrap(~name)


mpg <- 
  mpg %>% 
  mutate(auto = grepl("auto", mpg$trans))

mod4 <- glm(data = mpg, cty ~ displ * cyl * auto)
summary(mod4)

#This is Gonna Break R, Save File Before running Step
#mod5 <- glm(data = mpg, cty ~ (manufacturer + model + displ + year + cyl + trans + drv
                              # + fl + class + auto)^2)
formula(mod5)

step <- stepAIC(mod4)
step$formula

step2 <- stepAIC(mod5)

#mod_best <- glm(data = mpg, formula = formula(step)

mod_best <- glm(data = mpg, cty ~ displ+year+cyl*trans*drv*fl+class)
compare_performance(mod1, mod2, mod3, mod4, mod_best) %>% plot()

mpg$pred_best <- predict(mod_best, mpg)
summary(mod_best)

check_model(mod_best)
#the more complicated the model, the less interpret-able. Keep it 5th grade for now

