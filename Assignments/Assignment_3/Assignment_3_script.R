# YOUR REMAINING HOMEWORK ASSIGNMENT (Fill in with code) ####

# 1.  Get a subset of the "iris" data frame where it's just even-numbered rows

seq(2,150,2) # here's the code to get a list of the even numbers between 2 and 150

even_iris <- iris[seq(2,150,2),]
View(even_iris)

# 2.  Create a new object called iris_chr which is a copy of iris, except where every column is a character class

iris_chr <- apply(iris, 2, as.character)

# 3.  Create a new numeric vector object named "Sepal.Area" which is the product of Sepal.Length and Sepal.Width

Sepal.Area <- iris$Sepal.Length*iris$Sepal.Width
class(Sepal.Area)

# 4.  Add Sepal.Area to the iris data frame as a new column

iris$Sepal.Area <- iris$Sepal.Length*iris$Sepal.Width
names(iris)

# 5.  Create a new dataframe that is a subset of iris using only rows where Sepal.Area is greater than 20 
# (name it big_area_iris)

# Base R: big_area_iris <- iris[iris$Sepal.Area > 20,]
# tidyverse:

library(tidyverse)

big_area_iris <- iris %>% 
  filter(Sepal.Area > 20)

# 6.  Upload the last numbered section of this R script (with all answers filled in and tasks completed) 
      # to canvas
      # I should be able to run your R script and get all the right objects generated
