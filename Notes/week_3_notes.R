mtcars

#build a data frame from mtcars with only rows that have more than 4 cyl

more_cyl <- mtcars[mtcars$cyl > 4,]

#pull out just the miles per gallon of those cars (mpg) and find the mean, min, and max

mean(more_cyl$mpg)
min(more_cyl$mpg)
max(more_cyl$mpg)

#object types ####
##numeric ####
1:10

##logical ####
c(TRUE, FALSE, TRUE)

##character ####
letters[3]

##integer ####
c(1L, 2L, 3L)

##data.frames ####
mtcars[rows, columns]
str(mtcars) #structure of data.frame
names(mtcars) #character vector with column names
View(mtcars) #show data
mtcars[,"mpg"]

##factor ####
#annoying but useful
as.factor(letters) #stored as levels
hair_color <- c("brown","blond", "black", "red", "red", "black")
as.factor(hair_color) #levels just show possible options,
c(as.factor(hair_color), "purple") #concat doesn't work because levels are numbers
hair_factor <- as.factor(hair_color)
levels(hair_factor) #show what levels are possible

#type conversions####
1:5 #numeric
as.character(1:5) #converts to character
as.numeric(letters) #letters have no inherent value, so NA
as.numeric(c("1", "b", "35")) #will convert what is applicable
as.logical(c("true","t", "T", "F","false")) #case sensitive for single letter
as.roman(1:5) #ROMAN NUMS

x <- as.logical(c("true","t", "T", "F","false"))
sum(x) #NA is missing data
sum(x, na.rm = TRUE) #remove NA from data

##practice: built-in df####
#turn all data in mtcars into character type
for(col in names(mtcars)){
  mtcars[,col] <- as.character(mtcars[,col])
}
str(mtcars)

#reset built-in data.frame to normal
data("mtcars")

##practice: csv df####
#read in a csv file and convert to character
df <- read.csv("./Data/cleaned_bird_data.csv")
str(df)

for(col in names(df)){
  df[,col] <- as.character(df[,col])
}

#write new file to computer
write.csv(df, "./Data/character_bird_new") #never edit original file, data sets are READ ONLY

# 'apply' family functions####
apply(mtcars, 2, as.character) #row = 1, column = 2
apply(mtcars, 2, as.logical) #positive numbers are T, negatives are F
apply(mtcars,2, as.factor) #if you want it factor for some reason

#lapply(list, function) for working with lists


#Packages####
library(tidyverse) #load in every time

#control shift m for weird lil symbol
mtcars %>% 
  filter(mpg > 19) #filter helps us subset df by rows

mtcars %>% 
  filter(mpg > 19 & vs == 1)

# %>% pipe, thing on the left becomes first argument for thing on the right
mtcars$mpg %>% mean()


#the following are functionally the same, but the latter is cleaner
abs(mean(mtcars$mpg))

mtcars$mpg %>% 
  mean() %>% 
  abs()