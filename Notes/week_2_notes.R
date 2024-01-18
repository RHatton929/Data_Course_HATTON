getwd()
list.files(path = "Assignments/", recursive = TRUE)

list.files(pattern = ".csv", recursive = TRUE)

x <- list.files(pattern = ".csv", recursive = TRUE) #made a chr variable of all csv files

readLines( "Data/wide_rent_income.csv")
df <- read.csv( "Data/wide_rent_income.csv")
read.csv( "Data/wide_rent_income.csv")

x[159]
read.csv(x[159])
dat <- read.csv(x[159])

#for loop practice
x <- c(1,2,3,4,5)

for (i in x){
  print(1+i)
}

#good for us, R is vectorized! For loops rarely needed.
x^3
1:5
1:5 + 1:5
seq(1,1000, by=7)

#get info about variable
class(x)
length(x)
length(length(x)) #should be one, bc length(x) prints one value
class(length(x)) #should still be numeric
class(class(x)) #will be character, bc it is a string in quotes

1+2 #3
"1" +2 #error message
"a" + "2" #error message
c("a", "2") #makes a char list of a, 2
c("a", 2) #makes a character list, bc all vectors must contain same class

#Vector: Has a Class (numeric, integer, character, logical)
#Has one dimension
#Must all be same class
#Length of a vector can = 0, ex. below
length(c())

#vector practice
a <- 1:10
b <- 2:11
c <- letters[1:10]
#c <- letters[c(1,3,5)]
d <- rep(TRUE, 10)

cbind(a,b,c)
z <- data.frame(a,b,c,d) #new data type! A list of vectors
#find the class, length, and dimensions of z
class(z)
length(z)
dim(z)

#logical class vectors
1 > 0
0 >= 0
3 < 1
1 == 1
1 != 1
5 > a
5 > length(a)
a[5>a]
z[5>a,] #data frames are two dimensional (row, column)
z[2,3] #second row, third column
z[1,] # first row, all columns
z[,1] # all rows of column one

#all rows of z where column c is equal to "b"
z[c=="b",]

#built in data set
iris
?iris

iris[iris$Sepal.Length > 5,]
nrow(iris[iris$Sepal.Length > 5,])

big_iris <- iris[iris$Sepal.Length > 5,]
#create a new column in bigiris that ==sepal length*sepal width
big_iris$Sepal.Area <- big_iris$Sepal.Length*big_iris$Sepal.Width

#show just "setosa" from big_iris
big_iris[big_iris$Species=="setosa",]
big_setosa <- big_iris[big_iris$Species=="setosa",]

#give the mean sepal area from big_setosa
mean(big_setosa$Sepal.Area)
plot(big_setosa$Sepal.Length, big_setosa$Sepal.Width)
sd(big_setosa$Sepal.Area)
sum(big_setosa$Sepal.Area)
min(big_setosa$Sepal.Area)
max(big_setosa$Sepal.Area)
summary(big_setosa$Sepal.Area)
cumsum(big_setosa$Sepal.Area)
cumprod(big_setosa$Sepal.Area)

#Practice HW: Do a bunch of vector practice
#in one statement: where setosa, and >5, where setosa or other species, QUERY!!!

#R package of the week: qr_code package
install.packages("qrcode")
library(qrcode)
url <- "website"
qr <- qrcode::qrcode(url)
plot(qr)

install.packages("tidyverse")
library(tidyverse)
