#Assignment 2 (Rachel Hatton)

#Task 4: Save all csv files in Data as an object

csv_files <- list.files(path = "Data", pattern = ".csv", recursive = TRUE)

#Task 5: Find how many files match that description using the length() function

length(csv_files)

#Task 6: Store the wingspan_vs_mass.csv file as object "df"

df <- read.csv(list.files(pattern = "wingspan_vs_mass.csv", recursive = TRUE))

#Task 7: Inspect the first 5 lines of "df" using the head() function

head(df, n=5)

#Task 8: Find any files (recursively) in the Data/ directory that begin with the
#letter “b” (lowercase)

list.files(path = "Data", pattern = "^b", recursive = TRUE)

#Task 9: Write a command that displays the first line of each of those “b” files

b_files <- list.files(path = "Data", pattern = "^b", recursive = TRUE, full.names = TRUE)

for (i in b_files) {
  print(readLines(i, n=1))
}

#Task 10: Do the same thing for all files that end in “.csv”

all_csv <- list.files(pattern = ".csv", recursive = TRUE)

for (i in all_csv) {
  print(readLines(i, n=1))
}