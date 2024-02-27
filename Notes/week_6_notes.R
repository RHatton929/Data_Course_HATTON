library(tidyverse)
library(janitor)
library(skimr)

df <- read_csv("./Data/Bird_Measurements.csv")

skim(df)

#female data
fdat <-
  df %>%
  clean_names() %>% 
  select(-starts_with("unsexed"), -starts_with("m_"), -ends_with("_n")) %>% 
  mutate(sex = "female") 

names(fdat) <- names(fdat) %>% str_remove("f_")

#male data
mdat <-
  df %>%
  clean_names() %>% 
  select(-starts_with("unsexed"), -starts_with("f_"), -ends_with("_n")) %>% 
  mutate(sex = "male")

names(mdat) <- names(mdat) %>% str_remove("m_")

#unsexed data
udat <-
  df %>%
  clean_names() %>% 
  select(-starts_with("f_"), -starts_with("m_"), -ends_with("_n")) %>% 
  mutate(sex = "unsexed")

names(udat) <- names(udat) %>% str_remove("unsexed_")

#rejoin d.f
clean <- 
  mdat %>% 
   full_join(fdat) %>% 
   full_join(udat)

#look through Zahn's bad data set, identify the many things that are wrong with it
##Show and Tell
library(measurements)

x <- c(12,31,44)
measurements::conv_unit(x, from = 'inch', to = 'ft')
measurements::conv_unit(x, from = 'inch', to = 'parsec')

#library(janitor)
#library(tidyverse)
#libbrary(readxl)

#clean_names(path)

#sites <- c(South Oak Spring Site 1)
#sites[1] %>%  str_replace_all(" ", "_")
#trap_days <- read_xlsx(path, sheet = sites[1], range = "B17:I17", col_names + FALSE)

#SOS2 <- read_xlsx(path, sheet = sites[1], range = "A2:I12") %>% 
    #pivot_longer(-species, names_to = "month", values_to = "obs_count") %>% 
    #mutate(site = sites[1],
          # month = str_to_sentence(month),
          # species = str_to_sentence(species))

#SOS2 <- SOS2 %>% 
    #full_join(data.frame(month = SOS2$month %>% unique, 
    #trap_days = trap_days[1,] %>% as.numeric()))

##Functionalize It####
#same file, different ranges and sheet number, enforce capitalization

#to make a function in R:
#function()
#then give it the variables

#read_trap_data <- 
# function(path, sheet, range1, range2){
#   trap_days <- read_xlsx(path, sheet = sheet, range = range1, col_names + FALSE)

#   x <- read_xlsx(path, sheet = sheet, range = range 2) %>% 
#     clean_names() %>% 
#     mutate(across(-species, as.numeric)) %>% 
#     pivot_longer(-species, names_to = "month", values_to = "obs_count") %>% 
#     mutate(site = sheet,
#             month = str_to_sentence(month),
#             species = str_to_sentence(species),
#             month = case_when(str_detect(month, "[J,j]an" ~ "January", Do all 12 months)

#   x <- x %>% 
#       full_join(data.frame(month = x$month %>% unique, 
#       trap_days = trap_days[1,] %>% as.numeric()))
#   return(x)
#}

#USE IT

#North_Oak_Spring_Site_1 <- 
#   read_trap_data(path = path, sheet = sites[2], range1 = "B15:15", range2 = "A2:I12")
