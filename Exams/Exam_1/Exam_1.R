#Exam 1 
#(Rachel Hatton)

#I.####
#Read the cleaned_covid_data.csv file into an R data frame. (20 pts)

library(tidyverse)

covid_data <- read_csv("./data/cleaned_covid_data.csv")
      
#II.####
#Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)

A_states <- 
  covid_data %>% 
  filter(grepl("^A", Province_State))

#III.####
#Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)

ggplot(A_states, mapping = aes(x = Last_Update,
                               y = Deaths,
                               color = Province_State)) +
  geom_point() +
  geom_smooth(se = FALSE, color = 'gray38') +
  facet_wrap(~ Province_State, scales = "free") +
  theme_bw()

#IV.####
#(Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)

state_max_fatality_rate <- 
  covid_data %>% 
  group_by(Province_State) %>% 
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio,
                                         na.rm = TRUE)) %>% 
  arrange(desc(Maximum_Fatality_Ratio))

#V.####
#Use that new data frame from task IV to create another plot. (20 pts)

ggplot(state_max_fatality_rate, mapping = aes(x = fct_reorder(Province_State, -Maximum_Fatality_Ratio),
                                              y = Maximum_Fatality_Ratio)) +
  geom_col(fill = 'slateblue3') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.4, 
                                   hjust = 1)) +
  labs(x = "Province_State")

#VI.####
#(BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time

covid_data %>%
  group_by(Last_Update) %>%
  summarize(Cumulative_Deaths = sum(Deaths)) %>% 
  mutate(Total_Deaths = cumsum(Cumulative_Deaths)) %>%
  ggplot(mapping = aes(x = Last_Update,
                       y = Total_Deaths)) +
  geom_line(color = "olivedrab4", size = 1) +
  theme_bw()
