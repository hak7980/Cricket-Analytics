# Loading libraries

library(tidyverse)
library(ggplot2)
library(janitor)
library(stringr)
library(lubridate)
library(stringi)
library(patchwork)
library(gganimate)


?save

### Getting the dataset ready

# Loading dataset
load("raw-data/odimetadata.Rdata")

#Adding year and over variables
subset <- 
  results %>%
  mutate(year = year(as.Date(
    date, format = "%m/%d/%Y"))) %>%
  mutate(over = stri_extract(ball, regex = "[.](.*)")) %>%
  mutate(over = substring(over, 2)) %>%
  mutate(over = gsub("\\..*","", over)) 

# Wrangling the data-set to include columns for each phase of the game
data <- subset %>% 
  na.omit(totalRuns) %>%
  mutate(phase = case_when(
    over %in% 0:10 ~ "First Ten",
    over %in% 11:20  ~ "Middle One",
    over %in% 21:30 ~ "Middle Two",
    over %in% 31:40 ~ "Middle Three",
    over %in% 41:50 ~ "Last Ten")) %>%

#Adding in the columns for the number of runs scored in each phase in 25-run increments
  group_by(phase, column_label) %>%
  mutate(phase_runs = sum(totalRuns)) %>%
  ungroup() %>%
  mutate(bracket = case_when(
    phase_runs %in% 0:25 ~ "Less than 25 runs",
    phase_runs %in% 25:50 ~ "25 to 50 runs",
    phase_runs %in% 50:75  ~ "50 to 75 runs",
    phase_runs %in% 75:100 ~ "75 to 100 runs",
    phase_runs >100 ~ "More than 100 Runs")) %>%
  
# Adding in columns for the average runs scored in each phase 
  group_by(year, phase) %>%
  mutate(average_runs = mean(phase_runs)) %>%
  ungroup()

save(data, file = "cricket_analytics/data/data.RData")


# Making plot of the proportion of matches won by Pakistan within each run bracket sorted
# accordingto the year the match was played 

# Adding in a column for the number and proportion of matches won by Pakistan in the 
# given year 

datapk <- 
  data %>%
  group_by(bracket, year) %>%
  mutate(win = ifelse(winner=="Pakistan",1,0)) %>%
  mutate(prop_won = sum(win)/n()) %>%
  ungroup()
    
# Making the first plot

p1 <- 
  datapk %>%
  filter(phase=="First Ten") %>%
  ggplot() +
  geom_point(aes(x = factor(year), 
                 group = factor(bracket), 
                 color = factor(bracket), y = prop_won), 
             size=2) +
  labs(title = 
         "Proportion of Matches won by Pakistan Sorted by Runs Scored in the \n First Five Overs", 
       subtitle = "Sorted According to Run Bracket - Period: 2010-2017",
       x = "Year", 
       y = "Proportion Won",
       col = "Runs Scored Bracket") +
  labs(caption = "Source: cricsheet.org") +
  theme_classic()+ 
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))
  
# Making the second plot of the proportion of matches won by Pakistan 
# within each run bracket sorted according to the year the match was played 

p2 <- 
  datapk %>%
  filter(phase=="Last Ten") %>%
  ggplot() +
  geom_point(aes(x = factor(year), 
                 group = factor(bracket), 
                 color = factor(bracket), y = prop_won), 
             size=2) +
  labs(title = 
         "Proportion of Matches won by Pakistan Sorted by Runs Scored in the \n Last Five Overs", 
       subtitle = "Sorted According to Run Bracket - Period: 2010-2017",
       x = "Year", 
       y = "Proportion Won",
       col = "Runs Scored Bracket") +
  labs(caption = "Source: cricsheet.org") +
  theme_classic()+ 
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))


# Making the third plot that shows how run-scoring has evolved over time

p3 <- data %>%
  group_by(year, phase) %>%
  summarize(average_runs = mean(average_runs)) %>%
  ggplot(aes(x = factor(year), 
             y = average_runs,
             group=phase,
             color=phase)) +
  geom_line() +
  labs(title = 
         "Runs Scored in Each Phase", 
       subtitle = "Period: 2010-2017",
       x = "Year", 
       y = "Average Runs",
       col = "Innings Phase") +
  labs(caption = "Source: cricsheet.org") +
  theme_classic()+ 
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))



