# Loading libraries

library(tidyverse)
library(ggplot2)
library(janitor)
library(stringr)
library(lubridate)
library(stringi)
library(patchwork)
library(gganimate)
library(lavaan)
library(gt)
library(gtsummary)
library(skimr)


### Getting the dataset ready

# Loading dataset
load("raw-data/odimetadata.Rdata")

#Adding year, over and wicket variables
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
  group_by(column_label, team) %>%
  mutate(bat_pos = match(batsman, unique(batsman))) %>%
  ungroup() %>%
  group_by(bat_pos, column_label, team) %>%
  mutate(pos_runs = sum(totalRuns)) %>%
  mutate(wicket = if_else(wicketPlayerOut=="nobody",0,1),
    phase = case_when(
    over %in% 0:10 ~ "First Ten",
    over %in% 11:20  ~ "Middle One",
    over %in% 21:30 ~ "Middle Two",
    over %in% 31:40 ~ "Middle Three",
    over %in% 41:50 ~ "Last Ten"),
    pp_rule = if_else(date>=2015-06-26,1,0),
    nb_rule = if_else(date>=2011-09-29,1,0),
    bo_rule = if_else(date>=2012-10-30,1,0),
    bt_rule = if_else(date>=2017-09-28,1,0)) %>%

#Adding in the columns for the number of runs scored in each phase in 25-run increments
  group_by(column_label, team) %>%
  mutate(inning_runs = sum(totalRuns),
         inning_wickets = sum(wicket)) %>%
  ungroup() %>%
  group_by(phase, column_label, team) %>%
  mutate(phase_runs = sum(totalRuns),
         phase_wickets = sum(wicket)) %>%
  ungroup() %>%
  mutate(bracket = case_when(
    phase_runs %in% 0:25 ~ "0 to 25 runs",
    phase_runs %in% 25:50 ~ "25 to 50 runs",
    phase_runs %in% 50:75  ~ "50 to 75 runs",
    phase_runs %in% 75:100 ~ "75 to 100 runs",
    phase_runs >100 ~ "More than 100 Runs")) %>%
  
# Adding in columns for the average runs scored in each phase 
  group_by(year, phase) %>%
  mutate(average_runs = mean(phase_runs)) %>%
  ungroup()

# Making phase a factor variable

data$phase <- ordered(data$phase, levels = c("First Ten", "Middle One", "Middle Two", "Middle Three", "Last Ten"))

# Making summary stats table
x <- data %>% skim(pos_runs, inning_runs, inning_wickets, phase_runs)

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
    
# Making the first plot, by first calculating win-percentage

p1 <- 
data %>%
  filter(team=="Pakistan") %>%
  group_by(bracket, year) %>%
  mutate(win = ifelse(winner=="Pakistan",1,0)) %>%
  mutate(prop_won = sum(win)/n()) %>%
  ungroup() %>%
  filter(year == 2017) %>%
  ggplot(aes(prop_won, average_runs)) +
  geom_col() +
  labs(title = 
         "Average Runs Scored by Batsman", 
       subtitle = "Period: 2010-2017",
       x = "Batting Position", 
       y = "Average Runs in the Year") +
  labs(caption = "Source: cricsheet.org") +
  theme_classic()+ 
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))
  
# Making the second plot that shows how run-scoring has evolved over time

p2 <- data %>%
  group_by(year) %>%
  summarize(inning_runs = mean(inning_runs)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(year), 
             y = inning_runs)) +
  geom_point() +
  labs(title = 
         "Average Runs Scored in Innings over Time", 
       subtitle = "Period: 2010-2017",
       x = "Year", 
       y = "Average Runs",
       col = "Innings Phase") +
  labs(caption = "Source: cricsheet.org") +
  theme_classic()+ 
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) 

# Third plot: runs by batting position in each year

p3 <- 
  data %>%
  group_by(year, bat_pos) %>%
  summarize(pos_runs = mean(pos_runs)) %>%
  ungroup() %>%
  filter(year == 2017) %>%
  ggplot(aes(bat_pos, pos_runs)) +
  geom_col() +
  labs(title = 
         "Average Runs Scored by Batsman", 
       subtitle = "Period: 2010-2017",
       x = "Batting Position", 
       y = "Average Runs in the Year") +
  labs(caption = "Source: cricsheet.org") +
  theme_classic()+ 
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- data %>%
  filter(phase=="First Ten") %>%
  mutate(win = ifelse(winner=="Pakistan",1,0)) %>%
  mutate(prop_won = sum(win)/n()) %>%
  ggplot(aes(x = average_runs, y = prop_won)) +
  geom_point() +
  stat_smooth()

p5 <- 
data %>%
  group_by(year, bat_pos) %>%
  summarize(phase_runs = mean(phase_runs)) %>%
  ungroup() %>%
  filter(year == 2017) %>%
  ggplot(aes(phase, phase_runs)) +
  geom_col() +
  labs(title = 
         "Average Runs Scored in a Phase", 
       subtitle = "Period: 2010-2017",
       x = "Phase", 
       y = "Average Runs Scored") +
  labs(caption = "Source: cricsheet.org") +
  theme_classic()+ 
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))





