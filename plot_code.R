# Loading libraries

library(tidyverse)
library(ggplot2)
library(janitor)
library(stringr)
library(lubridate)
library(stringi)
library(infer)
library(purr)
library(patchwork)
library(haven)
library(gt)
library(skimr)
library(broom)


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
  

# Wrangling the data-set to include columns for each phase
# of the game and for total runs scored in the phase,
# total wickets taken and variable indicating the position
# of bowlers and batsmen 
data <- subset %>% 
  na.omit(totalRuns) %>%
  group_by(column_label, team) %>%
  mutate(bat_pos = match(batsman, unique(batsman)),
         bow_pos = match(bowler, unique(bowler))) %>%
  ungroup() %>%
  group_by(bat_pos, column_label, team) %>%
  mutate(pos_runs = sum(totalRuns)) %>%
  ungroup() %>%
  mutate(wicket = if_else(wicketPlayerOut=="nobody",0,1),
    phase = case_when(
    over %in% 0:10 ~ "First Ten",
    over %in% 11:20  ~ "Middle One",
    over %in% 21:30 ~ "Middle Two",
    over %in% 31:40 ~ "Middle Three",
    over %in% 41:50 ~ "Last Ten"),
    pp_rule = if_else(date>=as.Date("06/26/2015", format = "%M/%d/%Y"),1,0),
    nb_rule = if_else(date>=as.Date("09/29/2011", format = "%M/%d/%Y"),1,0),
    bo_rule = if_else(date>=as.Date("10/30/2012", format = "%M/%d/%Y"),1,0),
    bt_rule = if_else(date>=as.Date("09/28/2017", format = "%M/%d/%Y"),1,0)) %>%
    group_by(bow_pos, column_label, team) %>%
    mutate(pos_wickets = sum(wicket)) %>%
    ungroup() %>%

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
    
  
# Making a plot that shows how run-scoring has evolved 
# over time. Created a variable by year of the mean inning
# runs in a year

p1 <- data %>%
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

# Making a plot that shows how wicket-taking has evolved 
# over time. Created a variable by year of the mean wickets
# taken a year

p2 <- 
  
  data %>%
  group_by(year) %>%
  summarize(inning_wickets = mean(inning_wickets)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(year), 
             y = inning_wickets)) +
  geom_point() +
  labs(title = 
         "Average Wickets Taken in Innings over Time", 
       subtitle = "Period: 2010-2017",
       x = "Year", 
       y = "Average Runs",
       col = "Innings Phase") +
  labs(caption = "Source: cricsheet.org") +
  theme_classic()+ 
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1)) 

# Third plot: runs by batting position in each year
# Created variable of mean runs by batting position
# for each year

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


# Histogram for runs in a phase created by first
# making a variable for the mean runs in 
# phase for each year

p4 <-
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

# Histogram for wickets in a phase created by first
# making a variable for the mean wickets in 
# phase for each year

p5 <- 
  data %>%
  filter(team == "Pakistan") %>%
  group_by(year, phase) %>%
  summarize(phase_wickets = mean(phase_wickets)) %>%
  ungroup() %>%
  filter(year == 2017) %>%
  ggplot(aes(phase, phase_runs)) +
  geom_col() +
  labs(title = 
         "Average Wickets Taken in a Phase", 
       subtitle = "Period: 2010-2017",
       x = "Phase", 
       y = "Average Wickets Taken") +
  labs(caption = "Source: cricsheet.org") +
  theme_classic()+ 
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))


# Histogram for wickets in a inning created by first
# making a variable for the mean wickets in 
# phase for each year

p6 <- 
  data %>%
  filter(team == "Pakistan") %>%
  group_by(bow_pos, year) %>%
  summarize(inning_wickets = mean(pos_wickets)) %>%
  ungroup() %>%
  filter(year == 2017) %>%
  ggplot(aes(bow_pos, pos_wickets)) +
  geom_col() +
  labs(title = 
         "Average Wickets Taken", 
       subtitle = "Period: 2010-2017",
       x = "Bowler Position", 
       y = "Average Wickets Taken") +
  labs(caption = "Source: cricsheet.org") +
  theme_classic()+ 
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Code to make regression table

regtable <- data %>%
  lm(inning_runs ~ bt_rule, data = .) %>%
  tidy(conf.int=TRUE) %>% 
  select(Variable = term,
         Estimate = estimate,
         `Lower Bound` = conf.low,
         `Upper Bound` = conf.high) %>%
  gt() %>% 
  tab_header(title = "Effect of Number of Tweets and Poll Quality on Reported Approval Rating",
             subtitle = "Data from fivethirtyeight and Trump Tweet Archive")


