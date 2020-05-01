library(tidyverse)
library(ggplot2)
library(janitor)
library(stringr)
library(lubridate)
library(stringi)
library(patchwork)
library(gganimate)
library(gifski)
library(shinythemes)
library(png)


ui <- navbarPage(theme = shinytheme("simplex"), "Cricket Analytics", 
                 
                 tabPanel("About Cricket",
                          fluidPage(
                            h4("What is Cricket")
                          )),
                 
                 tabPanel("Data",
                          fluidPage(HTML(paste(
                                h4("Data Gathering and Procesiong"),
                                "The primary data source used in this project is from", a(href="www.cricsheet.org", "cricsheet.org."),
                                "The website has compiled ball-by-ball data on over 5000 cricket matches.",
                                "The cricket data is grouped according to gender and then further divided based on the match format.", 
                                 "In this project, I used the yaml files for,",
                                tags$a(href="www.cricsheet.org/downloads/odis_male.zip","One Day Internationals"), "for men.",
                                '</br>',
                                "Using the",tags$a(href="https://cran.r-project.org/web/packages/yorkr/yorkr.pdf","yorkr"), "package,
                                I first converted the yaml files that contained the commentary data into .RData files. I then compiled 
                                the separate RData files into two seperate meta-datasets, one for each format.",
                                '</br>',
                                "All the data analysis is conducted using information from these two datasets."
                              )))),
                 
                 
                 tabPanel("Run-Scoring Trends",
                          fluidPage(
                            h4("Evoloution of Run Scoring over Time"),
                            plotOutput("plot2"),
                            p("The graph above shows the trends in run-scoring between 2006-2020.")
                            
                          )),
                 
                 tabPanel("Regression",
                          fluidPage(
                            h4("Evoloution of Run Scoring over Time"),
                          )),
                 
                 
  tabPanel("Analytics",
           fluidPage(
             h4("Win Percentage and Run Scoring"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("countryInput", "Country",         
                             choices = c("Choose one" = "", "Australia", "England","India", "Pakistan",
                                         "New Zealand", "Sri Lanka", "South Africa",
                                         "Zimbabwe", "Bangladesh"))),
               mainPanel(plotOutput("plot"))))),
  
  
  tabPanel("Background",
           fluidPage(
             HTML(
               paste(
                 h4("Motivation"),
                 "Hello! Welcome to my final project for GOV - 1005, 
      where I analyze granular data on over a thousand cricket 
      matches to glean interesting insights about the leading 
      cricketers and their teams.",'<br />', 
                 
                 '</br>',"I chose to focus my analysis on cricket because I am 
      from Pakistan and have been an avid follower of the sport
      for over a decade. In college, I find it hard to follow 
      the game as much as I would like to, so this project is 
      a way for me to further explore and contribute to 
      cricket, a sport that is an important part of my life. ",
                 
                 h4("Overview"),
                 
                 "Since 2011, ODI Cricket has changed drastically and
      the rate of run-scoring has increased substantially.
      For instance, the records for the highest ODI total, 
      the fastest century and half-centuy scored by Batsmen, 
      and numerous other batting records have been broke. 
      Experts have suggested that these changes are 
       the result of a number of a number of changes that were
       instituted following 2011.",'<br/>',
                 
                 '<br/>', "The purpose of this project, therefore, is to
      examine whether that is true. To do so, it looks at the 
      when drastic changes in the run-scoring have occured 
      and whether the changes are in-line with what was 
      predicted by experts. Taking the analysis a step further
      the project further looks at whether these changes have
      had a substantial impact on the way that ODI games
      are won"
               )))))

server <- function(input, output) {
  
  load("./data/data.Rdata")
  
  output$plot2 <- 
    renderPlot({
     data %>%
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
    })
  
    output$plot <-renderPlot({
        p1 <- data %>%
          filter(phase=="First Ten") %>%
          group_by(bracket, year) %>%
          mutate(win = ifelse(winner==input$countryInput,1,0)) %>%
          mutate(prop_won = sum(win)/n()) %>%
          ungroup() %>%
          ggplot() + geom_point(aes(x = factor(year), 
                                               group = factor(bracket),                                
                                               color = factor(bracket), y = prop_won), 
                                           size=2) +            
            labs(title = "Proportion of Matches won Sorted by Runs Scored in the \n First Ten Overs", subtitle = "Sorted According to Run Bracket - Period: 2010-2017",   x = "Year", y = "Proportion Won", col = "Runs Scored Bracket") +  labs(caption = "Source: cricsheet.org") + theme_classic()+  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
        
      p2 <- data %>%
      filter(phase=="Last Ten") %>%
      group_by(bracket, year) %>%
      mutate(win = ifelse(winner==input$countryInput,1,0)) %>%
      mutate(prop_won = sum(win)/n()) %>%
      ungroup() %>%
      ggplot() + geom_point(aes(x = factor(year), 
                                                     group = factor(bracket),                                
                                                     color = factor(bracket), y = prop_won), 
                                                 size=2) +            
          labs(title = "Proportion of Matches won Sorted by Runs Scored in the \n Last Ten Overs", subtitle = "Sorted According to Run Bracket - Period: 2010-2017",   x = "Year", y = "Proportion Won", col = "Runs Scored Bracket") +  labs(caption = "Source: cricsheet.org") + theme_classic()+  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))
        p1+p2+plot_layout(ncol=1)
        })

    }

shinyApp(ui = ui, server = server)
