library(tidyverse)
library(ggplot2)
library(janitor)
library(stringr)
library(lubridate)
library(stringi)
library(patchwork)
library(gganimate)
library(gifski)
library(skimr)
library(shinythemes)
library(png)
library(htmltools)
library(vembedr)

countries <- c("Choose one" = "", "Australia", "England","India", "Pakistan",
               "New Zealand", "Sri Lanka", "South Africa",
               "Zimbabwe", "Bangladesh", "Afghanistan", "Ireland", 
               "Canada", "Kenya", "Scotland")

years <- c("Choose one" = "", "2006", "2007", "2008",
           "2009", "2010", "2011", "2012", "2013", "2014",
           "2015", "2016", "2017")


ui <- navbarPage(theme = shinytheme("simplex"), "Cricket Analytics", 
                 
                 tabPanel("About the Project",
                          tabsetPanel(
                            tabPanel("What is Cricket?",
                                     fluidRow(div(img(src='ground.png'), style="text-align: center;"),
                                              p("Picture of the MCC in Melbourne, Australia. The world's largest cricket ground in terms of seating capacity", align="center" )),
                                     p("Cricket is a bat and ball game played most commonly played in the 
                          commonwealth countries. It is played between two teams, each consisting of eleven players.
                            First played in the early 16th century, the game is the second most popular sport in the world in terms of viewership. The bulk
                            of the sport's followers are in the commonwealth countries."),
                                     br(),
                                     p("This video below gives a brief overview of cricket:"),
                                     HTML('<center><iframe width="740" height="380" src="https://www.youtube.com/embed/g-beFHld19c" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></br>')),
                            tabPanel("Rules of ODI Cricket",
                                     h3("Brief Overview"),
                                     p("ODI Cricket is a form of cricket that lasts one entire day. It is shorter
                             than Test cricket which lasts a total of 5 days and longer than T20I cricket which 
                             lasts four hours on average."),
                                     p("There are two innings in a ODI game, each consisting of fifty overs.
                              The captain of the side that wins the toss decides whether to field or bat 
                              first. Everyone in the lineup can both bat and bowl, but each player can only bat once 
                              until they are dismissed. Similarly, each bowler can bowl a maximum of ten overs."),
                                     p("In order to win a ODI cricket game, a team needs to ensure that the opposition 
                              scores less runs in their innings than them. For example, if Team A bat first and scores 250 runs,
                              then they need to restrict Team B to less than 250 runs in order to win the game.
                              As a batsman, you atempt to score as many runs as possible in the shortest 
                              number of delivery whereas a bowler attempts to take as many wickets while conceeding
                              as little runs as possible."),
                                     h3("Detailed Explanation"),
                                     HTML(paste("The following explanations are taken from",
                                                a(href="http://www.abcofcricket.com", "abcofcricket.com:"))),
                                     mainPanel(align = "center",
                                               p("In a one-day match, each team bats only once and their
                                innings are limited to a set number of overs, usually fifty,
                                however, this can vary as a result of poor weather etc."),
                                               
                                               p("Other changes to the game include additional restrictions 
                                on where fielders may be placed (preventing teams from 
                                placing every fielder on the edge of the field to prevent
                                boundaries) and stricter rules on wide balls and short 
                                deliveries (to prevent teams from restricting scoring
                                by bowling deliveries that batters have no chance to score 
                                from)."),
                                               
                                               p("In most games, a white ball is used rather than 
                                the traditional red and as a result, the need to paint 
                                rather than stain the white ball gives it subtly different
                                characteristics in flight as it wears. The white ball is
                                generally restricted to matches played during the afternoon
                                and into the evening, such matches are also known as 
                                day-night matches. Day-night matches require the team 
                                batting second to commence their innings under stadium
                                lights as a result of the lack of available natural 
                                sunlight."))),
                            
                            tabPanel("Project Overview",
                                     br(),

                                     p("Since 2011, ODI Cricket has changed drastically and
                                        the rate of run-scoring has increased substantially.
                                        For instance, the records for the highest ODI total, 
                                        the fastest century and half-centuy scored by Batsmen, 
                                        and numerous other batting records have been broke. 
                                        Experts have suggested that these changes are 
                                         the result of a number of a number of changes that were
                                         instituted following 2011.
                                                                  
                                        The purpose of this project, therefore, is to
                                        examine whether that is true. To do so, it looks at the 
                                        when drastic changes in the run-scoring have occured 
                                        and whether the changes are in-line with what was 
                                        predicted by experts. Taking the analysis a step further
                                        the project further looks at whether these changes have
                                        had a substantial impact on the way that ODI games
                                        are won"))
                            
                          )), 
                 tabPanel("Data",
                          tabsetPanel(
                            tabPanel("Data Gathering and Processing",
                                     fluidPage(HTML(paste(
                                       h3("Sourcing"),
                                       "All the primary data used in this project was obtained from",
                                       a(href="www.cricsheet.org", "cricsheet.org."),
                                       "The website has recorded ball-by-ball commentary on all cricket matches played 2006 onwards in a yaml list format.",
                                       "While the website contains information on Test, ODI, and T20I matches for both men and women, I am focusin on",
                                       "men's ODI cricket matches only as they have undergone the most rule changes in recent years.",
                                       h3("Processing"),
                                       "The data processing part involved writing an R-script that downloaded the data from",
                                       "cricsheet, converted it into a Rdata frame and then combined it into a metadataset that",
                                       "contained the information for all the individual matches.",
                                       '</br>',
                                       "To do so, I used the",
                                       a(href="https://cran.r-project.org/web/packages/yorkr/yorkr.pdf","yorkr package"),
                                       "which allowed me to convert the list files into Rdata files, which I then compiled into one large dataset.",
                                       "The code for the entire process can be found in the gather script in the repo for this project.",
                                       h3("Wrangling"),
                                       "Before proceeding to the data-analysis, there were a number of changes that I had to make to the dataset",
                                       "While the list files contained some basic information, more grandular details were missing.",
                                       "Therefore, I added in the variables to be used in my analysis such as",
                                       "the batting position of the current batsman, the run`s scored by them,",
                                       "the total runs and wickets in an inninigs, and the average number of runs scored in each phase of the game.",
                                       "These serve as the key variables used in my analysis",
                                       "and their summary statistics can be found in the next tab."
                                     )))),
                            
                            tabPanel("Summary Statistics",
                                     
                                     tableOutput("sumstats")
                            )
                          )),
                 
                 tabPanel("Visualizations",
                          h4("The Evoloution of Run-Scoring in ODI Cricket"),
                          tabsetPanel(
                            tabPanel("Overall Trends",
                                     h4("Evoloution of Run Scoring over Time"),
                                     plotOutput("plot"),
                                     p("The graph above shows the trends in run-scoring between 2006-2020.")
                            ),
                            tabPanel("Runs Scored in Phase",
                                     selectInput("countryInput", "Country",
                                                 choices=countries),
                                     plotOutput("plot2"),
                                     sidebarPanel(width=12, 
                                       sliderInput("yearInput",
                                                   label = "Year:",
                                                   min = 2006, max = 2017,value=2006, sep="",ticks=FALSE))
                                       ),
                            
                            tabPanel("Runs Scored by Position",
                                     selectInput("countryInput2", "Country",
                                                 choices=countries),
                                     plotOutput("plot3"),
                                     sidebarPanel(width=12,
                                       sliderInput("yearInput2", 
                                                   label = "Year:",
                                                   min = 2006, max = 2017,value=2006,sep="", ticks=FALSE))                                       )
                          )),
                 
                 tabPanel("Regression Results",
                          fluidPage(
                            h4("Have Rule Changes Transformed ODI Cricket?"),
                          )),
                 
                 tabPanel("Background",
                          h3("Motivation"),
                          p("Hello!
                          I hope you enjoyed this final project done for my course, GOV - 1005 taught by Dr. David Kane."), 

                            p("I chose to focus my analysis on cricket because I am 
                            from Pakistan and have been an avid follower of the sport
                            for over a decade. In college, I find it hard to follow 
                            the game as much as I would like to, so this project is 
                            a way for me to further explore and contribute to 
                            cricket, a sport that is an important part of my life."),
                          
                          h3("About Me"),
                          p("My name is Hamid Khan and I am currently a Junior at Harvard College studying Economics with a minor in Statistics. 
                            You can reach me at hamidkhan@college.harvard.edu for any questions about this project.")))


server <- function(input, output) {
  
  load("./data/data.RData")
  
  output$plot <- 
    renderPlot({
      data %>%
        group_by(year) %>%
        summarize(inning_runs = mean(inning_runs)) %>%
        ungroup() %>%
        ggplot(aes(x = factor(year), 
                   y = inning_runs)) +
        geom_point() +
        labs(title = 
               "Average Runs Scored in Each Phase", 
             subtitle = "Period: 2010-2017",
             x = "Year", 
             y = "Average Runs",
             col = "Innings Phase") +
        labs(caption = "Source: cricsheet.org") +
        theme_classic()+ 
        theme(legend.position = "right",
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
  
  output$plot2 <-renderPlot({
    data %>%
      filter(team == input$countryInput) %>%
      group_by(year, phase) %>%
      summarize(phase_runs = mean(phase_runs)) %>%
      ungroup() %>%
      filter(year == input$yearInput) %>%
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
    
  })
  
  output$plot3 <-
    renderPlot({
      data %>%
        filter(team == input$countryInput2) %>%
        group_by(year, bat_pos) %>%
        summarize(pos_runs = mean(pos_runs)) %>%
        ungroup() %>%
        filter(year == input$yearInput2) %>%
        ggplot(aes(bat_pos, pos_runs)) +
        geom_col() +
        labs(title =
               "Average Runs Scored by Batsmen",
             subtitle = "Period: 2010-2017",
             x = "Batting Position",
             y = "Average Runs in the Year") +
        labs(caption = "Source: cricsheet.org") +
        theme_classic()+
        theme(legend.position = "right")
    })
  
  output$sumstats <- 
    renderTable(
      data %>% skim(pos_runs, inning_runs, inning_wickets, phase_runs)
    )
  
  output$picture <-
    renderText({
      c(
        '<img src="',
        "https://images.news18.com/ibnlive/uploads/1200x800/jpg/2018/01/DUvqSCdUMAA7Va7.jpg?impolicy=website&width=1200&height=800",
        '">'
      )})
  
}

shinyApp(ui = ui, server = server)
