library(tidyverse)
library(ggplot2)
library(janitor)
library(stringr)
library(lubridate)
library(stringi)
library(patchwork)
library(broom)
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

dependent <- c("Inning Runs" = "inning_runs", 
               "Inning Wickets" = "inning_wickets")

rule <- c("Powerplay Rule" = "pp_rule", "Newball Rule" = "nb_rule",
          "Bouncer Rule" = "bo_rule", "Bat Thickness Rule" = "bt_rule")


ui <- navbarPage(theme = shinytheme("simplex"), "Cricket Analytics", 
                 
                 tabPanel("About the Project",
                          tabsetPanel(
                            tabPanel("What is Cricket?",
                                     br(),
                                     p("Cricket is a bat and ball game played most commonly played in the 
                          commonwealth countries. It is played between two teams, each consisting of eleven players.
                            First played in the early 16th century, the game is the second most popular sport in the world in terms of viewership. The bulk
                            of the sport's followers are in the commonwealth countries."),
                                     p("The video below gives a brief overview of cricket:"),
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
                                     br(),
                                     HTML(paste("The following explanation is taken from",
                                                                a(href="http://www.abcofcricket.com", "abcofcricket.com:"))),
                                     br(),
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
                                sunlight.")),
                            
                            tabPanel("Project Overview",
                                     br(),
                                      h4("Rule Changes"),    
                                     p("Since 2006, ODI Cricket has changed drastically and
                                        the rate of run-scoring has increased substantially.
                                        For instance, the records for the highest ODI total, 
                                        the fastest century and half-centuy scored by Batsmen, 
                                        and numerous other batting records have been broke. 
                                        Experts have suggested that these changes are 
                                         the result of a number of a number of changes that were
                                         instituted following 2011."),
                                     p("While there have been numerous changes to ODI cricket in the past decade, 
                                       the following are some of the most important:"),
                                     HTML("<ul><li><b>Two new-ball rule:</b> on 09/29/2011, there was a 
                                     rule that made it mandatory to use two separate new balls by 
                                     each of the starting bowlers at the start of each ODI innings.
                                     Previously, there was only one ball and this rule was thought to
                                     favor the batsmen as it made reverse-swing, a useful asset
                                     for fast-bowlers made possible by an older cricket ball, 
                                     an unlikely proposition.</li>
                                          <li><b>Bouncer rule:</b> on 10/30/2012, there was a 
                                     rule that increased the limit of bouncers allowed in one 
                                     over from 1 to 2. A two-run penalty, imposed if the bowler 
                                     exceeded this limit was also removed and the rule was thought
                                     to have benefitted the bowlers by giving them more freedom 
                                     in their bowling choices.</li>
                                          <li><b>Powerplay rule:</b> on 06/26/2015, there was a 
                                     change to the rules pertaining to the fieling restrictions in 
                                     ODI cricket, known as powerplays. In a powerplay, there are only
                                     a certain number of fielders allowed within the inner-portion
                                     of a cricket field. Before this rule, there were only 5 fielders
                                     allowed outside the inner-ring in the last ten overs of the game
                                     which made it easier for batsmen to hit boundaries. However, this
                                     batting powerplay was removed in 2015, in an effort to help the 
                                     bowlers.</li>
                                          <li>Bat-thickness rule:</b> on 09/28/2017, there was a 
                                     limiitation imposed on the maximum thickness of a cricket bat. 
                                     The rule change was instituted in the face of mounting criticism
                                     with regards to the increasing thickness of the size of batsmen's
                                     bats. Thicker bats made it easier to put more power on the ball and
                                     facilitated the scoring of boundaries. This rule change was meant 
                                     to limit the impacts of bat thickness and, on the whole, favored
                                     the bowlers.</li></ul>"),
                                     h4("Purpose"),                          
                                        p("The purpose of this project, therefore, is to
                                        examine whether these rules changes have had 
                                        a substantial impact on outcomes in cricket.
                                        To do so, it looks at both visual and quantitative evidence
                                        to see whether any drastic changes in run-scoring or 
                                        wicket taking have occured, in line with what was 
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
                          h4("The Evoloution of ODI Cricket"),
                          p("These panels will show how ODI cricket has evolved
                            over the past few years. The two main variables that
                            determine the outcomes of a cricket match are the runs
                            that are scored by the batsmen and the wickets taken
                            by the bowlers. Therefore, the following few plots show
                            how these two key variables have changed for several
                            cricket teams over time."),
                          tabsetPanel(
                            tabPanel("Overall Trends",
                                     h4("Evoloution of Run Scoring over Time"),
                                     plotOutput("plot"),
                                     p("The graph above shows the trends in run-scoring between 2006-2017."),
                                     plotOutput("plot2"),
                                     p("The graph above shows the trends in wicket-taking between 2006-2017.")
                            ),
                            tabPanel("Runs Scored in Phase",
                                     selectInput("countryInput3", "Country",
                                                 choices=countries),
                                     plotOutput("plot3"),
                                     sidebarPanel(width=12, 
                                       sliderInput("yearInput3",
                                                   label = "Year:",
                                                   min = 2006, max = 2017,value=2006, sep="",ticks=FALSE)),
                                     p("This plot shows how the number of runs scored by
                                      a team in each phase (10-over chunk) has
                                       changed over time. The data can be viewed at the
                                       country level.")),
                            
                            tabPanel("Runs Scored by Position",
                                     selectInput("countryInput4", "Country",
                                                 choices=countries),
                                     plotOutput("plot4"),
                                     sidebarPanel(width=12,
                                       sliderInput("yearInput4", 
                                                   label = "Year:",
                                                   min = 2006, max = 2017,value=2006,sep="", ticks=FALSE)),
                                     p("This plot shows how the runs scored by
                                       batsmen batting at a particular position have
                                       changed over time. The data can be viewed at the
                                       country level.")),
                            
                            tabPanel("Wickets Lost in Phase",
                                     selectInput("countryInput5", "Country",
                                                 choices=countries),
                                     plotOutput("plot5"),
                                     sidebarPanel(width=12, 
                                                  sliderInput("yearInput5",
                                                              label = "Year:",
                                                              min = 2006, max = 2017,value=2006, sep="",ticks=FALSE)),
                                     p("This plot shows how the number of wickets lost by
                                      a team in each phase (10-over chunk) has
                                       changed over time. The data can be viewed at the
                                       country level.")),
                            
                            tabPanel("Wickets Taken by Position",
                                     selectInput("countryInput6", "Country",
                                                 choices=countries),
                                     plotOutput("plot6"),
                                     sidebarPanel(width=12,
                                                  sliderInput("yearInput6", 
                                                              label = "Year:",
                                                              min = 2006, max = 2017,value=2006,sep="", ticks=FALSE)),
                                     p("This plot shows how the number of wickets taken
                                       by a bowler bowling at a particular position has
                                       changed over time. The data can be viewed at the
                                       country level."))
                            
                            
                          )),
                 
                 tabPanel("Regression Results",
                            h3("Have Rule Changes Transformed ODI Cricket?"),
                          h5("Simple Correlational Model"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("depInput", "Dependent Variable",
                                        choices=dependent),
                              selectInput("ruleInput", "Rule",
                                          choices=rule)),
                            mainPanel(
                              tableOutput("regression")
                              )),
                          p("The dependent variable in the regression is
                            an indicator variable for whether the rule was 
                            in play or not. Therefore, the variable takes 
                            a value of 0 if the rule was not institued at the
                            time of the game and 1 if the rule was in place. The
                            independent variable, thereofore, measures the association
                            between the rule and the dependent variable.")
                          
                          ),
                 
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
                          HTML(paste("My name is Hamid Khan and I am currently a Junior at Harvard College studying Economics with a minor in Statistics. 
                            You can reach me at hamidkhan@college.harvard.edu for any questions about this project. The github
                            repository for this project can be found",
                               tags$a(href="https://github.com/hak7980/Cricket-Analytics", "here.")))
                          ))
 
# Detailed comments on the code for the plots can
# be found in the plot_code Rscript file in the repo

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
  
  output$plot2 <-
    renderPlot({
      data %>%
        group_by(year) %>%
        summarize(inning_wickets = mean(inning_wickets)) %>%
        ungroup() %>%
        ggplot(aes(x = factor(year), 
                   y = inning_wickets)) +
        geom_point() +
        labs(title = 
               "Average Wickets Taken in Each Phase", 
             subtitle = "Period: 2010-2017",
             x = "Year", 
             y = "Average Wickets",
             col = "Innings Phase") +
        labs(caption = "Source: cricsheet.org") +
        theme_classic()+ 
        theme(legend.position = "right",
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
  
  output$plot3 <-renderPlot({
    data %>%
      filter(team == input$countryInput3) %>%
      group_by(year, phase) %>%
      summarize(phase_runs = mean(phase_runs)) %>%
      ungroup() %>%
      filter(year == input$yearInput3) %>%
      ggplot(aes(phase, phase_runs)) +
      geom_col() +
      labs(title = 
             "Average Runs Scored in a Phase", 
           subtitle = (paste("Year", input$yearInput3, sep = ": ")),
           x = "Phase", 
           y = "Average Runs Scored") +
      labs(caption = "Source: cricsheet.org") +
      theme_classic()+ 
      theme(legend.position = "right",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$plot4 <-
    renderPlot({
      data %>%
        filter(team == input$countryInput4) %>%
        group_by(year, bat_pos) %>%
        summarize(pos_runs = mean(pos_runs)) %>%
        ungroup() %>%
        filter(year == input$yearInput4) %>%
        ggplot(aes(bat_pos, pos_runs)) +
        geom_col() +
        labs(title =
               "Average Runs Scored by Batsmen",
             subtitle = (paste("Year", input$yearInput4, sep = ": ")),
             x = "Batting Position",
             y = "Average Runs in the Year") +
        labs(caption = "Source: cricsheet.org") +
        theme_classic()+
        theme(legend.position = "right")
    })
  
  output$plot5 <-renderPlot({
    data %>%
      filter(team == input$countryInput5) %>%
      group_by(year, phase) %>%
      summarize(phase_wickets = mean(phase_wickets)) %>%
      ungroup() %>%
      filter(year == input$yearInput5) %>%
      ggplot(aes(phase, phase_wickets)) +
      geom_col() +
      labs(title = 
             "Average Wickets Lost in a Phase", 
           subtitle = (paste("Year", input$yearInput5, sep = ": ")),
           x = "Phase", 
           y = "Average Wickets Lost") +
      labs(caption = "Source: cricsheet.org") +
      theme_classic()+ 
      theme(legend.position = "right",
            axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  output$plot6 <-
    renderPlot({
      data %>%
        filter(team == input$countryInput6) %>%
        group_by(year, bow_pos) %>%
        summarize(pos_wickets = mean(pos_wickets)) %>%
        ungroup() %>%
        filter(year == input$yearInput6) %>%
        ggplot(aes(bow_pos, pos_wickets)) +
        geom_col() +
        labs(title =
               "Average Wickets Taken by Bowler",
             subtitle = (paste("Year", input$yearInput6, sep = ": ")),
             x = "Bowling Position",
             y = "Average Wickets in the Year") +
        labs(caption = "Source: cricsheet.org") +
        theme_classic()+
        theme(legend.position = "right")
    })
  
  # Summary stats of all numeric variables in the dataset
  output$sumstats <- 
    renderTable(
      data %>%
        select_if(.,is.numeric) %>%
        skim() 
    )
  
 # Function that saves regression variable inputs 
  myformula <- reactive({
    expln <- paste(c(input$ruleInput))
    as.formula(paste(input$depInput, " ~ ", expln))
  })
  
  # Making the output plot
  output$regression <-
    renderTable(
      lm(myformula(), data = data) %>%
        tidy(conf.int=TRUE) %>% 
        select(Variable = term,
               Estimate = estimate,
               `Lower Bound` = conf.low,
               `Upper Bound` = conf.high))

}

shinyApp(ui = ui, server = server)
