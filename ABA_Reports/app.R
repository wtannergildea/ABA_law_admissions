# Load necessary packages

library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)
library(ggthemes)
library(shinythemes)
library(scales)
library(plotly)

# Read in the two separate and one combined rds files.

all <- read_rds("all")

first_year <- read_rds("first_year")
  
grants <- read_rds("grants")

ethn_vis <- read_rds("ethn_vis")


####################################                 
# USER INTERFACE
####################################

ui <- navbarPage("2018 ABA Report Analysis", theme = shinytheme("yeti"),
  
####################################                 
# HOME PAGE 
####################################                
  
  tabPanel("Intro",
           
  fluidPage(
   
   # Application title
    
   titlePanel("What can we learn from law school ABA reports?"),
   
   # INSERT INTRODUCTORY TEXT HERE
   
   br(),
   
   p(paste("Each year, thousands of aspiring lawyers apply to law schools across the U.S. Although the total number of enrolled 1L's across the U.S. hit their lowest level in 41 years in 2015 [1], the competition for admission to the 'top' law schools remains steep. Near-perfect grade point averages, LSAT scores, and a small fortune are only part of the package today's applicants must trade for a coveted acceptance letter.")),
   
   br(),
   
   p(paste("Even for the well-credentialed, however, all applicants to law schools are faced with the same challenge: misinformation and disparate statistics. This dashboard aims to rectify this problem.")),
   
   br(),
   
   p(paste("Annually, law schools are mandated by the American Bar Association to compile a wide variety of statistics. These 'ABA reports' are then aggregated and released to the public. In them, you'll find the median accepted GPA's and LSAT scores at the top law schools. Trying to maximize your chances at grant aid? You'll find which schools are the most generous - and most stingey. You'll also find other statistics on measures of diversity and post-grad employment outcomes, instead of digging for this information yourself.")),
   
   br(),
   
   p(paste("I'd wager that most applicants are unaware these reports exist. As you consider applying to law school, I hope this ABA report dashboard clears up some of your questions. Good luck!")),
   
   br(),
   
   br(),
   
   br(),
   
   p(paste("[1] According to Law School Transparency, 37,056 first-year law students enrolled in the United States in 2015."))
   
   
   
      )
   ),

####################################
# ADMISSIONS DATA
####################################

tabPanel("Admissions Data",
         
         fluidPage(
           
           # Application title
           
          titlePanel("Admissions Data"),
          
          sidebarLayout(
            sidebarPanel(
              
              h3("Can I get in?"),
              
              h4("This is the burning question prospective applicants want answered the most."),
              
              h5("Although admissions committees will consider your personal statement, letters of recommendation, your resume, and other factors in their decision,
                 the two most important numbers are your undergraduate grade point average (GPA) and your LSAT score.")
            ),
          
          mainPanel(
        
          
          br(),
        
          plotOutput("acceptance_rate"),
          
          br(),
          br(),
          br(),
          
          plotOutput("GPAs"),
          
          br(),
          br(),
          br(),
          
          plotOutput("LSAT")
               
               )))),

####################################
# FINANCIAL DATA 
####################################

tabPanel("Financial Data",
         
         fluidPage(
           
           # Application title
           
           titlePanel("Financial Data"),
           
           sidebarLayout(
             sidebarPanel(
               
               h3("Can I pay?"),
               
               h4("It's not often the first question applicants ask, but it should be the most important."),
               
               br(),
               
               h5("Law school is expensive, and many graduates carry tens of thousands of dollars in debt. But that doesn't have to be the case."),
               
               br(),
               
               h5("While the tippy-top schools such as Yale and Harvard offer no merit-based aid,
                  you can win signficant cash to go to some of their top-ranked peers. If you're an above-average applicant and willing to swallow some of your pride,
                  attending a relatively less-selective school could save you some serious cash."),
               
               br(),
               
               h5("Below these statistics, I've listed a number of the most prestigious scholarships available at top law schools.")
               
               ),
             
             mainPanel(
               
               plotOutput("median_award"),
               
               br(),
               br(),
               br(),
               
               plotOutput("half_plus"),
           
               br(),
               br(),
               br(),
           
               plotOutput("half_to_full"),
               
               br(),
               br(),
               br(),
               
               plotOutput("full"),
               
               br(),
               br(),
               br(),
               
               plotOutput("more_than_full"),
               
               br(),
               br(),
               br(),
               
               plotOutput("any_aid")
               
)))),

####################################
# DIVERSITY STATS
####################################

tabPanel("Diversity",
         
         fluidPage(
           
           # Application title
           
           titlePanel("Diversity"),
           
           sidebarLayout(
            sidebarPanel(
              
              checkboxGroupInput("school",
                                "Select at least two schools to compare.",
                                
                                choices = list("Harvard University" = "Harvard University",
                                          "Yale University" = "Yale University",
                                          "Stanford University" = "Stanford University",
                                          "Pennsylvania, University Of" = "Pennsylvania, University Of",
                                          "Virginia, University Of" = "Virginia, University Of",
                                          "Columbia University" = "Columbia University",
                                          "Chicago, University Of" = "Chicago, University Of",
                                          "Southern California, University Of" = "Southern California, University Of",
                                          "Northwestern University" = "Northwestern University",
                                          "Michigan, University Of" = "Michigan, University Of",
                                          "California-Berkeley, University Of" = "California-Berkeley, University Of",
                                          "Duke University" = "Duke University",
                                          "Texas At Austin, University Of" = "Texas At Austin, University Of",
                                          "Cornell University" = "Cornell University",
                                          "Georgetown University" = "Georgetown University",
                                          "California-Los Angeles, University Of" = "California-Los Angeles, University Of",
                                          "New York University" = "New York University",
                                          "Vanderbilt University" = "Vanderbilt University",
                                          "Washington University" = "Washington University",
                                          "Minnesota, University Of" = "Minnesota, University Of"),
                                
                                
                                selected = c("Harvard University",
                                             "Yale University",
                                             "Stanford University"))),
                                             # "Pennsylvania, University Of",
                                             # "Virginia, University Of",
                                             # "Columbia University",
                                             # "Chicago, University Of",
                                             # "Southern California, University Of",
                                             # "Northwestern University",
                                             # "Michigan, University Of",
                                             # "California-Berkeley, University Of",
                                             # "Duke University",
                                             # "Texas At Austin, University Of",
                                             # "Cornell University",
                                             # "Georgetown University",
                                             # "California-Los Angeles, University Of",
                                             # "New York University",
                                             # "Vanderbilt University",
                                             # "Washington University",
                                             # "Minnesota, University Of"))),

           
          mainPanel(
             
           
           plotlyOutput("ethnicity")
        
           
         )))),

####################################
# POST-GRAD OUTCOMES
####################################

tabPanel("Post-Grad Outcomes",
         
         fluidPage(
           
           # Application title
           
           titlePanel("Post-Grad Outcomes")
           
           
           
         )),





############
# END CREDITS
#############

tabPanel("Footnotes",
         
         fluidPage(
           
           # I also want to add an acknowledgements page at the end.
           
           titlePanel("Footnotes"),
           
           br(),
        
           p(paste("My name is Tanner Gildea. I am a recent graduate of Harvard College.")),

           br(),
           
           p(paste("You may contact me at tannergildea@icloud.com.")),
           
           br(),

           p(paste("I may or may not apply to law school in the future.")),
           
           br(),
           
           p(paste("All data is publicly accessible here from the ABA website.")),
           
           br(),
           
           p(paste("This dashboard's code is available on my Github here."))
           
)))



###################################
# SERVER
###################################

server <- function(input, output) {

  ###############
  # ACCEPTANCE RATE 
  ###############
  
   output$acceptance_rate <- renderPlot({
  
     first_year %>% 
       arrange(acceptance_rate) %>% 
       slice(1:20) %>% 
       
       # start visualization
       
       ggplot(aes(x= reorder(school_name, acceptance_rate), 
                  y = acceptance_rate,
                  fill = "Blue")) +
       
       geom_col() + 
       
       geom_text(aes(label= u_s_news_and_world_ranking)) +
       
       # theme changes
       
       theme_economist() +
       scale_fill_economist() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       
       
       labs(x = NULL,
            y = "Acceptance Rate",
            title = "Most Competitive Law Schools") +
       
       guides(fill = FALSE)
     
     
   })
   
   
   ###############
   # GPA's
   ###############
   
   output$GPAs <- renderPlot({
  
   first_year %>% 
     arrange(desc(x50th_percentile_ugpa_all)) %>% 
     slice(1:20) %>% 
     
     # start visualization
     
     ggplot(aes(x= reorder(school_name, x50th_percentile_ugpa_all), 
                y = x50th_percentile_ugpa_all,
                fill = "yellow")) +
     
     geom_col() +
     
     geom_text(aes(label= u_s_news_and_world_ranking)) +
     
     # theme changes
     
     theme_economist() +
     scale_fill_economist() +
       
     coord_cartesian(ylim = c(3.7,4)) +
       
     scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
       
     
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     
     
     labs(x = NULL,
          y = "Median GPA",
          title = "Median Accepted Undergraduate GPA's") +
     
     guides(fill = FALSE) 
     
   })
   
   
   ###############
   # LSAT scores
   ###############
   
   output$LSAT <- renderPlot({
     
   first_year %>% 
     arrange(desc(x50th_percentile_lsat_all)) %>% 
     slice(1:20) %>% 
     
     # start visualization
     
     ggplot(aes(x= reorder(school_name, x50th_percentile_lsat_all), 
                y = x50th_percentile_lsat_all,
                fill = "Blue")) +
     
     geom_col() + 
     
     geom_text(aes(label= u_s_news_and_world_ranking)) +
     
     # theme changes
     
     theme_economist() +
     scale_fill_economist() +
       
     coord_cartesian(ylim = c(164,175)) +
       
     scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
       
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     
     labs(x = NULL,
          y = "Median LSAT Score",
          title = "Median Accepted LSAT Score") +
     
     guides(fill = FALSE)
   
  })
   
   ###############
   # Median Grant
   ###############
   
   output$median_award <- renderPlot({
     
   all %>% 
     arrange(u_s_news_and_world_ranking) %>% 
     slice(1:20) %>% 
     
     # start visualization
     
     ggplot(aes(x= reorder(school_name, ft_50th_percentile_grant_amount), 
                y = ft_50th_percentile_grant_amount,
                fill = "Blue")) +
     
     geom_col() + 
     
     geom_text(aes(label= u_s_news_and_world_ranking)) +
     
     # theme changes
     
     theme_economist() +
     scale_fill_economist() +
     
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     
     coord_cartesian(ylim = c(10000,45000)) +

     scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
       
     labs(x = NULL,
          y = "Dollars",
          title = "Median Grant Amount") +
     
     guides(fill = FALSE) 
     
   })
   
   ###############
   # Total percent receiving half or more aid
   ###############
   
   output$half_plus <- renderPlot({
     
   all %>% 
     arrange(u_s_news_and_world_ranking) %>% 
     slice(1:20) %>% 
     
     # start visualization
     
     ggplot(aes(x= reorder(school_name, half_plus_aid), 
                y = half_plus_aid,
                fill = "Blue")) +
     
     geom_col() + 
     
     geom_text(aes(label= u_s_news_and_world_ranking)) +
     
     # theme changes
     
     theme_economist() +
     scale_fill_economist() +
     
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     
     coord_cartesian(ylim = c(0,70)) +
     
     scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
     
     labs(x = NULL,
          y = "Percent",
          title = "Percent Receiving More than Half Tuition in Grant Aid") +
     
     guides(fill = FALSE) 
   
    })
   
   ###############
   # Half to Full
   ###############
   
   output$half_to_full <- renderPlot({
   
   all %>% 
     arrange(u_s_news_and_world_ranking) %>% 
     slice(1:20) %>% 
     
     # start visualization
     
     ggplot(aes(x= reorder(school_name, half_to_full_tuition_total_percent), 
                y = half_to_full_tuition_total_percent,
                fill = "Blue")) +
     
     geom_col() + 
     
     geom_text(aes(label= u_s_news_and_world_ranking)) +
     
     # theme changes
     
     theme_economist() +
     scale_fill_economist() +
     
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     
     coord_cartesian(ylim = c(0,50)) +
     
     labs(x = NULL,
          y = "Percent",
          title = "Percent Receiving Half to Full Tuition in Grant Aid") +
     
     guides(fill = FALSE) 
     
   })
   
   ###############
   # FULL Tuition Assistance by Selectivity
   ###############
   
   output$full <- renderPlot({
     
   all %>% 
     arrange(u_s_news_and_world_ranking) %>% 
     slice(1:20) %>% 
     
     # start visualization
     
     ggplot(aes(x= reorder(school_name, full_tuition_total_percent), 
                y = full_tuition_total_percent,
                fill = "Blue")) +
     
     geom_col() + 
     
     geom_text(aes(label= u_s_news_and_world_ranking)) +
     
     # theme changes
     
     theme_economist() +
     scale_fill_economist() +
     
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     
     coord_cartesian(ylim = c(0,20)) +
       
     scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
       
     labs(x = NULL,
          y = "Percent",
          title = "Percent Receiving Full Tuition in Grant Aid") +
     
     guides(fill = FALSE) 
   
    })
   
   ###############
   # More than Full Tuition
   ###############
   
   output$more_than_full <- renderPlot({
     
   all %>% 
     arrange(u_s_news_and_world_ranking) %>% 
     slice(1:20) %>% 
     
     # start visualization
     
     ggplot(aes(x= reorder(school_name, more_than_full_tuition_total_percent), 
                y = more_than_full_tuition_total_percent,
                fill = "Blue")) +
     
     geom_col() + 
     
     geom_text(aes(label= u_s_news_and_world_ranking)) +
     
     # theme changes
     
     theme_economist() +
     scale_fill_economist() +
     
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     
     coord_cartesian(ylim = c(0,10)) +
       
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
       
     
     labs(x = NULL,
          y = "Percent",
          title = "Percent Receiving More than Full Tuition in Grant Aid") +
     
     guides(fill = FALSE) 

    })
   
   ###############
   # Any Aid
   ###############
   
   output$any_aid <- renderPlot({
     
   
   all %>% 
     arrange(u_s_news_and_world_ranking) %>% 
     slice(1:20) %>% 
     
     # start visualization
     
     ggplot(aes(x= reorder(school_name, total_number_receiving_grants_total_percent), 
                y = total_number_receiving_grants_total_percent,
                fill = "Blue")) +
     
     geom_col() + 
     
     geom_text(aes(label= u_s_news_and_world_ranking)) +
     
     # theme changes
     
     theme_economist() +
     scale_fill_economist() +
     
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     
     coord_cartesian(ylim = c(30,100)) +
     
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
       
     labs(x = NULL,
          y = "Percent",
          title = "Percent of Students Receiving Any Grant Aid") +
     
     guides(fill = FALSE) 

    })
   
   
   ###############
   # Ethnicity Top 20 (percent white, minority, and international)
   ###############
   
   output$ethnicity <- renderPlotly({
     
   ethn_vis %>% 
     filter(school_name %in% input$school) %>%
     gather('Minority Race', White, 'Unknown Race', International, key = "label", value = "percent") %>% 
       
     ggplot(aes(x= school_name, 
                y = percent, 
                fill = label)) +
     
     geom_col(position = "dodge") + 
     
     facet_grid(~label) +
     
     # theme changes
     
     theme_economist() +
     scale_fill_economist() +
     
     theme(axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           axis.ticks.x = element_blank()) +
     
     scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
     
     labs(x = NULL,
          y = "Percent",
          title = "Diversity of Most Recent Graduating Class") +
     
     guides(fill = FALSE)
   
   })
   

}

# Run the application 
shinyApp(ui = ui, server = server)

