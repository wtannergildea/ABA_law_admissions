# Load necessary packages

library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)
library(ggthemes)
library(shinythemes)
library(scales)

# Read in the two separate and one combined rds files.

all <- read_rds("all")

first_year <- read_rds("first_year")
  
grants <- read_rds("grants")


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
   
   p(paste("BLAH BLAH BLAH TEST"))
   
   
      )
   ),

####################################
# ADMISSIONS DATA
####################################

tabPanel("Admissions Data",
         
         fluidPage(
           
           # Application title
           
          titlePanel("Admissions Data"),
           
          plotOutput("acceptance_rate"),
          
          br(),
          br(),
          br(),
          
          plotOutput("GPAs"),
          
          br(),
          br(),
          br(),
          
          plotOutput("LSAT")
               
               )),

####################################
# FINANCIAL DATA 
####################################

tabPanel("Financial Data",
         
         fluidPage(
           
           # Application title
           
           titlePanel("Financial Data"),
               
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
               
)),

####################################
# DIVERSITY STATS
####################################

tabPanel("Diversity",
         
         fluidPage(
           
           # Application title
           
           titlePanel("Diversity")
           
        
           
         )),

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
           
           titlePanel("Sources:"),
        
           
           p(paste("PLACEHOLDER TEXT"))
           
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
   

}

# Run the application 
shinyApp(ui = ui, server = server)

