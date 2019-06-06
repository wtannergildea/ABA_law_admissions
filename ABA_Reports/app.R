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

ui <- navbarPage("2018 ABA Report Analysis", theme = shinytheme("flatly"),
  
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
           
          plotOutput("acceptance_rate")
               
               )),

####################################
# FINANCIAL DATA 
####################################

tabPanel("Financial Data",
         
         fluidPage(
           
           # Application title
           
           titlePanel("Financial Data"),
           
           # This sidebar allows for the user to control the bin width of the visualization.
           
           sidebarLayout(
             sidebarPanel(
               sliderInput("bins",
                           "# of Bins:",
                           min = 10,
                           max = 50,
                           value = 30), 
               width = 2),
             
             
             # MAIN PANEL
             
             mainPanel(
               
             )

))))


###################################
# SERVER
###################################

server <- function(input, output) {
   
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
}

# Run the application 
shinyApp(ui = ui, server = server)

