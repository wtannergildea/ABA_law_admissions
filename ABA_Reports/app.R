# Load necessary packages

library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)
library(ggthemes)
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
      )
   )
),

####################################
# ADMISSIONS DATA
####################################

tabPanel("Admissions Data",
         
         fluidPage(
           
           # Application title
           
           titlePanel("Admissions Data"),
           
           # This sidebar allows for the user to control the bin width of the visualization.
           
           sidebarLayout(
             sidebarPanel(
               sliderInput("bins",
                           "# of Bins:",
                           min = 10,
                           max = 50,
                           value = 30), 
               width = 2),
             
             
             # The main panel will feature text and the two visualizations for this tab.
             
             mainPanel(
               
               )))),

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
             
             
             # The main panel will feature text and the two visualizations for this tab.
             
             mainPanel(
               
             ))))


###################################
# SERVER
###################################

server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

