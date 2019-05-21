library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)




first_year <- read_excel("2018_First_Year_Class.xlsx") %>% 
  clean_names() 

grants <- read_excel("2018_Grants_and_Scholarships.xlsx") %>% 
  clean_names()

grants %>% 
  arrange(desc(full_tuition_total_percent)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = school_name, y = full_tuition_total_percent)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

