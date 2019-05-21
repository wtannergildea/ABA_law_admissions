library(tidyverse)
library(ggplot2)
library(readxl)
library(janitor)
library(ggthemes)


# Load in the two excel spreadsheets from the ABA.

first_year <- read_excel("2018_First_Year_Class.xlsx") %>% 
  clean_names() %>% 
  mutate(school_name = str_to_title(school_name)) 

grants <- read_excel("2018_Grants_and_Scholarships.xlsx") %>% 
  clean_names() %>% 
  mutate(school_name = str_to_title(school_name))


# Let's make some nice visualizations.

###############
# ACCEPTANCE RATE 
###############

first_year %>% 
  arrange(acceptance_rate) %>% 
  slice(1:20) %>% 
  
  # start visualization
  
  ggplot(aes(x= reorder(school_name, acceptance_rate), y = acceptance_rate)) +
  
  geom_col() + 
  
  # theme changes
  
  theme_economist() +
  scale_fill_economist() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###############
# GPA's
###############

first_year %>% 
  arrange(desc(x50th_percentile_ugpa_all)) %>% 
  slice(1:20) %>% 
  
  # start visualization
  
  ggplot(aes(x= reorder(school_name, x50th_percentile_ugpa_all), y = x50th_percentile_ugpa_all)) +
  
  geom_col() + 
  
  # theme changes
  
  theme_economist() +
  scale_fill_economist() +
  coord_cartesian(ylim = c(3.5,4)) +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

###############
# LSAT scores
###############

first_year %>% 
  arrange(desc(x50th_percentile_lsat_all)) %>% 
  slice(1:20) %>% 
  
  # start visualization
  
  ggplot(aes(x= reorder(school_name, x50th_percentile_lsat_all), y = x50th_percentile_lsat_all)) +
  
  geom_col() + 
  
  # theme changes
  
  theme_economist() +
  scale_fill_economist() +
  coord_cartesian(ylim = c(160,175)) +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


###############

# Looking at grants.

# grants %>% 
#   arrange(desc(full_tuition_total_percent)) %>% 
#   slice(1:10) %>% 
#   ggplot(aes(x = school_name, y = full_tuition_total_percent)) + 
#   geom_point() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

