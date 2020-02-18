#####################
### Load Packages ###
#####################
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(reshape2)
library(choroplethr)
library(choroplethrZip)


#################
### Load Data ###
#################

df = read.csv('pete.csv') %>% mutate(month_name = as.character(month)) %>% select(-X) %>% filter(amount>0)

p_total_amount = read.csv('p_total_amount.csv') %>% select(-X) %>% mutate(region = as.character(region))
p_total_donors = read.csv('p_total_donors.csv') %>% select(-X) %>% mutate(region = as.character(region))
p_avg_donor = read.csv('p_avg_donor.csv') %>% select(-X) %>% mutate(region = as.character(region))


####################
### Load Choices ###
####################

choice_state = unique(df$state)


















