#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

# import toy dataset
feeding_rules <- read.csv("data/feeding_rules.csv")

# create size classes list
# this is for the input lists
size_classes <- 
  feeding_rules %>%
  filter(trait_type_resource == "size") %>%
  distinct(trait_resource)

fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      
      # specify the trait of the consumer
      selectizeInput("consumer_trait", "Trait of consumer", choices = NULL),
      
      # use all traits to specify what consumer eats
      selectizeInput("resource_trait", "What consumer eats", choices = size_classes, multiple = TRUE),
      
    ),
    mainPanel(
      verbatimTextOutput("values")
    )
  ), 
  title = "Options groups for select(ize) input")
