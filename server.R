#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(igraph)
library(tidyverse)
library(shiny)

# import pfim functions
source("lib/pfim.R")
# import graph plotting functions
source("lib/plot_graph.R")

# import toy dataset
feeding_rules <- read.csv("data/feeding_rules.csv")
traits_data <- read.csv("data/traits.csv") %>% as_tibble()

# create size classes list
# this is for the input lists
size_classes <- 
  feeding_rules %>%
  filter(trait_type_resource == "size") %>%
  distinct(trait_resource)


# Define server logic required to draw a histogram
function(input, output, session) {
  
  updateSelectizeInput(session, 'consumer_trait', choices = size_classes, selected = 'tiny')
  
  output$values <- renderPrint({
    tibble(trait_resource = input$resource_trait, trait_consumer = rep(input$consumer_trait, length(input$resource_trait)))
  })
}
