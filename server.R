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

# get the edge list
edge_list <- infer_edgelist(traits_data, feeding_rules)

## Make a graph object from inferred web edgelist ----
web <- igraph::graph_from_edgelist(edge_list, directed = TRUE)

make_3dfw(web) %>%
    plot_3dfw()

# Define server logic required to draw a histogram
function(input, output, session) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })



}
