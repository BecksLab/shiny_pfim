library(GGally)
library(intergraph)
library(network)
library(sna)
library(igraph)
library(tidyverse)
library(shiny)

# import pfim functions
source("lib/pfim.R")

feeding_rules <- read.csv("data/feeding_rules.csv")
traits_data <- read.csv("data/traits.csv") %>% as_tibble()

size_classes <- 
  feeding_rules %>%
  filter(trait_type_resource == "size") %>%
  distinct(trait_resource) %>%
  pull()

ui <- fluidPage(
  titlePanel("Consumer–Resource Trait Mapping"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("consumer", "Select Consumer Trait:",
                  choices = size_classes, selected = size_classes[1]),
      
      selectizeInput("resources", "Compatible Resource Traits:",
                     choices = size_classes,
                     multiple = TRUE),
      
      h4("Current Mapping:"),
      tableOutput("mappingTable")
    ),
    
    mainPanel(
      h4("Blah blah blah"),
      tableOutput("webPlot")
    )
  )
)

server <- function(input, output, session) {
  
  # Store mapping from consumer -> resource list
  mapping <- reactiveValues()
  
  # Initialize with empty selections
  observe({
    for (ct in size_classes) {
      if (is.null(mapping[[ct]])) {
        mapping[[ct]] <- character(0)
      }
    }
  })
  
  # When consumer changes, update the resources input
  observeEvent(input$consumer, {
    updateSelectizeInput(session, "resources",
                         selected = mapping[[input$consumer]])
  })
  
  # When resources change, save mapping
  observeEvent(input$resources, {
    mapping[[input$consumer]] <- input$resources
  }, ignoreNULL = FALSE)
  
  # Create long-format mapping dataframe as a reactive object
  mapping_df <- reactive({
    do.call(rbind, lapply(size_classes, function(ct) {
      if (length(mapping[[ct]]) == 0) {
        data.frame(trait_consumer = ct, trait_resource = NA_character_, stringsAsFactors = FALSE)
      } else {
        data.frame(trait_consumer = ct,
                   trait_resource = mapping[[ct]],
                   stringsAsFactors = FALSE)
      }
    }))
  })
  
  # Show the mapping in the table
  output$mappingTable <- renderTable({
    mapping_df()
  })
  
  # create new feeding rules
  feeding_rules_updated <- 
    reactive({
     if (nrow(mapping_df()) == 0) {
          feeding_rules %>%
         na.omit()
        } else {
          feeding_rules %>%
            filter(trait_type_resource != "size") %>%
            rbind(mapping_df() %>%
                    mutate(trait_type_resource = "size",
                           trait_type_consumer = "size"))%>%
            na.omit()
        }}
      )

  
  
  
}


shinyApp(ui, server)