library(bslib)
library(GGally)
library(intergraph)
library(network)
library(sna)
library(igraph)
library(tidyverse)
library(shiny)

# import pfim functions
source("lib/pfim.R")

feeding_rules <- read.csv("data/FoodWebRules.csv")
traits_data <- read.csv("data/TaxaShiny.csv") %>% 
  as_tibble()

size_classes <- c("mega", "giant", "large", "medium", "small", "tiny", "macro", "micro", "primary")

ui <- page_fill(
  
  layout_columns( 

    card(
      h4("Consumer–Resource Rules:"),
      uiOutput("consumer_resource_inputs")
         ), 
    card( 
      h4("Current Rules:"),
      tableOutput("mappingTable"),
      
      h4("Static size rules"),
      plotOutput("webPlot_static")),
    card(
      h4("Dynamic size rules"),
      plotOutput("webPlot_dynamic")
    ),
    col_widths = c(2, 4, 6) 
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
  
  # Dynamically generate selectizeInputs for each consumer trait
  output$consumer_resource_inputs <- renderUI({
    lapply(size_classes, function(ct) {
      selectizeInput(
        inputId = ct,
        label = paste(ct, "feeding classes:"),
        choices = size_classes,
        selected = mapping[[ct]],
        multiple = TRUE
      )
    })
  })
  
  # When any of the dynamic inputs change, save the mapping
  observe({
    for (ct in size_classes) {
      input_val <- input[[ct]]
      if (!is.null(input_val)) {
        mapping[[ct]] <- input_val
      }
    }
  })
  
  # Display mapping table
  output$mappingTable <- renderTable({
    data.frame(
      Consumer = size_classes,
      Resources = sapply(size_classes, function(ct) {
        paste(mapping[[ct]], collapse = ", ")
      }),
      stringsAsFactors = FALSE
    )
  })
  
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
  
  # create new feeding rules
  feeding_rules_updated <- 
    reactive({
      if (nrow(mapping_df()) == 0) {
        feeding_rules %>%
          na.omit()
      } else {
        feeding_rules %>%
          filter(trait_type_resource != "size_category") %>%
          rbind(mapping_df() %>%
                  mutate(trait_type_resource = "size_category",
                         trait_type_consumer = "size_category"))%>%
          na.omit()
      }}
    )
  
  web <- reactive({
    if (nrow(mapping_df()) == 0) {
      
      igraph::graph_from_edgelist(infer_edgelist(traits_data, feeding_rules,
                                                 col_taxon = "genus"), 
                                  directed = TRUE)
      
    } else {
      # use updated feeding rules
      igraph::graph_from_edgelist(infer_edgelist(traits_data, feeding_rules_updated(),
                                                 col_taxon = "genus"), 
                                  directed = TRUE)
    }}
  )  
  
  output$webPlot_static <- renderPlot(ggnet2(igraph::graph_from_edgelist(infer_edgelist(traits_data, feeding_rules,
                                                                                        col_taxon = "genus"), 
                                                                         directed = TRUE),
                                             size = 3))
  
  output$webPlot_dynamic <- renderPlot({
    ggnet2(web())
  })
  
  
  
}


shinyApp(ui, server)