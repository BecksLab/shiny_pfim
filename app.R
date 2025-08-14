library(bslib)
library(GGally)
library(igraph)
library(intergraph)
library(network)
library(shiny)
library(sna)
library(tidyverse)

# import pfim functions
source("lib/pfim.R")
source("lib/internals.R")

feeding_rules <- read.csv("data/FoodWebRules.csv")
traits_data <- read.csv("data/full_dataset.csv") %>% 
  as_tibble()

size_classes <- c("mega", "giant", "large", "medium", "small", "tiny", "macro", "micro", "primary")

feeding_rules_og <- list(mega = c("mega", "giant", "large", "medium", "macro", "micro", "primary"),
                         giant = c("giant", "large", "medium", "small", "macro", "micro", "primary"),
                         large = c("large", "medium", "small", "tiny", "macro", "micro", "primary"),
                         medium = c("medium", "small", "tiny", "macro", "micro", "primary"),
                         small = c("small", "tiny", "macro", "micro", "primary"),
                         tiny = c("tiny", "macro", "micro", "primary"),
                         macro = c("macro", "micro", "primary"),
                         micro = c("micro", "primary"),
                         primary = c("primary"))



# for trophic level colours
cols <- c(0.99, 1.99, 2.99, 3.99, 4.99, 5.99, 6.95, 1000)
tl_cols <- c("#DDDDDD", "#26b170", "#7ed348", "#97e7f5", "#009dd1", "#a663cc", "#520380", "#a1045a")

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
        mapping[[ct]] <- feeding_rules_og[[ct]]
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
  
  igraph_web <- igraph::graph_from_edgelist(infer_edgelist(traits_data, feeding_rules,
                                                           col_taxon = "genus"), 
                                            directed = TRUE)
  
  tls <- calc_node_tl_std(as_adjacency_matrix(igraph_web))
  # Fixed colors by trophic level
  values <- as.numeric(tls)
  ii <- cut(values, breaks = cols, include.lowest = TRUE)
  # Green, yellow, dark blue, dark red,light blue, orange, brown, pink
  nodecols <- tl_cols[ii]
  
  shps <- names(tls) %>%
    as_tibble() %>%
    mutate(shape = case_when(value  == "Otodus megalodon" ~ 17,
                             .default = as.numeric(16)),
           size = case_when(value  == "Otodus megalodon" ~ "shark",
                             .default = "not_shark"))
  
  output$webPlot_static <- renderPlot(ggnet2(igraph_web,
                                             size = shps$size,
                                             size.palette = c("shark" = 5, "not_shark" = 1),
                                             node.color = nodecols,
                                             shape = shps$shape,
                                             legend.position = 'none',
                                             edge.alpha = 0.5))
  
  
  output$webPlot_dynamic <- renderPlot({
    igraph_web <- web()
    
    tls <- calc_node_tl_std(as_adjacency_matrix(igraph_web))
    # Fixed colors by trophic level
    values <- as.numeric(tls)
    ii <- cut(values, breaks = cols, include.lowest = TRUE)
    # Green, yellow, dark blue, dark red,light blue, orange, brown, pink
    nodecols <- tl_cols[ii]
    
    shps <- names(tls) %>%
      as_tibble() %>%
      mutate(shape = case_when(value  == "Otodus megalodon" ~ 17,
                               .default = as.numeric(16)),
             size = case_when(value  == "Otodus megalodon" ~ "shark",
                              .default = "not_shark"))
    
    ggnet2(igraph_web,
           node.color = nodecols,
           shape = shps$shape,
           legend.position = 'none',
           edge.alpha = 0.5)
  })
  
  
  
}


shinyApp(ui, server)