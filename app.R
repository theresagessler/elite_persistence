library(shiny)
library(shinydashboard)
library(tidyverse)
library(igraph)
library(visNetwork)

# Load data
load("network.RData")

# Keep only MPs
mp_ids <- V(g)$name[V(g)$type == "MP"]

labels_mp <- labels %>%
  filter(id %in% mp_ids)

# Create named choices: "Name (ID)" -> label, but keep ID accessible
label_choices <- setNames(
  labels_mp$label,
  paste0(labels_mp$label, " (", labels_mp$id, ")")
)

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Network Explorer"),
  
  dashboardSidebar(
    selectizeInput(
      "target",
      "Person:",
      choices = label_choices,
      options = list(
        placeholder = 'Type a name...',
        maxOptions = 1000
      )
    ),
    
    sliderInput("degree", "Degrees of separation",
                min = 1, max = 6, value = 6),
    
    actionButton("run", "Run Analysis"),
    
    br(), br(),
    
    uiOutput("disambiguation_ui"),
    
    textOutput("error_message"),
    
    hr(),
    
    helpText("Search for a person and explore their connections.")
  ),
  
  dashboardBody(
    fluidRow(
      box(
        title = "Network Graph",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        visNetworkOutput("network_plot", height = "600px"),
        br(),
        h4(textOutput("connections_text"))
      )
    ),
    
    fluidRow(
      box(
        title = "Legend",
        width = 12,
        status = "info",
        solidHeader = TRUE,
HTML("
  <b>Colors:</b><br>
  <span style='color:#2C3E50;'>●</span> Target node<br>
  <span style='color:#E69F00;'>●</span> Slave Owners (SO)<br>
  <span style='color:#56B4E9;'>●</span> MPs<br>
  <span style='color:#BFC0C0;'>●</span> Other nodes
")
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  selected_id <- reactiveVal(NULL)
  
  observeEvent(input$target, {
    selected_id(NULL)
  })
  
  observeEvent(input$run, {
    
    req(input$target)
    
    # Extract label (input is label value)
    input_label <- input$target
    
    matches <- labels_mp %>%
      filter(label == input_label)
    
    if (nrow(matches) == 1) {
      selected_id(matches$id[1])
    } else {
      selected_id(NULL)
    }
  })
  
  # Disambiguation (rare now but still safe)
  output$disambiguation_ui <- renderUI({
    
    req(input$target)
    
    matches <- labels_mp %>%
      filter(label == input$target)
    
    if (nrow(matches) > 1) {
      selectInput(
        "disambiguation",
        "Multiple matches found — select one:",
        choices = setNames(matches$id, paste0(matches$label, " (", matches$id, ")"))
      )
    }
  })
  
  observeEvent(input$disambiguation, {
    selected_id(input$disambiguation)
  })
  
  # Main computation
  results <- eventReactive(input$run, {
    
    req(input$target)
    
    matches <- labels_mp %>%
      filter(label == input$target)
    
    if (nrow(matches) == 0) {
      return(list(error = TRUE, message = "Name not found"))
    }
    
    target_string <- selected_id()
    
    if (is.null(target_string)) {
      return(list(error = TRUE, message = "Please select a valid match"))
    }
    
    if (!(target_string %in% V(g)$name)) {
      return(list(error = TRUE, message = "Node not in network"))
    }
    
    myorder <- input$degree
    
    ego_net <- ego(
      g,
      order = myorder,
      nodes = V(g)[name == target_string],
      mode = "in"
    )[[1]]
    
    num_type_a <- sum(V(g)[ego_net]$type == "SO")
    
    ego_subgraph <- induced_subgraph(g, vids = ego_net)
    
    observation_node <- V(ego_subgraph)[name == target_string]
    type_b_nodes <- V(ego_subgraph)[type == "SO"]
    
    shortest_paths_to_b <- shortest_paths(
      ego_subgraph,
      from = observation_node,
      to = type_b_nodes,
      mode = "in"
    )$vpath
    
    shortest_paths_to_b <- shortest_paths_to_b[
      sapply(shortest_paths_to_b, length) > 0
    ]
    
    list(
      error = FALSE,
      num_type_a = num_type_a,
      paths = shortest_paths_to_b,
      ego_subgraph = ego_subgraph,
      target = target_string,
      target_label = input$target
    )
  })
  
  # Error message (inline)
  output$error_message <- renderText({
    res <- results()
    
    if (!is.null(res) && res$error) {
      paste("Error:", res$message)
    } else {
      ""
    }
  })
  
  # Connections text BELOW graph
  output$connections_text <- renderText({
    res <- results()
    
    if (is.null(res) || res$error) {
      return("")
    }
    
    paste(
      "Number of connected slave owners:",
      res$num_type_a
    )
  })
  
  # Network plot
  output$network_plot <- renderVisNetwork({
    res <- results()
    
    if (is.null(res) || res$error || length(res$paths) == 0) {
      return(NULL)
    }
    
    nodes_in_paths <- unique(unlist(res$paths))
    sub_g <- induced_subgraph(res$ego_subgraph, vids = nodes_in_paths)
    
nodes <- data.frame(
  id = V(sub_g)$name,
  label = ifelse(
    is.na(match(V(sub_g)$name, labels$id)),
    V(sub_g)$name,
    labels$label[match(V(sub_g)$name, labels$id)]
  ),
  group = V(sub_g)$type
)

# Add Wikidata link
nodes$url <- paste0("https://www.wikidata.org/wiki/", nodes$id)

# Tooltip (hover)
nodes$title <- paste0(
  "<b>", nodes$label, "</b><br>",
  "ID: ", nodes$id, "<br>",
  "<a href='", nodes$url, "' target='_blank'>Open in Wikidata</a>"
)
    
nodes$color <- ifelse(
  nodes$id == res$target, "#2C3E50",       # target (dark)
  ifelse(nodes$group == "SO", "#E69F00",   # muted amber
         ifelse(nodes$group == "MP", "#56B4E9", "#BFC0C0")) # soft blue / grey
)
    
    edges <- as_data_frame(sub_g, what = "edges")
    
visNetwork(nodes, edges) %>%
  visOptions(
    highlightNearest = TRUE,
    nodesIdSelection = TRUE
  ) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEvents(selectNode = "function(nodes) {
    var node = nodes.nodes[0];
    var url = this.body.data.nodes.get(node).url;
    window.open(url, '_blank');
  }") %>%
  visEdges(
    arrows = "to",
    color = list(color = "#A0A0A0"),
    width = 1.5
  ) %>%
  visLayout(randomSeed = 42)
  })
}

shinyApp(ui, server)