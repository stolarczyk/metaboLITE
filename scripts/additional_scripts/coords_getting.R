mynetwork <- visNetwork(visdata$nodes, visdata$edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 1) %>%
  visEdges(color = "black", arrows = "to",smooth = list(enabled = TRUE, type = "vertical",roundness = 0.1,forceDirection = "vertical")) %>%
  visPhysics(enabled = FALSE) # disable physics to move nodes

require(shiny)
server <- function(input, output) {
  output$network <- renderVisNetwork({
    mynetwork
  })
  
  vals <- reactiveValues(coords=NULL)
  
  output$view <- renderPrint({
    write.table(vals$coords, file = "/home/mstolarczyk/Uczelnia/UVA/shinyapp/textbooky_coords.csv", sep = ",")
    vals$coords
  })
  
  observe({
    input$getcoord
    visNetworkProxy("network") %>% visGetPositions()
    vals$coords <- if (!is.null(input$network_positions)) 
      do.call(rbind, input$network_positions)
  })
}

ui <- fluidPage(
  visNetworkOutput("network", height = "800px"),
  actionButton("getcoord", "View & Save Coordinates"),
  verbatimTextOutput("view")
)

shinyApp(ui = ui, server = server)