



# This is the user-interface definition of a Shiny web application.

library(shiny)
library(rsconnect)
library(visNetwork)
shinyUI(fluidPage(
  # Application title
  titlePanel("Toycon model"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "file",
        label = "Select model to visualize:",
        accept = ".xml",
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      selectInput(
        inputId = "color_metabolites",
        label = "Select metabolites color",
        choices = c("lightblue", "lightgreen", "red"),
        selected = "green",
        multiple = FALSE,
        selectize = TRUE,
        
        width = NULL,
        size = NULL
      ),
      selectInput(
        inputId = "color_reactions",
        label = "Select reactions color",
        choices = c("lightblue", "lightgreen", "red"),
        selected = "red",
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      radioButtons(
        inputId = "weighting",
        label = HTML("Apply weights to edges:"),
        choices = c(
          "None" = "none",
          "log(stoichiometry)" = "stoichiometry",
          "log(GIMME)" = "gimme",
          "GIMME&stoichiometry" = "gimmestoichiometry"
        )
      ),
      actionButton("update", "Update"),
      tableOutput(outputId = 'fluxes'),
      width = 4
    ),
    mainPanel(visNetworkOutput(
      "graph", width = "100%", height = "20%"
    ), width = 8)
  )
))
