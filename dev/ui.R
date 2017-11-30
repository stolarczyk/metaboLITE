



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
      uiOutput("Box2"),
      uiOutput("Box3"),
      uiOutput("Box1"),
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
      htmlOutput("text_flux"),
      tableOutput(outputId = 'fluxes'),
      width = 5
    ),
    mainPanel(
      visNetworkOutput(
      "graph", width = "100%", height = "20%"
    ), width = 7)
  )
))
