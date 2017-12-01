



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
        placeholder = "No file selected",
        width = "400px"
      ),
      uiOutput("change_media"),
      uiOutput("ko_rxn"),
      uiOutput("lbound"),
      uiOutput("ubound"),
      uiOutput("pick_rxn"),
      uiOutput("button_apply_media"),
      uiOutput("pick_rxn_ko"),
      uiOutput("button_apply_ko"),
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
