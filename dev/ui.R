




# This is the user-interface definition of a Shiny web application.

library(shiny)
library(rsconnect)
library(visNetwork)
library(shinythemes)
shinyUI(
  navbarPage(
    "Toycon model",
    theme = shinytheme("cosmo"),
    id = "tabs",
    tabPanel(
      "Visualize",
      sidebarPanel(
        fileInput(
          inputId = "file",
          label = "Select a model to visualize:",
          accept = "text/xml",
          buttonLabel = "Browse...",
          placeholder = "No file selected",
          width = "500px"
        ),
        
        div(style = "vertical-align:top;horizontal-align:left; width: 150px; height: 50px", uiOutput("change_media")),
        div(style = "vertical-align:top;horizontal-align:right; width: 150px;height: 60px", uiOutput("ko_rxn")),
        
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
        tableOutput(outputId = 'fluxes')
      ),
      mainPanel(visNetworkOutput(
        "graph", width = "600", height = "600"
      ))
    ),
    tabPanel(
      "Change media",
      sidebarPanel(
        uiOutput("lbound"),
        uiOutput("ubound"),
        uiOutput("pick_rxn"),
        uiOutput("button_apply_media"),
        htmlOutput("text_flux_media")
      )
    ),
    tabPanel(
      "KO reactions",
      sidebarPanel(
        uiOutput("pick_ko_rxn"),
        uiOutput("button_apply_ko"),
        htmlOutput("text_flux_ko")
      ),
      mainPanel(visNetworkOutput(
        "graph_ko", width = "100%", height = "100%"
      ))
    )
  )
)
