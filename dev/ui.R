





# This is the user-interface definition of a Shiny web application.

library(shiny)
library(shinythemes)
library(visNetwork)
library(shinyBS)

shinyUI(
  navbarPage(
    "Toycon model",
    theme = shinytheme("cosmo"),
    id = "tabs",
    tabPanel(
      "Visualize",
      sidebarPanel(
        div(style = "vertical-align:top; width: 30%;height: 60px", uiOutput("change_media")),
        div(style = "vertical-align:top; width: 30%;height: 60px", uiOutput("ko_rxn")),

        radioButtons(
          inputId = "weighting",
          label = HTML("Visualize flux on the graph:"),
          choices = c(
            "None" = "none",
            "log2(stoichiometry)" = "stoichiometry"
          ),width = "50%"
        ),
        
        popify(
          bsButton(inputId = "update",
                   label = "Apply weights"),
          title = "Weighting",
          content = "Greater flux through reactions in the model will be shown as thicker edges of the graph",
          placement = "right",
          trigger = "hover"
        ),
        
        
        htmlOutput("text_flux"),
        tableOutput(outputId = 'fluxes'),
        width = 5
      ),
      mainPanel(visNetworkOutput(
        "graph", width = "700", height = "600"
      ), width = 7)
    ),
    tabPanel(
      "Change media",
      sidebarPanel(
        uiOutput("pick_rxn"),
        uiOutput("lbound"),
        uiOutput("ubound"),
        uiOutput("button_apply_media"),
        htmlOutput("text_flux_media"),
        div(style = "vertical-align:top; width: 50%;height: 60px", htmlOutput("text_media")),
        div(style = "vertical-align:top; width: 25%;height: 60px", uiOutput("media1")),
        div(style = "vertical-align:top; width: 25%;height: 60px", uiOutput("media2")),
        div(style = "vertical-align:top; width: 25%;height: 60px", uiOutput("media3"))
      ),
      mainPanel(visNetworkOutput(
        "graph_media", width = "800", height = "600"
      ))
    ),
    tabPanel(
      "KO reactions",
      sidebarPanel(
        uiOutput("pick_ko_rxn"),
        div(style = "vertical-align:top; width: 50%;height: 60px", uiOutput("button_apply_ko")),
        div(style = "vertical-align:top; width: 30%;height: 60px",uiOutput("reset_ko")),
        htmlOutput("text_flux_ko"),
        tableOutput(outputId = 'fluxes_ko')
      ),
      mainPanel(visNetworkOutput(
        "graph_ko", width = "800", height = "600"
      ))
    )
  )
)
