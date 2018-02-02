





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
        div(style = "vertical-align:top; width: 50%;height: 60px", uiOutput("simulate_expr")),
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
                   label = "Apply"),
          title = "Apply the settings",
          content = "Greater flux through reactions or stoichiometry in the model will be shown as thicker edges of the graph",
          placement = "right",
          trigger = "hover"
        ),
        
        
        htmlOutput("text_flux"),
        tableOutput(outputId = 'fluxes'),
        #checkboxInput(inputId="pro",label="Pro mode",value = F),
        width = 5
      ),
      mainPanel(visNetworkOutput(
        "graph", width = "800", height = "600"
      ), width = 7)
    ),
    tabPanel(
      "Change media",
      sidebarPanel(        
        div(style = "vertical-align:top; width: 50%;height: 30px", htmlOutput("text_media")),
        div(style = "vertical-align:top; width: 45%;height: 60px", uiOutput("media1")),
        div(style = "vertical-align:top; width: 50%;height: 60px", uiOutput("media2")),
        div(style = "vertical-align:top; width: 45%;height: 60px", uiOutput("media3")),
        div(style = "vertical-align:top; width: 75%;height: 30px", htmlOutput("text_own")),
        uiOutput("pick_rxn"),
        uiOutput("range"),
        bsPopover("range","Technical information", "This slider adjusts the upper and lower bound, which define the maximum and minimum allowable fluxes of the reactions.","bottom",trigger = "click"),
        # uiOutput("lbound"),
        # uiOutput("ubound"),
        uiOutput("button_apply_media"),
        htmlOutput("text_flux_media")

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
    ),
    tabPanel(
      "Simulate expression changes",
      sidebarPanel(
        uiOutput("pick_expr_gene"),
        uiOutput("expr"),
        uiOutput("button_apply_expr"),
        htmlOutput("text_flux_expr"),
        tableOutput(outputId = 'fluxes_expr')
      ),
      mainPanel(visNetworkOutput(
        "graph_expr", width = "800", height = "600"
      ))
    )
  )
)
