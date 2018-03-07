





# This is the user-interface definition of a Shiny web application.

library(shiny)
library(shinythemes)
library(visNetwork)
library(shinyBS)
library(markdown)

shinyUI(
  navbarPage(
    "iNRG Model",
    theme = shinytheme("cosmo"),
    id = "tabs",
    tabPanel(
      "Visualize",
      sidebarPanel(
        fluidRow(class="myRowText",column(10,HTML("<u><b>Launch tabs with following functionalities:</b></u>"))),
        fluidRow(class="myRowButton",
                 column(2, bsButton(inputId = "change_media",label = "Change media")),
                 column(1,offset = 3,actionLink("change_media_popover","",icon=icon("question-circle-o")))
                 ),
        fluidRow(class="myRowButton",
                 column(2,bsButton(inputId = "ko_rxn",label = "KO reaction")),
                 column(1,offset = 2,actionLink("ko_rxn_popover","",icon=icon("question-circle-o")))
                 ),
        fluidRow(class="myRowButton",
                 column(3,bsButton(inputId = "simulate_expr",label = "Simulate expression changes")),
                 column(1,offset = 4,actionLink("simulate_expr_popover","",icon=icon("question-circle-o")))),
        fluidRow(class="myRowText",column(10,HTML("<u><b>Visualize the metabolic network: </b></u>"))),tags$head(tags$style(".myRowButton{height:50px;} .myRowText{height:30px}")),
        radioButtons(
          inputId = "weighting",
          label = HTML("Display weights:"),
          choices = c(
            "None" = "none",
            "Stoichiometry" = "stoichiometry"
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
        width = 4
      ),
      mainPanel(visNetworkOutput(
        "graph", width = "800", height = "600"
      ), width = 8)
    ),
    tabPanel(
      "Change media",
      value = "change_media",
      sidebarPanel(        
        div(style = "vertical-align:top; width: 50%;height: 30px", htmlOutput("text_media")),
        div(style = "vertical-align:top; width: 45%;height: 60px", uiOutput("media1")),
        div(style = "vertical-align:top; width: 50%;height: 60px", uiOutput("media2")),
        div(style = "vertical-align:top; width: 45%;height: 60px", uiOutput("media3")),
        div(style = "vertical-align:top; width: 75%;height: 30px", htmlOutput("text_own")),
        uiOutput("pick_rxn"),
        uiOutput("range"),
        bsPopover("range","Technical information", "This slider adjusts the upper and lower bound, which define the maximum and minimum allowable fluxes of the reactions.","bottom",trigger = "hover"),
        uiOutput("button_apply_media"),
        htmlOutput("text_flux_media")

      ),
      mainPanel(visNetworkOutput(
        "graph_media", width = "800", height = "600"
      ))
    ),
    tabPanel(
      "KO reactions",
      value = "ko_reactions",
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
      value = "simulate_expression_changes",
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
    ),
    tabPanel(
      "Help",
      value = "help",
      mainPanel(
        includeMarkdown(path = "help.md")
      )
    )
  )
)
