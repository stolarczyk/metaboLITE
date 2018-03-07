





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
      value="visualize",
      sidebarPanel(
        fluidRow(class="myRowText",column(10,HTML("<u><b>Launch tabs with following functionalities:</b></u>")),column(2,actionLink("tabs_popover","",icon=icon("question-circle-o")))),
        fluidRow(class="myRowButton",
                 column(6, bsButton(inputId = "change_media",label = "Change media",block = T))
                 ),
        fluidRow(class="myRowButton",
                 column(6,bsButton(inputId = "ko_rxn",label = "KO reaction",block = T))
                 ),
        fluidRow(class="myRowButton",
                 column(6,bsButton(inputId = "simulate_expr",label = "Change expression",block = T))
                 ),
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
        fluidRow(class="myRowText", column(10,HTML("<u><b>Use predefined media: </b></u>"))),
        fluidRow(class="myRowButton", column(6,bsButton(inputId = "media1",label = "Glucose free media"))),
        fluidRow(class="myRowButton", column(6,bsButton(inputId = "media2",label = "Microaerophilic media"))),
        fluidRow(class="myRowButton", column(6,bsButton(inputId = "media3",label = "Lactate rich media"))),
        div(style = "vertical-align:top; width: 75%;height: 30px", htmlOutput("text_own")),
        uiOutput("pick_rxn"),
        fluidRow(class="myRowButton",
                 column(10,uiOutput("range")),column(1,offset = 0,actionLink("range_popover","",icon=icon("question-circle-o")) )),
        fluidRow(column(4,uiOutput("button_apply_media"))),
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
        fluidRow(class="myRowText",column(9,HTML("<b>Pick a gene for expression adjustment:</b>")),column(1,offset = 0,actionLink("expression_popover","",icon=icon("question-circle-o")))),
        uiOutput("pick_expr_gene"),
        HTML("<b>Select the gene expression level:</b>"),
        uiOutput("expr"),
        uiOutput("button_apply_expr"),
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
