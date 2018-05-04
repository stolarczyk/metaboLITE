





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
    fluid = T,
    collapsible = T,
    tabPanel(
      "Visualize",
      value="visualize",
      sidebarPanel(
        fluidRow(class="myRowText",column(6,HTML("<u><b>Launch tabs:</b></u>")),column(1,offset = 0,actionLink("tabs_popover","",icon=icon("question-circle-o")))),
        fluidRow(class="myRowButton",
                 column(6, bsButton(inputId = "change_media",label = "Change media"))
                 ),
        fluidRow(class="myRowButton",
                 column(6,bsButton(inputId = "ko_rxn",label = "Gene knockout"))
                 ),
        fluidRow(class="myRowButton",
                 column(6,bsButton(inputId = "simulate_expr",label = "Transcriptomics experiment"))
                 ),
        fluidRow(class="myRowText",column(10,HTML("<u><b>Visualize the metabolic network: </b></u>"))),
        tags$head(tags$style(".myRowButton{height:50px;} .myRowText{height:50px}")),
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
        br(),br(),
        fluidRow(class="myRowText",column(5,HTML("<b>Objective value: </b>")),column(1,offset = 0,htmlOutput("text_flux")),column(1,offset = 1,actionLink("flux_popover","",icon=icon("question-circle-o")))),
        #htmlOutput("text_flux"),
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
        fluidRow(class="myRowText", column(8,HTML("<u><b>Use predefined media: </b></u>")),column(1,offset = 0,actionLink("apply_media_popover","",icon = icon("question-circle-o")))),
        fluidRow(class="myRowButton", column(6,bsButton(inputId = "media1",label = "Glucose free media"))),
        fluidRow(class="myRowButton", column(6,bsButton(inputId = "media2",label = "Microaerophilic media"))),
        fluidRow(class="myRowButton", column(6,bsButton(inputId = "media3",label = "Lactate rich media"))),
        fluidRow(class="myRowButton", column(6,bsButton(inputId = "media_custom",label = "Custom media"))),
        br(),
        fluidRow(class="myRowButton",column(5,HTML("<b>Objective value: </b>")),column(1,offset = 0,htmlOutput("text_flux_media")),column(1,offset = 1,actionLink("flux_popover_media","",icon=icon("question-circle-o")))),
        br(),
        div(style = "vertical-align:top; width: 75%;height: 30px", htmlOutput("text_own")),
        uiOutput("pick_rxn"),
        fluidRow(class="myRowButton",
                 column(10,uiOutput("range")),column(1,offset = 0,uiOutput("range_help") )),
        uiOutput("button_apply_media")
      ),
      mainPanel(visNetworkOutput(
        "graph_media", width = "800", height = "600"
      ),width = 8)
    ),
    tabPanel(
      "Gene knockout",
      value = "ko_reactions",
      sidebarPanel(
        fluidRow(class="myRowText",column(7,HTML("<b><u>Pick a reaction to knock out:</u></b>")), column(1,offset = 0,actionLink("pick_rxn_ko_popover","",icon=icon("question-circle-o")))),
        uiOutput("pick_ko_rxn"),
        div(style = "vertical-align:top; width: 50%;height: 60px", uiOutput("button_apply_ko")),
        div(style = "vertical-align:top; width: 30%;height: 60px",uiOutput("reset_ko")),
        br(),
        fluidRow(class="myRowButton", column(5,HTML("<b>Objective value: </b>")),column(1,htmlOutput("text_flux_ko")),column(1,offset = 1,actionLink("flux_popover_ko","",icon=icon("question-circle-o")))),
        tableOutput(outputId = 'fluxes_ko')
      ),
      mainPanel(visNetworkOutput(
        "graph_ko", width = "800", height = "600"
      ))
    ),
    tabPanel(
      "Transcriptomics experiment",
      value = "simulate_expression_changes",
      sidebarPanel(
        fluidRow(class="myRowText",column(9,HTML("<b>Pick a gene for expression adjustment:</b>")),column(1,offset = 0,actionLink("expression_popover","",icon=icon("question-circle-o")))),
        uiOutput("pick_expr_gene"),
        HTML("<b>Select the gene expression level:</b>"),
        uiOutput("expr"),
        uiOutput("button_apply_expr"),
        br(),br(),
        fluidRow(class="myRowButton", column(5,HTML("<b>Objective value: </b>")),column(1,htmlOutput("text_flux_expr")),column(1,offset = 1,actionLink("flux_popover_ko","",icon=icon("question-circle-o")))),        tableOutput(outputId = 'fluxes_expr'),width = 4
      ),
      mainPanel(visNetworkOutput(
        "graph_expr", width = "800", height = "600"
      ),width = 8)
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
