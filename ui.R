
# This is the user-interface definition of a Shiny web application.

library(shiny)
library(shinythemes)
library(visNetwork)
library(shinyBS)
library(markdown)
library(htmlwidgets)


shinyUI(
  navbarPage(
    "iNRG Model",
    theme = shinytheme("cosmo"),
    id = "tabs",
    fluid = T,
    collapsible = T,
    tabPanel(
      "Visualize network",
      value="visualize",
      sidebarPanel(
        verbatimTextOutput('text'),
        fluidRow(class="myRowList",column(6,selectInput(inputId = "pick_model",label = "Choose model:",choices = list("iNRG"="toycon","Ecoli"="ecoli"),selected = "toycon",width = "55%")),column(2,offset = 0,popify(actionLink("choose_model_popover","",icon=icon("question-circle-o")),title = "Pick a model to run the experimets with",options = list(container = "body"),content = "There are two models available. First is a toy example which transparently shows the concepts of genome-scale metabolic modeling. The second one is a real-life example, great for investigation of such models behavior.",trigger = "click",placement = "right"))),
        fluidRow(class="myRowText",column(6,HTML("<u><b>Run experiments:</b></u>")),column(1,offset = 0,actionLink("tabs_popover","",icon=icon("question-circle-o")))),
        fluidRow(class="myRowButton",
                 column(7, bsButton(inputId = "change_media",block = T,label = "Change media"))
        ),
        fluidRow(class="myRowButton",
                 column(7,bsButton(inputId = "ko_rxn",block = T,label = "Simulate gene knockout"))
        ),
        fluidRow(class="myRowButton",
                 column(7,bsButton(inputId = "simulate_expr",block = T,label = "Integrate transcriptomic data"))
        ),
        hr(),
        fluidRow(class="myRowText",column(10,HTML("<u><b>Visualize the metabolic network: </b></u>"))),
        tags$head(tags$style(".myRowButton{height:50px;} .myRowText{height:50px} .myRowList{height:100px;}")),
        tags$head(tags$style(HTML("hr {border-top: 2px solid #bcbcbc;}"))),
        radioButtons(
          inputId = "weighting",
          label = HTML("Display weights:"),selected = "none",
          choices = c(
            "None" = "none",
            "Stoichiometry" = "stoichiometry"
          ),width = "50%"
        ),
        
        popify(
          bsButton(inputId = "update",
                   label = "Apply",value = T),
          title = "Apply the settings",
          content = "Greater flux through reactions or stoichiometry in the model will be shown as thicker edges of the graph",
          placement = "right",
          trigger = "hover"
        ),
        br(),
        hr(),
        fluidRow(class="myRowText",column(5,HTML("<b>Objective value: </b>")),column(1,offset = 0,htmlOutput("text_flux")),column(1,offset = 1,popify(actionLink("flux_popover","",icon=icon("question-circle-o")),title = "Objective value",
                                                                                                                                                      content = "Since iNRG represents the energy metabolism its biological objective is ATP production. Consequently, this reaction is maximized while solving the FBA problem. Therefore, it can be interpreted as arbitrary units of ATP.",
                                                                                                                                                      placement = "right",
                                                                                                                                                      trigger = "click",                                                                                                                                  options = list(container = "body")))),
        DT::dataTableOutput('fluxes'),
        width = 4
      ),
      mainPanel(visNetworkOutput(
        "graph",height = "700"
      ), width = 8)
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