


# This is the user-interface definition of a Shiny web application.

library(shiny)
library(shinythemes)
library(visNetwork)
library(shinyBS)
library(markdown)
library(htmlwidgets)


shinyUI(
  navbarPage(
    "iNRG Model", #chamnge the name
    theme = shinytheme("cosmo"),
    id = "tabs",
    fluid = T,
    collapsible = T,
    tabPanel(
      "Visualize network",
      value = "visualize",
      sidebarPanel(
        h3("Experiment setup"),
        br(),
        br(),
        fluidRow(class = "myRowText", column(7, HTML(
          "<u><b>Choose the model:</b></u>"
        )), column(
          2,
          offset = 0,
          popify(
            actionLink("choose_model_popover", "", icon = icon("question-circle")),
            title = "Pick a model to run the experimets with",
            options = list(container = "body"),
            content = "There are two models available. First is a toy example which transparently shows the concepts of genome-scale metabolic modeling. The second one is a real-life example, great for investigation of such models behavior.",
            trigger = "click",
            placement = "right"
          )
        )),
        fluidRow(class = "myRowText", column(
          8,
          HTML(
            "<font color='#808080'>The choice will be effective for each experiment in this application.</font>"
          )
        )),
        br(),
        fluidRow(column(
          7,
          selectInput(
            inputId = "pick_model",
            label = NULL,
            choices = list("iNRG" = "toycon", "Ecoli core metabolism" = "ecoli"),
            selected = "toycon",
            width = "100%"
          )
        ), column(
          2,
          offset = 0,
          actionLink("model_stats", "", icon = icon("info-circle"))
        )),
        tags$div(id = 'placeholder'),
        fluidRow(class = "myRowText", column(7, HTML(
          "<u><b>Select the biological objective:</b></u>"
        )), column(
          2,
          offset = 0,
          popify(
            actionLink("select_objective_popover", "", icon = icon("question-circle")),
            options = list(container = "body"),
            title = "Select the biological objective",
            content = "The biological objective is the reaction that is relevant to the problem being studied. In the case of predicting growth, the objective is biomass production. Whereas in case of investigation energy metabolism - the ATP production.",
            trigger = "click",
            placement = "right"
          )
        )),
        tags$div(id = 'placeholder1'),
        fluidRow(class = "myRowText", column(
          7, HTML("<u><b>Run experiments:</b></u>")
        ), column(
          1, offset = 0, actionLink("tabs_popover", "", icon = icon("question-circle"))
        )),
        fluidRow(class = "myRowButton",
                 column(
                   7,
                   bsButton(
                     inputId = "change_media",
                     block = T,
                     label = "Change media"
                   )
                 )),
        fluidRow(class = "myRowButton",
                 column(
                   7,
                   bsButton(
                     inputId = "ko_rxn",
                     block = T,
                     label = "Simulate gene knockout"
                   )
                 )),
        fluidRow(class = "myRowButton",
                 column(
                   7,
                   bsButton(
                     inputId = "simulate_expr",
                     block = T,
                     label = "Integrate transcriptomic data"
                   )
                 )),
        hr(),
        fluidRow(class = "myRowText", column(
          10, HTML("<u><b>Visualize the metabolic network: </b></u>")
        )),
        tags$head(
          tags$style(
            ".myRowButton{height:50px;} .myRowText{height:50px} .myRowList{height:100px;} .myRowSmall{height:20px;}"
          ),
          tags$style(".fa-question-circle {color:#919499}"),
          tags$style(".fa-info-circle {color:#919499}"),
          tags$style(".fa-table {color:#919499}"),
          tags$style(HTML("hr {border-top: 2px solid #bcbcbc;}"))
        ),
        radioButtons(
          inputId = "weighting",
          label = HTML("Display weights:"),
          selected = "none",
          choices = c("None" = "none",
                      "Stoichiometry" = "stoichiometry"),
          width = "50%"
        ),
        
        popify(
          bsButton(
            inputId = "update",
            label = "Update",
            value = T
          ),
          title = "Apply the settings",
          content = "Greater flux through reactions or stoichiometry in the model will be shown as thicker edges of the graph",
          placement = "right",
          trigger = "hover"
        ),
        br(),
        hr(),
        br(),
        h3("Results"),
        br(),
        br(),

        DT::dataTableOutput('fluxes'),
        bsModal(
          id = "modal_model_stats",
          title = "Model statistics",
          trigger = "model_stats",
          size = "small",
          DT::dataTableOutput("model_stats")
        ),
        width = 4
      ),
      mainPanel(visNetworkOutput("graph", height = "1200"), width = 8)
    ),
    tabPanel("Help",
             value = "help",
             mainPanel(includeMarkdown(path = "help.md")))
  )
)