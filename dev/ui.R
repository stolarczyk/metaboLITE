


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rsconnect)
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
      selectInput(
        inputId = "color_metabolites",
        label = "Select metabolites color",
        choices = c("lightblue", "lightgreen", "red"),
        selected = "green",
        multiple = FALSE,
        selectize = TRUE,
        
        width = NULL,
        size = NULL
      ),
      selectInput(
        inputId = "color_reactions",
        label = "Select reactions color",
        choices = c("lightblue", "lightgreen", "red"),
        selected = "red",
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      radioButtons(inputId = "weighting",label = HTML("Apply weights to edges:"), choices = c("None" = "none", "log10(stoichiometry)" = "stoichiometry", "log10(GIMME)" = "gimme")),
      actionButton("update", "Update"),
      textOutput("fluxes")
    ),
    mainPanel(visNetworkOutput("graph"))
  )
))
