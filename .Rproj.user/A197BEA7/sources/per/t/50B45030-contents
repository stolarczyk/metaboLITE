


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

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
        choices = c("lightgreen", "firebrick1", "lightblue"),
        selected = "lightblue",
        multiple = FALSE,
        selectize = TRUE,
        
        width = NULL,
        size = NULL
      ),
      selectInput(
        inputId = "color_reactions",
        label = "Select reactions color",
        choices = c("lightgreen", "firebrick1", "lightblue"),
        selected = "firebrick1",
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      radioButtons(inputId = "weighting",label = HTML("Apply weights to edges:"), choices = c("None" = "none", "log10(stoichiometry)" = "w", "GIMME" = "gimme")),
      actionButton("update", "Update")
    ),
    mainPanel(plotOutput("graph"))
  )
))
