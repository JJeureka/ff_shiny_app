#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(base)
library(dplyr)
library(jsonlite)
library(forcats)
library(ggplot2)
library(graphics)
library(grDevices)
library(httr)
library(kableExtra)
library(methods)
library(purrr)
library(readr)
library(stats)
library(tibble)
library(tidyr)
library(tidyverse)
library(yaml)

# app.R

library(shiny)
library(dplyr)
library(kableExtra)
library(jsonlite)
source("helper.R")  # This is the file that contains the display_data_function

# Define the UI
ui <- fluidPage(
  titlePanel("Fantasy Football Points Summary"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("league_input", "Enter League ID"),
      numericInput("year_input", "Enter Season Year", value = 2022, min = 2000, max = 2024),
      actionButton("submit_button", "Get Summary")
    ),
    
    mainPanel(
      uiOutput("summary_table")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Create a reactive expression for the summary table
  summary_table_reactive <- eventReactive(input$submit_runbutton, {
    league_input <- input$league_input
    year_input <- input$year_input
    
    # Call the display_data_function from the helper.R file
    display_data_function(league_input, year_input)
  })
  
  # Render the summary table when the submit button is clicked
  output$summary_table <- renderUI({
    summary_table_reactive() %>%
      HTML()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

