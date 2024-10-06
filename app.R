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
source("helper1.R")


ui <- fluidPage(
  titlePanel("Fantasy Football Points Summary"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("league_input", "Enter League ID"),
      numericInput("start_year", "Enter Starting Year", value = 2014, min = 2000, max = 2023),
      numericInput("end_year", "Enter Ending Year", value = 2023, min = 2000, max = 2023),
      actionButton("submit_button", "Get Summary")
    ),
    
    mainPanel(
      uiOutput("summary_table")
    )
  )
)

server <- function(input, output, session) {
  
  summary_table_reactive <- eventReactive(input$submit_button, {
    league_input <- input$league_input
    start_year <- input$start_year
    end_year <- input$end_year
    
    get_all_years(league_input, start_year, end_year)
  })
  
  output$summary_table <- renderUI({
    summary_table_reactive() %>%
      HTML()
  })
}

shinyApp(ui = ui, server = server)

