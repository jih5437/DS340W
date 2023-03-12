library(tidyverse)
library(rjson)
library(jsonlite)
library(shiny)
library(dplyr)
library(ggplot2)

kPlaylists <- read.csv("kaggle_playlists.csv")
kPlaylistsGenre <- read.csv("kaggle_genres.csv")

data <- kPlaylistsGenre %>%
  select(1, 2, 7, 20)

ui <- fluidPage(
  titlePanel("Filtering Song Characteristics"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("danceability_slider", "Danceability Threshold", min = 0, max = 1, value = 0.5, step = 0.01),
      sliderInput("energy_slider", "Energy Threshold", min = 0, max = 1, value = 0.5, step = 0.01),
      sliderInput("acousticness_slider", "Acousticness Threshold", min = 0, max = 1, value = 0.5, step = 0.01)
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output) {
  output$table <- renderTable({
    filtered_data <- data %>% 
      filter(danceability > input$danceability_slider,
             energy > input$energy_slider,
             acousticness > input$acousticness_slider) %>% 
      head(10)
    
    filtered_data
  })
}

shinyApp(ui = ui, server = server)