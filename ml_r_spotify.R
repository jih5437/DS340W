library(shiny)
library(spotifyr)
library(tibble)
library(class)

Sys.setenv(SPOTIFY_CLIENT_ID = '68588c6543464d40b3e17ca23e3f178c')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '32956d387caf47b38fbed3b39666d430')

access_token <- get_spotify_access_token()

# Load audio features data
playlist_tracks <- get_playlist_tracks("6UeSakyzhiEt4NB3UAd6NQ")
playlist_track_ids <- playlist_tracks$track.id
audio_features <- get_track_audio_features(playlist_track_ids)


# Train KNN model on audio features data
train_data <- audio_features[, c("danceability", "energy", "valence")]
train_labels <- audio_features$danceability
k <- 5
model <- knn(train_data, train_data, train_labels, k)

ui <- fluidPage(
  # Text input for artist name
  textInput(inputId = "artist_input", label = "Enter artist name"),
  
  # Sliders for danceability, energy, and valence
  sliderInput(inputId = "danceability_range", label = "Danceability:",
              min = 0, max = 1, value = c(0.3, 0.8), step = 0.1),
  sliderInput(inputId = "energy_range", label = "Energy:",
              min = 0, max = 1, value = c(0.3, 0.8), step = 0.1),
  sliderInput(inputId = "valence_range", label = "Valence:",
              min = 0, max = 1, value = c(0.3, 0.8), step = 0.1),
  
  # Submit button
  actionButton(inputId = "submit_button", label = "Submit"),
  
  # Table to display results
  dataTableOutput(outputId = "result_table1"),
  
  # Table to display additional results
  dataTableOutput(outputId = "result_table2")
)

server <- function(input, output) {
  # Create reactive expression for artist data
  artist_data <- reactive({
    pull <- get_artist_audio_features(input$artist_input)
    result <- data_frame(pull)
    if (is.data.frame(result)) {
      result
    } else {
      NULL
    }
  })
  
  # Filter the artist data based on the slider ranges
  filtered_data <- reactive({
    artist_data() %>%
      filter(danceability >= input$danceability_range[1] & danceability <= input$danceability_range[2],
             energy >= input$energy_range[1] & energy <= input$energy_range[2],
             valence >= input$valence_range[1] & valence <= input$valence_range[2])
  })
  
  # Predict similar artists using KNN model
  similar_artists <- reactive({
    new_data <- filtered_data() %>%
      select(danceability, energy, valence)
    if (nrow(new_data) > 0) {
      knn(train_data, new_data, train_labels, k)
    } else {
      NULL
    }
  })
  
  # Render table output
  output$result_table1 <- renderDataTable({
    if (!is.null(similar_artists())) {
      data.frame(Artist = similar_artists()) %>%
        filter(Artist != input$artist_input) %>%
        arrange(Artist)
    } else {
      NULL
    }
  })
  
  # Render another table output
  output$result_table2 <- renderDataTable({
    if (!is.null(filtered_data())) {
      filtered_data() %>%
        select(artist_name, danceability, energy, valence, track_name, external_urls.spotify) %>%
        arrange(artist_name)
    } else {
      NULL
    }
  })
}

shinyApp(ui, server)
