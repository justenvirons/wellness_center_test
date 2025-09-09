# Load required packages
# rsconnect::writeManifest()
library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(tidygeocoder)
library(sf)
library(dplyr)

# Load app data
load("wellness_app_data.RData")

ui <- fluidPage(
  titlePanel("Recruitment Screening Tool"),
  sidebarLayout(
    sidebarPanel(
      textInput("address", "Enter address (e.g., 4305 W Madison St, Chicago):", ""),
      actionButton("submit", "Submit"),
      # verbatimTextOutput("tract_output")
      div(style = "white-space: normal; word-wrap: break-word; margin-top: 10px; font-style: italic;",
          textOutput("tract_output"))
    ),
  mainPanel(
    leafletOutput("map", height = 600)
  )
    
    # mainPanel(
    #   textInput("address", "Enter address (e.g., 4305 W Madison St, Chicago):", ""),
    #   actionButton("submit", "Submit"),
    #   # verbatimTextOutput("tract_output")
    #   div(style = "white-space: normal; word-wrap: break-word; margin-top: 10px; font-style: italic;",
    #       textOutput("tract_output"))
    # )
  )
)

server <- function(input, output, session) {
  # Base map
  output$map <- renderLeaflet({
    wellness_center_map
  })
  
  observeEvent(input$submit, {
    req(input$address)
    
    result <- tryCatch({
      geo(address = input$address, method = 'census', lat = latitude, long = longitude, full_results = FALSE)
    }, error = function(e) {
      NULL
    })
    
    # Validate that result is not NULL and has expected columns
    if (!is.null(result) && nrow(result) > 0 && !is.na(result$longitude) && !is.na(result$latitude)) {
      coords <- st_as_sf(result, coords = c("longitude", "latitude"), crs = 4326)
      
      tract_match <- st_join(coords, catchment_areas, join = st_within)
      
      if (nrow(tract_match) > 0 && !is.na(tract_match$name[1])) {
        tract_id <- tract_match$name[1]
        
        output$tract_output <- renderText({
          paste0("Yes! This address is within the study boundary.")
        })
        
        leafletProxy("map") %>%
          setView(lng = result$longitude, lat = result$latitude, zoom = 11) %>% 
          clearMarkers() %>%
          addMarkers(lng = result$longitude, lat = result$latitude, popup = paste0("Inside ", tract_id," boundary")) 
        
      } else {
        output$tract_output <- renderText({"We are sorry but this address does not qualify for the study."})
        
        leafletProxy("map") %>%
          setView(lng = result$longitude, lat = result$latitude, zoom = 11) %>% 
          clearMarkers() %>%
          addMarkers(lng = result$longitude, lat = result$latitude) 
      }
      
    } else {
      output$tract_output <- renderText({"Address could not be geocoded."})
    }
  })
  
}

shinyApp(ui, server)

