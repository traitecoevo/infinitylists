library(shiny)
library(DT)
library(leaflet)
library(sf)
library(dplyr)

# Sample data
# This would typically be read from an external file
# observations <- read.csv("observations.csv")
observations <- data.frame(
  taxa = c("PlantA", "PlantA", "PlantA", "PlantA", "PlantA", "PlantA", "PlantA"),
  species = c("Species1", "Species2", "Species3", "Species4", "Species5", "Species6", "Species7"),
  voucher_type = c("Voucher1", "Voucher2", "Voucher1", "Voucher2", "Voucher1", "Voucher2", "Voucher1"),
  collection_date = as.Date(c("2020-01-01", "2021-01-01", "2019-01-01", "2020-01-05", "2021-03-01", "2022-01-01", "2022-03-01")),
  lat = c(-33.9, -33.86583, -33.86583, -33.86583, -33.865, -33.867, -33.86583),
  long = c(151.0106, 151.0113, 151.0113, 151.0106, 151.0106, 151.03, 151.01)
)

# Mockup list of places with their polygons (these would be real polygons in practice)
places <- list(
 "Duck1" = st_geometry(st_read("places/wategora-reserve-survey-area-approximate-boundaries.kml", crs = 4326)),
 "Duck2" = st_geometry(st_read("places/wategora-reserve-survey-area-approximate-boundaries.kml", crs = 4326))
 )





ui <- fluidPage(
  selectizeInput("place", "Choose a place:", choices = names(places), multiple = FALSE),
  selectizeInput("taxa", "Choose a taxa:", choices = unique(observations$taxa), multiple = FALSE),
  DTOutput("table"),
  leafletOutput("map")
)

server <- function(input, output) {
  filtered_data <- reactive({
    # Filter observations by selected taxa
    data <- observations[observations$taxa == input$taxa, ]
    
    # Check if data is within selected place polygon
    place_polygon <- places[[input$place]]
    points <- st_as_sf(data, coords = c("long", "lat"), crs = 4326)
    dplyr::tibble(data[st_intersects(points, place_polygon, sparse = FALSE)[, 1], ]) %>%
      dplyr::group_by(taxa) %>% dplyr::mutate(n=dplyr::n()) %>% dplyr::arrange(max(collection_date)) %>% dplyr::slice(1)
  })
  
  output$table <- renderDT({
    datatable(filtered_data())
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = filtered_data(), ~long, ~lat, popup = ~species) %>%
      addPolygons(data = place_polygon, color = "red")
  })
}

shinyApp(ui = ui, server = server)
