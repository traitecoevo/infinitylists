library(shiny)
library(DT)
library(leaflet)
library(sf)
library(dplyr)

# Sample data
# This would typically be read from an external file
# observations <- read.csv("observations.csv")
observations <- data.frame(
  taxa = c("PlantA", "PlantB", "PlantA", "PlantC", "PlantD", "PlantE", "PlantF"),
  species = c("Species1", "Species2", "Species3", "Species4", "Species5", "Species6", "Species7"),
  voucher_type = c("Voucher1", "Voucher2", "Voucher1", "Voucher2", "Voucher1", "Voucher2", "Voucher1"),
  collection_date = as.Date(c("2020-01-01", "2021-01-01", "2019-01-01", "2020-01-05", "2021-03-01", "2022-01-01", "2022-03-01")),
  lat = c(40, 42, 41, 43, 43.5, 44, 42.5),
  long = c(-100, -102, -101, -103, -102.5, -101.5, -102)
)

# Mockup list of places with their polygons (these would be real polygons in practice)
places <- list(
  "PlaceA" = matrix(c(-99, -99, -104, -104, -99,39, 44, 44, 39, 39), ncol=2),
  "PlaceB" = matrix(c(-101, -101, -105, -105, -101,42, 45, 45, 42, 42), ncol=2)
)

ui <- fluidPage(
  selectizeInput("place", "Choose a place:", choices = names(places), multiple = FALSE),
  selectizeInput("taxa", "Choose a taxa:", choices = unique(observations$taxa), multiple = FALSE),
  DTOutput("table")
)

server <- function(input, output) {
  filtered_data <- reactive({
    # Filter observations by selected taxa
    data <- observations[observations$taxa == input$taxa, ]
    
    # Check if data is within selected place polygon
    place_polygon <- st_polygon(list(places[[input$place]]))
    points <- st_as_sf(data, coords = c("long", "lat"), crs = 4326)
    dplyr::tibble(data[st_intersects(points, place_polygon, sparse = FALSE)[, 1], ]) %>%
      dplyr::group_by(taxa) %>% dplyr::mutate(n=dplyr::n()) %>% dplyr::arrange(max(collection_date)) %>% dplyr::slice(1)
  })
  
  output$table <- renderDT({
    datatable(filtered_data())
  })
}

shinyApp(ui = ui, server = server)
