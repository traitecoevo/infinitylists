library(shiny)
library(DT)
library(leaflet)
library(sf)
library(dplyr)
library(data.table)

ala<-data.table::fread("ala_nsw_inat_avh.csv")
ala$taxa<-stringr::word(ala$species,1,1)
ala$voucher_type<-case_when(ala$basisOfRecord=="PRESERVED_SPECIMEN" ~ "Collection",
                            TRUE ~ "Photograph")
ala$lat<-ala$decimalLatitude
ala$long<-ala$decimalLongitude
ala$year<-year(ala$eventDate)




# Mockup list of places with their polygons (these would be real polygons in practice)
places <- list(
 "Duck1" = st_geometry(st_read("places/wategora-reserve-survey-area-approximate-boundaries.kml", crs = 4326)),
 "Duck2" = st_geometry(st_read("places/wategora-reserve-survey-area-approximate-boundaries.kml", crs = 4326))
 )





ui <- fluidPage(
  selectizeInput(inputId="place", label ="Choose a place:", choices =  names(places)),
  selectizeInput(inputId="taxa", label ="Choose a taxa:", choices = sort(unique(ala$taxa))),
  DTOutput("table"),
  leafletOutput("map")
)

server <- function(input, output,session) {
  
  # Update the 'taxa' input choices based on user typing
  filtered_data <- reactive({
    req(input$taxa) 
    # Filter observations by selected taxa
    data <- ala[ala$taxa == input$taxa, ]
    # Check if data is within selected place polygon
    place_polygon <- places[[input$place]]
    points <- st_as_sf(data, coords = c("long", "lat"), crs = 4326)
    dplyr::tibble(data[st_intersects(points, place_polygon, sparse = FALSE)[, 1], ]) %>%
      dplyr::select(taxa,species,year,voucher_type,long,lat,references) %>%
      dplyr::arrange(species,year) %>%
      dplyr::group_by(species,voucher_type) %>%
      dplyr::summarize(year=max(year),n=n(),long=long[1],lat=lat[1])
  })
  
  output$table <- renderDT({
    datatable(filtered_data())
  })
  
  output$map <- renderLeaflet({
    place_polygon <- places[[input$place]]
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = filtered_data(), ~long, ~lat,group = ~species, popup = ~species) %>%
      addPolygons(data = place_polygon, color = "red")
  })
}


shinyApp(ui = ui, server = server)
