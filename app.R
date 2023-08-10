library(shiny)
library(DT)
library(leaflet)
library(sf)
library(dplyr)
library(data.table)

ala<-data.table::fread("ala_nsw_inat_avh.csv")

#  list of places with their polygons (kmls need to be valid polygons)
places <- list(
 "Duck River" = st_geometry(st_read("places/wategora-reserve-survey-area-approximate-boundaries.kml", crs = 4326)),
 "Fowlers Gap UNSW" = st_simplify(st_geometry(st_read("places/fowlers.kml", crs = 4326)), dTolerance = 0.01),
 "Smiths Lake and Vicinity" = st_geometry(st_read("places/unsw-smith-lake-field-station-and-vicinity.kml", crs = 4326))
 )

ui <- fluidPage(
  selectizeInput(inputId="place", label ="Choose a place:", choices =  names(places),selected = "Fowlers Gap UNSW"),
  selectizeInput(inputId="taxa", label ="Choose a taxa:", choices = c("Acacia","Eucalyptus","Sida"),selected = "Sida", #  only genera work at the moment
                 options = list(
                   placeholder = "e.g Acacia",
                   create = TRUE,
                   maxOptions = 500L
                 )),
  DTOutput("table"),
  leafletOutput("map")
)


server <- function(input, output,session) {
  
  
  filtered_data <- reactive({
    # Filter observations by selected taxa
    data <- ala[ala$taxa == input$taxa, ]
    place_polygon <- places[[input$place]]
    points <- st_as_sf(data, coords = c("long", "lat"), crs = 4326)
    # Check if data is within selected place polygon
    dplyr::tibble(data[st_intersects(points, place_polygon, sparse = FALSE)[, 1], ]) %>%
      dplyr::select(taxa,species,year,voucher_type,long,lat,voucher_location) %>%
      dplyr::arrange(species,year) %>%
      dplyr::group_by(species,voucher_type) %>%
      dplyr::summarize(`Most Recent Obs.`=max(year,na.rm=TRUE),n=n(),
                       long=long[1],lat=lat[1],
                       voucher_location=
                         case_when(grepl("https",voucher_location[1]) ~ paste0("<a href='",voucher_location[1],"'>","iNat","</a>"),
                                         TRUE ~ voucher_location[1]))
  })
  

#this updates the taxa input to have only genera observed within the bounding box of the place selected.  
#it is called by the map which only executes if the place changes.  
filter_inputs<-reactive({  
  place_polygon <- places[[input$place]]
  ss<-dplyr::filter(ala,lat<st_bbox(place_polygon)$ymax&lat>st_bbox(place_polygon)$ymin & long<st_bbox(place_polygon)$xmax& long>st_bbox(place_polygon)$xmin)
  shiny::observe({
    updateSelectizeInput(session,
                      "taxa",
                      choices = sort(unique(ss$taxa)),
                                     server = TRUE)
  })
})
  
  
  
  output$table <- renderDT({
    datatable(filtered_data(),escape = FALSE)
  })
  
  output$map <- renderLeaflet({
    species_colors <- colorFactor(palette = "Set2", domain = filtered_data()$voucher_type)
    url <- "https://cloud.google.com/maps-platform/terms"
    link_text <- "Google Maps"
    place_polygon <- places[[input$place]]
    filter_inputs()
    leaflet() %>%
      addTiles(urlTemplate ="https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",attribution=paste0('<a href="', url, '">', link_text, '</a>')) %>%
      addMarkers(data = filtered_data(), ~long, ~lat,
                      popup = paste(filtered_data()$species,filtered_data()$voucher_type)
                    ) %>%
      addPolygons(data = place_polygon, color = "red") 
  })
}


shinyApp(ui = ui, server = server)
