library(shiny)
library(DT)
library(leaflet)
library(sf)
library(dplyr)
library(data.table)
library(shinybusy)
library(shinythemes)
options(dplyr.summarise.inform = FALSE)

ala <- data.table::fread("ala_nsw_inat_avh.csv")
ala$native <-
  case_when(
    ala$native_anywhere_in_aus == "Native (APC)" ~ "Native",
    ala$native_anywhere_in_aus == "Introduced (APC)" ~ "Introduced",
    TRUE ~ "Unknown"
  ) #move to ala.processing.R

#  list of places with their polygons (kmls need to be valid polygons)
#  if this becomes 1000s we'll need to re-write this section to load on demand 
#  since this will slow down app loading
places <- list(
  "Wategora Reserve" = st_geometry(
    st_read(
      "places/wategora-reserve-survey-area-approximate-boundaries.kml",
      crs = 4326,
      quiet = TRUE
    )
  ),
  "Fowlers Gap UNSW" = st_simplify(st_zm(st_geometry(
    st_read("places/fowlers.kml", crs = 4326,
            quiet = TRUE), drop = TRUE, what = "ZM")
  ), dTolerance = 0.01),
  "Smiths Lake and Vicinity" = st_geometry(
    st_read(
      "places/unsw-smith-lake-field-station-and-vicinity.kml",
      crs = 4326,
      quiet = TRUE
    )
  )
)


ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("An Infinity of Lists: an Interactive Guide to the NSW Flora"),
  add_busy_spinner(spin = "fading-circle", color = "#0dc5c1"),
  selectizeInput(
    inputId = "place",
    label = "Choose a place:",
    choices =  names(places),
    selected = "Fowlers Gap UNSW"
  ),
  selectizeInput(
    inputId = "genus",
    label = "Choose a genus: (you can also select All, but it's slow so be patient)",
    choices = "Eucalyptus",
    selected = "Eucalyptus",
    #  only genera work at the moment
    options = list(maxOptions = 300L)
  ),
  DTOutput("table"),
  downloadButton('downloadData', 'Download CSV'),
  leafletOutput("map")
)


server <- function(input, output, session) {
  #this updates the taxa input to have only genera observed within the bounding box of the place selected.
  #it is called by the map which only executes if the place changes.
  observeEvent(input$place, {
    place_polygon <- places[[input$place]]
    ss <- ala[lat < st_bbox(place_polygon)$ymax & lat > st_bbox(place_polygon)$ymin & 
                long < st_bbox(place_polygon)$xmax & long > st_bbox(place_polygon)$xmin] #trying out data.table for speed
    updateSelectizeInput(
      session,
      "genus",
      selected = "Eucalyptus",
      choices = c("Eucalyptus", "All", sort(unique(ss$genus))),
      server = FALSE
    )
  })
  
  filtered_data <- reactive({
    # Filter observations by selected genus
    if (input$genus != "All") {
      data <- ala[ala$genus == input$genus, ]
    }
    else{
      data <- ala
    }
    place_polygon <- places[[input$place]]
    points <- st_as_sf(data, coords = c("long", "lat"), crs = 4326)
    # Check if data is within selected place polygon
    point_polygon_intersection <- data[st_intersects(points, place_polygon, sparse = FALSE)[, 1], ] 
    point_polygon_intersection <- as.data.table(point_polygon_intersection)
    
    result <- point_polygon_intersection[order(species, voucher_type, -as.integer(collectionDate))]
    result <- result[
      , .(
        `Most recent obs.` = {
          first_date <- first(collectionDate)
          if (is.na(first_date)) as.character(NA) else format(as.Date(first_date), "%e-%b-%Y")
        },
        n = .N,
        long = long[1],
        lat = lat[1],
        `Voucher location` = ifelse(
          grepl("https", voucher_location[1]), 
          paste0("<a href='", voucher_location[1], "'>", "iNat", "</a>"), 
          voucher_location[1]
        ),
        `observed by` = recordedBy[1],
        native = native[1]
      ), 
      by = .(species, voucher_type)
    ][, `Voucher type` := voucher_type]
  })
  
  
  
  
  
  
  output$table <- renderDT({
    datatable(
      filtered_data(),
      escape = FALSE,
      options = list(searching = TRUE, pageLength = 5)
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$map <- renderLeaflet({
    url <- "https://cloud.google.com/maps-platform/terms"
    link_text <- "Google Maps"
    place_polygon <- places[[input$place]]
    leaflet() %>%
      addTiles(
        urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",
        attribution = paste0('<a href="', url, '">', link_text, '</a>')
      ) %>%
      addMarkers(
        data = filtered_data(),
        ~ long,
        ~ lat,
        popup = paste(filtered_data()$species, filtered_data()$`Voucher type`)
      ) %>%
      addPolygons(data = place_polygon, color = "red")
  })
}


shinyApp(ui = ui, server = server)