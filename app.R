library(shiny)
library(DT)
library(leaflet)
library(sf)
library(dplyr)
library(data.table)
library(shinybusy)
library(shinythemes)



ala <- data.table::fread("ala_nsw_inat_avh.csv")
ala$native <-
  case_when(
    ala$native_anywhere_in_aus == "Native (APC)" ~ "Native",
    ala$native_anywhere_in_aus == "Introduced (APC)" ~ "Introduced",
    TRUE ~ "unknown"
  )

#  list of places with their polygons (kmls need to be valid polygons)
places <- list(
  "Duck River" = st_geometry(
    st_read(
      "places/wategora-reserve-survey-area-approximate-boundaries.kml",
      crs = 4326,
      quiet = TRUE
    )
  ),
  "Fowlers Gap UNSW" = st_simplify(st_geometry(
    st_read("places/fowlers.kml", crs = 4326,
            quiet = TRUE)
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
  titlePanel("The Infinity of Lists: an Interactive Guide to the NSW Flora"),
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
    options = list(maxOptions = 200L)
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
    ss <-
      dplyr::filter(
        ala,
        lat < st_bbox(place_polygon)$ymax &
          lat > st_bbox(place_polygon)$ymin &
          long < st_bbox(place_polygon)$xmax &
          long > st_bbox(place_polygon)$xmin
      )
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
    data[st_intersects(points, place_polygon, sparse = FALSE)[, 1], ] %>%
      dplyr::tibble() %>%
      dplyr::group_by(species, voucher_type) %>%
      dplyr::summarize(
        `Most recent obs.` = max(collectionDate, na.rm = TRUE),
        n = n(),
        long = long[1],
        lat = lat[1],
        `Voucher location` =
          case_when(
            grepl("https", voucher_location[1]) ~ paste0("<a href='", voucher_location[1], "'>", "iNat", "</a>"),
            TRUE ~ voucher_location[1]
          ),
        `observed by` = recordedBy[1],
        native = native[1],
      ) %>%
      rename(`Voucher type` = voucher_type)
  })
  
  
  
  
  
  
  output$table <- renderDT({
    datatable(
      filtered_data(),
      escape = FALSE,
      options = list(searching = TRUE, pageLength = 15)
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