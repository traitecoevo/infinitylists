# ----------------------
# Load Libraries
# ----------------------
library(shiny)
library(DT)
library(leaflet)
library(sf)
library(dplyr)
library(data.table)
library(shinybusy)
library(shinythemes)
options(dplyr.summarise.inform = FALSE)

# ----------------------
# Data Preparation
# ----------------------
ala <- data.table::fread("ala_nsw_inat_avh.csv")
ala$native <- case_when(
  ala$native_anywhere_in_aus == "Native (APC)" ~ "Native",
  ala$native_anywhere_in_aus == "Introduced (APC)" ~ "Introduced",
  TRUE ~ "Unknown"
)
ala$`Voucher type` <- ala$voucher_type
ala$Species <- ala$species

load_place <- function(path) {
  tryCatch({
    geom <- st_read(path, crs = 4326, quiet = TRUE)
    return(st_geometry(geom))
  }, error = function(e) {
    showNotification(paste("Error loading KML:", e$message), type = "error")
    return(NULL)
  })
}

places <- list(
  "Wategora Reserve" = load_place(
    "places/wategora-reserve-survey-area-approximate-boundaries.kml"
  ),
  "Fowlers Gap UNSW" = st_simplify(st_zm(
    load_place("places/fowlers.kml"),
    drop = TRUE,
    what = "ZM"
  ), dTolerance = 0.01),
  "Smiths Lake and Vicinity" = load_place("places/unsw-smith-lake-field-station-and-vicinity.kml")
)

# ----------------------
# UI
# ----------------------
ui <-
  fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel("An Infinity of Lists: an Interactive Guide to the NSW Flora"),
    add_busy_spinner(spin = "fading-circle", color = "#0dc5c1"),
    selectizeInput(
      inputId = "place",
      label = "Choose a preloaded place:",
      choices = names(places),
      selected = "Fowlers Gap UNSW"
    ),
    fileInput(
      "uploadKML",
      "Or upload your own KML (within NSW only)",
      accept = c(".kml")
    ),
    selectizeInput(
      inputId = "genus",
      label = "Choose a genus: (you can also select All, but it's slow so be patient)",
      choices = "Eucalyptus",
      selected = "Eucalyptus",
      options = list(maxOptions = 300L)
    ),
    
    textOutput("statsOutput"),
    tags$br(),
    DTOutput("table"),
    downloadButton('downloadData', 'Download CSV'),
    leafletOutput("map")
  )

# ----------------------
# Server
# ----------------------


server <- function(input, output, session) {
  # Function to update genus choices based on selected place
  update_genus_choices <- function(place) {
    place_polygon <- places[[place]]
    
    if (is.null(place_polygon)) {
      showNotification("Selected place data is not available.", type = "error")
      return(NULL)
    }
    
    ss <-
      ala[lat < st_bbox(place_polygon)$ymax &
            lat > st_bbox(place_polygon)$ymin &
            long < st_bbox(place_polygon)$xmax &
            long > st_bbox(place_polygon)$xmin]
    choices = c("Eucalyptus", "All", sort(unique(ss$genus)))
    return(choices)
  }
  
  # Observer to handle uploaded KML files
  observe({
    inFile <- input$uploadKML
    if (is.null(inFile))
      return(NULL)
    
    uploaded_place <- tryCatch({
      load_place(inFile$datapath)
    }, error = function(e) {
      showNotification(paste("Error processing KML:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(uploaded_place)) {
      places[[inFile$name]] <<- uploaded_place
      updateSelectizeInput(
        session,
        "place",
        choices = names(places),
        selected = inFile$name,
        server = FALSE
      )
    }
  })
  
  
  stats_text <- reactive({
    data <- intersect_data()
    
    total_species <- length(unique(data$Species))
    
    collections <- data[data$`Voucher type` == "Collection"]
    collections_count<-nrow(collections)
    collections_species <- length(unique(collections$species))
    
    photographic <- data[data$`Voucher type` == "Photograph",]
    photographic_count<-nrow(photographic)
    photographic_species <- length(unique(photographic$species))
    
    
    paste("There have been", total_species, "species in this genus observed within this polygon, with", collections_count, 
          "collections of", collections_species, "species and", photographic_count, 
          "photographic records of", photographic_species, "species.")
  })
  
  output$statsOutput <- renderText({
    stats_text()
  })
  
  # Observer to update genus input based on the selected place
  observeEvent(input$place, {
    updateSelectizeInput(
      session,
      "genus",
      selected = "Eucalyptus",
      choices = update_genus_choices(input$place),
      server = FALSE
    )
  })
  
  # Reactive expression to get filtered data
  intersect_data <- reactive({
    data <-
      if (input$genus != "All")
        ala[ala$genus == input$genus,]
    else
      ala
    place_polygon <- places[[input$place]]
    points <- st_as_sf(data, coords = c("long", "lat"), crs = 4326)
    point_polygon_intersection <-
      data[st_intersects(points, place_polygon, sparse = FALSE)[, 1],]
    point_polygon_intersection <-
      as.data.table(point_polygon_intersection)
    
    point_polygon_intersection[order(species, voucher_type, -as.integer(collectionDate))]
  })
   
  filtered_data<- reactive({ 
    result<-intersect_data()
    result <- result[, .(
      N = .N,
      `Most recent obs.` = {
        first_date <- first(collectionDate)
        if (is.na(first_date))
          as.character(NA)
        else
          format(as.Date(first_date), "%e-%b-%Y")
      },
      Long = long[1],
      Lat = lat[1],
      `Voucher location` = ifelse(
        grepl("https", voucher_location[1]),
        paste0(
          "<a href='",
          voucher_location[1],
          "' target='_blank'>",
          "iNat",
          "</a>"
        ),
        voucher_location[1]
      ),
      `Observed by` = recordedBy[1],
      `Native?` = native[1]
    ),
    by = .(Species, `Voucher type`)]
  })
  
  # Render data table
  output$table <- renderDT({
    datatable(
      filtered_data(),
      escape = FALSE,
      options = list(
        searching = TRUE,
        pageLength = 25,
        columnDefs = list(list(
          className = 'dt-left', targets = '_all'
        ))
      )
    )
  })
  
  # Handle CSV download
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Render Leaflet map
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
        ~ Long,
        ~ Lat,
        popup = paste(filtered_data()$species, filtered_data()$`Voucher type`)
      ) %>%
      addPolygons(data = place_polygon, color = "red")
  })
}



shinyApp(ui = ui, server = server)