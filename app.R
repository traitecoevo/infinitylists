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

#
# this section:
# move to ala_processing.R
#
ala$native <- case_when(
  ala$native_anywhere_in_aus == "Native (APC)" ~ "Native",
  ala$native_anywhere_in_aus == "Introduced (APC)" ~ "Introduced",
  TRUE ~ "Unknown"
)
ala$`Voucher type` <- ala$voucher_type
ala$Species <- ala$species
#




load_place <- function(path) {
  tryCatch({
    geom <- st_read(path, crs = 4326, quiet = TRUE)
    return(st_geometry(geom))
  }, error = function(e) {
    showNotification(paste("Error loading KML:", e$message), type = "error")
    return(NULL)
  })
}

create_circle_polygon <- function(lat, long, radius_m) {
  # Create a point in a geographical coordinate system (WGS 84)
  pt <- st_point(c(long, lat))
  pt <- st_sfc(pt, crs = 4326)  # Assign WGS 84 CRS
  
  # Transform the point to Pseudo Mercator for buffering
  pt_transformed <- st_transform(pt, 3857)
  
  # Create a buffer around the transformed point
  circle <- st_buffer(pt_transformed, radius_m)
  
  # Transform back to WGS 84
  circle <- st_transform(circle, 4326)
  
  return(circle)
}


places <- list(
  "Wategora Reserve" = load_place(
    "places/wategora-reserve-survey-area-approximate-boundaries.kml"
  ),
  "Fowlers Gap, UNSW" = st_simplify(st_zm(
    load_place("places/fowlers.kml"),
    drop = TRUE,
    what = "ZM"
  ), dTolerance = 0.01),
  "UNSW Smiths Lake and Vicinity" = load_place("places/unsw-smith-lake-field-station-and-vicinity.kml")
)

# ----------------------
# UI
# ----------------------
ui <- 
  fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel("An Infinity of Lists: an Interactive Guide to the NSW Flora"),
    add_busy_spinner(spin = "fading-circle", color = "#0dc5c1"),
    
    radioButtons("inputType", "Input method:", 
                 choices = list("Preloaded Place" = "preloaded", 
                                "Upload KML" = "upload",
                                "Choose a place in NSW" = "choose"),
                 selected = "preloaded", inline = TRUE),
    
    conditionalPanel(
      condition = "input.inputType == 'preloaded'",
      selectizeInput(
        inputId = "place",
        label = "Choose a preloaded place:",
        choices = names(places),
        selected = "Fowlers Gap, UNSW"
      )
    ),
    
    conditionalPanel(
      condition = "input.inputType == 'upload'",
      fileInput(
        "uploadKML",
        "Upload your own KML (within NSW only)",
        accept = c(".kml")
      )),
    conditionalPanel(
      condition = "input.inputType == 'choose'",
      textInput("latitude", "Latitude", "-33.8688"),       # default: Sydney latitude
      textInput("longitude", "Longitude", "151.2093"),     # default: Sydney longitude
      sliderInput("radius_m", "Radius (m)", min = 10, max = 10000, value = 5000, step = 10)
    ),
    
    radioButtons("taxonOfInterest", "Taxon of interest:", 
                 choices = list("Genus" = "genus", 
                                "Family" = "family"),
                 selected = "genus", inline = TRUE),
    
    conditionalPanel(
      condition = "input.taxonOfInterest == 'genus'",
      selectizeInput(
        inputId = "taxa_genus",
        label = "Choose a genus: (you can also select All, but it's slow so be patient)",
        choices = "Eucalyptus",
        selected = "Eucalyptus",
        options = list(maxOptions = 300L)
      )
    ),
    
    conditionalPanel(
      condition = "input.taxonOfInterest == 'family'",
      selectizeInput(
        inputId = "taxa_family",
        label = "Choose a family: (you can also select All, but it's slow so be patient)",
        choices = "Myrtaceae",
        selected = "Myrtaceae",
        options = list(maxOptions = 300L)
      )
    ),
    
    downloadButton('downloadData', 'Download CSV'),
    tags$br(),
    textOutput("statsOutput"),
    tags$br(),
    DTOutput("table"),
    leafletOutput("map")
  )

    
  

# ----------------------
# Server
# ----------------------


server <- function(input, output, session) {
  # Function to update genus choices based on selected place
  update_genus_choices <- function(place) {
    place_polygon <- places[[place]]
    ss <-
      ala[lat < st_bbox(place_polygon)$ymax &
            lat > st_bbox(place_polygon)$ymin &
            long < st_bbox(place_polygon)$xmax &
            long > st_bbox(place_polygon)$xmin]
    choices = c("Eucalyptus", "All", sort(unique(ss$genus)))
    return(choices)
  }
  
  update_family_choices <- function(place) {
    place_polygon <- places[[place]]
    ss <-
      ala[lat < st_bbox(place_polygon)$ymax &
            lat > st_bbox(place_polygon)$ymin &
            long < st_bbox(place_polygon)$xmax &
            long > st_bbox(place_polygon)$xmin]
    choices = c("Myrtaceae", "All", sort(unique(ss$family)))
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
  
  
  selected_polygon <- reactive({
    if (input$inputType == "preloaded") {
      return(places[[input$place]])
    } else if (input$inputType == "choose") {
      lat <- as.numeric(input$latitude)
      long <- as.numeric(input$longitude)
      radius_m <- as.numeric(input$radius_m)
      return(create_circle_polygon(lat, long, radius_m))
    } else if (input$inputType == "upload" && !is.null(places[[input$place]])) {
      return(places[[input$place]])
    } else {
      return(NULL)
    }
  })
  
  intersect_data <- reactive({
    if (input$taxonOfInterest == "genus") {
      data <- ala[genus == input$taxa_genus,]
    } else if (input$taxonOfInterest == "family") {
      data <- ala[family == input$taxa_family,]
    } else {
      data <- ala
    }
    
    place_polygon <- selected_polygon() # Use the reactive polygon 
    
    if (is.null(place_polygon)) return(data.table())
    
    points <- st_as_sf(data, coords = c("long", "lat"), crs = 4326)
    point_polygon_intersection <-
      data[st_intersects(points, place_polygon, sparse = FALSE)[, 1],]
    point_polygon_intersection <-
      as.data.table(point_polygon_intersection)
    
    point_polygon_intersection[order(species, voucher_type, -as.integer(collectionDate))]
  })
  
  stats_text <- reactive({
    data <- intersect_data()
    
    total_species <- length(unique(data$Species))
    
    collections <- data[data$`Voucher type` == "Collection"]
    collections_count <- nrow(collections)
    collections_species <- length(unique(collections$species))
    
    photographic <- data[data$`Voucher type` == "Photograph",]
    photographic_count <- nrow(photographic)
    photographic_species <- length(unique(photographic$species))
    
    
    paste("There have been", total_species, "species observed with", collections_count, 
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
          "taxa_genus",
          selected = "Eucalyptus",
          choices = update_genus_choices(input$place),
          server = FALSE)
        updateSelectizeInput(
          session,
          "taxa_family",
          choices = update_family_choices(input$place),
          selected = "Myrtaceae",
          server = FALSE
        )
  })
   
  # Reactive expression to summarize and filter data
  filtered_data<- reactive({ 
    result<-intersect_data()
    if (nrow(result) == 0) {
      return(data.table(Species = character(0), `Voucher type` = character(0), `Most recent obs.` = character(0), N = integer(0), Long = numeric(0), Lat = numeric(0), `Voucher location` = character(0), `Observed by` = character(0), Native = character(0)))
    }
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
    place_polygon <- selected_polygon() # Use the reactive polygon 
    leaflet() %>%
      addTiles(
        urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",
        attribution = paste0('<a href="', url, '">', link_text, '</a>')
      ) %>%
      addMarkers(
        data = filtered_data(),
        ~ Long,
        ~ Lat,
        popup = paste(filtered_data()$Species, filtered_data()$`Voucher type`)
      ) %>%
      addPolygons(data = place_polygon, color = "red")
  })

}




shinyApp(ui = ui, server = server)