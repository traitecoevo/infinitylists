# ----------------------
# Load Libraries
# ----------------------
library(shiny)
library(DT)
library(leaflet)
library(sf)
library(dplyr)
library(data.table)
library(lubridate)
library(arrow)
library(shinybusy)
library(shinythemes)
options(dplyr.summarise.inform = FALSE)

# ----------------------
# Data Preparation
# ----------------------

files_in_directory <- list.files(path = "data/")

#  ala <-read_parquet(paste0("data/", files_in_directory[1])) %>% data.table()
#           
# 
# # Initialize the variables
# if (nrow(ala) > 0) {
#   most_common_genus <- names(sort(table(ala$Genus), decreasing = TRUE)[1])
#   most_common_family <- names(sort(table(ala$Family), decreasing = TRUE)[1])
# } else {
#   most_common_genus <- NULL
#   most_common_family <- NULL
# }

min_lat <- -50
max_lat <- -10
min_long <- 110
max_long <- 163
# ala <- read_parquet("data/NSW-Fungi2023-08-23.parquet") |> data.table()


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
  "Fowlers Gap, UNSW" = load_place("places/fowlers2.kml"),
  "UNSW Smiths Lake and Vicinity" = load_place("places/unsw-smith-lake-field-station-and-vicinity.kml"),
  "Australian Botanic Garden Mount Annan" = load_place("places/mt-annan-australian-botanic-garden.kml"),
  "Grants Beach Walking Trail" = load_place("places/grants-beach-walking-trail.kml")
)

# ----------------------
# UI
# ----------------------
ui <- 
  fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel("An Infinity of Lists: an Interactive Guide to the NSW Biodiversity"),
    add_busy_spinner(spin = "fading-circle", color = "#0dc5c1"),
    selectInput("ala_path", "Choose a file:", choices = files_in_directory),
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
      numericInput("latitude", "Latitude", value = -33.8688, min = min_lat, max = -27),  # default: Sydney latitude
      numericInput("longitude", "Longitude", value = 151.2093, min = min_long, max = max_long),  # default: Sydney longitude
      verbatimTextOutput("warning"),
      selectInput(
        inputId = "radiusChoice",
        label = "Choose a radius:",
        choices = c("100m" = 100,
                    "500m" = 500,
                    "1km" = 1000,
                    "2km" = 2000,
                    "5km" = 5000,
                    "10km" = 10000,
                    "50km" = 50000),
        selected = 5000
      )
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
        selected = "All",
        options = list(maxOptions = 300L)
      )
    ),
    
    conditionalPanel(
      condition = "input.taxonOfInterest == 'family'",
      selectizeInput(
        inputId = "taxa_family",
        label = "Choose a family: (you can also select All, but it's slow so be patient)",
        choices = "My",
        selected = "All",
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
    most_common_genus <- names(sort(table(ala_data()$Genus), decreasing = TRUE)[1])
    ss <-
      ala_data()[Lat < st_bbox(place_polygon)$ymax &
            Lat > st_bbox(place_polygon)$ymin &
            Long < st_bbox(place_polygon)$xmax &
            Long > st_bbox(place_polygon)$xmin]
    choices = c(most_common_genus, "All", sort(unique(ss$Genus)))
    return(choices)
  }
  
  update_family_choices <- function(place) {
    place_polygon <- places[[place]]
    most_common_family <- names(sort(table(ala_data()$Family), decreasing = TRUE)[1])
    ss <-
      ala_data()[Lat < st_bbox(place_polygon)$ymax &
            Lat > st_bbox(place_polygon)$ymin &
            Long < st_bbox(place_polygon)$xmax &
            Long > st_bbox(place_polygon)$xmin]
    choices = c(most_common_family, "All", sort(unique(ss$Family)))
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
  
  observe({
    
    lat_out_of_range <- input$latitude < min_lat || input$latitude > max_lat
    lon_out_of_range <- input$longitude < min_long || input$longitude > 165
    
    lat_is_empty <- is.null(input$latitude) || is.na(input$latitude)
    lon_is_empty <- is.null(input$longitude) || is.na(input$longitude)
    
    if (lat_out_of_range || lon_out_of_range || lat_is_empty || lon_is_empty) {
      # Initialize a warning message
      warning_msg <- ""
      
      # Update the warning message based on which values are out of range
      if (lat_out_of_range || lat_is_empty) {
        warning_msg <- paste0(warning_msg, "Entered latitude is out of the allowed range. Please enter a value between -50 and -27.\n")
        # Reset the latitude value to the default
        #updateNumericInput(session, "latitude", value = -33.8688)
      }
      if (lon_out_of_range || lon_is_empty) {
        warning_msg <- paste0(warning_msg, "Entered longitude is out of the allowed range. Please enter a value between min_long and 165.")
        # Reset the longitude value to the default
        # updateNumericInput(session, "longitude", value = 151.2093)
      }
      
      # Display the warning to the user
      output$warning <- renderText(warning_msg)
    } else {
      # If the values are okay, don't display any warning
      output$warning <- renderText("")
    }
  })
  
  
  selected_polygon <- reactive({
    if (input$inputType == "preloaded") {
      return(places[[input$place]])
    } else if (input$inputType == "choose") {
      lat <- as.numeric(input$latitude)
      long <- as.numeric(input$longitude)
      radius_m <- as.numeric(input$radiusChoice)
      return(create_circle_polygon(lat, long, radius_m))
    } else if (input$inputType == "upload" && !is.null(places[[input$place]])) {
      return(places[[input$place]])
    } else {
      return(NULL)
    }
  })
  
  intersect_data <- reactive({
    if (input$taxonOfInterest == "genus") {
      if (input$taxa_genus == "All") {
        data <- ala_data()  # if "all" is selected, don't filter
      } else {
        data <- ala_data()[Genus == input$taxa_genus,]
      }
    } else if (input$taxonOfInterest == "family") {
      if (input$taxa_family == "All") {
        data <- ala_data()  # if "all" is selected, don't filter
      } else {
        data <- ala_data()[Family == input$taxa_family,]
      }
    } else {
      data <- ala_data()
    }
    
    
    place_polygon <- selected_polygon() # Use the reactive polygon 
    
    if (is.null(place_polygon)) return(data.table())
    
    points <- st_as_sf(data, coords = c("Long", "Lat"), crs = 4326)
    point_polygon_intersection <-
      data[st_intersects(points, place_polygon, sparse = FALSE)[, 1],]
    point_polygon_intersection <-
      as.data.table(point_polygon_intersection)
    
    point_polygon_intersection[order(Species, `Voucher Type`, -as.integer(`Collection Date`))]
  })
  
  stats_text <- reactive({
    data <- intersect_data()
    
    total_species <- length(unique(data$Species))
    
    collections <- data[data$`Voucher Type` == "Collection"]
    collections_count <- nrow(collections)
    collections_species <- length(unique(collections$Species))
    
    photographic <- data[data$`Voucher Type` == "Photograph",]
    photographic_count <- nrow(photographic)
    photographic_species <- length(unique(photographic$Species))
    
    
    paste("There have been", total_species, "species observed with", collections_count, 
          "collections of", collections_species, "species and", photographic_count, 
          "photographic records of", photographic_species, "species.")
  })
  
  output$statsOutput <- renderText({
    stats_text()
  })
  
  
  ala_data <- reactive({
    open_dataset(paste0("data/", input$ala_path)) |> filter(Lat<0) |> collect() |> data.table() #add filtering to bounding box plus buffer here
  })
  
  # A reactive to combine your two inputs
  combined_input <- reactive({
    list(place = input$place, ala_path = input$ala_path)
  })
  
  # Observe changes in the combined input
  observeEvent(combined_input(), {
    
    # Directly get the data from the ala_data reactive
    current_ala <- ala_data()
    
    most_common_genus <- names(sort(table(current_ala$Genus), decreasing = TRUE)[1])
    most_common_family <- names(sort(table(current_ala$Family), decreasing = TRUE)[1])
    
    updateSelectizeInput(
      session,
      "taxa_genus",
      selected = most_common_genus,
      choices = update_genus_choices(input$place),
      server = FALSE)
    
    updateSelectizeInput(
      session,
      "taxa_family",
      choices = update_family_choices(input$place),
      selected = most_common_family,
      server = FALSE
    )
  })

   
  # Reactive expression to summarize and filter data
  filtered_data<- reactive({ 
    result<-intersect_data()
    if (nrow(result) == 0) {
      return(data.table(Species = character(0), `Voucher Type` = character(0), `Most recent obs.` = character(0), N = integer(0), Long = numeric(0), Lat = numeric(0), `Voucher location` = character(0), `Observed by` = character(0), Native = character(0)))
    }
    result <- result[, .(
      N = .N,
      `Most recent obs.` = {
        first_date <- first(`Collection Date`)
        if (is.na(first_date))
          as.character(NA)
        else
          format(first_date, "%d-%b-%Y")
      
      },
      Long = Long[1],
      Lat = Lat[1],
      `Voucher Location` = ifelse(
        grepl("https", `Voucher Location`[1]),
        paste0(
          "<a href='",
          `Voucher Location`[1],
          "' target='_blank'>",
          "iNat",
          "</a>"
        ),
        `Voucher Location`[1]
      ),
      `Observed by` = `Recorded by`[1]
    ),
    by = .(Species, `Voucher Type`)]
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
      data <- filtered_data()
      data$`Voucher Location`<-gsub("<a href='","",data$`Voucher Location`)
      data$`Voucher Location`<-gsub("' target='_blank'>iNat</a>","",data$`Voucher Location`)
      write.csv(data, file, row.names = FALSE)
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
        popup = paste(filtered_data()$Species, filtered_data()$`Voucher Type`)
      ) %>%
      addPolygons(data = place_polygon, color = "red")
  })

}




shinyApp(ui = ui, server = server)