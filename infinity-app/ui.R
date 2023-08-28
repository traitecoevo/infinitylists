

# ----------------------
# Load Libraries
# ----------------------

# Libraries for Shiny web applications
library(shiny)
library(shinybusy)
library(shinythemes)

# Libraries for data manipulation and transformation
library(DT)            # For rendering data tables in Shiny
library(dplyr)         # For data manipulation
library(data.table)    # For efficient data manipulation
library(lubridate)     # For date-time manipulation
library(arrow)         # For reading/writing different file formats efficiently

# Libraries for spatial data handling and visualization
library(leaflet)       # For rendering interactive maps
library(sf)            # For spatial data operations
library(leaflet.extras)

# To suppress warning messages when summarizing data
options(dplyr.summarise.inform = FALSE)

# ----------------------
# Data Preparation
# ----------------------

# Get the list of files in the data directory
files_in_directory <- list.files(path = "data/")

# Define bounding box for Australia
min_lat <- -50
max_lat <- -10
min_long <- 110
max_long <- 163

# Function to add a buffer around a given geometry (polygon or multipolygon)
add_buffer <- function(geom, buffer_size_meters) {
  # Check if the input is an sfc_POLYGON, sfc_MULTIPOLYGON or their "sfg" equivalents
  if (!inherits(geom,
                c(
                  "sfc_POLYGON",
                  "sfg_POLYGON",
                  "sfc_MULTIPOLYGON",
                  "sfg_MULTIPOLYGON"
                ))) {
    stop(
      "Input must be of type sfc_POLYGON, sfg_POLYGON, sfc_MULTIPOLYGON, or sfg_MULTIPOLYGON."
    )
  }
  
  # Transform to UTM zone based on centroid longitude
  centroid <- st_centroid(geom)
  utm_zone <- floor((st_coordinates(centroid)[1] + 180) / 6) + 1
  crs_utm <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84")
  geom_utm <- st_transform(geom, crs_utm)
  
  # Add buffer
  buffered_utm <- st_buffer(geom_utm, dist = buffer_size_meters)
  
  # Transform back to original CRS (4326)
  buffered <-
    st_transform(buffered_utm, "+proj=longlat +datum=WGS84 +no_defs")
  
  return(buffered)
}


# Function to load spatial data from a given file path
load_place <- function(path) {
  tryCatch({
    geom <- st_read(path, crs = 4326, quiet = TRUE)
    return(st_geometry(geom))
  }, error = function(e) {
    showNotification(paste("Error loading KML:", e$message), type = "error")
    return(NULL)
  })
}

# Function to create a circular polygon around a given lat-long coordinate with a specified radius
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


# Load predefined spatial boundaries for various places
places <- list(
  "Wategora Reserve" = load_place(
    "places/wategora-reserve-survey-area-approximate-boundaries.kml"
  ),
  "Fowlers Gap, UNSW" = load_place("places/unsw-fowlers.kml"),
  "UNSW Smiths Lake and Vicinity" = load_place("places/unsw-smith-lake-field-station-and-vicinity.kml"),
  "Australian Botanic Garden Mount Annan" = load_place("places/mt-annan-australian-botanic-garden.kml"),
  "Grants Beach Walking Trail" = load_place("places/grants-beach-walking-trail.kml")
)
# ----------------------
# UI
# ----------------------
# Define the user interface for the Shiny app
ui <-
  fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel(
      "An Infinity of Lists: an Interactive Guide to the Australian Biodiversity"
    ),
    add_busy_spinner(spin = "fading-circle", color = "#0dc5c1"),
    selectInput("ala_path", "Choose a file:", choices = files_in_directory),
    radioButtons(
      "inputType",
      "Input method:",
      choices = list(
        "Preloaded Place" = "preloaded",
        "Upload KML" = "upload",
        "Choose a place in Australia" = "choose"
      ),
      selected = "preloaded",
      inline = TRUE
    ),
    
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
      fileInput("uploadKML",
                "Upload your own KML",
                accept = c(".kml"))
    ),
    conditionalPanel(
      condition = "input.inputType == 'choose'",
      numericInput(
        "latitude",
        "Latitude",
        value = -33.8688,
        min = min_lat,
        max = max_long
      ),
      # default: Sydney latitude
      numericInput(
        "longitude",
        "Longitude",
        value = 151.2093,
        min = min_long,
        max = max_long
      ),
      # default: Sydney longitude
      verbatimTextOutput("warning"),
      selectInput(
        inputId = "radiusChoice",
        label = "Choose a radius:",
        choices = c(
          "100m" = 100,
          "500m" = 500,
          "1km" = 1000,
          "2km" = 2000,
          "5km" = 5000,
          "10km" = 10000,
          "50km" = 50000
        ),
        selected = 5000
      )
    ),
    
    radioButtons(
      "taxonOfInterest",
      "Taxon of interest:",
      choices = list("Genus" = "genus",
                     "Family" = "family"),
      selected = "genus",
      inline = TRUE
    ),
    
    conditionalPanel(
      condition = "input.taxonOfInterest == 'genus'",
      selectizeInput(
        inputId = "taxa_genus",
        label = "Choose a genus: ",
        choices = "All",
        selected = "All",
        options = list(maxOptions = 300L)
      )
    ),
    
    conditionalPanel(
      condition = "input.taxonOfInterest == 'family'",
      selectizeInput(
        inputId = "taxa_family",
        label = "Choose a family:",
        choices = "All",
        selected = "All",
        options = list(maxOptions = 300L)
      )
    ),
    selectInput(
      inputId = "buffer_size",
      label = "Choose a buffer:",
      choices = c(
        "0m (ie. no buffer)" = 0,
        "100m" = 100,
        "500m" = 500,
        "1km" = 1000,
        "2km" = 2000,
        "5km" = 5000,
        "10km" = 10000,
        "50km" = 50000
      ),
      selected = 0
    ),
    downloadButton('downloadData', 'Download all obs CSV'),
    tags$br(),
    textOutput("statsOutput"),
    tags$br(),
    DTOutput("table"),
    leafletOutput("map", height = 500)
  )
