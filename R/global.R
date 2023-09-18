# ----------------------
# Data Preparation
# ----------------------


# ----------------------
# Load Libraries
# ----------------------

# Libraries for Shiny web applications

# Libraries for data manipulation and transformation
library(DT)            # For rendering data tables in Shiny
library(dplyr)         # For data manipulation
library(data.table)    # For efficient data manipulation
library(lubridate)     # For date-time manipulation
library(arrow)         # For reading/writing different file formats efficiently

# Libraries for spatial data handling and visualization
library(leaflet)       # For rendering interactive maps
library(leaflet.extras)

# To suppress warning messages when summarizing data
options(dplyr.summarise.inform = FALSE)

# Get the list of files in the data directory
files_in_directory <- list.files(path = "data/", pattern = ".parquet")
taxa_names <-
  gsub("Australia-(.+?)-[0-9]{4}-[0-9]{2}-[0-9]{2}.parquet",
       "\\1",
       files_in_directory)
files_in_directory <- setNames(files_in_directory, taxa_names)

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
  centroid <- sf::st_centroid(geom)
  utm_zone <- floor((sf::st_coordinates(centroid)[1] + 180) / 6) + 1
  crs_utm <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84")
  geom_utm <- sf::st_transform(geom, crs_utm)
  
  # Add buffer
  buffered_utm <- sf::buffer(geom_utm, dist = buffer_size_meters)
  
  # Transform back to original CRS (4326)
  buffered <-
    sf::st_transform(buffered_utm, "+proj=longlat +datum=WGS84 +no_defs")
  
  return(buffered)
}


# Function to load spatial data from a given file path
load_place <- function(path) {
  tryCatch({
    geom <- sf::st_read(path, crs = 4326, quiet = TRUE)
    geom <-
      sf::st_zm(geom, drop = TRUE, what = "ZM") # Drop the Z-dimension if present
    return(sf::st_geometry(geom))
  }, error = function(e) {
    cat("Error loading KML:", e$message, "\n") # Print the error message
    return(NULL)
  })
}

# Function to create a circular polygon around a given lat-long coordinate with a specified radius
create_circle_polygon <- function(lat, long, radius_m) {
  # Create a point in a geographical coordinate system (WGS 84)
  pt <- sf::st_point(c(long, lat))
  pt <- sf::st_sfc(pt, crs = 4326)  # Assign WGS 84 CRS
  
  # Transform the point to Pseudo Mercator for buffering
  pt_transformed <- sf::st_transform(pt, 3857)
  
  # Create a buffer around the transformed point
  circle <- sf::st_buffer(pt_transformed, radius_m)
  
  # Transform back to WGS 84
  circle <- sf::st_transform(circle, 4326)
  
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
  "Grants Beach Walking Trail" = load_place("places/grants-beach-walking-trail.kml"),
  "North Head - Sydney Harbour Federation Trust" = load_place("places/north-head-sydney-harbour-federation-trust.kml")
)


# Function to filter data based on taxon of interest
filter_by_taxon <- function(input, ala_data) {
  if (input$taxonOfInterest == "genus") {
    if (input$taxa_genus == "All") {
      return(ala_data())
    } else {
      return(ala_data()[Genus == input$taxa_genus,])
    }
  } else if (input$taxonOfInterest == "family") {
    if (input$taxa_family == "All") {
      return(ala_data())
    } else {
      return(ala_data()[Family == input$taxa_family,])
    }
  } else {
    return(ala_data())
  }
}

# Function to determine which points are inside the target polygon
points_in_target <- function(points, place_polygon) {
  sf::st_intersects(points, place_polygon, sparse = FALSE)[, 1]
}

# Function to determine which points are inside the buffer
points_in_buffer <- function(points, place_polygon, buffer_size) {
  buffer_place <- add_buffer(place_polygon, buffer_size)
  sf::st_intersects(points, buffer_place, sparse = FALSE)[, 1]
}

# Custom Github hyperlink icon
target <- bsplus::shiny_iconlink(name = "github")
target$attribs$href <- "https://github.com/traitecoevo/infinitylists"
