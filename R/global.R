
# To suppress warning messages when summarizing data
# not sure if this is needed or not
# options(dplyr.summarise.inform = FALSE)



#' Add buffer around a given geometry (polygon or multipolygon)
#'
#' @param geom Geometry to which the buffer will be added.
#' @param buffer_size_meters Size of the buffer in meters.
#' @return sf object with the buffered geometry.
#' @noRd
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
  buffered_utm <- sf::st_buffer(geom_utm, dist = buffer_size_meters)
  
  # Transform back to original CRS (4326)
  buffered <-
    sf::st_transform(buffered_utm, "+proj=longlat +datum=WGS84 +no_defs")
  
  return(buffered)
}


#' Load spatial data from a given file path
#'
#' @param path Path to the spatial data file.
#' @return Geometry of the spatial data from the file.
#' @noRd
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

#' Create a circular polygon around a given lat-long coordinate with a specified radius
#'
#' @param lat Latitude of the center of the circle.
#' @param long Longitude of the center of the circle.
#' @param radius_m Radius of the circle in meters.
#' @return Circular polygon geometry.
#' @noRd
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


places <- list(
  "Wategora Reserve" = load_place(
    "inst/extdata/places/wategora-reserve-survey-area-approximate-boundaries.kml"
  ),
  "Fowlers Gap, UNSW" = load_place("inst/extdata/places/unsw-fowlers.kml"),
  "UNSW Smiths Lake and Vicinity" = load_place(
    "inst/extdata/places/unsw-smith-lake-field-station-and-vicinity.kml"
  ),
  "Australian Botanic Garden Mount Annan" = load_place(
    "inst/extdata/places/mt-annan-australian-botanic-garden.kml"
  ),
  "Grants Beach Walking Trail" = load_place("inst/extdata/places/grants-beach-walking-trail.kml"),
  "North Head Sanctuary" = load_place(
    "inst/extdata/places/north-head-sydney-harbour-federation-trust.kml"),
  "Lord Howe Island and Surroundings" = load_place(
    "inst/extdata/places/lord-howe-island-and-surrounding-islands.kml")
)


#' Filter data based on taxon of interest
#'
#' @param input User input for taxon of interest.
#' @param ala_data Dataset containing the taxon information.
#' @return Filtered dataset based on the taxon of interest.
#' @noRd
filter_by_taxon <- function(input, ala_data) {
  if (input$taxonOfInterest == "genus") {
    if (input$taxa_genus == "All") {
      return(ala_data())
    } else {
      return(ala_data()[Genus == input$taxa_genus, ])
    }
  } else if (input$taxonOfInterest == "family") {
    if (input$taxa_family == "All") {
      return(ala_data())
    } else {
      return(ala_data()[Family == input$taxa_family, ])
    }
  } else {
    return(ala_data())
  }
}

#' Determine which points are inside the target polygon
#'
#' @param points Set of points to check.
#' @param place_polygon Target polygon geometry.
#' @return Logical vector indicating which points are inside the target polygon.
#' @noRd
points_in_target <- function(points, place_polygon) {
  
  # Get the sparse list representation of intersections
  intersects_list <- sf::st_intersects(points, place_polygon)
  
  # Create a logical vector indicating whether each point intersects with any polygon
  result_vector <- sapply(intersects_list, function(x) length(x) > 0)
  
  return(result_vector)
}

#' Determine which points are inside the buffer
#'
#' @param points Set of points to check.
#' @param place_polygon Polygon geometry around which the buffer is added.
#' @param buffer_size Size of the buffer in meters.
#' @return Logical vector indicating which points are inside the buffer.
#' @noRd
points_in_buffer <- function(points, place_polygon, buffer_size) {
  buffer_place <- add_buffer(place_polygon, buffer_size)
  intersects_list <- sf::st_intersects(points, buffer_place)
  result_vector <- sapply(intersects_list, function(x) length(x) > 0)
  return(result_vector)
}

#' Check for updates and download if available
#'
#' @return Message indicating the status of the update check and download.
#' @noRd
check_and_download_update <- function() {
  infinity_file_path <-
    file.path(system.file(package = "infinitylists"), "data")
  
  current_version <- "0.0.0"
  if (file.exists(file.path(infinity_file_path, "infinitylistversion.txt")))
    current_version <-
    readLines(file.path(infinity_file_path, "infinitylistversion.txt"))
  
  # Fetch the latest release information using the GitHub API
  url <-
    paste0(
      "https://api.github.com/repos/",
      "traitecoevo",
      "/",
      "infinitylists",
      "/releases/latest"
    )
  
  # Simulate network down
  network_down <- as.logical(Sys.getenv("NETWORK_UP", unset = FALSE)) 
  
  if(httr::http_error(url)| network_down){
    message("No internet connection or data source down, try again later")
    return(NULL)
  } 
  response <- httr::GET(url)
  release_data <-
    jsonlite::fromJSON(httr::content(response, "text"))
  
  # Extract the latest version tag
  latest_version <- gsub("v", "", release_data$tag_name)
  
  # Compare the versions (Assumes semantic versioning)
  if (as.numeric(gsub("\\.", "", latest_version)) > as.numeric(gsub("\\.", "", current_version))) {
    if (!file.exists(infinity_file_path)) {
      dir.create(infinity_file_path, recursive = TRUE)
    }
    
    cat("New version found:", latest_version, "\n")
    
    # Download binary files
    for (i in 1:nrow(release_data$assets)) {
      asset_name <- release_data$assets[i, "name"]
      binary_url <- release_data$assets[i, "browser_download_url"]
      
      cat("Downloading:", asset_name, "\n")
      download.file(
        binary_url,
        destfile = file.path(infinity_file_path, asset_name),
        mode = "wb"
      )
    }
    writeLines(latest_version,
               file.path(infinity_file_path, "infinitylistversion.txt")) #creating a bug elsewhere
    cat("Update complete.\n")
  } else {
    cat(paste0(
      "You have the latest version (",
      latest_version,
      ") of the data.\n"
    ))
  }
}
