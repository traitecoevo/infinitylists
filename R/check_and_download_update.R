check_and_download_update <- function() {
  
  current_version="0.0.0"
  if (file.exists("infinity-app/data/infinitylistversion.txt")) current_version<- readLines("infinity-app/data/infinitylistversion.txt")

    # Fetch the latest release information using the GitHub API
  url <- paste0("https://api.github.com/repos/", "traitecoevo", "/", "infinitylists", "/releases/latest")
  response <- httr::GET(url)
  release_data <- jsonlite::fromJSON(httr::content(response, "text"))
  
  # Extract the latest version tag
  latest_version <- gsub("v", "", release_data$tag_name)
  
  # Compare the versions (Assumes semantic versioning)
  if (as.numeric(gsub("\\.", "", latest_version)) > as.numeric(gsub("\\.", "", current_version))) {
    cat("New version found:", latest_version, "\n")
    
    # Download binary files
    for (i in 1:nrow(release_data$assets)) {
      asset_name <- release_data$assets[i, "name"]
      binary_url <- release_data$assets[i, "browser_download_url"]
      
      cat("Downloading:", asset_name, "\n")
      download.file(binary_url, destfile = paste0("infinity-app/data/",asset_name), mode = "wb")
    }
    writeLines(latest_version, "infinity-app/data/infinitylistversion.txt")
    cat("Update complete.\n")
  } else {
    cat("You have the latest version.\n")
  }
}

# Example usage
check_and_download_update()
