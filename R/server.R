#' Infinity List Server Function
#'
#' Initializes and sets up the server-side logic for the Infinity List Shiny application.
#' This function manages the user interactions, data processing, and rendering
#' of both UI elements and outputs (tables, maps, stats). It handles file uploads,
#' data filtering, and interaction with map elements.
#'
#' @param ... Additional arguments that might be passed to server functions.
#' These could be passed from a Shiny UI function or another server function.
#'
#' @details
#' The server function handles several main tasks:
#' - Observes and processes KML file uploads.
#' - Reactively creates polygons based on user inputs.
#' - Filters and processes ALA data based on user-selected polygons.
#' - Computes and displays statistics about observed species.
#' - Updates select input choices based on the available data.
#' - Renders and displays a data table with specific details.
#' - Provides a CSV download handler for the displayed data.
#' - Renders a Leaflet map with markers, polygons, and buffers.
#'
#' Key internal functions include:
#' - `load_place()`: For processing KML files.
#' - `create_circle_polygon()`: To create polygons from user inputs.
#' - `points_in_target()`: To determine if points lie within a target polygon.
#' - `points_in_buffer()`: To determine if points lie within a buffer around a polygon.
#' - `filter_by_taxon()`: Filters the ALA data by selected taxa.
#' - `add_buffer()`: Adds a buffer around a given polygon.
#'
#' @return
#' This function sets up and returns a server function for the Shiny app.
#' It does not have a direct return value, but rather, it sets up reactive outputs,
#' observers, and expressions that the Shiny app will utilize.
#'
#' @seealso 
#' \code{\link{infinitylistApp}}: Main function that launches the Infinity List Shiny app.
#'
#' @export
#'
infinity_server <- function(...){
  server <- function(input, output, session) {
    
    
    # Observer to handle uploaded KML files
    uploaded_place <- eventReactive(input$uploadKML,{
      load_place(input$uploadKML$datapath)
    })
    
    
    circle_polygon <- eventReactive(input$executeButton, {
      lat <- tryCatch(as.numeric(input$latitude), error = function(e) NA)
      long <- tryCatch(as.numeric(input$longitude), error = function(e) NA)
      radius_m <- tryCatch(as.numeric(input$radiusChoice), error = function(e) NA)
      
      if (is.na(lat) || is.na(long) || is.na(radius_m)) {
        return(NULL)
      }
      
      return(create_circle_polygon(lat, long, radius_m))
    })
    
    
    selected_polygon <- reactive({
      switch(input$inputType,
             "preloaded" = {
               return(places[[input$place]])
             },
             
             "choose" = {
               return(circle_polygon())
             },
             
             "upload" = {
                 return(uploaded_place())
             },
               return(NULL)
             
      )
    })
    
    
    ala_data <- reactive({
      long_buffer <- 0.578 #50 km approx
      lat_buffer <- 0.45 #50 km approx
      place_polygon <- selected_polygon()
      arrow::open_dataset(file.path(system.file(package = "infinitylists", "data",input$ala_path))) |>
        dplyr::filter(
          Lat < sf::st_bbox(place_polygon)$ymax + lat_buffer &
            Lat > sf::st_bbox(place_polygon)$ymin - lat_buffer &
            Long < sf::st_bbox(place_polygon)$xmax + long_buffer &
            Long > sf::st_bbox(place_polygon)$xmin - long_buffer
        ) |>
        dplyr::collect() |> data.table::data.table() 
    })
    
    intersect_data <- reactive({
      data <- filter_by_taxon(input, ala_data)
      
      place_polygon <- selected_polygon()
      
      if (is.null(place_polygon)) {
        return(data)
      }
      
      if (nrow(data) > 0) {
        points <- sf::st_as_sf(data, coords = c("Long", "Lat"), crs = 4326)
        
        in_target <- points_in_target(points, place_polygon)
        
        buffer_size <- as.numeric(input$buffer_size)
        in_buffer_all <- points_in_buffer(points, place_polygon, buffer_size)
        
        in_buffer_only <- in_buffer_all & !in_target
        
        data$`In target area` <- NA
        data$`In target area`[in_target] <- "in target"
        data$`In target area`[in_buffer_only] <- "only in buffer"
      } else {
        data$`In target area` <- NA
      }
      
      data <- dplyr::filter(data, !is.na(`In target area`))
      return(data)
    })
    
    
    
    stats_text <- reactive({
      data <- intersect_data()
      
      total_species <- length(unique(data$Species))
      total_genera <- length(unique(data$Genus))
      total_family <- length(unique(data$Family))
      
      native<-dplyr::filter(data, native_anywhere_in_aus=="native")
      if(nrow(native)>0) total_native_species <- length(unique(native$Species))
      else total_native_species<- "an unknown number"
        
      collections <- data[data$`Voucher Type` == "Collection",]
      collections_count <- nrow(collections)
      collections_species <- length(unique(collections$Species))
      
      photographic <- data[data$`Voucher Type` == "Photograph", ]
      photographic_count <- nrow(photographic)
      photographic_species <- length(unique(photographic$Species))
      
      
      paste0(
        "There have been ",
        total_species,
        " species observed ",
        "(",
        total_genera,
        " genera; ",
        total_family,
        " families) with ",
        collections_count,
        " collections of ",
        collections_species,
        " species and ",
        photographic_count,
        " photographic or audio records of ",
        photographic_species,
        " species. Of the ",
        total_species,
        " species observed, ",
        total_native_species,
        " are considered native to Australia."
      )
    })
    
    output$statsOutput <- renderText({
      stats_text()
    })
    
    
# Observe changes in intersect_data() and update choic
    observeEvent(list(input$place,input$buffer_size,
                      input$inputType,input$taxonOfInterest,
                      input$uploadKML,input$latitude,input$ala_path,input$executeButton),{
      
      updateSelectizeInput(
        session,
        "taxa_genus",
        selected = "All",
        choices = c("All", sort(unique(intersect_data()$Genus))),
        server = TRUE
      )
      
      updateSelectizeInput(
        session,
        "taxa_family",
        choices = c("All", sort(unique(intersect_data()$Family))),
        selected = "All",
        server = TRUE
      )
    })
    
    
    # Reactive expression to summarize and filter data
    filtered_data <- reactive({
      result <- intersect_data()
      if (nrow(result) == 0) {
        return(
          data.table::data.table(
            Species = character(0),
            `Voucher Type` = character(0),
            `Most recent obs.` = character(0),
            N = integer(0),
            Long = numeric(0),
            Lat = numeric(0),
            `Voucher location` = character(0),
            `Observed by` = character(0),
            Native = character(0)
          )
        )
      }
      
      # Sort the data by 'in target area' and 'Collection Date'
      result <-
        result[order(-as.integer(`In target area` == "in target"),
                     -`Collection Date`)]
      
      result <- result[, .(
        `In target area` = `In target area`[1],
        N = .N,
        `Most recent obs.` = {
          first_date <- first(`Collection Date`)
          if (is.na(first_date))
            as.character(NA)
          else
            first_date
        },
        Lat = Lat[1],
        Long = Long[1],
        `Repository` = ifelse(
          grepl("https", `Voucher Location`[1]),
          paste0(
            "<a href='",
            `Voucher Location`[1],
            "' target='_blank'>",
            "iNat",
            "</a>"
          ),
          paste0(
            "<a href='",
            "https://biocache.ala.org.au/occurrences/",`Record Id`[1],
            "' target='_blank'>",
            `Voucher Location`[1],
            "</a>"
          )),
        `Observed by` = `Recorded by`[1]
      ),
      by = .(Species, `Establishment means` = native_anywhere_in_aus,`Voucher type`=`Voucher Type`)]
      
      
      #removing rows from buffer that are in the target polygon.  
      target_species <- result[`In target area` == "in target", unique(Species)]
      result2 <- result[!(Species %in% target_species & `In target area` == "only in buffer")]
      
      return(result2)
    })
    
    # Render data table
    
    output$table <- DT::renderDT({
      data <- filtered_data()
      #preliminaries
      n_index <- which(names(data)=="N")-1 #not sure why this has to be off by 1
      default_page_length <- 25
      date_index <- which(names(data)=="Most recent obs.")
      data$N <- as.numeric(data$N)
      setorder(data,-N)  # sort by the "N" column in descending order
      
      #entres
      DT::datatable(
        data,
        rownames = FALSE,
        escape = FALSE,
        options = list(
          searching = TRUE,
          pageLength = default_page_length,
          order = list(list(n_index, 'desc')),
          # sort by the "N" column in descending order
          columnDefs = list(list(
            className = 'dt-left', targets = '_all'
          ))
        )
      ) |> DT::formatDate(date_index, method = "toLocaleDateString", 
                          params = list('en-AU', list(day = 'numeric', month = 'short', year = 'numeric'))
      )
    })
    
    # Handle CSV download
    output$downloadData <- downloadHandler(
      filename = function() {
        name_bits <- gsub(".parquet", "", input$ala_path)
        name_bits <- gsub("Australia-", "", name_bits)
        paste(input$place,
              "-",input$buffer_size,"m-buffer-",
              name_bits,
              ".csv",
              sep = "")
      },
      content = function(file) {
        data <- intersect_data()
        data$`Voucher Location` = ifelse(
          grepl("https", data$`Voucher Location`),
          data$`Voucher Location`
          ,
          paste0(
            "https://biocache.ala.org.au/occurrences/",data$`Record Id`
          ))
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # Render Leaflet map
    output$map <- leaflet::renderLeaflet({
      url <- "https://cloud.google.com/maps-platform/terms"
      link_text <- "Google Maps"
      place_polygon <- selected_polygon() # Use the reactive polygon
      buffer <- as.numeric(input$buffer_size)
      
      if (buffer == 0) {
        buffer_color <- rgb(1, 0, 0, alpha = 0)
      } else {
        buffer_color <- "darkorange"
      }
      
      leaflet::leaflet() |>
        leaflet::addTiles(
          urlTemplate = "https://mt1.google.com/vt/lyrs=y&x={x}&y={y}&z={z}",
          attribution = paste0('<a href="', url, '">', link_text, '</a>')
        ) |>
        leaflet::addMarkers(
          data = filtered_data(),
          lng = ~ filtered_data()$Long,
          lat = ~ filtered_data()$Lat,
          popup = ~ paste(filtered_data()$Species, filtered_data()$`Voucher type`),
          clusterOptions = leaflet::markerClusterOptions(maxClusterRadius = 20)
        ) |>
        leaflet::addPolygons(data = place_polygon, color = "red") |>
        leaflet::addPolygons(data = add_buffer(place_polygon, buffer), color = buffer_color)
    })
  }
  
}
