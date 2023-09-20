#' Create server for infinity lists
#'
#' @param ... 
infinity_server <- function(...){
  server <- function(input, output, session) {
    # Function to update genus choices based on selected place
    update_genus_choices <- function(place) {
      place_polygon <- selected_polygon()
      choices = c("All", sort(unique(intersect_data()$Genus)))
      return(choices)
    }
    
    update_family_choices <- function(place) {
      place_polygon <- selected_polygon()
      choices = c("All", sort(unique(intersect_data()$Family)))
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
               if (!is.null(places[[input$place]])) {
                 return(places[[input$place]])
               } else {
                 return(NULL)
               }
             },
             
             {
               return(NULL)
             }
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
      
      collections <- data[data$`Voucher Type` == "Collection",]
      collections_count <- nrow(collections)
      collections_species <- length(unique(collections$Species))
      
      photographic <- data[data$`Voucher Type` == "Photograph", ]
      photographic_count <- nrow(photographic)
      photographic_species <- length(unique(photographic$Species))
      
      
      paste(
        "There have been",
        total_species,
        "species observed with",
        collections_count,
        "collections of",
        collections_species,
        "species and",
        photographic_count,
        "photographic or audio records of",
        photographic_species,
        "species."
      )
    })
    
    output$statsOutput <- renderText({
      stats_text()
    })
    
    
    # A reactive to combine your two inputs
    combined_input <- reactive({
      list(place = input$place, ala_path = input$ala_path, type = input$inputType,execute = input$executeButton, buffer= input$buffer_size)
    })
    
    # Observe changes in the combined input
    observeEvent(combined_input(), {
      # Directly get the data from the ala_data reactive
      current_ala <- ala_data()
      
      updateSelectizeInput(
        session,
        "taxa_genus",
        selected = "All",
        choices = update_genus_choices(input$place),
        server = FALSE
      )
      
      updateSelectizeInput(
        session,
        "taxa_family",
        choices = update_family_choices(input$place),
        selected = "All",
        server = FALSE
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
          popup = ~ paste(filtered_data()$Species, filtered_data()$`Voucher Type`),
          clusterOptions = leaflet::markerClusterOptions(maxClusterRadius = 20)
        ) |>
        leaflet::addPolygons(data = place_polygon, color = "red") |>
        leaflet::addPolygons(data = add_buffer(place_polygon, buffer), color = buffer_color)
    })
  }
  
}
