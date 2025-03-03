

# ----------------------
# UI
# ----------------------
# Define the user interface for the Shiny app
ui <- function(){

  files_in_directory <- list.files(path = system.file(package = "infinitylists", "data/"), pattern = ".parquet")
  
  taxa_names <- gsub("-[0-9]{4}-[0-9]{2}-[0-9]{2}.parquet",
         "\\1",
         files_in_directory)
  
  taxa_names <- gsub("Living-Atlas-", "", taxa_names)
  

  files_in_directory <- setNames(files_in_directory, taxa_names)
  
  # Custom Github hyperlink icon
  target <- bsplus::shiny_iconlink(name = "github")
  target$attribs$href <- "https://github.com/traitecoevo/infinitylists"
  
  fluidPage(
    theme = shinythemes::shinytheme("cosmo"),
    titlePanel(
      windowTitle = "Infinity Lists",
      div("An Infinity of Lists: an Interactive Tool for Building Place-based Species Lists",
          img(src = "www/infinitylist_hex.svg", width=150))),
  
    
    shinybusy::add_busy_spinner(spin = "fading-circle", color = "#0dc5c1"),
    
    
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "inputType",
          "Choose a place:",
          choices = list(
            "Preloaded Place" = "preloaded",
            "Upload KML" = "upload",
            "Use current location" = "current",
            "Choose a lat/long" = "choose"
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
          fileInput(
            "uploadKML",
            "Upload your own KML",
            accept = c(".kml")
          )),
        
        conditionalPanel(
          condition = "input.inputType == 'current'",
          
          # Get Geolocation
          tags$script('
  $(document).ready(function () {
  
                var options = {
                  enableHighAccuracy: true,
                  timeout: 5000,
                  maximumAge: 0
                };
  
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
    Shiny.onInputChange("geolocation", false);
    }
    
   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
          console.log(coords.latitude + ", " + coords.longitude, "," + coords.accuracy);
          Shiny.onInputChange("geolocation", true);
          Shiny.onInputChange("geolat", coords.latitude);
          Shiny.onInputChange("geolong", coords.longitude);
          Shiny.onInputChange("accuracy", coords.accuracy);
      }, 1100)
  }
  });
  

'),
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
            selected = 100
          ),
          
          actionButton("executeButton", "Go")
        ),
        
        
        conditionalPanel(
          condition = "input.inputType == 'choose'",
          numericInput(
            "latitude",
            "Latitude",
            value = -34.1182,
            min = -90,
            max = 90,
            step = 0.00001),
          numericInput(
            "longitude",
            "Longitude",
            value = 151.0636,
            min = -180,
            max = 180,
            step = 0.00001
          ),
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
            selected = 100
          ),
          actionButton("executeButton", "Go")
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
        selectInput("ala_path", "Choose a taxon:", choices = files_in_directory, selected = files_in_directory["Plantae"]),
        radioButtons(
          "taxonOfInterest",
          "Taxonomic level:",
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
            options = list(maxOptions = 600L)
          )
        ),
        
        conditionalPanel(
          condition = "input.taxonOfInterest == 'family'",
          selectizeInput(
            inputId = "taxa_family",
            label = "Choose a family:",
            choices = "All",
            selected = "All",
            options = list(maxOptions = 600L)
          )
        ),
        
        downloadButton('downloadData', 'Download all obs CSV'),
        width=3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Map",
                   leaflet::leafletOutput("map", height = 500),
                   tags$br(),
                   div(style = "font-weight: bold; font-size: 24px; margin-top: 20px; margin-bottom: 20px;", textOutput("statsOutput")),
                   tags$br(),
                   div(style = "margin-bottom: 50px;") 
          ),
          
          tabPanel("Species records",
                   DT::DTOutput("table")
          ),
          tabPanel("Coords",
                   
                   h5("Latitude:"), verbatimTextOutput("lat"),
                   h5("Longitude:"), verbatimTextOutput("long"),
                   h5("Accuracy (m):"), verbatimTextOutput("accuracy")
          ),
          tabPanel("About",
                   h2("Frequently Asked Questions"),
                   
                   h4("1. What does this Shiny app do?"),
                   p("This app allows users to explore biodiversity across Australia and a few other countries. After selecting a location and a taxonomic group and, the app will generate a species list."),
                   
                   h4("2. Which taxonomic groups can I generate lists for?"),
                   p("You can generate lists for marsupials, plants, dragonflies + damselflies (Odonata), butterflies (Papilionoidea) and cicadas. We have also included snakes and lizards (Squamata) from Spain and arachnids from France as extra demos."),
                   
                   h4("3. What is a KML file?"),
                   p("KML stands for Keyhole Markup Language. A KML file stores geographic data and features, and allows these features to be displayed on a map in geospatial software such as ", tags$a(href = "https://earth.google.com/", "Google Earth ")),
                   
                   h4("4. Why isn't the Use current location filter working?"),
                   p("Before selecting Use current location, it is important that users allow location access on their mobile device or desktop for the browser they intend to use, otherwise this filter will not work and you will be disconnected from the app."),
                   
                   h4("5. Where can I find a KML file for the location I'm interested in?"),
                   p("For some locations, KML files already exist and can be found by searching the internet for '[place name] + KML'. If you cannot find a KML file for your location, you can manually create one in software such as ", tags$a(href = "https://earth.google.com/", "Google Earth "),  "by drawing a polygon, and then exporting it as a KML."),
                   
                   h4("6. Can I search for any location in the world?"),
                   p("You can search anywhere in the world via the GBIF node. The Living Atlas countries (including Australia) might be faster via other nodes like the ALA."),
                   
                   h4("7. Can I search for locations outside of Australia?"),
                   p("Yes you can! The best way to adapt infinitylists for other countries is to install and run a local version of infinitylists, and then run our vignette (see the README at https://github.com/traitecoevo/infinitylists for more guidance."),
                   
                   h4("8. Where do the data come from?"),
                   p("All Australian data are extracted from the Atlas of Living Australia (ALA), Australia's", tags$a(href= "https://www.ala.org.au", "national biodiversity database."), "Data from the rest of the world are extracted from the ",
                   tags$a(href = "www.gbif.org", "Global Biodiversity Information Facility (GBIF).")),
                   
                   h4("9. Which data sources are included?"),
                   p("The app generates lists from two data sources, both of which are associated with some kind of voucher, i.e., records that are 'verifiable'. First, all records associated with a physical voucher stored in an institution (such as herbaria and museums) are included for the voucher type 'Collection'. Second, all records from the online citizen science platform iNaturalist that have qualified to enter the ALA or GBIF are included for the voucher types 'Photograph' and 'Recording'."),
                   
                   h4("10. What does the buffer do?"),
                   p("Selecting a buffer includes additional species that have not been recorded within the main target area, but have been seen in the immediate surrounding area up to the defined radius."),
                   
                   h4("11. Which taxonomic trees does the app use?"),
                   p("The species names presented in the app follow the taxonomic trees used by the ALA and GBIF. For Australian plants, names are taken from the ", tags$a(href = "https://biodiversity.org.au/nsl/services/search/taxonomy", "Australian Plant Census "), "and for Australian animals, names are taken from the ", tags$a("Australian Faunal Directory ", href = "https://biodiversity.org.au/afd/home"), "with minor exceptions for both"),
                   
                   h4("12. Are records only included in the app if they are identified to species?"),
                   p("Yes, any records that are identified to a taxonomic level coarser than species will not be retrieved by the app. Also, any records identified to an infraspecific level will only have the species identification displayed within the app."),
                   
                   h4("13. Are any other records excluded from the app?"),
                   p("Yes, the following types of record are excluded: "),
                   tags$ol(
                     tags$li("iNaturalist records with a copyright license of All Rights Reserved"),
                     tags$li("All records pre-dating 1923"),
                     tags$li("All records with a coordinate uncertainty value of > 1000 m"),
                     tags$li("Records considered to have spatial issues by ALA")
                   ),
                   p("More information on ALA spatial issues can be found ", tags$a("here", href = "https://support.ala.org.au/support/solutions/articles/6000240256-getting-started-with-the-data-quality-filters")),
                   
                   h4("14. Does the app reveal the location of species with sensitive locations?"),
                   p("Species with sensitive locations are not included in our app. Any species for which records have their locality data obscured or generalised (whether by the original data provider, or by the ALA itself) are excluded from the app."),
                   
                   h4("15. How often is the app updated?"),
                   p("Our major data files from the ALA are re-downloaded roughly once every few months. The current data returned by the app was downloaded on 22nd October 2024."),
                   
                   h4("16. How can I download data from the app?"),
                   p("You can download any given generated list by clicking on the 'Download all obs CSV' button. This will download a CSV file containing all records within the target area (and buffer, if selected), not just the most recent records that are presented in the table."),
                   
                   h4("17. What do the different symbols on the map represent?"),
                   p("The blue markers represent a single record. The coloured circles represent clusters of points; the number in the centre of each circle shows how many records are within that cluster. Zooming in on the map will resolve these clusters into their individual points. If a cluster remains even when at the maximum zoom level, clicking it will resolve it into its individual points."),
                   
                   h4("18. Why is the app called 'An Infinity of Lists'?"),
                   p("The app's name is a reference to the book ", tags$a(href = "https://en.wikipedia.org/wiki/The_Infinity_of_Lists", 'The Infinity of Lists'), " by Italian author Umberto Eco."),
          ),
        )
      )
    ),
    
    tags$footer(
      "Powered by ",
      tags$a(href = "https://www.unsw.edu.au/science", "UNSW Faculty of Science"), 
      align = "right", style = "padding: 30px",
      
      div("Created by Will Cornwell, Fonti Kar and Thomas Mesaglio",  
          target)
      ),  
     
  )
  

}
