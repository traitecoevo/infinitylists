

# ----------------------
# UI
# ----------------------
# Define the user interface for the Shiny app
ui <-
  fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel(
      windowTitle = "Infinity List",
      div("An Infinity of Lists: an Interactive Guide to the Australian Biodiversity",
                  img(src = "infinitylist_hex.svg", width=150))
      
    ),
    
    add_busy_spinner(spin = "fading-circle", color = "#0dc5c1"),
   
     tabsetPanel(
      # Existing content wrapped in a tabPanel
      tabPanel("Main", 
    selectInput("ala_path", "Choose a file:", choices = files_in_directory),
    radioButtons(
      "inputType",
      "Input method:",
      choices = list(
        "Preloaded Place" = "preloaded",
        "Upload KML" = "upload",
        "Choose a lat/long in Australia" = "choose"
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
        "Upload your own KML (within NSW only)",
        accept = c(".kml")
      )),
    conditionalPanel(
      condition = "input.inputType == 'choose'",
      numericInput(
        "latitude",
        "Latitude",
        value = -33.8688,
        min = -90,
        max = 90,
        step = 0.00001),
      numericInput(
        "longitude",
        "Longitude",
        value = 148.2093,
        min = -180,
        max = 180,
        step = 0.00001
      ),
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
      ),
      actionButton("executeButton", "Go")
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
    div(style = "font-weight: bold; font-size: 24px; margin-top: 20px; margin-bottom: 20px;", textOutput("statsOutput")),
    tags$br(),
    DTOutput("table"),
    leafletOutput("map", height = 500),
    div(style = "margin-bottom: 50px;") 
      ),
    
    # New FAQ tabPanel
    tabPanel("FAQs",
             h2("Frequently Asked Questions"),
             
             h4("1. What does this app do?"),
             p("This app allows users to explore biodiversity across Australia. After selecting a taxonomic group and a location, the app will generate a species list."),
             
             h4("2. Which taxonomic groups can I generate lists for?"),
             p("You can generate lists for marsupials, plants, dragonflies + damselflies (Odonata), butterflies (Papilionoidea) or cicadas."),
             
             h4("4. What is a KML file?"),
             p("KML stands for Keyhole Markup Language. A KML file stores geographic data and features, and allows these features to be displayed on a map in geospatial software such as Google Earth."),
             
             h4("5. Where can I find a KML file for the location I'm interested in?"),
             p("For some locations, KML files already exist and can be found by searching the internet for '[place name] + KML'. If you cannot find a KML file for your location, you can manually create one in software such as Google Earth by drawing a polygon, and then exporting it as a KML."),
             
             h4("6. Can I search for any location in Australia?"),
             p("You can search anywhere in Australia, including external territories such as Norfolk Island or Christmas Island, or Australian waters."),
             
             h4("7. Where do the data come from?"),
             p("All data are extracted from the Atlas of Living Australia (ALA), Australia's national biodiversity database."),
             
             h4("8. Which data sources within the ALA are included?"),
             p("The app generates lists from two data sources, both of which are associated with some kind of voucher, i.e., records that are 'verifiable'. First, all records associated with a physical voucher stored in an Australian institution (such as herbaria and museums) are included for the voucher type 'Collection'. Second, all Australian records from the online citizen science platform iNaturalist that have qualified to enter the ALA are included for the voucher types 'Photograph' and 'Recording'."),
             
             h4("9. What does the buffer do?"),
             p("Selecting a buffer includes additional species that have not been recorded within the main target area, but have been seen in the immediate surrounding area up to the defined radius."),
             
             h4("10. Which taxonomic trees does the app use?"),
             p("The species names presented in the app follow the taxonomic trees used by the ALA. For plants, names are taken from the Australian Plant Census, and for animals, names are taken from the Australian Faunal Directory (with minor exceptions for both)."),
             
             h4("11. Are records only included in the app if they are identified to species?"),
             p("Yes, any records that are identified to a taxonomic level coarser than species will not be retrieved by the app."),
             
             h4("12. Does the app reveal the location of species with sensitive locations?"),
             p("Species with sensitive locations are not included in our app. Any species for which records have their locality data obscured or generalised (whether by the original data provider, or by the ALA itself) are excluded from the app."),
             
             h4("13. How often is the app updated?"),
             p("Our major data files from the ALA are re-downloaded once every month."),
             
             h4("14. How can I download data from the app?"),
             p("You can download any given generated list by clicking on the 'Download all obs CSV' button. This will download a CSV file containing all records within the target area (and buffer, if selected), not just the most recent records that are presented in the table."),
             
             h4("15. What do the different symbols on the map represent?"),
             p("The blue markers represent a single record. The coloured circles represent clusters of points; the number in the centre of each circle shows how many records are within that cluster. Zooming in on the map will resolve these clusters into their individual points. If a cluster remains at the maximum zoom level, clicking it will resolve it into its individual points."),
             
             h4("16. Why is the app called 'An Infinity of Lists'?"),
             p("The app's name is a reference to the book 'The Infinity of Lists' by Italian author Umberto Eco."),
    )
    )
  )

