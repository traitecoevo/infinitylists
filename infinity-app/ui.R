

# ----------------------
# UI
# ----------------------
# Define the user interface for the Shiny app
ui <-
  fluidPage(
    theme = shinytheme("cosmo"),
    titlePanel(
      "An Infinity of Lists: an Interactive Guide to Australian Biodiversity"
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
        min = min_lat,
        max = max_lat,
        step = 0.00001),
      numericInput(
        "longitude",
        "Longitude",
        value = 148.2093,
        min = min_long,
        max = max_long,
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
    tabPanel("FAQ",
             h2("Frequently Asked Questions"),
             
             h4("1. What does this app do?"),
             p("This app allows users to explore biodiversity across Australia. After selecting a taxonomic group and a location, the app will generate a species list."),
             
             h4("2. Which taxonomic groups can I generate lists for?"),
             p("You can generate lists for marsupials, plants, dragonflies + damselflies (Odonata) or butterflies (Papilionoidea)"),
             
             h4("2. How do I select a location?"),
             p("You can select a location using one of the three input methods: 'Preloaded Place', 'Upload KML', or 'Choose a lat/long in Australia'. Note that the five preloaded places are demonstration locations"),
             
             h4("6. What is a KML file?"),
             p("KML stands for Keyhole Markup Language. A KML file stores geographic data and features, and allows these features to be displayed on a map in geospatial software such as Google Earth."),
             
             h4("3. Where can I find a KML file for the location I'm interested in?"),
             p("For some locations, KML files already exist and can be found by searching the internet for '[place name] + KML'. If you cannot find a KML file for your location, you can manually create one in software such as Google Earth by drawing a polygon, and then exporting it as a KML."),
             
             h4("4. How do I download the data?"),
             p("You can download the data by clicking on the 'Download all obs CSV' button."),
             
             h4("6. How do I download the data?"),
             p("You can download the data by clicking on the 'Download all obs CSV' button."),
             
             h4("7. How do I download the data?"),
             p("You can download the data by clicking on the 'Download all obs CSV' button."),
             
             h4("8. How do I download the data?"),
             p("You can download the data by clicking on the 'Download all obs CSV' button."),
             
             h4("9. Why am I "),
             p("You can download the data by clicking on the 'Download all obs CSV' button."),
             
             h4("10. Why is the app called 'An Infinity of Lists'?"),
             p("The app's name is a reference to the book 'The Infinity of Lists' by Italian author Umberto Eco."),
             
             # Add more questions and answers as needed
    )
    )
  )

