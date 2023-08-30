

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
      condition = "input.inputType == 'choose'",
      numericInput(
        "latitude",
        "Latitude",
        value = -33.8688,
        min = min_lat,
        max = max_lat
      ),
      numericInput(
        "longitude",
        "Longitude",
        value = 148.2093,
        min = min_long,
        max = max_long
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
      actionButton("executeButton", "Execute")
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
             p("This app allows users to explore the biodiversity in various areas in Australia."),
             
             h4("2. How do I select a location?"),
             p("You can select a location using one of the three input methods: 'Preloaded Place', 'Upload KML', or 'Choose a place in Australia'."),
             
             h4("3. What is a buffer?"),
             p("A buffer is an additional area around your selected location that you can include in your data query."),
             
             h4("4. How do I download the data?"),
             p("You can download the data by clicking on the 'Download all obs CSV' button."),
             
             # Add more questions and answers as needed
    )
    )
  )

