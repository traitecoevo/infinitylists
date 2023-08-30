

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
    textOutput("statsOutput"),
    tags$br(),
    DTOutput("table"),
    leafletOutput("map", height = 500)
  )
