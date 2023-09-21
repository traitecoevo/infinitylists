#' Launch the Infinity List Application
#'
#' This function initializes and runs the "Infinity List" application using 
#' Shiny. Before launching, it checks for any available updates and downloads 
#' them if necessary. The app provides a user interface (`ui`) and server-side 
#' logic (`server`) to interactively work with the Infinity List.
#'
#' @param ... Additional arguments passed to `shinyApp()`. This can include
#'            parameters like `port`, `launch.browser`, and others as described 
#'            in the documentation for `shinyApp()`.
#'
#' @details The function carries out the following steps:
#' 1. Checks and downloads any available updates using `check_and_download_update()`.
#' 2. Initializes the user interface (`ui`) for the application.
#' 3. Sets up the server-side logic (`server`) for the application.
#' 4. Launches the Shiny application using `shinyApp()`.
#'
#' @return This function launches the Shiny application and does not return until 
#'         the app is terminated by the user.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Launch the Infinity List App
#'   infinitylistApp()
#' }
#'


infinitylistApp <- function(...){
  check_and_download_update()
  # Get the list of files in the data directory
  ui <- ui() 
  server <- infinity_server()
  
  shinyApp(ui, server, ...)
} 