#' Run Infinity List App in R
#'
#' @param ... 
#' @export

infinitylistApp <- function(...){
  check_and_download_update()
  # Get the list of files in the data directory
  ui <- ui() 
  server <- infinity_server()
  
  shinyApp(ui, server, ...)
} 