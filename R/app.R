#' Run Infinity List App in R
#'
#' @param ... 
#' @export

infinitylistApp <- function(...){
  ui <- ui() 
  server <- infinity_server()
  
  shinyApp(ui, server, ...)
} 