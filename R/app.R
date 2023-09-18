#' Run Infinity List App in R
#'
#' @param ... 
#' @export
#' @import shiny

infinitylistApp <- function(...){
  ui <- ui() 
  server <- infinity_server()
  
  shinyApp(ui, server, ...)
} 