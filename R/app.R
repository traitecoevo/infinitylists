#' Run Infinity List App in R
#'
#' @param ... 
#'
#' @return
#' @export
#' @import shiny

infinitylistApp <- function(...){
  ui <- ui() 
  server <- infinity_server()
  
  shinyApp(ui, server, ...)
} 