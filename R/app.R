#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#' @import shiny

infinitylistApp <- function(...){
  ui <- ui() 
  server <- server
  
  shinyApp(ui, server, ...)
} 