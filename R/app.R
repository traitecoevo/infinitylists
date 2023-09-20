#' Run Infinity List App in R
#'
#' @param ... 
#' @export

infinitylistApp <- function(...){
  check_and_download_update()
  # Get the list of files in the data directory
  files_in_directory <- list.files(path = system.file(package = "infinitylists", "data/"), pattern = ".parquet")
  
  taxa_names <-
    gsub("Australia-(.+?)-[0-9]{4}-[0-9]{2}-[0-9]{2}.parquet",
         "\\1",
         files_in_directory)
  
  files_in_directory <- setNames(files_in_directory, taxa_names)
  ui <- ui() 
  server <- infinity_server()
  
  shinyApp(ui, server, ...)
} 