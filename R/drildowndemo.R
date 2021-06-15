#' @importFrom fs path dir_exists
#' @importFrom shiny runApp
#' @importFrom rstudioapi navigateToFile

#' @export
drilldowndemo_run <- function(app_name) {
  app_file <- find_app(app_name = app_name)
  runApp(app_file)
}

#' @export
drilldowndemo_open <- function(app_name) {
  app_file <- find_app(app_name = app_name)
  navigateToFile(app_file)
}

find_app <- function(app_name) {
  app_folder <- NULL
  app_file <- NULL
  source_folder <- path("inst", app_name)
  installed_folder <- system.file(app_name, package = "drilldowndemo")
  if(dir_exists(source_folder)) app_folder <- source_folder
  if(is.null(app_folder) && dir_exists(installed_folder)) app_folder <- installed_folder
  if(!is.null(app_folder)) app_file <- path(app_folder, "app.R")
  if(is.null(app_file)) stop("App not found")
  app_file
}
