#' @importFrom fs path dir_exists
#' @importFrom shiny runApp


#' @export
drilldowndemo_run <- function(app_name) {
  source_folder <- path("inst", app_name)
  installed_folder <- system.file(app_name, package = "drilldowndemo")
  app_folder <- NULL
  if(dir_exists(source_folder)) app_folder <- source_folder
  if(is.null(app_folder) && dir_exists(installed_folder)) app_folder <- installed_folder
  if(!is.null(app_folder)) {
    runApp(path(app_folder, "app.R"))
  } else {
    stop("No app found in the '/inst' folder or package folder")
  }

}
