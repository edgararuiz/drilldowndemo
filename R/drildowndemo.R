#' @importFrom fs path dir_exists dir_ls file_exists path_file
#' @importFrom shiny runApp
#' @importFrom rstudioapi navigateToFile

#' @title Runs an example Shiny app
#' @param app_name Name of one of Shiny apps within the package. Possible values:
#' intro, tabs, navigation, details
#' @export
drilldowndemo_run <- function(app_name =  NULL) {
  app_file <- find_app(app_name = app_name)
  if (interactive()) runApp(app_file)
}

#' @title Opens an example Shiny app in the RStudio IDE
#' @param app_name Name of one of Shiny apps within the package. Possible values:
#' intro, tabs, navigation, details
#' @export
drilldowndemo_open <- function(app_name = NULL) {
  app_file <- find_app(app_name = app_name)
  if (interactive()) navigateToFile(app_file)
}

find_folder <- function() {
  app_folder <- NULL
  source_folder <- path("inst", "shiny")
  installed_folder <- system.file("shiny", package = "drilldowndemo")
  if (dir_exists(source_folder)) app_folder <- source_folder
  if (is.null(app_folder) && dir_exists(installed_folder)) app_folder <- installed_folder
  app_folder
}


find_app <- function(app_name = NULL) {
  app_folder <- find_folder()
  app_folders <- dir_ls(app_folder)
  sub_folders <- path_file(app_folders)
  app_names <- substr(sub_folders, 3, nchar(sub_folders))

  if(is.null(app_name)) {
    cat("Available apps:\n", paste0(sub_folders, "\n"))
    if(interactive()) {
      app_number_char <- readline(prompt="Enter the app number to select: ")
    } else {
      app_number_char <- 1
    }
    app_number <- as.numeric(app_number_char)
    if(app_number_char == 0) stop("Not a valid selection")
    app_name <- sub_folders[app_number]
  }

  app_match <- app_names == app_name
  if (any(app_match)) app_name <- sub_folders[app_match]
  file_path <- path(app_folder, app_name, "app.R")
  if (!file_exists(file_path)) stop("App not found")
  file_path
}
