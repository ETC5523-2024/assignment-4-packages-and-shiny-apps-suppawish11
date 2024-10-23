#' @title Launch a shiny app
#'
#' @description
#' This calls the shiny app based on the space_object dataset with analysis shown inside.
#'
#'
#' @return Return Shiny app
#'
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#'
#'
#'
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "spaceobs")
  shiny::runApp(app_dir, display.mode = "normal")
}
