#' Title
#'
#' @return
#' @export
#'
#' @examples
run_app <- function() {
  app_dir <- system.file("app", package = "spaceobs")
  shiny::runApp(app_dir, display.mode = "normal")
}
