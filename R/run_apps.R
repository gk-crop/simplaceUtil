#' Runs Shiny app
#'
#' @param ... parameters passed to shiny::runApp
#' @export

runSimplaceGuiApp <- function(...)
{
  shiny::runApp(appDir = system.file("shiny","gui", package = "simplaceUtil"),...)
}
