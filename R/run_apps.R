#' Runs Shiny app
#'
#' @param ... parameters passed to shiny::runApp
#' @export

runSimplaceGuiApp <- function(...)
{
  shiny::runApp(appDir = system.file("shiny","gui", package = "simplaceUtil"),...)
}

#' Runs Custom Shiny app
#'
#' User can create automatically a simple GUI for a specific solution/project
#' by specifying solution, project, list of parameters that could be interatively
#' changed as well a list of custom plot functions.
#'
#'
#' @param simplacedirs a list containing the named elements instdir, workdir, outdir and options
#' @param solution filename of a solution (or solution as text)
#' @param project filename of a project
#' @param paramlist list of named parameter elements
#' @param plotlist list of named functions that produce a plot
#' @param ... parameters passed to shiny::runApp
#' @return does not return a value, called for the side effect of running the shiny app
#' @export
#'
runSimplaceCustomGuiApp <- function(simplacedirs, solution, project, paramlist, plotlist, ...)
{
  shiny::shinyOptions(simplacedirs=simplacedirs)
  shiny::shinyOptions(solutions=solution)
  shiny::shinyOptions(projects=project)
  shiny::shinyOptions(paramlist=paramlist)
  shiny::shinyOptions(plotlist=plotlist)
  #shiny::runApp(appDir = "inst/shiny/customgui/",...)
  shiny::runApp(appDir = system.file("shiny","customgui", package = "simplaceUtil"),...)
}



