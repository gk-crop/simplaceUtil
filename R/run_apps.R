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
#' @param plotlist list of named functions that produce a plot
#' @param datalist list of named functions that transform output dataframes
#' @param paramlist list of named parameter elements
#' @param paramtransform function to transform the parameterlist
#' @param ... parameters passed to shiny::runApp
#' @return does not return a value, called for the side effect of running the shiny app
#' @export
#'
runSimplaceCustomGuiApp <- function(simplacedirs, solution, project,
                                    plotlist=list("Default"=simplaceUtil::plotScalarOutput),
                                    datalist=list("Default"=identity),
                                    paramlist=list(),
                                    paramtransform = identity,
                                    ...)
{
  shiny::shinyOptions(simplacedirs = simplacedirs)
  shiny::shinyOptions(solutions = solution)
  shiny::shinyOptions(projects = project)
  shiny::shinyOptions(plotlist = plotlist)
  shiny::shinyOptions(datalist = datalist)
  shiny::shinyOptions(paramlist = paramlist)
  shiny::shinyOptions(paramtransform = paramtransform )
  shiny::runApp(appDir = system.file("shiny","customgui", package = "simplaceUtil"),...)
}



