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
#' * `plotlist` takes a named list of functions of the form
#'     `f(data, xvalue, yvalues, simulationids, from, to)` and creates a plot
#'   - `data` is a data.frame
#'   - `xvalue` is a string with the column name for x
#'   - `yvalues` is a vector of strings with the column names for y
#'   - `simulationids` can be NULL or a vector of strings with simulation ids
#'   - `from` and `to` are dates for subsetting the data that will be plotted
#' * `datalist` takes a named list of functions of the form `f(data)` and return
#' the modified data.frame
#' * `paramlist` is a named list of parameters where the name of each element has to
#' match a `var` in the solution (unless the list will be transformed). Each parameter is a list with the elements
#'   - `label`
#'   - `value`
#'   - `min` and `max` or `options` (vector of possible values)
#' * `paramtransform` is a function that transforms the input parameters to hand
#' them over to Simplace
#'
#' @param simplacedirs a list containing the named elements instdir, workdir, outdir and options
#' @param solution filename of a solution (or solution as text)
#' @param project filename of a project
#' @param plotlist list of named functions that produce a plot
#' @param datalist list of named functions that transform output dataframes
#' @param paramlist list of named parameter elements that will be used for input sliders / option lists
#' @param paramtransform function to transform the parameterlist
#' @param ... parameters passed to shiny::runApp
#' @return does not return a value, called for the side effect of running the shiny app
#' @export
#' @examples
#' \dontrun{
#' instdir <- simplace::findFirstSimplaceInstallation()
#' simplacedirs <- list(
#'   instdir = instdir,
#'   workdir = paste0(instdir,"/simplace_run/simulation/"),
#'   outdir = paste0(instdir,"/simplace_run/output/")
#' )
#'
#' solution <- paste0(simplacedirs$workdir,"/gk/solution/calibration/Yield.sol.xml")
#'
#' paramlist <- list(
#'  vLUE = list(
#'    label = "Light Use efficiency",
#'    value = 3.0,
#'    min = 1.0,
#'    max = 6.5
#'  )
#' )
#'
#' plotlist <- list(
#'   "Default" = simplaceUtil::plotScalarOutput,
#'   "Boxplots" = function(data,x,y,sim,from,to) {
#'     if(length(y)==1 && is.numeric(data[[y]])) {
#'       boxplot(formula(paste(y,"~simulationid")),data)
#'     }
#'   }
#' )
#'
#' datalist <- list(
#'   "Aggregated Yield"=function(data) {
#'     dplyr::group_by(data, simulationid) |>
#'     dplyr::summarise(minY=min(Yield),maxY=max(Yield))},
#'   "Raw Data"=identity
#' )
#'
#' simplaceUtil::runSimplaceCustomGuiApp(
#'   simplacedirs, sol,
#'   plotlist = plotlist,
#'   datalist = datalist,
#'   paramlist = paramlist
#' )
#'
#' }
#'
runSimplaceCustomGuiApp <- function(simplacedirs,
                                    solution,
                                    project = NULL,
                                    plotlist=list("Default"=simplaceUtil::plotScalarOutput),
                                    datalist=list("Default"=identity),
                                    paramlist=list(),
                                    paramtransform = identity,
                                    ...)
{
  shiny::shinyOptions(simplacedirs = simplacedirs)
  shiny::shinyOptions(solution = solution)
  shiny::shinyOptions(project = project)
  shiny::shinyOptions(plotlist = plotlist)
  shiny::shinyOptions(datalist = datalist)
  shiny::shinyOptions(paramlist = paramlist)
  shiny::shinyOptions(paramtransform = paramtransform )
  shiny::runApp(appDir = system.file("shiny","customgui", package = "simplaceUtil"),...)
}



