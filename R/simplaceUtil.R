#' Package with utility function for working with Simplace
#'
#' Provides functions to work with the modeling framework Simplace
#'
#' - get elements of a simplace solution as dataframes
#' - visualise the structure of a solution as a graph
#' - helper functions for transforming output data
#' - simplified plot functions for simulation output variables
#' - shinyApp to visualise graphs, run simulations and plot results
#'
#' @author {Gunther Krauss}
#'
#' @examples \dontrun{
#' # run the GUI
#' runSimplaceGuiApp()
#' }
#'
#' # visualise the solution structure as graph
#' graph <- solutionToGraph(system.file("solution", "Yield.sol.xml",
#'   package = "simplaceUtil"))
#' DiagrammeR::render_graph(graph)
#' @name simplaceUtil
NULL
