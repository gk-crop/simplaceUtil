#' Modifies function to return a penalty value for parameters outside boundaries
#'
#' The method takes a function as well as values for lower and upper boundaries
#' and returns a modified function. The modified function returns the value of
#' the original function when the parameters are within boundaries and the penalty
#' value otherwise.
#'
#' Optionally an own function can be supplied to calculate whether the parameter
#' is valid. The boundary function must take 3 arguments: parameter,
#' lower boundary and upper boundary and must return TRUE or FALSE.
#'
#' A main use case of this method are optimisation / calibration tasks. If the
#' optimisation method and the function to optimise are both ignorant to boundaries
#' one can turn the function into a boundary sensitive one.
#'
#' @param fun function to be modified
#' @param l_bound vector with lower boundary values
#' @param u_bound vector with upper boundary values
#' @param penalty_value value if parameter outside boundaries
#' @param boundary_fun optional function for complex boundary conditions
#' @param ... arguments passed to original function
#' @param param_pos argument position of the parameter
#' @return a modified function that considers boundaries
#' @export
#' @examples
#' sqrt_bd <- enhanceFunctionWithBoundaries(sqrt, 0, 10)
#' sqrt_bd(-1)
#' sqrt_bd(1)
#' sqrt_bd(11)
#'
enhanceFunctionWithBoundaries <- function(fun, l_bound, u_bound,
                                  penalty_value=Inf,
                                  boundary_fun=NULL,
                                  param_pos=1,
                                  ...) {
  function(...) {
    invalid <- FALSE
    if(!is.null(boundary_fun)) {
      invalid <- !boundary_fun(...elt(param_pos),l_bound, u_bound)
    }
    else {
      invalid <- !all(l_bound <= ...elt(param_pos) & ...elt(param_pos) <= u_bound)
    }
    if(invalid) {
      penalty_value
    }
    else {
      fun(...)
    }
  }
}
