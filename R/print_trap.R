#' Print methods for trapezoidal sums
#'
#' Prints the results of trapezoidal sums
#'
#' @param trap An object of the class 'trapezoid'
#'
#' @return The result of the calculation
#'
#' @author Joseph Ludmir
#'
#' @rdname print_trap
#' @aliases print_trap, trapezoid-method
#' @export
setGeneric("print_trap",
           ## Sets up the generic for the new method being used.
           def=function(trap)
           {standardGeneric("print_trap")}
)
#' @export
setMethod("print_trap",
          ## Simply returns the calc slot, that is you are given the calculation.
          signature("trapezoid"),
          definition = function(trap){
            return(trap@calc)
          }
)
