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
           def=function(trap)
           {standardGeneric("print_trap")}
)
#' @export
setMethod("print_trap",
          signature("trapezoid"),
          definition = function(trap){
            return(trap@calc)
          }
)
