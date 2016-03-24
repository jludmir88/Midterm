#' Print methods for simpson sums
#'
#' Prints the results of simpson sums
#'
#' @param simp An object of the class 'simpson'
#'
#' @return The result of the calculation
#'
#' @author Joseph Ludmir
#'
#' @rdname print
#' @aliases print,simpson-method
#' @export
setGeneric("print_simp",
           def=function(simp)
           {standardGeneric("print_simp")}
)
#' @export
setMethod("print_simp",
          signature("simpson"),
          definition = function(simp){
            return(simp@calc)
          }
)
