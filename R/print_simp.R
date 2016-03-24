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
#' @rdname print_simp
#' @aliases print_simp, simpson-method
#' @export
setGeneric("print_simp",
           ## Sets up the generic for the method.
           def=function(simp)
           {standardGeneric("print_simp")}
)
#' @export
setMethod("print_simp",
          signature("simpson"),
          ## Allows for the calc slot of a simpson calculation to solely be returned
          ## thus giving you the result.
          definition = function(simp){
            return(simp@calc)
          }
)
