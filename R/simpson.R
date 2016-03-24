#' A simpson object.
#'
#' Object of class \code{simpson} are created by the \code{integrateIt} function
#' which is created by \code{x} and \code{y} and \code{a} and \code{b} and \code{choose_calc}
#' and the result is printed by \code{print_trap}
#'
#' An object of the class `simpson' has the following slots:
#' \itemize{
#' \item \code{x} The x-values of the function.
#' \item \code{y} The y-values of the function
#' \item \code{a} The starting point on the x-axis of integration.
#' \item \code{b} The ending point on the x-axis of integration.
#' \item \code{calc} The resulting Simpson sum by integration.
#' }
#'
#' @author Joseph Ludmir: \email{jludmir@@wustl.edu}
#' @aliases simpson-class initialize,simpson-method
#' @rdname simpson
#' @export
setClass(Class = "simpson",
         representation = representation(
           x = "numeric",
           y = "numeric",
           a = "numeric",
           b = "numeric",
           calc = "numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c(),
           a = c(),
           b = c(),
           calc = c()
         )
)
#' @export
setMethod("initialize", "simpson",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
)
#' @export
setValidity("simpson", function(object){
  if(object@a >= object@b){
    stop("The starting and ending points of integration must be different and the starting point must be smaller.")
  }
})
#' @export
setValidity("simpson", function(object){
  if(length(object@x) != length(object@y)){
    stop("The number of x values must equal the number of y values.")
  }
})
#' @export
setValidity("simpson", function(object){
  if((length(object@x)%%2) != 0){
    stop("The number of points being evaluated must be even.")
  }
})
#' @rdname trapezoid
#' @export
setGeneric("getsimpson",
           function(object="simpson")  {
             standardGeneric("getsimpson")
           }
)

#' @export
setMethod("getsimpson", "simpson",
          function(object){
            return(object@simpson)
          }
)
