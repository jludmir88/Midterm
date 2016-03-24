#' A trapezoid object.
#'
#' Object of class \code{trapezoid} are created by the \code{integrateIt} function
#' which is created by \code{x} and \code{y} and \code{a} and \code{b} and \code{choose_calc}
#' and the result is printed by \code{print_trap}
#'
#'
#' An object of the class `trapezoid' has the following slots:
#' \itemize{
#' \item \code{x} The x-values of the function.
#' \item \code{y} The y-values of the function
#' \item \code{a} The starting point on the x-axis of integration.
#' \item \code{b} The ending point on the x-axis of integration.
#' \item \code{calc} The resulting Trapezoidal sum by integration.
#' }
#'
#' @author Joseph Ludmir: \email{jludmir@@wustl.edu}
#' @aliases trapezoid-class initialize,trapezoid-method
#' @rdname trapezoid
#' @export
setClass(Class = "trapezoid",
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
setValidity("trapezoid", function(object){
  if(object@a >= object@b){
    stop("The starting and ending points of integration must be different and the starting point must be smaller.")
  }
})
#' @export
setValidity("trapezoid", function(object){
  if(length(object@x) != length(object@y)){
    stop("The number of x values must equal the number of y values.")
  }
})

#' @export
setMethod("initialize", "trapezoid",
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
)
#' @rdname trapezoid
#' @export
setGeneric("gettrapezoid",
           function(object="trapezoid")  {
             standardGeneric("gettrapezoid")
           }
)

#' @export
setMethod("gettrapezoid", "trapezoid",
          function(object){
            return(object@trapezoid)
          }
)
