#' A trapezoidal object.
#'
#' Object of class \code{trapezoid} are created by the \code{integrateIt} function
#' which is created by \code{x} and \code{y} and \code{a} and \code{b} and \code{choose_calc}
#'
#'
#' An object of the class `Trapezoid' has the following slots:
#' \itemize{
#' \item \code{x} The x-values of the function.
#' \item \code{y} The y-values of the function
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
           calc = "numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c(),
           calc = c()
         )
)
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
