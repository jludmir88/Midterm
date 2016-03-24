#' A simpson object.
#'
#' Object of class \code{simpson} are created by the \code{integrateIt} function
#' which is created by \code{x} and \code{y} and \code{a} and \code{b} and \code{choose_calc}
#'
#'
#' An object of the class `Trapezoid' has the following slots:
#' \itemize{
#' \item \code{x} The x-values of the function.
#' \item \code{y} The y-values of the function
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
           calc = "numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c(),
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
            return(object@trapezoid)
          }
)
