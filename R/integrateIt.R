#' A method to calculate the Trapezoidal and Simpson sums of a function.
#'
#' Creates objects of either class 'Trapezoid' or 'Simpson'
#'
#' An object of the these classes has the following slots:
#' @param x The x values of the function you use.
#' @param y The y values of the function you use.
#' @param a The start of the integration which is a point on the x-axis.
#' @param b The end of the integration which is a different point on the x-axis.
#' @param choose_calc A user choice to determine which sum you decide to calculate.
#'
#'
#'
#' @author Joseph Ludmir: \email{jludmir@@wustl.edu}
#' @aliases integrateIt-class
#' @rdname integrateIt
#' @export
setClass(Class = "trapezoid",
         representation = representation(
           name = "character",
           delegatesWon = "numeric",
           party = "character",
           delegatesNeeded = "numeric"
         ),
         prototype = prototype(
           name = c(),
           delegatesWon = c(),
           party = c(),
           delegatesNeeded = c()

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
