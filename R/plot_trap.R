#' Plots the graph of a function using trapezoidal sums
#'
#' Plots the results of trapezoidal sums
#'
#' @param trap An object of the class 'trapezoid'
#'
#' @return The plot of the results of the calculation
#'
#' @author Joseph Ludmir
#'
#' @rdname plot_trap
#' @aliases plot_trap, trapezoid-method
#' @export
setGeneric("plot_trap",
           def=function(trap)
           {standardGeneric("plot_trap")}
)
#' @export
setMethod("plot_trap",
          signature("trapezoid"),
          definition = function(trap){
            plot(trap@x, trap@y, type = "n", main = "The Visual of the Trapezoidal Sum",
                 xlab = "x", ylab = "y", xlim = c(trap@a, trap@b),
                 ylim = c(0, trap@y[trap@b]))
            segments(x0 = trap@x, y0 = 0, x1 = trap@x, y1 = trap@y)
            segments(x0 = trap@x[1:length(trap@x)-1], y0 = trap@y[1:length(trap@x)-1],
                     x1 = trap@x[2:length(trap@x)], y1 = trap@y[2:length(trap@x)])
          }
)
