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
           ## Sets up the generic method
           def=function(trap)
           {standardGeneric("plot_trap")}
)
#' @export
setMethod("plot_trap",
          signature("trapezoid"),
          definition = function(trap){
            ## We essentially plot all of the points of the trapezoidal sum by x and y value.
            plot(trap@x, trap@y,
            ## The type makes sure the plot is blank still, main is the title.
            type = "n", main = "The Visual of the Trapezoidal Sum",
            ## Creates the lables, sets the x limit to be the a and b values
            ## so the graph focuses in only on the area calculated that the user wanted.
            ## The y limit is set to be from 0 to the max of the y values of the area in question.
            xlab = "x", ylab = "y", xlim = c(trap@a, trap@b),
            ylim = c(0, max(trap@y[trap@a:trap@b])))
            ## This creates an initial line from the first x value to the second,
            ## and one from 0 on the y axis to the first y value.
            segments(x0 = trap@x, y0 = 0, x1 = trap@x, y1 = trap@y)
            ## Creates trapezoids for every intevral, drawing lines from each
            ## x value to the next and same with the y, from start to finish.
            segments(x0 = trap@x[1:length(trap@x)-1], y0 = trap@y[1:length(trap@x)-1],
                     x1 = trap@x[2:length(trap@x)], y1 = trap@y[2:length(trap@x)])
          }
)
