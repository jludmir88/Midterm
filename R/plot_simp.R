#' Plots the graph of a function using simpson sums
#'
#' Plots the results of simpson sums
#'
#' @param simp An object of the class 'simpson'
#'
#' @return The plot of the results of the calculation
#'
#' @author Joseph Ludmir
#'
#' @rdname plot_simp
#' @aliases plot_simp, simpson-method
#' @export
setGeneric("plot_simp",
           def=function(simp)
           {standardGeneric("plot_simp")}
)
#' @export
setMethod("plot_simp",
          signature("simpson"),
          definition = function(simp){
            ## Plots all the points of the simpson sums from start to finish
            ## of the region in question.
            plot(simp@x, simp@y, type = "n", main = "The Visual of the Simpson Sum",
                 xlab = "x", ylab = "y", xlim = c(simp@a, simp@b),
                 ylim = c(0, max(simp@y[simp@b])))
            ## Using the formulas provided on the slide, I simply defined
            ## all of the variables for every single interval, so u is always one
            ## interval behind f_u and the same goes for w and f_w.
            u <- simp@x[1:length(simp@x)-1]
            f_u <- simp@y[1:length(simp@y)-1]
            w <- simp@x[2:length(simp@x)]
            f_w <- simp@y[2:length(simp@y)]
            ## Setting the v based on the formula
            v <- ((w-u)/2)
            ## Doing the same for f_V
            f_v <- ((f_w-f_u)/2)
            ## Creates a function that calculates the p of the formula
            p_x <- function(x){
              p <- ((f_u*(((x-v)(x-w))/((u-v)(u-w)))) +
              (f_v*(((x-u)(x-w))/((v-u)(v-w)))) +
              (f_v*(((x-u)(x-v))/((w-u)(w-v)))))
              return(p)
            ## Plots all of the points
            points(c(u,w), c(f_u,f_w))
            points(v, f_v)
            ## Creates the curve
            curve(p, add = TRUE)
            }
            }

)
