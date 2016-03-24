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
#' @return An object of class 'trapezoid' or 'simpson' containing
#' \item {x} The x-values of the function.
#' \item {y} The y-values of the function
#' \item {a} The starting point on the x-axis of integration.
#' \item {b} The ending point on the x-axis of integration.
#' \item {calc} The resulting Trapezoidal or Simpson sum by integration.
#'
#' @author Joseph Ludmir: \email{jludmir@@wustl.edu}
#' @examples
#' x <- c(1,2,3,4)
#' y <- 2*x
#' a <- 0
#' b <- 5
#' integrateIt(x, y, a, b, t)
#' integrateIt(x, y, a, b, s)
#'
#' @seealso \code{\link{trapezoid}}, \code{\link{simpson}}
#' @aliases integrateIt-class
#' @rdname integrateIt
#' @export
setGeneric("integrateIt",
           def = function(x, y, a, b, choose_calc)
           {standardGeneric("integrateIt")}
          )


#' @export
setMethod("integrateIt",
          definition = function(x, y, a, b, choose_calc){
            # For this h we use the number of x points minus one because were looking
            # at the number of intervals.
            h <- (b-a)/((length(x)-1))
            x_values <- which(x == a):which(x == b)
            y_values <- which(y == a):which(y == b)
            y_size <- length(y_values)
            if(choose_calc = t){
            int_trap <- (h/2)*(y_values[1] + y_values[y_size]
                          + 2*(y_values[2]:y_values[y_size-1]))
            trap_calc <- new("trapezoid", x = x_values, y = y_values,
                             a = a, b = b, calc = int_trap)
            return(trap_calc)
            }
            if(choose_calc = s){
            even_y <- which(y_values %% 2 = 0)
            odd_y <- which(y_values %% 2 = 1)
            int_simp <- (h/3)*(y_values[1] + y_values[y_size] +
                    sum(4*y_values[seq(2,(y_size-1), by = 2)])+
                    sum(2*y_values[seq(3, (y_size-2))])
