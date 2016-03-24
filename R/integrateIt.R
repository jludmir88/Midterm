#' A method to calculate the Trapezoidal and Simpson sums of a function.
#'
#' Creates objects of either class 'Trapezoid' or 'Simpson'
#'
#' An object of the these classes has the following slots:
#' @param x The x values of the function you use.
#' @param y The y values of the function you use.
#' @param a The start of the integration which is a point on the x-axis.
#' @param b The end of the integration which is a different point on the x-axis.
#' @param choose_calc A character string to determine which sum you decide to calculate.
#'
#' @return An object of class 'trapezoid' or 'simpson' containing
#' \item{x} The x-values of the function.
#' \item{y} The y-values of the function
#' \item{a} The starting point on the x-axis of integration.
#' \item{b} The ending point on the x-axis of integration.
#' \item{calc} The resulting Trapezoidal or Simpson sum by integration.
#' @author Joseph Ludmir: \email{jludmir@@wustl.edu}
#' @examples
#' x <- c(1,2,3,4)
#' y <- 2*x
#' a <- 0
#' b <- 5
#' integrateIt(x, y, a, b, "t")
#' integrateIt(x, y, a, b, "s")
#'
#' @seealso \code{\link{trapezoid}}, \code{\link{simpson}}
#' @aliases integrateIt-class
#' @rdname integrateIt
#' @export
setGeneric("integrateIt",
           ## Sets generic method
           def = function(x, y, a, b, choose_calc)
           {standardGeneric("integrateIt")}
)


#' @export
setMethod("integrateIt",
          definition = function(x, y, a, b, choose_calc){
            ## Setting the x values that we care about based on the user choices of a and b.
            x_start <- which(x == a)
            x_end <- which(x == b)
            ## Create h using its formula
            h <- ((b-a)/((x_end - x_start)))
            ## The amount of x-values to be used
            x_size <- (x_end - x_start - 1)
            if(choose_calc == "t"){
            ## For the occasion in which there are only two intervals.
              if(x_size == 1){
                int_trap <- (h/2) * (y[x_start] + y[x_end])
                trap_calc <- new("trapezoid", x = x, y = y,
                                 a = a, b = b, calc = int_trap)
                return(trap_calc)
              } else {
              ## Multiplies h/2 by the addition of the first and last y values and
              ## the doubling of all the values in the middle.
                int_trap <- (h/2)*((y[x_start] + y[x_end]) +
                                     sum(2*(y[(x_start+1):(x_end-1)])))
                trap_calc <- new("trapezoid", x = x, y = y,
                                 a = a, b = b, calc = int_trap)
                return(trap_calc)
              }
            }
            ## The case in which you are only looking at two intervals.
            if(choose_calc == "s"){
              if((x_size) == 1){
                int_simp <- (h/3)*(y[x_start] + y[x_end])
                simp_calc <- new("simpson", x = x, y = y,
                                 a = a, b = b, calc = int_simp)
                return(simp_calc)
              } else {
                ## Essentially divides the y values in 3 groups, the first and last,
                ## the y-values at even indexes to be multiplied by four and the y-
                ## values at odd indexes to be multiplied by two. Then we multiply h/3
                ## by the sum of the entire total.
                multfour <- seq(x_start+1, x_end-1, 2)
                multtwo <- seq(x_start+2, x_end-2, 2)
                y_valstwo <- y[multtwo]
                y_valsfour <- y[multfour]
                int_simp <- h/3*(y[x_start] + sum(4*y_valsfour) +sum(2*y_valstwo) +
                            y[x_end])
                simp_calc <- new("simpson", x = x, y = y,
                                 a = a, b = b, calc = int_simp)

                return(simp_calc)
              }
            }
          })
