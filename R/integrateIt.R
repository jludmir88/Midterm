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
           def = function(x, y, a, b, choose_calc)
           {standardGeneric("integrateIt")}
          )


#' @export
setMethod("integrateIt",
          definition = function(x, y, a, b, choose_calc){
            x_start <- which(x == a)
            x_end <- which(x == b)
            h <- ((b-a)/((x_end - x_start)))
            x_size <- (x_end - x_start -1)
            if(choose_calc == "t"){
              if(x_size == 1){
                int_trap <- (h/2) * (y[x_start] + y[x_end])
                trap_calc <- new("trapezoid", x = x, y = y,
                                 a = a, b = b, calc = int_trap)
                return(trap_calc)
              } else {
                int_trap <- (h/2)*((y[x_start] + y[x_end]) +
                            sum(2*(y[(x_start+1):(x_end-1)])))
                trap_calc <- new("trapezoid", x = x, y = y,
                             a = a, b = b, calc = int_trap)
                return(trap_calc)
              }
            }
            if(choose_calc == "s"){
              if((x_size) == 2){
                int_simp <- (h/3)*(y[x_start] + y[x_end])
                simp_calc <- new("simpson", x = x, y = y,
                                 a = a, b = b, calc = int_simp)
                return(simp_calc)
              } else {
                int_simp <- h/3*(y[x_start] +
                            4*y[x_start-1] +
                            y[x_end] +
                sum(rep(c(4,2), times=(x_size-2)/2)*y[(x_start+1):(x_end-2)]))
                simp_calc <- new("simpson", x = x, y = y,
                             a = a, b = b, calc = int_simp)

            return(simp_calc)
              }
            }
})
