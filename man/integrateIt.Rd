% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/integrateIt-package.R, R/integrateIt.R
\docType{package}
\name{integrateIt}
\alias{integrateIt}
\alias{integrateIt-class}
\alias{integrateIt-package}
\title{integrateIt}
\usage{
integrateIt(x, y, a, b, choose_calc)
}
\arguments{
\item{x}{The x values of the function you use.}

\item{y}{The y values of the function you use.}

\item{a}{The start of the integration which is a point on the x-axis.}

\item{b}{The end of the integration which is a different point on the x-axis.}

\item{choose_calc}{A character string to determine which sum you decide to calculate.}
}
\value{
An object of class 'trapezoid' or 'simpson' containing
\item{x} The x-values of the function.
\item{y} The y-values of the function
\item{a} The starting point on the x-axis of integration.
\item{b} The ending point on the x-axis of integration.
\item{calc} The resulting Trapezoidal or Simpson sum by integration.
}
\description{
The integrateIt package approximates the area under a curve.

Creates objects of either class 'Trapezoid' or 'Simpson'
}
\details{
An object of the these classes has the following slots:
}
\examples{
x <- c(1,2,3,4)
y <- 2*x
a <- 0
b <- 5
integrateIt(x, y, a, b, "t")
integrateIt(x, y, a, b, "s")

}
\author{
Joseph Ludmir: \email{jludmir@wustl.edu}

Joseph Ludmir: \email{jludmir@wustl.edu}
}
\seealso{
\code{\link{trapezoid}}, \code{\link{simpson}}
}

