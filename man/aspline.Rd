\name{aspline}
\alias{aspline}

\title{Univariate Akima interpolation}
\description{
  The function returns a list of points which smoothly
  interpolate given data points, similar to a curve drawn by hand.}

\usage{
  aspline(x, y=NULL, xout, n = 50, ties = mean, method="original", degree=3) }

\arguments{
  \item{x, y}{vectors giving the coordinates of the points to be
    interpolated.  Alternatively a single plotting structure can be
    specified: see \code{\link{xy.coords}}.}
  \item{xout}{an optional set of values specifying where interpolation
    is to take place.}
  \item{n}{If \code{xout} is not specified, interpolation takes place at
    \code{n} equally spaced points spanning the interval [\code{min(x)},
    \code{max(x)}].}
  \item{ties}{Handling of tied \code{x} values.  Either a function
    with a single vector argument returning a single number result or
    the string \code{"ordered"}.}
  \item{method}{either \code{"original"} method after Akima (1970) or
    \code{"improved"} method after Akima (1991)}
  \item{degree}{if improved algorithm is selected: degree of the
    polynomials for the interpolating function}
}
  
\details{
  The original algorithm is based on a piecewise function composed of a
  set of polynomials, each of degree three, at most, and applicable to
  successive interval of the given points. In this method, the slope of
  the curve is determined at each given point locally, and each
  polynomial representing a portion of the curve between a pair of given
  points is determined by the coordinates of and the slopes at the
  points.  }

\value{
  A list with components \code{x} and \code{y},
  containing \code{n} coordinates which interpolate the given data
  points.  }

\references{
  Akima, H. (1970) A new method of interpolation
  and smooth curve fitting based on local procedures,
  J. ACM \bold{17}(4), 589-602

  Akima, H. (1991) A Method of Univariate Interpolation that Has
  the Accuracy of a Third-degree Polynomial. ACM Transactions on
  Mathematical Software, \bold{17}(3), 341-366.
  
}


\seealso{\code{\link{approx}}, \code{\link{spline}}}
\examples{
## regular spaced data
x <- 1:10
y <- c(rnorm(5), c(1,1,1,1,3))

xnew <- seq(-1, 11, 0.1)
plot(x, y, ylim=c(-3, 3), xlim=range(xnew))
lines(spline(x, y, xmin=min(xnew), xmax=max(xnew), n=200), col="blue")

lines(aspline(x, y, xnew), col="red")
lines(aspline(x, y, xnew, method="improved"), col="black", lty="dotted")
lines(aspline(x, y, xnew, method="improved", degree=10), col="green", lty="dashed")

## irregular spaced data
x <- sort(runif(10, max=10))
y <- c(rnorm(5), c(1,1,1,1,3))

xnew <- seq(-1, 11, 0.1)
plot(x, y, ylim=c(-3, 3), xlim=range(xnew))
lines(spline(x, y, xmin=min(xnew), xmax=max(xnew), n=200), col="blue")

lines(aspline(x, y, xnew), col="red")
lines(aspline(x, y, xnew, method="improved"), col="black", lty="dotted")
lines(aspline(x, y, xnew, method="improved", degree=10), col="green", lty="dashed")

## an example of Akima, 1991
x <- c(-3, -2, -1, 0,  1,  2, 2.5, 3)
y <- c( 0,  0,  0, 0, -1, -1, 0,   2)

plot(x, y, ylim=c(-3, 3))
lines(spline(x, y, n=200), col="blue")

lines(aspline(x, y, n=200), col="red")
lines(aspline(x, y, n=200, method="improved"), col="black", lty="dotted")
lines(aspline(x, y, n=200, method="improved", degree=10), col="green", lty="dashed")
}
\keyword{arith}
\keyword{dplot}
