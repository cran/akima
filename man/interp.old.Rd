\name{interp.old}
\title{Gridded Bivariate Interpolation for Irregular Data}
\alias{interp.new}
\alias{interp.old}
\description{
  These functions implement bivariate interpolation onto a grid
  for irregularly spaced input data. These functions are only for
  backward compatibility, use \code{\link{interp}} instead.
}
\usage{
interp.old(x, y, z, xo= seq(min(x), max(x), length = 40),
           yo=seq(min(y), max(y), length = 40), ncp = 0,
           extrap=FALSE, duplicate = "error", dupfun = NULL)
interp.new(x, y, z, xo = seq(min(x), max(x), length = 40),
           yo = seq(min(y), max(y), length = 40), linear = FALSE,
           ncp = NULL, extrap=FALSE, duplicate = "error", dupfun = NULL)
}
\arguments{
  \item{x}{
    vector of x-coordinates of data points or a
    \code{SpatialPointsDataFrame} object.
    Missing values are not accepted.
  }
  \item{y}{
    vector of y-coordinates of data points.
    Missing values are not accepted.

    If left as NULL indicates that \code{x} should be a
  \code{SpatialPointsDataFrame} and \code{z} names the variable of
  interest in this dataframe.
  }
  \item{z}{
    vector of z-coordinates of data points or a character variable
    naming the variable of interest in the
    \code{SpatialPointsDataFrame} \code{x}.

    Missing values are not accepted.

    \code{x}, \code{y}, and \code{z} must be the same length
    (execpt if \code{x} is a \code{SpatialPointsDataFrame}) and may
    contain no fewer than four points. The points of \code{x} and
    \code{y} cannot be collinear, i.e, they cannot fall on the same line
    (two vectors \code{x} and \code{y} such that \code{y = ax + b} for
    some \code{a}, \code{b} will not be accepted). \code{interp} is
    meant for cases in which you have \code{x}, \code{y} values
    scattered over a plane and a \code{z} value for each.  If, instead,
    you are trying to evaluate a mathematical function, or get a
    graphical interpretation of relationships that can be described by a
    polynomial, try \code{outer()}.
  }
  \item{xo}{
    vector of x-coordinates of output grid.  The default is 40 points
    evenly spaced over the range of \code{x}.  If extrapolation is not being
    used (\code{extrap=FALSE}, the default), \code{xo} should have a
    range that is close to or inside of the range of \code{x} for the
    results to be meaningful.
  }
  \item{yo}{vector of y-coordinates of output grid; analogous to
    \code{xo}, see above.}
  \item{linear}{logical -- indicating wether linear or spline
    interpolation should be used. supersedes old \code{ncp} parameter}
  \item{ncp}{
    deprecated, use parameter \code{linear}. Now only used by
    \code{interp.old()}.

    old meaning was:
    number of additional points to be used in computing partial
    derivatives at each data point.
    \code{ncp} must be either \code{0} (partial derivatives are not used), or at
    least 2 but smaller than the number of data points (and smaller than
    25).
  }
  \item{extrap}{
    logical flag: should extrapolation be used outside of the
    convex hull determined by the data points?}
  \item{duplicate}{character string indicating how to handle duplicate
    data points. Possible values are
    \describe{
      \item{\code{"error"}}{produces an error message,}
      \item{\code{"strip"}}{remove duplicate z values,}
      \item{ \code{"mean"},\code{"median"},\code{"user"}}{calculate
	mean , median or user defined function (\code{dupfun}) of duplicate
	z values.}
  }}
  \item{dupfun}{a function, applied to duplicate points if
    \code{duplicate= "user"}.}
}
\value{
  list with 3 components:
  \item{x,y}{
    vectors of x- and y- coordinates of output grid, the same as the input
    argument \code{xo}, or \code{yo}, if present.  Otherwise, their
    default, a vector 40 points evenly spaced over the range of the
    input \code{x}.}
  \item{z}{
    matrix of fitted z-values.  The value \code{z[i,j]} is computed
    at the x,y point \code{xo[i], yo[j]}. \code{z} has
    dimensions \code{length(xo)} times \code{length(yo)}.}

  If input is a \code{SpatialPointsDataFrame} a
  \code{SpatialPixelssDataFrame} is returned.
}
\note{
  \code{interp.new} is deprecated and \code{interp.old} will soon be
  deprecated.
}
\details{
  see \code{\link{interp}}
}
\references{
  Akima, H. (1978). A Method of Bivariate Interpolation and
  Smooth Surface Fitting for Irregularly Distributed Data Points.
  ACM Transactions on Mathematical Software \bold{4}, 148-164.

  Akima, H. (1996). Algorithm 761: scattered-data surface fitting that has
  the accuracy of a cubic polynomial.
  ACM Transactions on Mathematical Software \bold{22}, 362--371.
}
\seealso{
  \code{\link{contour}}, \code{\link{image}},
  \code{\link{approx}}, \code{\link{spline}},
  \code{\link{aspline}},
  \code{\link{outer}}, \code{\link{expand.grid}}.
}
\keyword{dplot}

