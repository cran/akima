\name{interpp}
\title{
  Pointwise Bivariate Interpolation for Irregular Data
}
\alias{interpp}
\alias{interpp.old}
\alias{interpp.new}
\usage{
interpp(x, y, z, xo, yo, ncp=0, extrap=F)
}
\arguments{
  \item{x}{
    vector of x-coordinates of data points.
    Missing values are not accepted.
  }
  \item{y}{
    vector of y-coordinates of data points.
    Missing values are not accepted.
  }
  \item{z}{
    vector of z-coordinates of data points.
    Missing values are not accepted.
    
    \code{x}, \code{y}, and \code{z} must be the same length  and may contain no fewer
    than four points. The points of \code{x} and \code{y}
    cannot be collinear, i.e, they cannot fall on the same line (two vectors
    \code{x} and \code{y} such that \code{y = ax + b} for some \code{a}, \code{b} will not be
    accepted). 
  }
  \item{xo}{
    vector of x-coordinates of points at which to evaluate the interpolating
    function.}
  \item{yo}{
    vector of y-coordinates of points at which to evaluate the interpolating
    function.}
  
\item{ncp}{
  number of additional points to be used in computing partial
  derivatives at each data point.  
  \code{ncp} must be either \code{0} (partial derivatives are not used, =
  linear interpolation), or at
  least 2 but smaller than the number of data points (and smaller than 25).
}
\item{extrap}{
  logical flag: should extrapolation be used outside of the
  convex hull determined by the data points?}
\item{duplicate}{
  indicates how to handle duplicate data points. Possible values are
  \code{"error"} - produces an error message, \code{"strip"} - remove
  duplicate z values, \code{"mean"},\code{"median"},\code{"user"}  -
  calculate  mean , median or user defined function of duplicate z
  values.
}
\item{dupfun}{this function is applied to duplicate points if \code{duplicate="user"} 
} 
}
\value{
  list with 3 components:
  
  \item{x}{
    vector of x-coordinates of output points, the same as the input
    argument \code{xo}.
  }
  \item{y}{
    vector of y-coordinates of output points, the same as the input
    argument \code{yo}.
  }
  \item{z}{
    fitted z-values.  The value \code{z[i]} is computed
    at the x,y point \code{x[i], y[i]}.
}
}
\section{NOTE}{
  Use \code{interp} if interpolation on a regular grid is wanted.

  The two versions  \code{interpp.old}  and \code{interpp.new} refer to
  Akimas Fortran code from 1978 and 1996 resp. At the moment
  \code{interpp.new}
  does not work porperly (it results in a segmentation fault), so it is
  not used from the call wrapper  \code{interp}.
}
\description{
  If \code{ncp} is zero, linear
  interpolation is used in the triangles bounded by data points.  
  Cubic interpolation is done if partial derivatives are used.
  If \code{extrap} is \code{FALSE}, z-values for points outside the convex hull are
  returned as \code{NA}.  
  No extrapolation can be performed if \code{ncp} is zero.
  
   The \code{interpp} function handles duplicate \code{(x,y)} points                in different ways. As default it will stop with an error message. But
  it can give duplicate points an unique \code{z} value according to the
  parameter \code{duplicate} (\code{mean},\code{median} or any other user defined function).
 
  
  The triangulation scheme used by \code{interp} works well if \code{x} and \code{y} have
  similar scales but will appear stretched if they have very different
  scales.  The spreads of \code{x} and \code{y} must be within four orders of magnitude
  of each other for \code{interpp} to work.
}
\references{
  Akima, H. (1978). A Method of Bivariate Interpolation and
  Smooth Surface Fitting for Irregularly Distributed Data Points.
  ACM Transactions on Mathematical Software,
  \bold{4}, 148-164.

  Akima, H. (1996). Algorithm 761: scattered-data surface fitting that has
  the accuracy of a cubic polynomial.
  ACM Transactions on Mathematical Software,
  \bold{22}, 362-371.
}
\seealso{
  \code{\link[base]{contour}}, \code{\link[base]{image}}, \code{\link[base]{approx}}, \code{\link[base]{spline}}, \code{\link[base]{outer}}, \code{\link[base]{expand.grid}},\code{\link{interp}}.
}
\examples{
data(akima)
# linear interpolation at points (1,2), (5,6) and (10,12)
akima.lip<-interpp(akima$x, akima$y, akima$z,c(1,5,10),c(2,6,12))
}
\keyword{dplot}
