\name{interpp}
\title{
  Pointwise Bivariate Interpolation for Irregular Data
}
\alias{interpp}
\usage{
interpp(x, y=NULL, z, xo, yo=NULL, linear=TRUE, extrap=FALSE,
        duplicate = "error", dupfun = NULL,
        jitter = 10^-12, jitter.iter = 6, jitter.random = FALSE,
        remove = !linear)
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
    contain no fewer than four points. The points of \code{x} and \code{y}
    cannot be collinear, i.e, they cannot fall on the same line (two vectors
    \code{x} and \code{y} such that \code{y = ax + b} for some \code{a},
    \code{b} will not be accepted).
  }
  \item{xo}{
    vector of x-coordinates of points at which to evaluate the interpolating
    function. If \code{x} is a \code{SpatialPointsDataFrame} this has
    also to be a \code{SpatialPointsDataFrame}.
  }
  \item{yo}{
    vector of y-coordinates of points at which to evaluate the interpolating
    function.

    If operating on \code{SpatialPointsDataFrame}s this is left as \code{NULL}
  }
  \item{linear}{logical -- indicating wether linear or spline
    interpolation should be used.
  }
  \item{extrap}{
    logical flag: should extrapolation be used outside of the
    convex hull determined by the data points? Not possible for linear
    interpolation.}
  \item{duplicate}{
    indicates how to handle duplicate data points. Possible values are
    \code{"error"} - produces an error message, \code{"strip"} - remove
    duplicate z values, \code{"mean"},\code{"median"},\code{"user"}  -
    calculate  mean , median or user defined function of duplicate z
    values.
  }
  \item{dupfun}{
    this function is applied to duplicate points if \code{duplicate="user"}
  }
  \item{jitter}{Jitter of amount of \code{diff(range(XX))*jitter} (XX=x
    or y) will be added to coordinates if collinear points are
    detected. Afterwards interpolation will be tried once again.

    Note that the jitter is not generated randomly unless
    \code{jitter.random} is set to \code{TRUE}. This ensures
    reproducable result. \code{\link[tripack]{tri.mesh}} of package
    \code{tripack} uses the same jitter mechanism. That means you can
    plot the triangulation on top of the interpolation and see the
    same triangulation as used for interpolation, see examples below.
  }
  \item{jitter.iter}{number of iterations to retry with jitter, amount
    will be increased in each iteration by \code{iter^1.5}}
  \item{jitter.random}{logical, see \code{jitter}, defaults to
    \code{FALSE}
  }
  \item{remove}{logical, indicates whether Akimas removal of thin triangles along
    the border of the convex hull should be performed, experimental setting!
    defaults to \code{!linear}, so it will be left out for linear interpolation
    by default. For some point configurations it is the only
    available option to skip this removal step.
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
  If input is \code{SpatialPointsDataFrame} than an according
    \code{SpatialPointsDataFrame} is returned.
}
\section{NOTE}{
  Use \code{interp} if interpolation on a regular grid is wanted.

  See \code{\link{interp}} for more details.
}
\description{
  These functions implement bivariate interpolation onto a set of points
  for irregularly spaced input data. These functions are only for
  backward compatibility, use \code{\link{interpp}} instead.

  If \code{linear} is \\code{TRUE}, linear
  interpolation is used in the triangles bounded by data points, otherwise
  cubic interpolation is done.

  If \code{extrap} is \code{FALSE}, z-values for points outside the
  convex hull are returned as \code{NA}. No extrapolation can be
  performed for linear interpolation.

  The \code{interpp} function handles duplicate \code{(x,y)} points in
  different ways. As default it will stop with an error message. But it
  can give duplicate points an unique \code{z} value according to the
  parameter \code{duplicate} (\code{mean},\code{median} or any other
  user defined function).

  The triangulation scheme used by \code{interp} works well if \code{x}
  and \code{y} have similar scales but will appear stretched if they
  have very different scales.  The spreads of \code{x} and \code{y} must
  be within four orders of magnitude of each other for \code{interpp} to
  work.
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

  R. J. Renka (1996). Algorithm 751: TRIPACK: a constrained
  two-dimensional Delaunay triangulation package.
  ACM Transactions on Mathematical Software.
  \bold{22}, 1-8.

  R. J. Renka and Ron Brown (1998). Remark on algorithm 761.
  ACM Transactions on Mathematical Software.
  \bold{24}, 383-385.
}
\seealso{
  \code{\link[graphics]{contour}}, \code{\link[graphics]{image}},
  \code{\link[stats]{approxfun}}, \code{\link[stats]{splinefun}},
  \code{\link[base]{outer}}, \code{\link[base]{expand.grid}},
  \code{\link{interp}}, \code{\link{aspline}}.
}
\examples{
data(akima)
# linear interpolation at points (1,2), (5,6) and (10,12)
akima.lip<-interpp(akima$x, akima$y, akima$z,c(1,5,10),c(2,6,12))
akima.lip$z
# spline interpolation at the same locations
akima.sip<-interpp(akima$x, akima$y, akima$z,c(1,5,10),c(2,6,12),
  linear=FALSE)
akima.sip$z
\dontrun{
    ## interaction with sp objects:
    library(sp)
    ## take 30 sample points out of meuse grid:
    data(meuse.grid)
    m0 <- meuse.grid[sample(1:3103,30),]
    coordinates(m0) <- ~x+y
    ## interpolate on this 30 points:
    ## note: both "meuse" and "m0" are sp objects
    ## (SpatialPointsDataFrame) !!
    ## arguments z and xo have to given, y has to be omitted!
    ipp <- interpp(meuse,z="zinc",xo=m0)
    spplot(ipp)
}
}
\keyword{dplot}
