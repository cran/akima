\name{interp}
\title{
  Gridded Bivariate Interpolation for Irregular Data
}
\alias{interp}
\alias{interp.new}
\alias{interp.old}
\usage{
interp(x, y, z, xo=seq(min(x), max(x), length = 40), yo=seq(min(y), max(y), length = 40), ncp=0, extrap=FALSE, duplicate = "error", dupfun = NULL)
interp.old(x, y, z, xo= seq(min(x), max(x), length = 40), yo=seq(min(y), max(y), length = 40), ncp=0, extrap=FALSE, duplicate = "error", dupfun = NULL)
interp.new(x, y, z, xo= seq(min(x), max(x), length = 40), yo=seq(min(y), max(y), length = 40), linear=FALSE, ncp=NULL, extrap=FALSE, duplicate = "error", dupfun = NULL)
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
    accepted). \code{interp} is meant for cases in which you have \code{x}, \code{y}
    values scattered over a plane and a \code{z} value for each.  If, instead,
    you are trying to evaluate a mathematical function, or get a graphical
    interpretation of relationships that can be described by a polynomial,
    try \code{outer()}.		
  }
  \item{xo}{
    vector of x-coordinates of output grid.  The default is 40 points
    evenly spaced over the range of \code{x}.  If extrapolation is not being
    used (\code{extrap=FALSE}, the default), \code{xo} should have a range that is
    close to or inside of the range of \code{x} for the results to be meaningful.
  }
  \item{yo}{
    vector of y-coordinates of output grid.  The default is 40 points
    evenly spaced over the range of \code{y}.  If extrapolation is not being
    used (\code{extrap=FALSE}, the default), \code{yo} should have a range that is
    close to or inside of the range of \code{y} for the results to be meaningful.
  }
  \item{linear}{logical, switch to linear interpolation in \code{interp.new}}
  \item{ncp}{
    number of additional points to be used in computing partial
    derivatives at each data point.  
    \code{ncp} must be either \code{0} (partial derivatives are not used), or at
    least 2 but smaller than the number of data points (and smaller than
    25). This option is only supported by \code{interp.old}.
  }
  \item{extrap}{
    logical flag: should extrapolation be used outside of the
    convex hull determined by the data points?
}
\item{duplicate}{
  indicates how to handle duplicate data points. Possible values are
  \code{"error"} - produces an error message, \code{"strip"} - remove
  duplicate z values, \code{"mean"},\code{"median"},\code{"user"}  -
  calculate  mean , median or user defined function of duplicate z
  values.}
\item{dupfun}{this function is applied to duplicate points if \code{duplicate="user"}         
}

}
\value{
  list with 3 components:
  
  \item{x}{
    vector of x-coordinates of output grid, the same as the input
    argument \code{xo}, if present.  Otherwise, a vector 40 points evenly spaced
    over the range of the input \code{x}.
  }
  \item{y}{
    vector of y-coordinates of output grid, the same as the input
    argument \code{yo}, if present.  Otherwise, a vector 40 points evenly spaced
    over the range of the input \code{x}.
  }
  \item{z}{
    matrix of fitted z-values.  The value \code{z[i,j]} is computed
    at the x,y point \code{x[i], y[j]}. \code{z} has
    dimensions \code{length(x)} times \code{length(y)} (\code{length(xo)} times \code{length(yo)}).
    
    
}}
\note{
  \code{interp} is a wrapper for the two versions \code{interp.old} (it
  uses (almost) the same Fortran code from Akima 1978 as the S-Plus version) and
  \code{interp.new}
  (it is based on new Fortran code from Akima 1996). For linear
  interpolation the old version is choosen, but spline interpolation is
  done by the new version.
  
  At the moment \code{interp.new} ignores \code{ncp} and does only
  bicubic spline interpolation.

  The resulting structure is suitable for input to the
  functions \code{contour} and \code{image}.  Check the requirements of
  these functions when choosing values  for \code{xo} and \code{yo}.
}
\description{
  If \code{ncp} is zero, linear
  interpolation is used in the triangles bounded by data points.  
  Cubic interpolation is done if partial derivatives are used.
  If \code{extrap} is \code{FALSE}, z-values for points outside the
  convex hull are returned as \code{NA}.  
  No extrapolation can be performed if \code{ncp} is zero.
  
  
  The \code{interp} function handles duplicate \code{(x,y)} points  
  in different ways. As default it will stop with an error message. But
  it can give duplicate points an unique \code{z} value according to the 
  parameter \code{duplicate} (\code{mean},\code{median} or any other
  user defined function).

  The triangulation scheme used by \code{interp} works well if \code{x}
  and \code{y} have
  similar scales but will appear stretched if they have very different
  scales.  The spreads of \code{x} and \code{y} must be within four
  orders of magnitude of each other for \code{interp} to work.
}
\references{
  Akima, H. (1978). A Method of Bivariate Interpolation and
  Smooth Surface Fitting for Irregularly Distributed Data Points.
  ACM Transactions on Mathematical Software,
  \bold{4}, 148-164.

  Akima, H. (1996). Algorithm 761: scattered-data surface fitting that has
  the accuracy of a cubic polynomial.
  ACM Transactions on Mathematical Software,
  \bold{22}, 362--371

}
\seealso{
  \code{\link[base]{contour}}, \code{\link[base]{image}}, \code{\link[base]{approx}}, \code{\link[base]{spline}}, \code{\link[base]{outer}}, \code{\link[base]{expand.grid}}.
}
\examples{
data(akima)
# linear interpolation
akima.li <- interp(akima$x, akima$y, akima$z)
image(akima.li$x,akima.li$y,akima.li$z)
contour(akima.li$x,akima.li$y,akima.li$z,add=TRUE)
points(akima$x,akima$y)

# increase smoothness
akima.smooth <- interp(akima$x, akima$y, akima$z,
      xo=seq(0,25, length=100),  yo=seq(0,20, length=100))
image(akima.smooth$x,akima.smooth$y,akima.smooth$z)
contour(akima.smooth$x,akima.smooth$y,akima.smooth$z,add=TRUE)
points(akima$x,akima$y)
# use triangulation library to
# show underlying triangulation:
if(library(tripack, logical.return=TRUE))
  plot(tri.mesh(akima),add=TRUE,lty="dashed")

# use only 15 points (interpolation only within convex hull!)
akima.part <- interp(akima$x[1:15],akima$y[1:15],akima$z[1:15])
image(akima.part$x,akima.part$y,akima.part$z)
contour(akima.part$x,akima.part$y,akima.part$z,add=TRUE)
points(akima$x[1:15],akima$y[1:15])

# spline interpolation, use 5 points to calculate derivatives
# interp gives `linear interpolation not yet implemented with interp.new()'
akima.spl <- interp.old(akima$x, akima$y, akima$z,
      xo=seq(0,25, length=100),  yo=seq(0,20, length=100),ncp=5)
image(akima.spl$x,akima.spl$y,akima.spl$z)
contour(akima.spl$x,akima.spl$y,akima.spl$z,add=TRUE)
points(akima$x,akima$y)

# example with duplicate points
data(airquality)
air <- airquality[(!is.na(airquality$Temp) & 
                   !is.na(airquality$Ozone) & 
                   !is.na(airquality$Solar.R)),]
# gives an error:
\dontrun{air.ip <- interp(air$Temp,air$Solar.R,air$Ozone)}
# use mean of duplicate points:
air.ip <- interp(air$Temp,air$Solar.R,air$Ozone,duplicate="mean")
}
\keyword{dplot}
% Converted by Sd2Rd version 0.2-a3.
