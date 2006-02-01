\name{interp}
\title{Gridded Bivariate Interpolation for Irregular Data}
\alias{interp}
\alias{interp.new}
\alias{interp.old}
\description{
  These functions implement bivariate interpolation onto a grid
  for irregularly spaced input data.  Bilinear or bicubic spline
  interpolation is applied using different versions of algorithms from
  Akima.
}
\usage{
interp(x, y, z, xo=seq(min(x), max(x), length = 40),
       yo=seq(min(y), max(y), length = 40),
       linear = TRUE, extrap=FALSE, duplicate = "error", dupfun = NULL, ncp = NULL)
interp.old(x, y, z, xo= seq(min(x), max(x), length = 40),
           yo=seq(min(y), max(y), length = 40), ncp = 0,
           extrap=FALSE, duplicate = "error", dupfun = NULL)
interp.new(x, y, z, xo = seq(min(x), max(x), length = 40),
           yo = seq(min(y), max(y), length = 40), linear = FALSE,
            ncp = NULL, extrap=FALSE, duplicate = "error", dupfun = NULL)
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

    \code{x}, \code{y}, and \code{z} must be the same length and may
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

    meaning was:
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
}
\note{
  \code{interp} is a wrapper for the two versions \code{interp.old} (it
  uses (almost) the same Fortran code from Akima 1978 as the S-Plus version) and
  \code{interp.new}
  (it is based on new Fortran code from Akima 1996). For linear
  interpolation the old version is choosen, but spline interpolation is
  done by the new version.

  Earlier versions (pre 0.5-1) of \code{interp} used the parameter
  \code{ncp} to choose between linear and cubic interpolation, this is now done
  by setting the logical parameter \code{linear}. Use of \code{ncp} is still
  possible, but is deprecated.

  The resulting structure is suitable for input to the
  functions \code{\link{contour}} and \code{\link{image}}.  Check
  the requirements of these functions when choosing values  for
  \code{xo} and \code{yo}.
}
\details{
  If \code{linear} is \code{TRUE} (default), linear
  interpolation is used in the triangles bounded by data points.
  Cubic interpolation is done if \code{linear} is set to \code{FALSE}.
  If \code{extrap} is \code{FALSE}, z-values for points outside the
  convex hull are returned as \code{NA}.
  No extrapolation can be performed for the linear case.

  The \code{interp} function handles duplicate \code{(x,y)} points
  in different ways. As default it will stop with an error message. But
  it can give duplicate points an unique \code{z} value according to the
  parameter \code{duplicate} (\code{mean},\code{median} or any other
  user defined function).

  The triangulation scheme used by \code{interp} works well if \code{x}
  and \code{y} have similar scales but will appear stretched if they have
  very different scales.  The spreads of \code{x} and \code{y} must be
  within four orders of magnitude of each other for \code{interp} to work.
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
\examples{
data(akima)
plot(y ~ x, data = akima, main = "akima example data")
with(akima, text(x, y, formatC(z,dig=2), adj = -0.1))

## linear interpolation
akima.li <- interp(akima$x, akima$y, akima$z)
image  (akima.li, add=TRUE)
contour(akima.li, add=TRUE)
points (akima, pch = 3)

## increase smoothness (using finer grid):
akima.smooth <-
 with(akima, interp(x, y, z, xo=seq(0,25, length=100),
                    yo=seq(0,20, length=100)))
image  (akima.smooth, main = "interp(<akima data>, *) on finer grid")
contour(akima.smooth, add = TRUE, col = "thistle")
points(akima, pch = 3, cex = 2, col = "blue")
# use triangulation package to show underlying triangulation:
if(library(tripack, logical.return=TRUE))
   plot(tri.mesh(akima), add=TRUE, lty="dashed")

# use only 15 points (interpolation only within convex hull!)
akima.part <- with(akima, interp(x[1:15], y[1:15], z[1:15]))
image(akima.part)
title("interp() on subset of only 15 points")
contour(akima.part, add=TRUE)
points(akima$x[1:15],akima$y[1:15], col = "blue")

## spline interpolation, two variants (AMS 526 "Old", AMS 761 "New")
## -----------------------------------------------------------------
## "Old": use 5 points to calculate derivatives -> many NAs
akima.sO <- interp.old(akima$x, akima$y, akima$z,
           xo=seq(0,25, length=100),  yo=seq(0,20, length=100), ncp=5)
table(is.na(akima.sO$z)) ## 3990 NA's; = 40 \%
akima.sO <- with(akima,
   interp.old(x,y,z, xo=seq(0,25, length=100), yo=seq(0,20, len=100), ncp = 4))
sum(is.na(akima.sO$z)) ## still 3429
image  (akima.sO, main = "interp.old(*, ncp = 4) [almost useless]")
contour(akima.sO, add = TRUE)

## "New:"
akima.spl <- with(akima, interp.new(x,y,z, xo=seq(0,25, length=100),
                                           yo=seq(0,20, length=100)))
## equivalent call via setting linear=FALSE in interp():
akima.spl <- with(akima, interp(x,y,z, xo=seq(0,25, length=100),
                                       yo=seq(0,20, length=100),
                                linear=FALSE))

contour(akima.spl, main = "smooth  interp(*, linear = FALSE)")
points(akima)

full.pal <- function(n) hcl(h = seq(340, 20, length = n))
cool.pal <- function(n) hcl(h = seq(120, 0, length = n) + 150)
warm.pal <- function(n) hcl(h = seq(120, 0, length = n) - 30)

filled.contour(akima.spl, color.palette = full.pal,
        plot.axes = { axis(1); axis(2);
                      title("smooth  interp(*, linear = FALSE)");
                      points(akima, pch = 3, col= hcl(c=100, l = 20))})
# no extrapolation!

## example with duplicate points :

data(airquality)
air <- subset(airquality,
              !is.na(Temp) & !is.na(Ozone) & !is.na(Solar.R))
# gives an error {duplicate ..}:
try( air.ip <- interp(air$Temp,air$Solar.R,air$Ozone, linear=FALSE) )
# use mean of duplicate points:
air.ip <- with(air, interp(Temp, Solar.R, log(Ozone), duplicate = "mean",
                           linear = FALSE))
image(air.ip, main = "Airquality: Ozone vs. Temp and Solar.R")
with(air, points(Temp, Solar.R))
}
\keyword{dplot}

