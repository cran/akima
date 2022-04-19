\name{interp}
\title{Gridded Bivariate Interpolation for Irregular Data}
\alias{interp}
\description{
  These functions implement bivariate interpolation onto a grid
  for irregularly spaced input data.  Bilinear or bicubic spline
  interpolation is applied using different versions of algorithms from
  Akima.
}
\usage{
interp(x, y=NULL, z, xo=seq(min(x), max(x), length = nx),
       yo=seq(min(y), max(y), length = ny),
       linear = TRUE, extrap=FALSE, duplicate = "error", dupfun = NULL,
       nx = 40, ny = 40,
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
    contain no fewer than four points. The points of \code{x} and
    \code{y} should not be collinear, i.e, they should not fall on the same line
    (two vectors \code{x} and \code{y} such that \code{y = ax + b} for
    some \code{a}, \code{b} will not produce menaningful results). Some
    heuristics is built in to avoid this case by adding small jitter to
    \code{x} and \code{y} when the number of \code{NA} values in the
    result exceeds 10\%.


    \code{interp} is
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
    interpolation should be used.}
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
  \item{nx}{dimension of output grid in x direction}
  \item{ny}{dimension of output grid in y direction}
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
    will be multiplied in each iteration by \code{iter^1.5}}
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
  \code{interp} uses Akimas new Fortran code (ACM 761) from 1996 in the revised
  version by Renka from 1998 for spline interpolation, the triangulation
  (based on Renkas tripack) is reused for linear interpolation. In this
  newer version Akima switched from his own triangulation to Renkas
  tripack (ACM 751).

  Note that earlier versions (up to version 0.5-12) as well as S-Plus
  used the old Fortran code from Akima 1978 (ACM 526).

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

  R. J. Renka (1996). Algorithm 751: TRIPACK: a constrained
  two-dimensional Delaunay triangulation package.
  ACM Transactions on Mathematical Software.
  \bold{22}, 1-8.

  R. J. Renka and Ron Brown (1998). Remark on algorithm 761.
  ACM Transactions on Mathematical Software.
  \bold{24}, 383-385.
}
\seealso{
  \code{\link{contour}}, \code{\link{image}},
  \code{\link{approx}}, \code{\link{spline}},
  \code{\link{aspline}},
  \code{\link{outer}}, \code{\link{expand.grid}},
  \code{link{franke.data}}.
}
\examples{
data(akima)
plot(y ~ x, data = akima, main = "akima example data")
with(akima, text(x, y, formatC(z,dig=2), adj = -0.1))

## linear interpolation
akima.li <- interp(akima$x, akima$y, akima$z)
li.zmin <- min(akima.li$z,na.rm=TRUE)
li.zmax <- max(akima.li$z,na.rm=TRUE)
breaks <- pretty(c(li.zmin,li.zmax),10)
colors <- heat.colors(length(breaks)-1)
with(akima.li, image  (x,y,z, breaks=breaks, col=colors))
with(akima.li,contour(x,y,z, levels=breaks, add=TRUE))
points (akima, pch = 3)

## increase smoothness (using finer grid):
akima.smooth <-
    with(akima, interp(x, y, z, nx=100, ny=100))
si.zmin <- min(akima.smooth$z,na.rm=TRUE)
si.zmax <- max(akima.smooth$z,na.rm=TRUE)
breaks <- pretty(c(si.zmin,si.zmax),10)
colors <- heat.colors(length(breaks)-1)

image  (akima.smooth, main = "interp(<akima data>, *) on finer grid",
        breaks=breaks, col=colors)
contour(akima.smooth, add = TRUE, levels=breaks, col = "thistle")
points(akima, pch = 3, cex = 2, col = "blue")

## use triangulation package to show underlying triangulation:
\dontrun{
if(library(tripack, logical.return=TRUE))
   plot(tri.mesh(akima), add=TRUE, lty="dashed")
}
## use only 15 points (interpolation only within convex hull!)
akima.part <- with(akima, interp(x[1:15], y[1:15], z[1:15]))
p.zmin <- min(akima.part$z,na.rm=TRUE)
p.zmax <- max(akima.part$z,na.rm=TRUE)
breaks <- pretty(c(p.zmin,p.zmax),10)
colors <- heat.colors(length(breaks)-1)

image(akima.part, breaks=breaks, col=colors)
title("interp() on subset of only 15 points")
contour(akima.part, levels=breaks, add=TRUE)
points(akima$x[1:15],akima$y[1:15], col = "blue")

## spline interpolation
akima.spl <- with(akima, interp(x, y, z, nx=100, ny=100, linear=FALSE))

contour(akima.spl, main = "smooth  interp(*, linear = FALSE)")
points(akima)

full.pal <- function(n) hcl(h = seq(340, 20, length = n))
cool.pal <- function(n) hcl(h = seq(120, 0, length = n) + 150)
warm.pal <- function(n) hcl(h = seq(120, 0, length = n) - 30)

filled.contour(akima.spl, color.palette = full.pal,
        plot.axes = { axis(1); axis(2);
                      title("smooth  interp(*, linear = FALSE)");
                      points(akima, pch = 3, col= hcl(c=100, l = 20))})
## no extrapolation!

\dontrun{
    ## interp can handle spatial point dataframes created by the sp package:
    library(sp)
    data(meuse)
    coordinates(meuse) <- ~x+y
    ## argument z has to be named, y has to be omitted!
    z <- interp(meuse,z="zinc",nx=100,ny=150)
    spplot(z,"zinc")
    z <- interp(meuse,z="zinc",nx=100,ny=150,linear=FALSE)
    spplot(z,"zinc")
}

\dontrun{
### An example demonstrating the problems that occur for rectangular
### gridded data.
###
require(tripack)
### Create irregularly spaced sample data on even values of x and y
### (the "14" makes it irregular spacing).
x   <- c(seq(2,10,2),14)
nx  <- length(x)
y   <- c(seq(2,10,2),14)
ny  <- length(y)
nxy <- nx*ny
xy  <- expand.grid(x,y)
colnames(xy) <- c("x","y")
### prepare a dataframe for interp
df  <- cbind(xy,z=rnorm(nxy))
### and a matrix for bicubic and bilinear
z   <- matrix(df$z,nx,ny)

old.par <- par(mfrow=c(2,2))
### First: bicubic spline interpolation:
### This is Akimas bicubic spline implementation for regular gridded
### data:
iRbic <- bicubic.grid(x,y,z,nx=250,ny=250)
### Note that this interpolation tends to extreme values in large cells.
### Therefore zmin and zmax are taken from here to generate the same
### color scheme for the next plots.
zmin <- min(iRbic$z, na.rm=TRUE)
zmax <- max(iRbic$z, na.rm=TRUE)
breaks <- pretty(c(zmin,zmax),10)
colors <- heat.colors(length(breaks)-1)
image(iRbic,breaks=breaks,col = colors)
contour(iRbic,col="black",levels=breaks,add=TRUE)
points(xy$x,xy$y)
title(main="bicubic interpolation",
      xlab="bcubic.grid(...)",
      sub="Akimas regular grid version, ACM 760")

### Now Akima splines with accurracy of bicubic polynomial
### for irregular gridded data:
iRspl <- with(df,interp(x,y,z,linear=FALSE,nx=250,ny=250))
### Note that the triangulation is created by adding small amounts
### of jitter to the coordinates, resulting in an unique triangulation.
### This jitter is not randomly choosen to get reproducable results.
### tri.mesh() from package tripack uses the same code and so produces the
### same triangulation.
image(iRspl,breaks=breaks,col = colors)
contour(iRspl,col="black",levels=breaks,add=TRUE)
plot(tri.mesh(xy$x,xy$y),col="white",add=TRUE)
title(main="bicubic* interpolation",
      xlab="interp(...,linear=FALSE)",
      ylab="*: accuracy of bicubic polynomial"
      sub="Akimas irregular grid version, ACM 761")

### Just for comparison an implementation of bilinear interpolation,
### only applicable to regular gridded data:
iRbil <- bilinear.grid(x,y,z,nx=250,ny=250)
### Note the lack of differentiability at grid cell borders.
image(iRbil,breaks=breaks,col = colors)
contour(iRbil,col="black",levels=breaks,add=TRUE)
points(xy$x,xy$y)
title(main="bilinear interpolation",
      xlab="bilinear.grid(...)",
      sub="only works for regular grid")

### Linear interpolation using the same triangulation as
### Akima bicubic splines for irregular gridded data.
iRlin <- with(df,interp(x,y,z,linear=TRUE,nx=250,ny=250))
### Note how the triangulation influences the interpolation.
### For this rectangular gridded dataset the triangulation
### in each rectangle is arbitraryly choosen from two possible
### solutions, hence the interpolation would change drastically
### when the triangulation changes. For this reason interp()
### is not meant for regular (rectangular) gridded data!
image(iRlin,breaks=breaks,col = colors)
contour(iRlin,col="black",levels=breaks,add=TRUE)
plot(tri.mesh(xy$x,xy$y),col="white",add=TRUE)
title(main="linear interpolation",
      xlab="interp(...,linear=TRUE)",
      sub="same triangulation as Akima irregular grid")

### And now four times Akima 761 with random jitter for
### triangulation correction, note that now interp() and tri.mesh()
### need the same random seed to produce identical triangulations!
for(i in 1:4){
    set.seed(42+i)
    iRspl <- with(df,interp(x,y,z,linear=FALSE,nx=250,ny=250,jitter.random=TRUE))
    image(iRspl,breaks=breaks,col = colors)
    contour(iRspl,col="black",levels=breaks,add=TRUE)
    set.seed(42+i)
    plot(tri.mesh(xy$x,xy$y,jitter.random=TRUE),col="white",add=TRUE)
    title(main="bicubic* interpolation",
          xlab="interp(...,linear=FALSE)",
          ylab="random jitter added",
          sub="Akimas irregular grid version, ACM 761")
}
par(old.par)

}
### Use all datasets from Franke, 1979:
data(franke)
for(i in 1:5)
    for(j in 1:3){
        FR <- franke.data(i,j,franke)
        IL <- with(FR, interp(x,y,z,linear=FALSE))
        image(IL)
        contour(IL,add=TRUE)
        with(FR,points(x,y))
    }
}
\keyword{dplot}

