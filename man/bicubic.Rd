\name{bicubic}
\alias{bicubic}
\title{
  Bivariate Interpolation for Data on a Rectangular grid
}
\description{
 The description in the Fortran code says:
  
 This subroutine performs interpolation of a bivariate function,
 z(x,y), on a rectangular grid in the x-y plane.  It is based on
 the revised Akima method.

 In this subroutine, the interpolating function is a piecewise
 function composed of a set of bicubic (bivariate third-degree)
 polynomials, each applicable to a rectangle of the input grid
 in the x-y plane.  Each polynomial is determined locally.

 This subroutine has the accuracy of a bicubic polynomial, i.e.,
 it interpolates accurately when all data points lie on a
 surface of a bicubic polynomial.

 The grid lines can be unevenly spaced.

}
\usage{
bicubic(x, y, z, x0, y0)
}

\arguments{
  \item{x}{
    a vector containing the \code{x} coordinates of the rectangular data grid.
  }
  \item{y}{
    a vector containing the \code{y} coordinates of the rectangular data grid.
  }
  \item{z}{
    a matrix containing the \code{z[i,j]} data values for the grid points (\code{x[i]},\code{y[j]}).
  }
  \item{x0}{
    vector of \code{x} coordinates used to interpolate at.
  }
  \item{y0}{
    vector of \code{y} coordinates used to interpolate at.
  }
}
\details{
  This functiuon is a R interface to Akima's Rectangular-Grid-Data
  Fitting algorithm (TOMS 760). The algorithm has the accuracy of a bicubic
  (bivariate third-degree) polynomial.
}
\value{
  This function produces a list of interpolated points:
  \item{x}{vector of \code{x} coordinates.}
  \item{y}{vector of \code{y} coordinates.}
  \item{z}{vector of interpolated data \code{z}.}
  If you need an output grid, see \code{\link{bicubic.grid}}.
}
\references{
  Akima, H. (1996) Rectangular-Grid-Data
  Surface Fitting that Has the Accuracy of a
  Bicubic Polynomial,
  J. ACM \bold{22}(3), 357-361
}
\note{
  Use \code{\link{interp}} for the general case of irregular gridded data!
}

\seealso{
  \code{\link{interp}}, \code{\link{bicubic.grid}}
  % maybe later:
  % \code{\link[rgeostat]{bilinear}}
}
\examples{
data(akima760)
# interpolate at the diagonal of the grid [0,8]x[0,10]
akima.bic <- bicubic(akima760$x,akima760$y,akima760$z,
                     seq(0,8,length=50), seq(0,10,length=50))
plot(sqrt(akima.bic$x^2+akima.bic$y^2), akima.bic$z, type="l")


##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, y, z, x0, y0) 
{
    nx <- length(x)
    ny <- length(y)
    if (dim(z)[1] != nx) 
        stop("dim(z)[1] and length of x differs!")
    if (dim(z)[2] != ny) 
        stop("dim(z)[2] and length of y differs!")
    n0 <- length(x0)
    if (length(y0) != n0) 
        stop("length of y0 and x0 differs!")
    ret <- .Fortran("rgbi3p", md = as.integer(1), nxd = as.integer(nx), 
        nyd = as.integer(ny), xd = as.double(x), yd = as.double(y), 
        zd = as.double(z), nip = as.integer(n0), xi = as.double(x0), 
        yi = as.double(y0), zi = double(n0), ier = integer(1), 
        wk = double(3 * nx * ny), PACKAGE = "akima")
    list(x = x0, y = y0, z = ret$zi)
  }
}

\keyword{ dplot }

