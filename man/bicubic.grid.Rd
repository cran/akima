\name{bicubic.grid}
\alias{bicubic.grid}
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
bicubic.grid(x,y,z,xlim,ylim,dx,dy)
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
  \item{xlim}{
    vector of length 2 giving lower and upper limit for range \code{x}
    coordinates used for  output grid.
  }
  \item{ylim}{
    vector of length 2 giving lower and upper limit for range of \code{y}
    coordinates used for  output grid.
  }
  \item{dx}{
    output grid spacing in \code{x} direction.
    }
  \item{dy}{
    output grid spacing in \code{y} direction.
    }
}
\details{
  This functiuon is a R interface to Akima's Rectangular-Grid-Data
  Fitting algorithm (TOMS 760). The algorithm has the accuracy of a bicubic
  (bivariate third-degree) polynomial.
}
\value{
  This function produces a grid of interpolated points, feasible to be
  used directly with \code{\link{image}} and \code{\link{contour}}:
  \item{x}{vector of \code{x} coordinates of the output grid.}
  \item{y}{vector of \code{y} coordinates of the output grid.}
  \item{z}{matrix of interpolated data for the output grid.}
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
  \code{\link{interp}}, \code{\link{bicubic}}
  % maybe later:
  % \code{\link[rgeostat]{bilinear}}
}
\examples{
data(akima760)
# interpolate at a grid [0,8]x[0,10]
akima.bic <- bicubic.grid(akima760$x,akima760$y,akima760$z,
                          c(0,8),c(0,10),0.1,0.1)
image(akima.bic)
contour(akima.bic, add=TRUE)
}
\keyword{ dplot }

