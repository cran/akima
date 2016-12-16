\name{bilinear}
\alias{bilinear}
\title{
  Bilinear Interpolation for Data on a Rectangular grid
}
\description{
  This is an implementation of a bilinear interpolating function.

  For a point (x0,y0) contained in a rectangle (x1,y1),(x2,y1),
  (x2,y2),(x1,y2) and x1<x2, y1<y2, the first step is to get z()
  at locations (x0,y1) and (x0,y2) as convex linear combinations
  z(x0,y*)=a*z(x1,y*)+(1-a)*z(x2,y*) where a=(x2-x1)/(x0-x1) for
  y*=y1,y2. In a second step z(x0,y0) is calculated as convex linear
  combination between z(x0,y1) and z(x0,y2) as
  z(x0,y1)=b*z(x0,y1)+(1-b)*z(x0,y2)  where b=(y2-y1)/(y0-y1).

  Finally, z(x0,y0) is a convex linear combination of the z values at
  the corners of the containing  rectangle with weights according to
  the distance from (x0,y0) to these corners.

  The grid lines can be unevenly spaced.

}
\usage{
bilinear(x, y, z, x0, y0)
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
\value{
  This function produces a list of interpolated points:
  \item{x}{vector of \code{x} coordinates.}
  \item{y}{vector of \code{y} coordinates.}
  \item{z}{vector of interpolated data \code{z}.}
  If you need an output grid, see \code{\link{bilinear.grid}}.
}
\references{
  Pascal Getreuer,
  Linear Methods for Image Interpolation,
  Image Processing On Line, 2011,
  http://www.ipol.im/pub/art/2011/g_lmii/article.pdf
}
\note{
  Use \code{\link{interpp}} for the general case of irregular gridded data!
}

\seealso{
  \code{\link{interp}}, \code{\link{bilinear.grid}}, \code{\link{bicubic.grid}}
}
\examples{
data(akima760)
# interpolate at the diagonal of the grid [0,8]x[0,10]
akima.bil <- bilinear(akima760$x,akima760$y,akima760$z,
                     seq(0,8,length=50), seq(0,10,length=50))
plot(sqrt(akima.bil$x^2+akima.bil$y^2), akima.bil$z, type="l")
}

\keyword{ dplot }

