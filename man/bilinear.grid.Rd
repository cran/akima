\name{bilinear.grid}
\alias{bilinear.grid}
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
bilinear.grid(x,y,z,xlim=c(min(x),max(x)),ylim=c(min(y),max(y)),
                         nx=40,ny=40,dx=NULL,dy=NULL)
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
  \item{nx}{
    output grid dimension in \code{x} direction.
    }
  \item{ny}{
    output grid dimension in \code{y} direction.
    }
  \item{dx}{
    output grid spacing in \code{x} direction, not used by default,
  overrides \code{nx} if specified.
    }
  \item{dy}{
    output grid spacing in \code{y} direction, not used by default,
    overrides \code{ny} if specified..
    }
}
\value{
  This function produces a grid of interpolated points, feasible to be
  used directly with \code{\link{image}} and \code{\link{contour}}:
  \item{x}{vector of \code{x} coordinates of the output grid.}
  \item{y}{vector of \code{y} coordinates of the output grid.}
  \item{z}{matrix of interpolated data for the output grid.}
}
\references{
  Pascal Getreuer,
  Linear Methods for Image Interpolation,
  Image Processing On Line, 2011,
  http://www.ipol.im/pub/art/2011/g_lmii/article.pdf
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
akima.bil <- bilinear.grid(akima760$x,akima760$y,akima760$z)
zmin <- min(akima.bil$z, na.rm=TRUE)
zmax <- max(akima.bil$z, na.rm=TRUE)
breaks <- pretty(c(zmin,zmax),10)
colors <- heat.colors(length(breaks)-1)
image(akima.bil, breaks=breaks, col=colors)
contour(akima.bil, levels=breaks, add=TRUE)
}
\keyword{ dplot }

