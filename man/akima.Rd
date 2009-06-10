\name{akima}
\alias{akima}
\title{
Waveform Distortion Data for Bivariate Interpolation
}
\description{
\code{akima} is a list with components \code{x}, \code{y} and \code{z} which
represents a smooth surface of \code{z} values at selected points
irregularly distributed in the \code{x-y} plane.

The data was taken from a study of waveform distortion in
electronic circuits, described in:
Hiroshi Akima, "A Method of Bivariate Interpolation and
Smooth Surface Fitting Based on Local Procedures", 
CACM,
Vol. 17, No. 1, January 1974, pp. 18-20.
}
\references{
Hiroshi Akima, "A Method of Bivariate Interpolation and
Smooth Surface Fitting for Irregularly Distributed Data
Points", 
ACM Transactions on Mathematical Software,
Vol. 4, No. 2, June 1978, pp. 148-159.
Copyright 1978, Association for Computing Machinery, Inc.,
reprinted by permission.
}
\examples{
\dontrun{
library(rgl)
data(akima)
# data
rgl.spheres(akima$x,akima$z , akima$y,0.5,color="red")
rgl.bbox()
# bivariate linear interpolation
# interp:
akima.li <- interp(akima$x, akima$y, akima$z, 
                   xo=seq(min(akima$x), max(akima$x), length = 100),
                   yo=seq(min(akima$y), max(akima$y), length = 100))
# interp surface:
rgl.surface(akima.li$x,akima.li$y,akima.li$z,color="green",alpha=c(0.5))
# interpp:
akima.p <- interpp(akima$x, akima$y, akima$z,
                    runif(200,min(akima$x),max(akima$x)),
                    runif(200,min(akima$y),max(akima$y)))
# interpp points:
rgl.points(akima.p$x,akima.p$z , akima.p$y,size=4,color="yellow")

# bivariate spline interpolation
# data
rgl.spheres(akima$x,akima$z , akima$y,0.5,color="red")
rgl.bbox()
# bivariate cubic spline interpolation
# interp:
akima.si <- interp(akima$x, akima$y, akima$z, 
                   xo=seq(min(akima$x), max(akima$x), length = 100),
                   yo=seq(min(akima$y), max(akima$y), length = 100),
                   linear = FALSE, extrap = TRUE)
# interp surface:
rgl.surface(akima.si$x,akima.si$y,akima.si$z,color="green",alpha=c(0.5))
# interpp:
akima.sp <- interpp(akima$x, akima$y, akima$z,
                    runif(200,min(akima$x),max(akima$x)),
                    runif(200,min(akima$y),max(akima$y)),
                   linear = FALSE, extrap = TRUE)
# interpp points:
rgl.points(akima.sp$x,akima.sp$z , akima.sp$y,size=4,color="yellow")

}
}
\keyword{datasets}
% Converted by Sd2Rd version 0.2-a3.
