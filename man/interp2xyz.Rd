\name{interp2xyz}
\alias{interp2xyz}
\title{From interp() Result, Produce 3-column Matrix}
\description{
  From an \code{\link{interp}()} result, produce a 3-column matrix
  or \code{\link{data.frame}} \code{cbind(x, y, z)}.
}
\usage{
interp2xyz(al, data.frame = FALSE)
}
\arguments{
  \item{al}{a \code{\link{list}} as produced from \code{\link{interp}()}.}
  \item{data.frame}{logical indicating if result should be
    \link{data.frame} or matrix (default).}
}
\value{
  a matrix (or data.frame) with three columns, called
  \code{"x"}, \code{"y"}, \code{"z"}.
}
\author{
  Martin Maechler, Jan.18, 2013
}
\seealso{
  \code{\link{expand.grid}()} is the \dQuote{essential ingredient} of
  \code{interp2xyz()}.

  \code{\link{interp}}.
}
\examples{
data(akima)
ak.spl <- with(akima, interp(x, y, z, linear = FALSE,
                             xo= seq(0,25, length=100),
                             yo= seq(0,20, length= 96)))
str(ak.spl)# list (x[i], y[j], z = <matrix>[i,j])

## Now transform to simple  (x,y,z)  matrix / data.frame :
str(am <- interp2xyz(ak.spl))
str(ad <- interp2xyz(ak.spl, data.frame=TRUE))
## and they are the same:
stopifnot( am == ad | (is.na(am) & is.na(ad)) )
}
\keyword{manip}
