"interp.old"<-function(x, y, z, xo = seq(min(x), max(x), length = 40),
                   yo = seq(min(y), max(y), length = 40), 
                   ncp = 0, extrap = FALSE, duplicate = "error", dupfun = NULL)
{
  if(!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
    stop("missing values and Infs not allowed")
  if(ncp>25){
    ncp <- 25
    cat("ncp too large, using ncp=25\n")
  }
  drx <- diff(range(x))
  dry <- diff(range(y))
  if(drx == 0 || dry == 0)
    stop("all data collinear")	# other cases caught in Fortran code
  if(drx/dry > 10000 || drx/dry < 0.0001)
    stop("scales of x and y are too dissimilar")
  n <- length(x)
  nx <- length(xo)
  ny <- length(yo)
  if(length(y) != n || length(z) != n)
    stop("Lengths of x, y, and z do not match")
  xy <- paste(x, y, sep =",")
  i <- match(xy, xy)
  if(duplicate=="user" && !is.function(dupfun))
    stop("duplicate=\"user\" requires dupfun to be set to a function")
  if(duplicate!="error")
    {
      centre <- function(x) {
        switch(duplicate,
               mean = mean(x),
               median = median(x),
               user = dupfun(x))
      }
      if(duplicate!="strip"){
        z <- unlist(lapply(split(z,i), centre))
        ord <- !duplicated(xy)
        x <- x[ord]
        y <- y[ord]
        n <- length(x)
      }
      else{
        ord <- (hist(i,plot=F,freq=T,breaks=seq(0.5,max(i)+0.5,1))$counts==1)
        x <- x[ord]
        y <- y[ord]
        z <- z[ord]
        n <- length(x)
      }
    }
  else
    if(any(duplicated(xy)))
      stop("duplicate data points")
  zo <- matrix(0, nx, ny)
  storage.mode(zo) <- "double"
  miss <- !extrap	#if not extrapolating use missing values
  misso <- matrix(miss, nx, ny)
  if(extrap & ncp == 0)
    warning("Cannot extrapolate with linear option")
  ans <- .Fortran("idsfft",
                  as.integer(1),
                  as.integer(ncp),
                  as.integer(n),
                  as.double(x),
                  as.double(y),
                  as.double(z),
                  as.integer(nx),
                  as.integer(ny),
                  x = as.double(xo),
                  y = as.double(yo),
                  z = zo,
                  integer((31 + ncp) * n + nx * ny),
                  double(5 * n),
                  misso = as.logical(misso))
  temp <- ans[c("x", "y", "z", "misso")]
  temp$z[temp$misso]<-NA
  temp[c("x", "y", "z")]
}
