bilinear <- function(x,y,z,x0,y0){
  nx <- length(x)
  ny <- length(y)
  if(dim(z)[1]!=nx)
    stop("dim(z)[1] and length of x differs!")
  if(dim(z)[2]!=ny)
    stop("dim(z)[2] and length of y differs!")
  n0 <- length(x0)
  if(length(y0)!=n0)
    stop("length of y0 and x0 differs!")

  ret <- .Fortran("biliip",
                  as.double(x0),
                  as.double(y0),
                  z0=double(n0),
                  as.integer(n0),
                  as.double(x),
                  as.double(y),
                  as.double(z),
                  as.integer(nx),
                  as.integer(ny),
                  ier=integer(1),
                  PACKAGE="akima")
  if(ret$ier==1)
      stop("duplicate coordinates in input grid!")
  else
      list(x=x0,y=y0,z=ret$z0)
}


bilinear.grid <- function(x,y,z,xlim=c(min(x),max(x)),ylim=c(min(y),max(y)),
                         nx=40,ny=40,dx=NULL,dy=NULL){
  Nx <- length(x)
  Ny <- length(y)
  if(dim(z)[1]!=Nx)
    stop("dim(z)[1] and length of x differs!")
  if(dim(z)[2]!=Ny)
    stop("dim(z)[2] and length of y differs!")
  if(!is.null(dx)){
      xi <- seq(xlim[1],xlim[2],by=dx)
      nx <- length(xi)
  } else {
      xi <- seq(ylim[1],ylim[2],length=nx)
  }

  if(!is.null(dx)){
      yi <- seq(ylim[1],ylim[2],by=dy)
      ny <- length(yi)
  } else {
      yi <- seq(ylim[1],ylim[2],length=ny)
  }
  xmat <- matrix(rep(xi,ny),nrow=ny,ncol=nx,byrow=TRUE)
  ymat <- matrix(rep(yi,nx),nrow=ny,ncol=nx,byrow=FALSE)

  xy <- cbind(c(xmat),c(ymat))

  n0 <- nx*ny

  ret <- bilinear(x,y,z,xy[,1],xy[,2])

 # return cell boundaries
  list(x=xi,y=yi,z=t(matrix(ret$z,nrow=ny,ncol=nx,byrow=F)))
}
