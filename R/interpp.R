"interpp"<-function(x, y=NULL, z, xo, yo=NULL, linear = TRUE, extrap = FALSE,
                    duplicate = "error", dupfun = NULL,
                    jitter = 10^-12, jitter.iter = 6, jitter.random = FALSE)
{

    ## handle sp data, save coordinate and value names
    is.sp <- FALSE
    sp.coord <- NULL
    sp.z <- NULL
    sp.proj4string <- NULL
    if(is.null(y)&&is.character(z)){
        if(class(xo)=="SpatialPointsDataFrame"){
            yo <- coordinates(xo)[,2]
            xo <- coordinates(xo)[,1]
        } else
            stop(paste("either x,y,z,xo,yo have to be numeric vectors",
                       "or both x and xo have to be SpatialPointsDataFrames",
                       "and z a name of a data column in x"))
        if(class(x)=="SpatialPointsDataFrame"){
            sp.coord <- dimnames(coordinates(x))[[2]]
            sp.z <- z
            sp.proj4string <- x@proj4string
            z <- x@data[,z]
            y <- coordinates(x)[,2]
            x <- coordinates(x)[,1]
            is.sp <- TRUE
        } else
            stop(paste("either x,y,z,xo,yo have to be numeric vectors",
                       "or both x and xo have to be SpatialPointsDataFrames",
                       "and z a name of a data column in x"))
    }
    if(!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
        stop("missing values and Infs not allowed")
    if(is.null(xo))
        stop("xo missing")
    if(is.null(yo))
        stop("yo missing")



    drx <- diff(range(x))
    dry <- diff(range(y))
    if(drx == 0 || dry == 0)
        stop("all data collinear")	# other cases caught in Fortran code
    if(drx/dry > 10000 || drx/dry < 0.0001)
        stop("scales of x and y are too dissimilar")
    n <- length(x)
    np <- length(xo)
    if(length(yo)!=np)
        stop("length of xo and yo differ")
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
                       user = dupfun(x)
                       )
            }
            if(duplicate!="strip"){
                z <- unlist(lapply(split(z,i), centre))
                ord <- !duplicated(xy)
                x <- x[ord]
                y <- y[ord]
                n <- length(x)
            }
            else{
                ord <- (hist(i,plot=FALSE,freq=TRUE,
                             breaks=seq(0.5,max(i)+0.5,1))$counts==1)
                x <- x[ord]
                y <- y[ord]
                z <- z[ord]
                n <- length(x)
            }
        }
    else
        if(any(duplicated(xy)))
            stop("duplicate data points")
    zo <- rep(0, np)
    miss <- !extrap	#if not extrapolating use missing values
    extrap <- rep(extrap, np)

    ans <- .Fortran("sdbi3p",
                    md = as.integer(1),
                    ndp = as.integer(n),
                    xd = as.double(x),
                    yd = as.double(y),
                    zd = as.double(z),
                    nx = as.integer(np),
                    x = as.double(xo),
                    y = as.double(yo),
                    z = as.double(zo),
                    ier = integer(1),
                    wk = double(17 * n),
                    iwk = integer(25 * n),
                    extrap = as.logical(extrap),
                    near = integer(n),
                    nxt = integer(n),
                    dist = double(n),
                    linear = as.logical(linear),
                    PACKAGE = "akima")
    if(miss)
        ans$z[ans$extrap]<-NA
    ## Error code 10 from sdsf3p indicates error code -2 from trmesh:
    ## first three points collinear.
    ## Try to add jitter to data locations to avoid collinearities,
    ## start with diff(range(x))*jitter*jitter.trials^1.5 and repeat for
    ## jitter.trials steps until success (ier=0)

    if(ans$ier==10)
        warning("collinear points, trying to add some jitter to avoid collinearities!")
    jitter.trials <- 1
    success <- FALSE
    while(jitter.trials<jitter.iter & !success){
        if(jitter.random){
            ## dont use random jitter for reproducabilty by default:
            xj <- x+rnorm(n,0,diff(range(x))*jitter*jitter.trials^1.5)
            yj <- y+rnorm(n,0,diff(range(y))*jitter*jitter.trials^1.5)
        } else {
            xj <- x+rep(c(-1,0,1),length.out=length(x))*diff(range(x))*jitter*jitter.trials^1.5
            yj <- y+rep(c(0,1,-1),length.out=length(y))*diff(range(y))*jitter*jitter.trials^1.5
        }
        ans <- .Fortran("sdbi3p",
                        md = as.integer(1),
                        ndp = as.integer(n),
                        xd = as.double(xj),
                        yd = as.double(yj),
                        zd = as.double(z),
                        nx = as.integer(np),
                        x = as.double(xo),
                        y = as.double(yo),
                        z = as.double(zo),
                        ier = integer(1),
                        wk = double(17 * n),
                        iwk = integer(25 * n),
                        extrap = as.logical(extrap),
                        near = integer(n),
                        nxt = integer(n),
                        dist = double(n),
                        linear = as.logical(linear),
                        PACKAGE = "akima")
        if(miss)
            ans$z[ans$extrap] <- NA
        if(linear)
            ans$z[ans$extrap] <- NA

        success <- (ans$ier==0)
        if(success)
            warning("success: collinearities reduced through jitter")
        jitter.trials <- jitter.trials+1
    }
    if(is.sp){
        nona <- !is.na(ans$z)
        ret <- data.frame(ans$x[nona],ans$y[nona],ans$z[nona])
        names(ret) <- c(sp.coord[1],sp.coord[2],sp.z)
        coordinates(ret) <- sp.coord
        ret@proj4string <- sp.proj4string
    } else {
        ret <- list(x=ans$x,y=ans$y,z=ans$z)
    }
    ret
}
