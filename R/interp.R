interp <-
    function(x, y=NULL, z,
             xo = seq(min(x), max(x), length = nx),
             yo = seq(min(y), max(y), length = ny), linear = TRUE,
             extrap = FALSE, duplicate = "error", dupfun = NULL,
             nx=40, ny=40,
             jitter = 10^-12, jitter.iter = 6, jitter.random = FALSE)
{
    ## handle sp data, save coordinate and value names
    is.sp <- FALSE
    sp.coord <- NULL
    sp.z <- NULL
    sp.proj4string <- NULL
    if(is.null(y)&&is.character(z)){
        if(class(x)=="SpatialPointsDataFrame"){
            sp.coord <- dimnames(coordinates(x))[[2]]
            sp.z <- z
            sp.proj4string <- x@proj4string
            z <- x@data[,z]
            y <- coordinates(x)[,2]
            x <- coordinates(x)[,1]
            is.sp <- TRUE
            xo = seq(min(x), max(x), length = nx)
            yo = seq(min(y), max(y), length = ny)
        } else
            stop("either x,y,z are numerical or x is SpatialPointsDataFrame and z a name of a data column in x")
    }

    if(!(all(is.finite(x)) && all(is.finite(y)) && all(is.finite(z))))
        stop("missing values and Infs not allowed")

    drx <- diff(range(x))
    dry <- diff(range(y))
    if(drx == 0 || dry == 0)
        stop("all data collinear")    # other cases caught in Fortran code
    if(drx/dry > 10000 || drx/dry < 0.0001)
        stop("scales of x and y are too dissimilar")
    n <- length(x)
    nx <- length(xo)
    ny <- length(yo)
    if(length(y) != n || length(z) != n)
        stop("Lengths of x, y, and z do not match")

    xy <- paste(x, y, sep = ",")# trick for 'duplicated' (x,y)-pairs
    if(duplicate == "error") {
        if(any(duplicated(xy)))
            stop("duplicate data points: need to set 'duplicate = ..' ")
    }
    else { ## duplicate != "error"

        i <- match(xy, xy)
        if(duplicate == "user")
            dupfun <- match.fun(dupfun)#> error if it fails

        ord <- !duplicated(xy)
        if(duplicate != "strip") {
            centre <- function(x)
                switch(duplicate,
                       mean = mean(x),
                       median = median(x),
                       user = dupfun(x))
            z <- unlist(lapply(split(z,i), centre))
        } else {
            z <- z[ord]
        }
        x <- x[ord]
        y <- y[ord]
        n <- length(x)
    }

    zo <- matrix(0, nx, ny)
    storage.mode(zo) <- "double"
    miss <- !extrap             # if not extrapolating, set missing values

    ans <- .Fortran("sdsf3p",
                    md = as.integer(1),
                    ndp = as.integer(n),
                    xd = as.double(x),
                    yd = as.double(y),
                    zd = as.double(z),
                    nx = as.integer(nx),
                    x = as.double(xo),
                    ny=as.integer(ny),
                    y = as.double(yo),
                    z = zo,
                    ier = integer(1),
                    wk = double(36 * n),
                    iwk = integer(25 * n),
                    extrap = as.logical(matrix(extrap,nx,ny)),
                    near = integer(n),
                    nxt = integer(n),
                    dist = double(n),
                    linear = as.logical(linear),
                    PACKAGE = "akima")
    if(miss)
        ans$z[ans$extrap] <- NA

    ## Error code 10 from sdsf3p indicates error code -2 from trmesh:
    ## first three points collinear.
    ## Try to add jitter to data locations to avoid collinearities,
    ## start with diff(range(x))*jitter*jitter.trials^1.5 and repeat for
    ## jitter.trials steps until success (ier=0)

    if(ans$ier==10){
        warning("collinear points, trying to add some jitter to avoid colinearities!")
        jitter.trials <- 1
        success <- FALSE
        while(jitter.trials<jitter.iter & !success){
            ## dont use random jitter for reproducabilty by default,
            ## same as in tri.mesh in package tripack:
            if(jitter.random){
                ## the randomness is only contained in a random shift
                ## of a regular -1,+/-0,+1,-1,+/-0,+1,... scheme
                ## determining the fixed amounts of jitter to be added
                ## to the x and y values separately.
                ## Really random jitter like this
                ## xj <- x+rnorm(n,0,diff(range(x))*jitter*jitter.trials^1.5)
                ## yj <- y+rnorm(n,0,diff(range(y))*jitter*jitter.trials^1.5)
                ## still triggers spurious errors in the triangulation.
                ## j <- sample(1:length(x), length(x))
                ## xj <- x[j]
                ## yj <- y[j]
                ## but it needs afterwards back ordering!
                j <- list()
                j[[1]] <- rep(c(-1,0,1),length.out=length(x))
                j[[2]] <- rep(c(0,1,-1),length.out=length(x))
                j[[3]] <- rep(c(1,-1,0),length.out=length(x))
                jx <- sample(1:3,1)
                jy <- sample(1:3,1)
                xj <- x+j[[jx]]*diff(range(x))*jitter*jitter.trials^1.5
                yj <- y+j[[jy]]*diff(range(y))*jitter*jitter.trials^1.5
            } else {
                xj <- x+rep(c(-1,0,1),length.out=length(x))*diff(range(x))*jitter*jitter.trials^1.5
                yj <- y+rep(c(0,1,-1),length.out=length(y))*diff(range(y))*jitter*jitter.trials^1.5
            }
            ans <- .Fortran("sdsf3p",
                            as.integer(1),
                            as.integer(n),
                            xd=as.double(xj),
                            yd=as.double(yj),
                            as.double(z),
                            as.integer(nx),
                            x = as.double(xo),
                            as.integer(ny),
                            y = as.double(yo),
                            z = zo,
                            ier = integer(1),
                            double(36 * n),
                            integer(25 * n),
                            extrap = as.logical(matrix(extrap,nx,ny)),
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
    }
    ## prepare return value
    if(is.sp){
        zm <- dim(ans$z)[1]
        zn <- dim(ans$z)[2]
        zvec <- c(ans$z)
        xvec <- c(matrix(rep(ans$x,zn),nrow=zm,ncol=zn,byrow=FALSE))
        yvec <- c(matrix(rep(ans$y,zm),nrow=zm,ncol=zn,byrow=TRUE))
        nona <- !is.na(zvec)
        ret <- data.frame(xvec[nona],yvec[nona],zvec[nona])
        names(ret) <- c(sp.coord[1],sp.coord[2],sp.z)
        coordinates(ret) <- sp.coord
        ret@proj4string <- sp.proj4string
        gridded(ret) <- TRUE
    } else {
        ret <- list(x=ans$x,y=ans$y,z=ans$z)
    }
    ret
}
