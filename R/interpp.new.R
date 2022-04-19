"interpp.new"<-function(x, y, z, xo, yo, extrap = FALSE,
                    duplicate = "error", dupfun = NULL)
{
    warning("interpp.new() is deprecated, use interpp()")
    interpp(x, y, z,
            xo,
            yo, linear=FALSE,
            extrap=extrap, duplicate=duplicate, dupfun=dupfun)
}
