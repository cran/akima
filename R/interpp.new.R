"interpp.new"<-function(x, y, z, xo, yo, extrap = FALSE,
                    duplicate = "error", dupfun = NULL)
{
    warning("interp.new() is deprecated, use interp()")
    interpp(x, y, z,
            xo,
            yo, linear=FALSE,
            extrap=extrap, duplicate=duplicate, dupfun=dupfun)
}
