"interpp.old"<-function(x, y, z, xo, yo, ncp = 0, extrap = FALSE,
                    duplicate = "error", dupfun = NULL)
{
    warning("interpp.old() is deprecated, use interpp()")
    interpp(x, y, z,
            xo,
            yo, linear=(ncp==0),
            extrap=extrap, duplicate=duplicate, dupfun=dupfun)
}
