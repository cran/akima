"interp.old"<-function(x, y, z, xo = seq(min(x), max(x), length = 40),
                   yo = seq(min(y), max(y), length = 40),
                   ncp = 0, extrap = FALSE, duplicate = "error", dupfun = NULL)
{
    warning("interp.old() is deprecated, use interp()")
    interp(x, y, z,
           xo,
           yo, linear=(ncp==0),
           extrap, duplicate, dupfun)
}
