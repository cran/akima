"interp"<-function(x, y, z, xo = seq(min(x), max(x), length = 40),
                   yo = seq(min(y), max(y), length = 40), 
                   ncp = 0, extrap = FALSE, duplicate = "error", dupfun = NULL)
{
 if (ncp==0)
   # use the old version for linear interpolation
   interp.old(x, y, z, xo, yo, ncp, extrap, duplicate, dupfun)
 else
   interp.new(x, y, z, xo, yo, ncp, extrap, duplicate, dupfun)
}
