interp.new <-
  function(x, y, z,
           xo = seq(min(x), max(x), length = 40),
           yo = seq(min(y), max(y), length = 40), linear = FALSE,
           ncp = NULL, extrap = FALSE, duplicate = "error", dupfun = NULL)
{
    warning("interp.new() is deprecated, use interp()")
    interp(x, y, z,
           xo,
           yo, linear,
           extrap, duplicate, dupfun)
}
