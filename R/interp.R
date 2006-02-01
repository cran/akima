interp <-
  function(x, y, z,
	   xo = seq(min(x), max(x), length = 40),
	   yo = seq(min(y), max(y), length = 40), linear=TRUE,
	   extrap = FALSE, duplicate = "error", dupfun = NULL, ncp=NULL)
{

  # for backward compatibility
  if(!is.null(ncp)){
    warning('use of \'ncp\' parameter is deprecated!')
    if(ncp==0)
      linear <- TRUE
    else if(ncp>0)
      linear <- FALSE
    else
      stop('ncp < 0 ?')
  }
  if(linear)
    ## use the old version for linear interpolation
    interp.old(x, y, z, xo = xo, yo = yo, ncp = 0,
	       extrap = extrap, duplicate = duplicate, dupfun = dupfun)
  else ## use the new one
    interp.new(x, y, z, xo = xo, yo = yo, linear = FALSE,
	       ncp = NULL,# not using 'ncp' argument
	       extrap = extrap, duplicate = duplicate, dupfun = dupfun)
}
