"interpp"<-function(x, y, z, xo, yo, linear = TRUE, extrap = FALSE,
                    duplicate = "error", dupfun = NULL, ncp=NULL)
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
                                        # the old Akima code:
    interpp.old(x, y, z, xo, yo, ncp=0, extrap, duplicate, dupfun)
  else
                                        # new code for splines
    interpp.new(x, y, z, xo, yo, extrap, duplicate, dupfun)
}
