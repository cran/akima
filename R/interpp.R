"interpp"<-function(x, y, z, xo, yo, ncp = 0, extrap = FALSE,
                    duplicate = "error", dupfun = NULL)
{
  # interpp.new has some bugs at the moment (segfaults), so use
  # the old Akima code:
  interpp.old(x, y, z, xo, yo, ncp, extrap, duplicate, dupfun)
}
