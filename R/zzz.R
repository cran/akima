.First.lib <- function(lib, pkg) {
  library.dynam("akima", pkg, lib)
}

if(version$minor < "62")
  library.dynam("akima")
