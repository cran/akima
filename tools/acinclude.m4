dnl acinclude.m4
dnl
dnl autoconf macro to check for NA related errors by compiling with -xO1
dnl on Sun Studio Compilers
dnl
AC_DEFUN([CHECK_SUN_COMPILER_OPTIMIZATION],[
  AC_CACHE_CHECK([for correct optimization with SUN compilers],
  r_cv_sun_opt5_$1,[
if test -z "${R_HOME}" ; then
  R_HOME=`R RHOME`
fi

PKG_CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
PKG_FFLAGS=`"${R_HOME}/bin/R" CMD config FFLAGS`


case "${host}" in
  x*-sun-solaris*) 
dnl use a hard coded call to src/tripack.f through R
dnl to determine wether it returns all NA. 
dnl then retry with -xO5
dnl 
dnl skip gcc:
    if test "${ac_cv_fc_compiler_gnu}" != yes; then
      cat >conftest.R <<EOT
check <- function(){
  dyn.load("src/tripack.so")
  x1 <- c(-1.17182301840432, -0.738152338408028, 0.0724389527039489, 0.605106093471307, -2.16072259908434)
  x2 <- c(0.76066459689394, 0.419761334644176, 1.04733588786577, 0.327582569290709, -3.35453228783072)
  n <- length(x1)
  ret <- .Fortran("trmesh",
                  as.integer(n),
                  x=as.double(x1),
                  y=as.double(x2),
                  tlist=integer(6*n-12),
                  tlptr=integer(6*n-12),
                  tlend=integer(n),
                  tlnew=as.integer(0),
                  tnear=integer(n),
                  tnext=integer(n),
                  tdist=double(n),
                  ier=as.integer(0))
  if(length(ret$tdist[[is.na(ret\$tdist)]])==5)
    cat("export ALL_NAS=yes\n", file="conftest.out")
  else
    cat("export ALL_NAS=no\n", file="conftest.out")
}
check()
EOT

      rm -f src/tripack.o
      rm -f src/tripack.so
      $R_HOME/bin/R CMD SHLIB src/tripack.f
     
      echo "source(\"conftest.R\")" | $R_HOME/bin/R --vanilla >/dev/null
      rm -f conftest.R
      rm -f src/tripack.o
      rm -f src/tripack.so
     
     
      eval "`cat ./conftest.out`"
      rm -f ./conftest.out
     
      if test -z "${ALL_NAS}" ; then
        echo "Failed to check for NAs in return values." 
        exit 1;
      else
        if test "${ALL_NAS}" = "yes"; then
          echo "NAs produced, default compiler optimization too high, trying to relax to -xO5"
          cat >conftest.R <<EOT
check <- function(){
  dyn.load("src/tripack.so")
  x1 <- c(-1.17182301840432, -0.738152338408028, 0.0724389527039489, 0.605106093471307, -2.16072259908434)
  x2 <- c(0.76066459689394, 0.419761334644176, 1.04733588786577, 0.327582569290709, -3.35453228783072)
  n <- length(x1)
  ret <- .Fortran("trmesh",
                  as.integer(n),
                  x=as.double(x1),
                  y=as.double(x2),
                  tlist=integer(6*n-12),
                  tlptr=integer(6*n-12),
                  tlend=integer(n),
                  tlnew=as.integer(0),
                  tnear=integer(n),
                  tnext=integer(n),
                  tdist=double(n),
                  ier=as.integer(0))
  if(length(ret$tdist[[is.na(ret\$tdist)]])==5)
    cat("export ALL_NAS=yes\n", file="conftest.out")
  else
    cat("export ALL_NAS=no\n", file="conftest.out")
}
check()
EOT

          rm -f src/tripack.o
          rm -f src/tripack.so
          MAKEFLAGS='CFLAGS=-xO5 FFLAGS=-O0' $R_HOME/bin/R CMD SHLIB src/tripack.f
         
          echo "source(\"conftest.R\")" | $R_HOME/bin/R --vanilla >/dev/null
          rm -f conftest.R
          rm -f src/tripack.o
          rm -f src/tripack.so
         
          eval "`cat ./conftest.out`"
          rm -f ./conftest.out
         
          if test "${ALL_NAS}" = "yes"; then
            echo "still NAs produced, giving up"
            exit 1
          else
            echo -n "will use -xO5 to avoid problems with NAs"
dnl strip out all -xO[1234] flags, add one -xO5 flag instead
            NEW_PKG_CFLAGS="-xO5 "
            for f in ${PKG_CFLAGS}; do
              case $f in
                  -xO[[1234]])
                break
                ;;
                *)
                  NEW_PKG_CFLAGS="${NEW_PKG_CFLAGS} $f"
                ;;
              esac
            done
            NEW_PKG_FFLAGS="-O0 "
            for f in ${PKG_FFLAGS}; do
              case $f in
                -xO[[1234]])
                  break
                ;;
                *)
                  NEW_PKG_FFLAGS="${NEW_PKG_FFLAGS} $f"
                ;;
              esac
            done
            PKG_CFLAGS=${NEW_PKG_CFLAGS}
            PKG_FFLAGS=${NEW_PKG_FFLAGS}
          fi
        fi
      fi
    fi
  ;;
*)
  echo -n "not running on x86*-solaris*"
  ;;
esac

  ]) # AC_CACHE_CHECK
]) # AC_DEFUN
