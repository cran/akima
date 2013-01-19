# package/configure.in
AC_PREREQ(2.50)
AC_INIT([akima],[0.5-10])
AC_CONFIG_SRCDIR(src/tripack.f)
#if you want to use more macros
m4_include([tools/acinclude.m4])

if test -z "${R_HOME}" ; then
 R_HOME=`R RHOME`
fi

if test -z "${R_HOME}" ; then
  echo "Cannot find the R distribution!"
  exit 1 
fi

CC=`"${R_HOME}/bin/R" CMD config CC`
F77=`"${R_HOME}/bin/R" CMD config F77`

AC_PROG_CC
AC_PROG_F77


CHECK_SUN_COMPILER_OPTIMIZATION

AC_SUBST(PKG_CFLAGS)
AC_SUBST(PKG_FFLAGS)

AC_OUTPUT(src/Makevars)

