# package/configure.in
AC_PREREQ([2.69])
AC_INIT([akima],[0.6-3])
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
FC=`"${R_HOME}/bin/R" CMD config FC`

AC_PROG_CC
AC_PROG_FC


CHECK_SUN_COMPILER_OPTIMIZATION

AC_SUBST(PKG_CFLAGS)
AC_SUBST(PKG_FFLAGS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

