#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([AQM],[0.1],[raffaele.montuoro@noaa.gov])

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([subdir-objects])

# Checks for programs.
AC_PROG_INSTALL
AC_PROG_RANLIB
AM_PROG_AR

AC_PROG_GREP
AC_PROG_SED
AC_PATH_PROG(CUT,cut)

AC_ARG_WITH([esmfmk],
  [AS_HELP_STRING([--with-esmfmk=<path-to-esmf.mk>],
    [set full path to makefile fragment from ESMF library.])],
  [],
  [with_esmfmk=no])

AS_IF([test "x$with_esmfmk" != xno],
  AC_CHECK_FILE(["$with_esmfmk"],
    [AC_SUBST(ESMFMKFILE,["$with_esmfmk"])
     with_esmfmk=yes],
    [AC_MSG_WARN([File $with_esmfmk not found. Carry on.])
     with_esmfmk=no]))

AS_IF([test "x$with_esmfmk" = xno],
  [AS_MESSAGE([checking environment for ESMFMKFILE...])
   AC_CHECK_FILE(["${ESMFMKFILE}"],
      [AC_SUBST(ESMFMKFILE,["${ESMFMKFILE}"])
       AC_MSG_RESULT([getting ESMF Makefile fragment from environment variable ESMFMKFILE])
       with_esmfmk=yes],
      [AC_MSG_WARN([Makefile fragment from ESMF library not found.])])])

AS_IF([test "x$with_esmfmk" = xyes],
  [FCOMPILER=`$GREP ESMF_F90COMPILER ${ESMFMKFILE} | $SED -e 's/ESMF_F90COMPILER *= *//g'`
#   for v in ESMF_F90COMPILEOPTS ESMF_F90COMPILEPATHS ESMF_F90COMPILECPPFLAGS
#   do
#     FCFLAGS="${FCFLAGS} `$GREP $v ${ESMFMKFILE} 2>/dev/null | $CUT -d= -f2-`"
#   done
   FCLIBS="${LDFLAGS}"
   for v in ESMF_F90LINKOPTS ESMF_F90LINKPATHS ESMF_F90LINKRPATHS ESMF_F90ESMFLINKLIBS
   do
     FCLIBS="${FCLIBS} `$GREP $v ${ESMFMKFILE} 2>/dev/null | $CUT -d= -f2-`"
   done
   AC_PROG_FC([$FCOMPILER])
   AC_SUBST([FCLIBS])],
   [AC_PROG_FC([mpiifort])
    FCFLAGS="${FCFLAGS} -O2"])

AS_IF([test "x$FC" = x],
  [AC_MSG_ERROR([Cannot find a suitable Fortran compiler.])],
  [AC_FC_MODULE_EXTENSION])

AC_PROG_F77($FC)

# Check for compiler vendor
AC_LANG_PUSH([Fortran])
ac_ext=F90
AX_COMPILER_VENDOR
AC_LANG_POP([Fortran])
case "x$ax_cv_fc_compiler_vendor" in
  "xintel")
     CCTM_FCFLAGS="-free -qoverride-limits"
     CCTM_FFLAGS="-fixed -132 -qoverride-limits"
     ;;
  "xgnu")
     CCTM_FCFLAGS="-ffree-line-length-none"
     CCTM_FFLAGS="-ffixed-line-length-132"
     ;;
  *)
     CCTM_FCFLAGS=
     CCTM_FFLAGS=
     ;;
esac
AC_SUBST([CCTM_FCFLAGS])
AC_SUBST([CCTM_FFLAGS])

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile
                 src/CMAQ/Makefile
                 src/shr/Makefile
                 src/drv/Makefile
                 src/io/Makefile
                 src/io/aqmio/Makefile
                 src/io/ioapi/Makefile
                 src/Makefile])
AC_OUTPUT

AS_ECHO("")
AS_ECHO("${PACKAGE_NAME} version ${PACKAGE_VERSION} successfully configured.")
AS_ECHO("")
AS_ECHO("Configuration settings:")
AS_ECHO("=======================")
AS_ECHO("  CC        = ${CC}")
AS_ECHO("  CFLAGS    = ${CFLAGS}")
AS_ECHO("")
AS_ECHO("  CPP       = ${CPP}")
AS_ECHO("  CPPFLAGS  = ${CPPFLAGS}")
AS_ECHO("")
AS_ECHO("  F77       = ${F77}")
AS_ECHO("  FFLAGS    = ${FFLAGS}")
AS_ECHO("")
AS_ECHO("  FC        = ${FC}")
AS_ECHO("  FCFLAGS   = ${FCFLAGS}")
AS_ECHO("")
AS_ECHO("  LDFLAGS   = ${LDFLAGS}")
AS_ECHO("  LIBS      = ${LIBS}")
AS_ECHO("  FCLIBS    = ${FCLIBS}")
AS_ECHO("=======================")
AS_IF([test "x$with_esmfmk" != xno],
  [AS_ECHO("NOTE: Settings imported from ESMF Makefile fragment: ${ESMFMKFILE}")],[])
AS_ECHO("")
AS_ECHO("Type 'make' to build.")
