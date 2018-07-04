#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([AQM], [0.1], [raffaele.montuoro@noaa.gov])

AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([subdir-objects])

# Checks for programs.
AC_PROG_INSTALL
AC_PROG_RANLIB
AM_PROG_AR

AC_PROG_FC
AC_PROG_F77($FC)

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile
                 src/CMAQ/CCTM/Makefile
                 src/CMAQ/CCTM/src/Makefile
                 src/CMAQ/Makefile
                 src/CMAQ/UTIL/Makefile
                 src/CMAQ/util/Makefile
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
