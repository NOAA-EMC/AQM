noinst_LIBRARIES = libshr.a

libshr_a_SOURCES = cmaq_shr_mod.F90 cmaq_types_mod.F90

libshr_a_FCFLAGS =
# libshr_a_FCFLAGS = -I $(builddir)/../CCTM/src

# libshr_a_DEPENDENCIES = $(builddir)/../CCTM/src/libCCTM.a

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f $(libshr_a_SOURCES:.F90=.$(FC_MODEXT))

clean-local: clean-modules

libshr_a-cmaq_shr_mod.$(OBJEXT): libshr_a-cmaq_types_mod.$(OBJEXT)
