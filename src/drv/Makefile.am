noinst_LIBRARIES = libdrv.a

libdrv_a_SOURCES = cmaq_model_mod.F90 cmaq_mod.F90

libdrv_a_FCFLAGS  = -I $(builddir)/../shr
libdrv_a_FCFLAGS += -I $(builddir)/../CMAQ/CCTM/src
libdrv_a_FCFLAGS += -I $(builddir)/../CMAQ/shr

libdrv_a_DEPENDENCIES = $(builddir)/../shr/libshr.a

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f $(libdrv_a_SOURCES:.F90=.$(FC_MODEXT))

clean-local: clean-modules

libdrv_a-cmaq_model_mod.$(OBJEXT): libdrv_a-cmaq_mod.$(OBJEXT)
