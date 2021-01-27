noinst_LIBRARIES = libdrv.a

libdrv_a_SOURCES = cmaq_model_mod.F90 cmaq_mod.F90

libdrv_a_CPPFLAGS = -Dverbose_driver

libdrv_a_FCFLAGS  = -I $(top_builddir)/src/shr
libdrv_a_FCFLAGS += -I $(top_builddir)/src/model
libdrv_a_FCFLAGS += -I $(top_builddir)/src/io/ioapi
libdrv_a_FCFLAGS += -I $(top_builddir)/src/io/aqmio

libdrv_a_DEPENDENCIES = $(top_builddir)/src/shr/libshr.a

include $(ESMFMKFILE)

libdrv_a_FCFLAGS += $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f $(libdrv_a_SOURCES:.F90=.$(FC_MODEXT))

clean-local: clean-modules

libdrv_a-cmaq_model_mod.$(OBJEXT): libdrv_a-cmaq_mod.$(OBJEXT)
