SUBDIRS = CMAQ shr drv .

cmaq_dir = $(builddir)/CMAQ
include CMAQ/cmaq.mk

include $(ESMFMKFILE)

lib_LIBRARIES = libaqm.a

libaqm_a_SOURCES = aqm_cap.F90 aqm_comp_mod.F90

libaqm_a_FCFLAGS  = -I shr -I drv
libaqm_a_FCFLAGS += $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)

libaqm_a_LIBADD = drv/libdrv_a-*.$(OBJEXT) shr/libshr_a-*.$(OBJEXT) $(cmaq_objects)

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f aqm.$(FC_MODEXT) aqm_comp_mod.$(FC_MODEXT)

clean-local: clean-modules


libaqm_a-aqm_cap.$(OBJEXT) : libaqm_a-aqm_comp_mod.$(OBJEXT)
libaqm_a-aqm_comp_mod.$(OBJEXT) : $(builddir)/shr/libshr.a
