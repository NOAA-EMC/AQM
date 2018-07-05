SUBDIRS = CMAQ shr .

cmaq_dir = $(builddir)/CMAQ
include CMAQ/cmaq.mk

include $(ESMFMKFILE)

lib_LIBRARIES = libaqm.a

libaqm_a_SOURCES = aqm_cap.F90 aqm_methods.F90

libaqm_a_FCFLAGS  = -I shr
libaqm_a_FCFLAGS += $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)

libaqm_a_LIBADD = shr/libshr_a-*.$(OBJEXT) $(cmaq_objects)

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f aqm.$(FC_MODEXT) aqm_methods.$(FC_MODEXT)

clean-local: clean-modules


libaqm_a-aqm_cap.$(OBJEXT) : libaqm_a-aqm_methods.$(OBJEXT)
libaqm_a-aqm_methods.$(OBJEXT) : $(builddir)/shr/libshr.a
