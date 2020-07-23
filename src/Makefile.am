SUBDIRS = io shr model drv .

include $(ESMFMKFILE)

lib_LIBRARIES = libaqm.a

libaqm_a_SOURCES = aqm_cap.F90 aqm_comp_mod.F90

libaqm_a_FCFLAGS  = -I $(builddir)/shr -I $(builddir)/drv -I $(builddir)/io/aqmio -I $(builddir)/io/ioapi -I $(builddir)/model
libaqm_a_FCFLAGS += $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)

libaqm_a_LIBADD  = $(builddir)/model/CMAQ/CCTM/src/*/*/libCCTM_a-*.$(OBJEXT) $(builddir)/model/src/libCCTM_a-*.$(OBJEXT)
libaqm_a_LIBADD += $(builddir)/drv/libdrv_a-*.$(OBJEXT) $(builddir)/shr/libshr_a-*.$(OBJEXT)
libaqm_a_LIBADD += $(builddir)/io/ioapi/*.$(OBJEXT) $(builddir)/io/aqmio/*.$(OBJEXT)

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f aqm.$(FC_MODEXT) aqm_comp_mod.$(FC_MODEXT)

clean-local: clean-modules


libaqm_a-aqm_cap.$(OBJEXT) : libaqm_a-aqm_comp_mod.$(OBJEXT)
libaqm_a-aqm_comp_mod.$(OBJEXT) : $(builddir)/shr/libshr.a
