SUBDIRS = io shr CMAQ drv .

cmaq_dir = $(builddir)/CMAQ
cmaq_objects = \
	$(cmaq_dir)/CCTM/src/aero/aero6/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/biog/beis3/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/cloud/acm_ae6/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/depv/m3dry/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/emis/emis/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/gas/ebi_cb6r3_ae6_aq/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/grid/cartesian/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/init/yamo/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/MECHS/cb6r3_ae6_aq/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/phot/inline/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/plrise/smoke/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/procan/pa/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/spcs/cgrid_spcs_nml/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/STENEX/noop/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/util/util/libCCTM_a-*.$(OBJEXT) \
	$(cmaq_dir)/CCTM/src/vdiff/acm2/libCCTM_a-*.$(OBJEXT)

include $(ESMFMKFILE)

lib_LIBRARIES = libaqm.a

libaqm_a_SOURCES = aqm_cap.F90 aqm_comp_mod.F90

libaqm_a_FCFLAGS  = -I shr -I drv -I io/ioapi -I CMAQ/
libaqm_a_FCFLAGS += $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)

libaqm_a_LIBADD = drv/libdrv_a-*.$(OBJEXT) shr/libshr_a-*.$(OBJEXT) io/ioapi/*.$(OBJEXT) io/aqmio/*.$(OBJEXT) $(cmaq_objects)

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f aqm.$(FC_MODEXT) aqm_comp_mod.$(FC_MODEXT)

clean-local: clean-modules


libaqm_a-aqm_cap.$(OBJEXT) : libaqm_a-aqm_comp_mod.$(OBJEXT)
libaqm_a-aqm_comp_mod.$(OBJEXT) : $(builddir)/shr/libshr.a
