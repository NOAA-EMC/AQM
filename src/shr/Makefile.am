noinst_LIBRARIES = libshr.a

libshr_a_SOURCES = aqm_config_mod.F90 aqm_const_mod.F90 \
	aqm_domain_mod.F90 aqm_emis_mod.F90 aqm_fires_mod.F90 aqm_internal_mod.F90 \
	aqm_model_mod.F90 aqm_rc_mod.F90 aqm_methods.F90 aqm_species_mod.F90 \
	aqm_state_mod.F90 aqm_tools_mod.F90 aqm_types_mod.F90

libshr_a_CPPFLAGS = -DSUBST_CONST=\"CONST.EXT\" -DSUBST_FILES_ID=\"FILES_CTM.EXT\"

libshr_a_FCFLAGS  = -I $(top_builddir)/src/io/ioapi
libshr_a_FCFLAGS += -I $(top_builddir)/src/io/aqmio
libshr_a_FCFLAGS += -I $(top_builddir)/src/model
libshr_a_FCFLAGS += -I $(top_srcdir)/src/model/CMAQ/CCTM/src/ICL/fixed/const
libshr_a_FCFLAGS += -I $(top_srcdir)/src/model/CMAQ/CCTM/src/ICL/fixed/filenames

include $(ESMFMKFILE)

libshr_a_FCFLAGS += $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)


.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f $(libshr_a_SOURCES:.F90=.$(FC_MODEXT))

clean-local: clean-modules

libshr_a-aqm_config_mod.$(OBJEXT): libshr_a-aqm_rc_mod.$(OBJEXT) \
	libshr_a-aqm_species_mod.$(OBJEXT) libshr_a-aqm_types_mod.$(OBJEXT)
libshr_a-aqm_const_mod.$(OBJEXT): libshr_a-aqm_types_mod.$(OBJEXT)
libshr_a-aqm_domain_mod.$(OBJEXT): libshr_a-aqm_types_mod.$(OBJEXT)
libshr_a-aqm_emis_mod.$(OBJEXT): libshr_a-aqm_internal_mod.$(OBJEXT) libshr_a-aqm_model_mod.$(OBJEXT) \
	libshr_a-aqm_rc_mod.$(OBJEXT) libshr_a-aqm_tools_mod.$(OBJEXT) libshr_a-aqm_types_mod.$(OBJEXT)
libshr_a-aqm_fires_mod.$(OBJEXT): libshr_a-aqm_const_mod.$(OBJEXT) libshr_a-aqm_emis_mod.$(OBJEXT) \
	libshr_a-aqm_model_mod.$(OBJEXT) libshr_a-aqm_state_mod.$(OBJEXT) libshr_a-aqm_rc_mod.$(OBJEXT)
libshr_a-aqm_model_mod.$(OBJEXT): \
	libshr_a-aqm_config_mod.$(OBJEXT) libshr_a-aqm_domain_mod.$(OBJEXT) \
	libshr_a-aqm_rc_mod.$(OBJEXT) libshr_a-aqm_species_mod.$(OBJEXT) \
	libshr_a-aqm_state_mod.$(OBJEXT) libshr_a-aqm_types_mod.$(OBJEXT)
libshr_a-aqm_methods.$(OBJEXT): libshr_a-aqm_model_mod.$(OBJEXT) libshr_a-aqm_emis_mod.$(OBJEXT) \
	libshr_a-aqm_rc_mod.$(OBJEXT) libshr_a-aqm_types_mod.$(OBJEXT) \
	libshr_a-aqm_config_mod.$(OBJEXT) libshr_a-aqm_const_mod.$(OBJEXT)
libshr_a-aqm_state_mod.$(OBJEXT): libshr_a-aqm_types_mod.$(OBJEXT)
libshr_a-aqm_tools_mod.$(OBJEXT): libshr_a-aqm_types_mod.$(OBJEXT)
