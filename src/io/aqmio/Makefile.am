include $(ESMFMKFILE)

noinst_LIBRARIES = libaqmio.a

libaqmio_a_SOURCES  = aqmio.F90

libaqmio_a_FFLAGS   = $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)
libaqmio_a_FCFLAGS  = $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f aqmio.$(FC_MODEXT)

clean-local: clean-modules
