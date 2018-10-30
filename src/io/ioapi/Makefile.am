include $(ESMFMKFILE)

noinst_LIBRARIES = libioapi.a

libioapi_a_SOURCES  = dt2str.f index1.f nextime.F sec2time.f time2sec.f yr2day.F
libioapi_a_SOURCES += m3utilio.F90

libioapi_a_FCFLAGS  = $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f m3utilio.$(FC_MODEXT)

clean-local: clean-modules

dt2str.$(OBJEXT) : nextime.$(OBJEXT)
nextime.$(OBJEXT) : time2sec.$(OBJEXT)
