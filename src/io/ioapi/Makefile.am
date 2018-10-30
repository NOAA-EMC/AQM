include $(ESMFMKFILE)

noinst_LIBRARIES = libioapi.a

libioapi_a_SOURCES  = dt2str.f getefile.F index1.f junit.F hhmmss.f mmddyy.F nextime.F sec2time.f secsdiff.F time2sec.f upcase.f yr2day.F
libioapi_a_SOURCES += m3utilio.F90

libioapi_a_FCFLAGS  = $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f m3utilio.$(FC_MODEXT)

clean-local: clean-modules

dt2str.$(OBJEXT) : hhmmss.$(OBJEXT) mmddyy.$(OBJEXT) nextime.$(OBJEXT)
getefile.$(OBJEXT) : libioapi_a-m3utilio.$(OBJEXT) junit.$(OBJEXT)
nextime.$(OBJEXT) : time2sec.$(OBJEXT)
