include $(ESMFMKFILE)

noinst_LIBRARIES = libioapi.a

libioapi_a_SOURCES  = dt2str.f getefile.F index1.f junit.F hhmmss.f mmddyy.F nextime.F sec2time.f secsdiff.F time2sec.f upcase.f yr2day.F
libioapi_a_SOURCES += m3utilio.F90

libioapi_a_FFLAGS   = $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)

libioapi_a_FCFLAGS  = $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)
libioapi_a_FCFLAGS += -DSUBST_FILES_ID=\"FILES_CTM.EXT\"
libioapi_a_FCFLAGS += -I $(srcdir)/../../CMAQ/CCTM/src/ICL/fixed/filenames

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f m3utilio.$(FC_MODEXT)

clean-local: clean-modules

libioapi_a-dt2str.$(OBJEXT) : libioapi_a-hhmmss.$(OBJEXT) libioapi_a-mmddyy.$(OBJEXT) libioapi_a-nextime.$(OBJEXT)
libioapi_a-getefile.$(OBJEXT) : libioapi_a-m3utilio.$(OBJEXT) libioapi_a-junit.$(OBJEXT)
libioapi_a-m3utilio.$(OBJEXT) : $(srcdir)/../../CMAQ/CCTM/src/ICL/fixed/filenames/FILES_CTM.EXT
libioapi_a-nextime.$(OBJEXT) : libioapi_a-time2sec.$(OBJEXT)
