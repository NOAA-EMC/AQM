include $(ESMFMKFILE)

noinst_LIBRARIES = libioapi.a

libioapi_a_SOURCES  = FDESC3.EXT PARMS3.EXT
libioapi_a_SOURCES += crlf.F currec.f currstep.f dt2str.f findc.f getefile.F index1.f julian.F junit.F hhmmss.f mmddyy.F nextime.F \
	poly.f promptmfile.f sec2time.f secsdiff.F setlam.f sortic.f str2real.f time2sec.f upcase.f wkday.F yr2day.F
libioapi_a_SOURCES += m3exit.F90 m3mesg.F90 m3msg2.F90 m3warn.F90 m3utilio.F90

libioapi_a_FFLAGS   = $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)

libioapi_a_FCFLAGS  = $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)
libioapi_a_FCFLAGS += -DSUBST_FILES_ID=\"FILES_CTM.EXT\"
libioapi_a_FCFLAGS += -I $(top_srcdir)/src/model/CMAQ/CCTM/src/ICL/fixed/filenames

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f m3utilio.$(FC_MODEXT)

clean-local: clean-modules

libioapi_a-currec.$(OBJEXT) : libioapi_a-nextime.$(OBJEXT)
libioapi_a-currstep.$(OBJEXT) : libioapi_a-currec.$(OBJEXT)
libioapi_a-dt2str.$(OBJEXT) : libioapi_a-hhmmss.$(OBJEXT) libioapi_a-mmddyy.$(OBJEXT) libioapi_a-nextime.$(OBJEXT)
libioapi_a-getefile.$(OBJEXT) : libioapi_a-m3utilio.$(OBJEXT) libioapi_a-junit.$(OBJEXT)
libioapi_a-m3utilio.$(OBJEXT) : $(top_srcdir)/src/model/CMAQ/CCTM/src/ICL/fixed/filenames/FILES_CTM.EXT
libioapi_a-nextime.$(OBJEXT) : libioapi_a-time2sec.$(OBJEXT)
