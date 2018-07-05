noinst_LIBRARIES = libutil.a

# libutil_a_SOURCES = dt2str.f getefile.F get_env_mod.f90 hhmmss.f mmddyy.F mydata.F \
# 	nextime.F sec2time.f secsdiff.F time2sec.f unload_cgrid.F upcase.f yr2day.F
libutil_a_SOURCES = dt2str.f getefile.F get_env_mod.f90 hhmmss.f mmddyy.F mydata.F \
	nextime.F sec2time.f secsdiff.F time2sec.f upcase.f yr2day.F


libutil_a_FFLAGS = -I .
libutil_a_FCFLAGS =

libutil_a-getefile.$(OBJEXT) : libutil_a-get_env_mod.$(OBJEXT)

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f get_env_module.$(FC_MODEXT) mydata_module.$(FC_MODEXT)

clean-local: clean-modules
