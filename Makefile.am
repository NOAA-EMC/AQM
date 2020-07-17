SUBDIRS = src
dist_doc_DATA = README
nodist_data_DATA = aqm.mk
nodist_include_HEADERS = src/aqm.$(FC_MODEXT)

EXTRA_DIST = examples

ACLOCAL_AMFLAGS = -I m4

aqm.mk:
	@echo "# ESMF self-describing build dependency makefile fragment" > $(@)
	@echo >> $(@)
	@echo "ESMF_DEP_FRONT     = aqm"                      >> $(@)
	@echo "ESMF_DEP_INCPATH   = $(includedir)"            >> $(@)
	@echo "ESMF_DEP_CMPL_OBJS = "                         >> $(@)
	@echo "ESMF_DEP_LINK_OBJS = $(libdir)/libaqm.a"       >> $(@)
	@echo "ESMF_DEP_SHRD_PATH = $(AQM_DEP_SHRD_PATH)"     >> $(@)
	@echo "ESMF_DEP_SHRD_LIBS = $(AQM_DEP_SHRD_LIBS)"     >> $(@)
	@echo
	@echo "Finished generating ESMF self-describing build dependency makefile fragment: $(@)"
	@echo

uninstall-local: 
	-rm -f $(datarootdir)/aqm.mk

CLEANFILES = aqm.mk
