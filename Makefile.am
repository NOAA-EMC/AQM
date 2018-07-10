SUBDIRS = src
dist_doc_DATA = README
nodist_data_DATA = aqm.mk

ACLOCAL_AMFLAGS = -I m4

aqm.mk:
	@echo "# ESMF self-describing build dependency makefile fragment" > $(@)
	@echo >> $(@)
	@echo "ESMF_DEP_FRONT     = AQM"                      >> $(@)
	@echo "ESMF_DEP_INCPATH   = $(abs_top_builddir)/src/" >> $(@)
	@echo "ESMF_DEP_CMPL_OBJS = "                         >> $(@)
	@echo "ESMF_DEP_LINK_OBJS = $(libdir)/libaqm.a"       >> $(@)
	@echo "ESMF_DEP_SHRD_PATH = "                         >> $(@)
	@echo "ESMF_DEP_SHRD_LIBS = mpp"                      >> $(@)
	@echo
	@echo "Finished generating ESMF self-describing build dependency makefile fragment: $(@)"
	@echo

uninstall-local: 
	-rm -f $(datarootdir)/aqm.mk

CLEANFILES = aqm.mk
