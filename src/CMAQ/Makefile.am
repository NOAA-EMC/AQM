SUBDIRS = util UTIL CCTM .

noinst_LIBRARIES = libCMAQ.a

libCMAQ_a_SOURCES =

libCMAQ_a_LIBADD = $(builddir)/CCTM/src/aero/aero6/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/cloud/acm/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/couple/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/depv/m3dry/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/driver/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/emis/emis/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/gas/ebi_cb6r3_ae6_aq/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/grid/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/init/yamo/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/MECHS/cb6r3_ae6_aq/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/phot/inline/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/spcs/cgrid_spcs_nml/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/vdiff/acm2/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/CCTM/src/util/util/libCCTM_a-*.$(OBJEXT) \
	$(builddir)/util/libutil_a-*.$(OBJEXT) \
	$(builddir)/UTIL/chemmech/src/*.$(OBJEXT) \
	$(builddir)/UTIL/create_ebi/src_RXNSU/*.$(OBJEXT)
