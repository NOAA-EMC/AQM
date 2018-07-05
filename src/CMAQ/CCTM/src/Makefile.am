noinst_LIBRARIES = libCCTM.a

# CCTM sources
libCCTM_a_SOURCES = 

# aero
libCCTM_a_SOURCES += \
	aero/aero6/AERO_DATA.F \
	aero/aero6/aero_depv.F \
	aero/aero6/aero_driver.F \
	aero/aero6/AERO_EMIS.F \
	aero/aero6/AEROMET_DATA.F \
	aero/aero6/AEROSOL_CHEMISTRY.F \
	aero/aero6/aero_subs.F \
	aero/aero6/AOD_DEFN.F \
	aero/aero6/coags.f \
	aero/aero6/getpar.f \
	aero/aero6/isocom.f \
	aero/aero6/isofwd.f \
	aero/aero6/isorev.f \
	aero/aero6/isrpia.inc \
	aero/aero6/PRECURSOR_DATA.F \
	aero/aero6/SOA_DEFN.F 

# cloud
libCCTM_a_SOURCES += \
	cloud/acm/concld_prop_acm.F \
	cloud/acm_ae6/hlconst.F

# couple
libCCTM_a_SOURCES += \
	couple/coupler_mod.F90

# depv
libCCTM_a_SOURCES += \
	depv/m3dry/cgrid_depv.F \
	depv/m3dry/DEPV_DEFN.F \
	depv/m3dry/DEPVVARS.F \
	depv/m3dry/gas_depv_map.F \
	depv/m3dry/LSM_MOD.F \
	depv/m3dry/m3dry.F

# driver
libCCTM_a_SOURCES += \
	driver/driver.F \
	driver/sciproc.F

# emis
libCCTM_a_SOURCES += \
	emis/emis/EMIS_DEFN.F \
	emis/emis/LUS_DEFN.F

# gas
libCCTM_a_SOURCES += \
	gas/ebi_cb6r3_ae6_aq/degrade_data.F \
	gas/ebi_cb6r3_ae6_aq/degrade.F \
	gas/ebi_cb6r3_ae6_aq/DEGRADE_SETUP_TOX.F \
	gas/ebi_cb6r3_ae6_aq/final_degrade.F \
	gas/ebi_cb6r3_ae6_aq/find_degraded.F \
	gas/ebi_cb6r3_ae6_aq/hrdata_mod.F \
	gas/ebi_cb6r3_ae6_aq/hrdriver.F \
	gas/ebi_cb6r3_ae6_aq/hrg1.F \
	gas/ebi_cb6r3_ae6_aq/hrg2.F \
	gas/ebi_cb6r3_ae6_aq/hrg3.F \
	gas/ebi_cb6r3_ae6_aq/hrg4.F \
	gas/ebi_cb6r3_ae6_aq/hrinit.F \
	gas/ebi_cb6r3_ae6_aq/hrprodloss.F \
	gas/ebi_cb6r3_ae6_aq/hrrates.F \
	gas/ebi_cb6r3_ae6_aq/hrsolver.F \
	gas/ebi_cb6r3_ae6_aq/init_degrade.F

# grid
libCCTM_a_SOURCES += \
	grid/HGRD_DEFN.F \
	grid/VGRD_DEFN.F

# includes
libCCTM_a_SOURCES += \
	ICL/fixed/const/CONST.EXT \
	ICL/fixed/emctrl/EMISPRM.EXT \
	ICL/fixed/filenames/FILES_CTM.EXT \
	ICL/fixed/mpi/PE_COMM.EXT

# init
libCCTM_a_SOURCES += \
	init/yamo/initscen.F \
	init/yamo/load_cgrid.F

# MECHS
libCCTM_a_SOURCES += \
	MECHS/cb6r3_ae6_aq/RXNS_DATA_MODULE.F90 \
	MECHS/cb6r3_ae6_aq/RXNS_FUNC_MODULE.F90

# phot sources
libCCTM_a_SOURCES += \
	phot/inline/AERO_PHOTDATA.F \
	phot/inline/CLOUD_OPTICS.F \
	phot/inline/complex_number_module.F90 \
	phot/inline/CSQY_DATA.F \
	phot/inline/o3totcol.f \
	phot/inline/OMI_1979_to_2015.dat \
	phot/inline/phot.F \
	phot/inline/PHOT_MET_DATA.F \
	phot/inline/PHOT_MOD.F \
	phot/inline/PHOTOLYSIS_ALBEDO.F \
	phot/inline/PHOT_OPTICS.dat \
	phot/inline/SEAS_STRAT_O3_MIN.F \
	phot/inline/twoway_rrtmg_aero_optics.F90

# scps
libCCTM_a_SOURCES += \
	spcs/cgrid_spcs_nml/CGRID_SPCS.F

# vdiff
libCCTM_a_SOURCES += \
	vdiff/acm2/ASX_DATA_MOD.F \
	vdiff/acm2/conv_cgrid.F \
	vdiff/acm2/eddyx.F \
	vdiff/acm2/matrix1.F \
	vdiff/acm2/tri.F \
	vdiff/acm2/vdiffacmx.F \
	vdiff/acm2/VDIFF_MAP.F \
	vdiff/acm2/vdiffproc.F

# util
libCCTM_a_SOURCES += \
	util/util/findex.f \
	util/util/setup_logdev.F

# MPI
# par/mpi/mpcomm_init.F
#
#
libCCTM_a_CPPFLAGS  = -DSUBST_FILES_ID=\"FILES_CTM.EXT\"
libCCTM_a_CPPFLAGS += -DSUBST_CONST=\"CONST.EXT\"
libCCTM_a_CPPFLAGS += -DSUBST_EMISPRM=\"EMISPRM.EXT\"
libCCTM_a_CPPFLAGS += -Dparallel -Dmpas

libCCTM_a_FFLAGS = -fixed -132 -override-limits
libCCTM_a_FFLAGS += -I . \
	-I $(srcdir)/ICL/fixed/filenames -I $(srcdir)/ICL/fixed/const -I $(srcdir)/ICL/fixed/emctrl \
	-I $(builddir)/../../util
libCCTM_a_FCFLAGS  = -free -override-limits
libCCTM_a_FCFLAGS += -I $(builddir)/../../util

.PHONY : clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f *.$(FC_MODEXT)

clean-local: clean-modules

# CCTM dependencies
# aero
aero/aero6/libCCTM_a-getpar.$(OBJEXT) : aero/aero6/libCCTM_a-AEROMET_DATA.$(OBJEXT) aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) 
aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT aero/aero6/libCCTM_a-AEROMET_DATA.$(OBJEXT) \
	spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) 
aero/aero6/libCCTM_a-aero_depv.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT vdiff/acm2/libCCTM_a-ASX_DATA_MOD.$(OBJEXT) \
	depv/m3dry/libCCTM_a-LSM_MOD.$(OBJEXT) aero/aero6/libCCTM_a-AEROMET_DATA.$(OBJEXT) aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) \
	spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) \
	grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) 
aero/aero6/libCCTM_a-aero_driver.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT aero/aero6/libCCTM_a-AEROSOL_CHEMISTRY.$(OBJEXT) \
	spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) aero/aero6/libCCTM_a-AOD_DEFN.$(OBJEXT) aero/aero6/libCCTM_a-AEROMET_DATA.$(OBJEXT) \
	aero/aero6/libCCTM_a-SOA_DEFN.$(OBJEXT) aero/aero6/libCCTM_a-PRECURSOR_DATA.$(OBJEXT) aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) \
	MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) 
aero/aero6/libCCTM_a-AERO_EMIS.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT ICL/fixed/const/CONST.EXT \
	vdiff/acm2/libCCTM_a-ASX_DATA_MOD.$(OBJEXT) couple/libCCTM_a-coupler_mod.$(OBJEXT) aero/aero6/libCCTM_a-PRECURSOR_DATA.$(OBJEXT) \
	spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) \
	grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) 
aero/aero6/libCCTM_a-AEROMET_DATA.$(OBJEXT) : ICL/fixed/const/CONST.EXT
aero/aero6/libCCTM_a-AEROSOL_CHEMISTRY.$(OBJEXT) : ICL/fixed/const/CONST.EXT aero/aero6/libCCTM_a-SOA_DEFN.$(OBJEXT) \
	aero/aero6/libCCTM_a-PRECURSOR_DATA.$(OBJEXT) aero/aero6/libCCTM_a-AEROMET_DATA.$(OBJEXT) aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) \
	MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) \
	couple/libCCTM_a-coupler_mod.$(OBJEXT) 
aero/aero6/libCCTM_a-aero_subs.$(OBJEXT) : aero/aero6/isrpia.inc ICL/fixed/const/CONST.EXT aero/aero6/libCCTM_a-AOD_DEFN.$(OBJEXT) \
	aero/aero6/libCCTM_a-AEROMET_DATA.$(OBJEXT) aero/aero6/libCCTM_a-SOA_DEFN.$(OBJEXT) aero/aero6/libCCTM_a-PRECURSOR_DATA.$(OBJEXT) \
	aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) 
aero/aero6/libCCTM_a-AOD_DEFN.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT vdiff/acm2/libCCTM_a-ASX_DATA_MOD.$(OBJEXT) \
	aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) \
	grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) couple/libCCTM_a-coupler_mod.$(OBJEXT) 
aero/aero6/libCCTM_a-isocom.$(OBJEXT) : aero/aero6/isrpia.inc
aero/aero6/libCCTM_a-isofwd.$(OBJEXT) : aero/aero6/isrpia.inc
aero/aero6/libCCTM_a-isorev.$(OBJEXT) : aero/aero6/isrpia.inc
aero/aero6/libCCTM_a-PRECURSOR_DATA.$(OBJEXT) : aero/aero6/libCCTM_a-AEROMET_DATA.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) \
	aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) 
aero/aero6/libCCTM_a-SOA_DEFN.$(OBJEXT) : spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) aero/aero6/libCCTM_a-AEROMET_DATA.$(OBJEXT) \
	aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) 

# cloud
cloud/acm/libCCTM_a-concld_prop_acm.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT ICL/fixed/const/CONST.EXT \
	vdiff/acm2/libCCTM_a-ASX_DATA_MOD.$(OBJEXT) phot/inline/libCCTM_a-PHOT_MET_DATA.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) \
	grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) couple/libCCTM_a-coupler_mod.$(OBJEXT) 

# couple

# depv
depv/m3dry/libCCTM_a-cgrid_depv.$(OBJEXT) : grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) 
depv/m3dry/libCCTM_a-DEPV_DEFN.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT ICL/fixed/const/CONST.EXT \
	vdiff/acm2/libCCTM_a-ASX_DATA_MOD.$(OBJEXT) depv/m3dry/libCCTM_a-LSM_MOD.$(OBJEXT) depv/m3dry/libCCTM_a-DEPVVARS.$(OBJEXT) \
	spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) 
depv/m3dry/libCCTM_a-gas_depv_map.$(OBJEXT) : depv/m3dry/libCCTM_a-DEPVVARS.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) 
depv/m3dry/libCCTM_a-LSM_MOD.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT couple/libCCTM_a-coupler_mod.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) 
depv/m3dry/libCCTM_a-m3dry.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT vdiff/acm2/libCCTM_a-ASX_DATA_MOD.$(OBJEXT) \
	grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) depv/m3dry/libCCTM_a-DEPVVARS.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) 

# driver
driver/libCCTM_a-driver.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT emis/emis/libCCTM_a-LUS_DEFN.$(OBJEXT) \
	spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) 

# grid

# init
init/yamo/libCCTM_a-initscen.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) \
	grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) 
init/yamo/libCCTM_a-load_cgrid.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT ICL/fixed/const/CONST.EXT \
	couple/libCCTM_a-coupler_mod.$(OBJEXT) aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) \
	grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) 

# MECHS
MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_FUNC_MODULE.$(OBJEXT) : spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) \
	MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) 

# phot
phot/inline/libCCTM_a-AERO_PHOTDATA.$(OBJEXT) : ICL/fixed/const/CONST.EXT phot/inline/libCCTM_a-twoway_rrtmg_aero_optics.$(OBJEXT) \
	spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) \
	grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) phot/inline/libCCTM_a-CSQY_DATA.$(OBJEXT) aero/aero6/libCCTM_a-AEROMET_DATA.$(OBJEXT) \
	aero/aero6/libCCTM_a-SOA_DEFN.$(OBJEXT) aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) 
phot/inline/libCCTM_a-CLOUD_OPTICS.$(OBJEXT) : phot/inline/libCCTM_a-CSQY_DATA.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) 
phot/inline/libCCTM_a-CSQY_DATA.$(OBJEXT) : grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) 
phot/inline/libCCTM_a-phot.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT phot/inline/libCCTM_a-SEAS_STRAT_O3_MIN.$(OBJEXT) \
	phot/inline/libCCTM_a-PHOT_MET_DATA.$(OBJEXT) phot/inline/libCCTM_a-PHOTOLYSIS_ALBEDO.$(OBJEXT) phot/inline/libCCTM_a-AERO_PHOTDATA.$(OBJEXT) \
	phot/inline/libCCTM_a-PHOT_MOD.$(OBJEXT) aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) \
	MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) phot/inline/libCCTM_a-CLOUD_OPTICS.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) \
	grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) couple/libCCTM_a-coupler_mod.$(OBJEXT) 
phot/inline/libCCTM_a-PHOT_MET_DATA.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT ICL/fixed/const/CONST.EXT \
	phot/inline/libCCTM_a-CLOUD_OPTICS.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) couple/libCCTM_a-coupler_mod.$(OBJEXT) 
phot/inline/libCCTM_a-PHOT_MOD.$(OBJEXT) : ICL/fixed/const/CONST.EXT phot/inline/libCCTM_a-AERO_PHOTDATA.$(OBJEXT) \
	phot/inline/libCCTM_a-CLOUD_OPTICS.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) phot/inline/libCCTM_a-CSQY_DATA.$(OBJEXT) 
phot/inline/libCCTM_a-PHOTOLYSIS_ALBEDO.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT depv/m3dry/libCCTM_a-LSM_MOD.$(OBJEXT) \
	phot/inline/libCCTM_a-PHOT_MOD.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) couple/libCCTM_a-coupler_mod.$(OBJEXT) 
phot/inline/libCCTM_a-SEAS_STRAT_O3_MIN.$(OBJEXT) : couple/libCCTM_a-coupler_mod.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) 
phot/inline/libCCTM_a-twoway_rrtmg_aero_optics.$(OBJEXT) : phot/inline/libCCTM_a-complex_number_module.$(OBJEXT) 

# emis
emis/emis/libCCTM_a-EMIS_DEFN.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT ICL/fixed/const/CONST.EXT \
	MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) vdiff/acm2/libCCTM_a-ASX_DATA_MOD.$(OBJEXT) couple/libCCTM_a-coupler_mod.$(OBJEXT) \
	aero/aero6/libCCTM_a-AERO_DATA.$(OBJEXT) aero/aero6/libCCTM_a-AERO_EMIS.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) \
	grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) 
emis/emis/libCCTM_a-LUS_DEFN.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) 

# gas
gas/ebi_cb6r3_ae6_aq/libCCTM_a-degrade_data.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-DEGRADE_SETUP_TOX.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-degrade.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-DEGRADE_SETUP_TOX.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-DEGRADE_SETUP_TOX.$(OBJEXT) : ICL/fixed/const/CONST.EXT MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) \
	spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-final_degrade.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-DEGRADE_SETUP_TOX.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-find_degraded.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-DEGRADE_SETUP_TOX.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrdriver.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT ICL/fixed/const/CONST.EXT ICL/fixed/emctrl/EMISPRM.EXT \
	gas/ebi_cb6r3_ae6_aq/libCCTM_a-DEGRADE_SETUP_TOX.$(OBJEXT) gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrdata_mod.$(OBJEXT) \
	MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_FUNC_MODULE.$(OBJEXT) aero/aero6/libCCTM_a-AEROSOL_CHEMISTRY.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) \
	grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) couple/libCCTM_a-coupler_mod.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrg1.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrdata_mod.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrg2.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrdata_mod.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrg3.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrdata_mod.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrg4.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrdata_mod.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrinit.$(OBJEXT) : ICL/fixed/const/CONST.EXT MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_FUNC_MODULE.$(OBJEXT) \
	MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrdata_mod.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrprodloss.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrdata_mod.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrrates.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrdata_mod.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrsolver.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-hrdata_mod.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) 
gas/ebi_cb6r3_ae6_aq/libCCTM_a-init_degrade.$(OBJEXT) : gas/ebi_cb6r3_ae6_aq/libCCTM_a-DEGRADE_SETUP_TOX.$(OBJEXT) MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) 

#par/mpi/mpcomm_init.$(OBJEXT) : par/mpi/mpcomm_init.F 
# spcs
spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) : MECHS/cb6r3_ae6_aq/libCCTM_a-RXNS_DATA_MODULE.$(OBJEXT) 

# util

# vdiff
vdiff/acm2/libCCTM_a-ASX_DATA_MOD.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT ICL/fixed/const/CONST.EXT \
	depv/m3dry/libCCTM_a-DEPVVARS.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) couple/libCCTM_a-coupler_mod.$(OBJEXT) 
vdiff/acm2/libCCTM_a-conv_cgrid.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT ICL/fixed/const/CONST.EXT \
	couple/libCCTM_a-coupler_mod.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) \
	grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) 
vdiff/acm2/libCCTM_a-eddyx.$(OBJEXT) : vdiff/acm2/libCCTM_a-ASX_DATA_MOD.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) 
vdiff/acm2/libCCTM_a-matrix1.$(OBJEXT) : spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) 
vdiff/acm2/libCCTM_a-tri.$(OBJEXT) : spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) 
vdiff/acm2/libCCTM_a-vdiffacmx.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT vdiff/acm2/libCCTM_a-VDIFF_MAP.$(OBJEXT) \
	vdiff/acm2/libCCTM_a-ASX_DATA_MOD.$(OBJEXT) depv/m3dry/libCCTM_a-DEPV_DEFN.$(OBJEXT) \
	emis/emis/libCCTM_a-EMIS_DEFN.$(OBJEXT) grid/libCCTM_a-HGRD_DEFN.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) 
vdiff/acm2/libCCTM_a-VDIFF_MAP.$(OBJEXT) : ICL/fixed/emctrl/EMISPRM.EXT aero/aero6/libCCTM_a-AERO_EMIS.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) 
vdiff/acm2/libCCTM_a-vdiffproc.$(OBJEXT) : ICL/fixed/filenames/FILES_CTM.EXT vdiff/acm2/libCCTM_a-VDIFF_MAP.$(OBJEXT) \
	vdiff/acm2/libCCTM_a-ASX_DATA_MOD.$(OBJEXT) depv/m3dry/libCCTM_a-DEPV_DEFN.$(OBJEXT) \
	emis/emis/libCCTM_a-EMIS_DEFN.$(OBJEXT) grid/libCCTM_a-VGRD_DEFN.$(OBJEXT) spcs/cgrid_spcs_nml/libCCTM_a-CGRID_SPCS.$(OBJEXT) 
