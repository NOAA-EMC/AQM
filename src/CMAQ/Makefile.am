noinst_LIBRARIES = libCCTM.a

# CCTM sources
CCTM = $(srcdir)/CCTM/src
libCCTM = libCCTM_a
libCCTM_a_SOURCES = 

# aero
AERO = $(CCTM)/aero/aero6
libAERO = $(AERO)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(AERO)/AERO_DATA.F \
	$(AERO)/aero_depv.F \
	$(AERO)/aero_driver.F \
	$(AERO)/AERO_EMIS.F \
	$(AERO)/AEROMET_DATA.F \
	$(AERO)/AEROSOL_CHEMISTRY.F \
	$(AERO)/aero_subs.F \
	$(AERO)/AOD_DEFN.F \
	$(AERO)/coags.f \
	$(AERO)/getpar.f \
	$(AERO)/isocom.f \
	$(AERO)/isofwd.f \
	$(AERO)/isorev.f \
	$(AERO)/isrpia.inc \
	$(AERO)/PRECURSOR_DATA.F \
	$(AERO)/PMDIAG_DATA.F \
	$(AERO)/SOA_DEFN.F 

# cloud
CLOUD = $(CCTM)/cloud/acm_ae6
libCLOUD = $(CLOUD)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(CLOUD)/concld_prop_acm.F \
	$(CLOUD)/hlconst.F

# couple

# depv
DEPV = $(CCTM)/depv/m3dry
libDEPV = $(DEPV)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(DEPV)/cgrid_depv.F \
	$(DEPV)/DEPV_DEFN.F \
	$(DEPV)/DEPVVARS.F \
	$(DEPV)/gas_depv_map.F \
	$(DEPV)/LSM_MOD.F \
	$(DEPV)/m3dry.F

# emis
EMIS = $(CCTM)/emis/emis
libEMIS = $(EMIS)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(EMIS)/EMIS_DEFN.F \
	$(EMIS)/LUS_DEFN.F \
	$(EMIS)/SSEMIS.F

# gas
GAS = $(CCTM)/gas/ebi_cb6r3_ae6_aq
libGAS = $(GAS)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(GAS)/degrade_data.F \
	$(GAS)/degrade.F \
	$(GAS)/DEGRADE_SETUP_TOX.F \
	$(GAS)/final_degrade.F \
	$(GAS)/find_degraded.F \
	$(GAS)/hrdata_mod.F \
	$(GAS)/hrdriver.F \
	$(GAS)/hrg1.F \
	$(GAS)/hrg2.F \
	$(GAS)/hrg3.F \
	$(GAS)/hrg4.F \
	$(GAS)/hrinit.F \
	$(GAS)/hrprodloss.F \
	$(GAS)/hrrates.F \
	$(GAS)/hrsolver.F \
	$(GAS)/init_degrade.F

# grid
GRID = $(CCTM)/grid/cartesian
libGRID = $(GRID)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(GRID)/GRID_CONF.F \
	$(GRID)/HGRD_DEFN.F \
	$(GRID)/VGRD_DEFN.F \
	$(GRID)/PCGRID_DEFN.F

# includes
ICL = $(CCTM)/ICL/fixed
libCCTM_a_SOURCES += \
	$(ICL)/const/CONST.EXT \
	$(ICL)/emctrl/EMISPRM.EXT \
	$(ICL)/filenames/FILES_CTM.EXT \
	$(ICL)/mpi/PE_COMM.EXT

# MECHS
MECHS = $(CCTM)/MECHS/cb6r3_ae6_aq
libMECHS = $(MECHS)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(MECHS)/RXNS_DATA_MODULE.F90 \
	$(MECHS)/RXNS_FUNC_MODULE.F90

# phot sources
PHOT = $(CCTM)/phot/inline
libPHOT = $(PHOT)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(PHOT)/AERO_PHOTDATA.F \
	$(PHOT)/CLOUD_OPTICS.F \
	$(PHOT)/complex_number_module.F90 \
	$(PHOT)/CSQY_DATA.F \
	$(PHOT)/o3totcol.f \
	$(PHOT)/OMI_1979_to_2015.dat \
	$(PHOT)/phot.F \
	$(PHOT)/PHOT_MET_DATA.F \
	$(PHOT)/PHOT_MOD.F \
	$(PHOT)/PHOTOLYSIS_ALBEDO.F \
	$(PHOT)/PHOT_OPTICS.dat \
	$(PHOT)/SEAS_STRAT_O3_MIN.F \
	$(PHOT)/twoway_rrtmg_aero_optics.F90

# scps
SPCS = $(CCTM)/spcs/cgrid_spcs_nml
libSPCS = $(SPCS)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(SPCS)/CGRID_SPCS.F

# vdiff
VDIFF = $(CCTM)/vdiff/acm2
libVDIFF = $(VDIFF)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(VDIFF)/ASX_DATA_MOD.F \
	$(VDIFF)/conv_cgrid.F \
	$(VDIFF)/eddyx.F \
	$(VDIFF)/matrix1.F \
	$(VDIFF)/tri.F \
	$(VDIFF)/vdiffacmx.F \
	$(VDIFF)/VDIFF_MAP.F \
	$(VDIFF)/vdiffproc.F

# util
UTIL = $(CCTM)/util/util
libUTIL = $(UTIL)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(UTIL)/findex.f \
	$(UTIL)/setup_logdev.F \
	$(UTIL)/UTILIO_DEFN.F

# UTIL
UTILS = $(srcdir)/UTIL/chemmech/src
libUTILS = $(UTILS)/$(libCCTM)-
# 	$(UTILS)/index1.f

#
libCCTM_a_CPPFLAGS  = -DSUBST_FILES_ID=\"FILES_CTM.EXT\"
libCCTM_a_CPPFLAGS += -DSUBST_CONST=\"CONST.EXT\"
libCCTM_a_CPPFLAGS += -DSUBST_EMISPRM=\"EMISPRM.EXT\"
libCCTM_a_CPPFLAGS += -Dparallel -Dmpas -D_AQM_

libCCTM_a_FFLAGS = $(CCTM_FFLAGS)
libCCTM_a_FFLAGS += \
	-I $(ICL)/filenames -I $(ICL)/const -I $(ICL)/emctrl \
	-I $(builddir)/../io/ioapi

libCCTM_a_FCFLAGS  = $(CCTM_FCFLAGS)
libCCTM_a_FCFLAGS += \
	-I $(builddir)/../../util \
	-I $(builddir)/../io/ioapi

.PHONY : clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f *.$(FC_MODEXT)

clean-local: clean-modules

#####################
# CCTM dependencies #
#####################

# aero
$(libAERO)AERO_DATA.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT) \
	$(libUTIL)setup_logdev.$(OBJEXT)

$(libAERO)aero_depv.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(libVDIFF)ASX_DATA_MOD.$(OBJEXT) \
	$(libDEPV)LSM_MOD.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) $(libAERO)AERO_DATA.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)

$(libAERO)aero_driver.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(libAERO)AEROSOL_CHEMISTRY.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libAERO)AOD_DEFN.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(libAERO)SOA_DEFN.$(OBJEXT) $(libAERO)PRECURSOR_DATA.$(OBJEXT) $(libAERO)AERO_DATA.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT) \
	$(libAERO)PMDIAG_DATA.$(OBJEXT)

$(libAERO)AERO_EMIS.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(ICL)/const/CONST.EXT \
	$(libVDIFF)ASX_DATA_MOD.$(OBJEXT) $(libAERO)PRECURSOR_DATA.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libAERO)AERO_DATA.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT) $(libUTIL)setup_logdev.$(OBJEXT)

$(libAERO)getpar.$(OBJEXT) : $(libAERO)AEROMET_DATA.$(OBJEXT) $(libAERO)AERO_DATA.$(OBJEXT) 

$(libAERO)AEROMET_DATA.$(OBJEXT) : $(ICL)/const/CONST.EXT

$(libAERO)AEROSOL_CHEMISTRY.$(OBJEXT) : $(ICL)/const/CONST.EXT $(libAERO)SOA_DEFN.$(OBJEXT) \
	$(libAERO)PRECURSOR_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) $(libAERO)AERO_DATA.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT) \
	$(libUTIL)setup_logdev.$(OBJEXT)

$(libAERO)aero_subs.$(OBJEXT) : $(AERO)/isrpia.inc $(ICL)/const/CONST.EXT $(libAERO)AOD_DEFN.$(OBJEXT) \
	$(libAERO)AEROMET_DATA.$(OBJEXT) $(libAERO)SOA_DEFN.$(OBJEXT) $(libAERO)PRECURSOR_DATA.$(OBJEXT) \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libUTIL)setup_logdev.$(OBJEXT)

$(libAERO)AOD_DEFN.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(libVDIFF)ASX_DATA_MOD.$(OBJEXT) \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libGRID)VGRD_DEFN.$(OBJEXT)

$(libAERO)isocom.$(OBJEXT) : $(AERO)/isrpia.inc
$(libAERO)isofwd.$(OBJEXT) : $(AERO)/isrpia.inc
$(libAERO)isorev.$(OBJEXT) : $(AERO)/isrpia.inc
$(libAERO)PRECURSOR_DATA.$(OBJEXT) : $(libAERO)AEROMET_DATA.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libUTIL)setup_logdev.$(OBJEXT)
$(libAERO)SOA_DEFN.$(OBJEXT) : $(libSPCS)CGRID_SPCS.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libUTIL)setup_logdev.$(OBJEXT)

# cloud
$(libCLOUD)concld_prop_acm.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(ICL)/const/CONST.EXT \
	$(libVDIFF)ASX_DATA_MOD.$(OBJEXT) $(libPHOT)PHOT_MET_DATA.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libGRID)VGRD_DEFN.$(OBJEXT) $(libUTIL)setup_logdev.$(OBJEXT)
$(libCLOUD)hlconst.$(OBJEXT) : $(libUTIL)UTILIO_DEFN.$(OBJEXT)

# depv
$(libDEPV)cgrid_depv.$(OBJEXT) : $(libGRID)HGRD_DEFN.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) 
$(libDEPV)DEPV_DEFN.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(ICL)/const/CONST.EXT \
	$(libVDIFF)ASX_DATA_MOD.$(OBJEXT) $(libDEPV)LSM_MOD.$(OBJEXT) $(libDEPV)DEPVVARS.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT)
$(libDEPV)gas_depv_map.$(OBJEXT) : $(libDEPV)DEPVVARS.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) 
$(libDEPV)LSM_MOD.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(libGRID)HGRD_DEFN.$(OBJEXT) 
$(libDEPV)m3dry.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(libVDIFF)ASX_DATA_MOD.$(OBJEXT) \
	$(libGRID)VGRD_DEFN.$(OBJEXT) $(libDEPV)DEPVVARS.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT)

# emis
$(libEMIS)EMIS_DEFN.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(ICL)/const/CONST.EXT \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libVDIFF)ASX_DATA_MOD.$(OBJEXT) \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AERO_EMIS.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libGRID)VGRD_DEFN.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) 
$(libEMIS)LUS_DEFN.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libUTIL)setup_logdev.$(OBJEXT)
$(libEMIS)SSEMIS.$(OBJEXT) :  $(ICL)/filenames/FILES_CTM.EXT $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libAERO)AEROMET_DATA.$(OBJEXT) $(libVDIFF)ASX_DATA_MOD.$(OBJEXT) $(libUTIL)setup_logdev.$(OBJEXT)

# gas
$(libGAS)degrade_data.$(OBJEXT) : $(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) 
$(libGAS)degrade.$(OBJEXT) : $(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) 
$(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) : $(ICL)/const/CONST.EXT $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) 
$(libGAS)final_degrade.$(OBJEXT) : $(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) 
$(libGAS)find_degraded.$(OBJEXT) : $(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) 
$(libGAS)hrdriver.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(ICL)/const/CONST.EXT $(ICL)/emctrl/EMISPRM.EXT \
	$(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) $(libGAS)hrdata_mod.$(OBJEXT) \
	$(libMECHS)RXNS_FUNC_MODULE.$(OBJEXT) $(libAERO)AEROSOL_CHEMISTRY.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libGRID)VGRD_DEFN.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT)
$(libGAS)hrg1.$(OBJEXT) : $(libGAS)hrdata_mod.$(OBJEXT) 
$(libGAS)hrg2.$(OBJEXT) : $(libGAS)hrdata_mod.$(OBJEXT) 
$(libGAS)hrg3.$(OBJEXT) : $(libGAS)hrdata_mod.$(OBJEXT) 
$(libGAS)hrg4.$(OBJEXT) : $(libGAS)hrdata_mod.$(OBJEXT) 
$(libGAS)hrinit.$(OBJEXT) : $(ICL)/const/CONST.EXT $(libMECHS)RXNS_FUNC_MODULE.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libGAS)hrdata_mod.$(OBJEXT) 
$(libGAS)hrprodloss.$(OBJEXT) : $(libGAS)hrdata_mod.$(OBJEXT) 
$(libGAS)hrrates.$(OBJEXT) : $(libGAS)hrdata_mod.$(OBJEXT) 
$(libGAS)hrsolver.$(OBJEXT) : $(libGAS)hrdata_mod.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libGAS)init_degrade.$(OBJEXT) : $(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) 

# grid
$(libGRID)GRID_CONF.$(OBJEXT) : $(libGRID)HGRD_DEFN.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT) \
	$(libUTIL)setup_logdev.$(OBJEXT)
$(libGRID)HGRD_DEFN.$(OBJEXT) : $(libUTIL)UTILIO_DEFN.$(OBJEXT) $(libUTIL)setup_logdev.$(OBJEXT)
$(libGRID)VGRD_DEFN.$(OBJEXT) : $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libGRID)PCGRID_DEFN.$(OBJEXT) : $(libGRID)GRID_CONF.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)

# MECHS
$(libMECHS)RXNS_FUNC_MODULE.$(OBJEXT) : $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)

# phot
$(libPHOT)AERO_PHOTDATA.$(OBJEXT) : $(ICL)/const/CONST.EXT $(libPHOT)twoway_rrtmg_aero_optics.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libGRID)VGRD_DEFN.$(OBJEXT) $(libPHOT)CSQY_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(libAERO)SOA_DEFN.$(OBJEXT) $(libAERO)AERO_DATA.$(OBJEXT) 
$(libPHOT)CLOUD_OPTICS.$(OBJEXT) : $(libPHOT)CSQY_DATA.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT) 
$(libPHOT)CSQY_DATA.$(OBJEXT) : $(libGRID)VGRD_DEFN.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) 
$(libPHOT)phot.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(libPHOT)SEAS_STRAT_O3_MIN.$(OBJEXT) \
	$(libPHOT)PHOT_MET_DATA.$(OBJEXT) $(libPHOT)PHOTOLYSIS_ALBEDO.$(OBJEXT) $(libPHOT)AERO_PHOTDATA.$(OBJEXT) \
	$(libPHOT)PHOT_MOD.$(OBJEXT) $(libAERO)AERO_DATA.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libPHOT)CLOUD_OPTICS.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libGRID)VGRD_DEFN.$(OBJEXT)
$(libPHOT)PHOT_MET_DATA.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(ICL)/const/CONST.EXT \
	$(libPHOT)CLOUD_OPTICS.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT)
$(libPHOT)PHOT_MOD.$(OBJEXT) : $(ICL)/const/CONST.EXT $(libPHOT)AERO_PHOTDATA.$(OBJEXT) \
	$(libPHOT)CLOUD_OPTICS.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libPHOT)CSQY_DATA.$(OBJEXT) 
$(libPHOT)PHOTOLYSIS_ALBEDO.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(libDEPV)LSM_MOD.$(OBJEXT) \
	$(libPHOT)PHOT_MOD.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT) \
	$(libGRID)PCGRID_DEFN.$(OBJEXT)
$(libPHOT)o3totcol.$(OBJEXT) : $(libUTIL)UTILIO_DEFN.$(OBJEXT)

$(libPHOT)SEAS_STRAT_O3_MIN.$(OBJEXT) : $(libGRID)VGRD_DEFN.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT)
$(libPHOT)twoway_rrtmg_aero_optics.$(OBJEXT) : $(libPHOT)complex_number_module.$(OBJEXT) 

# spcs
$(libSPCS)CGRID_SPCS.$(OBJEXT) : $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)

# util
$(libUTIL)setup_logdev.$(OBJEXT) : $(libUTIL)UTILIO_DEFN.$(OBJEXT)

# vdiff
$(libVDIFF)ASX_DATA_MOD.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(ICL)/const/CONST.EXT \
	$(libDEPV)DEPVVARS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) $(libDEPV)LSM_MOD.$(OBJEXT)
$(libVDIFF)conv_cgrid.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(ICL)/const/CONST.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libVDIFF)eddyx.$(OBJEXT) : $(libVDIFF)ASX_DATA_MOD.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT)
$(libVDIFF)matrix1.$(OBJEXT) : $(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT) 
$(libVDIFF)tri.$(OBJEXT) : $(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT) 
$(libVDIFF)vdiffacmx.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(libVDIFF)VDIFF_MAP.$(OBJEXT) \
	$(libVDIFF)ASX_DATA_MOD.$(OBJEXT) $(libDEPV)DEPV_DEFN.$(OBJEXT) \
	$(libEMIS)EMIS_DEFN.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) 
$(libVDIFF)VDIFF_MAP.$(OBJEXT) : $(ICL)/emctrl/EMISPRM.EXT $(libAERO)AERO_EMIS.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libUTIL)findex.$(OBJEXT)
$(libVDIFF)vdiffproc.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT $(libVDIFF)VDIFF_MAP.$(OBJEXT) \
	$(libVDIFF)ASX_DATA_MOD.$(OBJEXT) $(libDEPV)DEPV_DEFN.$(OBJEXT) \
	$(libEMIS)EMIS_DEFN.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) 
