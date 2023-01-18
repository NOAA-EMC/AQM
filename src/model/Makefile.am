noinst_LIBRARIES = libCCTM.a

# CCTM sources
CCTM = $(builddir)/CMAQ/CCTM/src
srcCCTM = $(srcdir)/CMAQ/CCTM/src
libCCTM = libCCTM_a
libCCTM_a_SOURCES =

# local version of CCTM source files
localCCTM = $(builddir)/src

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
	$(AERO)/opvis.F \
	$(AERO)/opavis.F \
	$(AERO)/oppmdiag.F \
	$(AERO)/opapmdiag.F \
	$(AERO)/PRECURSOR_DATA.F \
	$(AERO)/PMDIAG_DATA.F \
	$(AERO)/SOA_DEFN.F

# biog
BIOG = $(CCTM)/biog/beis3
libBIOG = $(BIOG)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(BIOG)/beis3.F \
	$(BIOG)/checkmem.f \
	$(BIOG)/czangle.F \
	$(BIOG)/getparb.f \
	$(BIOG)/hrno.F \
	$(BIOG)/parsline.f \
	$(BIOG)/tmpbeis.F \
	$(BIOG)/wrdaymsg.f

# cloud
CLOUD = $(CCTM)/cloud/acm_ae6
libCLOUD = $(CLOUD)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(CLOUD)/hlconst.F
	$(CLOUD)/cldproc_acm.F
	$(CLOUD)/getalpha.F
	$(CLOUD)/indexn.f
	$(CLOUD)/rescld.F
	$(CLOUD)/scavwdep.F
	$(CLOUD)/aq_map.F
	$(CLOUD)/AQ_DATA.F

# depv
DEPV = $(CCTM)/depv/m3dry
libDEPV = $(DEPV)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(DEPV)/ABFLUX_MOD.F \
	$(DEPV)/BIDI_MOD.F \
	$(DEPV)/cgrid_depv.F \
	$(DEPV)/DEPV_DEFN.F \
	$(DEPV)/DEPVVARS.F \
	$(DEPV)/gas_depv_map.F \
	$(DEPV)/HGSIM.F \
	$(DEPV)/LSM_MOD.F \
	$(DEPV)/MOSAIC_MOD.F \
	$(DEPV)/opdepv_diag.F \
	$(DEPV)/opdepv_mos.F \
	$(DEPV)/opdepv_fst.F \
	$(DEPV)/m3dry.F

# emis
EMIS = $(CCTM)/emis/emis
libEMIS = $(EMIS)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(EMIS)/BEIS_DEFN.F \
	$(EMIS)/BIOG_EMIS.F \
	$(EMIS)/cropcal.F \
	$(EMIS)/EMIS_DEFN.F \
	$(EMIS)/LTNG_DEFN.F \
	$(EMIS)/LUS_DEFN.F \
	$(EMIS)/MGEMIS.F \
	$(EMIS)/opemis.F \
	$(EMIS)/PTBILIN.F \
	$(EMIS)/SSEMIS.F \
	$(EMIS)/STK_EMIS.F \
	$(EMIS)/STK_PRMS.F \
	$(EMIS)/tfabove.F \
	$(EMIS)/tfbelow.F \
	$(EMIS)/UDTYPES.F

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
	$(GRID)/PAGRD_DEFN.F \
	$(GRID)/PCGRID_DEFN.F

# includes
ICL = $(srcCCTM)/ICL/fixed
libCCTM_a_SOURCES += \
	$(ICL)/const/CONST.EXT \
	$(ICL)/emctrl/EMISPRM.EXT \
	$(ICL)/filenames/FILES_CTM.EXT \
	$(ICL)/mpi/PE_COMM.EXT

# init
INIT = $(CCTM)/init/yamo
libINIT = $(INIT)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(INIT)/initscen.F \
	$(INIT)/load_cgrid.F

# MECHS
MECHS = $(CCTM)/MECHS/cb6r3_ae6_aq
libMECHS = $(MECHS)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(MECHS)/RXNS_DATA_MODULE.F90 \
	$(MECHS)/RXNS_FUNC_MODULE.F90

# Process Analysis
PA = $(CCTM)/procan/pa
libPA = $(PA)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(PA)/PA_DEFN.F \
	$(PA)/pa_update.F

# phot sources
PHOT = $(CCTM)/phot/inline
libPHOT = $(PHOT)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(PHOT)/CLOUD_OPTICS.F \
	$(PHOT)/complex_number_module.F90 \
	$(PHOT)/CSQY_DATA.F \
	$(PHOT)/OMI_1979_to_2015.dat \
	$(PHOT)/opphot.F \
	$(PHOT)/phot.F \
	$(PHOT)/PHOT_MET_DATA.F \
	$(PHOT)/PHOT_MOD.F \
	$(PHOT)/PHOTOLYSIS_ALBEDO.F \
	$(PHOT)/PHOT_OPTICS.dat \
	$(PHOT)/SEAS_STRAT_O3_MIN.F \
	$(PHOT)/twoway_rrtmg_aero_optics.F90

# plrise
PLRISE = $(CCTM)/plrise/smoke
libPLRISE = $(PLRISE)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(PLRISE)/delta_zs.f \
	$(PLRISE)/fire_plmris.F \
	$(PLRISE)/openlayout.F \
	$(PLRISE)/oppt3d_diag.F \
	$(PLRISE)/plmris.F   \
	$(PLRISE)/plsprd.f   \
	$(PLRISE)/preplm.f   \
	$(PLRISE)/ungridb2.f \
	$(PLRISE)/write3_distr.f

# scps
SPCS = $(CCTM)/spcs/cgrid_spcs_nml
libSPCS = $(SPCS)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(SPCS)/CGRID_SPCS.F

# STENEX
STENEX = $(CCTM)/STENEX/noop
libSTENEX = $(STENEX)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(STENEX)/noop_comm_module.f \
	$(STENEX)/noop_data_copy_module.f \
	$(STENEX)/noop_gather_module.f \
	$(STENEX)/noop_global_max_module.f \
	$(STENEX)/noop_global_min_module.f \
	$(STENEX)/noop_global_sum_module.f \
	$(STENEX)/noop_init_module.f \
	$(STENEX)/noop_modules.f \
	$(STENEX)/noop_slice_module.f \
	$(STENEX)/noop_term_module.f \
	$(STENEX)/noop_util_module.f

# util
UTIL = $(CCTM)/util/util
libUTIL = $(UTIL)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(UTIL)/bmatvec.F \
	$(UTIL)/findex.f \
	$(UTIL)/get_envlist.f \
	$(UTIL)/setup_logdev.F \
	$(UTIL)/subhdomain.F \
	$(UTIL)/UTILIO_DEFN.F

# vdiff
VDIFF = $(CCTM)/vdiff/acm2
libVDIFF = $(VDIFF)/$(libCCTM)-
libCCTM_a_SOURCES += \
	$(VDIFF)/aero_sedv.F \
	$(VDIFF)/conv_cgrid.F \
	$(VDIFF)/matrix1.F \
	$(VDIFF)/opddep.F \
	$(VDIFF)/opddep_fst.F \
	$(VDIFF)/opddep_mos.F \
	$(VDIFF)/rddepv.F \
	$(VDIFF)/SEDIMENTATION.F \
	$(VDIFF)/tri.F \
	$(VDIFF)/VDIFF_DIAG.F \
	$(VDIFF)/VDIFF_MAP.F \
	$(VDIFF)/vdiffproc.F

# local replacements for CCTM files
liblocalCCTM = $(localCCTM)/$(libCCTM)-
libCCTM_a_SOURCES += \
	${localCCTM}/o3totcol.f \
	${localCCTM}/vdiffacmx.F \
	${localCCTM}/PTMAP.F \
	${localCCTM}/PT3D_DATA_MOD.F \
	${localCCTM}/PT3D_DEFN.F \
	${localCCTM}/PT3D_FIRE_DEFN.F \
	${localCCTM}/PT3D_STKS_DEFN.F \
	${localCCTM}/ASX_DATA_MOD.F \
	${localCCTM}/DUST_EMIS.F \
	${localCCTM}/AERO_PHOTDATA.F


libCCTM_a_CPPFLAGS  = -DSUBST_FILES_ID=\"FILES_CTM.EXT\"
libCCTM_a_CPPFLAGS += -DSUBST_CONST=\"CONST.EXT\"
libCCTM_a_CPPFLAGS += -DSUBST_EMISPRM=\"EMISPRM.EXT\"
libCCTM_a_CPPFLAGS += -DSUBST_PE_COMM=\"PE_COMM.EXT\"
libCCTM_a_CPPFLAGS += -DSUBST_COMM=NOOP_COMM
libCCTM_a_CPPFLAGS += -DSUBST_BARRIER=NOOP_BARRIER
libCCTM_a_CPPFLAGS += -DSUBST_SUBGRID_INDEX=NOOP_SUBGRID_INDEX
libCCTM_a_CPPFLAGS += -DMOSAIC_MOD=MOSAIC_MODULE -DMosaic_Mod=Mosaic_Module
libCCTM_a_CPPFLAGS += -DAQCHEM=DUMMY_AQCHEM -DCONVCLD_ACM=DUMMY_CONVCLD_ACM
libCCTM_a_CPPFLAGS += -DEDDYX=DUMMY_EDDYX
libCCTM_a_CPPFLAGS += -DOPCONC=DUMMY_OPCONC -DOPACONC=DUMMY_OPACONC
libCCTM_a_CPPFLAGS += -DOPWDEP=DUMMY_OPWDEP -DWR_INIT=DUMMY_WR_INIT
libCCTM_a_CPPFLAGS += -Dverbose_aero -Dverbose_gas
libCCTM_a_CPPFLAGS += -Dmpas -D_AQM_

libCCTM_a_FFLAGS = $(CCTM_FFLAGS)
libCCTM_a_FFLAGS += \
	-I $(ICL)/filenames -I $(ICL)/const -I $(ICL)/emctrl -I$(ICL)/mpi \
	-I $(top_builddir)/src/io/ioapi -I $(top_builddir)/src/shr \
	-I $(top_builddir)/src/io/aqmio \
	-I $(AERO)

libCCTM_a_FCFLAGS  = $(CCTM_FCFLAGS)
libCCTM_a_FCFLAGS += -I $(top_builddir)/src/io/ioapi

include $(ESMFMKFILE)

libCCTM_a_FFLAGS += $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS)

.PHONY : clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f *.$(FC_MODEXT)

clean-local: clean-modules

#####################
# CCTM dependencies #
#####################

# aero
$(libAERO)AERO_DATA.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libAERO)AEROMET_DATA.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)aero_depv.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libDEPV)LSM_MOD.$(OBJEXT) \
	$(libDEPV)MOSAIC_MOD.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)aero_driver.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(libAERO)AEROSOL_CHEMISTRY.$(OBJEXT) $(libAERO)AOD_DEFN.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libAERO)PMDIAG_DATA.$(OBJEXT) $(libAERO)PRECURSOR_DATA.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libAERO)SOA_DEFN.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)AERO_EMIS.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/filenames/FILES_CTM.EXT \
	$(libAERO)AERO_DATA.$(OBJEXT) $(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(liblocalCCTM)DUST_EMIS.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libAERO)PRECURSOR_DATA.$(OBJEXT) \
	$(liblocalCCTM)PT3D_DEFN.$(OBJEXT) $(liblocalCCTM)PTMAP.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libEMIS)SSEMIS.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)AEROMET_DATA.$(OBJEXT) : $(ICL)/const/CONST.EXT
$(libAERO)AEROSOL_CHEMISTRY.$(OBJEXT) : $(ICL)/const/CONST.EXT \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libAERO)PRECURSOR_DATA.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libAERO)SOA_DEFN.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)aero_subs.$(OBJEXT) : $(ICL)/const/CONST.EXT $(AERO)/isrpia.inc \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(libAERO)AOD_DEFN.$(OBJEXT) $(libAERO)PRECURSOR_DATA.$(OBJEXT) \
	$(libAERO)SOA_DEFN.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)AOD_DEFN.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libAERO)AERO_DATA.$(OBJEXT) $(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)getpar.$(OBJEXT) : \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT)
$(libAERO)isocom.$(OBJEXT) : $(AERO)/isrpia.inc
$(libAERO)isofwd.$(OBJEXT) : $(AERO)/isrpia.inc
$(libAERO)isorev.$(OBJEXT) : $(AERO)/isrpia.inc
$(libAERO)opvis.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)opavis.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)oppmdiag.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libAERO)PMDIAG_DATA.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)opapmdiag.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libAERO)PMDIAG_DATA.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)PRECURSOR_DATA.$(OBJEXT) : \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libAERO)SOA_DEFN.$(OBJEXT) : \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)

# biog
$(libBIOG)beis3.$(OBJEXT) : \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libEMIS)BIOG_EMIS.$(OBJEXT)
$(libBIOG)czangle.$(OBJEXT) : $(ICL)/const/CONST.EXT \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libBIOG)hrno.$(OBJEXT) : \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libEMIS)BIOG_EMIS.$(OBJEXT) \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libDEPV)LSM_MOD.$(OBJEXT) \
	$(libSTENEX)noop_modules.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libBIOG)parsline.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libBIOG)tmpbeis.$(OBJEXT) : \
	$(libEMIS)BIOG_EMIS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libSTENEX)noop_modules.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libBIOG)wrdaymsg.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)

# cloud
$(libCLOUD)hlconst.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)

# depv
$(libDEPV)ABFLUX_MOD.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/filenames/FILES_CTM.EXT \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libDEPV)BIDI_MOD.$(OBJEXT) \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libDEPV)LSM_MOD.$(OBJEXT) \
	$(libDEPV)MOSAIC_MOD.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libDEPV)BIDI_MOD.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libDEPV)cgrid_depv.$(OBJEXT) : \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libDEPV)DEPV_DEFN.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/filenames/FILES_CTM.EXT \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libDEPV)BIDI_MOD.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libDEPV)DEPVVARS.$(OBJEXT) \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libDEPV)LSM_MOD.$(OBJEXT) \
	$(libDEPV)MOSAIC_MOD.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libDEPV)gas_depv_map.$(OBJEXT) : \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libDEPV)DEPVVARS.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libDEPV)HGSIM.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/filenames/FILES_CTM.EXT \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libDEPV)BIDI_MOD.$(OBJEXT) \
	$(libDEPV)DEPVVARS.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libDEPV)LSM_MOD.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libDEPV)LSM_MOD.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libDEPV)MOSAIC_MOD.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libDEPV)LSM_MOD.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libDEPV)opdepv_diag.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libDEPV)DEPVVARS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libDEPV)opdepv_mos.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libDEPV)DEPVVARS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libDEPV)LSM_MOD.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libDEPV)opdepv_fst.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libDEPV)DEPVVARS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libDEPV)LSM_MOD.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libDEPV)m3dry.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libDEPV)ABFLUX_MOD.$(OBJEXT) $(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) \
	$(libDEPV)BIDI_MOD.$(OBJEXT) $(libDEPV)DEPVVARS.$(OBJEXT) \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libDEPV)HGSIM.$(OBJEXT) \
	$(libDEPV)LSM_MOD.$(OBJEXT) $(libDEPV)MOSAIC_MOD.$(OBJEXT) \
	$(libSTENEX)noop_modules.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT) \
	$(libGRID)VGRD_DEFN.$(OBJEXT)

# emis
$(libEMIS)BEIS_DEFN.$(OBJEXT) : $(ICL)/const/CONST.EXT \
	$(libEMIS)BIOG_EMIS.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libEMIS)BIOG_EMIS.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libEMIS)cropcal.$(OBJEXT) : \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libEMIS)EMIS_DEFN.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/filenames/FILES_CTM.EXT \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AERO_EMIS.$(OBJEXT) \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libEMIS)BEIS_DEFN.$(OBJEXT) \
	$(libEMIS)BIOG_EMIS.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libDEPV)DEPV_DEFN.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libEMIS)LTNG_DEFN.$(OBJEXT) $(libEMIS)MGEMIS.$(OBJEXT) \
	$(liblocalCCTM)PT3D_DEFN.$(OBJEXT) $(liblocalCCTM)PTMAP.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libEMIS)STK_EMIS.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libEMIS)LTNG_DEFN.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libEMIS)LUS_DEFN.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libEMIS)MGEMIS.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/filenames/FILES_CTM.EXT \
	$(libAERO)AEROMET_DATA.$(OBJEXT) $(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libGRID)PCGRID_DEFN.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libEMIS)opemis.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libEMIS)EMIS_DEFN.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT) \
	$(libGRID)VGRD_DEFN.$(OBJEXT)
$(libEMIS)PTBILIN.$(OBJEXT) : \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libEMIS)STK_PRMS.$(OBJEXT) \
	$(libEMIS)UDTYPES.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT)
$(libEMIS)SSEMIS.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libSTENEX)noop_modules.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libEMIS)STK_EMIS.$(OBJEXT) : \
	$(libEMIS)UDTYPES.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libEMIS)STK_PRMS.$(OBJEXT) : \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libEMIS)UDTYPES.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libEMIS)tfabove.$(OBJEXT) : \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libEMIS)LUS_DEFN.$(OBJEXT)
$(libEMIS)tfbelow.$(OBJEXT) : \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libEMIS)LUS_DEFN.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)

# gas
$(libGAS)degrade_data.$(OBJEXT) : \
	$(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT)
$(libGAS)degrade.$(OBJEXT) : \
	$(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT)
$(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) : $(ICL)/const/CONST.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libGAS)final_degrade.$(OBJEXT) : \
	$(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT)
$(libGAS)find_degraded.$(OBJEXT) : \
	$(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT)
$(libGAS)hrdriver.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/emctrl/EMISPRM.EXT $(ICL)/filenames/FILES_CTM.EXT \
	$(libAERO)AEROSOL_CHEMISTRY.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libGAS)hrdata_mod.$(OBJEXT) $(libPA)PA_DEFN.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libMECHS)RXNS_FUNC_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT)
$(libGAS)hrg1.$(OBJEXT) : \
	$(libGAS)hrdata_mod.$(OBJEXT)
$(libGAS)hrg2.$(OBJEXT) : \
	$(libGAS)hrdata_mod.$(OBJEXT)
$(libGAS)hrg3.$(OBJEXT) : \
	$(libGAS)hrdata_mod.$(OBJEXT)
$(libGAS)hrg4.$(OBJEXT) : \
	$(libGAS)hrdata_mod.$(OBJEXT)
$(libGAS)hrinit.$(OBJEXT) : $(ICL)/const/CONST.EXT \
	$(libGAS)hrdata_mod.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libMECHS)RXNS_FUNC_MODULE.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libGAS)hrprodloss.$(OBJEXT) : \
	$(libGAS)hrdata_mod.$(OBJEXT)
$(libGAS)hrrates.$(OBJEXT) : \
	$(libGAS)hrdata_mod.$(OBJEXT)
$(libGAS)hrsolver.$(OBJEXT) : \
	$(libGAS)hrdata_mod.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libGAS)init_degrade.$(OBJEXT) : \
	$(libGAS)DEGRADE_SETUP_TOX.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT)

# grid
$(libGRID)GRID_CONF.$(OBJEXT) : \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT)
$(libGRID)HGRD_DEFN.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libGRID)VGRD_DEFN.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libGRID)PAGRD_DEFN.$(OBJEXT) : \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libSTENEX)noop_modules.$(OBJEXT) \
	$(libPA)PA_DEFN.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libGRID)PCGRID_DEFN.$(OBJEXT) : \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)

# init
$(libINIT)initscen.$(OBJEXT) : \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libSTENEX)noop_modules.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libINIT)load_cgrid.$(OBJEXT) : $(ICL)/const/CONST.EXT \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)

# MECHS
$(libMECHS)RXNS_FUNC_MODULE.$(OBJEXT) : \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)

# PA
$(libPA)pa_update.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/emctrl/EMISPRM.EXT $(ICL)/filenames/FILES_CTM.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libSTENEX)noop_modules.$(OBJEXT) $(libPA)PA_DEFN.$(OBJEXT) \
	$(libGRID)PAGRD_DEFN.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)

# phot
$(libPHOT)CLOUD_OPTICS.$(OBJEXT) : \
	$(libPHOT)CSQY_DATA.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libPHOT)CSQY_DATA.$(OBJEXT) : \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libPHOT)opphot.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libPHOT)PHOT_MET_DATA.$(OBJEXT) \
	$(libPHOT)PHOT_MOD.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libPHOT)phot.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libAERO)AERO_DATA.$(OBJEXT) $(liblocalCCTM)AERO_PHOTDATA.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libPHOT)CLOUD_OPTICS.$(OBJEXT) \
	$(libSTENEX)noop_modules.$(OBJEXT) $(libGRID)PCGRID_DEFN.$(OBJEXT) \
	$(libPHOT)PHOT_MET_DATA.$(OBJEXT) $(libPHOT)PHOT_MOD.$(OBJEXT) \
	$(libPHOT)PHOTOLYSIS_ALBEDO.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libPHOT)SEAS_STRAT_O3_MIN.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libPHOT)PHOT_MET_DATA.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/filenames/FILES_CTM.EXT \
	$(libPHOT)CLOUD_OPTICS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libPHOT)PHOT_MOD.$(OBJEXT) : $(ICL)/const/CONST.EXT \
	$(liblocalCCTM)AERO_PHOTDATA.$(OBJEXT) $(libPHOT)CLOUD_OPTICS.$(OBJEXT) \
	$(libPHOT)CSQY_DATA.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libPHOT)PHOTOLYSIS_ALBEDO.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libDEPV)LSM_MOD.$(OBJEXT) $(libSTENEX)noop_modules.$(OBJEXT) \
	$(libGRID)PCGRID_DEFN.$(OBJEXT) $(libPHOT)PHOT_MOD.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libPHOT)SEAS_STRAT_O3_MIN.$(OBJEXT) : \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libPHOT)twoway_rrtmg_aero_optics.$(OBJEXT) : \
	$(libPHOT)complex_number_module.$(OBJEXT)

# plrise
$(libPLRISE)fire_plmris.$(OBJEXT) : $(ICL)/const/CONST.EXT
$(libPLRISE)openlayout.$(OBJEXT) : \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libPLRISE)oppt3d_diag.$(OBJEXT) : \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libPLRISE)plmris.$(OBJEXT) : $(ICL)/const/CONST.EXT
$(libPLRISE)preplm.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libPLRISE)ungridb2.$(OBJEXT) : \
	$(libGRID)HGRD_DEFN.$(OBJEXT) $(libEMIS)PTBILIN.$(OBJEXT)
$(libPLRISE)write3_distr.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)

# spcs
$(libSPCS)CGRID_SPCS.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)

# STENEX
$(libSTENEX)noop_modules.$(OBJEXT) : \
	$(libSTENEX)noop_comm_module.$(OBJEXT) \
	$(libSTENEX)noop_data_copy_module.$(OBJEXT) \
	$(libSTENEX)noop_gather_module.$(OBJEXT) \
	$(libSTENEX)noop_global_max_module.$(OBJEXT) \
	$(libSTENEX)noop_global_min_module.$(OBJEXT) \
	$(libSTENEX)noop_global_sum_module.$(OBJEXT) \
	$(libSTENEX)noop_init_module.$(OBJEXT) \
	$(libSTENEX)noop_slice_module.$(OBJEXT) \
	$(libSTENEX)noop_term_module.$(OBJEXT) \
	$(libSTENEX)noop_util_module.$(OBJEXT)

# util
$(libUTIL)setup_logdev.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libUTIL)subhdomain.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)

# vdiff
$(libVDIFF)aero_sedv.$(OBJEXT) : \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libVDIFF)conv_cgrid.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/filenames/FILES_CTM.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libVDIFF)matrix1.$(OBJEXT) : \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT) \
	$(libGRID)VGRD_DEFN.$(OBJEXT)
$(libVDIFF)opddep.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libVDIFF)opddep_fst.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libDEPV)LSM_MOD.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libVDIFF)opddep_mos.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libDEPV)LSM_MOD.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libVDIFF)rddepv.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libVDIFF)SEDIMENTATION.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/filenames/FILES_CTM.EXT \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT) \
	$(libVDIFF)VDIFF_DIAG.$(OBJEXT) $(libVDIFF)VDIFF_MAP.$(OBJEXT)
$(libVDIFF)tri.$(OBJEXT) : \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)VGRD_DEFN.$(OBJEXT)
$(libVDIFF)VDIFF_DIAG.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libSTENEX)noop_modules.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libVDIFF)VDIFF_MAP.$(OBJEXT) : $(ICL)/emctrl/EMISPRM.EXT \
	$(libAERO)AERO_EMIS.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(libVDIFF)vdiffproc.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libDEPV)BIDI_MOD.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libDEPV)DEPV_DEFN.$(OBJEXT) \
	$(libEMIS)EMIS_DEFN.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libDEPV)HGSIM.$(OBJEXT) $(libDEPV)LSM_MOD.$(OBJEXT) \
	$(libPA)PA_DEFN.$(OBJEXT) $(libVDIFF)SEDIMENTATION.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT) $(libVDIFF)VDIFF_DIAG.$(OBJEXT) \
	$(libVDIFF)VDIFF_MAP.$(OBJEXT)

# local replacements
$(liblocalCCTM)o3totcol.$(OBJEXT) : \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(liblocalCCTM)vdiffacmx.$(OBJEXT) : $(ICL)/filenames/FILES_CTM.EXT \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) $(libDEPV)BIDI_MOD.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libDEPV)DEPV_DEFN.$(OBJEXT) $(libEMIS)EMIS_DEFN.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT) \
	$(libVDIFF)VDIFF_MAP.$(OBJEXT)
$(liblocalCCTM)PTMAP.$(OBJEXT) : $(libAERO)AERO_DATA.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(liblocalCCTM)PT3D_DEFN.$(OBJEXT) : $(libGRID)GRID_CONF.$(OBJEXT) \
	$(liblocalCCTM)PTMAP.$(OBJEXT) $(liblocalCCTM)PT3D_FIRE_DEFN.$(OBJEXT) \
	$(liblocalCCTM)PT3D_STKS_DEFN.$(OBJEXT)
$(liblocalCCTM)PT3D_FIRE_DEFN.$(OBJEXT) : $(libAERO)AERO_DATA.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(liblocalCCTM)PT3D_DATA_MOD.$(OBJEXT) \
	$(liblocalCCTM)PTMAP.$(OBJEXT) $(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) \
	$(libEMIS)STK_EMIS.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(liblocalCCTM)PT3D_STKS_DEFN.$(OBJEXT) : $(libAERO)AERO_DATA.$(OBJEXT) \
	$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) \
	$(libGRID)GRID_CONF.$(OBJEXT) $(libSPCS)CGRID_SPCS.$(OBJEXT) \
	$(liblocalCCTM)PT3D_DATA_MOD.$(OBJEXT) $(liblocalCCTM)PTMAP.$(OBJEXT) \
	$(libEMIS)STK_EMIS.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/filenames/FILES_CTM.EXT $(ICL)/mpi/PE_COMM.EXT \
        $(libDEPV)DEPVVARS.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
        $(libDEPV)LSM_MOD.$(OBJEXT) $(libSTENEX)noop_modules.$(OBJEXT) \
        $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(liblocalCCTM)DUST_EMIS.$(OBJEXT) : $(ICL)/const/CONST.EXT $(ICL)/filenames/FILES_CTM.EXT \
       $(libAERO)AERO_DATA.$(OBJEXT) $(liblocalCCTM)ASX_DATA_MOD.$(OBJEXT) \
       $(libGRID)GRID_CONF.$(OBJEXT) $(libGRID)HGRD_DEFN.$(OBJEXT) \
       $(libEMIS)LUS_DEFN.$(OBJEXT) $(libUTIL)UTILIO_DEFN.$(OBJEXT)
$(liblocalCCTM)AERO_PHOTDATA.$(OBJEXT) : $(ICL)/const/CONST.EXT \
	$(libAERO)AERO_DATA.$(OBJEXT) $(libAERO)AEROMET_DATA.$(OBJEXT) \
	$(libSPCS)CGRID_SPCS.$(OBJEXT) $(libPHOT)twoway_rrtmg_aero_optics.$(OBJEXT) \
	$(libPHOT)CSQY_DATA.$(OBJEXT) $(libGRID)GRID_CONF.$(OBJEXT) \
	$(libMECHS)RXNS_DATA_MODULE.$(OBJEXT) $(libAERO)SOA_DEFN.$(OBJEXT) \
	$(libUTIL)UTILIO_DEFN.$(OBJEXT)
