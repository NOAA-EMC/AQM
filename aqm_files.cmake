# Author: Brian Curtis
# This list was taken from the file Makefile.am found in subdirectories

list(APPEND aqm_files
    src/aqm_cap.F90
    src/aqm_comp_mod.F90
)

list(APPEND aqm_shr_files
    src/shr/aqm_config_mod.F90
    src/shr/aqm_const_mod.F90
    src/shr/aqm_domain_mod.F90
    src/shr/aqm_emis_mod.F90
    src/shr/aqm_prod_mod.F90
    src/shr/aqm_fires_mod.F90
    src/shr/aqm_internal_mod.F90
    src/shr/aqm_logger_mod.F90
    src/shr/aqm_model_mod.F90
    src/shr/aqm_rc_mod.F90
    src/shr/aqm_methods.F90
    src/shr/aqm_species_mod.F90
    src/shr/aqm_state_mod.F90
    src/shr/aqm_tools_mod.F90
    src/shr/aqm_types_mod.F90
)

list(APPEND aqm_drv_files
    src/drv/cmaq_model_mod.F90
    src/drv/cmaq_mod.F90
)

list(APPEND aqm_aqmio_files
    src/io/aqmio/aqmio.F90
)

list(APPEND aqm_ioapi_files
    src/io/ioapi/FDESC3.EXT
    src/io/ioapi/PARMS3.EXT
    src/io/ioapi/crlf.F
    src/io/ioapi/currec.f
    src/io/ioapi/currstep.f
    src/io/ioapi/dt2str.f
    src/io/ioapi/findc.f
    src/io/ioapi/getefile.F
    src/io/ioapi/index1.f
    src/io/ioapi/julian.F
    src/io/ioapi/junit.F
    src/io/ioapi/hhmmss.f
    src/io/ioapi/mmddyy.F
    src/io/ioapi/nextime.F
    src/io/ioapi/poly.f
    src/io/ioapi/promptmfile.f
    src/io/ioapi/sec2time.f
    src/io/ioapi/secsdiff.F
    src/io/ioapi/setlam.f
    src/io/ioapi/sortic.f
    src/io/ioapi/str2real.f
    src/io/ioapi/time2sec.f
    src/io/ioapi/upcase.f
    src/io/ioapi/wkday.F
    src/io/ioapi/yr2day.F
    src/io/ioapi/m3exit.F90
    src/io/ioapi/m3mesg.F90
    src/io/ioapi/m3msg2.F90
    src/io/ioapi/m3warn.F90
    src/io/ioapi/m3utilio.F90
)

set(CCTM_ROOT "src/model/CMAQ/CCTM/src")
set(AERO "${CCTM_ROOT}/aero/aero6")
set(BIOG "${CCTM_ROOT}/biog/beis3")
set(CLOUD "${CCTM_ROOT}/cloud/acm_ae6")
set(DEPV "${CCTM_ROOT}/depv/m3dry")
set(EMIS "${CCTM_ROOT}/emis/emis")
set(GAS "${CCTM_ROOT}/gas/ebi_cb6r3_ae6_aq")
set(GRID "${CCTM_ROOT}/grid/cartesian")
set(ICL "${CCTM_ROOT}/ICL/fixed")
set(INIT "${CCTM_ROOT}/init/yamo")
set(MECHS "${CCTM_ROOT}/MECHS/cb6r3_ae6_aq")
set(PA "${CCTM_ROOT}/procan/pa")
set(PHOT "${CCTM_ROOT}/phot/inline")
set(PLRISE "${CCTM_ROOT}/plrise/smoke")
set(SPCS "${CCTM_ROOT}/spcs/cgrid_spcs_nml")
set(STENEX "${CCTM_ROOT}/STENEX/noop")
set(UTIL "${CCTM_ROOT}/util/util")
set(VDIFF "${CCTM_ROOT}/vdiff/acm2")
set(localCCTM "src/model/src")
list(APPEND aqm_CCTM_files
	${AERO}/AERO_DATA.F
	${AERO}/aero_driver.F
	${AERO}/AEROMET_DATA.F
	${AERO}/AEROSOL_CHEMISTRY.F
	${AERO}/aero_subs.F
	${AERO}/AOD_DEFN.F
	${AERO}/coags.f
	${AERO}/getpar.f
	${AERO}/isofwd.f
	${AERO}/isorev.f
	${AERO}/isrpia.inc
	${AERO}/opvis.F
	${AERO}/opavis.F
	${AERO}/oppmdiag.F
	${AERO}/opapmdiag.F
	${AERO}/PRECURSOR_DATA.F
	${AERO}/PMDIAG_DATA.F
	${AERO}/SOA_DEFN.F
	${BIOG}/beis3.F
	${BIOG}/checkmem.f
	${BIOG}/czangle.F
	${BIOG}/getparb.f
	${BIOG}/hrno.F
	${BIOG}/parsline.f
	${BIOG}/tmpbeis.F
	${BIOG}/wrdaymsg.f
	${CLOUD}/hlconst.F
	${CLOUD}/cldproc_acm.F
	${CLOUD}/getalpha.F
	${CLOUD}/indexn.f
	${CLOUD}/rescld.F
	${CLOUD}/scavwdep.F
	${CLOUD}/aq_map.F
	${CLOUD}/AQ_DATA.F
	${DEPV}/ABFLUX_MOD.F
	${DEPV}/BIDI_MOD.F
	${DEPV}/cgrid_depv.F
	${DEPV}/DEPV_DEFN.F
	${DEPV}/DEPVVARS.F
	${DEPV}/gas_depv_map.F
	${DEPV}/HGSIM.F
	${DEPV}/LSM_MOD.F
	${DEPV}/MOSAIC_MOD.F
	${DEPV}/opdepv_diag.F
	${DEPV}/opdepv_mos.F
	${DEPV}/opdepv_fst.F
	${DEPV}/m3dry.F
	${EMIS}/BEIS_DEFN.F
	${EMIS}/BIOG_EMIS.F
	${EMIS}/cropcal.F
	${EMIS}/EMIS_DEFN.F
	${EMIS}/LTNG_DEFN.F
	${EMIS}/LUS_DEFN.F
	${EMIS}/MGEMIS.F
	${EMIS}/opemis.F
	${EMIS}/PTBILIN.F
	${EMIS}/SSEMIS.F
	${EMIS}/STK_EMIS.F
	${EMIS}/STK_PRMS.F
	${EMIS}/tfabove.F
	${EMIS}/tfbelow.F
	${EMIS}/UDTYPES.F
	${GAS}/degrade_data.F
	${GAS}/degrade.F
	${GAS}/DEGRADE_SETUP_TOX.F
	${GAS}/final_degrade.F
	${GAS}/find_degraded.F
	${GAS}/hrdata_mod.F
	${GAS}/hrdriver.F
	${GAS}/hrg1.F
	${GAS}/hrg2.F
	${GAS}/hrg3.F
	${GAS}/hrg4.F
	${GAS}/hrinit.F
	${GAS}/hrprodloss.F
	${GAS}/hrrates.F
	${GAS}/hrsolver.F
	${GAS}/init_degrade.F
	${GRID}/GRID_CONF.F
	${GRID}/HGRD_DEFN.F
	${GRID}/VGRD_DEFN.F
	${GRID}/PAGRD_DEFN.F
	${GRID}/PCGRID_DEFN.F
	${ICL}/const/CONST.EXT
	${ICL}/emctrl/EMISPRM.EXT
	${ICL}/filenames/FILES_CTM.EXT
	${ICL}/mpi/PE_COMM.EXT
	${INIT}/initscen.F
	${INIT}/load_cgrid.F
	${MECHS}/RXNS_DATA_MODULE.F90
	${MECHS}/RXNS_FUNC_MODULE.F90
	${PA}/PA_DEFN.F
	${PA}/pa_update.F
	${PHOT}/complex_number_module.F90
	${PHOT}/OMI_1979_to_2015.dat
	${PHOT}/opphot.F
	${PHOT}/phot.F
	${PHOT}/PHOT_MET_DATA.F
	${PHOT}/PHOT_MOD.F
	${PHOT}/PHOTOLYSIS_ALBEDO.F
	${PHOT}/PHOT_OPTICS.dat
	${PHOT}/SEAS_STRAT_O3_MIN.F
	${PHOT}/twoway_rrtmg_aero_optics.F90
	${PLRISE}/delta_zs.f
	${PLRISE}/fire_plmris.F
	${PLRISE}/openlayout.F
	${PLRISE}/oppt3d_diag.F
	${PLRISE}/plmris.F
	${PLRISE}/plsprd.f
	${PLRISE}/preplm.f
	${PLRISE}/ungridb2.f
	${PLRISE}/write3_distr.f
	${SPCS}/CGRID_SPCS.F
	${STENEX}/noop_comm_module.f
	${STENEX}/noop_util_module.f
	${UTIL}/bmatvec.F
	${UTIL}/findex.f
	${UTIL}/get_envlist.f
	${UTIL}/setup_logdev.F
	${UTIL}/subhdomain.F
	${UTIL}/UTILIO_DEFN.F
	${VDIFF}/aero_sedv.F
	${VDIFF}/conv_cgrid.F
	${VDIFF}/matrix1.F
	${VDIFF}/opddep.F
	${VDIFF}/opddep_fst.F
	${VDIFF}/opddep_mos.F
	${VDIFF}/rddepv.F
	${VDIFF}/SEDIMENTATION.F
	${VDIFF}/tri.F
	${VDIFF}/VDIFF_DIAG.F
	${VDIFF}/VDIFF_MAP.F
	${VDIFF}/vdiffproc.F
	${localCCTM}/o3totcol.f
	${localCCTM}/vdiffacmx.F
	${localCCTM}/PTMAP.F
	${localCCTM}/PT3D_DATA_MOD.F
	${localCCTM}/PT3D_DEFN.F
	${localCCTM}/PT3D_FIRE_DEFN.F
	${localCCTM}/PT3D_STKS_DEFN.F
	${localCCTM}/ASX_DATA_MOD.F
	${localCCTM}/DUST_EMIS.F
	${localCCTM}/AERO_PHOTDATA.F
	${localCCTM}/noop_modules.f
	${localCCTM}/CSQY_DATA.F
	${localCCTM}/isocom.f
	${localCCTM}/aero_depv.F
	${localCCTM}/AERO_EMIS.F
	${localCCTM}/CLOUD_OPTICS.F
)
