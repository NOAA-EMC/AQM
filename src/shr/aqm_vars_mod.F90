module aqm_vars_mod

   IMPLICIT NONE

!
! the next are variables that aqmdriver will need
!
!  REAL, PARAMETER             :: epsilc = 1.e-16
!  INTEGER, PARAMETER          :: num_emis_ant = 9
!  INTEGER, PARAMETER          :: num_emis_dust = 5
!  INTEGER, PARAMETER          :: num_emis_seas = 4
!  INTEGER, PARAMETER          :: ne_area = 41
!  INTEGER, PARAMETER          :: nmegan = 1
!   INTEGER, PARAMETER          :: kemit = 1
   INTEGER           :: stepbioe,stepfirepl
   REAL, ALLOCATABLE :: aqm( :, :, :, : )
   REAL, ALLOCATABLE :: emis_ant( :, :, :,:)
   REAL, ALLOCATABLE :: emis_vol( :, :, :,:)
   REAL, ALLOCATABLE :: relhum( :, :, : )
   REAL, ALLOCATABLE :: e_bio( :, :, : )
   REAL, ALLOCATABLE :: dms_0( :, :)
   REAL, ALLOCATABLE :: ttday( :, :)
   REAL, ALLOCATABLE :: tcosz( :, :)
   REAL, ALLOCATABLE :: erod( :, :,:)
   REAL, ALLOCATABLE :: emis_dust( :, :, :,:)
   REAL, ALLOCATABLE :: srce_dust( :, :, :,:) 
   REAL, ALLOCATABLE :: emis_seas( :, :, :,:)
   REAL, ALLOCATABLE :: backg_oh( :, :, : )
   REAL, ALLOCATABLE :: backg_h2o2( :, :, : )
   REAL, ALLOCATABLE :: backg_no3( :, :, : )
   REAL, ALLOCATABLE :: oh_t( :, :, : )
   REAL, ALLOCATABLE :: h2o2_t( :, :, : )
   REAL, ALLOCATABLE :: no3_t( :, :, : )
   REAL, ALLOCATABLE :: ebu_in(  :, :, : )
   REAL, ALLOCATABLE :: ebu( :, :, :, : )
   REAL, ALLOCATABLE :: mean_fct_agtf( :,  : )
   REAL, ALLOCATABLE :: mean_fct_agef( :,  : )
   REAL, ALLOCATABLE :: mean_fct_agsv( :,  : )
   REAL, ALLOCATABLE :: mean_fct_aggr( :,  : )
   REAL, ALLOCATABLE :: firesize_agtf( :,  : )
   REAL, ALLOCATABLE :: firesize_agef( :,  : )
   REAL, ALLOCATABLE :: firesize_agsv( :,  : )
   REAL, ALLOCATABLE :: firesize_aggr( :,  : )
   REAL, ALLOCATABLE :: ash_fall( :,  : )
   REAL, ALLOCATABLE :: dust_fall( :,  : )
   REAL, ALLOCATABLE :: pm2_5_dry( : , : , : )
   REAL, ALLOCATABLE :: pm2_5_water( : , : , : )
   REAL, ALLOCATABLE :: aerwrf( : , : , : )
   REAL, ALLOCATABLE :: pm2_5_dry_ec( : , : , : )
   REAL, ALLOCATABLE :: pm10( : , : , : )
   REAL, ALLOCATABLE :: sebio_iso( : , : )
   REAL, ALLOCATABLE :: sebio_oli( : , : )
   REAL, ALLOCATABLE :: sebio_api( : , : )
   REAL, ALLOCATABLE :: sebio_lim( : , : )
   REAL, ALLOCATABLE :: sebio_xyl( : , : )
   REAL, ALLOCATABLE :: sebio_hc3( : , : )
   REAL, ALLOCATABLE :: sebio_ete( : , : )
   REAL, ALLOCATABLE :: sebio_olt( : , : )
   REAL, ALLOCATABLE :: sebio_ket( : , : )
   REAL, ALLOCATABLE :: sebio_ald( : , : )
   REAL, ALLOCATABLE :: sebio_hcho( : , : )
   REAL, ALLOCATABLE :: sebio_eth( : , : )
   REAL, ALLOCATABLE :: sebio_ora2( : , : )
   REAL, ALLOCATABLE :: sebio_co( : , : )
   REAL, ALLOCATABLE :: sebio_nr( : , : )
   REAL, ALLOCATABLE :: noag_grow( : , : )
   REAL, ALLOCATABLE :: noag_nongrow( : , : )
   REAL, ALLOCATABLE :: nononag( : , : )
   REAL, ALLOCATABLE :: slai( : , : )
   REAL, ALLOCATABLE :: ebio_iso( : , : )
   REAL, ALLOCATABLE :: ebio_oli( : , : )
   REAL, ALLOCATABLE :: ebio_api( : , : )
   REAL, ALLOCATABLE :: ebio_lim( : , : )
   REAL, ALLOCATABLE :: ebio_xyl( : , : )
   REAL, ALLOCATABLE :: ebio_hc3( : , : )
   REAL, ALLOCATABLE :: ebio_ete( : , : )
   REAL, ALLOCATABLE :: ebio_olt( : , : )
   REAL, ALLOCATABLE :: ebio_ket( : , : )
   REAL, ALLOCATABLE :: ebio_ald( : , : )
   REAL, ALLOCATABLE :: ebio_hcho( : , : )
   REAL, ALLOCATABLE :: ebio_eth( : , : )
   REAL, ALLOCATABLE :: ebio_ora2( : , : )
   REAL, ALLOCATABLE :: ebio_co( : , : )
   REAL, ALLOCATABLE :: ebio_nr( : , : )
   REAL, ALLOCATABLE :: ebio_no( : , : )

   !shc stuff for MEGAN v2.04

   real, ALLOCATABLE :: EFmegan(:, : , :)


   real, ALLOCATABLE :: msebio_isop(:, : )
   real, ALLOCATABLE :: pftp_bt(:, : )
   real, ALLOCATABLE :: pftp_nt(:, : )
   real, ALLOCATABLE :: pftp_sb(:, : )
   real, ALLOCATABLE :: pftp_hb(:, : )

   real, ALLOCATABLE :: mlai(:, :, : )
   real, ALLOCATABLE :: mtsa(:, :, : )
   real, ALLOCATABLE :: mswdown(:, :, : )

   real, ALLOCATABLE :: mebio_isop(:, : )
   real, ALLOCATABLE :: mebio_apin(:, : )
   real, ALLOCATABLE :: mebio_bpin(:, : )
   real, ALLOCATABLE :: mebio_bcar(:, : )
   real, ALLOCATABLE :: mebio_acet(:, : )
   real, ALLOCATABLE :: mebio_mbo(:, : )
   real, ALLOCATABLE :: mebio_no(:, : )
!
! stuff for optical driver
!
! next three are for feedback to physics
!
   REAL, ALLOCATABLE :: extt( :, :, :, : )
   REAL, ALLOCATABLE :: ssca( :, :, :, : )
   REAL, ALLOCATABLE :: asympar( :, :, :, : )
!
! output diagnosics
!
   REAL, ALLOCATABLE :: aod( :,: )
   REAL, ALLOCATABLE :: ext_coeff( :, :, :,:)
   REAL, ALLOCATABLE :: bscat_coeff( :, :, :,:)
   REAL, ALLOCATABLE :: asym_par( :, :, :,:)
!
! optical calculations, necessary for feedback to physics and diagnostics
   real, ALLOCATABLE :: tauaersw(:, : , :, : )
   real, ALLOCATABLE :: tauaerlw(:, : , :, : )
   real, ALLOCATABLE :: bscoefsw(:, : , :, : )
   real, ALLOCATABLE :: gaersw(:, : , :, : )
   real, ALLOCATABLE :: waersw(:, : , :, : )
   real, ALLOCATABLE :: tauaer1(:, : , : )
   real, ALLOCATABLE :: tauaer2(:, : , : )
   real, ALLOCATABLE :: tauaer3(:, : , : )
   real, ALLOCATABLE :: tauaer4(:, : , : )
   real, ALLOCATABLE :: gaer1(:, : , : )
   real, ALLOCATABLE :: gaer2(:, : , : )
   real, ALLOCATABLE :: gaer3(:, : , : )
   real, ALLOCATABLE :: gaer4(:, : , : )
   real, ALLOCATABLE :: waer1(:, : , : )
   real, ALLOCATABLE :: waer2(:, : , : )
   real, ALLOCATABLE :: waer3(:, : , : )
   real, ALLOCATABLE :: waer4(:, : , : )
   real, ALLOCATABLE :: bscoef1(:, : , : )
   real, ALLOCATABLE :: bscoef2(:, : , : )
   real, ALLOCATABLE :: bscoef3(:, : , : )
   real, ALLOCATABLE :: bscoef4(:, : , : )
   real, ALLOCATABLE :: l2aer(:, : , : , : )
   real, ALLOCATABLE :: l3aer(:, : , : , : )
   real, ALLOCATABLE :: l4aer(:, : , : , : )
   real, ALLOCATABLE :: l5aer(:, : , : , : )
   real, ALLOCATABLE :: l6aer(:, : , : , : )
   real, ALLOCATABLE :: l7aer(:, : , : , : )
!
! photolysis
!
   real, ALLOCATABLE   :: ph_o31d(:, : , : )
   real, ALLOCATABLE   :: ph_o33p(:, : , : )
   real, ALLOCATABLE   :: ph_no2(:, : , : )
   real, ALLOCATABLE   :: ph_no3o2(:, : , : )
   real, ALLOCATABLE   :: ph_no3o(:, : , : )
   real, ALLOCATABLE   :: ph_hno2(:, : , : )
   real, ALLOCATABLE   :: ph_hno3(:, : , : )
   real, ALLOCATABLE   :: ph_hno4(:, : , : )
   real, ALLOCATABLE   :: ph_h2o2(:, : , : )
   real, ALLOCATABLE   :: ph_ch2or(:, : , : )
   real, ALLOCATABLE   :: ph_ch2om(:, : , : )
   real, ALLOCATABLE   :: ph_ch3cho(:, : , : )
   real, ALLOCATABLE   :: ph_ch3coch3(:, : , : )
   real, ALLOCATABLE   :: ph_ch3coc2h5(:, : , : )
   real, ALLOCATABLE   :: ph_hcocho(:, : , : )
   real, ALLOCATABLE   :: ph_ch3cocho(:, : , : )
   real, ALLOCATABLE   :: ph_hcochest(:, : , : )
   real, ALLOCATABLE   :: ph_ch3o2h(:, : , : )
   real, ALLOCATABLE   :: ph_ch3coo2h(:, : , : )
   real, ALLOCATABLE   :: ph_ch3ono2(:, : , : )
   real, ALLOCATABLE   :: ph_hcochob(:, : , : )
   real, ALLOCATABLE   :: ph_macr(:, : , : )
   real, ALLOCATABLE   :: ph_n2o5(:, : , : )
   real, ALLOCATABLE   :: ph_o2(:, : , : )
   real, ALLOCATABLE   :: ph_pan(:, : , : )
   real, ALLOCATABLE   :: ph_acet(:, : , : )
   real, ALLOCATABLE   :: ph_mglo(:, : , : )
   real, ALLOCATABLE   :: ph_hno4_2(:, : , : )
   real, ALLOCATABLE   ::ph_n2o (:, : , : )
   real, ALLOCATABLE   ::ph_pooh (:, : , : )
   real, ALLOCATABLE   ::ph_mpan (:, : , : )
   real, ALLOCATABLE   ::ph_mvk (:, : , : )
   real, ALLOCATABLE   ::ph_etooh (:, : , : )
   real, ALLOCATABLE   ::ph_prooh (:, : , : )
   real, ALLOCATABLE   ::ph_onitr (:, : , : )
   real, ALLOCATABLE   ::ph_acetol (:, : , : )
   real, ALLOCATABLE   ::ph_glyald (:, : , : )
   real, ALLOCATABLE   ::ph_hyac (:, : , : )
   real, ALLOCATABLE   ::ph_mek (:, : , : )
   real, ALLOCATABLE   ::ph_open (:, : , : )
   real, ALLOCATABLE   ::ph_gly (:, : , : )
   real, ALLOCATABLE   ::ph_acetp (:, : , : )
   real, ALLOCATABLE   ::ph_xooh (:, : , : )
   real, ALLOCATABLE   ::ph_isooh (:, : , : )
   real, ALLOCATABLE   ::ph_alkooh (:, : , : )
   real, ALLOCATABLE   ::ph_mekooh (:, : , : )
   real, ALLOCATABLE   ::ph_tolooh (:, : , : )
   real, ALLOCATABLE   ::ph_terpooh (:, : , : )
   real, ALLOCATABLE   ::ph_cl2 (:, : , : )
   real, ALLOCATABLE   ::ph_hocl (:, : , : )
   real, ALLOCATABLE   ::ph_fmcl (:, : , : )
!
! stuff for more complpex aerosols and gas phase aqm
!
real      ,ALLOCATABLE   :: h2oaj(:,:,:)
real      ,ALLOCATABLE   :: h2oai(:,:,:)
real      ,ALLOCATABLE   :: nu3(:,:,:)
real      ,ALLOCATABLE   :: ac3(:,:,:)
real      ,ALLOCATABLE   :: cor3(:,:,:)
real      ,ALLOCATABLE   :: asulf(:,:,:)
real      ,ALLOCATABLE   :: ahno3(:,:,:)
real      ,ALLOCATABLE   :: anh3(:,:,:)
real      ,ALLOCATABLE   :: cvaro1(:,:,:)
real      ,ALLOCATABLE   :: cvaro2(:,:,:)
real      ,ALLOCATABLE   :: cvalk1(:,:,:)
real      ,ALLOCATABLE   :: cvole1(:,:,:)
real      ,ALLOCATABLE   :: cvapi1(:,:,:)
real      ,ALLOCATABLE   :: cvapi2(:,:,:)
real      ,ALLOCATABLE   :: cvlim1(:,:,:)
real      ,ALLOCATABLE   :: cvlim2(:,:,:)
real      ,ALLOCATABLE   :: mob(:,:,:)
real      ,ALLOCATABLE   :: cvasoaX(:,:,:)
real      ,ALLOCATABLE   :: cvasoa1(:,:,:)
real      ,ALLOCATABLE   :: cvasoa2(:,:,:)
real      ,ALLOCATABLE   :: cvasoa3(:,:,:)
real      ,ALLOCATABLE   :: cvasoa4(:,:,:)
real      ,ALLOCATABLE   :: cvbsoaX(:,:,:)
real      ,ALLOCATABLE   :: cvbsoa1(:,:,:)
real      ,ALLOCATABLE   :: cvbsoa2(:,:,:)
real      ,ALLOCATABLE   :: cvbsoa3(:,:,:)
real      ,ALLOCATABLE   :: cvbsoa4(:,:,:)
real      ,ALLOCATABLE   :: asoa1j(:,:,:)
real      ,ALLOCATABLE   :: asoa1i(:,:,:)
real      ,ALLOCATABLE   :: asoa2j(:,:,:)
real      ,ALLOCATABLE   :: asoa2i(:,:,:)
real      ,ALLOCATABLE   :: asoa3j(:,:,:)
real      ,ALLOCATABLE   :: asoa3i(:,:,:)
real      ,ALLOCATABLE   :: asoa4j(:,:,:)
real      ,ALLOCATABLE   :: asoa4i(:,:,:)
real      ,ALLOCATABLE   :: bsoa1j(:,:,:)
real      ,ALLOCATABLE   :: bsoa1i(:,:,:)
real      ,ALLOCATABLE   :: bsoa2j(:,:,:)
real      ,ALLOCATABLE   :: bsoa2i(:,:,:)
real      ,ALLOCATABLE   :: bsoa3j(:,:,:)
real      ,ALLOCATABLE   :: bsoa3i(:,:,:)
real      ,ALLOCATABLE   :: bsoa4j(:,:,:)
real      ,ALLOCATABLE   :: bsoa4i(:,:,:)
real      ,ALLOCATABLE   :: addt(:,:,:)
real      ,ALLOCATABLE   :: addx(:,:,:)
real      ,ALLOCATABLE   :: addc(:,:,:)
real      ,ALLOCATABLE   :: etep(:,:,:)
real      ,ALLOCATABLE   :: oltp(:,:,:)
real      ,ALLOCATABLE   :: olip(:,:,:)
real      ,ALLOCATABLE   :: cslp(:,:,:)
real      ,ALLOCATABLE   :: limp(:,:,:)
real      ,ALLOCATABLE   :: hc5p(:,:,:)
real      ,ALLOCATABLE   :: hc8p(:,:,:)
real      ,ALLOCATABLE   :: tolp(:,:,:)
real      ,ALLOCATABLE   :: xylp(:,:,:)
real      ,ALLOCATABLE   :: apip(:,:,:)
real      ,ALLOCATABLE   :: isop(:,:,:)
real      ,ALLOCATABLE   :: hc3p(:,:,:)
real      ,ALLOCATABLE   :: ethp(:,:,:)
real      ,ALLOCATABLE   :: o3p(:,:,:)
real      ,ALLOCATABLE   :: tco3(:,:,:)
real      ,ALLOCATABLE   :: mo2(:,:,:)
real      ,ALLOCATABLE   :: o1d(:,:,:)
real      ,ALLOCATABLE   :: olnn(:,:,:)
real      ,ALLOCATABLE   :: olnd(:,:,:)
real      ,ALLOCATABLE   :: rpho(:,:,:)
real      ,ALLOCATABLE   :: xo2(:,:,:)
real      ,ALLOCATABLE   :: ketp(:,:,:)
real      ,ALLOCATABLE   :: xno2(:,:,:)
real      ,ALLOCATABLE   :: ol2p(:,:,:)
real      ,ALLOCATABLE   :: oln(:,:,:)
real      ,ALLOCATABLE   :: macp(:,:,:)
real      ,ALLOCATABLE   :: hocoo(:,:,:)
real      ,ALLOCATABLE   :: bzno2_o(:,:,:)
real      ,ALLOCATABLE   :: bz_o(:,:,:)
real      ,ALLOCATABLE   :: tbu_o(:,:,:)
real      ,ALLOCATABLE   :: vprm_in(:,:,:,:)
real      ,ALLOCATABLE   :: rad_vprm(:)
real      ,ALLOCATABLE   :: lambda_vprm(:)
real      ,ALLOCATABLE   :: alpha_vprm(:)
real      ,ALLOCATABLE   :: resp_vprm(:)
real      ,ALLOCATABLE   :: wet_in(:,:,:,:)
real      ,ALLOCATABLE   :: biomt_par(:)
real      ,ALLOCATABLE   :: emit_par(:)

!real      ,allocatable   :: rcav(:)
!real      ,allocatable   :: rnav(:)
#if 0
real      ,allocatable   :: rc2d(:)
real      ,allocatable   :: rn2d(:)
#else
real      ,pointer   :: rc2d(:)
real      ,pointer   :: rn2d(:)
#endif

! -- input variables
! -- include:
! -- cntl/module_sfc_variables.F90
#if 0
real      ,allocatable :: sm3d(:,:)   ! soil moisture
real,allocatable :: ts2d(:)             ! skin temperature
real,allocatable :: us2d(:)             ! friction velocity/equivalent momentum flux
real,allocatable :: rsds(:),rsds_ave(:) ! radiation SW downward at surface, snapshot/averaged over ArchvIntvl
real,allocatable :: vfrac2d(:)
real,allocatable :: vtype2d(:)
real,allocatable :: stype2d(:)
real,allocatable :: snwdph2d(:)
real,allocatable :: slmsk2d(:)          ! surface land mask, 0/1/2: ocean/land/ice
real,allocatable :: zorl2d(:)
real,allocatable :: hf2d(:),  hf_ave(:) ! sensible heatflux, snapshot/averaged over ArchvIntvl (W/m2)
#else
real,pointer :: sm3d(:,:)   ! soil moisture
real,pointer :: ts2d(:)             ! skin temperature
real,pointer :: us2d(:)             ! friction velocity/equivalent momentum flux
real,pointer :: rsds(:) ! radiation SW downward at surface, snapshot/averaged over ArchvIntvl
real,allocatable :: rsds_ave(:) ! radiation SW downward at surface, snapshot/averaged over ArchvIntvl
real,pointer :: vfrac2d(:)
real,pointer :: vtype2d(:)
real,pointer :: stype2d(:)
real,pointer :: snwdph2d(:)
real,pointer :: slmsk2d(:)          ! surface land mask, 0/1/2: ocean/land/ice
real,pointer :: zorl2d(:)
real,pointer :: hf2d(:) ! sensible heatflux, snapshot/averaged over ArchvIntvl (W/m2)
real,allocatable :: hf_ave(:) ! sensible heatflux, snapshot/averaged over ArchvIntvl (W/m2)
#endif

! -- cntl/module_variables.F90

#if 0
real,pointer     :: us3d  (:,:)   ! zonal wind (m/s)
real,pointer     :: vs3d  (:,:)   ! meridional wind (m/s)
real,pointer     :: ws3d  (:,:)   ! vertical wind (Pa/s)
real,allocatable :: dp3d  (:,:)   ! del p between coord levels (pascals)
real,allocatable :: dpinit(:,:)   ! lyr thknss for class B tracer transport
real,allocatable :: mp3d  (:,:)   ! Montgomery Potential (m^2/s^2)
real,allocatable :: tk3d  (:,:)   ! temperature, kelvin
real,allocatable :: dcudt (:,:)   ! convective tendency from GF used for GWD
real,allocatable :: dcudq (:,:)   ! convective tendency from GF used for GWD
real,allocatable :: dcudu (:,:)   ! convective tendency from GF used for GWD
real,allocatable :: dcudv (:,:)   ! convective tendency from GF used for GWD
real,allocatable :: relvor(:,:)   ! relative vorticity (s^-1)
real,allocatable :: potvor(:,:)   ! potential vorticity
real,pointer     :: tr3d  (:,:,:) ! 1=pot.temp,2=water vapor,3=cloud water,4=ozone
real,allocatable :: trdp  (:,:,:) ! (tracer x thknss) for tracer transport eq.
real,allocatable :: rh3d  (:,:)   ! relative humidity from 0 to 1
real,allocatable :: qs3d  (:,:)   ! saturation specific humidity
#else
real,pointer     :: us3d  (:,:)   ! zonal wind (m/s)
real,pointer     :: vs3d  (:,:)   ! meridional wind (m/s)
real,pointer     :: ws3d  (:,:)   ! vertical wind (Pa/s)
real,allocatable :: dp3d  (:,:)   ! del p between coord levels (pascals)
real,allocatable :: dpinit(:,:)   ! lyr thknss for class B tracer transport
real,allocatable :: mp3d  (:,:)   ! Montgomery Potential (m^2/s^2)
real,pointer     :: tk3d  (:,:)   ! temperature, kelvin
real,allocatable :: dcudt (:,:)   ! convective tendency from GF used for GWD
real,allocatable :: dcudq (:,:)   ! convective tendency from GF used for GWD
real,allocatable :: dcudu (:,:)   ! convective tendency from GF used for GWD
real,allocatable :: dcudv (:,:)   ! convective tendency from GF used for GWD
real,allocatable :: relvor(:,:)   ! relative vorticity (s^-1)
real,allocatable :: potvor(:,:)   ! potential vorticity
real,pointer     :: tr3d  (:,:,:) ! 1=pot.temp,2=water vapor,3=cloud water,4=ozone
real,allocatable :: trdp  (:,:,:) ! (tracer x thknss) for tracer transport eq.
real,allocatable :: rh3d  (:,:)   ! relative humidity from 0 to 1
real,allocatable :: qs3d  (:,:)   ! saturation specific humidity
#endif

! -- misc
#if 0
real,allocatable ::  area     (    :) ! the area of cell polygon (m**2)
#else
real,pointer ::  area     (    :) ! the area of cell polygon (m**2)
#endif
real,allocatable ::  rarea    (    :) ! reciprocal of the "area"
!       This module specifies aqm variables.  
#if 0
real,allocatable :: rcav(:)       ! accumulated convective precipitation since last aqm call
real,allocatable :: rnav(:)       ! accumulated ls precipitation since last aqm call
#else
real,pointer :: rcav(:)       ! accumulated convective precipitation since last aqm call
real,pointer :: rnav(:)       ! accumulated ls precipitation since last aqm call
#endif
! WRF physics and aqm parameters
!real,allocatable ::  deg_lat(:),deg_lon(:)      ! lat and lon in degrees
real,pointer ::  deg_lat(:),deg_lon(:)      ! lat and lon in degrees
real,allocatable :: dm0(:)        !  dms reference emissions
real,allocatable :: emiss_ash_dt(:) !  emissions for 
real,allocatable :: emiss_ash_height(:) !  emissions for 
real,allocatable :: emiss_ash_mass(:) !  emissions for 
real,allocatable :: emiss_tr_dt(:) !  emissions for 
real,allocatable :: emiss_tr_height(:) !  emissions for 
real,allocatable :: emiss_tr_mass(:) !  emissions for 
real,allocatable :: ero1(:)       !  dust erosion factor
real,allocatable :: ero2(:)       !  dust erosion factor
real,allocatable :: ero3(:)       !  dust erosion factor
#if 0
real,allocatable :: exch  (:,:)   ! exchange coeffs
#else
real,pointer :: exch  (:,:)   ! exchange coeffs
#endif
real,allocatable :: h2o2_backgd(:,:) ! H2O2 background for GOCART
real,allocatable :: no3_backgd(:,:) ! NO3 background for GOCART
! integer :: nvl                   ! Number of vertical native levels
real,allocatable :: oh_backgd(:,:) ! OH background for GOCART
#if 0
real,allocatable :: pb2d(:)       ! Boundary layer height
real,allocatable :: ph3d(:,:)     ! geopotential (=gz), m^2/s^2
#else
real,pointer :: pb2d(:)       ! Boundary layer height
real,pointer :: ph3d(:,:)     ! geopotential (=gz), m^2/s^2
#endif
real,pointer     :: pr3d(:,:)     ! pressure (pascal)
real   ,allocatable :: th_pvsrf(:  ) ! pot.temp on pot.vorticty surface

! -- physics
   REAL, ALLOCATABLE :: convfac ( : , : , : )
   REAL, ALLOCATABLE :: dxy( :, :)
   REAL, ALLOCATABLE :: exch_h( : , : , : )
   REAL, ALLOCATABLE :: gsw( : , : )
   REAL, ALLOCATABLE :: hfx( : , : )
   INTEGER, ALLOCATABLE :: isltyp( : , : )
   INTEGER, ALLOCATABLE :: ivgtyp( : , : )
   REAL, ALLOCATABLE :: moist( :, :, :, : )
   REAL, ALLOCATABLE :: p8w( : , : , : )
   REAL, ALLOCATABLE :: pbl( : , : )
   REAL, ALLOCATABLE :: p_phy( : , : , : )
   REAL, ALLOCATABLE :: rmol( : , : )
   REAL, ALLOCATABLE :: rri( : , : , : )
   REAL, ALLOCATABLE :: t8w( : , : , : )
   REAL, ALLOCATABLE :: t_phy( : , : , : )
   REAL, ALLOCATABLE :: tsk( :, :)
   REAL, ALLOCATABLE :: u10( : , : )
   REAL, ALLOCATABLE :: u_phy( : , : , : )
   REAL, ALLOCATABLE :: ust( : , : )
   REAL, ALLOCATABLE :: v10( : , : )
   REAL, ALLOCATABLE :: vegfra( : , : )
   REAL, ALLOCATABLE :: v_phy( : , : , : )
   REAL, ALLOCATABLE :: xland( : , : )
   REAL, ALLOCATABLE :: xlat( : , : )
   REAL, ALLOCATABLE :: xlong( : , : )
   REAL, ALLOCATABLE :: z_at_w ( : , : , : )
   REAL, ALLOCATABLE :: znt( : , : )
   REAL, ALLOCATABLE :: dz8w( : , : , : )
   REAL, ALLOCATABLE :: rho_phy( : , : , : )
   REAL, ALLOCATABLE :: smois( :, :, : )
   REAL, ALLOCATABLE :: vvel( : , : , : )
   REAL, ALLOCATABLE :: zmid ( : , : , : )
   REAL, ALLOCATABLE :: snowh( : , : )
   REAL, ALLOCATABLE :: clayf( : , : )
   REAL, ALLOCATABLE :: sandf( : , : )
   REAL, ALLOCATABLE :: gd_cloud( :, : , : )
   REAL, ALLOCATABLE :: gd_cldfr( :, : , : )

  ! -- aqmistry
  real,allocatable :: clayfrac(:)   !  clay fraction (AFWA dust saqme)
  real,allocatable :: emi_d1(:) !  emissions for dust 
  real,allocatable :: emi_d2(:) !  emissions for dust 
  real,allocatable :: emi_d3(:) !  emissions for dust 
  real,allocatable :: emi_d4(:) !  emissions for dust 
  real,allocatable :: emi_d5(:) !  emissions for dust 
  real,allocatable :: emiss_ab(:,:) !  emissions for all available species
  real,allocatable :: emiss_abu(:,:)!  emissions for all available species
  real,allocatable :: dustfall(:)   !  dust fall (g/m2)
  real,allocatable :: ashfall(: )   !  volcanic ash fall (g/m2)
  real,allocatable :: plumestuff(:,:) !  fire info
  real,allocatable :: sandfrac(:)   !  sand fraction (AFWA dust saqme)
  real,allocatable :: ext_cof (:,:,:)   ! aerosol extinction coefficients
  real,allocatable :: sscal (:,:,:)   ! aerosol single scattering albedo
  real,allocatable :: asymp (:,:,:)   ! aerosol asymetry parameter
  real,allocatable :: aod2d(:)   !  aerosol optical depth
  real,allocatable :: pm25  (:,:)   ! pm2.5
  real,allocatable :: emiss_pm25(:) !  emissions for unspeciated pm25
  real,allocatable :: p10   (:,:)   ! pm10
  real,allocatable :: ebu_oc(:,:) !  fire OC 3d EMISSION
  real,allocatable :: oh_bg(:,:) !  oh gocart backgound 
  real,allocatable :: h2o2_bg(:,:) !  h2o2 gocart backgound 
  real,allocatable :: no3_bg(:,:) !  no3 gocart backgound
  real,allocatable :: wet_dep(:,:)  !  wet depostion  

  real,allocatable :: tr1_tavg (:,:)! tracer time average
  real,allocatable :: d1st_ave(:,:) !  dust avg. 
  real,allocatable :: d2st_ave(:,:) !  dust avg. 
  real,allocatable :: d3st_ave(:,:) !  dust avg. 
  real,allocatable :: d4st_ave(:,:) !  dust avg. 
  real,allocatable :: d5st_ave(:,:) !  dust avg. 
  real,allocatable :: dry_dep(:,:)  !  dry depostion  
  real,allocatable :: emiss_ab1(:,:) !  emissions for all available species
  real,allocatable :: emiss_co2(:)   !  emissions for co2
  real,allocatable :: emiss_ch4(:)   !  emissions for ch4
  real,allocatable :: emiss_sf6(:)   !  emissions for sf6
  real,allocatable :: trfall(:,:)  !  emissions for all available species
  real,allocatable :: emiss_oc(:)   !  emissions for organic carbon
  real,allocatable :: emiss_bc(:)   !  emissions for black carbon
  real,allocatable :: emiss_sulf(:) !  emissions for sulfate
  real,allocatable :: emiss_pm10(:) !  emissions for unspeciated pm10
  real,allocatable :: emid1_ave(:) !  emissions for dust 
  real,allocatable :: emid2_ave(:) !  emissions for dust 
  real,allocatable :: emid3_ave(:) !  emissions for dust 
  real,allocatable :: emid4_ave(:) !  emissions for dust 
  real,allocatable :: emid5_ave(:) !  emissions for dust 
  real,allocatable :: aod2d_ave(:) !  AOD 

  ! -- aqmistry prep
  real, allocatable :: raincv_b(:,:)

  ! -- sea salt
  real, allocatable :: seashelp(:,:)

  ! -- grid
  integer, allocatable :: perm(:)
  real, pointer :: loc_perm(:)
! integer, allocatable :: indx(:), map(:)
  

  real :: p_gocart (56)          ! list of gocart pressure levels (module_aqm_constants)

  public

end module aqm_vars_mod
