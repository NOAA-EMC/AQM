
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE PHOT ( CGRID, JDATE, JTIME, DTSTEP )

!-----------------------------------------------------------------------
!
! Function:  Calculates the photolysis rate constant to be used by the
!     chemical solver.  It calculates these rates at each gridcell using
!     codes adapted from JPROC.  Cloud correction now called within the
!     loops over MY-ROW & MY_COLS
!
! Preconditions: HGRD_INIT() called from PAR_INIT, which is called from
!     DRIVER
!
! Subroutines/Functions called: M3EXIT, SUBHFILE, CGRID_MAP,
!     OPPHOT, LOAD_CSQY_DATA, LOAD_OPTICS_DATA, INITIALIZE_ALBEDO, 
!     GET_PHOT_MET, UPDATE_SUN, GET_ALBEDO, GET_DROPLET_OPTICS, 
!     GET_ICE_OPTICS, GET_AGGREGATE_OPTICS, CLEAR_HYDROMETEOR_OPTICS, 
!     GET_AERO_DATA, O3TOTCOL, NEW_OPTICS, GET_ENVLIST,
!     init_can_mask,init_can_levs, init_can_trans,
!     get_can_mask, get_can_levs, and canopy_transfer
!
! Revision History.
!     Started 10/08/2004 with existing PHOT and JPROC coded by
!         Dr. Francis S. Binkowski
!         Carolina Environmental Program
!         University of North Carolina at Chapel Hill
!         email: frank_binkowski@unc.edu
!     August 2005, Sarav Arunachalam, CEP, UNC-CH
!       - Minor revisions while integrating with CMAQ
!       - Error check for NPHOTS added (this version works only for SAPRC-99)
!       - Added creation of new file CTM_RJ_1 to write out RJ values
!         for O3 and NO2 (both clear sky and cloud effects), and
!         ETOT_SFC, TAU_TOT and TAUO3_TOP values for 7 wavelengths
!     June 2007, David Wong
!       -- inline with CMAQ
!       - declare RJ as assumed shape array to match with the caller routine
!       - allow PE 0 only to open the output file
!       - output species: NO2_CLOUD and O3_CLOUD with AMISS value when all cells
!         are dark and JTIME_CHK = 0
!       - output species: NO2_CLOUD and O3_CLOUD with AMISS value when CLDATT is
!         0 and JTIME_CHK = 0
!     December 2007, Francis Binkowski
!         code has been modified to call the new on-line version that
!         has the cloud effects built in.  new photolysis routine to
!         replace PHOT in CMAQ
!     January 2008, Shawn Roselle
!       - reformatted for inclusion in CMAQ
!       - added additional 3-d photolysis rate diagnostic file
!       - moved code for opening the diagnostic files to a separate subroutine
!       - moved aerosol pointer evaluation to a FORTRAN module
!       - simplified code for writing the diagnostic file
!       - changed code to call NEW_OPTICS twice, once for clear sky and
!         another time for the cloudy fraction of the grid cell.  RJ's are
!         computed based on the cloud fraction weighting.
!      March 2011, Bill Hutzell
!       - enable wavelength dependent arrays to have an allocatable number
!         of wavelength bins
!       - added data structure and algorithm to compute a surface albedo that
!         depends on time and landuse catagory based on work by John Striecher
!         (AMAD/USEPA)
!       - revised writing to RJ1 file to include surface albedo
!       - moved photolysis and opacity data from CSQY module to an ASCII input
!         file
!       - added routine called LOAD_REF_DATA (inside the PHOT_MOD module) that i
!         reads this input file
!       - added call to a routine called AERO_PHOTDATA that returns opacity data
!         on the aerosol distribution
!       - revised NEW_OPTICS' arguments based on aerosol redesign in CMAQ
!         version 5.0
!     March 29, 2011 S.Roselle
!       - Replaced I/O API include files with UTILIO_DEFN
!     07 Jul 14 B.Hutzell: replaced mechanism include file(s) with fortran module
!     26 Sep 14 B.Hutzell: 1) moved calculation of surface albedo to its own
!                             fortran module
!                          2) changed loading procedure for loading optical data;
!                             two files now used
!                          3) reading and calculation of met and geo data
!                             now acomplished by a fortran module
!                          4) changed description and accounting of cloud effects
!                             from 2D liquid water clouds to 3D resolved and subgrid 
!                             clouds with multi-phases of water
!                          5) inserted calculation of aerosol optical properties via
!                             fortran module to improve efficiency in radiative
!                             transfer solution
!                          6) moved the O3TOTCOL routine from the PHOT_MOD to simplify
!                             the NEW_OPTICS routine
!                          7) Several miscellaneous changes attempting to improve efficiency
!     10 Jun 15 J.Young: Modified diagnostic output timestamp to fix for other than one
!                         hour time steps.
!     12 Aug 15 D.Wong:  Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O implementation
!     01 Feb 19 D.Wong: Implemented centralized I/O approach, removed all MY_N clauses
!     01 Mar 23 P.Campbell 
!         - Introduced integrated canopy shading effect on photolysis attenuation 
!           as descrived in Wang, C-T et al. (2025, ACP) "Quantifying Forest Canopy 
!           Shading and Turbulence Effects on Boundary Layer Ozone over the United States" 
!           and based on approximated version of Makar, P. A. et al. (2017, Nat. Commun.)
!         - Applied canopy attenuation of photolysis rates based on novel vegetative canopy data
!           (forest canopy height FCH, forest fraction, leaf area index LAI, clumping index, and
!           population density) and 5-layer cumulative LAI fractions
!     01 Mar 24 I.Ivanova 
!         - Update the integrated canopy shading effect with explicit 3 sub-layer canopy 
!           based on Makar, P. A. et al. (2017) "The effects of forest canopy shading 
!           and turbulence on boundary layer ozone." Nat. Commun. 8, 15243 
!           doi: 10.1038/ncomms15243 (2017).
!     25 Aug 24 I.Ivanova
!         - Introduce various ozone diagnostics and auxilliary diagnostics
!----------------------------------------------------------------------

C...modules

      USE RUNTIME_VARS, ONLY : START_DATE => STDATE, START_TIME => STTIME
      USE RXNS_DATA            ! chemistry varaibles and data
      USE GRID_CONF            ! horizontal & vertical domain specifications
      USE CGRID_SPCS           ! CGRID species number and offsets
      USE UTILIO_DEFN
      USE AERO_DATA            ! describes aerosol distribution
      USE PHOT_MOD             ! photolysis in-line module - inherits CSQY_DATA module
      USE AERO_PHOTDATA        ! arrays and routines for aerosol dimensions and refractive indices
      USE PHOTOLYSIS_ALBEDO    ! surface albedo data and routines
      USE PHOT_MET_DATA        ! Met and Grid data
      USE CLOUD_OPTICS         ! data and routines for optics of cloud hydrometeors
      USE SEAS_STRAT_O3_MIN    ! monthly minimum fraction of ozone column density above Pressure TOP
      !Used for canopy shade calculation
      USE ASX_DATA_MOD, ONLY : MET_DATA, GRID_DATA,    !uses met data
     &                         CANOPY_SHADE            ! IVAI: canopy inline option
      USE CENTRALIZED_IO_MODULE, ONLY : LAT, LON, HT, AREA ! IVAI: area
!IVAI
! NB. NLAYC/NLAYT definitions in GRID_CONF (Jan 9 2025)
      use can_mask             ! out: FRT_mask
      use can_levs_defn
      use can_trans_mod
!IVAI
      USE centralized_io_util_module, ONLY: IntegrateTrapezoid,interp_linear1_internal
      USE ELMO_DATA, ONLY : ELMO_AOD_550, ELMO_EXT_550

#ifdef parallel
      USE SE_MODULES           ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES         ! stenex (using NOOP_UTIL_MODULE)
#endif

      IMPLICIT NONE

!...include files

      INCLUDE SUBST_FILES_ID   ! file name parameters

!...arguments

      REAL,    POINTER      :: CGRID( :,:,:,: )  ! Species concentrations
      INTEGER, INTENT( IN ) :: JDATE         ! current Julian date (YYYYDDD)
      INTEGER, INTENT( IN ) :: JTIME         ! current time (HHMMSS)
      INTEGER, INTENT( IN ) :: DTSTEP( : )   ! time step vector (HHMMSS)

!...parameters

      LOGICAL, PARAMETER :: CLDATT = .TRUE.   ! include cloud attenuation

      REAL, PARAMETER :: DENS_CONV = ( 1.0E+03 * AVO / MWAIR ) * 1.0E-06  ! convert from kg/m**3 to #/cc
      REAL, PARAMETER :: PPM_MCM3  = 1.0E-06  ! convert from ppm to molecules / cc mol_Spec/mol_Air = ppm * 1E-06
      REAL, PARAMETER :: PRES_CONV = 1.0 / STDATMPA ! conversion factor Pa to atm
      REAL, PARAMETER :: ZTOA      = 50.0E3   ! height of top of atmosphere [ m ] (=50km)
                                              ! based a 2005 WRF model Documentation
      REAL, PARAMETER   :: EPSLON  = 1.0E-30  ! Small number

!...external functions: none

!...local variables

      LOGICAL, SAVE :: FIRSTIME = .TRUE.  ! Flag for first call to PHOT

      LOGICAL, SAVE :: CALL_INIT_ALBEDO = .TRUE.
      LOGICAL, SAVE :: CALL_GET_ALBEDO  = .TRUE.
      
      LOGICAL       :: ZERO_ICE

      CHARACTER(   3 ), ALLOCATABLE, SAVE :: WLTXT( : )
      CHARACTER(  16 )                    :: VARNM
      CHARACTER(  16 ), SAVE              :: PNAME = 'PHOT'
      CHARACTER(  16 )                    :: V_LIST( 2 )
      CHARACTER(  16 )                    :: REQUESTED_WAVE
      CHARACTER(  16 ), ALLOCATABLE       :: WAVE_LIST( : )

      CHARACTER(  80 ) :: VARDESC  ! environment variable description
      CHARACTER( 240 ) :: XMSG = ' '

      INTEGER,    SAVE :: LGC_O3   = 0 ! pointer to O3 in CGRID
      INTEGER,    SAVE :: LGC_NO2  = 0 ! pointer to NO2 in CGRID
      INTEGER,    SAVE :: LGC_CO   = 0 ! pointer to CO in CGRID
      INTEGER,    SAVE :: LGC_SO2  = 0 ! pointer to SO2 in CGRID
      INTEGER,    SAVE :: LGC_HCHO = 0 ! pointer to formaldehyde  in CGRID
      INTEGER,    SAVE :: TSTEP    ! output timestep in sec

      INTEGER ESTAT                ! status from environment var check
      INTEGER IPHOT                ! photolysis rate loop index
      INTEGER ROW
      INTEGER COL
      INTEGER LEV, LAY
      INTEGER SPC
      INTEGER IWL
      INTEGER L
      INTEGER V, N, MODE

      LOGICAL :: JTIME_CHK   ! To check for JTIME to write RJ values
      INTEGER, SAVE :: ODATE ! output date
      INTEGER, SAVE :: OTIME ! output time
      INTEGER, SAVE :: OSTEP ! time since last write diagnostics

      INTEGER ALLOCSTAT

      INTEGER ITMSTEP              ! one half synchronization timestep (sec)
      INTEGER MIDDATE              ! Date at time step midpoint
      INTEGER MIDTIME              ! Time at time step midpoint

      INTEGER, SAVE :: TDATE
      INTEGER, SAVE :: PECOL_OFFSET        ! Local Column Offset for processor
      INTEGER, SAVE :: PEROW_OFFSET        ! Local Column Offset for processor
      INTEGER, SAVE :: TSTEP_COUNT         ! counter between calls to write diagnostics

      REAL CURRHR          ! current GMT hour
      REAL JULIAN_DAY      ! time of year [days]
      REAL CURRHR_LST      ! local standard time at each grid cell
      REAL CTOP            ! cloud top in single dimension
      REAL CBASE           ! cloud base in single dimension
      REAL ZLEV            ! height in single dimension
      REAL ZEN             ! cosine of zenith angle
      REAL SINLAT          ! sine of latitude
      REAL COSLAT          ! cosine of latitude
      REAL RSQD            ! square of soldist
      REAL ZSFC            ! surface height (msl) [ m ]
      REAL EQT             ! equation of time
      REAL SOLDIST         ! solar distance [ au ]
      REAL SINDEC          ! sine of the solar declination
      REAL COSDEC          ! cosine of the solar declination
      REAL COSZEN          ! working cosine of the solar zenith angle
      REAL SINZEN          ! working sine of the solar zenith angle
      REAL LATCR           ! local latitude
      REAL LONCR           ! local longitude
      REAL OWATER_FRAC     ! Open water fraction
      REAL SNOW_FRAC       ! Snow fractional coverage
      REAL SEAICE_FRAC     ! Sea Ice fraction
      REAL RES_SKY_REFLECT ! reflection coefficient based on resolved sky
      REAL RES_SKY_TRANS   ! diffuse transmission coefficient based on resolved sky
      REAL RES_SKY_TRANSD  ! direct transmission coefficient based on resolved sky

      REAL                    :: TOTAL_O3_COLUMN ! total ozone column density, DU

      REAL,              SAVE :: JYEAR          = 0.0 ! year
      REAL,              SAVE :: JD_STRAT_O3MIN = 0.0 ! Julian day (YYYYDDD) of min fraction for stratos ozone

      INTEGER, PARAMETER  :: DAYS( 12 ) = (/ 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 /)
      INTEGER, SAVE       :: IMONTH = 0

      REAL, ALLOCATABLE, SAVE :: ETOT_SFC ( : )    ! total downward irradiance at sfc [ Watts / m**2  ]
      REAL, ALLOCATABLE, SAVE :: TAUO3_TOP( : )    ! optical depth of ozone above model domain
      REAL, ALLOCATABLE, SAVE :: TAU_RAY  ( : )    ! Rayleigh optical depth above model domain
      REAL, ALLOCATABLE, SAVE :: TAUC_AERO( :,: )  ! aerosol optical depth at layer bottom
      REAL, ALLOCATABLE, SAVE :: TAU_TOT  ( :,: )  ! total optical depth at layer bottom
      REAL, ALLOCATABLE, SAVE :: TAU_CLOUD( :,: )  ! cloud optical depth at layer bottom

      REAL, ALLOCATABLE, SAVE :: SSA      ( : ) ! aerosol single scattering albedo, column average

      REAL MSCALE          ! combined factor to scale ppm to Molecules / cm**3
                           ! and correct for ambient temperaure and pressure

! FSB new arrays for new on-line cloud version

      REAL, ALLOCATABLE, SAVE :: LWC    ( : )      ! cloud liquid water content [ g/m**3 ]
      REAL, ALLOCATABLE, SAVE :: RWC    ( : )      ! rain water content [ g/m**3 ]
      REAL, ALLOCATABLE, SAVE :: IWC    ( : )      ! ice liquid water content [ g/m**3 ]
      REAL, ALLOCATABLE, SAVE :: SWC    ( : )      ! snow content [ g/m**3 ]
      REAL, ALLOCATABLE, SAVE :: GWC    ( : )      ! graupel content [ g/m**3 ]
      REAL, ALLOCATABLE, SAVE :: CLDFRAC( : )      ! fractional cloud cover
      REAL, ALLOCATABLE, SAVE :: BLKPRS ( : )      ! Air pressure in [ Pa ]
      REAL, ALLOCATABLE, SAVE :: BLKTA  ( : )      ! Air temperature [ K ]
      REAL, ALLOCATABLE, SAVE :: BLKDENS( : )      ! Air density  [ molecules / m**3 ]
      REAL, ALLOCATABLE, SAVE :: BLKZH  ( : )      ! layer half-height [ m ]
      REAL, ALLOCATABLE, SAVE :: BLKO3  ( : )      ! O3 concentration [ molecules/cm**3 ]
      REAL, ALLOCATABLE, SAVE :: BLKNO2 ( : )      ! NO2 concentration [ molecules/cm**3 ]
      REAL, ALLOCATABLE, SAVE :: BLKZF  ( : )      ! layer full-height [ m ]

      REAL, ALLOCATABLE, SAVE :: BLKRJ_RES( :, : ) ! photolysis rates
      REAL, ALLOCATABLE, SAVE :: BLKRJ_ACM( :, : ) ! photolysis rates

      LOGICAL, ALLOCATABLE, SAVE :: CLOUDS( : )    ! Does layer have clouds?
      LOGICAL                    :: NEW_PROFILE    ! Has atmospheric temperature and density profile changed?
      LOGICAL                    :: DARK           ! Are this processor's cells in darkness?

! Canopy new_optics arrays
      REAL MSCALE_CAN      ! combined factor to scale ppm to Molecules / cm**3
                           ! and correct for ambient temperaure and pressure
      REAL, ALLOCATABLE, SAVE  :: O3_PPM       (:)
      REAL, ALLOCATABLE, SAVE  :: NO2_PPM      (:)
      REAL, ALLOCATABLE, SAVE  :: BLKTA_CAN    (:)
      REAL, ALLOCATABLE, SAVE  :: BLKPRS_CAN   (:)
      REAL, ALLOCATABLE, SAVE  :: BLKDENS_CAN  (:)
      REAL, ALLOCATABLE, SAVE  :: BLKZH_CAN    (:)
      REAL, ALLOCATABLE, SAVE  :: BLKZF_CAN    (:)
      REAL, ALLOCATABLE, SAVE  :: BLKO3_CAN    (:)
      REAL, ALLOCATABLE, SAVE  :: BLKNO2_CAN   (:)
      LOGICAL, ALLOCATABLE, SAVE  :: CLOUDS_CAN   (:)
      REAL, ALLOCATABLE, SAVE  :: CLDFRAC_CAN  (:)
      REAL, ALLOCATABLE, SAVE  :: BLKRJ_RES_CAN(:, :)
      REAL, ALLOCATABLE, SAVE  :: BLKRJ_ACM_CAN(:, :)
      REAL, ALLOCATABLE, SAVE  :: TAUC_AERO_CAN(:, :)
      REAL, ALLOCATABLE, SAVE  :: TAU_TOT_CAN  (:, :)
      REAL, ALLOCATABLE, SAVE  :: TAUO3_TOP_CAN   (:)
      REAL, ALLOCATABLE, SAVE  :: TAU_RAY_CAN     (:)
      REAL, ALLOCATABLE, SAVE  :: SSA_CAN         (:)
      REAL, ALLOCATABLE, SAVE  :: TAU_CLOUD_CAN(:, :)

! Canopy arrays
      REAL, ALLOCATABLE, SAVE  :: RJ_CORR_TOP ( :, :)    ! canopyshading correction to J-values (hc)
      REAL, ALLOCATABLE, SAVE  :: RJ_CORR_C1R ( :, :)    ! canopyshading correction to J-values (hc to 0.75*hc)
      REAL, ALLOCATABLE, SAVE  :: RJ_CORR_C2R ( :, :)    ! canopyshading correction to J-values (hc to 0.50*hc)
      REAL, ALLOCATABLE, SAVE  :: RJ_CORR_C3R ( :, :)    ! canopyshading correction to J-values (hc to 0.35*hc)
      REAL, ALLOCATABLE, SAVE  :: RJ_CORR_C4R ( :, :)    ! canopyshading correction to J-values (hc to 0.20*hc)
      REAL, ALLOCATABLE, SAVE  :: RJ_CORR_BOT ( :, :)    ! canopyshading correction to J-values (0.20*hc to bottom)
      REAL, ALLOCATABLE, SAVE  :: RJ_CORR     ( :, :)    ! total/integrated canopy shading correction to J-values
      REAL, ALLOCATABLE, SAVE  :: RJ_CORR6    ( :, :)    ! total/integrated canopy shading correction to J-values
      REAL, ALLOCATABLE, SAVE  :: ZCANX    ( : )         ! canopy heights[m]
      REAL, ALLOCATABLE, SAVE  :: RJ_CORRX ( : )         ! canopy height dependent photolysis attenuation factor
      REAL, ALLOCATABLE, SAVE  :: RJ_CORRX6( : )         ! canopy height dependent photolysis attenuation factor
      REAL                     :: XCAN     ( 2 )         ! canopy height interpolation bounds
      REAL                     :: YCAN     ( 2 )         ! photolysisattenuation interpolation bounds
      REAL ZFL, ZCAN, XCANOUT                            ! local canopy variables
      INTEGER, PARAMETER      :: MAXCAN = 1000           ! Declare local maximum canopy layers

!IVAI
      REAL, ALLOCATABLE, SAVE  :: ZCANX6   ( : )        ! canopy heights[m]

! Local canopy variables
!      REAL :: RJ_CORR_TOP, RJ_CORR_C1R, RJ_CORR_C2R, RJ_CORR_C3R, RJ_CORR_C4R, RJ_CORR_BOT  ! local canopy variables
      REAL :: BOTCAN, LAI_C1R, LAI_C2R, LAI_C3R, LAI_C4R, COEFF ! local canopy variables
      REAL :: TOTAL_O3_COLUMN_CAN
      INTEGER                 :: COUNTCAN, KCAN, KK, II
!IVAI
      INTEGER                 :: KOUNT
      INTEGER                 :: S
      INTEGER, PARAMETER      :: MAXCAN6 = 6            ! Declare local canopy layers
      REAL, ALLOCATABLE, SAVE :: ZFT(:)                 ! height combined canopy plus resolved model layers
      REAL, ALLOCATABLE, SAVE :: rj_corrf(:), rj_corrmid(:)
      REAL, ALLOCATABLE, SAVE :: RJ_CORRM ( :, :, :)    ! 3D canopy shading correction to J-values

! Diagnostic output arrays
!      REAL, ALLOCATABLE ::    o3old(:,:,:),  o3new(:,:,:), o3tend(:,:,:)! 3D
!      REAL, ALLOCATABLE :: aux2d_01(:,:), aux2d_02(:,:), aux2d_03(:,:)  ! 2D
!IVAI

!...Variables for diagnostic outputs

      REAL, ALLOCATABLE, SAVE :: N_EXCEED_TROPO3( :,: )  ! Number of adjustments tropospheric ozone optical depth

      REAL, ALLOCATABLE, SAVE :: TOTAL_OC( :,: )         ! total ozone column [DU]
      REAL, ALLOCATABLE, SAVE :: TROPO_OC( :,: )         ! tropospheric ozone column [DU]
      REAL, ALLOCATABLE, SAVE :: NO2_COLUMN ( :,: )      ! tropospheric NO2 column []
      REAL, ALLOCATABLE, SAVE :: CO_COLUMN  ( :,: )      ! tropospheric CO column  []
      REAL, ALLOCATABLE, SAVE :: HCHO_COLUMN( :,: )      ! tropospheric HCHO column [DU]
      REAL, ALLOCATABLE, SAVE :: SO2_COLUMN ( :,: )      ! tropospheric SO2 column [DU]
      REAL, ALLOCATABLE, SAVE :: TROPO_O3_EXCEED( :,: )   ! Factor used to adjust tropospheric ozone optical depth
      REAL, ALLOCATABLE, SAVE :: TRANSMIS_DIFFUSE( :,: ) ! diffuse transmission coefficient at surface
      REAL, ALLOCATABLE, SAVE :: TRANSMIS_DIRECT( :,: )  ! direct transmission coefficient at surface
      REAL, ALLOCATABLE, SAVE :: REFLECT_COEFF( :,: )    ! reflection coefficient at top of atmosphere
      REAL, ALLOCATABLE, SAVE :: TAU_AERO_WL ( :,:,: )   ! total aerosol optical depth
      REAL, ALLOCATABLE, SAVE :: TAU_CLOUD_WL( :,:,: )   ! total cloud optical depth
      REAL, ALLOCATABLE, SAVE :: CLR_TRANSMISSION( :,: ) ! diffuse transmission coefficient of clouds
      REAL, ALLOCATABLE, SAVE :: CLR_REFLECTION  ( :,: ) ! reflection coefficient of cloud
      REAL, ALLOCATABLE, SAVE :: CLR_TRANS_DIRECT( :,: ) ! direct transmission coefficient of clouds
#ifdef phot_debug      
      REAL, ALLOCATABLE, SAVE :: ASY_CLOUD_WL( :,:,: ) ! columm average of cloud asymmetry factor
      REAL, ALLOCATABLE, SAVE :: SSA_CLOUD_WL( :,:,: ) ! columm average of cloud single scattering albedo
#endif      
      REAL, ALLOCATABLE, SAVE :: TAU_TOT_WL  ( :,:,: ) ! total optical depth
      REAL, ALLOCATABLE, SAVE :: TAUO3_TOP_WL( :,:,: ) ! optical depth of ozone above model domain

      REAL, ALLOCATABLE, SAVE :: AERO_SSA  ( :,:,:,: ) ! aerosol single scattering albedo
      REAL, ALLOCATABLE, SAVE :: AERO_ASYM ( :,:,:,: ) ! aerosol asymmetry factor
      REAL, ALLOCATABLE, SAVE :: TOT_EXT   ( :,:,:,: ) ! total extinction for layer [1/Km]
      REAL, ALLOCATABLE, SAVE :: GAS_EXT   ( :,:,:,: ) ! clear sky extinction for layer [1/Km]
      REAL, ALLOCATABLE, SAVE :: AERO_EXT  ( :,:,:,: ) ! aerosol extinction for layer [1/Km]
      REAL, ALLOCATABLE, SAVE :: ACTINIC_FX( :,:,:,: ) ! net actinic flux [watts/m**2]
      REAL, ALLOCATABLE, SAVE :: OUTPUT_BUFF ( :,:,: ) ! output buffer for DIAG2 and DIAG3 files

      INTEGER          IOSX            ! i/o and allocate memory status

      INTERFACE
         SUBROUTINE O3TOTCOL ( LATITUDE, LONGITUDE, JDATE, JTIME, OZONE )
            INTEGER, INTENT( IN )    :: JDATE      ! Julian day of the year (yyyyddd)
            INTEGER, INTENT( IN )    :: JTIME      ! time (hhmmss)
            REAL,    INTENT( IN )    :: LATITUDE   ! latitude of point on earth's surface
            REAL,    INTENT( IN )    :: LONGITUDE  ! longitude of point on earth's surface
            REAL,    INTENT( INOUT ) :: OZONE      ! total column ozone [DU]
         END SUBROUTINE O3TOTCOL
         SUBROUTINE CONVCLD_PROP_ACM( JDATE, JTIME, TSTEP )
            INTEGER, INTENT( IN )    :: JDATE
            INTEGER, INTENT( IN )    :: JTIME
            INTEGER, INTENT( IN )    :: TSTEP( 3 )
         END SUBROUTINE CONVCLD_PROP_ACM
      END INTERFACE

! ----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.

         TSTEP  = TIME2SEC( DTSTEP( 1 ) ) ! output timestep for phot diagnostic files

!...Set flag to initialize calculating aerosol extinction at 550 nm via Angstrom Exponents
         CALCULATE_EXT_550 = .TRUE. !PHOTDIAG

         PECOL_OFFSET = COLSD_PE( 1, MYPE+1 ) - 1
         PEROW_OFFSET = ROWSD_PE( 1, MYPE+1 ) - 1

         CALL INIT_PHOT_SHARED()

!...Allocate array needed to calculation aerosol and cloud optical properties

         CALL INIT_AERO_DATA(  )
         
         CALL INIT_CLOUD_OPTICS(  )

!IVAI
! In-line canopy shading option? (default = false)
      ! IF (FIRSTIME )
      IF ( CANOPY_SHADE ) THEN                      ! FIRSTIME

         CALL init_can_mask( JDATE, JTIME )         ! FIRSTIME

         CALL init_can_levs( CGRID, JDATE, JTIME )  ! FIRSTIME
! ================
! Out:
!      CONC_MOD        <= CONC    ! ALL CANOPY & NON-CANOPY COLUMNS !
!      CONC_CAN(1,2,3) <= CONC(1) ! ALL CANOPY & NON-CANOPY COLUMNS !
! ===============

         CALL init_can_trans(CGRID, JDATE, JTIME )  ! FIRSTIME


!...Allocate and initialize new canopy arrays
         ALLOCATE( RJ_CORRX     ( MAXCAN ) )
         ALLOCATE( ZCANX        ( MAXCAN ) )

         ALLOCATE( RJ_CORRX6    ( MAXCAN6) )
         ALLOCATE( ZCANX6       ( MAXCAN6) )

         ALLOCATE( 
     &           RJ_CORR_TOP  ( NCOLS, NROWS ),
     &           RJ_CORR_C1R  ( NCOLS, NROWS ),
     &           RJ_CORR_C2R  ( NCOLS, NROWS ),
     &           RJ_CORR_C3R  ( NCOLS, NROWS ),
     &           RJ_CORR_C4R  ( NCOLS, NROWS ),
     &           RJ_CORR_BOT  ( NCOLS, NROWS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating sub-canopy photolysis rate correction arrays'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE( ZFT       (NLAYT),
     &             rj_corrf  (NLAYT),
     &             rj_corrmid(NLAYT), STAT = ALLOCSTAT)
         IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating canopy RJ correction arrays'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE( 
     &            RJ_CORR      ( NCOLS, NROWS ), 
     &            RJ_CORR6     ( NCOLS, NROWS ),
     &            RJ_CORRM     ( NCOLS, NROWS, NLAYS + NLAYC ),  !IVAI
     &            STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating canopy photolysis rate correction arrays'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

!         ALLOCATE(
! 2D
!     &           aux2d_01( NCOLS, NROWS ),
!     &           aux2d_02( NCOLS, NROWS ),
!     &           aux2d_03( NCOLS, NROWS ),
! 3D
!     &              o3old( NCOLS, NROWS, NLAYS ),
!     &              o3new( NCOLS, NROWS, NLAYS ),
!     &             o3tend( NCOLS, NROWS, NLAYS ))

! Initialize canopy arrays
         RJ_CORRX(:)=0.0
         ZCANX   (:)=0.0
         RJ_CORR_TOP(:,:)=0.0
         RJ_CORR_C1R(:,:)=0.0
         RJ_CORR_C2R(:,:)=0.0
         RJ_CORR_C3R(:,:)=0.0
         RJ_CORR_C4R(:,:)=0.0
         RJ_CORR_BOT(:,:)=0.0
         RJ_CORR    (:,:)=0.0

         ZCANX6   (:)=0.0
         ZFT      (:)=0.0
         rj_corrf (:)=0.0
         rj_corrmid(:)=0.0

         RJ_CORR  (:,:)=0.0
         RJ_CORR6 (:,:)=0.0
         RJ_CORRM (:,:,:)=0.0  !IVAI

      END IF ! ( CANOPY_SHADE )

! set cosine values for sun effectively below horizon
         COS85 = COS( 85.0 * PI180 )

!...Initialize Surface albedo method

         IF ( .NOT. INITIALIZE_ALBEDO( JDATE, JTIME ) ) THEN
              XMSG = 'Failure initializing photolysis surface albedo algorithm'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE( ETOT_SFC ( NWL ) )

         ALLOCATE( LWC    ( NLAYS ) ) 
         ALLOCATE( RWC    ( NLAYS ) ) 
         ALLOCATE( IWC    ( NLAYS ) ) 
         ALLOCATE( SWC    ( NLAYS ) ) 
         ALLOCATE( GWC    ( NLAYS ) ) 
         ALLOCATE( BLKPRS ( NLAYS ) ) 
         ALLOCATE( BLKTA  ( NLAYS ) ) 
         ALLOCATE( BLKDZ  ( NLAYS ) )
         ALLOCATE( BLKDENS( NLAYS ) ) 
         ALLOCATE( BLKZH  ( NLAYS ) ) 
         ALLOCATE( BLKO3  ( NLAYS ) ) 
         ALLOCATE( BLKNO2 ( NLAYS ) ) 
         ALLOCATE( BLKZF  ( NLAYS+1 ) )
         ALLOCATE( CLOUDS ( NLAYS ) ) 
         ALLOCATE( CLDFRAC( NLAYS ) ) 

         ALLOCATE( BLKRJ_RES( NLAYS,NPHOTAB ) )
         ALLOCATE( BLKRJ_ACM( NLAYS,NPHOTAB ) )

         ALLOCATE( TAUO3_TOP( NWL ) )
         ALLOCATE( TAU_RAY  ( NWL ) )
         ALLOCATE( SSA      ( NWL ) )

         ALLOCATE( TAU_CLOUD( NLAYS,NWL ) )
         ALLOCATE( TAUC_AERO( NLAYS,NWL ) )
         ALLOCATE( TAU_TOT  ( NLAYS,NWL ) )

!IVAI
!...Allocate and initialize new canopy arrays
      IF ( CANOPY_SHADE ) THEN

! New optics canopy arrays
         ALLOCATE(
     &              O3_PPM    (NLAYT),
     &             NO2_PPM    (NLAYT),
     &           BLKTA_CAN    (NLAYT),
     &           BLKPRS_CAN   (NLAYT),
     &           BLKDENS_CAN  (NLAYT),
     &           BLKZH_CAN    (NLAYT),
     &           BLKZF_CAN    (NLAYT+1),
     &           BLKO3_CAN    (NLAYT),
     &           BLKNO2_CAN   (NLAYT),
     &           CLOUDS_CAN   (NLAYT),
     &           CLDFRAC_CAN  (NLAYT),
     &           BLKRJ_RES_CAN(NLAYT, NPHOTAB),  !out
     &           BLKRJ_ACM_CAN(NLAYT, NPHOTAB),  !out
     &           TAUC_AERO_CAN(NLAYT, NWL),      !out
     &           TAU_TOT_CAN  (NLAYT, NWL),      !out
     &           TAUO3_TOP_CAN       (NWL),      !inout
     &           TAU_RAY_CAN         (NWL),      !inout
     &           SSA_CAN             (NWL),      !out
     &           TAU_CLOUD_CAN( NLAYT,NWL),      !inout
     &           STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating canopy new_optics arrays'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         TAUC_AERO_CAN(:, :)=0.0  !out
         TAU_TOT_CAN  (:, :)=0.0  !out
         TAU_CLOUD_CAN(:, :)=0.0  !out

      END IF ! CANOPY_SHADE
!IVAI

         ALLOCATE( TOTAL_OC ( NCOLS,NROWS ) )
         ALLOCATE( TAU_AERO_550    ( NCOLS,NROWS ) )
         TAU_AERO_550  = 0.0

         IF ( PHOTDIAG ) THEN
            ALLOCATE( TROPO_OC   ( NCOLS,NROWS ) )
            ALLOCATE( CO_COLUMN  ( NCOLS,NROWS ) )
            ALLOCATE( SO2_COLUMN ( NCOLS,NROWS ) )
            ALLOCATE( NO2_COLUMN ( NCOLS,NROWS ) )
            ALLOCATE( HCHO_COLUMN( NCOLS,NROWS ) )
            ALLOCATE( TROPO_O3_EXCEED ( NCOLS,NROWS ) )
            ALLOCATE( N_EXCEED_TROPO3 ( NCOLS,NROWS ) )
            ALLOCATE( TRANSMIS_DIFFUSE( NCOLS,NROWS ) )
            ALLOCATE( TRANSMIS_DIRECT ( NCOLS,NROWS ) )
            ALLOCATE( REFLECT_COEFF   ( NCOLS,NROWS ) )
            ALLOCATE( CLR_TRANSMISSION( NCOLS,NROWS ) )
            ALLOCATE( CLR_TRANS_DIRECT( NCOLS,NROWS ) )
            ALLOCATE( CLR_REFLECTION  ( NCOLS,NROWS ) )
            ALLOCATE( TAU_AERO_WL     ( NCOLS,NROWS,NWL ) )
            ALLOCATE( TAU_CLOUD_WL    ( NCOLS,NROWS,NWL ) )
#ifdef phot_debug            
            ALLOCATE( SSA_CLOUD_WL( NCOLS,NROWS,NWL ) )
            ALLOCATE( ASY_CLOUD_WL( NCOLS,NROWS,NWL ) )
#endif            
            ALLOCATE( TAU_TOT_WL  ( NCOLS,NROWS,NWL ) )
            ALLOCATE( TAUO3_TOP_WL( NCOLS,NROWS,NWL ) )
 
            N_EXCEED_TROPO3 = 0.0
            TROPO_O3_EXCEED = 0.0
            TSTEP_COUNT     = 0
            TROPO_OC        = 0.0
            CO_COLUMN       = 0.0
            SO2_COLUMN      = 0.0
            NO2_COLUMN      = 0.0
            HCHO_COLUMN     = 0.0

!...write wavelength data to a character array

            ALLOCATE ( WLTXT( NWL ) )

            DO IWL = 1, NWL
               WRITE( WLTXT( IWL ),'(I3.3)' ) INT( WAVELENGTH( IWL ) )
            END DO

! get wanted number of layers for PHOTDIAG2 and PHOTDIAG3 files
            IF ( NLAYS_DIAG .EQ. 0 ) NLAYS_DIAG = NLAYS
            NLAYS_DIAG = MAX( 1, MIN( NLAYS_DIAG, NLAYS))

! get wanted wavelengths for PHOTDIAG2 and PHOTDIAG3 files
            ALLOCATE( WAVE_LIST( NWL ) )
            WAVE_LIST( : ) = ''
            IF ( NWAVE .GT. NWL )
     &          CALL LOG_MESSAGE( LOGDEV, 'Error: the number of ' //
     &          'wavelengths the user has requested for diagnostic ' //
     &          'photolysis output exceeds the number of internal model ' //
     &          'wavelengths.' )
            IF ( NWAVE .EQ. 0 ) THEN ! use all wavelenghts
               N_DIAG_WVL = NWL
               ALLOCATE ( DIAG_WVL( N_DIAG_WVL ) , STAT = ALLOCSTAT )
               IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating DIAG_WVL'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
               DO IWL = 1, NWL
                  DIAG_WVL( IWL )  = IWL
               END DO
               WRITE(LOGDEV,'(5X,A,I3)')'Environment Variable NWAVE_PHOTDIAG not found '
     &      // 'setting PHOTDIAG2 and PHOTDIAG3 to output all wavelengths. Integer '
     &      // 'truncated values are below.'
               DO IWL = 1, N_DIAG_WVL
                  SPC = DIAG_WVL( IWL )
                  WRITE(LOGDEV,'(5X,I3,1X,A16)')IWL, WLTXT(SPC)
               END DO
            ELSE ! use the environment list
                WAVE_LIST( 1:NWAVE ) = WAVE_ENV( 1:NWAVE )
                N_DIAG_WVL = 0
                 ! first remove identical values
                DO V = 1, NWAVE-1
                   DO L = (V+1), NWAVE
                      IF( TRIM( WAVE_LIST( V ) ) .EQ. TRIM( WAVE_LIST( L ) ) )THEN
                         WAVE_LIST( L ) = " "
                      END IF
                   END DO
                END DO
                 ! Now count number of unique values
                DO V = 1, NWAVE
                   IF( LEN_TRIM( WAVE_LIST( V ) ) .GT. 0 )N_DIAG_WVL = N_DIAG_WVL + 1
                END DO
                ALLOCATE ( DIAG_WVL( N_DIAG_WVL ) , STAT = ALLOCSTAT )
                IF ( ALLOCSTAT .NE. 0 ) THEN
                  XMSG = 'Failure allocating DIAG_WVL'
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               END IF
                ! Next find unique list value in wavelenght spectrum
               IWL = 0
               DO V = 1, NWAVE
                   IF( LEN_TRIM( WAVE_LIST( V ) ) .LT. 1 )CYCLE
                   IWL = IWL + 1
                   DIAG_WVL( IWL )  = INDEXR ( TRIM( WAVE_LIST( V ) ), NWL, WLTXT )
                   IF ( DIAG_WVL( IWL ) .LT. 1 ) THEN
                      WRITE(LOGDEV,'(5X,A)')'PHOT: Cannot find requested wavelength, '
     &                              // TRIM(  WAVE_LIST( IWL ) ) // ' for DIAG2 and DIAG3 files '
     &                              // ' in spectrum '
                   END IF
               END DO
               IF( MINVAL( DIAG_WVL ) .LT. 1 )THEN
                  XMSG = 'FAILED TO find the above requested wavelenght spectrum '
                  WRITE( LOGDEV,'(5X,A)')XMSG
                  XMSG = 'Permitted integer truncated values of wavelenght spectrum '
                  DO IWL = 1, NWL
                     WRITE(LOGDEV,'(10X,I3,1X,A16)')IWL, WLTXT(IWL)
                  END DO
                  XMSG = 'ERROR using the environment variable, NWAVE_PHOTDIAG '
                  CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
               ELSE
                  WRITE(LOGDEV,'(5X,A,I3)')'Environment Variable NWAVE_PHOTDIAG found '
     &      //    'setting PHOTDIAG2 and PHOTDIAG3 to output below wavelenghts'
                  DO IWL = 1, N_DIAG_WVL
                     SPC = DIAG_WVL( IWL )
                     WRITE(LOGDEV,'(5X,I3,1X,A16)')IWL, WLTXT(SPC)
                  END DO
               END IF
            END IF
            WRITE(LOGDEV,'(/)')


            ALLOCATE ( AERO_ASYM( NCOLS,NROWS,NLAYS_DIAG,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D AERO_ASYM'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( AERO_SSA( NCOLS,NROWS,NLAYS_DIAG,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D AERO_SSA'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( AERO_EXT( NCOLS,NROWS,NLAYS_DIAG,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D AERO_EXT'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( TOT_EXT( NCOLS,NROWS,NLAYS_DIAG,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D TOT_EXT'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( GAS_EXT( NCOLS,NROWS,NLAYS_DIAG,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D GAS_EXT'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( ACTINIC_FX( NCOLS,NROWS,NLAYS_DIAG,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ACTINIC_FX'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( OUTPUT_BUFF( NCOLS,NROWS,NLAYS_DIAG ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating OUTPUT_BUFF'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'CO'
            LGC_CO = INDEX1( VARNM, N_GC_SPC, GC_SPC )
            IF ( LGC_CO .LE. 0 ) THEN
               XMSG = 'Could not find ' // VARNM // 'in species table'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               WRITE(LOGDEV,95101)
            END IF

            VARNM = 'SO2'
            LGC_SO2 = INDEX1( VARNM, N_GC_SPC, GC_SPC )
            IF ( LGC_SO2 .LE. 0 ) THEN
               XMSG = 'Could not find ' // VARNM // 'in species table'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               WRITE(LOGDEV,95101)
            END IF

            VARNM = 'HCHO'
            LGC_HCHO = INDEX1( VARNM, N_GC_SPC, GC_SPC )
            IF ( LGC_HCHO .LE. 0 ) THEN
               VARNM = 'FORM'
               LGC_HCHO = INDEX1( VARNM, N_GC_SPC, GC_SPC )
               IF ( LGC_HCHO .LE. 0 ) THEN
                  XMSG = 'Could not find HCHO or FORM, i.e., formaldehyde, in species table'
                  CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
                  WRITE(LOGDEV,95101)
               END IF
            END IF

!...open the photolysis diagnostic files
            ODATE = START_DATE; OTIME = START_TIME; OSTEP = 0
#ifdef phot_write_start
            IF ( IO_PE_INCLUSIVE ) CALL OPPHOT ( ODATE, OTIME, DTSTEP( 1 ) )
#else
            CALL NEXTIME ( ODATE, OTIME, DTSTEP( 1 ) )  ! output timestamp ending time
            IF ( IO_PE_INCLUSIVE ) CALL OPPHOT ( ODATE, OTIME, DTSTEP( 1 ) )
! reset ODATE and OTIME for counting
            ODATE = START_DATE; OTIME = START_TIME
#endif

         END IF  ! photdiag
         CALL SUBST_BARRIER

         ALLOCATE ( AERO_EXT_550( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
             XMSG = 'Failure allocating 3D AERO_EXT_550'
             CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

!...set pointers to species O3 and NO2 in CGRID

         VARNM = 'O3'
         LGC_O3 = INDEX1( VARNM, N_GC_SPC, GC_SPC )
         IF ( LGC_O3 .LE. 0 ) THEN
            XMSG = 'Could not find ' // VARNM // 'in species table'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

         VARNM = 'NO2'
         LGC_NO2 = INDEX1( VARNM, N_GC_SPC, GC_SPC )
         IF ( LGC_NO2 .LE. 0 ) THEN
            XMSG = 'Could not find ' // VARNM // 'in species table'
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT3 )
         END IF

      END IF  ! FIRSTIME

      IF ( INT ( JD_STRAT_O3MIN ) .NE. JDATE ) THEN
!...set minimum fraction of ozone column above PTOP

         CALL SEASONAL_STRAT_O3( JDATE, JTIME )
         MIN_STRATO3_FRAC = MONTH_STRAT_03_FRAC
         MAX_TROPOO3_FRAC = MAX( 1.0 - MONTH_STRAT_03_FRAC, 0.0 )
         
         JD_STRAT_O3MIN = REAL( JDATE, 4)
      END IF
!...initialize variables tracking whether stratosphere ozone column satisfies
!...climatological averages.

      O3_TOGGLE_AVE     = 0.0
      O3_TOGGLE_MIN     = 1.0
      N_TROPO_O3_TOGGLE = 0
      TSTEP_COUNT       = TSTEP_COUNT + 1

      MIDDATE = JDATE
      MIDTIME = JTIME
      ITMSTEP = TIME2SEC( DTSTEP( 2 ) ) / 2
      CALL NEXTIME( MIDDATE, MIDTIME, SEC2TIME( ITMSTEP ) )

      CALL CONVCLD_PROP_ACM( JDATE, JTIME, DTSTEP )
      CALL GET_PHOT_MET( JDATE, JTIME, MIDDATE, MIDTIME )

!...Get cosine of solar parameters and set DARK

      CALL UPDATE_SUN( JDATE, JTIME, MIDDATE, MIDTIME)
      
      RSQD = DIST_TO_SUN * DIST_TO_SUN

      IF ( MAXVAL( COSINE_ZENITH ) .LE. 0.0 ) THEN
         DARK = .TRUE.
      ELSE
         DARK = .FALSE.
      END IF

!...set surface albedos

      CALL GET_ALBEDO( MIDDATE, MIDTIME, COSINE_ZENITH, LAT, LON )

!...SA  Write COSINE_ZENITH array at the end of each output tstep

      JTIME_CHK         = .FALSE.
      OSTEP  = OSTEP + TIME2SEC( DTSTEP( 2 ) )
      JTIME_CHK = ( OSTEP .GE. TIME2SEC( DTSTEP( 1 ) ) )
      IF ( JTIME_CHK ) THEN
        OSTEP = 0
        CALL NEXTIME( ODATE, OTIME, DTSTEP( 1 ) )
      END IF
#ifdef phot_write_start
      JTIME_CHK = ( ODATE .EQ. STDATE .AND. OTIME .EQ. STTIME )
#endif

      IF ( PHOTDIAG ) THEN
#ifdef parallel_io
         IF ( .NOT. IO_PE_INCLUSIVE ) THEN
            IF ( .NOT. OPEN3( CTM_RJ_1, FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open ' // TRIM(CTM_RJ_1)
               CALL M3EXIT( PNAME, ODATE, OTIME, XMSG, XSTAT1 )
            END IF
            IF ( .NOT. OPEN3( CTM_RJ_2, FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open ' // TRIM(CTM_RJ_2)
               CALL M3EXIT( PNAME, ODATE, OTIME, XMSG, XSTAT1 )
            END IF
            IF ( .NOT. OPEN3( CTM_RJ_3, FSREAD3, PNAME ) ) THEN
               XMSG = 'Could not open ' // TRIM(CTM_RJ_3)
               CALL M3EXIT( PNAME, ODATE, OTIME, XMSG, XSTAT1 )
            END IF
         END IF
#endif
      END IF 

!IVAI
!      print*, 'PHOT: AFTER FIRSTIME '
!      print*, 'PHOT: GRID_DATA AREA = ', AREA (1:10,1:10), LAT(1:10,1:10)

      IF (CANOPY_SHADE ) THEN

        CALL get_can_mask( JDATE, JTIME)

! ===============
! In get_can_levs, the vertical layers are going down (reversed to phot)
!
! k = 1, NLAYT from  top to bottom of augmented canopy+resolved model layers
!      1 = top model layer
!  NLAYS = bottom resolved model layer
!  NLAYC = 3 canopy layers
!  NLAYT = NLAYS + NLAYC, augmented canopy plus resolved model layers
! NLAYS+1= top    canopy layer at 1.0*CH
! NLAYS+2= middle canopy layer at 0.5*CH
! NLAYS+3= bottom canopy layer at 0.2*CH
! =================

        CALL get_can_levs(CGRID, JDATE, JTIME, MIDDATE, MIDTIME)

! ================
! Out: ZMID_CAN  (:,:,NLAYT)
!      ZMOM_CAN  (:,:,NLAYT+1)
!      TA_CAN    (:,:,NLAYT)
!      QV_CAN
!      PRES_CAN
!      DENS_CAN
!      CONC_MOD        <= CGRID      ! ALL CANOPY & NON-CANOPY COLUMNS !
!      CONC_CAN(1,2,3) <= CGRID(1)   ! ALL CANOPY & NON-CANOPY COLUMNS !

! Output variables
!  massair_can(:,:, NLAYT)    :  mass of air in canopy layers (kg)
!  massair    (:,:, NLAYS)    :  mass of air in model layers (kg)
!                                        (gathered canopy + resolved scale columns)
!   nfrct  (NLAYT,  :,:)        :  Number of original model levels contributing to canopy level k
!   ifrct  (NLAYT,2,:,:)        :  Index of the original model level contributing to canopy level k
!   frctr2c(NLAYT,2,:,:)        :  Fractional contribution of the original model level to canopy level k
!   frctc2r(NLAYT,2,:,:)        :  Fractional contribution of the canopy level to the original model level
!
!===============================================================================
! Distribute tracer concentration from model resolved layers into canopy layers
! flag = 0 "resolved_to_canopy"
!===============================================================================

        CALL canopy_transfer(CGRID, JDATE, JTIME, 0 )

! ==============
!  Input:
!   CGRID   (:,:, NLAYS, NSPCSD) :  Chemical tracers conc. ppmv on model levels
!   CONC_MOD(:,:, NLAYS, NSPCSD) :  Chemical tracers conc. ppmv on model levels
!
!  Output:
!   CONC_CAN(:,:, NLAYT, NSPCSD) :  Chemical tracers conc. ppmv on combined canopy+resolved layers
!                                    ! CANOPY COLUMNS ONLY !
!   CONC_2M (:,:       , NSPCSD) :  2M Chemical tracers conc. ppmv Diagnostics
!
! ================================

      DO L = 1, NLAYS !from bottom to top model layer
        DO ROW = 1, NROWS
          DO COL = 1, NCOLS

          KOUNT = 0

          IF ( FRT_MASK(COL,ROW) > 0.) THEN  ! CANOPY_SHADE & FRT_MASK

            IF (.FALSE.) THEN
            IF ( KOUNT < 3 )  THEN

              print*, 'PHOT: AFTER R2C CAN L=1 ', 
     &           frctr2c(NLAYT,1,COL,ROW), 
     &           frctr2c(NLAYT,2,COL,ROW),! 1cy: order is 1=top NLAYT=1cy
     &       float(nfrct(NLAYT,  COL,ROW  ))

              print*, 'PHOT: AFTER R2C MOD: ', L,
     &           TA( COL,ROW,L ),  PRES( COL,ROW, L ), DENS( COL,ROW, L ),
     &         AREA( COL,ROW   ), 
     &           ZM( COL,ROW,L ), ZFULL( COL,ROW, L )


              print*, 'PHOT: AFTER R2C CAN L= ', NLAYC + L,
     &             TA_CAN(COL,ROW, NLAYC + L),
     &           PRES_CAN(COL,ROW, NLAYC + L), DENS_CAN(COL,ROW, NLAYC + L),
     &             ZH_CAN(COL,ROW, NLAYC + L),   ZF_CAN(COL,ROW, NLAYC + L)

! Print 3 canopy levels only at KOUNT number of canopy columns
              print*, 'PHOT AFTER, CAN L= ', L,
     &               TA_CAN(COL,ROW, L),
     &             PRES_CAN(COL,ROW, L), DENS_CAN(COL,ROW, L),
     &               ZH_CAN(COL,ROW, L),   ZF_CAN(COL,ROW, L)

            DO SPC = 1, NUMB_MECH_SPC

              S = CGRID_INDEX( SPC )
              ! S=LGC_O3  is Ozone
              ! S=LGC_NO2 is NO2

! Print all levels only at KOUNT number of canopy columns
              if (L < 3) 
     &          print*, 'PHOT: AFTER R2C SPC: ', S, L,
     &        CGRID   ( COL,ROW, L,         S ), 
!     &        CONC_MOD( COL,ROW, L,         S ),
     &        CONC_CAN( COL,ROW, NLAYC + L, S )  ! L= NLAYC+1, NLAYT

              if (L == 1) 
     &          print*, 'PHOT: AFTER R2C SPC: ', S, L,
     &        CONC_CAN( COL,ROW, 3,         S ), 
     &        CONC_CAN( COL,ROW, 2,         S ),
     &        CONC_CAN( COL,ROW, 1,         S )

            END DO ! species loop

            END IF ! KOUNT
            END IF ! .FALSE.

! Print up to KOUNT number of canopy columns
            KOUNT = KOUNT + 1

          END IF ! CANOPY_SHADE & FRT_MASK

! Temporary output
!            aux2d_03( COL,ROW ) = CONC_CAN(COL,ROW, 1, LGC_O3 )

          END DO
        END DO
      END DO ! L=1,NLAYS

      END IF ! CANOPY_SHADE
!IVAI

      CALCULATE_EXT_550 = .TRUE. !JTIME_CHK
      
!...If sun below horizon at all cells, zero photolysis rates & exit
!...  (assumes sun below horizon at *all* levels!)

      IF ( DARK ) THEN

         RJ = 0.0
         RJ_SUB = 0.0
         RJ_RES = 0.0
         ETOT_SFC_WL  = 0.0
         AERO_EXT_550 = 0.0
         TAU_AERO_550 = 0.0
!IVAI
         IF (CANOPY_SHADE ) THEN
           RJ_CAN = 0.0 
           RJ_RES_CAN = 0.0
           RJ_SUB_CAN = 0.0
         END IF 
!IVAI

!...Initialize ETOT_SFC, TAU_AERO, TAU_TOT, TAUO3_TOP to 0.0

!...Write data to output diagnostic file

         IF ( JTIME_CHK .AND. PHOTDIAG ) THEN

            TAUO3_TOP_WL = 0.0
            TAU_AERO_WL  = 0.0
            TAU_CLOUD_WL = 0.0
#ifdef phot_debug                   
            SSA_CLOUD_WL = 0.0
            ASY_CLOUD_WL = 0.0
#endif                   
            TAU_TOT_WL   = 0.0
            TOT_EXT      = 0.0
            GAS_EXT      = 0.0
            AERO_EXT     = 0.0
            AERO_SSA     = 0.0
            AERO_ASYM    = 0.0
            ACTINIC_FX   = 0.0

            TRANSMIS_DIFFUSE  = 0.0
            TRANSMIS_DIRECT   = 0.0
            REFLECT_COEFF     = 0.0
            CLR_TRANSMISSION  = 0.0
            CLR_TRANS_DIRECT  = 0.0
            CLR_REFLECTION    = 0.0 

            DO ROW = 1, NROWS
               DO COL = 1, NCOLS
                  BLKDENS( 1 ) = DENS ( COL,ROW,1 ) * DENS_CONV ! [molecules / cm**3]
                  BLKDZ  ( 1 ) = ZFULL( COL,ROW,1 )
                  DO L = 2, NLAYS
                     BLKDENS( L ) = DENS( COL,ROW,L ) * DENS_CONV ! [molecules / cm**3]
                     BLKDZ  ( L ) = ZFULL( COL,ROW,L ) - ZFULL( COL,ROW,L-1 )
                  END DO
                  MSCALE = 1.0E-19 ! 100.0*10E-15*PPM_MCM3, so units are petamolecules/cm2
                  CALL COLUMN_GAS( IGAS=LGC_CO,   UNIT_FACTOR=MSCALE, COLUMN_DENSITY=CO_COLUMN   )
                  CALL COLUMN_GAS( IGAS=LGC_SO2,  UNIT_FACTOR=MSCALE, COLUMN_DENSITY=SO2_COLUMN  )
                  CALL COLUMN_GAS( IGAS=LGC_NO2,  UNIT_FACTOR=MSCALE, COLUMN_DENSITY=NO2_COLUMN  )
                  CALL COLUMN_GAS( IGAS=LGC_HCHO, UNIT_FACTOR=MSCALE, COLUMN_DENSITY=HCHO_COLUMN )
                  MSCALE = 1.0E-4 * CONC_TO_DU ! so units are Dobsons
                  CALL COLUMN_GAS( IGAS=LGC_O3, UNIT_FACTOR=MSCALE, COLUMN_DENSITY=TROPO_OC )
!...get total ozone column based on OMI observations
                  CALL O3TOTCOL ( LAT( COL,ROW ), LON( COL,ROW ), MIDDATE, MIDTIME, TOTAL_OC( COL,ROW ) )
               END DO
            END DO
         ELSE
           DO ROW = 1, NROWS
              DO COL = 1, NCOLS
!...get total ozone column based on OMI observations
                  CALL O3TOTCOL ( LAT( COL,ROW ), LON( COL,ROW ), MIDDATE,MIDTIME, TOTAL_OC( COL,ROW ) )
              END DO
           END DO
         END IF  ! if JTIME_CHK and PHOTDIAG

      ELSE  ! all cells not dark

!...MAIN loop over all rows and columns
         LOOP_ROWS: DO ROW = 1, NROWS
            LOOP_COLS: DO COL = 1, NCOLS
         
               PHOT_COL = COL + PECOL_OFFSET
               PHOT_ROW = ROW + PEROW_OFFSET

               COSZEN = COSINE_ZENITH( COL,ROW ) ! local cosine of solar zenith angle

               TAU_AERO_550( COL,ROW ) = 0.0
               AERO_EXT_550( COL,ROW,: ) = 0.0
               IF ( COSZEN .LE. 0.0 ) THEN
!...the cell is dark: set variables to zero and cycle
                  RJ    ( COL,ROW, :,: ) = 0.0
                  RJ_RES( COL,ROW, :,: ) = 0.0
                  RJ_SUB( COL,ROW, :,: ) = 0.0
                  ETOT_SFC_WL ( COL,ROW, : ) = 0.0
!IVAI
                  IF (CANOPY_SHADE ) THEN
                    RJ_CAN    ( COL,ROW, :,: ) = 0.0  !IVAI
                    RJ_RES_CAN( COL,ROW, :,: ) = 0.0  !IVAI
                    RJ_SUB_CAN( COL,ROW, :,: ) = 0.0  !IVAI
                  END IF
!IVAI
   
                  IF ( JTIME_CHK .AND. PHOTDIAG ) THEN
                     TAUO3_TOP_WL( COL,ROW, : ) = 0.0
                     TAU_AERO_WL ( COL,ROW, : ) = 0.0
                     TAU_CLOUD_WL( COL,ROW, : ) = 0.0
#ifdef phot_debug                   
                     SSA_CLOUD_WL( COL,ROW, : ) = 0.0
                     ASY_CLOUD_WL( COL,ROW, : ) = 0.0
#endif                   
                     TAU_TOT_WL( COL,ROW, : )   = 0.0
                     TOT_EXT   ( COL,ROW, :,: ) = 0.0
                     GAS_EXT   ( COL,ROW, :,: ) = 0.0
                     AERO_EXT  ( COL,ROW, :,: ) = 0.0
                     AERO_SSA  ( COL,ROW, :,: ) = 0.0
                     AERO_ASYM ( COL,ROW, :,: ) = 0.0
                     ACTINIC_FX( COL,ROW, :,: ) = 0.0
                   
!                     TROPO_O3_EXCEED( COL,ROW )  = 0.0
                     TRANSMIS_DIFFUSE( COL,ROW ) = 0.0
                     TRANSMIS_DIRECT ( COL,ROW ) = 0.0
                     REFLECT_COEFF   ( COL,ROW ) = 0.0
                     CLR_TRANSMISSION( COL,ROW ) = 0.0
                     CLR_TRANS_DIRECT( COL,ROW ) = 0.0
                     CLR_REFLECTION  ( COL,ROW ) = 0.0 

                     BLKDENS( 1 ) = DENS ( COL,ROW,1 ) * DENS_CONV ! [molecules / cm**3]
                     BLKDZ  ( 1 ) = ZFULL( COL,ROW,1 )
                     DO L = 2, NLAYS
                        BLKDENS( L ) = DENS( COL,ROW,L ) * DENS_CONV ! [molecules / cm**3]
                        BLKDZ  ( L ) = ZFULL( COL,ROW,L ) - ZFULL( COL,ROW,L-1 )
                     END DO
                     MSCALE = 1.0E-19 ! 100.0*10E-15*PPM_MCM3, so units are petamolecules/cm2
                     CALL COLUMN_GAS( IGAS=LGC_CO,   UNIT_FACTOR=MSCALE, COLUMN_DENSITY=CO_COLUMN   )
                     CALL COLUMN_GAS( IGAS=LGC_SO2,  UNIT_FACTOR=MSCALE, COLUMN_DENSITY=SO2_COLUMN  )
                     CALL COLUMN_GAS( IGAS=LGC_NO2,  UNIT_FACTOR=MSCALE, COLUMN_DENSITY=NO2_COLUMN  )
                     CALL COLUMN_GAS( IGAS=LGC_HCHO, UNIT_FACTOR=MSCALE, COLUMN_DENSITY=HCHO_COLUMN )
                     MSCALE = 1.0E-4 * CONC_TO_DU ! so units are Dobsons
                     CALL COLUMN_GAS( IGAS=LGC_O3, UNIT_FACTOR=MSCALE, COLUMN_DENSITY=TROPO_OC )
!...get total ozone column based on OMI observations
                     CALL O3TOTCOL ( LAT( COL,ROW ), LON( COL,ROW ), MIDDATE, MIDTIME, TOTAL_OC( COL,ROW ) )
                  END IF

                  CYCLE LOOP_COLS

               END IF

!...initialize BLKRJ using F90 array operations.

               BLKRJ_RES = 0.0
               BLKRJ_ACM = 0.0

               IF (CANOPY_SHADE ) THEN
                 BLKRJ_RES_CAN = 0.0  !IVAI
                 BLKRJ_ACM_CAN = 0.0  !IVAI
               END IF

!...Set height of lowest level to zero

               BLKZF( 1 ) = 0.0
               IF (CANOPY_SHADE ) BLKZF_CAN( 1 ) = 0.0      !IVAI

               ZSFC   = HT( COL,ROW ) ! surface height [m]
               SINZEN = SQRT( 1.0 - COSZEN * COSZEN ) ! sine of zenith angle

!...get total ozone column based on OMI observations
               CALL O3TOTCOL ( LAT( COL,ROW ), LON( COL,ROW ), MIDDATE, MIDTIME, TOTAL_O3_COLUMN )

!IVAI:
               IF (CANOPY_SHADE ) 
     &           TOTAL_O3_COLUMN_CAN = TOTAL_O3_COLUMN
!IVAI
            
               IF ( USE_ACM_CLOUD .OR. CLDATT ) THEN
                  OWATER_FRAC = MAX( ( 1.0 - SEAICE( COL,ROW ) ), 0.0 )
     &                        * WATER_FRACTION( COL,ROW )
                  SEAICE_FRAC = SEAICE( COL,ROW ) * WATER_FRACTION( COL,ROW )
                  SNOW_FRAC   = SNOCOV( COL,ROW )
                  COL_CLOUD   = PHOT_COL 
                  ROW_CLOUD   = PHOT_ROW
               END IF

!...loop over vertical layers ambient air conditions and gas concentration
               DO L = 1, NLAYS
!...Fetch the grid cell ambient data at each layer.

                  BLKTA  ( L )   = TA   ( COL,ROW,L )             ! temperature [K]
                  BLKPRS ( L )   = PRES ( COL,ROW,L ) / STDATMPA  ! [atmospheres]
                  BLKDENS( L )   = DENS ( COL,ROW,L ) * DENS_CONV ! [molecules / cm**3]
                  BLKZH  ( L )   = ZM   ( COL,ROW,L )             ! mid layer height [m]
                  BLKZF  ( L+1 ) = ZFULL( COL,ROW,L )             ! full layer height [m]

!...set scale factor for [ppm] -> [molecule / cm**3]
!...  To go from ppm to molecule/cc:
!...  molecule/cc = ppm *  1.0E-06 * DENS (given in molecule/cc)

                  MSCALE = BLKDENS( L ) * PPM_MCM3

!...fetch ozone and no2 and convert to [ molecules / cm **3 ]
!...  and adjust the volume for ambient temperature and pressure.

                  BLKO3 ( L ) = CGRID( COL,ROW,L,LGC_O3  ) * MSCALE
                  BLKNO2( L ) = CGRID( COL,ROW,L,LGC_NO2 ) * MSCALE
                  ZLEV = BLKZF( L )
               END DO ! loop on layers ambient conditions and gases

         IF ( CANOPY_SHADE .AND. FRT_MASK(COL,ROW) > 0. ) THEN

! First, carry over original model values for the matching layers
            DO L = 1, NLAYS !from bottom to top model layer

!              BLKZH_CAN ( NLAYC + L )    =    ZM(COL,ROW, L)    ! mid  layer height [m]
!              BLKZF_CAN ( NLAYC + L + 1) = ZFULL(COL,ROW, L)    ! full layer height [m] (upper interface)
! Uncomment to use values from get_can_levs
               BLKZH_CAN ( NLAYC + L )    = ZH_CAN(COL,ROW, NLAYC + L)  ! mid  layer height [m]
! NB. Should not include height 0 m (i.e. upper interface)
               BLKZF_CAN ( NLAYC + L + 1) = ZF_CAN(COL,ROW, NLAYC + L ) ! full layer height [m]

!              BLKTA_CAN  ( NLAYC + L ) =   TA ( COL,ROW,L )
!              BLKPRS_CAN ( NLAYC + L ) = PRES ( COL,ROW,L ) / STDATMPA  ! [atmospheres]
!              BLKDENS_CAN( NLAYC + L ) = DENS ( COL,ROW,L ) * DENS_CONV ! [molecules / cm**3]
! Uncomment to use values from get_can_levs
               BLKTA_CAN  ( NLAYC + L ) =   TA_CAN( COL,ROW, NLAYC + L )
               BLKPRS_CAN ( NLAYC + L ) = PRES_CAN( COL,ROW, NLAYC + L ) / STDATMPA  ! [atmospheres]
               BLKDENS_CAN( NLAYC + L ) = DENS_CAN( COL,ROW, NLAYC + L ) * DENS_CONV ! [molecules / cm**3]

!...set scale factor for [ppm] -> [molecule / cm**3]
!...  To go from ppm to molecule/cc:
!...  molecule/cc = ppm *  1.0E-06 * DENS (given in molecule/cc)

               MSCALE_CAN = BLKDENS_CAN( NLAYC + L ) * PPM_MCM3

!              BLKO3_CAN ( NLAYC + L ) = CGRID( COL,ROW,L,LGC_O3  ) * MSCALE_CAN
!              BLKNO2_CAN( NLAYC + L ) = CGRID( COL,ROW,L,LGC_NO2 ) * MSCALE_CAN
! Uncomment to use values from canopy_transfer "resolved_to_canopy" BAD ! ??
               BLKO3_CAN ( NLAYC + L ) = CONC_CAN( COL,ROW,NLAYC + L,LGC_O3  ) * MSCALE_CAN
               BLKNO2_CAN( NLAYC + L ) = CONC_CAN( COL,ROW,NLAYC + L,LGC_NO2 ) * MSCALE_CAN

            END DO ! loop on resolved model layers NLAYS

!...loop over 3 canopy layers NLAYC 
            DO L = 1, NLAYC ! from bottom to top of canopy

!              BLKZH_CAN ( L )    =    ZM(COL,ROW, 1)    ! mid  layer height [m]
!              BLKZF_CAN ( L + 1) = ZFULL(COL,ROW, 1)    ! full layer height [m]
! Uncomment to use values from get_can_levs
               BLKZH_CAN ( L )    = ZH_CAN(COL,ROW, L)  ! mid  layer height [m]
! NB. Should not include height 0 m (i.e. upper interface)
               BLKZF_CAN ( L + 1) = ZF_CAN(COL,ROW, L)  ! full layer height [m]

!              BLKTA_CAN  ( L ) = BLKTA  ( 1 )
!              BLKPRS_CAN ( L ) = BLKPRS ( 1 )
!              BLKDENS_CAN( L ) = BLKDENS( 1 )
! Uncomment to use values from get_can_levs
               BLKTA_CAN  ( L ) =   TA_CAN( COL,ROW,L )
               BLKPRS_CAN ( L ) = PRES_CAN( COL,ROW,L ) / STDATMPA  ! [atmospheres]
               BLKDENS_CAN( L ) = DENS_CAN( COL,ROW,L ) * DENS_CONV ! [molecules / cm**3]

!...set scale factor for [ppm] -> [molecule / cm**3]
!...  To go from ppm to molecule/cc:
!...  molecule/cc = ppm *  1.0E-06 * DENS (given in molecule/cc)

               MSCALE_CAN = BLKDENS_CAN( L ) * PPM_MCM3

!              BLKO3_CAN ( L ) = CGRID( COL,ROW,1,LGC_O3  ) * MSCALE_CAN
!              BLKNO2_CAN( L ) = CGRID( COL,ROW,1,LGC_NO2 ) * MSCALE_CAN
! Uncomment to use values from canopy_transfer "resolved_to_canopy" 
               BLKO3_CAN ( L ) = CONC_CAN( COL,ROW,L,LGC_O3  ) * MSCALE_CAN  ! from canopy_transfer
               BLKNO2_CAN( L ) = CONC_CAN( COL,ROW,L,LGC_NO2 ) * MSCALE_CAN  ! from canopy_transfer
            END DO


! Move above
!           DO L = 1, NLAYT
!              BLKZH_CAN ( L )    = ZH_CAN(COL,ROW, L)    ! mid  layer height [m]
! NB. Should not include height 0 m (i.e. upper interface)
!              BLKZF_CAN ( L + 1) = ZF_CAN(COL,ROW, L )   ! full layer height [m] 
!           END DO

         END IF ! CANOPY_SHADE
!IVAI

! Cloudy atmosphere
               IF ( CLDATT .AND. CFRAC_2D( COL,ROW ) .GT. 0.0 ) THEN
                  DO L = 1, NLAYS
                 
                     IF ( CFRAC_3D( COL,ROW,L ) .GT. 0.0 ) THEN
                        CLOUDS ( L )        = .TRUE.
                        CLOUD_LAYERING( L ) = .TRUE.
                        CLDFRAC( L )        = CFRAC_3D( COL,ROW,L )
!... set hydrometeor concentrations for resolved cloud
                        MSCALE   = 1.0E+3 * DENS ( COL,ROW,L )
                        IWC( L ) = MSCALE * QI( COL,ROW,L )
                        GWC( L ) = MSCALE * QG( COL,ROW,L )
                        SWC( L ) = MSCALE * QS( COL,ROW,L )    
                        LWC( L ) = MSCALE * QC( COL,ROW,L )
                        RWC( L ) = MSCALE * QR( COL,ROW,L )
                     ELSE
                        CLOUDS ( L )        = .FALSE.
                        CLOUD_LAYERING( L ) = .FALSE.
                        CLDFRAC( L )        = 0.0
                        IWC( L ) = 0.0
                        GWC( L ) = 0.0
                        SWC( L ) = 0.0    
                        LWC( L ) = 0.0
                        RWC( L ) = 0.0
                     END IF
                  END DO ! loop on layers clouds
! get optical properties of resolved cloud hydrometeors
                  CALL GET_DROPLET_OPTICS( NLAYS, BLKTA, OWATER_FRAC, SEAICE_FRAC, SNOW_FRAC, LWC )     
                  CALL GET_ICE_OPTICS( NLAYS, BLKTA, IWC )
                  CALL GET_AGGREGATE_OPTICS( NLAYS, RWC, SWC, GWC )     

! Cloud-free atmosphere
               ELSE
                  CLOUDS         = .FALSE.
                  CLOUD_LAYERING = .FALSE.
                  CLDFRAC        = 0.0
!  hydrometeor concentrations
                  LWC = 0.0
                  IWC = 0.0
                  RWC = 0.0
                  SWC = 0.0
                  RWC = 0.0
                  CALL CLEAR_HYDROMETEOR_OPTICS()
               END IF

!IVAI:
            IF ( CANOPY_SHADE .AND. FRT_MASK(COL,ROW) > 0.) THEN
! Above canopy layers
               DO L = 1, NLAYS
                  CLOUDS_CAN (NLAYC+L) = CLOUDS (L)
                  CLDFRAC_CAN(NLAYC+L) = CLDFRAC(L)
               END DO

! Sub-canopy layers: set to 1st model layer
               DO L = 1, NLAYC
                  CLOUDS_CAN (L) = CLOUDS (1)
                  CLDFRAC_CAN(L) = CLDFRAC(1)
               END DO
            END IF ! CANOPY_SHADE
!IVAI
            
!..calculate needed aerosol properties in column

!              IF ( CORE_SHELL ) THEN
                  CALL GET_AERO_DATA ( COL,ROW, NLAYS, DENS, CGRID )
!              ELSE
!                 CALL AERO_OPTICS_INTERNAL( COL,ROW, NLAYS, CGRID )
!              END IF

! set surface albedo

               DO IWL = 1, NWL
                  ALB( IWL ) = SURFACE_ALBEDO( IWL, COL,ROW )
               END DO
!...calculate resolved-sky photolysis rates at all layers:

               NEW_PROFILE    = .TRUE. 
               ONLY_SOLVE_RAD = .FALSE. 
                             
               ! Clear-sky call to new_optics !IVAI
               CALL NEW_OPTICS ( JDATE, JTIME, NLAYS,                       !In: NLAYS # vertical layers model
     &                           BLKTA, BLKPRS, BLKDENS, BLKZH, BLKZF,
     &                           BLKO3, BLKNO2,
     &                           ZSFC, COSZEN, SINZEN, RSQD,
     &                           NEW_PROFILE, CLOUDS, CLDFRAC,              !In
     &                           BLKRJ_RES, TAUC_AERO, TAU_TOT, TAUO3_TOP,  !Out: RJ resolved-sky
     &                           TAU_RAY, SSA, TAU_CLOUD, TOTAL_O3_COLUMN ) !Out

!IVAI: In COL,ROW LOOP
            IF ( CANOPY_SHADE .AND. FRT_MASK(COL,ROW) > 0. ) THEN

               NEW_PROFILE    = .TRUE.
               ONLY_SOLVE_RAD = .FALSE.

               ! Clear-sky call to new_optics on combined canopy plus resolved model layers
               CALL NEW_OPTICS ( JDATE, JTIME, NLAYT,  !in: NLAYT  scalar
     &           BLKTA_CAN, BLKPRS_CAN, BLKDENS_CAN,   !in: (NLAYT)
     &           BLKZH_CAN, BLKZF_CAN,                 !in: (NLAYT)
     &           BLKO3_CAN, BLKNO2_CAN,                !in: (NLAYT)
     &           ZSFC, COSZEN, SINZEN, RSQD,           !in:        scalar
     &           NEW_PROFILE,                          !in:        logical 
     &           CLOUDS_CAN, CLDFRAC_CAN,              !in:  (NLAYT) logical
     &           BLKRJ_RES_CAN,                        !out: (NLAYT,NPHOTAB) resolved-sky photolysis rates
     &           TAUC_AERO_CAN, TAU_TOT_CAN,           !out: (NLAYT, NWL)
     &           TAUO3_TOP_CAN,                        !inout:      (NWL)
     &           TAU_RAY_CAN, SSA_CAN,                 !out:        (NWL)
     &           TAU_CLOUD_CAN,                        !out: (NLAYT, NWL)
     &           TOTAL_O3_COLUMN_CAN )                 !inout:       scalar

            END IF ! CANOPY_SHADE
 
!IVAI

!...load diagnostic file arrays

               ! Aerosol extinction and optical depth are saved every
               ! time step
               FORALL ( L = 1:NLAYS )
                  AERO_EXT_550( COL,ROW,L ) = 1000.0 * AERO_EXTI_550( L )
               END FORALL
               DO LEV = 1, NLAYS
                  TAU_AERO_550 ( COL,ROW ) = TAU_AERO_550 ( COL,ROW )
     &                                     + AERO_EXTI_550( LEV ) * BLKDZ( LEV )
               END DO

               IF ( PHOTDIAG .AND. .NOT. STRATO3_MINS_MET ) THEN
                    N_EXCEED_TROPO3( COL,ROW ) =  N_EXCEED_TROPO3( COL,ROW ) + 1.0
                    TROPO_O3_EXCEED( COL,ROW ) = TROPO_O3_COLUMN/(MAX_TROPOO3_FRAC*TOTAL_O3_COLUMN) - 1.0
     &                                         + TROPO_O3_EXCEED( COL,ROW )
               END IF 

               FORALL ( IWL = 1:NWL )
                  ETOT_SFC_WL ( COL,ROW,IWL ) = IRRADIANCE( 1,IWL )
               END FORALL

               IF ( JTIME_CHK .AND. PHOTDIAG  ) THEN
                  TOTAL_OC( COL,ROW )         = REAL( TOTAL_O3_COLUMN )
                  TRANSMIS_DIFFUSE( COL,ROW ) = TRANSMISSION
                  TRANSMIS_DIRECT( COL,ROW )  = TRANS_DIRECT
                  REFLECT_COEFF( COL,ROW )    = REFLECTION


                  DO IWL = 1, NWL 
                     TAUO3_TOP_WL( COL,ROW,IWL ) = TAUO3_TOP( IWL )
                     TAU_AERO_WL ( COL,ROW,IWL ) = TAUC_AERO( 1,IWL )
                     TAU_TOT_WL  ( COL,ROW,IWL ) = TAU_TOT  ( 1,IWL )
                     TAU_CLOUD_WL( COL,ROW,IWL ) = TAU_CLOUD( 1,IWL )
#ifdef phot_debug                      
                     SSA_CLOUD_WL( COL,ROW,IWL ) = AVE_SSA_CLD  ( IWL )
                     ASY_CLOUD_WL( COL,ROW,IWL ) = AVE_ASYMM_CLD( IWL )
#endif                      
                  END DO


                  DO L = 1, N_DIAG_WVL
                     IWL = DIAG_WVL( L )
                     FORALL ( LEV = 1:NLAYS_DIAG )
                        ACTINIC_FX( COL,ROW,LEV,L )  = ACTINIC_FLUX( LEV,IWL )
                        TOT_EXT ( COL,ROW,LEV,L )    = 1000.0 * EXTINCTION( LEV,IWL )
                        GAS_EXT ( COL,ROW,LEV,L )    = 1000.0 * GAS_EXTINCTION( LEV,IWL )
                        AERO_EXT( COL,ROW,LEV,L )    = 1000.0 * AERO_EXTI_COEF( LEV,IWL )
                     END FORALL
                     FORALL ( LEV = 1:NLAYS_DIAG, AERO_EXTI_COEF( LEV,IWL ) .GT. EPSLON )
                        AERO_SSA ( COL,ROW,LEV,L ) = AERO_SCAT_COEF( LEV,IWL )
     &                                             / AERO_EXTI_COEF( LEV,IWL )
                        AERO_ASYM( COL,ROW,LEV,L ) = AERO_ASYM_FAC( LEV,IWL )
                     END FORALL
                     FORALL ( LEV = 1:NLAYS_DIAG, AERO_EXTI_COEF( LEV,IWL ) .LE. EPSLON )
                        AERO_SSA ( COL,ROW,LEV,L ) = 1.0
                        AERO_ASYM( COL,ROW,LEV,L ) = 0.0
                     END FORALL
                  END DO

                  IF ( COSZEN .LE. COS85 ) THEN
                     ! calculate because NEW_OPTICS sets BLKDZ and TROPO_O3_COLUMN to zero
                     BLKDZ( 1 ) = BLKZF( 2 )
                     DO L = 2, NLAYS
                        BLKDZ( L ) = BLKZF( L+1 ) - BLKZF( L )
                     END DO
                     MSCALE = 1.0E-4 * CONC_TO_DU ! so units are Dobsons
                     CALL COLUMN_GAS( IGAS=LGC_O3, UNIT_FACTOR=MSCALE, COLUMN_DENSITY=TROPO_OC )
                  ELSE
                     TROPO_OC( COL,ROW ) = TROPO_O3_COLUMN
                  END IF
                  MSCALE = 1.0E-19 ! 100.0*10E-15*PPM_MCM3, so units are petamolecules/cm2
                  CALL COLUMN_GAS( IGAS=LGC_CO,   UNIT_FACTOR=MSCALE, COLUMN_DENSITY=CO_COLUMN   )
                  CALL COLUMN_GAS( IGAS=LGC_SO2,  UNIT_FACTOR=MSCALE, COLUMN_DENSITY=SO2_COLUMN  )
                  CALL COLUMN_GAS( IGAS=LGC_NO2,  UNIT_FACTOR=MSCALE, COLUMN_DENSITY=NO2_COLUMN  )
                  CALL COLUMN_GAS( IGAS=LGC_HCHO, UNIT_FACTOR=MSCALE, COLUMN_DENSITY=HCHO_COLUMN )
               END IF

!Set Photolysis rates to resolved sky values
               FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
                  RJ( COL,ROW, L,IPHOT ) = 60.0 * BLKRJ_RES( L,IPHOT ) 
               END FORALL  ! Loop on layers and NPHOTAB
               FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
                  RJ_RES( COL,ROW, L,IPHOT ) = 60.0 * BLKRJ_RES( L,IPHOT )
               END FORALL  ! Loop on layers and NPHOTAB

!IVAI
! Temporary output
!            aux2d_03( COL,ROW ) = RJ (COL,ROW, 1, LO3O1D)

!Get Photolysis rates to combined (canopy plus resolved model) layers
            KOUNT = 0
            IF ( CANOPY_SHADE ) THEN 
            IF ( FRT_MASK(COL,ROW) > 0. ) THEN

!Set Photolysis rates to pre-determined above and below canopy values
               FORALL ( L = 1:NLAYT, IPHOT = 1:NPHOTAB )
                  RJ_CAN( COL,ROW, L,IPHOT ) = 60.0 * BLKRJ_RES_CAN( L,IPHOT )
               END FORALL
               FORALL ( L = 1:NLAYT, IPHOT = 1:NPHOTAB )
                  RJ_RES_CAN( COL,ROW, L,IPHOT ) = 60.0 * BLKRJ_RES_CAN( L,IPHOT )
               END FORALL

! Print all layers only at KOUNT number of canopy columns 
           IF ( .FALSE. ) THEN
           IF (KOUNT < 3 ) THEN

               DO L = 1, 3     ! NLAYS (from bottom to model top)

! Inputs to new_optics:
                  print*, 'PHOT RJ INPUTS MOD: L= ', L, 
     &     RJ( COL,ROW, L, LO3O1D), 
     &        BLKRJ_RES(L, LO3O1D),
     &            BLKTA(L), 
     &           BLKPRS(L), BLKDENS(L),   ! atm & molec cm-3
     &            BLKZH(L),   BLKZF(L+1),
     &            BLKO3(L),  BLKNO2(L) 

                  print*, 'PHOT RJ INPUTS CAN: L= ', NLAYC + L, 
     &         RJ_CAN( COL,ROW, NLAYC + L, LO3O1D), 
     &         BLKRJ_RES_CAN(NLAYC + L, LO3O1D), 
     &             BLKTA_CAN(NLAYC + L), 
     &            BLKPRS_CAN(NLAYC + L), BLKDENS_CAN(NLAYC + L),
     &             BLKZH_CAN(NLAYC + L),   BLKZF_CAN(NLAYC + L+1),
     &             BLKO3_CAN(NLAYC + L),  BLKNO2_CAN(NLAYC + L) 

               END DO

               DO L = 1, NLAYC ! from bottom to top of canopy
                  print*, 'PHOT RJ INPUTS CAN: L= ', L, 
     &      RJ_CAN( COL,ROW, L, LO3O1D), 
     &         BLKRJ_RES_CAN(L, LO3O1D),
     &             BLKTA_CAN(L),
     &            BLKPRS_CAN(L), BLKDENS_CAN(L),
     &             BLKZH_CAN(L),   BLKZF_CAN(L+1),
     &             BLKO3_CAN(L),  BLKNO2_CAN(L)
               END DO

           END IF ! KOUNT
           END IF ! .FALSE.

! Print up to KOUNT canopy columns
           KOUNT = KOUNT + 1 ! CANOPY_SHADE .AND. FRT_MASK
! End Print


! Temporary output
!           aux2d_02( COL,ROW ) = RJ_CAN( COL,ROW, 2, LO3O1D)
!           aux2d_01( COL,ROW ) = RJ_CAN( COL,ROW, 1, LO3O1D)

            ELSE  ! FRT_MASK <= 0. Non-Canopy columns
!
! Non-canopy columns

! Above canopy layersr:
!Set Photolysis rates to resolved sky values above canopy
               FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
                  RJ_CAN( COL,ROW, NLAYC+L,IPHOT ) = 60.0 * BLKRJ_RES( L,IPHOT )
               END FORALL
               FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
                  RJ_RES_CAN( COL,ROW, NLAYC+L,IPHOT ) = 60.0 * BLKRJ_RES( L,IPHOT )
               END FORALL

! Sub-canopy layers:
!Set Photolysis rates below canopy to 1hy (bottom) model layer
               FORALL ( L = 1:NLAYC, IPHOT = 1:NPHOTAB )
                  RJ_CAN( COL,ROW, L,IPHOT ) = 60.0 * BLKRJ_RES( 1,IPHOT )
               END FORALL
               FORALL ( L = 1:NLAYC, IPHOT = 1:NPHOTAB )
                  RJ_RES_CAN( COL,ROW, L,IPHOT ) = 60.0 * BLKRJ_RES( 1,IPHOT )
               END FORALL

            END IF  ! FRT_MASK
            END IF  ! CANOPY_SHADE

!            IF (KOUNT < 3 ) print*, 'PHOT: BEFORE ACM_CLOUD '
!IVAI

               IF ( USE_ACM_CLOUD ) THEN
                  IF ( ACM_CLOUDS( COL,ROW ) .GT. 0.0 ) THEN
! save resolved sky reflection and transmission coefficients for possible latter use
                     RES_SKY_REFLECT = REFLECTION
                     RES_SKY_TRANS   = TRANSMISSION
                     RES_SKY_TRANSD  = TRANS_DIRECT
!...find the highest layer of the sub-grid (convective) cloud
                     DO LEV = NLAYS, 1, -1
                        IF ( ACM_CFRAC( LEV, COL,ROW ) .GT. 0.0 ) EXIT
                     END DO
!...replace the lower layers with sub-grid cloud properties
                     DO L = 1, LEV    !IVAI: NB. LEV is cloud top
                        SWC( L ) = 0.0
                        IF ( ACM_CFRAC( L,COL,ROW ) .GT. 0.0 ) THEN
                           CLOUDS ( L ) = .TRUE.
                           CLDFRAC( L ) = 1.0
                           MSCALE = 1.0E+3 * DENS ( COL,ROW, L )
                           LWC( L ) = MSCALE * ACM_QC( L,COL,ROW )
                           IWC( L ) = MSCALE * ACM_QI( L,COL,ROW )
                           RWC( L ) = MSCALE * ACM_QR( L,COL,ROW )
                           GWC( L ) = MSCALE * ACM_QG( L,COL,ROW )
                        ELSE
                           CLOUDS( L )  = .FALSE.
                           CLDFRAC( L ) = 0.0
                           LWC( L )     = 0.0
                           IWC( L )     = 0.0
                           RWC( L )     = 0.0
                           GWC( L )     = 0.0
                        END IF
                        CLOUD_LAYERING( L ) = .FALSE.
                     END DO

!IVAI
                  IF ( CANOPY_SHADE ) THEN   ! ACM_CLOUDS
                  IF ( FRT_MASK(COL,ROW) > 0.) THEN
! Canopy columns 
                    ! Above canopy layers
                     DO L = 1, NLAYS
                        CLOUDS_CAN (NLAYC+L) = CLOUDS (L)
                        CLDFRAC_CAN(NLAYC+L) = CLDFRAC(L)
                     END DO

                     ! Set sub-canopy layer values to 1st model layer
                     DO L = 1, NLAYC
                        CLOUDS_CAN (L) = CLOUDS (1) 
                        CLDFRAC_CAN(L) = CLDFRAC(1)
                     END DO

                  ELSE   ! ( .NOT. FRT_MASK )

! Non-Canopy columns
                    ! Above canopy layers
                     DO L = 1, NLAYS
                        CLOUDS_CAN (NLAYC+L) = CLOUDS (L)
                        CLDFRAC_CAN(NLAYC+L) = CLDFRAC(L)
                     END DO

                     ! Set sub-canopy layer values to 1st model layer
                     DO L = 1, NLAYC
                        CLOUDS_CAN (L) = CLOUDS (1)
                        CLDFRAC_CAN(L) = CLDFRAC(1)
                     END DO
                  END IF ! ( FRT_MASK )
                  END IF ! ( CANOPY_SHADE )

!                  IF (KOUNT < 3 ) print*, 'PHOT: IN ACM_CLOUD -> CANOPY_SHADE'
!IVAI
                 
! get optical properties of of subgrid cloud hydrometeors
                     CALL GET_DROPLET_OPTICS( LEV, BLKTA, OWATER_FRAC, SEAICE_FRAC, SNOW_FRAC, LWC )     
                     CALL GET_ICE_OPTICS( LEV, BLKTA, IWC )     
                     CALL GET_AGGREGATE_OPTICS( LEV, RWC, SWC, GWC )     

!...calculate the acm-cloud photolysis rates for all layers:
                     NEW_PROFILE = .FALSE.   
                     CALL NEW_OPTICS ( JDATE, JTIME, NLAYS,
     &                           BLKTA, BLKPRS, BLKDENS, BLKZH, BLKZF,
     &                           BLKO3, BLKNO2,
     &                           ZSFC, COSZEN, SINZEN, RSQD,
     &                           NEW_PROFILE, CLOUDS, CLDFRAC,
     &                           BLKRJ_ACM, TAUC_AERO, TAU_TOT, TAUO3_TOP,
     &                           TAU_RAY, SSA, TAU_CLOUD, TOTAL_O3_COLUMN )

!IVAI: 

!                  IF (KOUNT < 3 ) print*, 'PHOT: AFTER NEW_OPTICS '
!...Call new_optics for the cloudy fraction of the grid cell
!...calculate the acm-cloud photolysis rates for combined (canopy plus resolved) model layers NLAYT
                  IF ( CANOPY_SHADE .AND. FRT_MASK(COL,ROW) > 0.) THEN

                     NEW_PROFILE = .FALSE.
                     CALL NEW_OPTICS ( JDATE, JTIME, NLAYT,  !in: NLAYT scalar
     &                  BLKTA_CAN, BLKPRS_CAN, BLKDENS_CAN,  !in: (NLAYT)
     &                  BLKZH_CAN, BLKZF_CAN,                !in: (NLAYT)
     &                  BLKO3_CAN, BLKNO2_CAN,               !in: (NLAYT)
     &                  ZSFC, COSZEN, SINZEN, RSQD,          !in:        scalar
     &                  NEW_PROFILE,                         !in:        logical
     &                  CLOUDS_CAN, CLDFRAC_CAN,             !in:  (NLAYT) logical
     &                  BLKRJ_ACM_CAN,                       !out: (NLAYT,NPHOTAB) resolved-sky photolysis rates
     &                  TAUC_AERO_CAN, TAU_TOT_CAN,          !out: (NLAYT, NWL)
     &                  TAUO3_TOP_CAN,                       !inout:      (NWL)
     &                  TAU_RAY_CAN, SSA_CAN,                !out:        (NWL)
     &                  TAU_CLOUD_CAN,                       !out: (NLAYS, NWL)
     &                  TOTAL_O3_COLUMN_CAN )                !inout:     scalar
                  END IF
!                  IF (KOUNT < 3 ) print*, 'PHOT: AFTER NEW_OPTICS -> CANOPY_SHADE '
!IVAI

!...load diagnostic file arrays
!...compute a cloud-fraction weighted average of ETOT_SFC and TAU_TOT
!...  note that both TAUC_AERO and TAUO3_TOP are the same for clear and
!...  cloudy regions
                     MSCALE = MAX( 1.0 - ACM_CLOUDS( COL,ROW ), 0.0 )
                     DO IWL = 1, NWL
                       ETOT_SFC_WL ( COL,ROW,IWL ) = MSCALE * ETOT_SFC_WL( COL,ROW,IWL )
     &                                             + ACM_CLOUDS( COL,ROW ) * IRRADIANCE( 1,IWL )
                     END DO

                     IF ( JTIME_CHK .AND. PHOTDIAG ) THEN

                        TRANSMIS_DIRECT( COL,ROW )  = MSCALE * TRANSMIS_DIRECT( COL,ROW )
     &                                              + ACM_CLOUDS( COL,ROW ) * TRANS_DIRECT
                        TRANSMIS_DIFFUSE( COL,ROW ) = MSCALE * TRANSMIS_DIFFUSE( COL,ROW )
     &                                              + ACM_CLOUDS( COL,ROW ) * TRANSMISSION
                        REFLECT_COEFF( COL,ROW )    = MSCALE * REFLECT_COEFF( COL,ROW )
     &                                              + ACM_CLOUDS( COL,ROW ) * REFLECTION
                        DO IWL = 1, NWL
                           TAU_TOT_WL  ( COL,ROW,IWL ) = MSCALE * TAU_TOT_WL( COL,ROW,IWL )
     &                                                 + ACM_CLOUDS( COL,ROW ) * TAU_TOT( 1,IWL )
                           TAU_CLOUD_WL( COL,ROW,IWL ) = MSCALE * TAU_CLOUD_WL( COL,ROW,IWL )
     &                                                 + ACM_CLOUDS( COL,ROW ) * TAU_CLOUD( 1,IWL )
#ifdef phot_debug
                           SSA_CLOUD_WL( COL,ROW,IWL ) = MSCALE * SSA_CLOUD_WL( COL,ROW,IWL )
     &                                                 + ACM_CLOUDS( COL,ROW ) * AVE_SSA_CLD ( IWL )
                           ASY_CLOUD_WL( COL,ROW,IWL ) = MSCALE * ASY_CLOUD_WL( COL,ROW,IWL )
     &                                                 + ACM_CLOUDS( COL,ROW ) * AVE_ASYMM_CLD( IWL )
#endif
                        END DO   ! iwl

                        DO LEV = 1, NLAYS_DIAG
                           DO L = 1, N_DIAG_WVL
                              IWL = DIAG_WVL( L )
                              TOT_EXT( COL,ROW,LEV,L )    = MSCALE * TOT_EXT( COL,ROW,LEV,L )
     &                                                    + ACM_CLOUDS( COL,ROW ) * EXTINCTION( LEV,IWL )
                              ACTINIC_FX( COL,ROW,LEV,L ) = MSCALE * ACTINIC_FX( COL,ROW,LEV,L )
     &                                                    + ACM_CLOUDS( COL,ROW ) * ACTINIC_FLUX( LEV,IWL )
                           END DO
                        END DO
                     END IF  ! photdiag
!Photolysis rates become a weighted average of the values from resolved and ACM skies
                     FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
                        RJ_SUB( COL,ROW, L, IPHOT ) = 60.0 * BLKRJ_ACM( L,IPHOT )
                        RJ( COL,ROW, L, IPHOT )     = ACM_CLOUDS( COL,ROW ) *  RJ_SUB( COL,ROW, L, IPHOT )
     &                                              + MSCALE * RJ( COL,ROW,L,IPHOT )
                     END FORALL  ! Loop on layers and PHOT

!IVAI
! Temporary output: ACM skies
!                    aux2d_03(COL,ROW) = RJ( COL,ROW, 1, LO3O1D)

! Canopy & Non-Canopy Columns:
!Photolysis rates become a weighted average of the values from resolved and ACM skies
                  IF (CANOPY_SHADE .AND. FRT_MASK(COL,ROW) > 0. ) THEN
! Canopy columns
                     FORALL ( L = 1:NLAYT, IPHOT = 1:NPHOTAB )
                        RJ_SUB_CAN( COL,ROW, L, IPHOT ) = 60.0 * BLKRJ_ACM_CAN( L,IPHOT )
                        RJ_CAN( COL,ROW, L, IPHOT ) = ACM_CLOUDS( COL,ROW ) * RJ_SUB_CAN( COL,ROW, L, IPHOT )
     &                                              + MSCALE * RJ_CAN( COL,ROW,L,IPHOT )
                     END FORALL  ! Loop on layers and PHOT

                  ELSE ! Non-Canopy columns   

! Non-Canopy Columns
! Above canopy layers
                     FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
                        RJ_SUB( COL,ROW, L, IPHOT ) = 60.0 * BLKRJ_ACM( L,IPHOT )
                        RJ_CAN( COL,ROW, NLAYC + L, IPHOT ) = ACM_CLOUDS( COL,ROW ) * RJ_SUB( COL,ROW, L, IPHOT )
     &                                                      + MSCALE * RJ_CAN( COL,ROW, NLAYC + L,IPHOT )
                     END FORALL  ! Loop on layers and PHOT

! Sub-canopy layers
                     FORALL ( L = 1:NLAYC, IPHOT = 1:NPHOTAB )
                       RJ_SUB( COL,ROW, 1, IPHOT ) = 60.0 * BLKRJ_ACM( 1,IPHOT )
                       RJ_CAN( COL,ROW, L, IPHOT ) = ACM_CLOUDS( COL,ROW ) * RJ_SUB( COL,ROW, 1, IPHOT )
     &                                             + MSCALE * RJ_CAN( COL,ROW,L,IPHOT )
                     END FORALL  ! Loop on layers and PHOT

                  END IF ! CANOPY_SHADE/FRT_MASK

                  END IF ! ACM_CLOUDS > 0
               END IF ! not USE_ACM_CLOUD and  ACM_CLOUDS > 0

! Print after ACM_CLOUDS correction
         KOUNT = 0
         IF (.FALSE.) THEN
         IF ( CANOPY_SHADE .AND. FRT_MASK(COL,ROW) > 0. ) THEN

! Print all layers only at KOUNT number of canopy columns
            IF (KOUNT < 3 ) THEN

               DO L = 1, 3 ! NLAYS (from bottom to model top)
                  print*, 'PHOT RJ_ACM MOD: L= ', L,
     &              RJ( COL,ROW, L, LO3O1D), BLKRJ_ACM( L,LO3O1D)

                  print*, 'PHOT RJ_ACM CAN: L= ', NLAYC + L,
     &              RJ_CAN( COL,ROW, NLAYC + L, LO3O1D), BLKRJ_ACM_CAN( NLAYC + L,LO3O1D)

               END DO ! NLAYS

               DO L = 1, NLAYC ! from bottom to top of canopy
                  print*, 'PHOT RJ_ACM CAN: L= ', L,
     &              RJ_CAN( COL,ROW, L, LO3O1D), BLKRJ_ACM_CAN( L,LO3O1D)
               END DO

            END IF ! KOUNT

! Print up to KOUNT canopy columns
            KOUNT = KOUNT + 1

         END IF ! CANOPY_SHADE & FRT_MASK
         END IF ! .FALSE.
! End Print

! Temporary output: after ACM skies, before canopy correction
! NB. When dark values are not defined E+38 ???
!        aux2d_03(COL,ROW) =      RJ(COL,ROW,1,LO3O1D)
!        aux2d_02(COL,ROW) =  RJ_CAN(COL,ROW,4,LO3O1D)
!        aux2d_01(COL,ROW) =  RJ_CAN(COL,ROW,1,LO3O1D)
!IVAI

!------------------------CANOPY PHOTOLYSIS CORRECTION/REDUCTION Section NOAA-ARL-------------------------------------------
!Conditions to reduce weighted average of photolysis rates (RJ) due to canopy shading (if user-defined=true); P. C. Campbell
!Following is based on work of ECCC in GEM-MACHv2.1:  Makar et al. (2017)
!Makar, P., Staebler, R., Akingunola, A. et al. The effects of forest canopy shading and turbulence on boundary layer ozone.
!Nat Commun 8, 15243 (2017). https://doi.org/10.1038/ncomms15243

              KOUNT = 0
              IF ( CANOPY_SHADE .AND. FRT_MASK(COL,ROW) > 0. ) THEN ! compute canopy shade reduction factor (RJ_CORR)

!Fraction of cumulative LAI above each canopy layer interface counting down (C1R, C2R, C3R, C4R data from ECCC)

              LAI_C1R = Met_Data%C1R( COL,ROW )
              LAI_C2R = Met_Data%C2R( COL,ROW )
              LAI_C3R = Met_Data%C3R( COL,ROW )
              LAI_C4R = Met_Data%C4R( COL,ROW )

!Calculate the attenuation below the various fractions of canopy heights
              !RJ_CORR effectly represents the beam attenuation and reduces photolysis.
              !Nilson, T. A theoretical analysis of the frequency of gaps in plant stands. Agric.
              !Meterol. 8, 25�~Z~L~Z~L~@~S38 (1971).
              COEFF = 0.5 * (Met_Data%LAIE(COL,ROW) *Met_Data%CLU(COL,ROW))/MAX(0.05, COSZEN)

              RJ_CORRX6(1) = 1. ! top of canopy
              RJ_CORRX6(2) = MAX(1.0E-10, EXP(- LAI_C1R * COEFF ))
              RJ_CORRX6(3) = MAX(1.0E-10, EXP(- LAI_C2R * COEFF ))
              RJ_CORRX6(4) = MAX(1.0E-10, EXP(- LAI_C3R * COEFF ))
              RJ_CORRX6(5) = MAX(1.0E-10, EXP(- LAI_C4R * COEFF ))
              RJ_CORRX6(6) = MAX(1.0E-10, EXP(- COEFF ))

              RJ_CORR_TOP( COL,ROW ) = 1. ! top of canopy
              RJ_CORR_C1R( COL,ROW ) = MAX(1.0E-10, EXP(- LAI_C1R * COEFF )) 
              RJ_CORR_C2R( COL,ROW ) = MAX(1.0E-10, EXP(- LAI_C2R * COEFF ))
              RJ_CORR_C3R( COL,ROW ) = MAX(1.0E-10, EXP(- LAI_C3R * COEFF ))
              RJ_CORR_C4R( COL,ROW ) = MAX(1.0E-10, EXP(- LAI_C4R * COEFF ))
              RJ_CORR_BOT( COL,ROW ) = MAX(1.0E-10, EXP(- COEFF ))

              ZCANX6(1) = Met_Data%FCH( COL,ROW )
              ZCANX6(2) = ZCANX6(1) *0.75
              ZCANX6(3) = ZCANX6(1) *0.50
              ZCANX6(4) = ZCANX6(1) *0.35
              ZCANX6(5) = ZCANX6(1) *0.20
              ZCANX6(6) = 0.0

              RJ_CORRX (:) = 0.0
              DO LEV = 1, NLAYS !loop through model layers

                 IF (LEV .EQ. 1) THEN !first model layer
                    KCAN = 1
                 ELSE               !check subsequent model layers
                    IF (  Met_Data%FCH( COL,ROW ) .GT. Met_Data%ZF( COL,ROW,LEV-1 )
     &               .AND. Met_Data%FCH( COL,ROW ) .LE. Met_Data%ZF( COL,ROW,LEV ) ) THEN
                        KCAN = 1
                    ELSE
                        KCAN = 0
                    END IF
                 END IF

                 IF (KCAN .EQ. 1) THEN !canopy could be inside model layer

!Interpolate to get attenuation profile below canopy inside respective model layer 
                    ZFL = Met_Data%ZF( COL,ROW,LEV )
                    ZCAN = ZFL    ! Initialize top (m) = Bottom of model layer
                    COUNTCAN = 0  ! Initialize canopy layers

                    IF (LEV .EQ. 1) THEN  !Find bottom in each model layer
                       BOTCAN = 0.5
                    ELSE
                        BOTCAN = Met_Data%ZF( COL,ROW,LEV-1 )
                    END IF

                    DO WHILE (ZCAN.GE.BOTCAN)
                        IF ( ZCAN .GT. Met_Data%FCH( COL,ROW ) ) THEN
                           COUNTCAN = COUNTCAN + 1
                           ZCANX(COUNTCAN) = ZCAN
                           RJ_CORRX (COUNTCAN) = 1.0
                        ELSE IF ( ZCAN .LE. Met_Data%FCH( COL,ROW )   .AND.
     &                            ZCAN .GT. Met_Data%FCH( COL,ROW )*0.75 ) THEN
                           COUNTCAN = COUNTCAN + 1
                           XCAN(2) = Met_Data%FCH( COL,ROW )
                           YCAN(2) = 1.0
                           XCAN(1) = Met_Data%FCH( COL,ROW )*0.75
                           YCAN(1) = RJ_CORR_C1R( COL,ROW )
                           XCANOUT = ZCAN
                           ZCANX(COUNTCAN) = ZCAN
                           RJ_CORRX (COUNTCAN) = interp_linear1_internal(XCAN,YCAN,XCANOUT)
                        ELSE IF ( ZCAN .LE. Met_Data%FCH( COL,ROW )*0.75   .AND.
     &                                       ZCAN .GT. Met_Data%FCH( COL,ROW )*0.50 ) THEN
                           COUNTCAN = COUNTCAN + 1
                           XCAN(2) = Met_Data%FCH( COL,ROW )*0.75
                           YCAN(2) = RJ_CORR_C1R( COL,ROW )
                           XCAN(1) = Met_Data%FCH( COL,ROW )*0.50
                           YCAN(1) = RJ_CORR_C2R( COL,ROW )
                           XCANOUT = ZCAN
                           ZCANX(COUNTCAN) = ZCAN
                           RJ_CORRX (COUNTCAN) = interp_linear1_internal(XCAN,YCAN,XCANOUT)
                        ELSE IF ( ZCAN .LE. Met_Data%FCH( COL,ROW )*0.50   .AND.
     &                            ZCAN .GT. Met_Data%FCH( COL,ROW )*0.35 ) THEN
                           COUNTCAN = COUNTCAN + 1
                           XCAN(2) = Met_Data%FCH( COL,ROW )*0.50
                           YCAN(2) = RJ_CORR_C2R( COL,ROW )
                           XCAN(1) = Met_Data%FCH( COL,ROW )*0.35
                           YCAN(1) = RJ_CORR_C3R( COL,ROW )
                           XCANOUT = ZCAN
                           ZCANX(COUNTCAN) = ZCAN
                           RJ_CORRX (COUNTCAN) = interp_linear1_internal(XCAN,YCAN,XCANOUT)
                        ELSE IF ( ZCAN .LE. Met_Data%FCH( COL,ROW )*0.35  .AND.
     &                            ZCAN .GT. Met_Data%FCH( COL,ROW )*0.20 ) THEN
                           COUNTCAN = COUNTCAN + 1
                           XCAN(2) = Met_Data%FCH( COL,ROW )*0.35
                           YCAN(2) = RJ_CORR_C3R( COL,ROW )
                           XCAN(1) = Met_Data%FCH( COL,ROW )*0.20
                           YCAN(1) = RJ_CORR_C4R( COL,ROW )
                           XCANOUT = ZCAN
                           ZCANX(COUNTCAN) = ZCAN
                           RJ_CORRX (COUNTCAN) = interp_linear1_internal(XCAN,YCAN,XCANOUT)
                        ELSE IF ( ZCAN .LE. Met_Data%FCH( COL,ROW )*0.20 ) THEN
                           COUNTCAN = COUNTCAN + 1
                           XCAN(2) = Met_Data%FCH( COL,ROW )*0.20
                           YCAN(2) = RJ_CORR_C4R( COL,ROW )
                           XCAN(1) = 0.5
                           YCAN(1) = RJ_CORR_BOT( COL,ROW )
                           XCANOUT = ZCAN
                           ZCANX(COUNTCAN) = ZCAN
                           RJ_CORRX (COUNTCAN) = interp_linear1_internal(XCAN,YCAN,XCANOUT)
                        END IF

                        ZCAN = ZCAN-0.5  !step down in-canopy resolution of 0.5 m

                    END DO !end loop on canopy layers

!Integrate to get best attenuation value to use within canopy
                    RJ_CORR( COL,ROW ) = IntegrateTrapezoid(ZCANX (COUNTCAN:1:-1),RJ_CORRX (COUNTCAN:1:-1)) /
     &              ZFL

!Apply attenuation factors above and below canopy
                    RJ( COL,ROW, LEV, : ) = RJ( COL,ROW, LEV, : )*RJ_CORR( COL,ROW )

!Apply attenuation value within canopy and take average above and within canopy values

                 END IF  !IF (KCAN .EQ. 1) canopy height could be inside model layer

              END DO  !end loop on model layers

!IVAI
! Work out correction factors for layers above and below canopy (resolved model layers plus canopy layers)
              DO L = 1, NLAYT       ! from top to bottom of combined (canopy plus resolved model) layers

                 ZFT(L) = ZMOM_CAN(COL,ROW, L)

! Correction factor for all model layers (above and below canopy)
                 if (ZFT(L) >= ZCANX6(1)) then
                   rj_corrf(L) = 1.0 ! no attenuation; model layer is above canopy height
                 else
                   ! loop through 5 cumulative LAI fraction layers
                   DO KK = 1, MAXCAN6 - 1 !from top FCH to bottom of canopy
! ZCANX6(1) = Met_Data%FCH( COL,ROW )
! ZCANX6(2) = ZCANX6(1) *0.75
! ZCANX6(3) = ZCANX6(1) *0.50
! ZCANX6(4) = ZCANX6(1) *0.35
! ZCANX6(5) = ZCANX6(1) *0.20
! ZCANX6(6) = 0.0
                   if (ZFT(L) < ZCANX6(KK) .and. ZFT(L) >= ZCANX6(KK+1))
     &              rj_corrf(L) = RJ_CORRX6(KK+1)
     &                         + (RJ_CORRX6(KK) - RJ_CORRX6(KK+1))
     &                         / (   ZCANX6(KK) -    ZCANX6(KK+1)) 
     &                         * (       ZFT(L) -    ZCANX6(KK+1))
                   ENDDO

                 end if

              END DO ! L = 1, NLAYT
              rj_corrf(NLAYT + 1) = RJ_CORRX6(6)

! Mid-level correction:  average of full level corrections
              DO L = 1, NLAYT       ! from top to bottom of combined layers
                rj_corrmid(L) = (rj_corrf(L) + rj_corrf(L+1)) * 0.5
              END DO

              DO L = 1, NLAYT       ! from top to bottom of combined layers
                II = NLAYT + 1 - L ! from bottom to top of combined layers
                RJ_CORRM(COL,ROW, II) = rj_corrmid(L)
              END DO

!  Scale the model rate constants downward due to shading:
              FORALL ( L = 1:NLAYT, IPHOT = 1:NPHOTAB )
                RJ_CAN( COL,ROW, L, IPHOT ) = RJ_CAN( COL,ROW, L, IPHOT )*RJ_CORRM( COL,ROW, L )
              END FORALL  ! Loop on layers and IPHOT

!IVAI
! Print after RJ_CORR correction
! Print up to KOUNT number of canopy columns
!           IF (KOUNT < 3 ) THEN

!              DO L = 1, 3 ! NLAYS (from bottom to model top)
!                 print*, 'PHOT RJ_CORR MOD: L= ', L,
!    &              RJ( COL,ROW, L, LO3O1D), RJ_CORR( COL,ROW )

!                print*, 'PHOT RJ_CORR CAN: L= ', NLAYC + L,
!    &              RJ_CAN( COL,ROW, NLAYC + L, LO3O1D), RJ_CORRM( COL,ROW, NLAYC + L )

!             END DO ! NLAYS

!             DO L = 1, NLAYC ! from bottom to top of canopy
!                print*, 'PHOT RJ_CORR CAN: L= ', L,
!    &              RJ_CAN( COL,ROW, L, LO3O1D), RJ_CORRM( COL,ROW, L )
!             END DO

!          END IF ! KOUNT

! Print up to KOUNT canopy columns
!          KOUNT = KOUNT + 1

! End Print

              END IF  ! (CANOPY_SHADE & FRT_MASK )

!               print*, 'PHOT:  AFTER RJ CANOPY CORRECTION '
!IVAI

               IF ( JTIME_CHK .AND. PHOTDIAG ) THEN ! compute clear sky reflection and transmission coefficients
                  IF ( ANY( CLOUDS ) ) THEN
                     IF ( CFRAC_2D( COL,ROW ) .GT. 0.0 ) THEN ! resolved and subgrid clouds exist
                        CLOUDS         = .FALSE.
                        NEW_PROFILE    = .FALSE.
                        ONLY_SOLVE_RAD = .TRUE.
                        CALL NEW_OPTICS ( JDATE, JTIME, NLAYS, 
     &                                    BLKTA, BLKPRS, BLKDENS, BLKZH, BLKZF,
     &                                    BLKO3, BLKNO2,
     &                                    ZSFC, COSZEN, SINZEN, RSQD,
     &                                    NEW_PROFILE, CLOUDS, CLDFRAC,
     &                                    BLKRJ_RES, TAUC_AERO, TAU_TOT, TAUO3_TOP,
     &                                    TAU_RAY, SSA, TAU_CLOUD, TOTAL_O3_COLUMN)
                        CLR_REFLECTION  ( COL,ROW ) = REFLECTION
                        CLR_TRANSMISSION( COL,ROW ) = TRANSMISSION
                        CLR_TRANS_DIRECT( COL,ROW ) = TRANS_DIRECT
                     ELSE ! only subgrid clouds exist
                        CLR_REFLECTION  ( COL,ROW ) = RES_SKY_REFLECT
                        CLR_TRANSMISSION( COL,ROW ) = RES_SKY_TRANS
                        CLR_TRANS_DIRECT( COL,ROW ) = RES_SKY_TRANSD
                     END IF
                  ELSE ! no cloud in vertical column
                     CLR_REFLECTION  ( COL,ROW ) = REFLECTION
                     CLR_TRANSMISSION( COL,ROW ) = TRANSMISSION
                     CLR_TRANS_DIRECT( COL,ROW ) = TRANS_DIRECT
                  END IF
               END IF

            END DO LOOP_COLS
         END DO LOOP_ROWS

      END IF

      ! Store PM Diagnostic AOD and extinction
      ELMO_AOD_550 = TAU_AERO_550
      ELMO_EXT_550 = AERO_EXT_550

!...report on whether stratospheric ozone column satisfies climatological minimums
      IF( N_TROPO_O3_TOGGLE .GT. 0 )THEN
         O3_TOGGLE_AVE = O3_TOGGLE_AVE / REAL( N_TROPO_O3_TOGGLE )
         WRITE( LOGDEV, 9500 )'PHOT: Exceedance of tropospheric ozone column ',
     &   'or below top of model domains based on stratospheric column minimum ',
     &   'at date and time; ', JDATE, JTIME, N_TROPO_O3_TOGGLE, (1.0/O3_TOGGLE_AVE - 1.0),
     &    (1.0/O3_TOGGLE_MIN - 1.0)
      END IF

!...write diagnostic data to output file at the end of every output tstep

      IF ( JTIME_CHK ) THEN
        IF ( PHOTDIAG ) THEN

         VARNM = 'COSZENS'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME,
!IVAI: 
! hrdriver & phot:
!    &      o3old ( :,:,1 )          ) ) THEN ! 
!    &      AUX2D_03                 ) ) THEN ! "rj_can" hrdriver/phot
!    &      AUX2D_02                 ) ) THEN ! "o3_new_can" hrdriver
!    &      AUX2D_01                 ) ) THEN ! "o3_tend_can" hrdriver
!
! canopy_transfer:
!    &      CONC_MOD  (:,:, 2  , LGC_O3 )  ) ) THEN  ! O3 = 4
!    &      CGRID      (:,:, 1  , LGC_O3 )  ) ) THEN  ! O3 = 4
!    &      CONC_CAN  (:,:, 4  , LGC_O3 )  ) ) THEN  !
!    &      CONC_2M   (:,:,      LGC_NO2)  ) ) THEN  !
!    &      frctc2r(NLAYS    ,1,:,:)     ) ) THEN
! NB. nfrct(NLAYS    ) = 1-2
!      "   (NLAYS + 1) = 1
!      "   (NLAYS + 2) = 1
!      "   (NLAYS + 3) = 1
!    &  float(nfrct(NLAYS    , :,:  ))) ) THEN
!    &  float(nfrct(NLAYS + 1, :,:  ))) ) THEN  ! 3rd (top) canopy layer
!    &  float(nfrct(NLAYS + 2, :,:  ))) ) THEN  ! 2nd (mid) canopy layer
!    &  float(nfrct(NLAYS + 3, :,:  ))) ) THEN  ! 1st (bot) canopy layer
!
! phot:
!    &      RJ_CORRM( :,:,1 )        ) ) THEN
!    &      RJ      ( :,:,1, LO3O1D) ) ) THEN
!    &      RJ_CAN  ( :,:,3, LO3O1D) ) ) THEN 
     &      COSINE_ZENITH            ) ) THEN
!    &      Grid_Data%AREA (:,:)     ) ) THEN
!
! can_mask:
!    &      FRT_mask) ) THEN
!
! can_levs_defn:
!    &      TA_CAN  ( :,:,3 )        ) ) THEN 
!    &      QV_CAN  ( :,:,3 )        ) ) THEN 
!    &      PRES_CAN( :,:,4 )        ) ) THEN
!    &      DENS_CAN( :,:,1 )        ) ) THEN  ! 1st canopy layer

!    &      ZM      (:,:,1 )        ) ) THEN   ! 1st model  layer height ~ 23.9775m
!    &      ZFULL   (:,:,1 )        ) ) THEN   ! 1st model  layer height ~ 41.37m to 48.15m
!
!    &      ZMID_CAN(:,:,NLAYS )    ) ) THEN   ! 1st model  layer (reversed layer order) <52.78m
!
!    &      ZMID_CAN (:,:,NLAYS + 1) ) ) THEN   ! top canopy layer (reversed layer order) <26.39m
!    &      ZMOM_CAN (:,:,NLAYS + 1) ) ) THEN   ! top canopy layer (reversed layer order)
!    &        ZF_CAN (:,:,3        ) ) ) THEN   ! = ZMOM_CAN(NLAYS + 1)

!    &      SIGMID_CAN(:,:,NLAYS + 1) ) ) THEN
!    &      SIGMOM_CAN(:,:,NLAYS + 1) ) ) THEN
!IVAI
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'OZONE_COLUMN'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TOTAL_OC ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'CO_COLUMN'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, CO_COLUMN ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         VARNM = 'SO2_COLUMN'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, SO2_COLUMN ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         VARNM = 'NO2_COLUMN'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, NO2_COLUMN ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         VARNM = 'HCHO_COLUMN'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, HCHO_COLUMN ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'TROPO_O3_COLUMN'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TROPO_OC ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'TRANS_DIFFUSE'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TRANSMIS_DIFFUSE ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'TRANS_DIRECT'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TRANSMIS_DIRECT ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'REFLECTION'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, REFLECT_COEFF ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'CLR_TRANS_DIF'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, CLR_TRANSMISSION ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'CLR_TRANS_DIR'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, CLR_TRANS_DIRECT ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'CLR_REFLECTION'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, CLR_REFLECTION ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'TROPO_O3_EXCEED'
         TROPO_O3_EXCEED = TROPO_O3_EXCEED / REAL( MAX(1, TSTEP_COUNT) )
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TROPO_O3_EXCEED ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         TROPO_O3_EXCEED = 0.0 ! reset sum and counter
         TSTEP_COUNT     = 0

         VARNM = 'N_EXCEED_TROPO3'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, N_EXCEED_TROPO3 ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         N_EXCEED_TROPO3 = 0.0 ! reset counter

         VARNM = 'JNO2'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, 
! IVAI
! hrdriver:
!    &      o3new ( :,:,1 )          ) ) THEN
!    &      AUX2D_03                 ) ) THEN ! "rj_can" hrdriver/phot
!    &      AUX2D_02                 ) ) THEN ! "o3_new_can" hrdriver
!    &      AUX2D_01                 ) ) THEN ! "o3_tend_can" hrdriver/rbdriver
! canopy_transfer:
!    &      CONC_MOD  (:,:, 1  , LGC_NO2)  ) ) THEN
!    &      CONC_CAN  (:,:, 3  , LGC_O3 )  ) ) THEN
!    &      CONC_2M   (:,:,    , LGC_NO2)  ) ) THEN
! NB. ifrct (NLAYS +1, 1) = 63-64  (resolved to canopy)
!       "   (NLAYS +1, 1) = 64
!       "   (NLAYS +2, 1) = 64  ifrct (NLAYS +1, 2) = 64
!       "   (NLAYS +3, 1) = 64
!    & float(ifrct (NLAYS    ,1,:,:))    ) ) THEN
!    & float(ifrct (NLAYS + 1,1,:,:))    ) ) THEN   ! 3rd (top) canopy layer (NLAYT - 2)
!    & float(ifrct (NLAYS + 2,1,:,:))    ) ) THEN   ! 2nd (mid) canopy layer (NLAYT - 1)
!    & float(ifrct (NLAYT    ,1,:,:))    ) ) THEN   ! 1st (bot) canopy layer
!
! phot:
     &      RJ       ( :,:,1, LNO2  )) ) THEN
!    &      RJ       ( :,:,1, LO3O1D)) ) THEN
!    &      RJ_CAN   ( :,:,1, LNO2  )) ) THEN
!    &      RJ_CORRM ( :,:,1 )       ) ) THEN  
!
! can_levs_defn:
!    &      TA_CAN   ( :,:,2 )       ) ) THEN 
!    &      QV_CAN   ( :,:,2 )       ) ) THEN
!    &      PRES_CAN ( :,:,5 )       ) ) THEN  ! 2hy model layer
!    &      PRES_CAN ( :,:,2 )       ) ) THEN
!    &      DENS_CAN ( :,:,5 )       ) ) THEN  ! 2hy model layer
!    &      DENS_CAN ( :,:,1 )       ) ) THEN  ! 2nd canopy layer
!
!    &      ZM       ( :,:,NLAYS)     ) ) THEN ! top model  layer height ~ 57,229.7m
!    &      ZM       ( :,:,NLAYS - 1) ) ) THEN ! second to top    model layer ~ 49,654.2m 
!    &      ZFULL    ( :,:,NLAYS - 1) ) ) THEN ! second to top    model layer ~ 47,529.9 to 53,107.2m

!    &      ZMID_CAN (:,:,1)         ) ) THEN ! top    model layer height <57,229.7m
!
!    &      ZMID_CAN (:,:,NLAYS + 2) ) ) THEN ! 2nd canopy layer (reversed layer order) < 22.7274m
!    &      ZMOM_CAN (:,:,NLAYS + 2) ) ) THEN
!    &        ZF_CAN (:,:,2        ) ) ) THEN

!    &      SIGMID_CAN(:,:,NLAYS + 2) ) ) THEN ! 2nd canopy layer
! IVAI 
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'JO3O1D'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, 
!IVAI
! rbdriver/hrdriver:
!    &      o3tend( :,:,1 )          ) ) THEN
!    &      AUX2D_03                 ) ) THEN ! "rj_can" hrdriver/phot
!    &      AUX2D_02                 ) ) THEN ! "o3_new_can" hrdriver/rbdriver
!    &      AUX2D_01                 ) ) THEN ! "o3_tend_can"/"o3_mod_1hy"
! canopy_transfer:
!    &      CONC_MOD  (:,:, 1  , LGC_O3 )  ) ) THEN  ! 1hy model layer
!    &      CONC_CAN  (:,:, 1  , LGC_O3 )  ) ) THEN  ! 1st (bot) canopy payer
!    &      CONC_CAN  (:,:, 1  , LGC_O3 )  ) ) THEN
!    &      CONC_2M   (:,:,      LGC_O3 )  ) ) THEN
!    &          massair(:,:,NLAYS - 1 )    ) ) THEN  ! 2hy layer
!    &      massair_can(:,:,NLAYT)    ) ) THEN 
!
! NB. FC2R(NLAYS    ,1) < ??  (canopy to resolved)
!    &      frctc2r(NLAYT    ,1,:,:)      ) ) THEN
! NB. FR2C(NLAYS    ,1) < 0.7  (resolved to canopy)
!       " (NLAYS + 1,1) < 0.35
!       " (NLAYS + 2,1) < 0.35     
!       " (NLAYT    ,1) < 0.35
!    &      frctr2c(NLAYS    ,1,:,:)      ) ) THEN
!    &      frctr2c(NLAYS + 1,1,:,:)      ) ) THEN  ! 3rd (top) canopy layer
!    &      frctr2c(NLAYS + 2,1,:,:)      ) ) THEN  ! 2nd (mid) canopy layer
!    &      frctr2c(NLAYT    ,1,:,:)      ) ) THEN  ! 1st (bot) canopy layer
! phot:
     &      RJ       ( :,:,1 ,LO3O1D) ) ) THEN   ! bottom model  layer
!    &      RJ_CAN   ( :,:,1 ,LO3O1D) ) ) THEN  
!    &      RJ_CORRM ( :,:,1 )        ) ) THEN
! can_levs_defn:
!    &      TA_CAN   ( :,:,1 )        ) ) THEN   ! 1st canopy layer
!    &      QV_CAN   ( :,:,1 )        ) ) THEN
!    &      PRES_CAN ( :,:,4 )        ) ) THEN   ! 1hy model layer
!    &      PRES_CAN ( :,:,1 )        ) ) THEN   ! 
!    &      DENS_CAN ( :,:,4 )        ) ) THEN   ! 1hy model layer
!    &      DENS_CAN ( :,:,1 )        ) ) THEN   ! 1st canopy layer
!
!    &      ZM       ( :,:,NLAYS - 2) ) ) THEN   ! 3rd top model layer height ~ 44,695.3m
!    &      ZM       ( :,:,2 )        ) ) THEN   ! 2nd model layer height ~64.903-75.555m
!    &      ZFULL    ( :,:,2 )        ) ) THEN   ! 2nd model layer height ~88.43-102.956m

!    &      ZMID_CAN ( :,:,NLAYS - 1) ) ) THEN   ! 2nd model layer (reversed layer order) <75.183m
!
!    &      ZMID_CAN ( :,:,NLAYT    ) ) ) THEN   ! bottom canopy layer (reversed layer order) <10.56m
!    &      ZMOM_CAN ( :,:,NLAYT    ) ) ) THEN   ! bottom canopy layer (reversed layer order)
!    &        ZF_CAN ( :,:,1        ) ) ) THEN   ! = ZMOM_CAN (NLAYT)

!    &      SIGMID_CAN(:,:,NLAYT    ) ) ) THEN   ! 1st (bot) canopy layer (reversed layer order)
!IVAI
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'RESOLVED_CFRAC'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, CFRAC_2D ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         VARNM = 'RESOLVED_WBAR'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, AVE_HYDROMETEORS ) ) THEN
             XMSG = 'Error writing variable ' // VARNM
             CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IF ( USE_ACM_CLOUD ) THEN
             VARNM = 'SUBGRID_CFRAC'
             IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, ACM_CLOUDS ) ) THEN
                XMSG = 'Error writing variable ' // VARNM
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
             VARNM = 'SUBGRID_WBAR'
             IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, ACM_AVE_H2O ) ) THEN
                XMSG = 'Error writing variable ' // VARNM
                CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
             END IF
         END IF

         DO IWL = 1, NWL

            VARNM = 'ETOT_SFC_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, ETOT_SFC_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, ODATE, OTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'AOD_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, TAU_AERO_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TAU_CLOUD_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, TAU_CLOUD_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

#ifdef phot_debug
            VARNM = 'SSA_CLOUD_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, SSA_CLOUD_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'ASY_CLOUD_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, ASY_CLOUD_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
#endif

            VARNM = 'TAU_TOT_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, TAU_TOT_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TAUO3_TOP_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE,
     &                         OTIME, TAUO3_TOP_WL( :,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'ALBEDO_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME,
     &                         SURFACE_ALBEDO( IWL,:,: ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

         END DO  ! iwl

         VARNM = 'AOD_W550_ANGST'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TAU_AERO_550 ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &          'Photolysis Surface Summary written to', CTM_RJ_1,
     &          'for date and time', ODATE, OTIME

         DO IPHOT = 1, NPHOTAB
            OUTPUT_BUFF( 1:NCOLS,1:NROWS,1:NLAYS_DIAG ) = RJ( 1:NCOLS,1:NROWS,1:NLAYS_DIAG,IPHOT )
            IF ( .NOT. WRITE3( CTM_RJ_2, PHOTAB( IPHOT ), ODATE,
     &                         OTIME, OUTPUT_BUFF  ) ) THEN
               XMSG = 'Could not write ' // CTM_RJ_2 // ' file'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END DO

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &          'Photolysis Rates written to', CTM_RJ_2,
     &          'for date and time', ODATE, OTIME

         VARNM = 'CFRAC_3D'
         OUTPUT_BUFF( 1:NCOLS,1:NROWS,1:NLAYS_DIAG ) = CFRAC_3D( 1:NCOLS,1:NROWS,1:NLAYS_DIAG )
         IF ( .NOT. WRITE3( CTM_RJ_3, VARNM, ODATE, OTIME, OUTPUT_BUFF ) ) THEN
              XMSG = 'Could not write ' // TRIM( VARNM ) // ' to ' // CTM_RJ_3 // ' file'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         DO L = 1, N_DIAG_WVL
            IWL = DIAG_WVL( L )

            VARNM = 'ACTINIC_FX_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_3, VARNM, ODATE, OTIME, ACTINIC_FX( :,:,:,L ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'AERO_SSA_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_3, VARNM, ODATE, OTIME, AERO_SSA( :,:,:,L ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'AERO_ASYM_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_3, VARNM, ODATE, OTIME, AERO_ASYM( :,:,:,L ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'EXT_AERO_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_3, VARNM, ODATE, OTIME, AERO_EXT( :,:,:,L ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'EXT_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_3, VARNM, ODATE, OTIME, TOT_EXT( :,:,:,L ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'GAS_EXT_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_3, VARNM, ODATE, OTIME, GAS_EXT( :,:,:,L ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END DO

         VARNM = 'EXT_AERO_W550'
         IF ( .NOT. WRITE3( CTM_RJ_3, VARNM, ODATE, OTIME, AERO_EXT_550( :,:,: ) ) ) THEN
             XMSG = 'Error writing variable ' // VARNM
             CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &          'Radiative and Optical Data written to', CTM_RJ_3,
     &          'for date and time', ODATE, OTIME

        END IF   ! PHOTDIAG
      END IF   ! if JTIME_CHK 
      TAU_AERO_550 = 0.0

1003  FORMAT( 8X, 'Processor ',I4.4,' is in darkness at ', I8.7, ':', I6.6,
     &        1X, 'GMT - no photolysis')
9500  FORMAT(3(/ A), I7, 1X, I6.6, 1X, / "Total Number: ", I9, ";Mean Value: ", F9.6,
     &       "; Max Value: ",F9.6 /)

95101 FORMAT('Diagnostic Output will have zero values for the column density.',
     &     / 'The lack of information does not affect model predictions.' )

      CONTAINS
          SUBROUTINE COLUMN_GAS( IGAS, UNIT_FACTOR, COLUMN_DENSITY )
! Purpose: calculates column density in unit based on the value of UNIT_FACTOR
             IMPLICIT NONE
! argument:
               INTEGER, INTENT( IN  ) :: IGAS                  ! species index in CGRID
               REAL,    INTENT( OUT ) :: COLUMN_DENSITY( :,: ) ! units determined by inputs
               REAL,    INTENT( IN  ) :: UNIT_FACTOR           ! converts from 10E6*molecules*cm-2
! local parameter:
!               REAL, PARAMETER :: UNIT_FACTOR = 1.0E-6 * CONC_TO_DU ! unit conversion factor

               IF( IGAS .LE. 0 )RETURN ! assumes column_density set to zero at allocation

               COLUMN_DENSITY( COL,ROW ) = 0.0
               DO LEV = 1, NLAYS
                  COLUMN_DENSITY( COL,ROW ) = ( UNIT_FACTOR * BLKDENS( LEV ) )
     &                                      * CGRID( COL,ROW,LEV,IGAS  ) * BLKDZ ( LEV )
     &                                      + COLUMN_DENSITY( COL,ROW )
               END DO

          END SUBROUTINE COLUMN_GAS
      END SUBROUTINE PHOT
