
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


! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header: /project/yoj/arc/CCTM/src/phot/phot_inline/phot.F,v 1.7 2011/10/21 16:11:28 yoj Exp $

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!IVAI
!     SUBROUTINE PHOT ( MDATE, MTIME, JDATE, JTIME, DTSTEP, RJ )
      SUBROUTINE PHOT ( MDATE, MTIME, JDATE, JTIME, DTSTEP, RJ, 
     &                  o3old   , o3new,    o3tend,     !IVAI: 3D
     &                  aux2d_01, aux2d_02, aux2d_03 )  !IVAI: 2D
!IVAI

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
! Subroutines/Functions called: INIT3, M3EXIT, SUBHFILE, CGRID_MAP,
!     OPPHOT, LOAD_CSQY_DATA, LOAD_OPTICS_DATA, INITIALIZE_ALBEDO, 
!     GET_PHOT_MET, UPDATE_SUN, GET_ALBEDO, GET_DROPLET_OPTICS, 
!     GET_ICE_OPTICS, GET_AGGREGATE_OPTICS, CLEAR_HYDROMETEOR_OPTICS, 
!     GET_AERO_DATA, O3TOTCOL, NEW_OPTICS
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
!         ETOT_SFC, TAU_AERO, TAU_TOT and TAUO3_TOP values for 7 wavelengths
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
!       - revised NEW_OPTICS' arguments based on  aerosol redesign in CMAQ
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
!                          5) inserted calculation of aerosol optical properites via
!                             fortran module to improve efficiency in radiative
!                             transfer solution
!                          6) moved the O3TOTCOL routine from the PHOT_MOD to simplify
!                             the NEW_OPTICS routine
!                          7) Several miscellaneous changes attempting to improve efficiency
!     10 Jun 15 J.Young: Modified diagnostic output timestamp to fix for other than one
!                         hour time steps.
!     12 Aug 15 D.Wong:  Replaced MYPE with IO_PE_INCLUSIVE for parallel I/O implementation
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

      USE RXNS_DATA            ! chemistry varaibles and data
      USE CGRID_SPCS           ! CGRID species number and offsets
      USE PCGRID_DEFN          ! get cgrid
      USE UTILIO_DEFN
      USE AERO_DATA            ! describes aerosol distribution
      USE PHOT_MOD             ! photolysis in-line module - inherits CSQY_DATA module
      USE AERO_PHOTDATA        ! arrays and routines for aerosol dimensions and refractive indices
      USE PHOTOLYSIS_ALBEDO    ! surface albedo data and routines
      USE PHOT_MET_DATA        ! Met and Grid data
      USE CLOUD_OPTICS         ! data and routines for optics of cloud hydrometeors
!      USE STRATOS_O3_MINFRACS  ! annual minimum fraction of ozone column density above Pressure TOP
!      USE SEAS_STRAT_O3_FRACS  ! monthly minimum fraction of ozone column density above Pressure TOP
      USE SEAS_STRAT_O3_MIN    ! monthly minimum fraction of ozone column density above Pressure TOP
!Used for canopy shade calculation
      USE ASX_DATA_MOD, ONLY : MET_DATA, GRID_DATA     !uses met data
!IVAI
! NB. Moved NLAYC/NLAYT definitions to GRID_CONF (Jan 9 2025)
      use can_mask             !FRT_mask
      use can_levs_defn
      use can_trans_mod
!IVAI
      USE centralized_io_util_module, ONLY: IntegrateTrapezoid, interp_linear1_internal !basic utilities

#ifdef parallel
      USE SE_MODULES           ! stenex (using SE_UTIL_MODULE)
#else
      USE NOOP_MODULES         ! stenex (using NOOP_UTIL_MODULE)
#endif

      IMPLICIT NONE

!...include files

      INCLUDE SUBST_FILES_ID   ! file name parameters
!     INCLUDE SUBST_CONST      ! physical constants--moved to PHOT_MOD.

!...arguments

      INTEGER, INTENT( IN ) :: MDATE         ! "centered" Julian date (YYYYDDD)
      INTEGER, INTENT( IN ) :: MTIME         ! "centered" time (HHMMSS)
      INTEGER, INTENT( IN ) :: JDATE         ! current Julian date (YYYYDDD)
      INTEGER, INTENT( IN ) :: JTIME         ! current time (HHMMSS)
      INTEGER, INTENT( IN ) :: DTSTEP( : )   ! time step vector (HHMMSS)

!IVAI: Allocated in hrdriver/hrdata_mod
      REAL,    INTENT( IN    ) :: o3old(:,:,:), o3new(:,:,:), o3tend(:,:,:)
      REAL,    INTENT( INOUT ) :: aux2d_01(:,:), aux2d_02(:,:), aux2d_03(:,:)
!IVAI

!     REAL RJ( NCOLS,NROWS,NLAYS, NPHOTAB )
      REAL,    INTENT( OUT ) :: RJ( :,:,:,: )  ! gridded J-values  (1/min units)

!     REAL CGRID( NCOLS,NROWS,NLAYS, * )  ! Conc array
      REAL, SAVE, POINTER :: CGRID( :,:,:,: ) ! species concentrations

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
      LOGICAL, SAVE :: PHOTDIAG           ! Flag for PHOTDIAG file

      LOGICAL, SAVE :: CALL_INIT_ALBEDO = .TRUE.
      LOGICAL, SAVE :: CALL_GET_ALBEDO  = .TRUE.
      
      LOGICAL       :: ZERO_ICE

      CHARACTER(   3 ), ALLOCATABLE, SAVE :: WLTXT( : )
      CHARACTER(  16 )                    :: VARNM
      CHARACTER(  16 ), SAVE              :: PNAME = 'PHOT'
      CHARACTER(  16 ), SAVE              :: CTM_PHOTDIAG = 'CTM_PHOTDIAG'

      CHARACTER(  80 ) :: VARDESC  ! environment variable description
      CHARACTER( 240 ) :: XMSG = ' '

      INTEGER,    SAVE :: LOGDEV
      INTEGER,    SAVE :: LGC_O3   ! pointer to O3 in CGRID
      INTEGER,    SAVE :: LGC_NO2  ! pointer to NO2 in CGRID
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
      LOGICAL JTIME_CHK            ! To check for JTIME to write RJ values
      INTEGER ODATE                ! output date
      INTEGER OTIME                ! output time 

      INTEGER ALLOCSTAT

      INTEGER, SAVE :: TDATE
      INTEGER, SAVE :: GXOFF, GYOFF        ! global origin offset from file
      INTEGER, SAVE :: PECOL_OFFSET        ! Local Column Offset for processor
      INTEGER, SAVE :: PEROW_OFFSET        ! Local Column Offset for processor
      INTEGER, SAVE :: TSTEP_COUNT         ! counter between calls to write diagnostics

! for INTERPX
      INTEGER, SAVE :: STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
      INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3

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

! Canopy in-line control
      CHARACTER( 20 ), SAVE :: CTM_CANOPY_SHADE = 'CTM_CANOPY_SHADE    ' ! env var for in-line
      LOGICAL,         SAVE :: CANOPY_SHADE   ! flag in-lining canopy shading

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

! Canopy RJ correction arrays
      REAL, ALLOCATABLE, SAVE  :: RJ_CORR     ( :, :)    ! total/integrated canopy shading correction to J-values
      REAL, ALLOCATABLE, SAVE  :: RJ_CORR6    ( :, :)    ! total/integrated canopy shading correction to J-values
      REAL, ALLOCATABLE, SAVE  :: ZCANX    ( : )        ! canopy heights[m]
      REAL, ALLOCATABLE, SAVE  :: RJ_CORRX ( : )        ! canopy height dependent photolysis attenuation factor
      REAL, ALLOCATABLE, SAVE  :: RJ_CORRX6( : )        ! canopy height dependent photolysis attenuation factor
      REAL                     :: XCAN     ( 2 )        ! canopy height interpolation bounds
      REAL                     :: YCAN     ( 2 )        ! photolysisattenuation interpolation bounds
      REAL, ALLOCATABLE, SAVE  :: ZCANX6   ( : )        ! canopy heights[m]

      ! hc
      ! hc to 0.75*hc ?
      ! hc to 0.50*hc ?
      ! hc to 0.35*hc ?
      ! hc to 0.20*hc ?
      ! 0.20*hc to bottom
      REAL :: RJ_CORR_TOP, RJ_CORR_C1R, RJ_CORR_C2R, RJ_CORR_C3R, RJ_CORR_C4R, RJ_CORR_BOT  ! local canopy variables
      REAL :: ZFL, ZCAN, XCANOUT, BOTCAN, LAI_C1R, LAI_C2R, LAI_C3R, LAI_C4R, COEFF ! local canopy variables
      REAL :: TOTAL_O3_COLUMN_CAN
      INTEGER                 :: COUNTCAN, KCAN, KK, II
      INTEGER, PARAMETER      :: MAXCAN = 1000          ! Declare local maximum canopy layers
!IVAI
      INTEGER                 :: KOUNT
      INTEGER                 :: S
      INTEGER, PARAMETER      :: MAXCAN6 = 6            ! Declare local canopy layers
      REAL, ALLOCATABLE, SAVE :: ZFT(:)                 ! height combined canopy plus resolved model layers
      REAL, ALLOCATABLE, SAVE :: rj_corrf(:), rj_corrmid(:)
      REAL, ALLOCATABLE, SAVE :: RJ_CORRM ( :, :, :)    ! 3D canopy shading correction to J-values
!IVAI

!...Variables for diagnostic outputs

      REAL, ALLOCATABLE, SAVE :: N_EXCEED_TROPO3( :,: )  ! Number of adjustments tropospheric ozone optical depth

      REAL, ALLOCATABLE, SAVE :: TOTAL_OC( :,: )         ! total ozone column [DU]
      REAL, ALLOCATABLE, SAVE :: TROPO_OC( :,: )         ! tropospheric ozone column [DU]
      REAL, ALLOCATABLE, SAVE :: TROPO_O3_EXCEED( :,: )   ! Factor used to adjust tropospheric ozone optical depth
      REAL, ALLOCATABLE, SAVE :: TRANSMIS_DIFFUSE( :,: ) ! diffuse transmission coefficient at surface
      REAL, ALLOCATABLE, SAVE :: TRANSMIS_DIRECT( :,: )  ! direct transmission coefficient at surface
      REAL, ALLOCATABLE, SAVE :: REFLECT_COEFF( :,: )    ! reflection coefficient at top of atmosphere
      REAL, ALLOCATABLE, SAVE :: ETOT_SFC_WL ( :,:,: )   ! total downward irradiance at sfc [ Watts / m**2  ]
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
      REAL, ALLOCATABLE, SAVE :: TAU       ( :,:,:,: ) ! optical depth
      REAL, ALLOCATABLE, SAVE :: TAU_AERO  ( :,:,:,: ) ! aerosol optical depth
      REAL, ALLOCATABLE, SAVE :: ACTINIC_FX( :,:,:,: ) ! net actinic flux [watts/m**2]

      INTEGER          IOSX            ! i/o and allocate memory status

      INTERFACE
         SUBROUTINE O3TOTCOL ( LATITUDE, LONGITUDE, JDATE, OZONE )
            INTEGER, INTENT( IN )    :: JDATE      ! Julian day of the year (yyyyddd)
            REAL,    INTENT( IN )    :: LATITUDE   ! latitude of point on earth's surface
            REAL,    INTENT( IN )    :: LONGITUDE  ! longitude of point on earth's surface
            REAL,    INTENT( INOUT ) :: OZONE      ! total column ozone [DU]
         END SUBROUTINE O3TOTCOL
      END INTERFACE

! ----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

C In-line canopy shading option? (default = false)

         CANOPY_SHADE = ENVYN( 'CTM_CANOPY_SHADE',
     &                   'Flag for in-line canopy shading',
     &                   .FALSE., IOSX )

         IF ( CANOPY_SHADE ) THEN
            XMSG = 'Using in-line canopy shading option'
            CALL M3MSG2( XMSG )
         END IF

         FIRSTIME = .FALSE.
         LOGDEV   = INIT3()

         TSTEP  = TIME2SEC( DTSTEP( 1 ) ) ! output timestep for phot diagnostic files

         CGRID => PCGRID( 1:MY_NCOLS,1:MY_NROWS,:,: )

!...Get photolysis rate diagnostic file flag

         PHOTDIAG = .FALSE.         ! default
         VARDESC= 'Flag for writing the photolysis rate diagnostic file'
         PHOTDIAG = ENVYN( CTM_PHOTDIAG, VARDESC, PHOTDIAG, ESTAT )
         IF ( ESTAT .NE. 0 ) WRITE( LOGDEV, '(5X, A)' ) VARDESC
         IF ( ESTAT .EQ. 1 ) THEN
            XMSG = 'Environment variable improperly formatted'
            CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
         ELSE IF ( ESTAT .EQ. -1 ) THEN
            XMSG =
     &          'Environment variable set, but empty ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         ELSE IF ( ESTAT .EQ. -2 ) THEN
            XMSG = 'Environment variable not set ... Using default:'
            WRITE( LOGDEV, '(5X, A, I9)' ) XMSG, JTIME
         END IF

!...Get met file offsets

         CALL SUBHFILE ( GRID_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2 )
         CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )
         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )
 
         PECOL_OFFSET = COLSD_PE( 1, MYPE+1 ) - 1
         PEROW_OFFSET = ROWSD_PE( 1, MYPE+1 ) - 1

         CALL LOAD_CSQY_DATA( )

         CALL LOAD_OPTICS_DATA( )         
 
!...Allocate array needed to calculation aerosol and cloud optical properties

         CALL INIT_AERO_DATA(  )
         
         CALL INIT_CLOUD_OPTICS(  )

!IVAI
         IF ( CANOPY_SHADE ) THEN                      ! FIRSTIME
            CALL init_can_mask( JDATE, JTIME )         ! FIRSTIME

            CALL init_can_levs( CGRID, JDATE, JTIME )  ! FIRSTIME
! ================
! Out:
!      CGRID_MOD        <= CGRID    ! ALL CANOPY & NON-CANOPY COLUMNS !
!      CGRID_CAN(1,2,3) <= CGRID(1) ! ALL CANOPY & NON-CANOPY COLUMNS !
! ===============

            CALL init_can_trans(CGRID, JDATE, JTIME )  ! FIRSTIME
         END IF
!IVAI

!...Allocate and initialize new canopy arrays
        IF ( CANOPY_SHADE ) THEN

           ALLOCATE( RJ_CORRX     ( MAXCAN ) )
           ALLOCATE( ZCANX        ( MAXCAN ) )

           ALLOCATE( RJ_CORRX6    ( MAXCAN6) )
           ALLOCATE( ZCANX6       ( MAXCAN6) )

           ALLOCATE( ZFT      (NLAYT))
           ALLOCATE( rj_corrf (NLAYT))
           ALLOCATE( rj_corrmid(NLAYT))

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
     &           TAU_CLOUD_CAN(NLAYT, NWL),      !inout
     &           STAT = ALLOCSTAT )
           IF ( ALLOCSTAT .NE. 0 ) THEN
              XMSG = 'Failure allocating canopy new_optics arrays'
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

           ZCANX    (:)=0.0
           ZCANX6   (:)=0.0
           ZFT      (:)=0.0
           rj_corrf (:)=0.0
           rj_corrmid(:)=0.0

           TAUC_AERO_CAN(:, :)=0.0  !out
           TAU_TOT_CAN  (:, :)=0.0  !out
           TAU_CLOUD_CAN(:, :)=0.0  !out

           RJ_CORR  (:,:)=0.0
           RJ_CORR6 (:,:)=0.0
           RJ_CORRM (:,:,:)=0.0  !IVAI
        END IF

!...Initialize Surface albedo method

         IF ( .NOT. INITIALIZE_ALBEDO( JDATE, JTIME, LOGDEV ) ) THEN
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

         ALLOCATE( TOTAL_OC ( NCOLS,NROWS ) )

         IF ( PHOTDIAG ) THEN
            ALLOCATE( TROPO_OC ( NCOLS,NROWS ) )
            ALLOCATE( TROPO_O3_EXCEED( NCOLS,NROWS ) )
            ALLOCATE( N_EXCEED_TROPO3( NCOLS,NROWS ) )
            ALLOCATE( TRANSMIS_DIFFUSE( NCOLS,NROWS ) )
            ALLOCATE( TRANSMIS_DIRECT ( NCOLS,NROWS ) )
            ALLOCATE( REFLECT_COEFF   ( NCOLS,NROWS ) )
            ALLOCATE( CLR_TRANSMISSION( NCOLS,NROWS ) )
            ALLOCATE( CLR_TRANS_DIRECT( NCOLS,NROWS ) )
            ALLOCATE( CLR_REFLECTION  ( NCOLS,NROWS ) )
            ALLOCATE( ETOT_SFC_WL     ( NCOLS,NROWS,NWL ) )
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

            DIAG_WVL( 1 )    = 1
            DIAG_WVL( N_DIAG_WVL ) = NWL

            ALLOCATE ( AERO_ASYM( NCOLS,NROWS,NLAYS,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D AERO_ASYM'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( AERO_SSA( NCOLS,NROWS,NLAYS,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D AERO_SSA'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( TAU_AERO( NCOLS,NROWS,NLAYS,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D TAU_AERO'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( TAU( NCOLS,NROWS,NLAYS,N_DIAG_WVL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating 3D TAU'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            ALLOCATE ( ACTINIC_FX( NCOLS,NROWS,NLAYS,NWL ), STAT = ALLOCSTAT )
            IF ( ALLOCSTAT .NE. 0 ) THEN
               XMSG = 'Failure allocating ACTINIC_FX'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

!...write wavelength data to a character array

            ALLOCATE ( WLTXT( NWL ) )

            DO IWL = 1, NWL
               WRITE( WLTXT( IWL ),'(I3.3)' ) INT( WAVELENGTH( IWL ) )
            END DO

!...open the photolysis rate diagnostic files

            ODATE = JDATE; OTIME = JTIME
#ifndef phot_extra_tstep
            CALL NEXTIME ( ODATE, OTIME, DTSTEP( 1 ) )  ! output timestamp ending time
#endif
            IF ( IO_PE_INCLUSIVE ) CALL OPPHOT ( ODATE, OTIME, DTSTEP( 1 ) )

            CALL SUBST_BARRIER

         END IF  ! photdiag

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

#ifdef phot_extra_tstep
      ELSE
         IF ( PHOTDIAG ) THEN
            ODATE = JDATE; OTIME = JTIME
            CALL NEXTIME ( ODATE, OTIME, DTSTEP( 2 ) )  ! sync time step
         END IF
#endif
      END IF  ! FIRSTIME

      IF ( JD_STRAT_O3MIN .NE. JDATE ) THEN
!...set minimum fraction of ozone column above PTOP

         CALL SEASONAL_STRAT_O3( JDATE, JTIME )
         MIN_STRATO3_FRAC = MONTH_STRAT_03_FRAC
         MAX_TROPOO3_FRAC = MAX( 1.0 - MONTH_STRAT_03_FRAC, 0.0 )
         WRITE( LOGDEV,*)'PHOT: MIN_STRATO3_FRAC = ',MIN_STRATO3_FRAC 
         
         JD_STRAT_O3MIN = REAL( JDATE, 4)
      END IF
!...initialize variables tracking whether stratosphere ozone column satisfies
!...climatological averages.

      O3_TOGGLE_AVE     = 0.0
      O3_TOGGLE_MIN     = 1.0
      N_TROPO_O3_TOGGLE = 0
      TSTEP_COUNT       = TSTEP_COUNT + 1

      CALL GET_PHOT_MET( JDATE, JTIME, MDATE, MTIME )

!...Get cosine of solar parameters and set DARK

      CALL UPDATE_SUN( JDATE, JTIME, MDATE, MTIME )
      
      RSQD = DIST_TO_SUN * DIST_TO_SUN

      IF ( MAXVAL( COSINE_ZENITH ) .LE. 0.0 ) THEN
         DARK = .TRUE.
      ELSE
         DARK = .FALSE.
      END IF

!...set surface albedos

      CALL GET_ALBEDO( MDATE, MTIME, LOGDEV, COSINE_ZENITH, LAT, LON )

!...SA  Write COSINE_ZENITH array at the end of each output tstep

      IF ( PHOTDIAG ) THEN
#ifndef phot_extra_tstep
         ODATE = JDATE; OTIME = JTIME
         CALL NEXTIME ( ODATE, OTIME, DTSTEP( 2 ) )  ! sync time step
#endif
         JTIME_CHK = ( MOD( TIME2SEC( OTIME ), TSTEP ) .EQ. 0 ) 
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
         END IF
#endif
      ELSE     
         JTIME_CHK = .FALSE.
      END IF 

!IVAI
      IF (CANOPY_SHADE ) THEN

! Move to rbdriver/hrdriver NB. Need NLAYC/NLAYT dims for block arrays
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

        CALL get_can_levs(CGRID, JDATE, JTIME, MDATE, MTIME)

! ================
! Out: ZMID_CAN  (:,:,NLAYT)
!      ZMOM_CAN  (:,:,NLAYT+1)
!      TA_CAN    (:,:,NLAYT)
!      QV_CAN
!      PRES_CAN
!      DENS_CAN
!      CGRID_MOD        <= CGRID      ! ALL CANOPY & NON-CANOPY COLUMNS !
!      CGRID_CAN(1,2,3) <= CGRID(1)   ! ALL CANOPY & NON-CANOPY COLUMNS !

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

! Jan9: move to rbdriver
        CALL canopy_transfer(CGRID, JDATE, JTIME, 0 )

! ==============
!  Input:
!   CGRID    (:,:, NLAYS, NSPCSD) :  Chemical tracers conc. ppmv on model levels
!   CGRID_MOD(:,:, NLAYS, NSPCSD) :  Chemical tracers conc. ppmv on model levels
!
!  Output:
!   CGRID_CAN(:,:, NLAYT, NSPCSD) :  Chemical tracers conc. ppmv on combined canopy+resolved layers
!                                    ! CANOPY COLUMNS ONLY !
!   CGRID_2M (:,:       , NSPCSD) :  2M Chemical tracers conc. ppmv Diagnostics
!
! ================================

      DO L = 1, NLAYS !from bottom to top model layer
        DO ROW = 1, MY_NROWS
          DO COL = 1, MY_NCOLS

          KOUNT = 0

! In (CANOPY_SHADE)
          IF ( FRT_MASK(COL,ROW) > 0.) THEN  ! CANOPY_SHADE & FRT_MASK

            IF (.FALSE.) THEN
            IF ( KOUNT < 3 )  THEN

              print*, 'PHOT: AFTER R2C CAN L=1 ', 
     &           frctr2c(NLAYT,1,COL,ROW), 
     &           frctr2c(NLAYT,2,COL,ROW),! 1cy: order is 1=top NLAYT=1cy
     &       float(nfrct(NLAYT,  COL,ROW  ))

              print*, 'PHOT: AFTER R2C MOD: ', L,
     &           TA( COL,ROW,L ),  PRES( COL,ROW, L ), DENS( COL,ROW, L ),
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
     &        CGRID    ( COL,ROW, L,         S ), 
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

      
!...If sun below horizon at all cells, zero photolysis rates & exit
!...  (assumes sun below horizon at *all* levels!)

      IF ( DARK ) THEN

         RJ = 0.0
         RJ_CAN=0.0  !IVAI RJ_CAN   (:,:,:,:)

!...write to the log file, CTM_RJ_1 file and return

         WRITE( LOGDEV, 1003 ) MYPE, JDATE, JTIME

!...Initialize ETOT_SFC, TAU_AERO, TAU_TOT, TAUO3_TOP to 0.0

!...Write data to output diagnostic file

         TOTAL_OC = 0.0

         IF ( JTIME_CHK ) THEN

            TROPO_OC     = 0.0
            ETOT_SFC_WL  = 0.0
            TAUO3_TOP_WL = 0.0
            TAU_AERO_WL  = 0.0
            TAU_CLOUD_WL = 0.0
#ifdef phot_debug                   
            SSA_CLOUD_WL = 0.0
            ASY_CLOUD_WL = 0.0
#endif                   
            TAU_TOT_WL   = 0.0
            TAU          = 0.0
            TAU_AERO     = 0.0
            AERO_SSA     = 0.0
            AERO_ASYM    = 0.0
            ACTINIC_FX   = 0.0

!           TROPO_O3_EXCEED   = 0.0
            TRANSMIS_DIFFUSE  = 0.0
            TRANSMIS_DIRECT   = 0.0
            REFLECT_COEFF     = 0.0
            CLR_TRANSMISSION  = 0.0
            CLR_TRANS_DIRECT  = 0.0
            CLR_REFLECTION    = 0.0 

         END IF  ! if JTIME_CHK

      ELSE  ! all cells not dark

!...MAIN loop over all rows and columns
         LOOP_ROWS: DO ROW = 1, MY_NROWS
            LOOP_COLS: DO COL = 1, MY_NCOLS
         
               PHOT_COL = COL + PECOL_OFFSET
               PHOT_ROW = ROW + PEROW_OFFSET

               COSZEN = COSINE_ZENITH( COL,ROW ) ! local cosine of solar zenith angle

               IF ( COSZEN .LE. 0.0 ) THEN
!...the cell is dark: set variables to zero and cycle
                  RJ    ( COL,ROW, :,: ) = 0.0
                  RJ_CAN( COL,ROW, :,: ) = 0.0  !IVAI
   
                  IF ( JTIME_CHK ) THEN
                     TOTAL_OC( COL,ROW )        = 0.0                            
                     TROPO_OC( COL,ROW )        = 0.0                            
                     ETOT_SFC_WL ( COL,ROW, : ) = 0.0
                     TAUO3_TOP_WL( COL,ROW, : ) = 0.0
                     TAU_AERO_WL ( COL,ROW, : ) = 0.0
                     TAU_CLOUD_WL( COL,ROW, : ) = 0.0
#ifdef phot_debug                   
                     SSA_CLOUD_WL( COL,ROW, : ) = 0.0
                     ASY_CLOUD_WL( COL,ROW, : ) = 0.0
#endif                   
                     TAU_TOT_WL( COL,ROW, : )   = 0.0
                     TAU       ( COL,ROW, :,: ) = 0.0
                     TAU_AERO  ( COL,ROW, :,: ) = 0.0
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
                  END IF

                  CYCLE LOOP_COLS

               END IF

!...initialize BLKRJ using F90 array operations.

               BLKRJ_RES = 0.0
               BLKRJ_ACM = 0.0

               BLKRJ_RES_CAN = 0.0  !IVAI
               BLKRJ_ACM_CAN = 0.0  !IVAI

!...Set height of lowest level to zero

               BLKZF( 1 ) = 0.0
               BLKZF_CAN( 1 ) = 0.0      !IVAI

               ZSFC   = HT( COL,ROW ) ! surface height [m]
               SINZEN = SQRT( 1.0 - COSZEN * COSZEN ) ! sine of zenith angle

!...local latitude and longitude

!               LATCR = LAT( COL,ROW )
!               LONCR = LON( COL,ROW )

!...get total ozone column based on OMI observations
               CALL O3TOTCOL ( LAT( COL,ROW ), LON( COL,ROW ), JDATE, TOTAL_O3_COLUMN )

!IVAI:
               TOTAL_O3_COLUMN_CAN = TOTAL_O3_COLUMN
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

! IVAI:
! Uncomment to use t2m temperature
!                 BLKTA  ( L )   = Met_Data%TEMP2(COL,ROW)   ! 2-m  temperature [K]
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
! Uncomment to use values from canopy_transfer "resolved_to_canopy" BAD !! ???
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
                  CALL GET_AERO_DATA ( COL,ROW, NLAYS, CGRID )
!              ELSE
!                 CALL AERO_OPTICS_INTERNAL( COL,ROW, NLAYS, CGRID )
!              END IF

! set surface albedo

               FORALL ( IWL = 1:NWL )
                  ALB( IWL ) = SURFACE_ALBEDO( IWL, COL,ROW )
               END FORALL
!set min/max fractions of ozone column in stratosphere and troposphere
!              MIN_STRATO3_FRAC = MIN_STRAT_03_FRAC( COL, ROW )
!              MAX_TROPOO3_FRAC = MAX( 1.0 - MIN_STRAT_03_FRAC( COL, ROW ), 0.0 )
!                MIN_STRATO3_FRAC = MONTH_STRAT_03_FRAC( COL, ROW )
!                MAX_TROPOO3_FRAC = MAX( 1.0 - MONTH_STRAT_03_FRAC( COL, ROW ), 0.0 )
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
               IF ( PHOTDIAG .AND. .NOT. STRATO3_MINS_MET ) THEN
                    N_EXCEED_TROPO3( COL,ROW ) =  N_EXCEED_TROPO3( COL,ROW ) + 1.0
                    TROPO_O3_EXCEED( COL,ROW ) = TROPO_O3_COLUMN/(MAX_TROPOO3_FRAC*TOTAL_O3_COLUMN) - 1.0
!     &                                         + 1.0 / TROPO_O3_TOGGLE - 1.0
     &                                         + TROPO_O3_EXCEED( COL,ROW )
!               ELSE IF(  PHOTDIAG ) THEN
!                     TROPO_O3_EXCEED( COL,ROW ) = 0.0
               END IF 

               IF ( JTIME_CHK  ) THEN
                  TOTAL_OC( COL,ROW )         = REAL( TOTAL_O3_COLUMN )
                  TROPO_OC( COL,ROW )         = REAL( TROPO_O3_COLUMN )
                  TRANSMIS_DIFFUSE( COL,ROW ) = TRANSMISSION
                  TRANSMIS_DIRECT( COL,ROW )  = TRANS_DIRECT
                  REFLECT_COEFF( COL,ROW )    = REFLECTION


                  FORALL( IWL = 1:NWL )
                     ETOT_SFC_WL ( COL,ROW,IWL ) = IRRADIANCE( 1,IWL )
                     TAUO3_TOP_WL( COL,ROW,IWL ) = TAUO3_TOP( IWL )
                     TAU_AERO_WL ( COL,ROW,IWL ) = TAUC_AERO( 1,IWL )
                     TAU_TOT_WL  ( COL,ROW,IWL ) = TAU_TOT  ( 1,IWL )
                     TAU_CLOUD_WL( COL,ROW,IWL ) = TAU_CLOUD( 1,IWL )
#ifdef phot_debug                      
                     SSA_CLOUD_WL( COL,ROW,IWL ) = AVE_SSA_CLD  ( IWL )
                     ASY_CLOUD_WL( COL,ROW,IWL ) = AVE_ASYMM_CLD( IWL )
#endif                      
                  END FORALL
                  FORALL ( LEV = 1:NLAYS, IWL = 1:NWL  )
                     ACTINIC_FX( COL,ROW,LEV,IWL ) = ACTINIC_FLUX( LEV,IWL )
                  END FORALL
                  
                  DO L = 1, N_DIAG_WVL
                     IWL = DIAG_WVL( L )
                     FORALL ( LEV = 1:NLAYS )
                        TAU     ( COL,ROW,LEV,L ) = TAU_TOT  ( LEV,IWL )
                        TAU_AERO( COL,ROW,LEV,L ) = TAUC_AERO( LEV,IWL )
                     END FORALL
                     FORALL ( LEV = 1:NLAYS, AERO_EXTI_COEF( LEV,IWL ) .GT. EPSLON )
                        AERO_SSA ( COL,ROW,LEV,L ) = AERO_SCAT_COEF( LEV,IWL )
     &                                             / AERO_EXTI_COEF( LEV,IWL )
                        AERO_ASYM( COL,ROW,LEV,L ) = AERO_ASYM_FAC( LEV,IWL )
                     END FORALL
                     FORALL ( LEV = 1:NLAYS, AERO_EXTI_COEF( LEV,IWL ) .LE. EPSLON )
                        AERO_SSA ( COL,ROW,LEV,L ) = 1.0
                        AERO_ASYM( COL,ROW,LEV,L ) = 0.0
                     END FORALL
                  END DO
               END IF

!Set Photolysis rates to resolved sky values
               FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
                  RJ( COL,ROW, L,IPHOT ) = 60.0 * BLKRJ_RES( L,IPHOT ) 
               END FORALL  ! Loop on layers and NPHOTAB

!IVAI
! Temporary output
!            aux2d_03( COL,ROW ) = RJ (COL,ROW, 1, LO3O1D)

!Get Photolysis rates to combined (canopy plus resolved model) layers
            KOUNT = 0
            IF ( CANOPY_SHADE .AND. FRT_MASK(COL,ROW) > 0. ) THEN

!Set Photolysis rates to pre-determined above and below canopy values
               FORALL ( L = 1:NLAYT, IPHOT = 1:NPHOTAB )
                  RJ_CAN( COL,ROW, L,IPHOT ) = 60.0 * BLKRJ_RES_CAN( L,IPHOT )
               END FORALL

! Print all layers only at KOUNT number of canopy columns 
!           IF (KOUNT < 3 ) THEN

! NB. RJ E-2 smaller than RJ_CAN :  e.g. 8.3082254E-05  1.8573621E-03 ???
!              DO L = 1, 3     ! NLAYS (from bottom to model top)

! Inputs to new_optics:
!                 print*, 'PHOT RJ INPUTS MOD: L= ', L, 
!    &     RJ( COL,ROW, L, LO3O1D), 
!    &        BLKRJ_RES(L, LO3O1D),
!    &            BLKTA(L), 
!    &           BLKPRS(L), BLKDENS(L),   ! atm & molec cm-3
!    &            BLKZH(L),   BLKZF(L+1),
!    &            BLKO3(L),  BLKNO2(L) 

!                 print*, 'PHOT RJ INPUTS CAN: L= ', NLAYC + L, 
!    &         RJ_CAN( COL,ROW, NLAYC + L, LO3O1D), 
!    &         BLKRJ_RES_CAN(NLAYC + L, LO3O1D), 
!    &             BLKTA_CAN(NLAYC + L), 
!    &            BLKPRS_CAN(NLAYC + L), BLKDENS_CAN(NLAYC + L),
!    &             BLKZH_CAN(NLAYC + L),   BLKZF_CAN(NLAYC + L+1),
!    &             BLKO3_CAN(NLAYC + L),  BLKNO2_CAN(NLAYC + L) 

!              END DO

!              DO L = 1, NLAYC ! from bottom to top of canopy
!                 print*, 'PHOT RJ INPUTS CAN: L= ', L, 
!    &      RJ_CAN( COL,ROW, L, LO3O1D), 
!    &         BLKRJ_RES_CAN(L, LO3O1D),
!    &             BLKTA_CAN(L),
!    &            BLKPRS_CAN(L), BLKDENS_CAN(L),
!    &             BLKZH_CAN(L),   BLKZF_CAN(L+1),
!    &             BLKO3_CAN(L),  BLKNO2_CAN(L)
!              END DO

!           END IF ! KOUNT

! Print up to KOUNT canopy columns
!           KOUNT = KOUNT + 1 ! CANOPY_SHADE .AND. FRT_MASK
! End Print


! Temporary output
!           aux2d_02( COL,ROW ) = RJ_CAN( COL,ROW, 2, LO3O1D)
!           aux2d_01( COL,ROW ) = RJ_CAN( COL,ROW, 1, LO3O1D)

! !!! START OVERWRITE CANOPY COLUMNS!!!
! NB. If applying overwrite below, the canopy layers will only differ in RJ_CORRM factor
! !!! Overwrite canopy layers with 1hy modle layer !!!
!Set Photolysis rates to resolved sky values above canopy
!Nov2:         FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
!Nov2:            RJ_CAN( COL,ROW, NLAYC+L,IPHOT ) = 60.0 * BLKRJ_RES( L,IPHOT )
!Nov2:         END FORALL
!Set Photolysis rates below canopy to 1hy (bottom) model layer
!Nov2:         FORALL ( L = 1:NLAYC, IPHOT = 1:NPHOTAB )
!Nov2:            RJ_CAN( COL,ROW, L,IPHOT ) = 60.0 * BLKRJ_RES( 1,IPHOT )
!Nov2:         END FORALL
! !!! END OVERWRITE CANOPY COLUMNS !!!

            ELSE  ! Non-Canopy columns
!
! Non-canopy columns

! Above canopy layersr:
!Set Photolysis rates to resolved sky values above canopy
               FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
                  RJ_CAN( COL,ROW, NLAYC+L,IPHOT ) = 60.0 * BLKRJ_RES( L,IPHOT )
               END FORALL

! Sub-canopy layers:
!Set Photolysis rates below canopy to 1hy (bottom) model layer
               FORALL ( L = 1:NLAYC, IPHOT = 1:NPHOTAB )
                  RJ_CAN( COL,ROW, L,IPHOT ) = 60.0 * BLKRJ_RES( 1,IPHOT )
               END FORALL

            END IF  ! CANOPY_SHADE & FRT_MASK
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
!                    write(logdev,*)'ACM cloud present fraction, cloud lwc(lev),iwc(lev),rwc(1),gwc(1) = ',
!     &              ACM_CLOUDS( COL,ROW ),lwc(lev),iwc(lev),rwc(1),gwc(1)

!IVAI
!           IF ( CANOPY_SHADE .AND. FRT_MASK(COL,ROW) > 0.) THEN
! Canopy & Non-Canopy columns 
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
!           END IF
!IVAI
                 
! get optical properties of of subgrid cloud hydrometeors
                     CALL GET_DROPLET_OPTICS( LEV, BLKTA, OWATER_FRAC, SEAICE_FRAC, SNOW_FRAC, LWC )     
                     CALL GET_ICE_OPTICS( LEV, BLKTA, IWC )     
                     CALL GET_AGGREGATE_OPTICS( LEV, RWC, SWC, GWC )     

!...calculate the acm-cloud photolysis rates for all layers:
                     NEW_PROFILE = .FALSE.   
                     CALL NEW_OPTICS ( JDATE, JTIME, NLAYS,
     &                                 BLKTA, BLKPRS, BLKDENS, BLKZH, BLKZF,
     &                                 BLKO3, BLKNO2,
     &                                 ZSFC, COSZEN, SINZEN, RSQD,
     &                                 NEW_PROFILE, CLOUDS, CLDFRAC,
     &                                 BLKRJ_ACM, TAUC_AERO, TAU_TOT, TAUO3_TOP,
     &                                 TAU_RAY, SSA, TAU_CLOUD, TOTAL_O3_COLUMN )

!IVAI: 
!...Call new_optics for the cloudy fraction of the grid cell
!...calculate the acm-cloud photolysis rates for combined (canopy plus
!resolved) model layers NLAYT
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
!IVAI

!...load diagnostic file arrays
!...compute a cloud-fraction weighted average of ETOT_SFC and TAU_TOT
!...  note that both TAUC_AERO and TAUO3_TOP are the same for clear and
!...  cloudy regions
                     MSCALE = MAX( 1.0 - ACM_CLOUDS( COL,ROW ), 0.0 )

                     IF ( JTIME_CHK ) THEN

                        TRANSMIS_DIRECT( COL,ROW )  = MSCALE * TRANSMIS_DIRECT( COL,ROW )
     &                                              + ACM_CLOUDS( COL,ROW ) * TRANS_DIRECT
                        TRANSMIS_DIFFUSE( COL,ROW ) = MSCALE * TRANSMIS_DIFFUSE( COL,ROW )
     &                                              + ACM_CLOUDS( COL,ROW ) * TRANSMISSION
                        REFLECT_COEFF( COL,ROW )    = MSCALE * REFLECT_COEFF( COL,ROW )
     &                                              + ACM_CLOUDS( COL,ROW ) * REFLECTION
                        FORALL ( IWL = 1:NWL )
                           ETOT_SFC_WL ( COL,ROW,IWL ) = MSCALE * ETOT_SFC_WL( COL,ROW,IWL )
     &                                                 + ACM_CLOUDS( COL,ROW ) * IRRADIANCE( 1,IWL )
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
                        END FORALL   ! iwl
                        FORALL ( LEV = 1:NLAYS, IWL = 1:NWL )
                           ACTINIC_FX( COL,ROW,LEV,IWL ) = MSCALE * ACTINIC_FX( COL,ROW,LEV,IWL )
     &                                                   + ACM_CLOUDS( COL,ROW ) * ACTINIC_FLUX( LEV,IWL )
                        END FORALL ! lev and iwl

                        DO L = 1, N_DIAG_WVL
                           IWL = DIAG_WVL( L )
                           FORALL ( LEV = 1:NLAYS)
                              TAU( COL,ROW,LEV,L ) = MSCALE * TAU( COL,ROW,LEV,L )
     &                                             + ACM_CLOUDS( COL,ROW ) * TAU_TOT( LEV,IWL )
                           END FORALL
                        END DO
                     END IF  ! photdiag
!Photolysis rates become a weighted average of the values from resolved and ACM skies
                     FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
                         RJ( COL,ROW, L, IPHOT ) = 60.0 * ACM_CLOUDS( COL,ROW ) * BLKRJ_ACM( L,IPHOT )
     &                                           + MSCALE * RJ( COL,ROW,L,IPHOT )
                     END FORALL  ! Loop on layers and PHOT

!IVAI
! Temporary output: ACM skies
!                    aux2d_03(COL,ROW) = RJ( COL,ROW, 1, LO3O1D)

! Canopy & Non-Canopy Columns:
!Photolysis rates become a weighted average of the values from resolved and ACM skies
            IF (CANOPY_SHADE .AND. FRT_MASK(COL,ROW) > 0. ) THEN
! Canopy columns
               FORALL ( L = 1:NLAYT, IPHOT = 1:NPHOTAB )
                 RJ_CAN( COL,ROW, L, IPHOT ) = 60.0 * ACM_CLOUDS( COL,ROW ) * BLKRJ_ACM_CAN( L,IPHOT )
     &                                        + MSCALE * RJ_CAN( COL,ROW,L,IPHOT )
               END FORALL  ! Loop on layers and PHOT

            ELSE ! Non-Canopy columns   

! Non-Canopy Columns
! Above canopy layers
               FORALL ( L = 1:NLAYS, IPHOT = 1:NPHOTAB )
                 RJ_CAN( COL,ROW, NLAYC + L, IPHOT ) = 60.0 * ACM_CLOUDS( COL,ROW ) * BLKRJ_ACM( L,IPHOT )
     &                         + MSCALE * RJ_CAN( COL,ROW, NLAYC + L,IPHOT )
               END FORALL  ! Loop on layers and PHOT

! Sub-canopy layers
               FORALL ( L = 1:NLAYC, IPHOT = 1:NPHOTAB )
                 RJ_CAN( COL,ROW, L, IPHOT ) = 60.0 * ACM_CLOUDS( COL,ROW ) * BLKRJ_ACM( 1,IPHOT )
     &                                        + MSCALE * RJ_CAN( COL,ROW,L,IPHOT )
               END FORALL  ! Loop on layers and PHOT

            END IF

                  END IF ! ACM_CLOUDS > 0
               END IF ! USE_ACM_CLOUD

! Print after ACM_CLOUDS correction
!        KOUNT = 0
!        IF ( CANOPY_SHADE .AND. FRT_MASK(COL,ROW) > 0. ) THEN

! Print all layers only at KOUNT number of canopy columns
!           IF (KOUNT < 3 ) THEN

!              DO L = 1, 3 ! NLAYS (from bottom to model top)
!                 print*, 'PHOT RJ_ACM MOD: L= ', L,
!    &              RJ( COL,ROW, L, LO3O1D), BLKRJ_ACM( L,LO3O1D)

!                 print*, 'PHOT RJ_ACM CAN: L= ', NLAYC + L,
!    &              RJ_CAN( COL,ROW, NLAYC + L, LO3O1D), BLKRJ_ACM_CAN( NLAYC + L,LO3O1D)

!              END DO ! NLAYS

!              DO L = 1, NLAYC ! from bottom to top of canopy
!                 print*, 'PHOT RJ_ACM CAN: L= ', L,
!    &              RJ_CAN( COL,ROW, L, LO3O1D), BLKRJ_ACM_CAN( L,LO3O1D)
!              END DO

!           END IF ! KOUNT

! Print up to KOUNT canopy columns
!           KOUNT = KOUNT + 1

!        END IF ! CANOPY_SHADE & FRT_MASK
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

              RJ_CORR_TOP = 1. ! top of canopy
              RJ_CORR_C1R = MAX(1.0E-10, EXP(- LAI_C1R * COEFF )) 
              RJ_CORR_C2R = MAX(1.0E-10, EXP(- LAI_C2R * COEFF ))
              RJ_CORR_C3R = MAX(1.0E-10, EXP(- LAI_C3R * COEFF ))
              RJ_CORR_C4R = MAX(1.0E-10, EXP(- LAI_C4R * COEFF ))
              RJ_CORR_BOT = MAX(1.0E-10, EXP(- COEFF ))

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
                           YCAN(1) = RJ_CORR_C1R
                           XCANOUT = ZCAN
                           ZCANX(COUNTCAN) = ZCAN
                           RJ_CORRX (COUNTCAN) = interp_linear1_internal(XCAN,YCAN,XCANOUT)
                        ELSE IF ( ZCAN .LE. Met_Data%FCH( COL,ROW )*0.75   .AND.
     &                                       ZCAN .GT. Met_Data%FCH( COL,ROW )*0.50 ) THEN
                           COUNTCAN = COUNTCAN + 1
                           XCAN(2) = Met_Data%FCH( COL,ROW )*0.75
                           YCAN(2) = RJ_CORR_C1R
                           XCAN(1) = Met_Data%FCH( COL,ROW )*0.50
                           YCAN(1) = RJ_CORR_C2R
                           XCANOUT = ZCAN
                           ZCANX(COUNTCAN) = ZCAN
                           RJ_CORRX (COUNTCAN) = interp_linear1_internal(XCAN,YCAN,XCANOUT)
                        ELSE IF ( ZCAN .LE. Met_Data%FCH( COL,ROW )*0.50   .AND.
     &                            ZCAN .GT. Met_Data%FCH( COL,ROW )*0.35 ) THEN
                           COUNTCAN = COUNTCAN + 1
                           XCAN(2) = Met_Data%FCH( COL,ROW )*0.50
                           YCAN(2) = RJ_CORR_C2R
                           XCAN(1) = Met_Data%FCH( COL,ROW )*0.35
                           YCAN(1) = RJ_CORR_C3R
                           XCANOUT = ZCAN
                           ZCANX(COUNTCAN) = ZCAN
                           RJ_CORRX (COUNTCAN) = interp_linear1_internal(XCAN,YCAN,XCANOUT)
                        ELSE IF ( ZCAN .LE. Met_Data%FCH( COL,ROW )*0.35  .AND.
     &                            ZCAN .GT. Met_Data%FCH( COL,ROW )*0.20 ) THEN
                           COUNTCAN = COUNTCAN + 1
                           XCAN(2) = Met_Data%FCH( COL,ROW )*0.35
                           YCAN(2) = RJ_CORR_C3R
                           XCAN(1) = Met_Data%FCH( COL,ROW )*0.20
                           YCAN(1) = RJ_CORR_C4R
                           XCANOUT = ZCAN
                           ZCANX(COUNTCAN) = ZCAN
                           RJ_CORRX (COUNTCAN) = interp_linear1_internal(XCAN,YCAN,XCANOUT)
                        ELSE IF ( ZCAN .LE. Met_Data%FCH( COL,ROW )*0.20 ) THEN
                           COUNTCAN = COUNTCAN + 1
                           XCAN(2) = Met_Data%FCH( COL,ROW )*0.20
                           YCAN(2) = RJ_CORR_C4R
                           XCAN(1) = 0.5
                           YCAN(1) = RJ_CORR_BOT
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
!Testing:      rj_corrmid(L) = rj_corrf(L)
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
!IVAI
         END IF  !CANOPY_SHADE/FRT_MASK

               IF ( JTIME_CHK ) THEN ! compute clear sky reflection and transmission coefficients
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

      END IF !all cells not dark

!IVAI
5010  FORMAT( 'PHOT: NI_CAN1 =', I9, ' NI_NOCAN1 =', I9, ' MY_NCOLS =', I9)
5011  FORMAT( 'PHOT: NI_CANT =', I9, ' NI_NOCANT =', I9, ' MY_NROWS =', I9)
!IVAI

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

         VARNM = 'COSZENS'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME,
!IVAI: 
! hrdriver & phot:
!    &      o3old ( :,:,1 )          ) ) THEN ! 
     &      AUX2D_03                 ) ) THEN ! "rj_can" hrdriver/phot
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
!    &      COSINE_ZENITH            ) ) THEN
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

         VARNM = 'TROPO_O3_COLUMN'
         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, TROPO_OC ) ) THEN
            XMSG = 'Error writing variable ' // VARNM
            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF

         IMONTH = IMONTH + 1
         IF( IMONTH .GT. 12 )THEN
             IMONTH = 1
             TDATE = 2011001
         END IF
         TDATE = TDATE + DAYS( IMONTH )
!         CALL SEASONAL_STRAT_O3(TDATE, JTIME )


!         VARNM = 'MIN_FRAC_STRATO3'
!         IF ( .NOT. WRITE3( CTM_RJ_1, VARNM, ODATE, OTIME, MONTH_STRAT_03_FRAC ) ) THEN
!            XMSG = 'Error writing variable ' // VARNM
!            CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!         END IF


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
     &      AUX2D_02                 ) ) THEN ! "o3_new_can" hrdriver
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
!    &      RJ       ( :,:,1, LNO2  )) ) THEN
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
!    &      RJ       ( :,:,1 ,LO3O1D) ) ) THEN   ! bottom model  layer
     &      RJ_CAN   ( :,:,1 ,LO3O1D) ) ) THEN  
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

            VARNM = 'TAU_AERO_W' // WLTXT( IWL )
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

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &          'RJ Values written to', CTM_RJ_1,
     &          'for date and time', ODATE, OTIME

         DO IPHOT = 1, NPHOTAB
            IF ( .NOT. WRITE3( CTM_RJ_2, PHOTAB( IPHOT ), ODATE,
     &                         OTIME, RJ( :,:,:,IPHOT ) ) ) THEN
               XMSG = 'Could not write ' // CTM_RJ_2 // ' file'
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END DO

         VARNM = 'CFRAC_3D'
         IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, CFRAC_3D ) ) THEN
              XMSG = 'Could not write ' // TRIM( VARNM ) // ' to ' // CTM_RJ_2 // ' file'
              CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         END IF
         
         DO IWL = 1, NWL
            VARNM = 'ACTINIC_FX_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, ACTINIC_FX( :,:,:,IWL ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END DO

         DO L = 1, N_DIAG_WVL
            IWL = DIAG_WVL( L )

            VARNM = 'AERO_SSA_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, AERO_SSA( :,:,:,L ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'AERO_ASYM_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, AERO_ASYM( :,:,:,L ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TAU_AERO_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, TAU_AERO( :,:,:,L ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF

            VARNM = 'TAU_W' // WLTXT( IWL )
            IF ( .NOT. WRITE3( CTM_RJ_2, VARNM, ODATE, OTIME, TAU( :,:,:,L ) ) ) THEN
               XMSG = 'Error writing variable ' // VARNM
               CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF
         END DO

         WRITE( LOGDEV, '( /5X, 3( A, :, 1X ), I8, ":", I6.6 )' )
     &          'RJ and Optical Data written to', CTM_RJ_2,
     &          'for date and time', ODATE, OTIME

      END IF   ! if JTIME_CHK 

1003  FORMAT( 8X, 'Processor ',I4.4,' is in darkness at ', I8.7, ':', I6.6,
     &        1X, 'GMT - no photolysis')
9500  FORMAT(3(/ A), I7, 1X, I6.6, 1X, / "Total Number: ", I9, ";Mean Value: ", F9.6,
     &       "; Max Value: ",F9.6 /)

      RETURN
      END SUBROUTINE PHOT
