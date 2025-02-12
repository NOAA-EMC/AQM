
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

C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: /project/yoj/arc/CCTM/src/gas/ros3/rbdriver.F,v 1.7 2011/10/21 16:11:10 yoj Exp $

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%


       SUBROUTINE CHEM( CONC, JDATE, JTIME, TSTEP )

C**********************************************************************
C
C  Function: To control gas phase chemistry calculations performed by
C            the vectorized Rosenbrock solver
C
C  Preconditions: None
C
C  Key Subroutines/Functions Called: RBINIT
C                                    RBSPARSE
C                                    PHOT
C                                    CALCKS
C                                    RBSOLVER
C                                    FIND_DEGRADED
C                                    INIT_DEGRADE
C                                    FINAL_DEGRADE
C
C  Revision History: Prototype created by Jerry Gipson, August, 2004
C                    Based on the solver described by Sandu et al
C                    ( Atm. Env., Vol. 31, No. 20, 1997 ) and included
C                    in the Kinetic PreProcessor ( see for example 
C                    Sandu et al., At, Env., Vol. 37, 5097-5114, 
C                    2003). This code also incorporates efficiency
C                    concepts originally developed by M. Jacobson
C                    for SMVGEAR (Atm. Env., Vol 28, No 2, 1994).
C                    Adapted from Subroutine CHEM in CMAQ SMVGEAR
C
C                    31 Jan 05 J.Young: dyn alloc - establish both horizontal
C                    & vertical domain specifications in one module (GRID_CONF)
C                    29 Jul 05     WTH: Added IF blocks that call degrade 
C                                       routines if MECHNAME contains 'TX' 
C                                       substring.
C                    28 Jun 10 J.Young: convert for Namelist redesign
C                    29 Mar 11 S.Roselle: Replaced I/O API include files
C                               with UTILIO_DEFN
C                    31 Aug 11 B.Hutzell revised method that determines calling
C                              degrade routine
C                    29 Sep 11 D.Wong: incorporated twoway model implementation
C                    18 Jan 13 B.Hutzell: 1) added using heteorogeneous rate constants
C                    by using function in AEROSOL_CHEMISTRY module,
C                    15 Jul 14 B.Hutzell: 1) replaced mechanism include files with 
C                    RXNS_DATA module, 2) replaced call to CALCLK with CALC_RCONST in 
C                    RXNS_FUNCTION module, 3) enabled reactions between all species 
C                    types by using unit conversion factors and 4) revised usage for 
C                    INIT_DEGRADE and FINAL_DEGRADE routines
C                    02 Dec 14 B.Hutzell 1) added terrestrial data to conduct surface
C                    dependent reactions and 2) modified the call CALC_RCONST routine
C                    16 Sep 16 J.Young: update for inline procan (IRR)
C**********************************************************************

      USE RXNS_DATA
      USE RXNS_FUNCTION
      USE GRID_CONF             ! horizontal & vertical domain specifications
      USE RBDATA                ! ROS3 solver data
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN
      USE AEROSOL_CHEMISTRY
      USE DEGRADE_SETUP_TOX, ONLY : N_REACT, RXTANT_MAP
      USE PA_DEFN, Only: LIRR   ! Process Anaylsis control and data variable

!IVAI
      USE UTILIO_DEFN

      USE PHOT_MOD, ONLY : LO3O1D

      use can_mask

! =====================
!     call init_can_mask called in phot 
!     call  get_can_mask called in phot 
! Out:
!      FRT_MASK         ( get_can_mask)
! =====================

      use can_levs_defn

! =====================
! In:  CONC
!
! Out: ZMID_CAN  (:,:,NLAYT)    (get_can_levs)
!      ZMOM_CAN  (:,:,NLAYT+1)
!      TA_CAN    (:,:,NLAYT)
!      QV_CAN
!      PRES_CAN
!      DENS_CAN
!      CONC_MOD        <= CONC       ! ALL CANOPY & NON-CANOPY COLUMNS !
!      CONC_CAN(1,2,3) <= CONC(1)   ! ALL CANOPY & NON-CANOPY COLUMNS !
!      RJ_CAN
! =====================

      use can_trans_mod

! =====================
!     call canopy_transfer
!  In:
!   CONC    (:,:, NLAYS, NSPCSD) :  Chemical tracers conc. ppmv on model levels
!   CONC_MOD(:,:, NLAYS, NSPCSD) :  Chemical tracers conc. ppmv on model levels
!
!  Output: (canopy_transfer)
!   CONC_CAN(:,:, NLAYT, NSPCSD) :  Chemical tracers conc. ppmv on combined canopy+resolved layers
!                                    ! CANOPY COLUMNS ONLY !
! ================================
!IVAI


      IMPLICIT NONE 

C..Includes:

      INCLUDE SUBST_FILES_ID    ! CMAQ files
      INCLUDE SUBST_CONST       ! CMAQ constants
 
C..Arguments:

      REAL, POINTER :: CONC( :,:,:,: )  ! Concentrations

      INTEGER JDATE                     ! Current date (YYYYDDD)
      INTEGER JTIME                     ! Current time (HHMMSS)
      INTEGER TSTEP( 3 )                ! Time step vector (HHMMSS)
! IVAI:
! TSTEP(1) = local output step
! TSTEP(2) = sciproc sync. step (chem)
! TSTEP(3) = twoway model time step w.r.t. wrf time
! IVAI

C..Parameters:

      INTEGER, PARAMETER :: IZERO = 0                ! Integer zero

      REAL,    PARAMETER :: CONCMIN   = 1.0E-30         ! Minimum conc
      REAL,    PARAMETER :: CONCOFM   = 1.0E+06         ! conc. of M = 1E+06 ppm
      REAL,    PARAMETER :: PA2ATM    = 1.0 / STDATMPA  ! Pascal to atm conv fac
      REAL,    PARAMETER :: MAOMV     = MWAIR / MWWAT   ! Mol Wt of air over Mol Wt of water
      REAL,    PARAMETER :: QV_TO_PPM = CONCOFM * MAOMV ! factor to convert water wapor into ppm

C..External Functions:

C..Local Variables:

      LOGICAL, SAVE :: LFIRST    = .TRUE. ! Flag for first call to this subroutine
      LOGICAL, SAVE :: FIRSTCALL = .TRUE. ! Another Flag for first call
      LOGICAL, SAVE :: LIRRBLK         ! Flag for IRR to be done for block

      INTEGER, SAVE :: LOGDEV          ! Unit number of output log
      INTEGER, SAVE :: NOXYZ           ! Total number of grid cells

! IVAI
      INTEGER N                        ! Reaction rates index
      INTEGER, SAVE :: NOXYZ_CAN       ! Total number of grid cells in canopy layers
! IVAI

      REAL,    SAVE :: AIRFC           ! Factor to convert gms air to ppm
!     REAL,    SAVE :: CONV( NLAYS )   ! Volumetric emis conv facs

      REAL( 8 )     :: CHEMSTEP   ! Chem integration interval (min)
      REAL( 8 )     :: VALLOW     ! Value holder for sort routine

      CHARACTER(  16 ) :: PNAME = 'RBDRIVER' ! Procedure name
      CHARACTER(  16 ) :: VNAME              ! Name of I/O API data variable
      CHARACTER( 144 ) :: MSG                ! Message text
     
      INTEGER C, R, L, S      ! Loop indices
      INTEGER ALLOCSTAT       ! Allocate status code
      INTEGER OFFSET          ! Starting cell number of a block
      INTEGER NCSP            ! Mech no: 1=gas/day 2=gas/night
      INTEGER BLK             ! Loop index for block of cells
      INTEGER CELLNUM         ! Cell number 
      INTEGER COL             ! Column index
      INTEGER IPAR            ! Pointer for cell sort routine
      INTEGER IRVAL           ! Pointer for cell sort routine
      INTEGER IRXN            ! Reaction number
      INTEGER ISP             ! Species index
      INTEGER ISPOLD          ! Species number in original order
      INTEGER ISPNEW          ! Species number in new sorted order 
      INTEGER ITMSTEP         ! Chemistry integration interval (sec)   
      INTEGER JPAR            ! Pointer for cell sort routine
      INTEGER JREORD          ! Index holder for sort routine
      INTEGER LEV             ! Layer index
      INTEGER LVAL            ! Pointer for cell sort routine
      INTEGER MIDDATE         ! Date at time step midpoint
      INTEGER MIDTIME         ! Time at time step midpoint
      INTEGER NCELL           ! Index for number of cells
      INTEGER NIRRCLS         ! No. of cells in block for IRR
      INTEGER NPH             ! Index for number of phot. rxns in PHOT
      INTEGER NRX             ! Index for number of reactions
      INTEGER ROW             ! Row index
      INTEGER SPC             ! Species loop index
      INTEGER VAR             ! Variable number on I/O API file

      INTEGER, ALLOCATABLE, SAVE :: IRRCELL( : )   ! Cell No. of an IRR cell

C for interpolating meteorological inputs
 
      INTEGER       :: GXOFF, GYOFF     ! global origin offset from file
      INTEGER, SAVE :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3
      INTEGER, SAVE :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2
      INTEGER, SAVE :: STRTCOLEM1, ENDCOLEM1, STRTROWEM1, ENDROWEM1
       
      REAL, ALLOCATABLE, SAVE :: SEAICE ( :, : )          ! fractional seaice cover, [-] 
!IVAI
      REAL, ALLOCATABLE, SAVE :: TEMP2( :, :)             ! 2-m temperature (K)
      REAL, ALLOCATABLE, SAVE :: Q2   ( :, :)             ! 2-m water vapor (Kg/Kg air)
!IVAI

!      REAL, ALLOCATABLE, SAVE :: DENSA_J( :, :, : )      ! Cell density (Kg/m**3)
      REAL, ALLOCATABLE, SAVE :: DENS   ( :, :, : )      ! Cell density (Kg/m**3)
      REAL, ALLOCATABLE, SAVE :: PRES   ( :, :, : )      ! Cell pressure (Pa)
      REAL, ALLOCATABLE, SAVE :: QV     ( :, :, : )      ! Cell water vapor (Kg/Kg air)
      REAL, ALLOCATABLE, SAVE :: TA     ( :, :, : )      ! Cell temperature (K)

!IVAI
      INTEGER       :: KOUNT

      REAL, ALLOCATABLE, SAVE:: AUX2D_01( :, :)          ! 2D auxiliary output
      REAL, ALLOCATABLE, SAVE:: AUX2D_02( :, :)          ! 2D auxiliary output
      REAL, ALLOCATABLE, SAVE:: AUX2D_03( :, :)          ! 2D auxiliary output
      REAL, ALLOCATABLE, SAVE:: o3_old  ( :, :, : )      ! o3 before gas-phase
      REAL, ALLOCATABLE, SAVE:: o3_new  ( :, :, : )      ! o3 after  gas-phase
      REAL, ALLOCATABLE, SAVE:: o3_tend ( :, :, : )      ! o3 tendency gas-phase
      REAL, ALLOCATABLE, SAVE:: no2_old ( :, :, : )      ! no2 before gas-phase
      REAL, ALLOCATABLE, SAVE:: no2_new ( :, :, : )      ! no2 after  gas-phase
      REAL, ALLOCATABLE, SAVE:: no2_tend( :, :, : )      ! no2 tendency gas-phase
!IVAI

      REAL,    ALLOCATABLE       :: OPEN_ZONE   ( :,: )       ! fractional sea water cover, [-]
      REAL,    ALLOCATABLE       :: SURF_ZONE   ( :,: )       ! fractional surf-zone cover, [-]
      LOGICAL, ALLOCATABLE, SAVE :: LAND_ZONE   ( :,: )       ! Is the surface totally land?

!     REAL :: RJBLK( BLKSIZE,NPHOTAB )           ! J-values for each cell in 
                                                 ! a block

!      REAL :: RJ( NCOLS,NROWS,NLAYS,NPHOTAB )  ! J-values for each cell      
      REAL,      ALLOCATABLE, SAVE :: RJ( :,:,:,: )  ! J-values for each cell      

      REAL( 8 ), ALLOCATABLE, SAVE :: BLKHET( :, : )

#ifdef rbstats
      CHARACTER( 16 ), SAVE :: CTM_RBSTATS_1 = 'CTM_RBSTATS_1' 
      CHARACTER( 16 ), ALLOCATABLE, SAVE :: VSTATS( : ) 

      INTEGER, SAVE                      :: WSTEP = 0     
      INTEGER, ALLOCATABLE, SAVE         :: STAT_SUM( :,:,:,: )
      INTEGER  EDATE, ETIME

      REAL     ALLOCATABLE, SAVE         :: STATOUT( :, :, : )
#endif

      INTERFACE
!        SUBROUTINE PHOT ( MDATE, MTIME, JDATE, JTIME, DTSTEP, RJ )
         SUBROUTINE PHOT ( MDATE, MTIME, JDATE, JTIME, DTSTEP, RJ,
     &                     o3_old, o3_new, o3_tend,              !IVAI: 3D
     &                     aux2d_01, aux2d_02, aux2d_03 )        !IVAI: 2D
            INTEGER, INTENT( IN )  :: MDATE       !"centered" Julian date (YYYYDDD)
            INTEGER, INTENT( IN )  :: MTIME       !"centered" time (HHMMSS)
            INTEGER, INTENT( IN )  :: JDATE       ! Current date (YYYYDDD)
            INTEGER, INTENT( IN )  :: JTIME       ! Current time (HHMMSS)
            INTEGER, INTENT( IN )  :: DTSTEP( : ) ! time step vector (HHMMSS)
            REAL,    INTENT( OUT ) :: RJ( :,:,:,: ) ! gridded J-values  (1/min units)
          REAL,    INTENT( IN )  :: o3_old (:,:,:)    ! 3D output !IVAI
          REAL,    INTENT( IN )  :: o3_new (:,:,:)    ! 3D output !IVAI
          REAL,    INTENT( IN )  :: o3_tend(:,:,:)    ! 3D output !IVAI
          REAL,    INTENT( INOUT )  :: aux2d_01(:,:)  ! 2D aux output !IVAI
          REAL,    INTENT( INOUT )  :: aux2d_02(:,:)  ! 2D aux output !IVAI
          REAL,    INTENT( INOUT )  :: aux2d_03(:,:)  ! 2D aux output !IVAI
         END SUBROUTINE PHOT
! IVAI: Comment out PA_ routines (can't compile)
!         SUBROUTINE PA_IRR_CKBLK ( NUMCELLS, LIRRFLAG, OFFSET,
!     &                             CCOL, CROW, CLEV, NORDCELL,
!     &                             NIRRCLS, IRRCELL )
!            INTEGER, INTENT( IN )  :: NUMCELLS, OFFSET
!            LOGICAL, INTENT( OUT ) :: LIRRFLAG
!            INTEGER, INTENT( IN )  :: CCOL( : ), CROW( : ), CLEV( : )
!            INTEGER, INTENT( IN )  :: NORDCELL( : )
!            INTEGER, INTENT( INOUT ) :: NIRRCLS
!            INTEGER, INTENT( INOUT ) :: IRRCELL( : )
!         END SUBROUTINE PA_IRR_CKBLK
!         SUBROUTINE PA_IRR_BLKENDC ( OFFSET, 
!     &                               CCOL, CROW, CLEV, NORDCELL,
!     &                               NIRRCLS, IRRCELL )
!            INTEGER, INTENT( IN ) :: OFFSET
!            INTEGER, INTENT( IN ) :: CCOL( : ), CROW( : ), CLEV( : )
!            INTEGER, INTENT( IN ) :: NORDCELL( : )
!            INTEGER, INTENT( INOUT ) :: NIRRCLS
!            INTEGER, INTENT( INOUT ) :: IRRCELL( : )
!         END SUBROUTINE PA_IRR_BLKENDC
! IVAI
         SUBROUTINE RBSOLVER ( LOGDEV, JDATE, JTIME, CHEMSTEP, NCSP,
     &                         LIRRFLAG, NIRRCLS, IRRCELL )
            INTEGER,   INTENT( IN ) :: LOGDEV, JDATE, JTIME
            REAL( 8 ), INTENT( IN ) :: CHEMSTEP
            INTEGER,   INTENT( IN ) :: NCSP
            LOGICAL,   INTENT( IN ) :: LIRRFLAG
            INTEGER,   INTENT( INOUT ) :: NIRRCLS
            INTEGER,   INTENT( IN ) :: IRRCELL( : )
         END SUBROUTINE RBSOLVER
         SUBROUTINE FIND_DEGRADED( JDATE, JTIME, LOGDEV, CALL_DEGRADE )
           INTEGER, INTENT( IN )  :: JDATE        ! current model date , coded YYYYDDD
           INTEGER, INTENT( IN )  :: JTIME        ! current model time , coded HHMMSS
           INTEGER, INTENT( IN )  :: LOGDEV       ! unit number for the log file
           LOGICAL, INTENT( OUT ) :: CALL_DEGRADE ! whether to call degradation routines
         END SUBROUTINE FIND_DEGRADED
         SUBROUTINE INIT_DEGRADE( CBLK, TCELL, DCELL, PHOTO_CELL, NUMCELLS,
     &                         JDATE, JTIME, LOGDEV, BLKID )
           REAL( 8 ), INTENT( IN ) :: CBLK ( :,: )      !  species concentration in cell
           REAL( 8 ), INTENT( IN ) :: TCELL( : )        !  cell temperature  [ k ]
           REAL( 8 ), INTENT( IN ) :: DCELL( : )        !  cell air density  [ kg/m^3 ]
           REAL( 8 ), INTENT( IN ) :: PHOTO_CELL( :,: ) ! Photolysis table for cell [1/s]
           INTEGER,   INTENT( IN ) :: NUMCELLS  ! number of grid cells in block
           INTEGER,   INTENT( IN ) :: JDATE     ! current model date , coded YYYYDDD
           INTEGER,   INTENT( IN ) :: JTIME     ! current model time , coded HHMMSS
           INTEGER,   INTENT( IN ) :: LOGDEV    ! unit number for the log
           INTEGER,   INTENT( IN ) :: BLKID     ! number for the block
         END SUBROUTINE INIT_DEGRADE      
         SUBROUTINE FINAL_DEGRADE( CBLK )
           REAL( 8 ), INTENT( INOUT ) :: CBLK( :,: ) ! species conc in cell
         END SUBROUTINE FINAL_DEGRADE
      END INTERFACE

C**********************************************************************

      IF ( NUMB_MECH_SPC .EQ. 0 ) THEN
         CALL M3MESG( '*** WARNING: Gas-Chemistry processing bypassed!' )
         RETURN
      END IF

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, call routines to set-up for Gear solver and 
c  set-up to do emissions here if that option is invoked
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LFIRST ) THEN
         LFIRST   = .FALSE.
         LOGDEV   = INIT3( )
         ROS3_LOG = LOGDEV
!        GASLOG   = LOGDEV

         IF ( .NOT. CELLVAR_ALLOC() ) THEN
            MSG = 'Failure allocating variables dependent on horizontal extents'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

! IVAI: NB. NLAYC/NLAYT must be defined in GRID_CONF before RBINIT

         CALL RBINIT
         CALL RBSPARSE( LOGDEV )
         NOXYZ = MY_NCOLS * MY_NROWS * NLAYS

! IVAI
         NOXYZ_CAN = MY_NCOLS * MY_NROWS * (NLAYS + NLAYC)
         WRITE( LOGDEV, *) 'RBDRIVER: NLAYC NLAYT = ', NLAYC, NLAYT
! IVAI

C...Initialize and report data

!NB. 
! NOXYZ=16384   NBLKS=328       (BLKSIZE=50)
! BLKLEN(1)=50  BLKLEN( NBLKS )=34 (last block)
         WRITE( LOGDEV, 92020 ) NOXYZ, BLKSIZE, NBLKS, BLKLEN( 1 ), BLKLEN( NBLKS )

! IVAI
! NOXYZ= 17152  NBLKS_CAN= 344  (BLKSIZE=50)
! BLKLEN_CAN(1)=50   BLKLEN_CAN( NBLKS_CAN )= 2 (last block)
         WRITE( LOGDEV, 92020 ) NOXYZ_CAN, BLKSIZE, NBLKS_CAN, BLKLEN_CAN( 1 ), BLKLEN_CAN( NBLKS_CAN )
! IVAI

         WRITE( LOGDEV, 92040 ) GLBL_RTOL, GLBL_ATOL

C.. Get fractional seawater coverage from the OCEAN file.

         ALLOCATE( OPEN_ZONE( NCOLS, NROWS ), SURF_ZONE( NCOLS, NROWS ) )

         CALL SUBHFILE ( OCEAN_1, GXOFF, GYOFF,
     &                   STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3 )

         VNAME = 'OPEN'
         IF ( .NOT. INTERPX( OCEAN_1, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,1,
     &                       JDATE, JTIME, OPEN_ZONE ) ) THEN
            MSG = 'Could not read OPEN from ' // OCEAN_1
            CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

C.. Get fractional surf-zone coverage from the OCEAN file.

         VNAME = 'SURF'
         IF ( .NOT. INTERPX( OCEAN_1, VNAME, PNAME,
     &                       STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,1,
     &                       JDATE, JTIME, SURF_ZONE ) ) THEN
            MSG = 'Could not read SURF from ' // OCEAN_1
            CALL M3EXIT ( PNAME, JDATE, JTIME, MSG, XSTAT1 )
         END IF

         ALLOCATE( LAND_ZONE( NCOLS, NROWS ) )
         
         DO ROW = 1, NROWS
            DO COL = 1, NCOLS
               IF( OPEN_ZONE( COL,ROW ) + SURF_ZONE( COL,ROW ) .GT. 0.0 )THEN
                   LAND_ZONE( COL,ROW ) = .FALSE.
               ELSE
                   LAND_ZONE( COL,ROW ) = .TRUE.
               END IF
             END DO
         END DO

         DEALLOCATE( OPEN_ZONE, SURF_ZONE )

         CALL SUBHFILE ( MET_CRO_2D, GXOFF, GYOFF,
     &                   STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 )

         CALL SUBHFILE ( MET_CRO_3D, GXOFF, GYOFF,
     &                   STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 )

         STARTCOLCO = COLSX_PE( 1, MYPE + 1 )
         ENDCOLCO   = COLSX_PE( 2, MYPE + 1 )
         STARTROWCO = ROWSX_PE( 1, MYPE + 1 )
         ENDROWCO   = ROWSX_PE( 2, MYPE + 1 )

         ALLOCATE( DENS( NCOLS, NROWS, NLAYS ), PRES( NCOLS, NROWS, NLAYS ),
     &             QV  ( NCOLS, NROWS, NLAYS ), TA  ( NCOLS, NROWS, NLAYS ),
     &             RJ  ( NCOLS, NROWS, NLAYS, NPHOTAB ),
     &             SEAICE( NCOLS, NROWS ),
! IVAI
! 2D
     &             Q2    ( NCOLS, NROWS ),       TEMP2   ( NCOLS, NROWS ),
! 2D AUX
     &           aux2d_01( NCOLS, NROWS ),       aux2d_02( NCOLS, NROWS ),
! 3D AUX
     &           aux2d_03( NCOLS, NROWS ),
! 3D
     &             o3_old( NCOLS, NROWS, NLAYS ), no2_old( NCOLS, NROWS, NLAYS ),
     &             o3_new( NCOLS, NROWS, NLAYS ), no2_new( NCOLS, NROWS, NLAYS ),
     &             o3_tend(NCOLS, NROWS, NLAYS ), no2_tend(NCOLS, NROWS, NLAYS ))

         ALLOCATE( IRRCELL( BLKSIZE ) )
         IRRCELL = 0

c..Open file for solver stats if requested
#ifdef rbstats
         ALLOCATE( VSTATS( 3 ) )
         VSTATS( 1 ) = 'N_STRT_FAILS'
         VSTATS( 2 ) = 'N_FAILS'
         VSTATS( 3 ) = 'N_STEPS'

         IF ( MYPE .EQ. 0 ) THEN

            IF ( .NOT. OPEN3( CTM_CONC_1, FSREAD3, PNAME ) ) THEN
               MSG = 'Could not open ' // CTM_CONC_1 // ' file for readonly'
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF

            IF ( .NOT. DESC3( CTM_CONC_1 ) ) THEN
               MSG = 'Could not get description of concentration file '
     &             // CTM_CONC_1
               CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT2 )
            END IF

            EDATE = JDATE
            ETIME = JTIME
            CALL NEXTIME( EDATE, ETIME, TSTEP( 1 ) )

            SDATE3D = EDATE
            STIME3D = ETIME
            NVARS3D = 3
            NCOLS3D = GL_NCOLS
            NROWS3D = GL_NROWS
            NLAYS3D = NLAYS
            VNAME3D( 1 ) = 'N_STRT_FAILS'
            VNAME3D( 2 ) = 'N_FAILS'
            VNAME3D( 3 ) = 'N_STEPS'
            VDESC3D( 1 ) = 'Number of fails at start'
            VDESC3D( 2 ) = 'Number of step fails'
            VDESC3D( 3 ) = 'Number of steps'
            UNITS3D( 1 ) = 'None'
            UNITS3D( 2 ) = 'None'
            UNITS3D( 3 ) = 'None'
            VTYPE3D( 1 ) = M3REAL
            VTYPE3D( 2 ) = M3REAL
            VTYPE3D( 3 ) = M3REAL
            IF ( .NOT. OPEN3( CTM_RBSTATS_1, FSNEW3, PNAME ) ) THEN
               MSG = 'Could not create '// TRIM( CTM_RBSTATS_1 ) // ' file'
               CALL M3EXIT( PNAME, SDATE3D, STIME3D, MSG, XSTAT2 )
            END IF

         END IF

         ALLOCATE( STAT_SUM( MY_NCOLS, MY_NROWS, NLAYS, 3 ) )
         ALLOCATE(  STATOUT( MY_NCOLS, MY_NROWS, NLAYS ) )


         STAT_SUM = 0

#endif

         ALLOCATE( BLKHET( BLKSIZE, NHETERO ) )

C Determine whether DEGRADE rountines are needed.

         CALL FIND_DEGRADED( JDATE, JTIME, LOGDEV, CALL_DEG )
         IF( CALL_DEG ) THEN
            WRITE( LOGDEV, * ) 'DEGRADE ROUTINES USED'
            WRITE( LOGDEV, * ) 'Mechanism contains degraded species'
#ifdef verbose_gas         
         ELSE
            WRITE( LOGDEV, * ) 'DEGRADE ROUTINES not USED'
            WRITE( LOGDEV, * ) 'Mechanism contains NO degraded species'
#endif            
         ENDIF

C set up degradation array


         ALLOCATE( Y_DEGRADE( BLKSIZE, NSPCSD ) )

      END IF      ! First call

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Start of integration driver after first call
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      NIRRCLS = 0

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Set date and time to center of time step, get necessary physical 
C  data, and get photolysis rates
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      MIDDATE = JDATE
      MIDTIME = JTIME
      ITMSTEP = TIME2SEC( TSTEP( 2 ) )
      CHEMSTEP = REAL( ITMSTEP, 8 ) / 60.0D0
      CALL NEXTIME( MIDDATE, MIDTIME, SEC2TIME( ITMSTEP / 2 ) )
 
C.. Get fractional seaice coverage from the METCRO2D file.

      VNAME = 'SEAICE'
      IF ( .NOT. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    MIDDATE, MIDTIME, SEAICE ) ) THEN
         MSG = 'Could not read SEAICE from MET_CRO_2D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

!IVAI
!.. Get 2-m temperature from the METCRO2D file.
      VNAME = 'TEMP2'
      If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    MIDDATE, MIDTIME, TEMP2 ) ) Then
         MSG = 'Could not read TEMP2 from MET_CRO_2D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      End If

!.. Get 2-m Specific Humidity from the METCRO2D file.
      VNAME = 'Q2'
      If ( .Not. INTERPX( MET_CRO_2D, VNAME, PNAME,
     &                    STRTCOLMC2,ENDCOLMC2, STRTROWMC2,ENDROWMC2, 1,1,
     &                    MIDDATE, MIDTIME, Q2 ) ) Then
         MSG = 'Could not read Q2 from MET_CRO_2D'
         Call M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      End If
!IVAI

C.. Get ambient temperature in K

      VNAME = 'TA' 
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, TA ) ) THEN
         MSG = 'Could not read TA from MET_CRO_3D'
         CALL M3EXIT( PNAME, MIDDATE, MIDTIME, MSG, XSTAT1 )
      END IF
      
C.. Get specific humidity in Kg H2O / Kg air

      VNAME = 'QV'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, QV ) ) THEN
         MSG = 'Could not read QV from MET_CRO_3D'
         CALL M3EXIT( PNAME, MIDDATE, MIDTIME, MSG, XSTAT1 )
      END IF 
      
C.. Get pressure in Pascals

      VNAME = 'PRES'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, PRES ) ) THEN
         MSG = 'Could not read PRES from MET_CRO_3D'
         CALL M3EXIT ( PNAME, MIDDATE, MIDTIME, MSG, XSTAT1 )
      END IF

C.. Get density in Kg/m^3

      VNAME = 'DENS'
      IF ( .NOT. INTERPX( MET_CRO_3D, VNAME, PNAME,
     &                    STRTCOLMC3,ENDCOLMC3, STRTROWMC3,ENDROWMC3, 1,NLAYS,
     &                    MIDDATE, MIDTIME, DENS ) ) THEN
         MSG = 'Could not read DENS from MET_CRO_3D'
         CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )
      END IF

C.. Get photolysis rates in /min

      CALL PHOT ( MIDDATE, MIDTIME, JDATE, JTIME, TSTEP, RJ,
!IVAI
     &            o3_old  (:,:,:), o3_new  (:,:,:), o3_tend (:,:,:),  ! 3D
     &            aux2d_01(:,:)  , aux2d_02(:,:)  , aux2d_03(:,:)  )  ! 2D
!IVAI

C.. Get Heterogeneous Rates using Aerosol Surface Area. Also Store
C   a snapshot of the aerosol surface area so that it can be
C   appropriately updated after the solver finds a solution.

      CALL HETCHEM_RATES( TA, PRES, QV, CONC, DENS )

      WRITE( LOGDEV, *) , 'RBDRIVER: NBLKS, NBLKS_CAN= ', NBLKS, NBLKS_CAN

! IVAI: NB. Species indices defined ! in "rbdata_mod", for O3 = 4)
! Species before gas-phase tendencies added
      o3_old     (:,:,:) = CONC    (:,:,:,4) ! O3 = 4
!IVAI

! Initialize canopy levels 
      If ( CANOPY_SHADE  ) Then

c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++
!     IF (.FALSE.) THEN    ! ( CANOPY_SHADE  )
      DO R = 1, MY_NROWS
         DO C = 1, MY_NCOLS

            KOUNT = 0
            IF ( FRT_MASK(C,R) > 0. ) THEN

            IF ( KOUNT < 4 )  THEN

               DO L = 1, 4 !NLAYT

               WRITE( LOGDEV, '( A, 2I4, I3, 1X, I7, 1X, I6 ) ' )
     &              'Canopy Debug output for col/row/lev/date/time:',
     &               C, R, L, JDATE, JTIME

               WRITE( LOGDEV,  '( A, 1X, 1PE13.5, 1PE13.5, 1PE13.5, 1PE13.5)' )
     &         'PHY CANOPY ', TA_CAN(C,R, L), PRES_CAN(C,R, L), DENS_CAN(C,R, L), QV_CAN(C,R, L)

               END DO ! NLAYT

            END IF ! KOUNT

! Print up to KOUNT number of canopy columns
               KOUNT = KOUNT + 1

            END IF ! FRT_MASK

         END DO
      END DO
!     END IF ! .FALSE. & CANOPY_SHADE 
c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++

! Temporary output
!     aux2d_03(:,:) = QV      (:,:,1)          ! coszens
!     aux2d_02(:,:) = DENS    (:,:,1)          ! jno2
!     aux2d_01(:,:) = TA      (:,:,1)          ! jo3o1d
!     aux2d_03(:,:) = QV_CAN  (:,:,1)          ! coszens
!     aux2d_02(:,:) = DENS_CAN(:,:,1)          ! jno2
!     aux2d_01(:,:) = TA_CAN  (:,:,1)          ! jo3o1d

! Temporary output
!     aux2d_03(:,:) = FRT_MASK(:,:)
!     aux2d_03(:,:) = RJ     (:,:,1,LO3O1D)    ! coszens
!     aux2d_02(:,:) = RJ_CAN (:,:,1,LO3O1D)    ! jno2

!     aux2d_02(:,:) = real(    KHETERO(NHETERO,:,:,1 ),4) ! NHETERO=11 reaction
!     aux2d_03(:,:) = real(KHETERO_CAN(NHETERO,:,:,1 ),4) ! NHETERO=11 reaction

! This is CONC_can from canopy_transfer after r2c (flag = 0)
!     aux2d_02(:,:) = CONC_CAN(:,:,1,4) ! 1st canopy layer O3 = 4

!!!!!!!!!!!!!!!!!
! !!! TESTING !!! Overwrite met3d & cgrid calculated in get_can_levs (phot)
!!!!!!!!!!!!!!!!! 
! NB. Comment out to use values from canopy_transfer "resolved_to_canopy"
!
!... First, carry over original model values for the matching layers
!     TA_CAN   (:,:,NLAYC+1:NLAYT)   = TA  (:,:,1:NLAYS)
!     QV_CAN   (:,:,NLAYC+1:NLAYT)   = QV  (:,:,1:NLAYS)
!     PRES_CAN (:,:,NLAYC+1:NLAYT)   = PRES(:,:,1:NLAYS)
!     DENS_CAN (:,:,NLAYC+1:NLAYT)   = DENS(:,:,1:NLAYS)

! Overwrite CONC_can calculated in canopy_transfer "resolved_to_canopy"
!   NB. Comment out to use values from canopy_transfer "resolved_to_canopy"
!     CONC_CAN(:,:,NLAYC+1:NLAYT,:) = CONC(:,:,1:NLAYS,:)

!... Initialize the canopy layers with 1hy layer values
!     TA_CAN(:,:,3) = TA( :,:,1) ! using values from get_can_levs when commented out
!     TA_CAN(:,:,2) = TA( :,:,1)
!     TA_CAN(:,:,1) = TA( :,:,1)

!     QV_CAN(:,:,3) = QV( :,:,1)
!     QV_CAN(:,:,2) = QV( :,:,1)
!     QV_CAN(:,:,1) = QV( :,:,1)

!     PRES_CAN(:,:,3) = PRES( :,:,1)
!     PRES_CAN(:,:,2) = PRES( :,:,1)
!     PRES_CAN(:,:,1) = PRES( :,:,1)

!     DENS_CAN(:,:,3) = DENS( :,:,1)
!     DENS_CAN(:,:,2) = DENS( :,:,1)
!     DENS_CAN(:,:,1) = DENS( :,:,1)

! NB. Must initialize all canopy & non-canopy columns RJ_CAN & KHETERO_CAN

!... First, carry over original model values for the matching layers
! NB. KHETERO_CAN(NHETERO,NCOLS, NROWS, NLAYT) is real(8)
      KHETERO_CAN(:,:,:,NLAYC+1:NLAYT)   = KHETERO(:,:,:,1:NLAYS) ! real(8)

!... Initialize the canopy layers with 1hy layer values
      KHETERO_CAN(:,:,:,3) = KHETERO(:,:,:,1)  ! real(8)
      KHETERO_CAN(:,:,:,2) = KHETERO(:,:,:,1)  ! real(8)
      KHETERO_CAN(:,:,:,1) = KHETERO(:,:,:,1)  ! real(8)

! Overwrite CONC_can calculated in canopy_transfer "resolved_to_canopy"
! NB. Comment out to use values from canopy_transfer "resolved_to_canopy"
!     CONC_CAN(:,:,3,:) = CONC(:,:,1,:)
!     CONC_CAN(:,:,2,:) = CONC(:,:,1,:)
!     CONC_CAN(:,:,1,:) = CONC(:,:,1,:)

!!!!!!!!!!!!!!!!!!!!!
! !!! END TESTING !!! overwrite met3d & cgrid calculated in get_can_levs
!!!!!!!!!!!!!!!!!!!!!

      End If  !CANOPY_SHADE

! NB. Species indices defined ! in "rbdata_mod", for O3 = 4)
! Species before gas-phase tendencies added
      o3_old_can (:,:,:) = CONC_CAN(:,:,:,4) ! O3 = 4

!IVAI

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Set flag for reordering of cells and put cells in sequential  
c  order initially
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      LORDERING = .TRUE.
      IF ( .NOT. LREORDER .OR. NBLKS .EQ. 1 ) LORDERING = .FALSE.
      DO NCELL = 1, NOXYZ
         NORDCELL( NCELL ) = NCELL
      END DO

!IVAI: NB. Same LORDERING flag applies on canopy layers
!NB.  NORDCELL = 1 ... 16384
!     write( logdev,* ) 'RBDRIVER: NORDCELL = ', NORDCELL(:)

      IF (CANOPY_SHADE) THEN

      DO NCELL = 1, NOXYZ_CAN
         NORDCELL_CAN( NCELL ) = NCELL
      END DO

!NB. NORDCELL_CAN = 1... 17152
!     write( logdev,* ) 'RBDRIVER: NORDCELL_CAN = ', NORDCELL_CAN(:)

      End If  !CANOPY_SHADE

!IVAI

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Top of loop over blocks. This loop will be done once if
C  no reordering, twice if reordering is required
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
100   CONTINUE

      write( logdev,* ) 'driver-lordering,lreorder: ', lordering, lreorder

      ERRMX2 = 0.0D0

      WRITE( LOGDEV, *) , 'RBDRIVER 500: NBLKS= ', NBLKS
      DO 500 BLK = 1, NBLKS
         BLKID = BLK
         NUMCELLS = BLKLEN( BLK )
         OFFSET = BLKCNO( BLK )
         IF ( .NOT. LORDERING .AND. LIRR ) THEN
             LIRRBLK = .FALSE.
! IVAI
!             CALL PA_IRR_CKBLK ( NUMCELLS, LIRRBLK, OFFSET,
!     &                           CCOL, CROW, CLEV, NORDCELL, NIRRCLS,
!     &                           IRRCELL )
!             CALL PA_IRR_BLKSTRT( NUMCELLS )
! IVAI
         END IF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell physical data in the block arrays, converting
C  pressure to atmospheres, water vapor to ppm, emissions to ppm/min,
C  setting to land if seaice coverage is nonzero 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         DO NCELL = 1, NUMCELLS
            CELLNUM = NORDCELL( OFFSET + NCELL )
            COL = CCOL( CELLNUM )
            ROW = CROW( CELLNUM )
            LEV = CLEV( CELLNUM )
            BLKTEMP( NCELL ) = REAL( TA( COL,ROW,LEV ), 8 )
            BLKDENS( NCELL ) = REAL( DENS( COL,ROW,LEV ), 8 )
            BLKSVOL( NCELL ) = REAL( 1.0 / BLKDENS( NCELL ), 8 )
            BLKPRES( NCELL ) = REAL( PA2ATM * PRES( COL, ROW, LEV ), 8 )
            BLKCH2O( NCELL ) = REAL( MAX( QV_TO_PPM * QV( COL,ROW,LEV ), 0.0 ), 8)
            IF( LAND_ZONE( COL,ROW ) .OR. SEAICE (COL,ROW) .GT. 0.0 )THEN
                BLKLAND (NCELL) = .TRUE.
            ELSE
                BLKLAND (NCELL) = .FALSE.
            END IF            
         END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell concentrations in the block arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO ISP = 1, ISCHANG( NCS )
            SPC    = CGRID_INDEX( ISP )
!            SPC    = ISP 
            ISPNEW = IOLD2NEW( ISP,NCS )
            DO NCELL = 1, NUMCELLS
               CELLNUM = NORDCELL( OFFSET + NCELL )
               COL = CCOL( CELLNUM )
               ROW = CROW( CELLNUM )
               LEV = CLEV( CELLNUM )
               IF( CONVERT_CONC( ISP ) )THEN 
                   Y( NCELL,ISPNEW ) = REAL( MAX( FORWARD_CONV( ISP ) * BLKSVOL( NCELL )
     &                               *       CONC( COL,ROW,LEV,SPC ), CONCMIN), 8 )
               ELSE
                   Y( NCELL,ISPNEW ) = REAL( MAX( CONC( COL,ROW,LEV,SPC ), CONCMIN), 8 )
               END IF
            END DO                 
         END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C   Get photolytic, heteorogeneous and thermal rate constants & call solver
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LSUNLIGHT = .FALSE.

         DO NCELL = 1, NUMCELLS
            CELLNUM = NORDCELL( OFFSET + NCELL )
            COL = CCOL( CELLNUM )   ! wrong order
            ROW = CROW( CELLNUM )
            LEV = CLEV( CELLNUM )
!IVAI
!              IF (LEV .EQ. 1)
!    &         WRITE( LOGDEV, '( A, 2I4, I3, 1X, I7, 1X, I6 ) ' )
!    &              'Canopy Debug output for col/row/lev/date/time:',
!    &               COL, ROW, LEV, JDATE, JTIME
!IVAI
            DO NPH = 1, NHETERO
               BLKHET( NCELL, NPH ) =  KHETERO( NPH, COL, ROW, LEV )
            END DO
            DO NPH = 1, NPHOTAB
               RJBLK( NCELL,NPH ) = REAL( RJ( COL,ROW,LEV,NPH ), 8 )
               IF ( RJBLK( NCELL, NPH ) .GT. 0.0D0 ) LSUNLIGHT = .TRUE.
!IVAI
!              IF (LEV .EQ. 1)
!    &         WRITE( LOGDEV,  '( A, I3, 1X, 1PE13.5, E13.5 )' )
!    &        'RJ ', NPH, RJBLK( NCELL,NPH ), RJ( COL,ROW,LEV,NPH )
!IVAI
            END DO                         
         END DO
         
         CALL CALC_RCONST( BLKTEMP, BLKPRES, BLKCH2O, RJBLK, BLKHET, LSUNLIGHT, BLKLAND, RKI, NUMCELLS )

! IVAI
!        write(logdev,*) 'RBDRIVER 500: Canopy Debug output from RCONST '
!        DO NCELL = 1, NUMCELLS
!          DO N = 1, N_RXNS
!            WRITE( LOGDEV, '( A, I3, 1X, 1PE13.5 )' )
!    &                     'RKI ', N, RKI( NCELL, N )
!          END DO
!        END DO
! IVAI

         IF ( LSUNLIGHT ) THEN
            NCSP = NCS
         ELSE
            NCSP = NCS + 1
         END IF

C  update cell concentrations for degradation routines

         IF ( CALL_DEG ) THEN

            Y_DEGRADE = 0.0
            DO ISP = 1, NSPCSD
               DO NCELL = 1, NUMCELLS
                  CELLNUM = NORDCELL( OFFSET + NCELL )
                  COL = CCOL( CELLNUM )
                  ROW = CROW( CELLNUM )
                  LEV = CLEV( CELLNUM )
                  Y_DEGRADE( NCELL,ISP ) = REAL(MAX( CONC( COL,ROW,LEV,ISP ), CONCMIN), 8 )
               END DO
            END DO

!IVAI
! NB. Not used
            WRITE( LOGDEV, *) 'RBDRIVER 500: BLKID = ', BLKID, NUMCELLS
!IVAI
C initialize degradation routines

            CALL INIT_DEGRADE( Y_DEGRADE, BLKTEMP, BLKDENS, RJBLK,
     &                         NUMCELLS, JDATE, JTIME, LOGDEV, BLKID )

         END IF

#ifdef rbstats

         NSTEPS = 0
         NFAILS = 0
         N_BAD_STARTS = 0

#endif

         CALL RBSOLVER( LOGDEV, JDATE, JTIME, CHEMSTEP, NCSP,
     &                  LIRRBLK, NIRRCLS, IRRCELL ) 

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  If not ordering cells, save performance statistics and
C  store updated concentrations.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( .NOT. LORDERING ) THEN

C..Update concentrations
            DO ISP = 1, ISCHANG( NCS )
               ISPOLD  = INEW2OLD( ISP,NCS )
               SPC     = CGRID_INDEX( ISPOLD )
               DO NCELL = 1, NUMCELLS
                  CELLNUM = NORDCELL( OFFSET + NCELL )
                  COL = CCOL( CELLNUM )
                  ROW = CROW( CELLNUM )
                  LEV = CLEV( CELLNUM )
                  IF( CONVERT_CONC( ISPOLD ) )THEN 
                     CONC( COL,ROW,LEV,SPC ) = REAL( REVERSE_CONV( ISPOLD ) 
     &                                       *       BLKDENS( NCELL ) * Y( NCELL,ISP ), 4)
                  ELSE
                     CONC( COL,ROW,LEV,SPC ) = REAL( Y( NCELL,ISP ), 4)
                  END IF
              END DO
            END DO




            IF ( CALL_DEG ) THEN

C  Update degradation array with species treated by Rosenbach solver
C
C               DO ISP = 1, ISCHANG( NCS )
C                  ISPOLD  = INEW2OLD( ISP, NCS )
C                  DO NCELL = 1, NUMCELLS
C                     Y_DEGRADE( NCELL,ISPOLD ) = Y( NCELL,ISP )
C                  END DO
C               END DO


C  Update CGRID based on the degradation routines
               CALL FINAL_DEGRADE( Y_DEGRADE )
               UPDATE_DEGRADED: DO ISP = 1, N_REACT
                  S = RXTANT_MAP( ISP )
                  IF( S .LE. 0 )CYCLE UPDATE_DEGRADED                  
                  DO SPC = 1, NUMB_MECH_SPC
                     IF( S .EQ. CGRID_INDEX( SPC ) )CYCLE UPDATE_DEGRADED
                  END DO
                  DO NCELL = 1, NUMCELLS
                     CELLNUM = NORDCELL( OFFSET + NCELL )
                     COL = CCOL( CELLNUM )
                     ROW = CROW( CELLNUM )
                     LEV = CLEV( CELLNUM )
                     CONC( COL,ROW,LEV,S ) = REAL( Y_DEGRADE( NCELL,S ), 4)
                  END DO
               END DO UPDATE_DEGRADED
            END IF            !WTH

#ifdef rbstats

            DO NCELL = 1, NUMCELLS
               CELLNUM = NORDCELL( OFFSET + NCELL )
               COL = CCOL( CELLNUM )
               ROW = CROW( CELLNUM )
               LEV = CLEV( CELLNUM )
               STAT_SUM( COL,ROW,LEV,1 ) = STAT_SUM( COL,ROW,LEV,1 )
     &                                   + N_BAD_STARTS
               STAT_SUM( COL,ROW,LEV,2 ) = STAT_SUM( COL,ROW,LEV,2 )
     &                                   + NFAILS
               STAT_SUM( COL,ROW,LEV,3 ) = STAT_SUM( COL,ROW,LEV,3 )
     &                                   + NSTEPS
            END DO

#endif
                      
! IVAI
!            IF ( LIRRBLK ) CALL PA_IRR_BLKENDC ( OFFSET, CCOL, CROW, CLEV,
!     &                                           NORDCELL, NIRRCLS, IRRCELL )
! IVAI

         END IF

500   CONTINUE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!IVAI

! Species after gas-phase
      o3_new  (:,:,:) = CONC(:,:,:,4)

! Gas-phase tendency  ppmv sec-1
      o3_tend (:,:,:) = ( o3_new(:,:,:) -  o3_old(:,:,:)) /
     &                  REAL (CHEMSTEP, 4) ! ppm  sec-1

! Temporary output
!     aux2d_03(:,:) = o3_tend     (:,:,3)          ! coszens
!     aux2d_02(:,:) = o3_tend     (:,:,2)          ! jno2
      aux2d_01(:,:) = o3_tend     (:,:,1)          ! jo3o1d

!IVAI

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Top of canopy loop over blocks. This loop will be done once if
C  no reordering, twice if reordering is required
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

200   CONTINUE

      IF ( CANOPY_SHADE ) THEN

      write( logdev,* ) 'Canopy driver-lordering,lreorder: ', lordering, lreorder

!IVAI
      WRITE( LOGDEV, *) , 'RBDRIVER 555: CANOPY LOOP BEGINS NBLKS_CAN= ', NBLKS_CAN
!IVAI

      ERRMX2_CAN = 0.0D0

! Temporary loop over NBLKS instead of NBLKS_CAN
      DO 555 BLK = 1, NBLKS
!     DO 555 BLK = 1, NBLKS_CAN
         BLKID = BLK
         NUMCELLS = BLKLEN( BLK )
         OFFSET = BLKCNO( BLK )
!        NUMCELLS = BLKLEN_CAN( BLK )
!        OFFSET = BLKCNO_CAN( BLK )

!IVAI
!        WRITE( LOGDEV, *) 'RBDRIVER 555: OFFSET = ', BLKID, NUMCELLS, OFFSET
!IVAI

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell physical data in the block arrays, converting
C  pressure to atmospheres, water vapor to ppm, emissions to ppm/min,
C  setting to land if seaice coverage is nonzero
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

         DO NCELL = 1, NUMCELLS
            CELLNUM = NORDCELL( OFFSET + NCELL )
            COL = CCOL( CELLNUM )
            ROW = CROW( CELLNUM )
            LEV = CLEV( CELLNUM )
!IVAI
!           write( LOGDEV, *) 'RBDRIVER 555: COL,ROW,LEV= ', COL,ROW,LEV,CELLNUM, NUMCELLS
!IVAI
            BLKTEMP( NCELL ) = REAL(   TA_CAN( COL,ROW,LEV ), 8 )
            BLKDENS( NCELL ) = REAL( DENS_CAN( COL,ROW,LEV ), 8 )
            BLKSVOL( NCELL ) = REAL( 1.0 / BLKDENS( NCELL ), 8 )
            BLKPRES( NCELL ) = REAL( PA2ATM * PRES_CAN( COL, ROW, LEV ), 8 )
            BLKCH2O( NCELL ) = REAL( MAX( QV_TO_PPM * QV_CAN( COL,ROW,LEV ), 0.0 ), 8)
!IVAI
            IF (FRT_MASK(COL,ROW) > 0. .AND. LEV .EQ. 1) THEN
               WRITE( LOGDEV, '( A, 2I4, I3, 1X, I7, 1X, I6 ) ' )
     &              'Canopy Debug output for col/row/lev/date/time:',
     &               COL, ROW, LEV, JDATE, JTIME
               WRITE( LOGDEV,  '( A, 1X, 1PE13.5, 1PE13.5, 1PE13.5, 1PE13.5)' )
     &         'PHY CANOPY 555: ', BLKTEMP( NCELL ), BLKDENS( NCELL ), 
     &                             BLKPRES( NCELL ), BLKCH2O( NCELL )
            END IF ! LEV=1

            IF( FRT_MASK( COL,ROW ) > 0. )THEN
                BLKCANOPY (NCELL) = .TRUE.
            ELSE
                BLKCANOPY (NCELL) = .FALSE.
            END IF
!IVAI
            IF( LAND_ZONE( COL,ROW ) .OR. SEAICE (COL,ROW) .GT. 0.0 )THEN
                BLKLAND (NCELL) = .TRUE.
            ELSE
                BLKLAND (NCELL) = .FALSE.
            END IF

         END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Put the grid cell concentrations in the block arrays
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         DO ISP = 1, ISCHANG( NCS )
            SPC    = CGRID_INDEX( ISP )
!            SPC    = ISP
            ISPNEW = IOLD2NEW( ISP,NCS )
            DO NCELL = 1, NUMCELLS
               CELLNUM = NORDCELL( OFFSET + NCELL )
               COL = CCOL( CELLNUM )
               ROW = CROW( CELLNUM )
               LEV = CLEV( CELLNUM )
!IVAI
               IF (FRT_MASK(COL,ROW) > 0. .AND. LEV .EQ. 1)
     &         WRITE( LOGDEV, '( A, 2I4, I3, 1X, I7, 1X, I6 ) ' )
     &              'Canopy Debug output for col/row/lev/date/time:',
     &               COL, ROW, LEV, JDATE, JTIME
!IVAI
               IF( CONVERT_CONC( ISP ) )THEN
                   Y( NCELL,ISPNEW ) = REAL( MAX( FORWARD_CONV( ISP ) * BLKSVOL( NCELL )
     &                               *       CONC_CAN( COL,ROW,LEV,SPC ), CONCMIN), 8 )
               ELSE
                   Y( NCELL,ISPNEW ) = REAL( MAX( CONC_CAN( COL,ROW,LEV,SPC ), CONCMIN), 8 )
               END IF

!IVAI: O3=4 SPC
              IF (FRT_MASK(COL,ROW) > 0. .AND. LEV .EQ. 1)
     &        WRITE( LOGDEV,  '( A, I3, 1X, 1PE13.5, E13.5 )' )
     &                        'SP ', SPC, Y( NCELL, ISPNEW ), CONC_CAN( COL,ROW,LEV,SPC )

!IVAI
            END DO
         END DO

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C   Get photolytic, heteorogeneous and thermal rate constants & call solver
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         LSUNLIGHT = .FALSE.

         DO NCELL = 1, NUMCELLS
            CELLNUM = NORDCELL( OFFSET + NCELL )
            COL = CCOL( CELLNUM )   ! wrong order
            ROW = CROW( CELLNUM )
            LEV = CLEV( CELLNUM )
            DO NPH = 1, NHETERO
               BLKHET( NCELL, NPH ) =  KHETERO_CAN( NPH, COL, ROW, LEV )
!IVAI
               IF (FRT_MASK(COL,ROW) > 0. .AND. LEV .EQ. 1) 
     &         WRITE( LOGDEV,  '( A, I3, 1X, 1PE13.5, E13.5 )' )
     &        'KHETERO_CAN ', NPH, BLKHET( NCELL,NPH ), KHETERO_CAN( NPH, COL, ROW, LEV )
!IVAI
            END DO
            DO NPH = 1, NPHOTAB
               RJBLK( NCELL,NPH ) = REAL( RJ_CAN( COL,ROW,LEV,NPH ), 8 )
               IF ( RJBLK( NCELL, NPH ) .GT. 0.0D0 ) LSUNLIGHT = .TRUE.
!IVAI
               IF (FRT_MASK(COL,ROW) > 0. .AND. LEV .EQ. 1) 
     &         WRITE( LOGDEV,  '( A, I3, 1X, 1PE13.5, E13.5 )' )
     &        'RJ_CAN ', NPH, RJBLK( NCELL,NPH ), RJ_CAN( COL,ROW,LEV,NPH )
!IVAI
            END DO
         END DO

         CALL CALC_RCONST( BLKTEMP, BLKPRES, BLKCH2O, RJBLK, BLKHET, LSUNLIGHT, BLKLAND, RKI, NUMCELLS )

! IVAI
!        write(logdev,*) 'RBDRIVER 555: Canopy Debug output from RCONST ', BLKID
         DO NCELL = 1, NUMCELLS
            CELLNUM = NORDCELL( OFFSET + NCELL )
            COL = CCOL( CELLNUM )
            ROW = CROW( CELLNUM )
            LEV = CLEV( CELLNUM )
! IVAI
!          IF (FRT_MASK(COL,ROW) > 0. .AND. LEV .EQ. 1)
           IF (FRT_MASK(COL,ROW) > 0. )
     &        WRITE( LOGDEV, '( A, 2I4, I3, 1X, I7, 1X, I6 ) ' )
     &        '555 Debug output for col/row/lev/date/time:',
     &        COL, ROW, LEV, JDATE, JTIME

!          DO N = 1, N_RXNS
!            IF (FRT_MASK(COL,ROW) > 0. .AND. LEV .EQ. 1)
!    &       WRITE( LOGDEV, '( A, I3, 1X, 1PE13.5 )' )
!    &                     'RKI ', N, RKI( NCELL, N )
!          END DO
! NB. N_RXNS=329
           IF (FRT_MASK(COL,ROW) > 0. .AND. LEV .EQ. 1)
     &       WRITE( LOGDEV, '( A, I3, 1X, 329 (1PE13.5) )' )
     &                     'RKI ', N_RXNS, RKI( NCELL, 1:N_RXNS )
         END DO
! IVAI

         IF ( LSUNLIGHT ) THEN
            NCSP = NCS
         ELSE
            NCSP = NCS + 1
         END IF

C  update cell concentrations for degradation routines

         IF ( CALL_DEG ) THEN

            Y_DEGRADE = 0.0
            DO ISP = 1, NSPCSD
               DO NCELL = 1, NUMCELLS
                  CELLNUM = NORDCELL( OFFSET + NCELL )
                  COL = CCOL( CELLNUM )
                  ROW = CROW( CELLNUM )
                  LEV = CLEV( CELLNUM )
                  Y_DEGRADE( NCELL,ISP ) = REAL(MAX( CONC_CAN( COL,ROW,LEV,ISP ), CONCMIN), 8 )
               END DO
            END DO

!IVAI: NB. INIT_DEGRADE Not used

C initialize degradation routines ! IVAI: NB. Not used

            CALL INIT_DEGRADE( Y_DEGRADE, BLKTEMP, BLKDENS, RJBLK, 
     &                         NUMCELLS, JDATE, JTIME, LOGDEV, BLKID )

         END IF

#ifdef rbstats

         NSTEPS = 0
         NFAILS = 0
         N_BAD_STARTS = 0

#endif

         write(logdev,*) 'RBDRIVER 555: CALL RBSOLVER OFFSET=', BLKID, NUMCELLS, OFFSET
         CALL RBSOLVER( LOGDEV, JDATE, JTIME, CHEMSTEP, NCSP,
     &                  LIRRBLK, NIRRCLS, IRRCELL )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  If not ordering cells, save performance statistics and
C  store updated concentrations.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         IF ( .NOT. LORDERING ) THEN

C..Update concentrations
            DO ISP = 1, ISCHANG( NCS )
               ISPOLD  = INEW2OLD( ISP,NCS )
               SPC     = CGRID_INDEX( ISPOLD )
               DO NCELL = 1, NUMCELLS
                  CELLNUM = NORDCELL( OFFSET + NCELL )
                  COL = CCOL( CELLNUM )
                  ROW = CROW( CELLNUM )
                  LEV = CLEV( CELLNUM )
                  IF( CONVERT_CONC( ISPOLD ) )THEN
                     CONC_CAN( COL,ROW,LEV,SPC ) = REAL( REVERSE_CONV( ISPOLD )
     &                                       *       BLKDENS( NCELL ) * Y( NCELL,ISP ), 4)
                  ELSE
                     CONC_CAN( COL,ROW,LEV,SPC ) = REAL( Y( NCELL,ISP ), 4)
                  END IF
              END DO
            END DO

            IF ( CALL_DEG ) THEN

C  Update degradation array with species treated by Rosenbrock solver
C
C               DO ISP = 1, ISCHANG( NCS )
C                  ISPOLD  = INEW2OLD( ISP, NCS )
C                  DO NCELL = 1, NUMCELLS
C                     Y_DEGRADE( NCELL,ISPOLD ) = Y( NCELL,ISP )
C                  END DO
C               END DO


C  Update CGRID based on the degradation routines
               CALL FINAL_DEGRADE( Y_DEGRADE )
               UPDATE_DEGRADED_CAN: DO ISP = 1, N_REACT
                  S = RXTANT_MAP( ISP )
                  IF( S .LE. 0 )CYCLE UPDATE_DEGRADED_CAN
                  DO SPC = 1, NUMB_MECH_SPC
                     IF( S .EQ. CGRID_INDEX( SPC ) )CYCLE UPDATE_DEGRADED_CAN
                  END DO
                  DO NCELL = 1, NUMCELLS
                     CELLNUM = NORDCELL( OFFSET + NCELL )
                     COL = CCOL( CELLNUM )
                     ROW = CROW( CELLNUM )
                     LEV = CLEV( CELLNUM )
                     CONC_CAN( COL,ROW,LEV,S ) = REAL( Y_DEGRADE( NCELL,S ), 4)
                  END DO
               END DO UPDATE_DEGRADED_CAN
            END IF            !WTH

#ifdef rbstats

            DO NCELL = 1, NUMCELLS
               CELLNUM = NORDCELL( OFFSET + NCELL )
               COL = CCOL( CELLNUM )
               ROW = CROW( CELLNUM )
               LEV = CLEV( CELLNUM )
               STAT_SUM( COL,ROW,LEV,1 ) = STAT_SUM( COL,ROW,LEV,1 )
     &                                   + N_BAD_STARTS
               STAT_SUM( COL,ROW,LEV,2 ) = STAT_SUM( COL,ROW,LEV,2 )
     &                                   + NFAILS
               STAT_SUM( COL,ROW,LEV,3 ) = STAT_SUM( COL,ROW,LEV,3 )
     &                                   + NSTEPS
            END DO

#endif

! IVAI
!            IF ( LIRRBLK ) CALL PA_IRR_BLKENDC ( OFFSET, CCOL, CROW, CLEV,
!     &                                           NORDCELL_CAN, NIRRCLS, IRRCELL )
! IVAI

         END IF

555   CONTINUE  ! BLK = 1, NBLKS

      END IF ! CANOPY_SHADE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF ( CANOPY_SHADE ) THEN

! Species after gas-phase (NB. only canopy-grid cells are updated)
      o3_new_can (:,:,:) = CONC_CAN(:,:,:,4) ! O3 = 4

! Temporary output
!     aux2d_02(:,:) = o3_new_can  (:,:,1)
!     aux2d_02(:,:) = CONC_CAN(:,:,1,4) ! O3 = 4 => o3_new_can

      o3_tend_can (:,:,:) = ( o3_new_can(:,:,:) -  o3_old_can(:,:,:)) /
     &                       REAL (CHEMSTEP, 4) ! ppmv sec-1

! Temporary output
      aux2d_03(:,:) = o3_tend_can (:,:,3)          ! coszens
      aux2d_02(:,:) = o3_tend_can (:,:,1)          ! jno2
!     aux2d_01(:,:) = o3_tend_can (:,:,1)          ! jo3o1d

!===============================================================================
! Gather tracer concentration from canopy layers into model resolved layers (flag = 1)
!===============================================================================

      CALL canopy_transfer(CONC, JDATE, JTIME, 1 )

! Input:
!   CONC_CAN after gas-phase
!
! Output:
!   CONC_MOD after gas-phase

! Temporary output
!     aux2d_01(:,:) =  CONC_MOD(:,:,NLAYS,4) ! top model layer O3 = 4
!     aux2d_01(:,:) =  CONC_MOD(:,:,2,4) ! 2hy model layer O3 = 4
!     aux2d_01(:,:) =  CONC_MOD(:,:,1,4) ! 1hy model layer O3 = 4


! Update CGRID for advection, deposition etc.
      DO L = 1, NLAYS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS

            KOUNT = 0
            IF ( FRT_MASK(C,R) > 0. ) THEN

c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++
            IF (.FALSE.) THEN
            IF ( KOUNT < 3 .AND. L == 1)  THEN

            DO SPC = 1, NUMB_MECH_SPC

               S = CGRID_INDEX( SPC )
               print*, 'RBDRIVER: AFTER C2R SPC= ', S,
! N_GAS_CHEM_SPC = 127 (NO2 = 1 O3 = 4)
!    &         CONC_CAN(C,R,1, S), CONC_CAN(C,R,2, S), CONC_CAN(C,R,3, S)  ! O3 = 4
     &         CONC    (C,R,1, S), CONC_MOD(C,R,1, S)
            END DO

            END IF ! KOUNT L=1
            END IF !.FALSE.
c++++++++++++++++++++++++Debug section++++++++++++++++++++++++++++++++++

! Print up to KOUNT number of canopy columns
            KOUNT = KOUNT + 1

! Update CGRID with values from canopy_transfer
            DO SPC = 1, NUMB_MECH_SPC
               S = CGRID_INDEX( SPC )

!!! Udate CONC with C2R mass transfer values from canopy_transfer(1)
! All levels
               CONC(C,R,L,S) = CONC_MOD(C,R,L,S)
!!! Udate CONC with C2R mass transfer values from canopy_transfer(1)

            END DO

            END IF !FRT_MASK

            END DO
         END DO
      END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      END IF ! CANOPY_SHADE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!IVAI

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  End of block loop; reorder cells if necessary and go back do the  
C  block loop again.  Taken from Jacobson 1994.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF ( LORDERING ) THEN
         LORDERING = .FALSE.     
         LVAL = NOXYZ / 2 + 1
         IRVAL = NOXYZ
600      CONTINUE
         IF ( LVAL .GT. 1 ) THEN
            LVAL = LVAL - 1
            VALLOW = ERRMX2( LVAL )
            JREORD = NORDCELL( LVAL )
         ELSE
            VALLOW = ERRMX2( IRVAL )
            JREORD = NORDCELL( IRVAL )
            ERRMX2( IRVAL ) = ERRMX2( 1 )
            NORDCELL( IRVAL ) = NORDCELL( 1 )
            IRVAL = IRVAL - 1
            IF ( IRVAL.EQ.1 ) THEN
               ERRMX2( IRVAL ) = VALLOW
               NORDCELL( IRVAL ) = JREORD
               GO TO 100
            END IF
         END IF
         IPAR = LVAL
         JPAR = LVAL + LVAL
650      CONTINUE
         IF ( JPAR .LE. IRVAL ) THEN
            IF ( JPAR .LT. IRVAL ) THEN
               IF ( ERRMX2( JPAR ) .LT. ERRMX2( JPAR + 1 ) ) JPAR = JPAR + 1
            END IF
            IF ( VALLOW .LT. ERRMX2( JPAR )) THEN
               ERRMX2( IPAR ) = ERRMX2( JPAR )
               NORDCELL( IPAR ) = NORDCELL( JPAR )
               IPAR = JPAR
               JPAR = JPAR + JPAR
            ELSE
               JPAR = IRVAL + 1
            END IF
            GO TO 650
         END IF
         ERRMX2( IPAR ) = VALLOW
         NORDCELL( IPAR ) = JREORD
         GO TO 600

!IVAI: Repeat on canopy columns "reorder cells if necessary..."
      IF (CANOPY_SHADE) THEN

         LVAL = NOXYZ_CAN / 2 + 1
         IRVAL = NOXYZ_CAN
700      CONTINUE
         IF ( LVAL .GT. 1 ) THEN
            LVAL = LVAL - 1
            VALLOW = ERRMX2_CAN( LVAL )
            JREORD = NORDCELL_CAN( LVAL )
         ELSE
            VALLOW = ERRMX2_CAN( IRVAL )
            JREORD = NORDCELL_CAN( IRVAL )
            ERRMX2_CAN( IRVAL ) = ERRMX2_CAN( 1 )
            NORDCELL_CAN( IRVAL ) = NORDCELL_CAN( 1 )
            IRVAL = IRVAL - 1
            IF ( IRVAL.EQ.1 ) THEN
               ERRMX2_CAN( IRVAL ) = VALLOW
               NORDCELL_CAN( IRVAL ) = JREORD
               GO TO 200
            END IF
         END IF
         IPAR = LVAL
         JPAR = LVAL + LVAL
750      CONTINUE
         IF ( JPAR .LE. IRVAL ) THEN
            IF ( JPAR .LT. IRVAL ) THEN
               IF ( ERRMX2_CAN( JPAR ) .LT. ERRMX2_CAN( JPAR + 1 ) ) JPAR = JPAR + 1
            END IF
            IF ( VALLOW .LT. ERRMX2_CAN( JPAR )) THEN
               ERRMX2_CAN( IPAR ) = ERRMX2_CAN( JPAR )
               NORDCELL_CAN( IPAR ) = NORDCELL_CAN( JPAR )
               IPAR = JPAR
               JPAR = JPAR + JPAR
            ELSE
               JPAR = IRVAL + 1
            END IF
            GO TO 750
         END IF
         ERRMX2_CAN( IPAR ) = VALLOW
         NORDCELL_CAN( IPAR ) = JREORD
         GO TO 700
      END IF ! (CANOPY_SHADE)
!IVAI
      END IF ! (LORDERING)
       
      !Update Aerosol Surface Area
      CALL HETCHEM_UPDATE_AERO( CONC )

!IVAI: to do...
      !Update Aerosol Surface Area
!     CALL HETCHEM_UPDATE_AERO( CONC_CAN )

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Output performance statistics if required and return
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

#ifdef rbstats

      WSTEP = WSTEP + TIME2SEC( TSTEP( 2 ) )
      EDATE = JDATE
      ETIME = JTIME
      CALL NEXTIME( EDATE, ETIME, TSTEP( 2 ) )
      IF ( WSTEP .GE. TIME2SEC( TSTEP( 1 ) ) ) THEN

         WSTEP = 0

         DO S = 1, 3
            DO R = 1, MY_NROWS
               DO C = 1, MY_NCOLS
                  DO L = 1, NLAYS
                     STATOUT( C, R, L ) = INT( STAT_SUM( C,R,L,S )
     &                                  +  0.00001 )
                  END DO
               END DO
            END DO

            IF ( .NOT. WRITE3( CTM_RBSTATS_1, VSTATS( S ),
     &            EDATE, ETIME, STATOUT ) ) THEN
               XMSG = 'Could not write ' // CTM_RBSTATS_1 // ' file'
               CALL M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            END IF  

         END DO

         STAT_SUM = 0.0

      END IF
       
#endif


      IF( FIRSTCALL )FIRSTCALL = .FALSE.
      
      RETURN

C*********************** FORMAT STATEMENTS ****************************
92020 FORMAT( / 10X, 'Chemistry Solver Blocking Parameters ... ',
     &        / 10X, 'Domain Size (number of cells):             ', I10
     &        / 10X, 'Dimensioning Block Size (number of cells): ', I10
     &        / 10X, 'Number of Blocks:        ', I10
     &        / 10X, 'Size of General Blocks:  ', I10
     &        / 10X, 'Size of Last Block:      ', I10 )
92040 FORMAT( / 10X, 'Rosenbrock Chemistry Solver Error Control ',
     &               'Parameters ...',
     &        / 10X, 'RTOL : ', 1PE12.3,
     &        / 10X, 'ATOL : ', 1PE12.3, ' ppm' )

      END

