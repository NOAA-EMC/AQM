      MODULE PT3D_DEFN

      IMPLICIT NONE

      LOGICAL,              SAVE :: PT3DEMIS   ! flag in-lining plume rise
      INTEGER,              SAVE :: NPTGRPS    ! no. pt src input file groups
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_PT( :,:,:,: ) ! 3D pt src non-PM emis
      REAL,    ALLOCATABLE, SAVE :: VDEMIS_PT_FIRE( :,:,:,: ) ! 3D pt src non-PM emis
      REAL,    ALLOCATABLE, SAVE :: PMEMIS_PT( :,:,:,: ) ! 3D pt src PM emis
      REAL,    ALLOCATABLE, SAVE :: VFRAC( :,:,: ) ! vertical fraction
      REAL,    ALLOCATABLE, SAVE :: BUFFER( : ) ! emission buffer

      PUBLIC PT3DEMIS, NPTGRPS, VDEMIS_PT, VDEMIS_PT_FIRE, PMEMIS_PT,
     &       PT3D_INIT, GET_PT3D_EMIS
      PRIVATE

C Stack emissions for sources within domain
      INTEGER,              SAVE :: EMLYRS       ! no. of emis layers
      INTEGER,              SAVE :: PM_EMLYRS    ! no. of emis layers for PM

C Gas-phase Namelist emissions factors
      REAL,    ALLOCATABLE, SAVE :: EMIS_FAC( : )  ! per species scaling factor

      CHARACTER( 240 )           :: XMSG = ' '

C Emission type
      CHARACTER( * ),  PARAMETER :: ETYPE = 'gbbepx'

C Species name for Fire Radiative Power
      CHARACTER( * ),  PARAMETER :: EMFRP = 'FRP'

      CONTAINS

C=======================================================================

         FUNCTION PT3D_INIT ( N_SPC_EMIS, EMLAYS, JDATE, JTIME, TSTEP )
     &                       RESULT ( SUCCESS )

         USE AQM_EMIS_MOD
         USE GRID_CONF                  ! horizontal & vertical domain specifications
         USE CGRID_SPCS                 ! CGRID mechanism species
         USE STK_EMIS, ONLY : STKSPC    ! hourly point source emissions
         USE PTMAP                      ! defines pt src species mapping to VDEMIS* arrays
         USE UTILIO_DEFN

         IMPLICIT NONE

C Includes:
C        INCLUDE SUBST_FILES_ID  ! file name parameters (for CTM_PT3D_DIAG)

C Arguments:
         INTEGER, INTENT( IN ) :: N_SPC_EMIS ! total no. of model emissions species
         INTEGER, INTENT( IN ) :: EMLAYS     ! number of emissions layers
         INTEGER, INTENT( IN ) :: JDATE      ! Julian date (YYYYDDD)
         INTEGER, INTENT( IN ) :: JTIME      ! time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP      ! output time step
         LOGICAL          SUCCESS

C External Functions:
         INTEGER,         EXTERNAL :: SETUP_LOGDEV

C Parameters:

C Local Variables:
         CHARACTER( 16 ), SAVE :: CTM_PT3DEMIS = 'CTM_PT3DEMIS    ' ! env var for in-line
                                                                    ! 3d pt src emis
         CHARACTER( 16 )       :: PNAME = 'PT3D_INIT       ' ! procedure name
         CHARACTER( 16 )       :: VNAME    ! variable name buffer
         CHARACTER( 16 ), SAVE, ALLOCATABLE :: STKGNAME( : ) ! stack groups file name

         INTEGER          IOS            ! i/o and allocate memory status

         INTEGER          LOGDEV

         INTEGER IDX
         INTEGER N, NSPC, NSPC1, NSPC2, NSPC3
         INTEGER S, S_OFFSET, V

         INTEGER, ALLOCATABLE :: MAP( : )

         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         LOGDEV = SETUP_LOGDEV()

C In-line 3D point source emissions?

         PT3DEMIS = ENVYN( 'CTM_PT3DEMIS',
     &                   'Flag for in-line 3d point source emissions',
     &                   .FALSE., IOS )

         IF ( PT3DEMIS ) THEN
            XMSG = 'Using in-line 3d point source emissions option'
            CALL M3MSG2( XMSG )
         ELSE
            RETURN
         END IF

C check if emissions are being provided

         EM => AQM_EMIS_GET( ETYPE )
         IF ( .NOT.ASSOCIATED( EM ) ) RETURN

C get number of different file groups (sectors)

         NPTGRPS = 0

C create point source internal array

         ALLOCATE( STKSPC( NPTGRPS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'STKSPC', PNAME )

C set number of emissions layers depending on whether plumerise is on

         SELECT CASE ( TRIM( EM % PLUMERISE ) )
            CASE ("sofiev")
               EMLYRS    = NLAYS
               PM_EMLYRS = NLAYS
            CASE DEFAULT
               EMLYRS    = 1
               PM_EMLYRS = 1
         END SELECT
         
C get point source emission mapping

         IF ( .NOT. PTMAP_INIT( ) ) THEN
            XMSG = 'Could not get point source mappings'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

C allocate emission arrays

         ALLOCATE ( VDEMIS_PT( NCOLS,NROWS,EMLYRS,N_SPC_PTEM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VDEMIS_PT', PNAME )
         VDEMIS_PT = 0.0   ! array assignment

         ALLOCATE ( VDEMIS_PT_FIRE( NCOLS,NROWS,EMLYRS,N_SPC_PTEM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VDEMIS_PT_FIRE', PNAME )
         VDEMIS_PT_FIRE = 0.0   ! array assignment

         ALLOCATE ( PMEMIS_PT( NCOLS,NROWS,EMLYRS,N_SPC_PTPM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'PMEMIS_PT', PNAME )
         PMEMIS_PT = 0.0   ! array

         ALLOCATE ( VFRAC( NCOLS,NROWS,EMLYRS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VFRAC', PNAME )
         VFRAC = 1.0   ! array

         ALLOCATE ( BUFFER( NCOLS * NROWS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'BUFFER', PNAME )
         BUFFER = 0.0   ! array

         SUCCESS = .TRUE.; RETURN

         END FUNCTION PT3D_INIT

C=======================================================================

         SUBROUTINE GET_PT3D_EMIS ( JDATE, JTIME, TSTEP )

! Revision History.
!     Aug 12, 15 D. Wong: added code to handle parallel I/O implementation

C-----------------------------------------------------------------------

C Time step part of laypoint

         USE AQM_EMIS_MOD
         USE AQM_FIRES_MOD
         USE AQM_RC_MOD
         USE RXNS_DATA, ONLY : MECHNAME !Get Chemical Mechanism Name
         USE GRID_CONF   ! horizontal & vertical domain specifications
         USE CGRID_SPCS  ! CGRID mechanism species
         USE AERO_DATA, ONLY : PMEM_MAP_NAME
         USE PTMAP       ! defines pt src species mapping to VDEMIS* arrays
         USE UTILIO_DEFN

         IMPLICIT NONE

C Includes:
C        INCLUDE SUBST_CONST     ! physical and mathematical constants
C        INCLUDE SUBST_FILES_ID  ! file name parameters (for CTM_PT3D_DIAG)

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME
         INTEGER, INTENT( IN ) :: TSTEP( 3 )

C Parameters:

C External functions:
         INTEGER,         EXTERNAL :: SETUP_LOGDEV

C Local variables:
         CHARACTER( 16 ) :: PNAME = 'GET_PT3D_EMIS   '   ! procedure name
         CHARACTER( 16 ) :: VNAME    ! variable name buffer

         INTEGER          IOS        ! i/o and allocate memory status
         INTEGER          L, S, V    ! counters
         INTEGER          C, R, K, N
         INTEGER          LOCALRC

         LOGICAL       :: IS_NOT_NVPOA, SAVE_POC

         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER, SAVE :: LOGDEV

         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM

C-----------------------------------------------------------------------

         IF ( FIRSTIME ) THEN
            FIRSTIME = .FALSE.
            LOGDEV = SETUP_LOGDEV()
         END IF

         EM => AQM_EMIS_GET( ETYPE )
         IF ( .NOT.ASSOCIATED( EM ) ) RETURN

C For each time step, compute the layer fractions...

         WRITE( XMSG,'(A, I7.6)' )
     &   'Calculating emissions point source layer fractions for', JTIME
         WRITE( LOGDEV,* ) ' '
         CALL M3MSG2( XMSG )

C        ... initialize emission arrays ...

         VDEMIS_PT      = 0.0   ! array assignment
         VDEMIS_PT_FIRE = 0.0   ! array assignment
         PMEMIS_PT      = 0.0   ! array assignment

C        ... initialize vertical fraction arrays ...
C        ... fire emissions are added to surface only by default ...

         VFRAC        = 0.0
         VFRAC(:,:,1) = 1.0

C Retrieve fire emissions and distribute according to plume-rise algorithm

C        ... plumerise   ...        

         SELECT CASE ( TRIM( EM % PLUMERISE ) )
            CASE ( "sofiev" )
               N = INDEX1( EMFRP, SIZE( EM % SPECIES ), EM % SPECIES )
               IF ( N > 0 ) THEN
C                 read in frp
                  BUFFER = 0.0
                  CALL AQM_EMIS_READ( ETYPE, EMFRP, BUFFER, RC=LOCALRC )
                  IF ( AQM_RC_CHECK( LOCALRC,
     &               MSG="failure while reading frp from " //
     &               TRIM( ETYPE ) // " emissions",
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
                  CALL AQM_PLUME_SOFIEV( EM, BUFFER, VFRAC, RC=LOCALRC )
                  IF ( AQM_RC_CHECK( LOCALRC,
     &               MSG="failed to compute plume rise",
     &               FILE=__FILE__, LINE=__LINE__ ) ) RETURN
               ELSE
                  CALL M3WARN( PNAME, JDATE, JTIME,
     &               EMFRP // "species not found. " //
     &               "Adding fire emissions to surface only" )
               END IF
            CASE ( "none" )
C                no plume rise
            CASE DEFAULT
C                plume rise is disabled by default
         END SELECT
         
C        ... gas species ...        

         IS_NOT_NVPOA = ( INDEX( MECHNAME, 'NVPOA' ) .EQ. 0 )

         DO S = 1, N_GSPC_EMIS
            N = PTEM_MAP( S )
            IF ( N .GT. 0 ) THEN
               BUFFER = 0.0
               CALL AQM_EMIS_READ( ETYPE, EM % TABLE( N, 1 ), BUFFER, RC=LOCALRC )
               IF ( AQM_RC_CHECK( LOCALRC, MSG="Failure while reading " //
     &            TRIM( EM % TABLE( N, 1 ) ) // " from " // TRIM( ETYPE ) // " emissions",
     &            FILE=__FILE__, LINE=__LINE__)) RETURN
C              Read Non-Carbon Organic Matter too if POC is Requested
               SAVE_POC = .FALSE.
               IF ( IS_NOT_NVPOA .AND. EM % TABLE( N, 1 ) .EQ. 'POC' ) THEN
                  CALL AQM_EMIS_READ( ETYPE, 'PNCOM', BUFFER, RC=LOCALRC )
                  IF ( AQM_RC_CHECK( LOCALRC,
     &               MSG="Failure while reading PNCOM" //
     &               " from " // TRIM( ETYPE ) // " emissions",
     &               FILE=__FILE__, LINE=__LINE__)) RETURN
                  SAVE_POC = IS_NOT_NVPOA
               END IF
               DO L = 1, EMLYRS
                  K = 0
                  DO R = 1, MY_NROWS
                     DO C = 1, MY_NCOLS
                        K = K + 1
                        VDEMIS_PT( C,R,L,N ) = VFRAC( C,R,L ) * BUFFER( K )
                     END DO
                  END DO
               END DO
               IF ( SAVE_POC ) THEN
                  DO L = 1, EMLYRS
                     K = 0
                     DO R = 1, MY_NROWS
                        DO C = 1, MY_NCOLS
                           K = K + 1
                           VDEMIS_PT_FIRE( C,R,L,N ) = VDEMIS_PT( C,R,L,N )
                        END DO
                     END DO
                  END DO
               END IF
            END IF
         END DO

C        ... aerosol species ...        

         DO S = 1, N_SPC_PTPM
            V = PTPM_MAP( S )
            BUFFER = 0.0
            CALL AQM_EMIS_READ( ETYPE, PMEM_MAP_NAME( V ), BUFFER, RC=LOCALRC )
            IF ( AQM_RC_CHECK( LOCALRC, MSG="Failure while reading " //
     &         TRIM( PMEM_MAP_NAME( V ) ) // " from " // TRIM( ETYPE ) // " emissions",
     &         FILE=__FILE__, LINE=__LINE__)) RETURN
            DO L = 1, PM_EMLYRS
               K = 0
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     K = K + 1
                     PMEMIS_PT( C,R,L,S ) = VFRAC( C,R,L ) * BUFFER( K )
                  END DO
               END DO
            END DO
         END DO
         
         RETURN

         END SUBROUTINE GET_PT3D_EMIS

      END MODULE PT3D_DEFN
