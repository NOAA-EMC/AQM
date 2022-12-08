      MODULE PT3D_EMIS_DEFN

      USE GRID_CONF           ! horizontal & vertical domain specifications      
      USE EMIS_DEFN
      USE AQM_EMIS_MOD, ONLY : AQM_EMIS_DESC, AQM_EMIS_GET,
     &                         AQM_EMIS_READ, AQM_INTERNAL_EMIS_TYPE

      IMPLICIT NONE

      INTEGER :: N_GSPC_EMIS
      INTEGER :: N_SPC_PT3DEM
      INTEGER :: N_SPC_PT3DPM

      INTEGER, ALLOCATABLE :: SPC_PT3DEM_FAC( : )
      INTEGER, ALLOCATABLE :: SPC_PT3DEM_MAP( : )
      INTEGER, ALLOCATABLE :: PT3DEM_MAP( : )
      INTEGER, ALLOCATABLE :: PT3DPM_MAP( : )

      REAL :: CNVTP         ! intermediate combined conv.  factor

C Emission type
      CHARACTER( * ), PARAMETER :: ETYPE = 'point-source'

      PRIVATE

      PUBLIC :: GET_PT3D_EMIS

      CONTAINS

         SUBROUTINE GET_PT3D_EMIS

         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN
         USE AQM_RC_MOD
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA, CONVPA
         USE EMIS_DEFN, ONLY: VDEMIS

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters
         INCLUDE SUBST_CONST

C Arguments:
c        NONE

C Local Variables:
         REAL              :: CNVTC         ! combined conversion factor
         REAL              :: CNVTE         ! combined conversion factor
         REAL              :: CNVTI         ! intermediate combined conv. factor
         REAL              :: CONVEM        ! emission conversion factor
         INTEGER           :: C, R, L, N, S, V, K, I ! loop induction variables
         INTEGER           :: IJ, IS
         INTEGER           :: NSRC, NP
         INTEGER           :: S_STRT, S_END ! substitute loop induction variables
         INTEGER           :: IOS, LOCALRC
         INTEGER           :: P( 2 )
         INTEGER, POINTER  :: IP( : )
         INTEGER, POINTER  :: JP( : )
         INTEGER, POINTER  :: IJMAP( : )
         REAL              :: DX1, DX2
         REAL, ALLOCATABLE :: BUFFER( : )

C Parameters:
         REAL, PARAMETER :: CMLMR = 1.0E+06       ! ppmV/Molar Mixing Ratio
         CHARACTER( 16 ) :: PNAME = 'GET_PT3D_EMIS'
         CHARACTER( 120 ) :: XMSG = ' '

         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER, SAVE :: EMLYRS

         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM

C-----------------------------------------------------------------------

         EM => AQM_EMIS_GET( ETYPE )
         IF ( .NOT.ASSOCIATED( EM ) ) RETURN

         IF (EM % COUNT == 0) RETURN

         IF ( FIRSTIME ) THEN

C Retrieve grid information
            IF ( GDTYP_GD .EQ. LATGRD3 ) THEN
               DX1 = DG2M * XCELL_GD ! in m.
               DX2 = DG2M * YCELL_GD
     &             * COS( PI180*( YORIG_GD + YCELL_GD*FLOAT( GL_NROWS/2)))! in m.
            ELSE
               DX1 = XCELL_GD        ! in m.
               DX2 = YCELL_GD        ! in m.
            END IF

C Get emission conversion factor from area emissions file
C           CALL OPEMIS ( JDATE, JTIME, N_GC_EMIS, EM_TRAC, CONVEM, EMLAYS )
C ... or ...
            CONVEM = 1.0E-03  ! assuming g-mol

            CNVTE = CMLMR * CONVEM * MWAIR / ( DX1 * DX2 )
            CNVTP = CONVPA * CNVTE

C set number of emissions layers depending on whether plumerise is on

            CALL AQM_EMIS_DESC( ETYPE, NLAYS=EMLYRS )

C get point source emission mapping
            IF ( AQM_RC_TEST( .NOT. PT3DEM_MAP_INIT( ),
     &              MSG="Failure initializing mapping for" //
     &              TRIM( ETYPE ) // " emissions",
     &              FILE=__FILE__, LINE=__LINE__ ) ) RETURN

            FIRSTIME = .FALSE.
         END IF

C Allocate Buffer space for Reading Emissions
         ALLOCATE ( BUFFER( SIZE( EM % LAT ) ), STAT = IOS )
         CALL CHECKMEM( IOS, 'BUFFER', PNAME )

         NSRC = SIZE( EM % IJMAP )

         ! -- loop over mapped emissions only

         ! -- (1) non-PM emissions
         DO S = 1, N_GSPC_EMIS
            V = SPC_PT3DEM_FAC( S )
            IF ( V .LT. 1 ) CYCLE
            N = PT3DEM_MAP( S )
            IF ( N .LT. 1 ) CYCLE

            CNVTI = CNVTP * SPC_PT3DEM_FAC( S )

            BUFFER = 0.0
            CALL AQM_EMIS_READ( ETYPE, EM % TABLE( N, 1 ),
     &         BUFFER, IP, JP, IJMAP, RC=LOCALRC)
            IF ( AQM_RC_CHECK( LOCALRC,
     &         MSG="Failure while reading " //
     &         TRIM( EM % TABLE( N, 1 ) ) // " from " //
     &         TRIM( ETYPE ) // " emissions",
     &         FILE=__FILE__, LINE=__LINE__ ) ) RETURN
            IF ( EM % TABLE( N, 1 ) .EQ. 'POC' ) THEN
               CALL AQM_EMIS_READ( ETYPE, 'PNCOM',
     &            BUFFER, IP, JP, IJMAP, RC=LOCALRC)
               IF ( AQM_RC_CHECK( LOCALRC,
     &            MSG="Failure while reading PNCOM emissions from" //
     &            TRIM( ETYPE ) // " emissions",
     &         FILE=__FILE__, LINE=__LINE__ ) ) RETURN
            END IF

            DO IS = 1, NSRC
               IJ = EM % IJMAP( IS )
               C  = EM % IP( IJ )
               R  = EM % JP( IJ )

               ! -- compute plume rise
               ! ...

               ! -- add emissions
               DO L = 1, EMLYRS
                  CNVTC = CNVTI * Grid_Data%RDX3F( L ) * Met_Data%RRHOJ( C,R,L )
                  VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                              + CNVTC * BUFFER( IJ )
               END DO
            END DO
         END DO

         ! -- (2) PM emissions (TBD)

         DEALLOCATE ( BUFFER )

         END SUBROUTINE GET_PT3D_EMIS


         FUNCTION PT3DEM_MAP_INIT( ) RESULT ( SUCCESS )

         USE CGRID_SPCS                                    ! CGRID mechanism species
         USE AERO_DATA, ONLY : N_EMIS_PM, PMEM_MAP_NAME    ! defines aerosol species
         USE AQM_EMIS_MOD, ONLY : AQM_EMIS_GET, AQM_INTERNAL_EMIS_TYPE
         USE UTILIO_DEFN

         IMPLICIT NONE

         LOGICAL SUCCESS

         INTEGER, EXTERNAL :: SETUP_LOGDEV

         CHARACTER( 16 )  :: PNAME = 'PT3DEM_MAP'  ! procedure name

         INTEGER IOS, LOGDEV

         INTEGER IDX
         INTEGER N, N_GAS_EMIS, NSPC, NSPC1, NSPC2, NSPC3
         INTEGER S, S_OFFSET, V

         INTEGER, ALLOCATABLE :: MAP( : )

         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM

C-----------------------------------------------------------------------

         LOGDEV = SETUP_LOGDEV()

         SUCCESS = .TRUE.

         N_GSPC_EMIS = 0

C check if emissions are being provided

         EM => AQM_EMIS_GET( ETYPE )
         IF ( .NOT.ASSOCIATED( EM ) ) RETURN

C compute emission offsets

         NSPC1 = N_GC_EMIS
         NSPC2 = NSPC1 + N_AE_EMIS
         NSPC3 = NSPC2 + N_NR_EMIS

C create auxiliary arrays mapping fire emission species to CMAQ gas and aerosol species

         NSPC = SIZE( EM % TABLE, DIM=1 )

         ALLOCATE( MAP( NSPC ), STAT = IOS )
         CALL CHECKMEM( IOS, 'MAP', PNAME )
         MAP = 0

C        ... gas species ...        

         NSPC1 = N_GC_EMIS
         NSPC2 = NSPC1 + N_AE_EMIS
         NSPC3 = NSPC2 + N_NR_EMIS

         N_GAS_EMIS = N_GC_EMIS + N_NR_EMIS + N_TR_EMIS

         ALLOCATE( SPC_PT3DEM_FAC( N_GAS_EMIS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'SPC_PT3DEM_FAC', PNAME )
         SPC_PT3DEM_FAC = 1.0

         ALLOCATE( SPC_PT3DEM_MAP( N_GAS_EMIS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'SPC_PT3DEM_MAP', PNAME )
         SPC_PT3DEM_MAP = -1

         ALLOCATE( PT3DEM_MAP( N_GAS_EMIS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'PT3DEM_MAP', PNAME )
         PT3DEM_MAP = -1

         S_OFFSET = 0
         DO S = 1, N_GC_EMIS
           IDX = INDEX1( GC_EMIS( S ), NSPC, EM % TABLE( 1, 1 ) )
           IF ( IDX .GT. 0 ) THEN
              PT3DEM_MAP    ( S ) = IDX
              SPC_PT3DEM_MAP( S ) = S
              SPC_PT3DEM_FAC( S ) = GC_EMIS_FAC( S )
           END IF
         END DO

         S_OFFSET = N_GC_EMIS
         DO S = 1, N_NR_EMIS
           IDX = INDEX1( NR_EMIS( S ), NSPC, EM % TABLE( 1, 1 ) )
           IF ( IDX .GT. 0 ) THEN
              V = S + S_OFFSET
              PT3DEM_MAP    ( V ) = IDX
              SPC_PT3DEM_MAP( V ) = S + NSPC2
              SPC_PT3DEM_FAC( V ) = NR_EMIS_FAC( S )
           END IF
         END DO

         S_OFFSET = S_OFFSET + N_NR_EMIS
         DO S = 1, N_TR_EMIS
           IDX = INDEX1( TR_EMIS( S ), NSPC, EM % TABLE( 1, 1 ) )
           IF ( IDX .GT. 0 ) THEN
              V = S + S_OFFSET
              PT3DEM_MAP    ( V ) = IDX
              SPC_PT3DEM_MAP( V ) = S + NSPC3
              SPC_PT3DEM_FAC( V ) = TR_EMIS_FAC( S )
           END IF
         END DO

         N_GSPC_EMIS = N_GAS_EMIS

C        ... aerosol species ...        

         N = 0
         DO S = 1, NSPC
            IDX = INDEX1( EM % TABLE( S, 1 ), N_EMIS_PM, PMEM_MAP_NAME )
            IF ( IDX .GT. 0 ) THEN
               N = N + 1 
               MAP( N ) = IDX
            END IF
         END DO

         N_SPC_PT3DEM = NSPC
         N_SPC_PT3DPM = N

         ALLOCATE( PT3DPM_MAP( N_SPC_PT3DPM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'PT3DPM_MAP', PNAME )
         PT3DPM_MAP = MAP( 1:N_SPC_PT3DPM )

         DEALLOCATE( MAP )

         END FUNCTION PT3DEM_MAP_INIT

      END MODULE PT3D_EMIS_DEFN
