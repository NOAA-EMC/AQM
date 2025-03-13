      MODULE PT3D_FIRE_DEFN

      ! USE PT3D_DATA_MOD

      IMPLICIT NONE


C Emission layers for sources within domain
      INTEGER :: EMLYRS       ! no. of emis layers


      CHARACTER( 240 )           :: XMSG = ' '

C Emission type
      CHARACTER( * ),  PARAMETER :: ETYPE = 'gbbepx'

C Species name for Fire Radiative Power
      CHARACTER( * ),  PARAMETER :: EMFRP = 'FRP'

      PRIVATE

      PUBLIC  GET_PT3D_FIRE_EMIS

      CONTAINS

C=======================================================================


         SUBROUTINE GET_PT3D_FIRE_EMIS ( JDATE, JTIME, EMVAR_PT, ISRM, 
     &                                VDEMIS_PT, PTLAYS )

! Revision History.
!     Oct 20, 23 Wei Li: change to use DESID mapping

C-----------------------------------------------------------------------

C Time step part of laypoint

         USE AQM_EMIS_MOD
         USE AQM_FIRES_MOD
         USE AQM_RC_MOD
         USE RXNS_DATA, ONLY : MECHNAME !Get Chemical Mechanism Name
         USE GRID_CONF   ! horizontal & vertical domain specifications
         USE CGRID_SPCS  ! CGRID mechanism species
         USE DESID_VARS, ONLY :  DESID_N_ISTR
         USE UTILIO_DEFN
         USE RUNTIME_VARS, only: LOGDEV

         IMPLICIT NONE

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME
         CHARACTER(16), INTENT( IN ) :: EMVAR_PT( : )
         INTEGER, INTENT( IN ) :: ISRM
         INTEGER, INTENT( OUT) :: PTLAYS
         REAL, INTENT(INOUT)  :: VDEMIS_PT( :,:,:,: )
C Parameters:

C External functions:
C         INTEGER,         EXTERNAL :: SETUP_LOGDEV

C Local variables:
         CHARACTER( 16 ) :: PNAME = 'GET_PT3D_EMIS   '   ! procedure name
         CHARACTER( 16 ) :: VNAME    ! variable name buffer

         INTEGER          IOS        ! i/o and allocate memory status
         INTEGER          L, S, V    ! counters
         INTEGER          C, R, K, M, N, NSPC
         INTEGER          LOCALRC

         REAL,    ALLOCATABLE :: VFRAC( :,:,: ) ! vertical fraction
         REAL,    ALLOCATABLE :: BUFFER( : ) ! emission buffe

         LOGICAL       :: IS_NOT_NVPOA, SAVE_POC

         LOGICAL, SAVE :: FIRSTIME = .TRUE.
C         INTEGER, SAVE :: LOGDEV

         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM

C-----------------------------------------------------------------------

C         IF ( FIRSTIME ) THEN
C            FIRSTIME = .FALSE.
C           LOGDEV = SETUP_LOGDEV()
C         END IF

         NULLIFY(EM)   
         EM => AQM_EMIS_GET( ETYPE )
         IF ( .NOT.ASSOCIATED( EM ) ) RETURN
         !number of species in this emission file
         NSPC = SIZE( EM % TABLE, DIM=1 )
         CALL AQM_EMIS_DESC( ETYPE, NLAYS=EMLYRS )
         PTLAYS = EMLYRS
C For each time step, compute the layer fractions...

         WRITE( XMSG,'(A, I7.6)' )
     &   'Calculating emissions point source layer fractions for',JTIME
         WRITE( LOGDEV,* ) ' '
         CALL M3MSG2( XMSG )

         ALLOCATE ( VFRAC( NCOLS,NROWS,EMLYRS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VFRAC', PNAME )

         ALLOCATE ( BUFFER( NCOLS * NROWS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'BUFFER', PNAME )
         BUFFER = 0.0   ! array

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

         DO S = 1, DESID_N_ISTR
            VNAME = EMVAR_PT( S )
            IF ( VNAME .EQ. '' ) CYCLE
            M = INDEX1( VNAME, NSPC, EM % TABLE( :, 1 ) )

            IF ( M .GT. 0 ) THEN
               BUFFER = 0.0
               CALL AQM_EMIS_READ( ETYPE, EM % TABLE( M, 1 ),
     &                             BUFFER, RC=LOCALRC )
               IF ( AQM_RC_CHECK( LOCALRC, MSG="Failure while reading "
     &            // TRIM( EM % TABLE( M, 1 ) ) // " from "
     &            // TRIM( ETYPE ) // " emissions",
     &            FILE=__FILE__, LINE=__LINE__)) RETURN
C              Read Non-Carbon Organic Matter too if POC is Requested
C               SAVE_POC = .FALSE.
C               IF ( IS_NOT_NVPOA .AND. EM % TABLE( M, 1 ) .EQ. 'POC' ) THEN
C                  CALL AQM_EMIS_READ( ETYPE, 'PNCOM', BUFFER, RC=LOCALRC )
C                  IF ( AQM_RC_CHECK( LOCALRC,
C     &               MSG="Failure while reading PNCOM" //
C     &               " from " // TRIM( ETYPE ) // " emissions",
C     &               FILE=__FILE__, LINE=__LINE__)) RETURN
C                  SAVE_POC = IS_NOT_NVPOA
C               END IF
               DO L = 1, PTLAYS
                  K = 0
                  DO R = 1, NROWS
                     DO C = 1, NCOLS
                        K = K + 1
                        VDEMIS_PT(S,L,C,R) = VFRAC( C,R,L ) * BUFFER( K )
                     END DO
                  END DO
               END DO
              ! IF ( SAVE_POC ) THEN    !comment out this for now 
              !    DO L = 1, PTLAYS
              !       K = 0
              !       DO R = 1, NROWS
              !          DO C = 1, NCOLS
              !             K = K + 1
              !             VDEMIS_PT(S,L,C,R) = VDEMIS_PT(S,L,C,R) + VDEMIS_PT( S,L,C,R )
              !          END DO
              !       END DO
              !    END DO
              ! END IF
            END IF
         END DO
         ! DEALLOCATE temporary variables
         DEALLOCATE ( BUFFER, VFRAC )

         RETURN

         END SUBROUTINE GET_PT3D_FIRE_EMIS

      END MODULE PT3D_FIRE_DEFN
