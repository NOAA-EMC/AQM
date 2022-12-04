      MODULE PT3D_EMIS_DEFN

      USE GRID_CONF           ! horizontal & vertical domain specifications      
      USE EMIS_DEFN

      IMPLICIT NONE

      INTEGER :: NSPC

C Emission type
      CHARACTER( * ), PARAMETER :: ETYPE = 'point-source'

      PRIVATE

      PUBLIC :: GET_PT3D_EMIS

      CONTAINS

         SUBROUTINE GET_PT3D_EMIS ( JDATE, JTIME, TSTEP, CONVPA, CGRID )

         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN
         USE DEPV_DEFN, ONLY: ABFLUX ! ammonia bidi flag
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA
         USE RXNS_DATA, ONLY: MECHNAME

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE, JTIME  ! date (YYYYDDD), time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP( 3 )    ! time step vector (HHMMSS)
         REAL,    INTENT( IN ) :: CONVPA        ! conversion factor to pressure in Pascals
         REAL, POINTER :: CGRID( :,:,:,: )

C Local Variables:
         REAL             CNVTC         ! combined conversion factor
         REAL             CNVTI         ! intermediate combined conv. factor
         REAL,    SAVE :: CNVTP         ! intermediate combined conv. factor
         REAL             DELT          ! interpolation factor
         INTEGER          C, R, L, N, S, V ! loop induction variables
         INTEGER          S_STRT, S_END ! substitute loop induction variables
         INTEGER, POINTER :: IP( : )
         INTEGER, POINTER :: JP( : )

         CHARACTER( 16 ) :: PNAME = 'GET_PT3D_EMIS'
         CHARACTER( 120 ) :: XMSG = ' '

         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER, SAVE :: STARTCOL, ENDCOL, STARTROW, ENDROW
         INTEGER, SAVE :: EMLYRS

         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM

C-----------------------------------------------------------------------

         EM => AQM_EMIS_GET( ETYPE )
         IF ( .NOT.ASSOCIATED( EM ) ) RETURN


         IF ( FIRSTTIME ) THEN

C Get domain decomp info from the emissions file
!           CALL SUBHFILE ( EMIS_1, GXOFF, GYOFF, STARTCOL, ENDCOL, STARTROW, ENDROW )
            CNVTP = CONVPA * CNVTE

C Set emission layers
            EMLYRS = NLAYS

C get point source emission mapping
            IF ( .NOT. PT3DEM_MAP( ) ) THEN
               XMSG = 'Could not get point source mappings'
               CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
               SUCCESS = .FALSE.; RETURN
            END IF

            NSRC = EM % COUNT

C Allocate Buffer space for Reading Emissions
            ALLOCATE ( BUFFER( NSRC ), STAT = IOS )
            CALL CHECKMEM( IOS, 'BUFFER', PNAME )

            FIRSTTIME = .FALSE.
         END IF

C        ... gas species only ...        

         DO S = 1, N_GSPC_EMIS
            P( 1 ) = PT3DEM_MAP( S )
            IF ( P( 1 ) .GT. 0 ) THEN

               NP = 1
               IF ( EM % TABLE( P( 1 ), 1 ) .EQ. 'POC' ) THEN
                 P( 2 ) = INDEX1( 'PNCOM', NSPC, EM % TABLE( 1, 1 ) )
                 IF ( AQM_RC_TEST( ( P( 2 ) .LE. 0 ),
     &              MSG="Failure while reading PNCOM" //
     &              " from " // TRIM( ETYPE ) // " emissions",
     &              FILE=__FILE__, LINE=__LINE__)) RETURN
                 NP = 2
               END IF

               DO K = 1, NP
                  N = P( K )
                  CALL AQM_EMIS_READ( ETYPE, EM % TABLE( N, 1 ),
     &              BUFFER, IP, JP, RC=LOCALRC)
                  IF ( AQM_RC_CHECK( LOCALRC, 
     &               MSG="Failure while reading " //
     &               TRIM( EM % TABLE( N, 1 ) ) // " from " // 
     &               TRIM( ETYPE) // " emissions",
     &               FILE=__FILE__, LINE=__LINE__)) RETURN
                  DO S = 1, NSRC
                     C = IP( S )
                     R = JP( S )
                     IF ( C .GT. O .AND. R .GT. 0 ) THEN
c                       ... compute plumerise ...
c                       vfrac = ...
                        DO L = 1, EMLYRS
                           VDEMIS_PT( C,R,L,N ) = VFRAC( L ) * BUFFER( S )
                        END DO
                     END IF
                  END DO
               END DO
            END IF

         END DO

C        ... aerosol species ...        

         DO S = 1, N_SPC_PT3DPM
            V = PT3DPM_MAP( S )
            BUFFER = 0.0
            CALL AQM_EMIS_READ( ETYPE, PMEM_MAP_NAME( V ), BUFFER,
            CRC=LOCALRC )
            IF ( AQM_RC_CHECK( LOCALRC, MSG="Failure while reading " //
     &         TRIM( PMEM_MAP_NAME( V ) ) // " from " // TRIM( ETYPE ) // " emissions",
     &         FILE=__FILE__, LINE=__LINE__)) RETURN
            DO L = 1, EMLYRS
               K = 0
               DO R = 1, MY_NROWS
                  DO C = 1, MY_NCOLS
                     K = K + 1
                     PMEMIS_PT( C,R,L,S ) = VFRAC( C,R,L ) * BUFFER( K )
                  END DO
               END DO
            END DO
         END DO


C Add to total emission array

C merge: VDEMIS_PT in mol/sec. Convert to ppm/sec
C If aerosols, then the PM emissions obtained from GET_PT3D_EMIS need to
C be
C accessed by GET_AERO_EMIS to be "speciated" and merged with the AE
C emissions.

         S_STRT = 1; S_END = N_GC_EMIS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               DO L = 1, EMLAYS
                  CNVTC = CNVTP * Grid_Data%RDX3F( L ) * Met_Data%RRHOJ( C,R,L )
                  DO S = S_STRT, S_END
                     V = SPC_PTEM_MAP( S ) ! <- pointer to non-PM (non-aerosol) species
                     N = PTEM_MAP( S )     ! <- pointer to non-PM emissions
                     IF ( V .LT. 1 ) CYCLE
                        VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                    + CNVTC * SPC_PT3DEM_FAC( S )
     &                                    * VDEMIS_PT( C,R,L,N )
                  END DO
               END DO
            END DO
         END DO

         S_STRT = S_END + 1; S_END = N_GC_EMIS + N_NR_EMIS + N_TR_EMIS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               DO L = 1, EMLAYS
                  CNVTC = CNVTP * Grid_Data%RDX3F( L ) *
                  CMet_Data%RRHOJ( C,R,L )
                  DO S = S_STRT, S_END
                     V = SPC_PTEM_MAP( S ) ! <- pointer to non-PM (non-aerosol) species
                     N = PTEM_MAP( S )     ! <- pointer to non-PM emissions
                     IF ( V .LT. 1 ) CYCLE
                     VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                                 + CNVTC * SPC_PT3DEM_FAC( S )
     &                                 * VDEMIS_PT( C,R,L,N )
                  END DO
               END DO
            END DO
         END DO


         END SUBROUTINE GET_PT3D_EMIS


         FUNCTION PT3DEM_MAP( ) RESULT ( SUCCESS )

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
         INTEGER N, N_GAS_EMIS, NSPC1, NSPC2, NSPC3
         INTEGER S, S_OFFSET, V

         INTEGER, ALLOCATABLE :: MAP( : )

         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM

C-----------------------------------------------------------------------

         LOGDEV = SETUP_LOGDEV()

         SUCCESS = .TRUE.

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

         END FUNCTION PT3DEM_MAP

      END MODULE PT3D_EMIS_DEFN
