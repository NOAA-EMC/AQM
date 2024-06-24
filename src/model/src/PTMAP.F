      MODULE PTMAP

      IMPLICIT NONE

      INTEGER, SAVE :: N_GSPC_EMIS = 0   ! number of gas species in diagnostic file

C Species names from input file used for point source non-PM emissions mapping
      INTEGER,         ALLOCATABLE, SAVE :: PTEM_MAP( : )
      INTEGER,         ALLOCATABLE, SAVE :: SPC_PTEM_MAP( : )
      REAL,            ALLOCATABLE, SAVE :: SPC_PTEM_FAC( : )

C Species names from input file used for point source PM emissions mapping
      INTEGER,         ALLOCATABLE, SAVE :: PTPM_MAP( : )

C Mapping for point source non-PM emissions (maps only GC, NR and TR)
      INTEGER, SAVE :: N_SPC_PTEM = 0   ! merged no. of unique species
      INTEGER, SAVE :: N_SPC_PTPM = 0   ! merged no. of unique species for PM

      PRIVATE

      PUBLIC :: N_GSPC_EMIS, N_SPC_PTEM, N_SPC_PTPM
      PUBLIC :: SPC_PTEM_FAC, SPC_PTEM_MAP
      PUBLIC :: PTEM_MAP, PTPM_MAP

      PUBLIC :: PTMAP_INIT
      PUBLIC :: PTMAP_TYPE_INIT

      CONTAINS

         FUNCTION PTMAP_INIT ( ) RESULT ( SUCCESS )

         USE CGRID_SPCS                                       ! CGRID mechanism species
         USE AERO_DATA,    ONLY : N_EMIS_PM, PMEM_MAP_NAME    ! defines aerosol species
         USE AQM_EMIS_MOD, ONLY : AQM_EMIS_GET, AQM_EMIS_ISPRESENT,
     &                            AQM_INTERNAL_EMIS_TYPE
         USE UTILIO_DEFN

         IMPLICIT NONE

         LOGICAL SUCCESS

         INTEGER I, IDX, IOS, IPT, NPT
         INTEGER N, N_GAS_EMIS, NSPC, NSPC1, NSPC2, NSPC3
         INTEGER S, S_OFFSET, V

         INTEGER,         ALLOCATABLE :: MAP( : )
         CHARACTER( 16 ), ALLOCATABLE :: EMSPC( : )

         TYPE PT_EMIS_TYPE
           TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM
         END TYPE PT_EMIS_TYPE

         TYPE( PT_EMIS_TYPE ) :: PT( 2 )

         CHARACTER( * ), PARAMETER :: PNAME = 'PTMAP_INIT'  ! procedure name

         CHARACTER( * ), PARAMETER :: ETYPES( 2 ) =
     &      (/ 'gbbepx      ', 'point-source' /)

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

C check if emissions are being provided

         NPT = SIZE( ETYPES )

         I = 0
         NSPC = 0
         DO IPT = 1, NPT
            IF ( AQM_EMIS_ISPRESENT( ETYPES( IPT ) ) ) THEN
               I = I + 1
               PT( I ) % EM => AQM_EMIS_GET( ETYPES( IPT ) )
               NSPC = NSPC + SIZE( PT( I ) % EM % TABLE, DIM=1 )
            END IF
         END DO

         IF ( I == 0 ) RETURN

         NPT = I

         ALLOCATE ( EMSPC( NSPC ), STAT = IOS )
         CALL CHECKMEM( IOS, 'EMSPC', PNAME )
         MAP = 0

         EMSPC = ""

         ! -- merge emission tables and remove duplicates

         I = 0
         DO IPT = 1, NPT
            DO N = 1, SIZE( PT( IPT ) % EM % TABLE, DIM=1 )
               IDX = INDEX1( TRIM( PT( IPT ) % EM % TABLE( N, 1 ) ), NSPC, EMSPC )
               IF ( IDX .LE. 0 ) THEN
                  I = I + 1
                  EMSPC( I ) = PT( IPT ) % EM % TABLE( N, 1 )
               END IF
            END DO
         END DO

         NSPC = I

C compute emission offsets

         NSPC1 = N_GC_EMIS
         NSPC2 = NSPC1 + N_AE_EMIS
         NSPC3 = NSPC2 + N_NR_EMIS

C create auxiliary arrays mapping fire emission species to CMAQ gas and aerosol species

         ALLOCATE( MAP( NSPC ), STAT = IOS )
         CALL CHECKMEM( IOS, 'MAP', PNAME )
         MAP = 0

C        ... gas species ...        

         NSPC1 = N_GC_EMIS
         NSPC2 = NSPC1 + N_AE_EMIS
         NSPC3 = NSPC2 + N_NR_EMIS

         N_GAS_EMIS = N_GC_EMIS + N_NR_EMIS + N_TR_EMIS

         ALLOCATE( SPC_PTEM_FAC( N_GAS_EMIS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'SPC_PTEM_FAC', PNAME )
         SPC_PTEM_FAC = 1.0

         ALLOCATE( SPC_PTEM_MAP( N_GAS_EMIS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'SPC_PTEM_MAP', PNAME )
         SPC_PTEM_MAP = -1

         ALLOCATE( PTEM_MAP( N_GAS_EMIS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'PTEM_MAP', PNAME )
         PTEM_MAP = -1

         N = 0

         S_OFFSET = 0
         DO S = 1, N_GC_EMIS
           IDX = INDEX1( GC_EMIS( S ), NSPC, EMSPC )
           IF ( IDX .GT. 0 ) THEN
              N = N + 1
              PTEM_MAP    ( S ) = N
              SPC_PTEM_MAP( S ) = S
              SPC_PTEM_FAC( S ) = GC_EMIS_FAC( S )
           END IF
         END DO

         S_OFFSET = N_GC_EMIS
         DO S = 1, N_NR_EMIS
           IDX = INDEX1( NR_EMIS( S ), NSPC, EMSPC )
           IF ( IDX .GT. 0 ) THEN
              N = N + 1
              V = S + S_OFFSET
              PTEM_MAP    ( V ) = N
              SPC_PTEM_MAP( V ) = S + NSPC2
              SPC_PTEM_FAC( V ) = NR_EMIS_FAC( S )
           END IF
         END DO

         S_OFFSET = S_OFFSET + N_NR_EMIS
         DO S = 1, N_TR_EMIS
           IDX = INDEX1( TR_EMIS( S ), NSPC, EMSPC )
           IF ( IDX .GT. 0 ) THEN
              N = N + 1
              V = S + S_OFFSET
              PTEM_MAP    ( V ) = N
              SPC_PTEM_MAP( V ) = S + NSPC3
              SPC_PTEM_FAC( V ) = TR_EMIS_FAC( S )
           END IF
         END DO

         N_SPC_PTEM  = N
         N_GSPC_EMIS = N_GAS_EMIS

C        ... aerosol species ...        

         N = 0
         DO S = 1, NSPC
            IDX = INDEX1( EMSPC( S ), N_EMIS_PM, PMEM_MAP_NAME )
            IF ( IDX .GT. 0 ) THEN
               N = N + 1 
               MAP( N ) = IDX
            END IF
         END DO

         N_SPC_PTPM = N

         ALLOCATE( PTPM_MAP( N_SPC_PTPM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'PTPM_MAP', PNAME )
         PTPM_MAP = MAP( 1:N_SPC_PTPM )

         DEALLOCATE( EMSPC, MAP )

         END FUNCTION PTMAP_INIT


         FUNCTION PTMAP_TYPE_INIT( EM,
     &                             N_GSPC_TYPE_EMIS,  N_SPC_TYPE_PTPM,
     &                             PTEM_TYPE_MAP,     PTPM_TYPE_MAP,
     &                             SPC_PTEM_TYPE_FAC, SPC_PTEM_TYPE_MAP,
     &                             ELABEL )
     &      RESULT ( SUCCESS )


         USE CGRID_SPCS                                       ! CGRID mechanism species
         USE AERO_DATA,    ONLY : N_EMIS_PM, PMEM_MAP_NAME    ! defines aerosol species
         USE AQM_EMIS_MOD, ONLY : AQM_INTERNAL_EMIS_TYPE
         USE UTILIO_DEFN

         IMPLICIT NONE

         ! -- arguments

         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM
         INTEGER,              INTENT( OUT ) :: N_GSPC_TYPE_EMIS
         INTEGER,              INTENT( OUT ) :: N_SPC_TYPE_PTPM
         INTEGER, ALLOCATABLE, INTENT( OUT ) :: PTEM_TYPE_MAP( : )
         INTEGER, ALLOCATABLE, INTENT( OUT ) :: PTPM_TYPE_MAP( : )
         INTEGER, ALLOCATABLE, INTENT( OUT ) :: SPC_PTEM_TYPE_FAC( : )
         INTEGER, ALLOCATABLE, INTENT( OUT ) :: SPC_PTEM_TYPE_MAP( : )
         CHARACTER( * ),       INTENT( IN  ) :: ELABEL

         ! -- local variables

         LOGICAL SUCCESS

         INTEGER IOS

         INTEGER IDX
         INTEGER N, N_GAS_EMIS, NSPC, NSPC1, NSPC2, NSPC3
         INTEGER S, S_OFFSET, V

         INTEGER, ALLOCATABLE :: MAP( : )

         CHARACTER( * ), PARAMETER :: PNAME = 'PTEM_TYPE_MAP'  ! procedure name

C-----------------------------------------------------------------------

         SUCCESS = .TRUE.

         N_GSPC_TYPE_EMIS = 0
         N_SPC_TYPE_PTPM  = 0

         IF ( .NOT.ASSOCIATED( EM ) ) RETURN

C compute emission offsets

         NSPC1 = N_GC_EMIS
         NSPC2 = NSPC1 + N_AE_EMIS
         NSPC3 = NSPC2 + N_NR_EMIS

C create auxiliary arrays mapping fire emission species to CMAQ gas and aerosol species

         NSPC = SIZE( EM % TABLE, DIM=1 )

         ALLOCATE( MAP( NSPC ), STAT = IOS )
         CALL CHECKMEM( IOS, 'MAP: ' // TRIM( ELABEL ), PNAME )
         MAP = 0

C        ... gas species ...

         NSPC1 = N_GC_EMIS
         NSPC2 = NSPC1 + N_AE_EMIS
         NSPC3 = NSPC2 + N_NR_EMIS

         N_GAS_EMIS = N_GC_EMIS + N_NR_EMIS + N_TR_EMIS

         ALLOCATE( SPC_PTEM_TYPE_FAC( N_GAS_EMIS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'SPC_PTEM_TYPE_FAC: ' // TRIM( ELABEL ), PNAME )
         SPC_PTEM_TYPE_FAC = 1.0

         ALLOCATE( SPC_PTEM_TYPE_MAP( N_GAS_EMIS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'SPC_PTEM_TYPE_MAP: ' // TRIM( ELABEL ), PNAME )
         SPC_PTEM_TYPE_MAP = -1

         ALLOCATE( PTEM_TYPE_MAP( N_GAS_EMIS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'PTEM_TYPE_MAP: ' // TRIM( ELABEL ), PNAME )
         PTEM_TYPE_MAP = -1

         N = 0

         S_OFFSET = 0
         DO S = 1, N_GC_EMIS
           IDX = INDEX1( GC_EMIS( S ), NSPC, EM % TABLE( 1, 1 ) )
           IF ( IDX .GT. 0 ) THEN
              N = N + 1
              PTEM_TYPE_MAP    ( S ) = IDX
              SPC_PTEM_TYPE_MAP( S ) = S
              SPC_PTEM_TYPE_FAC( S ) = GC_EMIS_FAC( S )
           END IF
         END DO

         S_OFFSET = N_GC_EMIS
         DO S = 1, N_NR_EMIS
           IDX = INDEX1( NR_EMIS( S ), NSPC, EM % TABLE( 1, 1 ) )
           IF ( IDX .GT. 0 ) THEN
              N = N + 1
              V = S + S_OFFSET
              PTEM_TYPE_MAP    ( V ) = IDX
              SPC_PTEM_TYPE_MAP( V ) = S + NSPC2
              SPC_PTEM_TYPE_FAC( V ) = NR_EMIS_FAC( S )
           END IF
         END DO

         S_OFFSET = S_OFFSET + N_NR_EMIS
         DO S = 1, N_TR_EMIS
           IDX = INDEX1( TR_EMIS( S ), NSPC, EM % TABLE( 1, 1 ) )
           IF ( IDX .GT. 0 ) THEN
              N = N + 1
              V = S + S_OFFSET
              PTEM_TYPE_MAP    ( V ) = IDX
              SPC_PTEM_TYPE_MAP( V ) = S + NSPC3
              SPC_PTEM_TYPE_FAC( V ) = TR_EMIS_FAC( S )
           END IF
         END DO

         N_GSPC_TYPE_EMIS = N_GAS_EMIS

C        ... aerosol species ...

         N = 0
         DO S = 1, N_SPC_PTPM
            V = PTPM_MAP( S )
            IDX = INDEX1( PMEM_MAP_NAME( V ), NSPC, EM % TABLE( 1, 1 ) )
            IF ( IDX .GT. 0 ) THEN
               N = N + 1
               MAP( N ) = S
            END IF
         END DO

         N_SPC_TYPE_PTPM = N

         ALLOCATE( PTPM_TYPE_MAP( N_SPC_TYPE_PTPM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'PTPM_TYPE_MAP: '// TRIM( ELABEL ), PNAME )
         PTPM_TYPE_MAP = MAP( 1:N_SPC_TYPE_PTPM )

         DEALLOCATE( MAP )

         END FUNCTION PTMAP_TYPE_INIT

      END MODULE PTMAP
