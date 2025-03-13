      MODULE PT3D_DEFN

      !USE PT3D_DATA_MOD
      USE PT3D_FIRE_DEFN
      USE PT3D_STKS_DEFN

      IMPLICIT NONE

      PRIVATE

      PUBLIC PT3D_INIT, GET_PT3D_EMIS !, VDEMIS_PT

      CHARACTER(  16 ), ALLOCATABLE, SAVE :: STKENAME( : )  

      CONTAINS

         FUNCTION PT3D_INIT ( JDATE, JTIME, TSTEP )
     &      RESULT ( SUCCESS )

         USE GRID_CONF                       ! horizontal & vertical domain specifications
         USE STK_EMIS  
         USE DESID_VARS  
         USE UTILIO_DEFN
         USE PTBILIN, ONLY: NPTGRPS
         USE RUNTIME_VARS

         IMPLICIT NONE

C         INTEGER, INTENT( IN ) :: N_SPC_EMIS ! total no. of model emissions species
C         INTEGER, INTENT( IN ) :: EMLAYS     ! number of emissions layers
         INTEGER, INTENT( IN ) :: JDATE      ! Julian date (YYYYDDD)
         INTEGER, INTENT( IN ) :: JTIME      ! time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP      ! output time step

         ! -- local variables
         INTEGER :: IOS
         INTEGER :: N, ISRM, V
C         INTEGER, ALLOCATABLE :: MAP( : )
         CHARACTER( 240 )     :: XMSG = ' '
         CHARACTER( 16 )      :: PNAME = 'PT3D_INIT       ' ! procedure name

         LOGICAL :: SUCCESS

         ! -- begin

         SUCCESS = .TRUE.

         ! -- In-line 3D point source emissions?


         IF ( NPTGRPS .GT. 0 ) THEN
            XMSG = 'Using in-line 3d point source emissions option'
            CALL M3MSG2( XMSG )
         ELSE
            RETURN
         END IF


C Initialize stack emissions data
         ALLOCATE ( STKENAME( NPTGRPS ), STAT = IOS )   ! stk emis files array
         CALL CHECKMEM( IOS, 'STKENAME', PNAME )
         STKENAME = ' '   ! array

         ! Retrive Point Source Filenames from Emissions Filename Vector
         DO N = 1, NPTGRPS
            ISRM = MAP_PTtoISRM( N )
            STKENAME( N ) = DESID_STREAM_NAME( ISRM )
         END DO

         ! Initialize Point Source Emissions
         IF ( .NOT. STK_EMIS_INIT( STKENAME, JDATE, JTIME ) ) THEN
            XMSG = 'Could not initialize stack parameters'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF


         END FUNCTION PT3D_INIT


         SUBROUTINE GET_PT3D_EMIS ( JDATE, JTIME, TSTEP,EMVAR_PT, ISRM,
     &                              VDEMIS_PT, PTLAYS,L_DESID_DIAG)

         INTEGER, INTENT( IN ) :: JDATE, JTIME
         INTEGER, INTENT( IN ) :: TSTEP( 3 )
         CHARACTER(16), INTENT( IN ) :: EMVAR_PT( : )
         INTEGER, INTENT( IN ) :: ISRM
         
         INTEGER, INTENT( OUT) :: PTLAYS
         REAL, INTENT(INOUT)  :: VDEMIS_PT( :,:,:,: )

! Local variables:
         LOGICAL       :: L_DESID_DIAG ! not really used


         IF (ISRM == 2)  !For Fire 
     &    CALL GET_PT3D_FIRE_EMIS ( JDATE, JTIME, EMVAR_PT, ISRM,
     &                             VDEMIS_PT, PTLAYS  )
         
         IF (ISRM == 3) !For point-source
     &    CALL GET_PT3D_STKS_EMIS ( JDATE, JTIME, EMVAR_PT, ISRM,
     &                             VDEMIS_PT, PTLAYS  )

         END SUBROUTINE GET_PT3D_EMIS

      END MODULE PT3D_DEFN
