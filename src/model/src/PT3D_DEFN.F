      MODULE PT3D_DEFN

      USE PT3D_DATA_MOD
      USE PT3D_FIRE_DEFN
      USE PT3D_STKS_DEFN

      IMPLICIT NONE

      PRIVATE

      PUBLIC PT3DEMIS, NPTGRPS, VDEMIS_PT, VDEMIS_PT_FIRE, PMEMIS_PT,
     &       PT3D_INIT, GET_PT3D_EMIS

      CONTAINS

         FUNCTION PT3D_INIT ( N_SPC_EMIS, EMLAYS, JDATE, JTIME, TSTEP )
     &      RESULT ( SUCCESS )

         USE GRID_CONF                       ! horizontal & vertical domain specifications
         USE PTMAP
         USE STK_EMIS, ONLY : STKSPC         ! hourly point source emissions
         USE UTILIO_DEFN

         IMPLICIT NONE

         INTEGER, INTENT( IN ) :: N_SPC_EMIS ! total no. of model emissions species
         INTEGER, INTENT( IN ) :: EMLAYS     ! number of emissions layers
         INTEGER, INTENT( IN ) :: JDATE      ! Julian date (YYYYDDD)
         INTEGER, INTENT( IN ) :: JTIME      ! time (HHMMSS)
         INTEGER, INTENT( IN ) :: TSTEP      ! output time step

         ! -- local variables
         INTEGER :: IOS
         INTEGER :: N, S, V
         INTEGER, ALLOCATABLE :: MAP( : )
         CHARACTER( 240 )     :: XMSG = ' '
         CHARACTER( 16 )      :: PNAME = 'PT3D_INIT       ' ! procedure name

         LOGICAL :: SUCCESS

         ! -- begin

         SUCCESS = .TRUE.

         ! -- In-line 3D point source emissions?

         PT3DEMIS = ENVYN( CTM_PT3DEMIS,
     &                   'Flag for in-line 3d point source emissions',
     &                   .FALSE., IOS )

         IF ( PT3DEMIS ) THEN
            XMSG = 'Using in-line 3d point source emissions option'
            CALL M3MSG2( XMSG )
         ELSE
            RETURN
         END IF

         ! -- merge PM maps from fire and point-source emissions

         IF ( .NOT. PTMAP_INIT( ) ) THEN
            XMSG = 'Could not merge point source mappings'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         ! -- initialize emission types

         IF ( .NOT. PT3D_FIRE_INIT ( N_SPC_EMIS, EMLAYS, JDATE, JTIME ) ) THEN
            XMSG = 'Could not initialize fire emissions'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         IF ( .NOT. PT3D_STKS_INIT ( ) ) THEN
            XMSG = 'Could not initialize point-source emissions'
            CALL M3WARN ( PNAME, JDATE, JTIME, XMSG )
            SUCCESS = .FALSE.; RETURN
         END IF

         ! -- allocate emission arrays

         ALLOCATE ( VDEMIS_PT( NCOLS, NROWS, EMLAYS, N_SPC_PTEM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VDEMIS_PT', PNAME )
         VDEMIS_PT = 0.0   ! array assignment

         ALLOCATE ( VDEMIS_PT_FIRE( NCOLS, NROWS, EMLAYS, N_SPC_PTEM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'VDEMIS_PT_FIRE', PNAME )
         VDEMIS_PT_FIRE = 0.0   ! array assignment

         ALLOCATE ( PMEMIS_PT( NCOLS, NROWS, EMLAYS, N_SPC_PTPM ), STAT = IOS )
         CALL CHECKMEM( IOS, 'PMEMIS_PT', PNAME )
         PMEMIS_PT = 0.0   ! array

         ! -- get number of different file groups (sectors)

         NPTGRPS = 0

         ! -- create point source internal array

         ALLOCATE( STKSPC( NPTGRPS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'STKSPC', PNAME )

         END FUNCTION PT3D_INIT


         SUBROUTINE GET_PT3D_EMIS ( JDATE, JTIME, TSTEP )

         INTEGER, INTENT( IN ) :: JDATE, JTIME
         INTEGER, INTENT( IN ) :: TSTEP( 3 )

C        ... initialize emission arrays ...

         VDEMIS_PT      = 0.0   ! array assignment
         VDEMIS_PT_FIRE = 0.0   ! array assignment
         PMEMIS_PT      = 0.0   ! array assignment

         CALL GET_PT3D_FIRE_EMIS ( JDATE, JTIME )
         CALL GET_PT3D_STKS_EMIS ( JDATE, JTIME )

         END SUBROUTINE GET_PT3D_EMIS
      
      function to_radian(degree) result(rad)
          ! degrees to radians
          real,intent(in) :: degree
          real, parameter :: deg_to_rad = atan(1.0)/45 ! exploit intrinsic atan to generate pi/180 runtime constant
          real :: rad
 
          rad = degree*deg_to_rad
      end function to_radian
 
      function haversine(deglat1,deglon1,deglat2,deglon2) result (dist)
          real,intent(in) :: deglat1,deglon1,deglat2,deglon2
          real :: a,c,dist,dlat,dlon,lat1,lat2
          real,parameter :: radius = 6372.8 ! in km
 
          dlat = to_radian(deglat2-deglat1)
          dlon = to_radian(deglon2-deglon1)
          lat1 = to_radian(deglat1)
          lat2 = to_radian(deglat2)
          a = (sin(dlat/2))**2 + cos(lat1)*cos(lat2)*(sin(dlon/2))**2
          c = 2*asin(sqrt(a))
          dist = radius*c
      end function haversine

       subroutine check(status)
       integer, intent ( in) :: status

       if(status /= nf90_noerr) then
         print *, 'netcdf error in PT3D_DEFN.F ', trim(nf90_strerror(status))
         stop "Stopped"
       end if
      end subroutine check

      END MODULE PT3D_DEFN
