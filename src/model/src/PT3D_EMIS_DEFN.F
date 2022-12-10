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

C External Functions:
         INTEGER, EXTERNAL :: SETUP_LOGDEV

C Local Variables:
         REAL              :: CNVTC         ! combined conversion factor
         REAL              :: CNVTE         ! combined conversion factor
         REAL              :: CNVTI         ! intermediate combined conv. factor
         REAL              :: CONVEM        ! emission conversion factor
         INTEGER           :: LOGDEV
         INTEGER           :: JDATE, JTIME
         INTEGER           :: C, R, L, N, S, V, K, I ! loop induction variables
         INTEGER           :: LBOT       ! layer containing plume bottom
         INTEGER           :: LTOP       ! layer containing plume top
         INTEGER           :: LPBL       ! first L: ZF(L) above mixing layer - ONLY for REPORT
         INTEGER           :: LSTK       ! first L: ZF(L) > STKHT
         INTEGER           :: IJ, IS
         INTEGER           :: NSRC, NP
         INTEGER           :: IOS, LOCALRC
         INTEGER           :: P( 2 )
         INTEGER, POINTER  :: IP( : )
         INTEGER, POINTER  :: JP( : )
         INTEGER, POINTER  :: IJMAP( : )
         REAL              :: DX1, DX2
         REAL              :: MV         ! mininum LFRAC
         REAL              :: PSFC       ! surface pressure [Pa]
         REAL              :: TSTK       ! temperature at top of stack [K]
         REAL              :: TSUM       ! tmp layer frac sum for renormalizing
         REAL              :: WSTK       ! wind speed  at top of stack [m/s]
         REAL              :: ZBOT       ! plume bottom elevation [m]
         REAL              :: ZTOP       ! plume top    elevation [m]
         REAL              :: ZDIFF      ! ZTOP - ZBOT
         REAL              :: ZF0, ZF1
         REAL              :: DDZ        ! 1 / ZDIFF
         REAL              :: ZPLM       ! plume centerline height above stack [m]
         REAL              :: USTMP      ! temp storage for ustar [m/s]
         REAL              :: HFLX       ! converted heat flux
         REAL, ALLOCATABLE :: BUFFER( : )
         REAL, ALLOCATABLE :: DTHDZ( : )
         REAL, ALLOCATABLE :: PRESF( : )
         REAL, ALLOCATABLE :: WSPD( : )
         REAL, ALLOCATABLE :: ZZF( : )
         REAL, ALLOCATABLE :: ZSTK( : )
         REAL, ALLOCATABLE :: DDZF( : )
         REAL, ALLOCATABLE :: TFRAC( : )

C Parameters:
         REAL, PARAMETER :: CMLMR = 1.0E+06  ! ppmV/Molar Mixing Ratio
         REAL, PARAMETER :: CNVPA2MB = 1.0E-2  ! convert Pa to mb
         REAL, PARAMETER :: USTARMIN = 0.1   ! Min valid value for USTAR
         CHARACTER(  8 ) :: CINT     ! integer to character buffer for Cwarning messages
         CHARACTER( 16 ) :: PNAME = 'GET_PT3D_EMIS'
         CHARACTER( 120 ) :: XMSG = ' '
         CHARACTER(  10 ), PARAMETER :: BLANK10 = '          '

         LOGICAL, SAVE :: FIRSTIME = .TRUE.
         INTEGER, SAVE :: EMLYRS

         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM

C-----------------------------------------------------------------------

         INTERFACE
           SUBROUTINE PREPLM( FIREFLG, EMLAYS, HMIX, HTS, PSFC, TS, DDZF, QV,
     &                        TA, UW, VW, ZH, ZF, PRES, LSTK, LPBL, TSTK,
     &                        WSTK, DTHDZ, WSPD )
             LOGICAL, INTENT( IN )  :: FIREFLG
             INTEGER, INTENT( IN )  :: EMLAYS
             REAL,    INTENT( IN )  :: HMIX
             REAL,    INTENT( IN )  :: HTS
             REAL,    INTENT( IN )  :: PSFC
             REAL,    INTENT( IN )  :: TS
             REAL,    INTENT( IN )  :: DDZF( : )
             REAL,    INTENT( IN )  :: QV  ( : )
             REAL,    INTENT( IN )  :: TA  ( : )
             REAL,    INTENT( IN )  :: UW  ( : )
             REAL,    INTENT( IN )  :: VW  ( : )
             REAL,    INTENT( IN )  :: ZH  ( : )
             REAL,    INTENT( IN )  :: ZF  ( : )
             REAL,    INTENT( IN )  :: PRES( 0: )
             INTEGER, INTENT( OUT ) :: LSTK
             INTEGER, INTENT( OUT ) :: LPBL
             REAL,    INTENT( OUT ) :: TSTK
             REAL,    INTENT( OUT ) :: WSTK
             REAL,    INTENT( OUT ) :: DTHDZ( : )
             REAL,    INTENT( OUT ) :: WSPD ( : )
           END SUBROUTINE PREPLM

           SUBROUTINE PLMRIS( EMLAYS, LSTK, HFX, HMIX,
     &                        STKDM, STKHT, STKTK, STKVE,
     &                        TSTK, USTAR, DTHDZ, TA, WSPD,
     &                        ZF, ZH, ZSTK, WSTK, ZPLM )
             INTEGER, INTENT( IN )  :: EMLAYS
             INTEGER, INTENT( IN )  :: LSTK
             REAL,    INTENT( IN )  :: HFX
             REAL,    INTENT( IN )  :: HMIX
             REAL,    INTENT( IN )  :: STKDM
             REAL,    INTENT( IN )  :: STKHT
             REAL,    INTENT( IN )  :: STKTK
             REAL,    INTENT( IN )  :: STKVE
             REAL,    INTENT( IN )  :: TSTK
             REAL,    INTENT( IN )  :: USTAR
             REAL,    INTENT( IN )  :: DTHDZ( : )
             REAL,    INTENT( IN )  :: TA   ( : )
             REAL,    INTENT( IN )  :: WSPD ( : )
             REAL,    INTENT( IN )  :: ZF ( 0:  )
             REAL,    INTENT( IN )  :: ZH   ( : )
             REAL,    INTENT( IN )  :: ZSTK ( : )
             REAL,    INTENT( INOUT ) :: WSTK
             REAL,    INTENT( OUT ) :: ZPLM
           END SUBROUTINE PLMRIS

           SUBROUTINE PLSPRD( DTHDZ, ZF, KZ, CEFSTK, PLTOP, PLBOT )
             REAL,    INTENT ( IN ) :: DTHDZ( : )
             REAL,    INTENT ( IN ) :: ZF( 0:  )
             INTEGER, INTENT ( IN ) :: KZ
             REAL,    INTENT ( IN ) :: CEFSTK
             REAL,    INTENT( OUT ) :: PLTOP
             REAL,    INTENT( OUT ) :: PLBOT
           END SUBROUTINE PLSPRD

         END INTERFACE

C-----------------------------------------------------------------------

         EM => AQM_EMIS_GET( ETYPE )
         IF ( .NOT.ASSOCIATED( EM ) ) RETURN

         IF (EM % COUNT == 0) RETURN

         IF ( FIRSTIME ) THEN

             LOGDEV = SETUP_LOGDEV()

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

         ALLOCATE ( TFRAC( EMLYRS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'TFRAC', PNAME )

         IF ( EMLYRS .GT. 1 ) THEN
            ALLOCATE ( DTHDZ( EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'DTHDZ', PNAME )

            ALLOCATE ( PRESF( 0:EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'PRESF', PNAME )

            ALLOCATE ( WSPD( EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'WSPD', PNAME )

            ALLOCATE ( ZZF( EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'ZZF', PNAME )

            ALLOCATE ( ZSTK( EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'ZSTK', PNAME )

            ALLOCATE ( DDZF( EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'DDZF', PNAME )
         END IF

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

               IF ( EMLYRS .GT. 1 ) THEN
C Loop through sources and compute plume rise

                  ZZF( 0 ) = 0.0
                  ZZF( 1:EMLYRS ) = Met_Data % ZF( C,R,1:EMLYRS )

C Set surface pressure (convert to mb from Pa)
                  PSFC = CNVPA2MB * Met_Data % PRSFC( C,R )

                  PRESF( 0:EMLYRS ) = CNVPA2MB * Met_Data % PRESF( C,R,: )

C Compute vertical gradient quantities
                  ZF0 = Met_Data % ZF( C,R,1 )
                  ZSTK( 1 ) = ZF0 - EM % STKHT( IJ )
                  DDZF( 1 ) = 1.0 / ZF0

                  DO L = 2, EMLYRS
                     ZF1 = Met_Data % ZF( C,R,L )
                     ZSTK( L ) = ZF1 - EM % STKHT( IJ )
                     DDZF( L ) = 1.0 / ( ZF1 - ZF0 )
                     ZF0 = ZF1
                  END DO

C Compute derived met vars needed before layer assignments
                  CALL PREPLM( .FALSE., EMLYRS,
     &               Met_Data % PBL   ( C,R ),   EM % STKHT( IJ ), PSFC,
     &               Met_Data % TEMP2 ( C,R ),   DDZF,
     &               Met_Data % QV    ( C,R,: ), Met_Data % TA     ( C,R,: ),
     &               Met_Data % UWINDA( C,R,: ), Met_Data % VWINDA ( C,R,: ),
     &               Met_Data % ZH    ( C,R,: ), Met_Data % ZF     ( C,R,: ),
     &               PRESF, LSTK, LPBL, TSTK, WSTK,
     &               DTHDZ, WSPD )

C Trap USTAR at a minimum realistic value
                  USTMP = MAX( Met_Data % USTAR( C,R ), USTARMIN )

C Convert heat flux (watts/m2 to m K /s )
                  HFLX = Met_Data % HFX( C,R ) / ( CPD * Met_Data % DENS1( C,R ) )

                  CALL PLMRIS( EMLYRS, LSTK, HFLX, Met_Data % PBL( C,R ),
     &               EM % STKDM( IJ ), EM % STKHT( IJ ),
     &               EM % STKTK( IJ ), EM % STKVE( IJ ),
     &               TSTK, USTMP,
     &               DTHDZ, Met_Data % TA( C,R,: ),
     &               WSPD, ZZF,
     &               Met_Data % ZH( C,R,: ), ZSTK,
     &               WSTK, ZPLM )

C Determine the bottom and top heights of the plume.
C Default Turner approach.  Plume thickness = amount of plume rise
C Plume rise DH = ZPLM minus the stack height STKHT
                  ZTOP = EM % STKHT( IJ )
     &                 + 1.5 * ( ZPLM - EM % STKHT( IJ ) )
                  ZBOT = EM % STKHT( IJ )
     &                 + 0.5 * ( ZPLM - EM % STKHT( IJ ) )

C Set up for computing plume fractions, assuming uniform distribution in pressure
C (~mass concentration -- minor hydrostatic assumption) from bottom to top.

                  IF ( ZTOP .LT. EM % STKHT( IJ ) ) THEN
                     WRITE( CINT,'( I8 )' ) S
                     WRITE( XMSG,94010 ) 'ERROR: Top of plume is less than '
     &                                   // 'top of stack for source:' // CINT
                     CALL M3MESG( XMSG )
                     WRITE( LOGDEV,* ) '    Zbot: ', ZBOT, ' Ztop: ', ZTOP
                     WRITE( LOGDEV,* ) '    Stack Top: ', EM % STKHT( IJ ),
     &                                 ' Plume Top: ', ZPLM
                     CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT2 )
                  END IF

C Allocate plume to layers (compute layer plume fractions)

C Compute LBOT, LTOP such that
C  ZZF( LBOT-1 ) <= ZBOT < ZZF( LBOT ) and
C  ZZF( LTOP-1 ) <= ZTOP < ZZF( LTOP )

                  DO L = 1, EMLYRS - 1
c                    IF ( ZBOT .LE. Met_Data % ZF( C,R,L ) ) THEN
                     IF ( ZBOT .LE. ZZF( L ) ) THEN
                        LBOT = L
                        GO TO  122
                     ELSE
                        TFRAC( L ) = 0.0             ! fractions below plume
                     END IF
                  END DO
                  LBOT = EMLYRS                      !  fallback

122               CONTINUE                           !  loop exit:  bottom found at LBOT

                  IF ( ZTOP .LE. ZZF( LBOT ) ) THEN  ! plume in this layer

                     TFRAC( LBOT ) = 1.0
                     LTOP = LBOT

                     DO L = LBOT + 1, EMLYRS         ! fractions above plume
                        TFRAC( L ) = 0.0
                     END DO

                  ELSE IF ( LBOT .EQ. EMLYRS ) THEN  ! plume above top layer

                     TFRAC( LBOT ) = 1.0

                     DO L = 1, EMLYRS - 1            ! fractions below plume
                        TFRAC( L ) = 0.0
                     END DO

                  ELSE                               ! plume crosses layers

                     DO L = LBOT + 1, EMLYRS
c                       IF ( ZTOP .LE. Met_Data % ZF( C,R,L ) ) THEN
                        IF ( ZTOP .LE. ZZF( L ) ) THEN
                           LTOP = L
                           GO TO 126
                        END IF
                     END DO
                     LTOP = EMLYRS                   !  fallback

126                  CONTINUE

                     ZDIFF = ZTOP - ZBOT
                     IF ( ZDIFF .GT. 0.0 ) THEN

                        DDZ  = 1.0 / ZDIFF
c                       TFRAC( LBOT ) = DDZ * ( Met_Data % ZF( C,R,LBOT ) - ZBOT )
c                       TFRAC( LTOP ) = DDZ * ( ZTOP - Met_Data % ZF( C,R,LTOP-1 ) )
                        TFRAC( LBOT ) = DDZ * ( ZZF( LBOT ) - ZBOT )
                        TFRAC( LTOP ) = DDZ * ( ZTOP - ZZF( LTOP-1 ) )

                     ELSE   ! ZDIFF .le. 0

                        WRITE( CINT,'( I8 )' ) S
                        WRITE( XMSG,94020 )
     &                     'Infinitely small plume created for source:,'
     &                     // CINT // CRLF() // BLANK10
     &                     // 'All emissions put in first layer.'
                        CALL M3WARN( PNAME, JDATE, JTIME, XMSG )
                        LBOT = 1; LTOP = 1
                        TFRAC( LBOT ) = 1.0

                     END IF

                     DO L = LBOT + 1, LTOP - 1       ! layers in plume
                        TFRAC( L ) = DDZ * ( Met_Data % ZF( C,R,L ) - Met_Data % ZF( C,R,L-1 ) )
                     END DO

                     DO L = LTOP + 1, EMLYRS         ! fractions above plume
                        TFRAC( L ) = 0.0
                     END DO

                  END IF

C If layer fractions are negative, put in the first layer
                  MV = MINVAL( TFRAC( 1:EMLYRS ) )
                  IF ( MV .LT. 0.0 ) THEN

                     WRITE( CINT,'( I8 )' ) S
                     WRITE( XMSG,94010 ) 'WARNING: One or more negative plume '
     &                        // 'fractions found for source:' // CINT
     &                        // CRLF() // BLANK10 // 'Plume reset to '
     &                        // 'put all emissions in surface layer.'
                     CALL M3MESG( XMSG )

                     TFRAC( 1 ) = 1.0
                     TFRAC( 2:EMLYRS ) = 0.0

                  END IF

                ELSE

                  TFRAC( 1:EMLYRS ) = 1.0

                END IF

               ! -- add emissions
               DO L = 1, EMLYRS
                  CNVTC = CNVTI * Grid_Data%RDX3F( L ) * Met_Data%RRHOJ( C,R,L )
                  VDEMIS( V,L,C,R ) = VDEMIS( V,L,C,R )
     &                              + CNVTC * TFRAC( L ) * BUFFER( IJ )
               END DO
            END DO
         END DO

         ! -- (2) PM emissions (TBD)

         ! -- free up memory

         DEALLOCATE ( BUFFER, TFRAC )

         IF ( EMLYRS .GT. 1 ) DEALLOCATE ( DDZF, DTHDZ, PRESF, WSPD, ZSTK, ZZF )

C------------------  FORMAT  STATEMENTS   ------------------------------

94010    FORMAT( 12( A, :, I8, :, 1X ) )
94020    FORMAT( 10( A, :, I7, :, 1X ) )


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
