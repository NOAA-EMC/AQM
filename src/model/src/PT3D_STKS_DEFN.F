      MODULE PT3D_STKS_DEFN

      USE GRID_CONF           ! horizontal & vertical domain specifications      
      USE PTMAP
      USE AQM_EMIS_MOD, ONLY : AQM_EMIS_DESC, AQM_EMIS_GET,
     &                         AQM_EMIS_READ, AQM_INTERNAL_EMIS_TYPE

      IMPLICIT NONE

      INTEGER :: N_GSPC_STKS_EMIS
      INTEGER :: N_SPC_PT3DEM
      INTEGER :: N_SPC_STKS_PTPM

      INTEGER, ALLOCATABLE :: SPC_PTEM_STKS_FAC( : )
      INTEGER, ALLOCATABLE :: SPC_PTEM_STKS_MAP( : )
      INTEGER, ALLOCATABLE :: PTEM_STKS_MAP( : )
      INTEGER, ALLOCATABLE :: PTPM_STKS_MAP( : )

      REAL :: CNVTP         ! intermediate combined conv.  factor

C Emission type
      CHARACTER( * ), PARAMETER :: ETYPE = 'point-source'

      PRIVATE

      PUBLIC :: N_SPC_STKS_PTPM, PTPM_STKS_MAP
      PUBLIC :: GET_PT3D_STKS_EMIS, PT3D_STKS_INIT

      CONTAINS

         SUBROUTINE GET_PT3D_STKS_EMIS( JDATE, JTIME )

         USE CGRID_SPCS          ! CGRID mechanism species
         USE UTILIO_DEFN
         USE AQM_RC_MOD
         USE AERO_DATA, ONLY : PMEM_MAP_NAME
         USE ASX_DATA_MOD, ONLY: MET_DATA, GRID_DATA, CONVPA
         USE PT3D_DATA_MOD

         IMPLICIT NONE

C Includes:
         INCLUDE SUBST_FILES_ID  ! file name parameters
         INCLUDE SUBST_CONST

C Arguments:
         INTEGER, INTENT( IN ) :: JDATE      ! Julian date (YYYYDDD)
         INTEGER, INTENT( IN ) :: JTIME      ! time (HHMMSS)

C External Functions:
         INTEGER, EXTERNAL :: SETUP_LOGDEV

C Local Variables:
         INTEGER           :: LOGDEV
         INTEGER           :: C, R, L, M, N, S, V, K, I ! loop induction variables
         INTEGER           :: LBOT       ! layer containing plume bottom
         INTEGER           :: LTOP       ! layer containing plume top
         INTEGER           :: LPBL       ! first L: ZF(L) above mixing layer - ONLY for REPORT
         INTEGER           :: LSTK       ! first L: ZF(L) > STKHT
         INTEGER           :: IJ, IS
         INTEGER           :: NSRC, NP
         INTEGER           :: IOS, LOCALRC
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
         REAL, ALLOCATABLE :: VFRAC( :, : )

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

C set number of emissions layers depending on whether plumerise is on

            CALL AQM_EMIS_DESC( ETYPE, NLAYS=EMLYRS )

            FIRSTIME = .FALSE.
         END IF

C Allocate Buffer space for Reading Emissions
         NSRC = SIZE( EM % IJMAP )

         ALLOCATE ( BUFFER( SIZE( EM % LAT ) ), STAT = IOS )
         CALL CHECKMEM( IOS, 'BUFFER', PNAME )

         ALLOCATE ( VFRAC( NSRC, EMLYRS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'TFRAC', PNAME )

         ALLOCATE ( TFRAC( EMLYRS ), STAT = IOS )
         CALL CHECKMEM( IOS, 'TFRAC', PNAME )

         IF ( EMLYRS .GT. 1 ) THEN
            ALLOCATE ( DTHDZ( EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'DTHDZ', PNAME )

            ALLOCATE ( PRESF( 0:EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'PRESF', PNAME )

            ALLOCATE ( WSPD( EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'WSPD', PNAME )

            ALLOCATE ( ZZF( 0:EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'ZZF', PNAME )

            ALLOCATE ( ZSTK( EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'ZSTK', PNAME )

            ALLOCATE ( DDZF( EMLYRS ), STAT = IOS )
            CALL CHECKMEM( IOS, 'DDZF', PNAME )
         END IF

         ! -- loop over mapped emissions only

         DO IS = 1, NSRC
            IJ = EM % IJMAP( IS ) % NLOC
            C  = EM % IJMAP( IS ) % I
            R  = EM % IJMAP( IS ) % J

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
     &            Met_Data % PBL   ( C,R ),   EM % STKHT( IJ ), PSFC,
     &            Met_Data % TEMP2 ( C,R ),   DDZF,
     &            Met_Data % QV    ( C,R,: ), Met_Data % TA     ( C,R,: ),
     &            Met_Data % UWINDA( C,R,: ), Met_Data % VWINDA ( C,R,: ),
     &            Met_Data % ZH    ( C,R,: ), Met_Data % ZF     ( C,R,: ),
     &            PRESF, LSTK, LPBL, TSTK, WSTK,
     &            DTHDZ, WSPD )

C Trap USTAR at a minimum realistic value
               USTMP = MAX( Met_Data % USTAR( C,R ), USTARMIN )

C Convert heat flux (watts/m2 to m K /s )
               HFLX = Met_Data % HFX( C,R ) / ( CPD * Met_Data % DENS1( C,R ) )

               CALL PLMRIS( EMLYRS, LSTK, HFLX, Met_Data % PBL( C,R ),
     &            EM % STKDM( IJ ), EM % STKHT( IJ ),
     &            EM % STKTK( IJ ), EM % STKVE( IJ ),
     &            TSTK, USTMP,
     &            DTHDZ, Met_Data % TA( C,R,: ),
     &            WSPD, ZZF,
     &            Met_Data % ZH( C,R,: ), ZSTK,
     &            WSTK, ZPLM )

C Determine the bottom and top heights of the plume.
C Default Turner approach.  Plume thickness = amount of plume rise
C Plume rise DH = ZPLM minus the stack height STKHT
               ZTOP = EM % STKHT( IJ )
     &              + 1.5 * ( ZPLM - EM % STKHT( IJ ) )
               ZBOT = EM % STKHT( IJ )
     &              + 0.5 * ( ZPLM - EM % STKHT( IJ ) )

C Set up for computing plume fractions, assuming uniform distribution in pressure
C (~mass concentration -- minor hydrostatic assumption) from bottom to top.

               IF ( ZTOP .LT. EM % STKHT( IJ ) ) THEN
                  WRITE( CINT,'( I8 )' ) IJ
                  WRITE( XMSG,94010 ) 'ERROR: Top of plume is less than '
     &                                // 'top of stack for source:' // CINT
                  CALL M3MESG( XMSG )
                  WRITE( XMSG,'("    Zbot: ",F12.6," Ztop: ",F12.6' //
     &                         '"    Stack Top: ",F12.6," Plume Top: ",F12.6)')
     &                         ZBOT, ZTOP, EM % STKHT( IJ ), ZPLM
                  CALL M3MESG( XMSG )
                  WRITE( LOGDEV,* ) '    Zbot: ', ZBOT, ' Ztop: ', ZTOP
                  WRITE( LOGDEV,* ) '    Stack Top: ', EM % STKHT( IJ ),
     &                              ' Plume Top: ', ZPLM
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

                     WRITE( CINT,'( I8 )' ) IJ
                     WRITE( XMSG,94020 )
     &                  'Infinitely small plume created for source:,'
     &                  // CINT // CRLF() // BLANK10
     &                  // 'All emissions put in first layer.'
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

                  WRITE( CINT,'( I8 )' ) IJ
                  WRITE( XMSG,94010 ) 
     &                  'WARNING: One or more negative plume '
     &               // 'fractions found for source:' // CINT
     &               // CRLF() // BLANK10 // 'Plume reset to '
     &               // 'put all emissions in surface layer.'
                  CALL M3MESG( XMSG )

                  TFRAC( 1 ) = 1.0
                  TFRAC( 2:EMLYRS ) = 0.0

               END IF

            ELSE

               TFRAC( 1:EMLYRS ) = 1.0

            END IF

            VFRAC( IS, : ) = TFRAC

         END DO

         DEALLOCATE ( TFRAC )

         ! -- (1) non-PM emissions

         DO S = 1, N_GSPC_STKS_EMIS
            M = PTEM_STKS_MAP( S )
            IF ( M .LT. 1 ) CYCLE
            N = PTEM_MAP( S )

            BUFFER = 0.0
            CALL AQM_EMIS_READ( ETYPE, EM % TABLE( M, 1 ), BUFFER, RC=LOCALRC)
            IF ( AQM_RC_CHECK( LOCALRC,
     &         MSG="Failure while reading " //
     &         TRIM( EM % TABLE( M, 1 ) ) // " from " //
     &         TRIM( ETYPE ) // " emissions",
     &         FILE=__FILE__, LINE=__LINE__ ) ) RETURN
            IF ( EM % TABLE( M, 1 ) .EQ. 'POC' ) THEN
               CALL AQM_EMIS_READ( ETYPE, 'PNCOM', BUFFER, RC=LOCALRC)
               IF ( AQM_RC_CHECK( LOCALRC,
     &            MSG="Failure while reading PNCOM emissions from" //
     &            TRIM( ETYPE ) // " emissions",
     &         FILE=__FILE__, LINE=__LINE__ ) ) RETURN
            END IF

            ! -- add emissions
            DO L = 1, EMLYRS
               DO IS = 1, NSRC
                  IJ = EM % IJMAP( IS ) % NLOC
                  C  = EM % IJMAP( IS ) % I
                  R  = EM % IJMAP( IS ) % J
                  VDEMIS_PT( C,R,L,N ) = VDEMIS_PT( C,R,L,N )
     &                                 + VFRAC( IS, L ) * BUFFER( IJ )
               END DO
            END DO
         END DO

         ! -- (2) PM emissions

         DO S = 1, N_SPC_STKS_PTPM
            N = PTPM_STKS_MAP( S )
            V = PTPM_MAP( N )
            BUFFER = 0.0
            CALL AQM_EMIS_READ( ETYPE, PMEM_MAP_NAME( V ), BUFFER, RC=LOCALRC )
            IF ( AQM_RC_CHECK( LOCALRC, MSG="Failure while reading " //
     &         TRIM( PMEM_MAP_NAME( V ) ) // " from " // TRIM( ETYPE ) // " emissions",
     &         FILE=__FILE__, LINE=__LINE__)) RETURN
            DO L = 1, EMLYRS
               DO IS = 1, NSRC
                  IJ = EM % IJMAP( IS ) % NLOC
                  C  = EM % IJMAP( IS ) % I
                  R  = EM % IJMAP( IS ) % J
                  PMEMIS_PT( C,R,L,N ) = PMEMIS_PT( C,R,L,N )
     &                                 + VFRAC( IS, L ) * BUFFER( IJ )
               END DO
            END DO
         END DO

         ! -- free up memory

         DEALLOCATE ( BUFFER, VFRAC )

         IF ( EMLYRS .GT. 1 ) DEALLOCATE ( DDZF, DTHDZ, PRESF, WSPD, ZSTK, ZZF )

C------------------  FORMAT  STATEMENTS   ------------------------------

94010    FORMAT( 12( A, :, I8, :, 1X ) )
94020    FORMAT( 10( A, :, I7, :, 1X ) )


         END SUBROUTINE GET_PT3D_STKS_EMIS


         FUNCTION PT3D_STKS_INIT( ) RESULT ( SUCCESS )

         USE AQM_RC_MOD, ONLY: AQM_RC_TEST

         TYPE( AQM_INTERNAL_EMIS_TYPE ), POINTER :: EM

         LOGICAL :: SUCCESS

         SUCCESS = .TRUE.

         EM => AQM_EMIS_GET( ETYPE )
         IF ( .NOT.ASSOCIATED( EM ) ) RETURN

         IF (EM % COUNT == 0) RETURN

         SUCCESS = PTMAP_TYPE_INIT( EM, 
     &                              N_GSPC_STKS_EMIS,  N_SPC_STKS_PTPM,
     &                              PTEM_STKS_MAP,     PTPM_STKS_MAP,
     &                              SPC_PTEM_STKS_FAC, SPC_PTEM_STKS_MAP,
     &                              "STKS" )

         IF ( AQM_RC_TEST( .NOT. SUCCESS, 
     &           MSG="Failure initializing mapping for" //
     &           TRIM( ETYPE ) // " emissions",
     &           FILE=__FILE__, LINE=__LINE__ ) ) RETURN

         END FUNCTION PT3D_STKS_INIT

      END MODULE PT3D_STKS_DEFN
