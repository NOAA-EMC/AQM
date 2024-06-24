!--------------------------------------------------------------------!
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
C $Header: /project/yoj/arc/CCTM/src/vdiff/acm2/vdiffacm2.F,v 1.13 2012/01/19 14:37:47 yoj Exp $

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
!     SUBROUTINE VDIFFACMX ( DTSEC, SEDDY, DDEP, ICMP, CNGRD, DDEPJ, DDEPJ_FST )
      SUBROUTINE VDIFFACMX ( DTSEC, SEDDY, DDEP, ICMP, DDEPJ, DDEPJ_FST, CNGRD )

C-----------------------------------------------------------------------
C Asymmetric Convective Model v2 (ACM2/ACM1) -- Pleim(2006/2014)
C Function:
C   calculates vertical diffusion

C Subroutines and Functions Called:
C   INIT3, SEC2TIME, TIME2SEC, WRITE3, NEXTIME,
C   M3EXIT, EDDYX, TRI, MATRIX, PA_UPDATE_EMIS, PA_UPDATE_DDEP

C Revision History:
C   Analogous to VDIFFACM2
C 11 Apr 13 J.Young: fix double adjustment of conc for DDBF in heterogeneous HONO
C           if-then-else clauses; eliminate some white space
C 13 May 13 J.Young: access met data from VDIFF_MET module
C                    change CRANKP to THBAR, CRANKQ to THETA
C 25 May 13 J.Young: re-do the acm/eddy algorithm for computational efficiency
C 30 Apr 14 J.Young: switch THBAR and THETA
C  2 May 14 J.Pleim, J.Young: replace the banded tridiagonal matrix solver for the
C           convective PBL, with the ACM1 matrix solver followed by the tridiagonal
C           matrix solver
C   30 May 14 J.Young: split vdiff calculation out of vdiff proc.
C   07 Nov 14 J.Bash: Updated for the ASX_DATA_MOD shared data module. 
C-----------------------------------------------------------------------

      USE CGRID_SPCS          ! CGRID mechanism species
      USE GRID_CONF
      USE EMIS_DEFN          
      USE DEPV_DEFN
      USE ASX_DATA_MOD
      USE VDIFF_MAP
      USE UTILIO_DEFN
      USE BIDI_MOD

      IMPLICIT NONE

      INCLUDE SUBST_FILES_ID  ! file name parameters

      CHARACTER( 120 ) :: XMSG = ' '

C Arguments:
      REAL, INTENT( IN )    :: DTSEC                ! model time step in seconds
C--- SEDDY is strictly an input, but it gets modified here
      REAL, INTENT( INOUT ) :: SEDDY    ( :,:,: )   ! flipped EDDYV
      REAL, INTENT( INOUT ) :: DDEP     ( :,:,: )   ! ddep accumulator
      REAL, INTENT( INOUT ) :: ICMP     ( :,:,: )   ! component flux accumlator 
      REAL, INTENT( INOUT ), OPTIONAL :: DDEPJ    ( :,:,:,: ) ! ddep for mosaic
      REAL, INTENT( INOUT ), OPTIONAL :: DDEPJ_FST( :,:,:,: ) ! ddep for stomtal/cuticular pathway
      REAL, INTENT( INOUT ) :: CNGRD    ( :,:,:,: ) ! cgrid replacement

C Parameters:

C explicit, THETA = 0, implicit, THETA = 1     ! Crank-Nicholson: THETA = 0.5
      REAL, PARAMETER :: THETA = 0.5,
     &                   THBAR = 1.0 - THETA

C External Functions: None

C Local Variables:

      CHARACTER( 16 ), SAVE :: PNAME = 'VDIFFACMX'

      LOGICAL, SAVE :: FIRSTIME = .TRUE.

      REAL, ALLOCATABLE, SAVE :: DD_FAC     ( : )   ! combined subexpression
      REAL, ALLOCATABLE, SAVE :: DDBF       ( : )   ! secondary DDEP
      REAL, ALLOCATABLE, SAVE :: CMPF       ( : )   ! intermediate CMP
      REAL, ALLOCATABLE, SAVE :: CONC       ( :,: ) ! secondary CGRID expression
      REAL, ALLOCATABLE, SAVE :: EMIS       ( :,: ) ! emissions subexpression
      REAL        DTDENS1                       ! DT * layer 1 air density

C ACM Local Variables
      REAL        DFACP, DFACQ
      REAL        RP, RQ
      REAL, ALLOCATABLE, SAVE :: DEPVCR     ( : )   ! dep vel in one cell
      REAL, ALLOCATABLE, SAVE :: EFAC1 ( : )
      REAL, ALLOCATABLE, SAVE :: EFAC2 ( : )
      REAL, ALLOCATABLE, SAVE :: POL   ( : )    ! prodn/lossrate = PLDV/DEPV
      REAL        PLDV_HONO                     ! PLDV for HONO
      REAL        DEPV_NO2                      ! dep vel of NO2
      REAL        DEPV_HNO3                     ! dep vel of HNO3
      INTEGER, SAVE :: NO2_HIT, HONO_HIT, HNO3_HIT, NO2_MAP, HNO3_MAP
      INTEGER, SAVE :: NH3_HIT
      REAL        DTS

      INTEGER, SAVE :: LOGDEV
      INTEGER     ASTAT
      INTEGER     C, R, L, S, V, I, J           ! loop induction variables
      INTEGER     MDATE, MTIME                  ! internal simulation date&time

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN

         FIRSTIME = .FALSE.
         LOGDEV = INIT3()

         MDATE = 0; MTIME = 0

C set auxiliary depv arrays

         ALLOCATE ( DD_FAC( N_SPC_DEPV  ),
     &              DDBF  ( N_SPC_DEPV ),
     &              DEPVCR( N_SPC_DEPV ),
     &              EFAC1 ( N_SPC_DEPV ),
     &              EFAC2 ( N_SPC_DEPV ),
     &              POL   ( N_SPC_DEPV ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating DD_FAC, DDBF, DEPVCR, EFAC1, EFAC2, or POL'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( CMPF( LCMP ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CMPF'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF

         ALLOCATE ( CONC( N_SPC_DIFF,NLAYS ),
     &              EMIS( N_SPC_DIFF,NLAYS ), STAT = ASTAT )
         IF ( ASTAT .NE. 0 ) THEN
            XMSG = 'Failure allocating CONC or EMIS'
            CALL M3EXIT( PNAME, MDATE, MTIME, XMSG, XSTAT1 )
         END IF
         CONC = 0.0; EMIS = 0.0   ! array assignment

         HONO_HIT = 0; HNO3_HIT = 0; NO2_HIT  = 0; NH3_HIT = 0
                       HNO3_MAP = 0; NO2_MAP  = 0
         DO V = 1, N_SPC_DEPV
            IF ( DV2DF_SPC( V ) .EQ. 'NO2' ) THEN
               NO2_HIT = V
               NO2_MAP = DV2DF( V )
            ELSE IF ( DV2DF_SPC( V ) .EQ. 'HONO' ) THEN
               HONO_HIT = V
            ELSE IF ( DV2DF_SPC( V ) .EQ. 'HNO3' ) THEN
               HNO3_HIT = V
               HNO3_MAP = DV2DF( V )
            ELSE IF ( DV2DF_SPC( V ) .EQ. 'NH3' ) THEN
               NH3_HIT = V
            END IF
         END DO
 
      END IF   !  if Firstime

C ------------------------------------------- Row, Col LOOPS -----------

      DTS = DTSEC

      DO 345 R = 1, MY_NROWS
      DO 344 C = 1, MY_NCOLS

         DTDENS1 = DTS * Met_Data%DENS1( C,R )
         DFACP = THETA * DTS
         DFACQ = THBAR * DTS

         DO L = 1, NLAYS
            DO V = 1, N_SPC_DIFF
               CONC( V,L ) = CNGRD( DIFF_MAP( V ),L,C,R )
            END DO
         END DO

         EMIS = 0.0      ! array assignment
         DO L = 1, EMLAYS
            DO V = 1, N_SPC_DIFF
               EMIS( V,L ) = DTS * VDEMIS( DF2EM( V ),L,C,R )
            END DO
         END DO

         RP = DFACP * Met_Data%RDEPVHT( C,R )
         RQ = DFACQ * Met_Data%RDEPVHT( C,R )
         DO V = 1, N_SPC_DEPV
            DDBF( V )   = DDEP( V,C,R )
            DEPVCR( V ) = DEPV( V,C,R )
            DD_FAC( V ) = DTDENS1 * DD_CONV( V ) * DEPVCR( V )
            EFAC1 ( V ) = EXP( -DEPVCR( V ) * RP )
            EFAC2 ( V ) = EXP( -DEPVCR( V ) * RQ )
            POL   ( V ) = PLDV( V,C,R ) / DEPVCR( V )
            IF ( ABFLUX .AND. V .EQ. NH3_HIT ) THEN
               DO I = 1, LCMP          
                  CMPF( I ) = ICMP( I,C,R )
               END DO
            END IF            
         END DO
         PLDV_HONO = PLDV( HONO_HIT,C,R )

C-----------------------------------------------------------------------

            DO V = 1, N_SPC_DEPV

C --------- HET HONO RX -----------------

C Use special treatment for HNO3
C HNO3 produced via the heterogeneous reaction sticks on surfaces and
C is accounted as depositional loss; calculate increased deposition loss
               IF ( V .EQ. HNO3_HIT ) THEN
                  S = HNO3_MAP
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC1( V )
                  DEPV_HNO3 = DEPVCR( V ) + PLDV_HONO / CONC( NO2_MAP,1 )
                  DD_FAC( V ) = DTDENS1 * DD_CONV( V ) * DEPV_HNO3
                  DDBF( V ) = DDBF( V ) + THETA * DD_FAC( V ) * CONC( S,1 )

C Use special treatment for NO2
C Loss of NO2 via the heterogeneous reaction is accounted for as an additional
C depositional loss. Add the loss of NO2 via the heterogeneous reaction
C to the regular deposition velocity (increased dep. vel.).  This will
C reduce the NO2 conc. in the atmosphere without affecting the depositional loss.
               ELSE IF ( V .EQ. NO2_HIT ) THEN
                  S = NO2_MAP
                  DEPV_NO2 = DEPVCR( V ) + 2.0 * PLDV_HONO / CONC( S,1 )
                  EFAC1 ( V ) = EXP( -DEPV_NO2 * RP )
                  EFAC2 ( V ) = EXP( -DEPV_NO2 * RQ )
                  POL   ( V ) = PLDV( V,C,R ) / DEPV_NO2
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC1( V )
                  DDBF( V ) = DDBF( V ) + THETA * DD_FAC( V ) * CONC( S,1 )
  
C --------- END of HET HONO RX ----------

               ELSE
                  S = DV2DF( V )
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC1( V )
                  DDBF( V ) = DDBF( V ) + THETA * DD_FAC( V ) * CONC( S,1 )

                  IF ( ABFLUX .AND. V .EQ. NH3_HIT ) THEN
                     DO I = 1, LCMP        
                        CMPF( I ) = CMPF( I ) + THETA * CMP( I,C,R ) * DD_CONV( V ) * DTDENS1
                     END DO
                  END IF
               END IF

            END DO
    
C --------- ADD EMISSIONS ---------------

            DO L = 1, NLAYS
               DO V = 1, N_SPC_DIFF
                  CONC( V,L ) = CONC( V,L ) + EMIS( V,L )
               END DO
            END DO

C --------- END EMISSIONS ---------------

            DO V = 1, N_SPC_DEPV

C --------- HET HONO RX -----------------

               IF ( V .EQ. HNO3_HIT ) THEN
                  S = HNO3_MAP
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC2( V )
                  DDBF( V ) = DDBF( V ) + THBAR * DD_FAC( V ) * CONC( S,1 )

               ELSE IF ( V .EQ. NO2_HIT ) THEN
                  S = NO2_MAP
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC2( V )
                  DDBF( V ) = DDBF( V ) + THBAR * DD_FAC( V ) * CONC( S,1 )

C --------- END of HET HONO RX ----------

               ELSE
                  S = DV2DF( V )
                  CONC( S,1 ) = POL( V ) + ( CONC( S,1 ) - POL( V ) ) * EFAC2( V )
                  DDBF( V ) = DDBF( V ) + THBAR * DD_FAC( V ) * CONC( S,1 )

                  IF ( ABFLUX .AND. V .EQ. NH3_HIT ) THEN
                     DO I = 1, LCMP
                        CMPF( I ) = CMPF( I ) + THBAR * CMP( I,C,R ) * DD_CONV( V ) * DTDENS1   
                     END DO
                  END IF

               END IF

            END DO
    
         DO L = 1, NLAYS
            DO V = 1, N_SPC_DIFF
               CNGRD( DIFF_MAP( V ),L,C,R ) = CONC( V,L )
            END DO
         END DO

         DO V = 1, N_SPC_DEPV
            DDEP( V,C,R ) = DDBF( V )
         END DO
 
         IF ( ABFLUX ) THEN
            DO I = 1, LCMP
               ICMP( I,C,R ) = CMPF( I )
            END DO
         END IF
 
344   CONTINUE         !  end loop on col C
345   CONTINUE         !  end loop on row R

      RETURN
      END
