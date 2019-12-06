! ---------------------------------------------------
! Replacement subroutine for VDIFFACMX:
! - Updates concentrations array with input emissions
! ---------------------------------------------------

SUBROUTINE VDIFFACMX ( DTSEC, SEDDY, DDEP, ICMP, DDEPJ, DDEPJ_FST, CNGRD )

  USE HGRD_DEFN
  USE EMIS_DEFN          
  USE VDIFF_MAP

  IMPLICIT NONE

! Arguments:
  REAL, INTENT( IN )    :: DTSEC                ! model time step in seconds
!--- SEDDY is strictly an input, but it gets modified here
  REAL, INTENT( INOUT ) :: SEDDY    ( :,:,: )   ! flipped EDDYV
  REAL, INTENT( INOUT ) :: DDEP     ( :,:,: )   ! ddep accumulator
  REAL, INTENT( INOUT ) :: ICMP     ( :,:,: )   ! component flux accumlator 
  REAL, INTENT( INOUT ), OPTIONAL :: DDEPJ    ( :,:,:,: ) ! ddep for mosaic
  REAL, INTENT( INOUT ), OPTIONAL :: DDEPJ_FST( :,:,:,: ) ! ddep for stomtal/cuticular pathway
  REAL, INTENT( INOUT ) :: CNGRD    ( :,:,:,: ) ! cgrid replacement

! -- local variables
  INTEGER :: R, C, L, V

! -- begin
  DO R = 1, MY_NROWS
    DO C = 1, MY_NCOLS
      DO L = 1, EMLAYS
        DO V = 1, N_SPC_DIFF
          CNGRD( DIFF_MAP( V ),L,C,R ) = &
            CNGRD( DIFF_MAP( V ),L,C,R ) + DTSEC * VDEMIS( DF2EM( V ),L,C,R )
        END DO
      END DO
    END DO
  END DO

END SUBROUTINE VDIFFACMX
