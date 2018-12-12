      SUBROUTINE SORTIC( N, IND, TBLC )
        INTEGER,   INTENT(IN)    :: N
        INTEGER,   INTENT(INOUT) :: IND( N )
        CHARACTER, INTENT(IN)    :: TBLC( N )

        INTEGER I, INDJ, J
        CHARACTER C

        DO J = 2, N
          INDJ = IND(J)
          C = TBLC(INDJ)
          DO I = J-1, 1, -1
            IF (TBLC(IND(I)).LE.C) GOTO 2
            IND(I+1) = IND(I)
          END DO
          I = 0
 2        IND(I+1) = INDJ
        END DO

      END SUBROUTINE SORTIC
