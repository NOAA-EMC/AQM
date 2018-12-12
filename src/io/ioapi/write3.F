      LOGICAL FUNCTION WRITE3( FNAME, VNAME, JDATE, JTIME, BUFFER )
        IMPLICIT NONE
        CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
        INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
        INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
        REAL         , INTENT(IN   ) :: BUFFER(*)  !  output buffer array
        WRITE3 = .TRUE.
      END FUNCTION WRITE3
