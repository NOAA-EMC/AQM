 
      LOGICAL FUNCTION READ3( FNAME, VNAME, LAYER, JDATE,JTIME, BUFFER)

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
        INTEGER      , INTENT(IN   ) :: LAYER           !  layer number, or 0
        INTEGER      , INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
        INTEGER      , INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
        REAL         , INTENT(  OUT) :: BUFFER(*)       !  input buffer array

        READ3 = .TRUE.

      END FUNCTION READ3
