
        CHARACTER*16 FUNCTION PROMPTMFILE( PROMPT, FMODE, 
     &                                     DEFAULT, CALLER )

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        
        CHARACTER*(*), INTENT(IN   ) :: PROMPT         !  prompt for user
        INTEGER      , INTENT(IN   ) :: FMODE          !  file opening-mode
        CHARACTER*(*), INTENT(IN   ) :: DEFAULT        !  default logical file name
        CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging messages

        PROMPTMFILE = DEFAULT

        END FUNCTION PROMPTMFILE

