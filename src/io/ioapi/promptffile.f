 
        INTEGER FUNCTION PROMPTFFILE( PROMPT, 
     &                                RDONLY, FMTTED, DEFAULT, CALLER )

        IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:
        
        CHARACTER*(*), INTENT(IN   ) :: PROMPT         !  prompt for user
        LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
        LOGICAL      , INTENT(IN   ) :: FMTTED         !  TRUE iff file should be formatted
        CHARACTER*(*), INTENT(IN   ) :: DEFAULT        !  default logical file name
        CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging messages

        PROMPTFFILE = -1
        RETURN

        END FUNCTION PROMPTFFILE

