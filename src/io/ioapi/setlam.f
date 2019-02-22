        LOGICAL FUNCTION SETLAM( A, B, C, X, Y )

        IMPLICIT NONE

C...........   ARGUMENTS:

        REAL, INTENT(IN   ) :: A          !  first secant latitude
        REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
        REAL, INTENT(IN   ) :: C          !  central meridian
        REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
        REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters

C.......   Scratch variables:

        CHARACTER*256   MESG

C.......   Check validity of input parameters:

        IF ( A .LT. -90.0 ) THEN
            WRITE( MESG, 94020 ) 'Bad first latitude A =', A
            CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( A .GT. B ) THEN
            WRITE( MESG, 94020 ) 'Bad latitudes A ', A, 'B =', B
            CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( B .GE.   90.0 ) THEN
            WRITE( MESG, 94020 ) 'Bad second latitude B =', B
            CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( C .LT. -180.0 ) THEN
            WRITE( MESG, 94020 ) 'Bad central longitude C =', C
            CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( C .GT.  180.0 ) THEN
            WRITE( MESG, 94020 ) 'Bad central longitude C =', C
            CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( X .LT. -180.0 ) THEN
            WRITE( MESG, 94020 ) 'Bad origin longitude X =', X
            CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( X .GT.  180.0 ) THEN
            WRITE( MESG, 94020 ) 'Bad origin longitude X =', X
            CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( Y .LT. -90.0 ) THEN
            WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        ELSE IF ( Y .GE.   90.0 ) THEN
            WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
            CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
            SETLAM = .FALSE.
            RETURN
        END IF

        SETLAM = .TRUE.

94020   FORMAT( A, 1PG14.5, :, 2X )

        END FUNCTION SETLAM
