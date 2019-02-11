module m3utilio

  implicit none

  INCLUDE 'PARMS3.EXT'        !  I/O API PARAMETERs
  INCLUDE 'FDESC3.EXT'        !  I/O API file headers

!...........   Token-values ("magic numbers"):

  integer, parameter :: default_logdev = 6

!...........   EXTERNAL Functions:

  INTEGER           :: INTERP3
  INTEGER, EXTERNAL :: ENVINT
  INTEGER, EXTERNAL :: FINDC
  INTEGER, EXTERNAL :: INDEX1, SEC2TIME, TIME2SEC, GETEFILE
  INTEGER, EXTERNAL :: JULIAN, JUNIT
  INTEGER, EXTERNAL :: PROMPTFFILE
  INTEGER, EXTERNAL :: SECSDIFF
  INTEGER, EXTERNAL :: WKDAY
  LOGICAL, EXTERNAL :: DSCGRID
  LOGICAL, EXTERNAL :: ENVYN
  LOGICAL, EXTERNAL :: INTERPX
  LOGICAL, EXTERNAL :: READ3, WRITE3, XTRACT3
  REAL,    EXTERNAL :: ENVREAL, POLY, STR2REAL
  CHARACTER*10, EXTERNAL :: HHMMSS
  CHARACTER*14, EXTERNAL :: MMDDYY
  CHARACTER*16, EXTERNAL :: PROMPTMFILE

!...........   INTERFACE Blocks:

  INTERFACE
    CHARACTER*2 FUNCTION CRLF()
    END FUNCTION CRLF
  END INTERFACE

  INTERFACE
    LOGICAL FUNCTION CURRSTEP ( JDATE, JTIME,         &
                                SDATE, STIME, TSTEP,  &
                                CDATE, CTIME )
      INTEGER, INTENT(IN   ) :: SDATE, STIME    !  starting d&t for the sequence
      INTEGER, INTENT(IN   ) :: TSTEP           !  time step for the sequence
      INTEGER, INTENT(IN   ) :: JDATE, JTIME    !  d&t requested
      INTEGER, INTENT(  OUT) :: CDATE, CTIME    !  d&t for timestep of JDATE:JTIME
    END FUNCTION CURRSTEP
  END INTERFACE

  INTERFACE           !  get file-description for FNAME
    LOGICAL FUNCTION  DESC3( FNAME )
      CHARACTER*(*), INTENT(IN   ) :: FNAME   !  file name
    END FUNCTION  DESC3
  END INTERFACE

  INTERFACE
    LOGICAL FUNCTION SETLAM( A, B, C, X, Y )
      REAL, INTENT(IN   ) :: A          !  first secant latitude
      REAL, INTENT(IN   ) :: B          !  second secant latitude.  B > A
      REAL, INTENT(IN   ) :: C          !  central meridian
      REAL, INTENT(IN   ) :: X          !  Lambert easting  in meters
      REAL, INTENT(IN   ) :: Y          !  Lambert northing in meters
    END FUNCTION SETLAM
  END INTERFACE

  INTERFACE
    SUBROUTINE  M3MESG( MESSAGE )
      CHARACTER*(*), INTENT(IN   ) :: MESSAGE
    END SUBROUTINE  M3MESG
  END INTERFACE

  INTERFACE
    SUBROUTINE  M3MSG2( MESSAGE )
      CHARACTER*(*), INTENT(IN   ) :: MESSAGE
    END SUBROUTINE  M3MSG2
  END INTERFACE

  public

contains

  integer function init3()
    init3 = default_logdev
  end function init3

  LOGICAL FUNCTION  OPEN3( FNAME, FSTATUS, PGNAME )
    CHARACTER(LEN=*), INTENT(IN) :: FNAME
    INTEGER,          INTENT(IN) :: FSTATUS
    CHARACTER(LEN=*), INTENT(IN) :: PGNAME
    OPEN3 = .TRUE.
  END FUNCTION  OPEN3

  LOGICAL FUNCTION CLOSE3( FNAME )
    CHARACTER*(*), INTENT(IN   ) :: FNAME   !  l
    CLOSE3 = .TRUE.
  END FUNCTION CLOSE3

  subroutine m3parag( nmesg, msgs )
    integer,          intent(in) :: nmesg
    character(len=*), intent(in) :: msgs( nmesg )

    ! -- local variables
    integer :: n

    do n = 1, nmesg
      call m3mesg( msgs(n) )
    end do
  end subroutine m3parag

  LOGICAL FUNCTION SETENVVAR( LNAME, VALUE )
    CHARACTER(LEN=*), INTENT( IN ) ::   LNAME
    CHARACTER(LEN=*), INTENT( IN ) ::   VALUE
    SETENVVAR = .TRUE.
  END FUNCTION SETENVVAR

end module m3utilio
