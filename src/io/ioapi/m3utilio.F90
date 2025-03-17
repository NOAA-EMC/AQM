module m3utilio

  implicit none

  INCLUDE 'PARMS3.EXT'        !  I/O API PARAMETERs
  INCLUDE 'FDESC3.EXT'        !  I/O API file headers

!...........   Token-values ("magic numbers"):

  integer, parameter :: default_logdev = 6

  logical :: M3IO_ENABLED = .TRUE.

!...........   EXTERNAL Functions:

  INTEGER           :: INTERP3
  INTEGER, EXTERNAL :: ENVINT
  INTEGER, EXTERNAL :: FINDC
  INTEGER, EXTERNAL :: SEC2TIME, GETEFILE
  INTEGER, EXTERNAL :: JULIAN
  INTEGER, EXTERNAL :: PROMPTFFILE
  INTEGER, EXTERNAL :: SECSDIFF
  INTEGER, EXTERNAL :: WKDAY
  LOGICAL, EXTERNAL :: DSCGRID
  LOGICAL, EXTERNAL :: ENVYN
  LOGICAL, EXTERNAL :: INTERPX
  LOGICAL, EXTERNAL :: XTRACT3
  REAL,    EXTERNAL :: ENVREAL, POLY
  CHARACTER*14, EXTERNAL :: MMDDYY

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

  INTERFACE
    SUBROUTINE DAYMON( JDATE, MNTH, MDAY )
      INTEGER, INTENT(IN   ) :: JDATE     !  Julian date, format YYYYDDD = 1000*Year + Day
      INTEGER, INTENT(  OUT) :: MNTH    !  month (1...12)
      INTEGER, INTENT(  OUT) :: MDAY    !  day-of-month (1...28,29,30,31)
    END SUBROUTINE  DAYMON
  END INTERFACE

  INTERFACE           !  get file-description for FNAME
    LOGICAL FUNCTION  DESC3( FNAME )
      CHARACTER*(*), INTENT(IN   ) :: FNAME   !  file name
    END FUNCTION  DESC3
  END INTERFACE

  INTERFACE
    CHARACTER*10 FUNCTION HHMMSS ( JTIME )
      INTEGER, INTENT(IN) :: JTIME
    END FUNCTION HHMMSS
  END INTERFACE

!  INTERFACE
!    INTEGER FUNCTION INDEX1( NAME, N, NLIST )
!      CHARACTER*(*), INTENT(IN   ) :: NAME        !  Character string being searched for
!      INTEGER      , INTENT(IN   ) :: N           !  Length of array to be searched
!      CHARACTER*(*), INTENT(IN   ) :: NLIST(*)    !  array to be searched
!    END FUNCTION INDEX1
!  END INTERFACE

  INTERFACE INDEXKEY

   INTEGER FUNCTION INDEX1( NAME, N, NLIST )
      CHARACTER*(*), INTENT(IN   ) :: NAME        !  Character string being searched for
      INTEGER      , INTENT(IN   ) :: N           !  Length of array to be searched
      CHARACTER*(*), INTENT(IN   ) :: NLIST(*)    !  array to be searched
   END FUNCTION INDEX1

   INTEGER FUNCTION INDEXINT1( IKEY, N, NLIST )
      INTEGER, INTENT(IN   ) :: IKEY        !  integer being searched for
      INTEGER, INTENT(IN   ) :: N           !  Length of array to be searched
      INTEGER, INTENT(IN   ) :: NLIST(*)    !  array to be searched
   END FUNCTION INDEXINT1

   INTEGER FUNCTION INDEXL1( LKEY, N, NLIST )
     INTEGER*8, INTENT(IN   ) :: LKEY        !  integer being searched for
     INTEGER,   INTENT(IN   ) :: N           !  Length of array to be searched
     INTEGER*8, INTENT(IN   ) :: NLIST(*)    !  array to be searched
   END FUNCTION INDEXL1

  END INTERFACE       !!  indexkey


  INTERFACE
    INTEGER FUNCTION JUNIT()
    END FUNCTION JUNIT
  END INTERFACE

  INTERFACE
    CHARACTER*16  FUNCTION PROMPTMFILE( PROMPT,  FMODE, DEFAULT, CALLER )
      CHARACTER*(*), INTENT(IN   ) :: PROMPT         !  prompt for user
      INTEGER      , INTENT(IN   ) :: FMODE          !  file opening-mode
      CHARACTER*(*), INTENT(IN   ) :: DEFAULT        !  default logical file name
      CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging messages
    END FUNCTION  PROMPTMFILE
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
    REAL FUNCTION STR2REAL( STRING )
      CHARACTER*(*), INTENT(IN   ) :: STRING
    END FUNCTION STR2REAL
  END INTERFACE

  INTERFACE
    INTEGER  FUNCTION  TIME2SEC ( TIME )
    INTEGER, INTENT(IN   ) :: TIME    !  formatted HHMMSS
    END FUNCTION  TIME2SEC
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

  INTERFACE
    SUBROUTINE M3EXIT( CALLER, JDATE, JTIME, MSGTXT, ISTAT )
      CHARACTER*(*), INTENT(IN   ) :: CALLER          !  name of the caller
      INTEGER      , INTENT(IN   ) :: JDATE, JTIME    !  model date&time for the error
      CHARACTER*(*), INTENT(IN   ) :: MSGTXT          !  error message
      INTEGER      , INTENT(IN   ) :: ISTAT           !  exit status for program
    END SUBROUTINE M3EXIT
  END INTERFACE

  INTERFACE READ3
    MODULE PROCEDURE READ3_INTEGER
    MODULE PROCEDURE READ3_REAL
  END INTERFACE

  INTERFACE
    LOGICAL FUNCTION WRITE3_REAL2D( FNAME, VNAME, JDATE, JTIME, BUFFER )
      CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
      CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
      INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
      INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
      REAL         , INTENT(IN   ) :: BUFFER(:,:)  !  output buffer array
    END FUNCTION WRITE3_REAL2D
    LOGICAL FUNCTION WRITE3_REAL4D( FNAME, VNAME, JDATE, JTIME, BUFFER )
      CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
      CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
      INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
      INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
      REAL         , INTENT(IN   ) :: BUFFER(:,:,:,:)  !  output buffer array
    END FUNCTION WRITE3_REAL4D
  END INTERFACE

  INTERFACE WRITE3
    MODULE PROCEDURE WRITE3_INTEGER
    MODULE PROCEDURE WRITE3_INTEGER2D
    MODULE PROCEDURE WRITE3_REAL
    MODULE PROCEDURE WRITE3_REAL1D
           PROCEDURE WRITE3_REAL2D
    MODULE PROCEDURE WRITE3_REAL3D
           PROCEDURE WRITE3_REAL4D
  END INTERFACE

  public

contains

  INTEGER FUNCTION INIT3()
    INTEGER :: IOS
    INTEGER, SAVE :: IOUNIT = -1
    
    IF (IOUNIT > 0) THEN
      INIT3 = IOUNIT
    ELSE
      IF (M3IO_ENABLED) THEN
        IOUNIT = DEFAULT_LOGDEV
      ELSE
        IOUNIT = JUNIT()
        OPEN(UNIT=IOUNIT, FILE='/dev/null', IOSTAT=IOS, ACTION='WRITE')
        IF (IOS /= 0) IOUNIT = DEFAULT_LOGDEV
      END IF
      INIT3 = IOUNIT
    END IF
  END FUNCTION INIT3

  LOGICAL FUNCTION  OPEN3( FNAME, FSTATUS, PGNAME )
    CHARACTER(LEN=*), INTENT(IN) :: FNAME
    INTEGER,          INTENT(IN) :: FSTATUS
    CHARACTER(LEN=*), INTENT(IN) :: PGNAME
    include SUBST_FILES_ID    

    OPEN3 = .TRUE.
    IF (trim(fname) == trim(LUFRAC_CRO)) THEN
      OPEN3 = .FALSE.
    END IF
  END FUNCTION  OPEN3

  LOGICAL FUNCTION CLOSE3( FNAME )
    CHARACTER*(*), INTENT(IN   ) :: FNAME   !  l
    CLOSE3 = .TRUE.
  END FUNCTION CLOSE3

  LOGICAL FUNCTION READ3_INTEGER( FNAME, VNAME, LAYER, JDATE,JTIME, BUFFER)

!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
    INTEGER      , INTENT(IN   ) :: LAYER           !  layer number, or 0
    INTEGER      , INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
    INTEGER      , INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
    INTEGER      , INTENT(  OUT) :: BUFFER(*)       !  input buffer array

    READ3_INTEGER = .TRUE.

  END FUNCTION READ3_INTEGER

  LOGICAL FUNCTION READ3_REAL( FNAME, VNAME, LAYER, JDATE,JTIME, BUFFER)

!...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
    INTEGER      , INTENT(IN   ) :: LAYER           !  layer number, or 0
    INTEGER      , INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
    INTEGER      , INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
    REAL         , INTENT(  OUT) :: BUFFER(*)       !  input buffer array

    READ3_REAL = .TRUE.

  END FUNCTION READ3_REAL

  LOGICAL FUNCTION WRITE3_REAL( FNAME, VNAME, JDATE, JTIME, BUFFER )
    CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
    INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
    INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
    REAL         , INTENT(IN   ) :: BUFFER     !  output buffer array
    WRITE3_REAL = .TRUE.
  END FUNCTION WRITE3_REAL

  LOGICAL FUNCTION WRITE3_REAL1D( FNAME, VNAME, JDATE, JTIME, BUFFER )
    CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
    INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
    INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
    REAL         , INTENT(IN   ) :: BUFFER(:)  !  output buffer array
    WRITE3_REAL1D = .TRUE.
  END FUNCTION WRITE3_REAL1D

  LOGICAL FUNCTION WRITE3_REAL3D( FNAME, VNAME, JDATE, JTIME, BUFFER )
    CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
    INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
    INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
    REAL         , INTENT(IN   ) :: BUFFER(:,:,:)  !  output buffer array
    WRITE3_REAL3D = .TRUE.
  END FUNCTION WRITE3_REAL3D

  LOGICAL FUNCTION WRITE3_INTEGER( FNAME, VNAME, JDATE, JTIME, BUFFER )
    CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
    INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
    INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
    INTEGER      , INTENT(IN   ) :: BUFFER(:)  !  output buffer array
    WRITE3_INTEGER = .TRUE.
  END FUNCTION WRITE3_INTEGER

  LOGICAL FUNCTION WRITE3_INTEGER2D( FNAME, VNAME, JDATE, JTIME, BUFFER )
    CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
    INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
    INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
    INTEGER      , INTENT(IN   ) :: BUFFER(:,:)  !  output buffer array
    WRITE3_INTEGER2D = .TRUE.
  END FUNCTION WRITE3_INTEGER2D

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
