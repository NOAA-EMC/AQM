module m3utilio

  use ESMF

  implicit none

  integer, parameter :: default_logdev = 6
  integer, parameter :: fsread3 = 0
  integer :: xstat1, xstat2, xstat3

  private

  public :: init3, m3exit, m3mesg, m3parag, m3warn, open3
  public :: xstat1, xstat2, xstat3
  public :: fsread3

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

  subroutine m3exit( caller, jdate, jtime, msgtxt, xstat )
    character(len=*),  intent(in) :: caller
    integer,           intent(in) :: jdate, jtime
    character(len=*),  intent(in) :: msgtxt
    integer, optional, intent(in) :: xstat

    call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg=trim(caller) // ':' // trim(msgtxt))
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end subroutine m3exit

  subroutine m3mesg( message )
    character(len=*), intent(in) :: message

    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
  end subroutine m3mesg

  subroutine m3parag( nmesg, msgs )
    integer,          intent(in) :: nmesg
    character(len=*), intent(in) :: msgs( nmesg )

    ! -- local variables
    integer :: n

    do n = 1, nmesg
      call m3mesg( msgs(n) )
    end do
  end subroutine m3parag

  subroutine m3warn( caller, jdate, jtime, msgtxt )
    character(len=*), intent(in) :: caller
    integer,          intent(in) :: jdate, jtime
    character(len=*), intent(in) :: msgtxt

    call ESMF_LogWrite(trim(caller) // ':' // trim(msgtxt), ESMF_LOGMSG_WARNING)
  end subroutine m3warn

end module m3utilio
