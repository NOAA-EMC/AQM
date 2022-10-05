module aqm_rc_mod

  implicit none

  integer, parameter :: AQM_RC_SUCCESS = 0
  integer, parameter :: AQM_RC_FAILURE = -1
  integer  :: mylocalpet
  public

contains

  logical function aqm_rc_check(rcToCheck, msg, file, line, rc)

    integer,                    intent(in)  :: rcToCheck
    character(len=*), optional, intent(in)  :: msg
    character(len=*), optional, intent(in)  :: file
    integer,          optional, intent(in)  :: line
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer, parameter :: maxMsgLen = 255
    character(len=maxMsgLen) :: descr  = "Internal error"
    character(len=maxMsgLen) :: label  = ""

    ! -- begin
    if (present(rc)) rc = rcToCheck

    aqm_rc_check = (rcToCheck /= AQM_RC_SUCCESS)

    if (aqm_rc_check) then
      if (present(msg))  descr = msg
      if (present(line)) write(label, '(i0)') line
      if (present(file)) label = trim(file) // ":" // trim(label)
      if (len_trim(label) > 0) label = " " // trim(label)
      write(0,'("ERROR:",a,1x,a)') trim(label), trim(descr)
      flush(0)
      write(6,'("ERROR:",a,1x,a)') trim(label), trim(descr)
      flush(6)
    end if

  end function aqm_rc_check

  subroutine aqm_rc_set(rcToCheck, msg, file, line, rc)

    integer,                    intent(in)  :: rcToCheck
    character(len=*), optional, intent(in)  :: msg
    character(len=*), optional, intent(in)  :: file
    integer,          optional, intent(in)  :: line
    integer,          optional, intent(out) :: rc

    ! -- local variables
    logical :: flag

    ! -- begin
    flag = aqm_rc_check(rcToCheck, msg=msg, file=file, line=line, rc=rc)

  end subroutine aqm_rc_set

  logical function aqm_rc_test(flagToTest, msg, file, line, rc)

    logical,                    intent(in)  :: flagToTest
    character(len=*), optional, intent(in)  :: msg
    character(len=*), optional, intent(in)  :: file
    integer,          optional, intent(in)  :: line
    integer,          optional, intent(out) :: rc

    ! -- local variables

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    aqm_rc_test = flagToTest

    if (flagToTest) call aqm_rc_set(AQM_RC_FAILURE, msg=msg, file=file, line=line, rc=rc)

  end function aqm_rc_test

end module aqm_rc_mod
