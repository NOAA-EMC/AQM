subroutine m3warn( caller, jdate, jtime, msgtxt )
  use ESMF
  character(len=*), intent(in) :: caller
  integer,          intent(in) :: jdate, jtime
  character(len=*), intent(in) :: msgtxt

  write(0, '("WARNING: ",a)') trim(caller) // ':' // trim(msgtxt)
  flush 0

  call ESMF_LogWrite(trim(caller) // ':' // trim(msgtxt), ESMF_LOGMSG_WARNING)
end subroutine m3warn

