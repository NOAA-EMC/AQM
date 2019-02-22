subroutine m3exit( caller, jdate, jtime, msgtxt, xstat )
  use ESMF
  character(len=*),  intent(in) :: caller
  integer,           intent(in) :: jdate, jtime
  character(len=*),  intent(in) :: msgtxt
  integer,           intent(in) :: xstat

  call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
    msg=trim(caller) // ':' // trim(msgtxt))
  call ESMF_Finalize(endflag=ESMF_END_ABORT)
end subroutine m3exit
