  subroutine m3mesg( message )
    use ESMF
    character(len=*), intent(in) :: message

    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
  end subroutine m3mesg
