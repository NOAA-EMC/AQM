logical function envyn(name, description, defaultval, status)

  use aqm_model_mod, only : aqm_config_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check

  character(len=*), intent(in)  :: name
  character(len=*), intent(in)  :: description
  logical,          intent(in)  :: defaultval
  integer,          intent(out) :: status

  ! -- local variables
  integer :: deCount, localrc
  type(aqm_config_type), pointer :: config => null()

  ! -- begin
  envyn = defaultval
  status = 0

  call aqm_model_get(deCount=deCount, config=config, rc=localrc)
  if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) then
    status = -2
    return
  end if

  if (deCount < 1) return

  select case (trim(name))
    case ('CTM_DEPVFILE')
      envyn = config % ctm_depvfile
    case ('CTM_PMDIAG')
      envyn = config % ctm_pmdiag
    case ('CTM_PHOTODIAG')
      envyn = config % ctm_photodiag
    case default
      status = -2
  end select

end function envyn

subroutine nameval(name, eqname)

  use aqm_model_mod, only : aqm_config_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check

  character(len=*), intent(in)  :: name
  character(len=*), intent(out) :: eqname

  ! -- local variables
  integer :: deCount, localrc
  type(aqm_config_type), pointer :: config => null()

  ! -- begin
  eqname = ""

  call aqm_model_get(deCount=deCount, config=config, rc=localrc)
  if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) return

  if (deCount < 1) return

  select case (trim(name))
    case ('gc_matrix_nml')
      eqname = config % gc_matrix_nml
    case ('ae_matrix_nml')
      eqname = config % ae_matrix_nml
    case ('nr_matrix_nml')
      eqname = config % nr_matrix_nml
    case ('tr_matrix_nml')
      eqname = config % tr_matrix_nml
    case default
      ! -- nothing to do
  end select
  
end subroutine nameval
