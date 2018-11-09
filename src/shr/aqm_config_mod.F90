module aqm_config_mod

  use aqm_rc_mod
  use aqm_types_mod, only : AQM_MAXSTR
  use aqm_comm_mod,  only : aqm_comm_bcast, aqm_comm_isroot

  implicit none

  character(len=*), parameter :: aqm_file_nml = 'input.nml'

  ! -- data structure for configuration options
  type aqm_config_type
    sequence
    character(len=AQM_MAXSTR) :: ae_matrix_nml = ""
    character(len=AQM_MAXSTR) :: gc_matrix_nml = ""
    character(len=AQM_MAXSTR) :: nr_matrix_nml = ""
    character(len=AQM_MAXSTR) :: tr_matrix_nml = ""
    character(len=AQM_MAXSTR) :: csqy_data     = ""
    character(len=AQM_MAXSTR) :: optics_data   = ""
    character(len=AQM_MAXSTR) :: omi           = ""
    logical                   :: ctm_photodiag = .false.
    logical                   :: ctm_pmdiag    = .false.
    logical                   :: ctm_depvfile  = .false.
    logical                   :: run_aero      = .false.
    integer                   :: spcs_start_index = 4
  end type aqm_config_type

  private

  public :: aqm_config_type

  public :: aqm_config_read

contains

  subroutine aqm_config_read(config, rc)

    type(aqm_config_type), intent(inout) :: config
    integer, optional,     intent(out)   :: rc

    ! -- local variables
    integer, parameter :: unit = 200

    integer                :: localrc, iostat
    logical                :: lbuffer(4)
    character(AQM_MAXSTR)  :: sbuffer(7)

    ! -- variables in input namelist
    character(len=AQM_MAXSTR) :: ae_matrix_nml
    character(len=AQM_MAXSTR) :: gc_matrix_nml
    character(len=AQM_MAXSTR) :: nr_matrix_nml
    character(len=AQM_MAXSTR) :: tr_matrix_nml
    character(len=AQM_MAXSTR) :: csqy_data
    character(len=AQM_MAXSTR) :: optics_data
    character(len=AQM_MAXSTR) :: omi
    logical                   :: ctm_photodiag
    logical                   :: ctm_pmdiag
    logical                   :: ctm_depvfile
    logical                   :: run_aero

    namelist /aqm_nml/ &
      ae_matrix_nml, &
      gc_matrix_nml, &
      nr_matrix_nml, &
      tr_matrix_nml, &
      csqy_data,     &
      optics_data,   &
      omi,           &
      ctm_photodiag, &
      ctm_pmdiag,    &
      ctm_depvfile,  &
      run_aero

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- set defaults
    ae_matrix_nml = ""
    gc_matrix_nml = ""
    nr_matrix_nml = ""
    tr_matrix_nml = ""
    csqy_data     = ""
    optics_data   = ""
    omi           = ""
    ctm_photodiag = .false.
    ctm_pmdiag    = .false.
    ctm_depvfile  = .false.
    run_aero      = .false.

    ! -- read aqm configuration namelist
    if (aqm_comm_isroot()) then
      open(unit, file=aqm_file_nml, form='formatted', status='old', iostat=iostat)
    end if
    call aqm_comm_bcast(iostat, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
    if (aqm_rc_test((iostat /= 0), msg="Failed to open namelist file: "//aqm_file_nml, &
        file=__FILE__, line=__LINE__, rc=rc)) return
    if (aqm_comm_isroot()) then
      rewind(unit)
      read(unit, nml=aqm_nml, iostat=iostat )
      close(unit)
      write(6, nml=aqm_nml)
    end if
    call aqm_comm_bcast(iostat, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
    if (aqm_rc_test((iostat /= 0), msg="Failed to read &aqm namelist in: "//aqm_file_nml, &
        file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- pack strings into buffer
    sbuffer = (/ &
      ae_matrix_nml, &
      gc_matrix_nml, &
      nr_matrix_nml, &
      tr_matrix_nml, &
      csqy_data,     &
      optics_data,   &
      omi            &
    /)
    ! -- broadcast string variable
    call aqm_comm_bcast(sbuffer, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- set string values to config
    config % ae_matrix_nml = sbuffer(1)
    config % gc_matrix_nml = sbuffer(2)
    config % nr_matrix_nml = sbuffer(3)
    config % tr_matrix_nml = sbuffer(4)
    config % csqy_data     = sbuffer(5)
    config % optics_data   = sbuffer(6)
    config % omi           = sbuffer(7)

    ! -- pack logicals into buffer
    lbuffer = (/ &
      ctm_photodiag, &
      ctm_pmdiag,    &
      ctm_depvfile,  &
      run_aero       &
    /)
    ! -- broadcast string variable
    call aqm_comm_bcast(lbuffer, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- set string values to config
    config % ctm_photodiag = lbuffer(1)
    config % ctm_pmdiag    = lbuffer(2)
    config % ctm_depvfile  = lbuffer(3)
    config % run_aero      = lbuffer(4)

  end subroutine aqm_config_read

end module aqm_config_mod
