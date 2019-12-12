module aqm_config_mod

  use aqm_rc_mod
  use aqm_types_mod,   only : AQM_MAXSTR
  use aqm_species_mod, only : aqm_species_type
  use aqm_comm_mod,    only : aqm_comm_bcast, aqm_comm_isroot

  implicit none

  character(len=*), parameter :: aqm_file_nml = 'input.nml'

  ! -- data structure for configuration options
  type aqm_config_type
    character(len=AQM_MAXSTR) :: ae_matrix_nml = ""
    character(len=AQM_MAXSTR) :: gc_matrix_nml = ""
    character(len=AQM_MAXSTR) :: nr_matrix_nml = ""
    character(len=AQM_MAXSTR) :: tr_matrix_nml = ""
    character(len=AQM_MAXSTR) :: csqy_data     = ""
    character(len=AQM_MAXSTR) :: emi_inname    = ""
    character(len=AQM_MAXSTR) :: optics_data   = ""
    character(len=AQM_MAXSTR) :: omi           = ""
    integer                   :: atm_mp        = 0
    integer                   :: ctm_tstep     = 0
    logical                   :: ctm_photodiag = .false.
    logical                   :: ctm_pmdiag    = .false.
    logical                   :: ctm_depvfile  = .false.
    logical                   :: run_aero      = .false.
    type(aqm_species_type), pointer :: species => null()
  end type aqm_config_type

  private

  public :: aqm_config_type

  public :: aqm_config_read
  public :: aqm_config_species_init

contains

  subroutine aqm_config_read(config, rc)

    type(aqm_config_type), intent(inout) :: config
    integer, optional,     intent(out)   :: rc

    ! -- local variables
    integer, parameter :: unit = 200

    integer                :: localrc, iostat
    logical                :: lbuffer(4)
    character(AQM_MAXSTR)  :: sbuffer(8)

    ! -- variables in input namelist
    character(len=AQM_MAXSTR) :: ae_matrix_nml
    character(len=AQM_MAXSTR) :: gc_matrix_nml
    character(len=AQM_MAXSTR) :: nr_matrix_nml
    character(len=AQM_MAXSTR) :: tr_matrix_nml
    character(len=AQM_MAXSTR) :: csqy_data
    character(len=AQM_MAXSTR) :: emi_inname
    character(len=AQM_MAXSTR) :: optics_data
    character(len=AQM_MAXSTR) :: omi
    integer                   :: atm_mp
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
      emi_inname,    &
      optics_data,   &
      omi,           &
      atm_mp,        &
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
    emi_inname    = ""
    optics_data   = ""
    omi           = ""
    atm_mp        = 0
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
      emi_inname,    &
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
    config % emi_inname    = sbuffer(6)
    config % optics_data   = sbuffer(7)
    config % omi           = sbuffer(8)

    ! -- broadcast integer variable
    call aqm_comm_bcast(atm_mp, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- set integer values to config
    config % atm_mp = atm_mp

    ! -- pack logicals into buffer
    lbuffer = (/ &
      ctm_photodiag, &
      ctm_pmdiag,    &
      ctm_depvfile,  &
      run_aero       &
    /)
    ! -- broadcast logical variable
    call aqm_comm_bcast(lbuffer, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- set logical values to config
    config % ctm_photodiag = lbuffer(1)
    config % ctm_pmdiag    = lbuffer(2)
    config % ctm_depvfile  = lbuffer(3)
    config % run_aero      = lbuffer(4)

  end subroutine aqm_config_read

  subroutine aqm_config_species_init(config, rc)

    type(aqm_config_type), intent(inout) :: config
    integer, optional,     intent(out)   :: rc

    ! -- local variables
    integer :: localrc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- allocate memory for data type
    if (.not.associated(config % species)) then
      allocate(config % species, stat=localrc)
      if (aqm_rc_test((localrc /= 0), &
        msg="Failed to allocate species data container", &
        file=__FILE__, line=__LINE__, rc=rc)) return
    end if

    ! -- set start tracer index depending on microphysics scheme
    ! -- used in the coupled atmospheric model
    config % species % p_aqm_beg = 0

    select case (config % atm_mp)
      case (11)
        config % species % p_aqm_beg = 8
        ! -- set hydrometeors pointers
        config % species % p_atm_qv = 1
        config % species % p_atm_qc = 2
        config % species % p_atm_qr = 3
        config % species % p_atm_qi = 4
        config % species % p_atm_qs = 5
        config % species % p_atm_qg = 6
        ! -- set ozone pointer
        config % species % p_atm_o3 = 7
      case (99)
        config % species % p_aqm_beg = 4
        ! -- set hydrometeors pointers
        config % species % p_atm_qv = 1
        config % species % p_atm_qc = 2
        ! -- set ozone pointer
        config % species % p_atm_o3 = 3
      case default
        call aqm_rc_set(AQM_RC_FAILURE, &
          msg="atm_mp option can only be 11 (GFDL) or 99 (Zhao/Carr/Sundqvist).", &
          file=__FILE__, line=__LINE__, rc=rc)
    end select

  end subroutine aqm_config_species_init

end module aqm_config_mod
