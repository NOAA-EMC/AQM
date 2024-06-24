module aqm_config_mod

  use ESMF
  use NUOPC
  use NUOPC_Model, only : NUOPC_ModelGet

  use aqm_rc_mod
  use aqm_logger_mod,  only : aqm_logger_active, &
                              aqm_logger_log,    &
                              aqm_logger_warn
  use aqm_types_mod,   only : AQM_MAXSTR
  use aqm_species_mod, only : aqm_species_type

  use m3utilio,        only : m3io_enabled

  implicit none

  ! -- data structure for configuration options
  type aqm_config_type
    character(len=AQM_MAXSTR) :: name          = ""
    character(len=AQM_MAXSTR) :: ae_matrix_nml = ""
    character(len=AQM_MAXSTR) :: gc_matrix_nml = ""
    character(len=AQM_MAXSTR) :: nr_matrix_nml = ""
    character(len=AQM_MAXSTR) :: tr_matrix_nml = ""
    character(len=AQM_MAXSTR) :: csqy_data     = ""
    character(len=AQM_MAXSTR) :: optics_data   = ""
    character(len=AQM_MAXSTR) :: omi           = ""
    character(len=AQM_MAXSTR) :: mp_map        = ""
    character(len=AQM_MAXSTR) :: ctm_stdout    = ""
    integer                   :: dy_map_beg    = 0
    integer                   :: ctm_stdate    = 0
    integer                   :: ctm_sttime    = 0
    integer                   :: ctm_tstep     = 0
    integer                   :: ctm_runlen    = 0
    logical                   :: initial_run   = .true.
    logical                   :: biosw_yn      = .false.
    logical                   :: ctm_aod       = .false.
    logical                   :: ctm_depvfile  = .false.
    logical                   :: ctm_photodiag = .false.
    logical                   :: ctm_pmdiag    = .false.
    logical                   :: ctm_wb_dust   = .false.
    logical                   :: mie_optics    = .false.
    logical                   :: init_conc     = .false.
    logical                   :: run_aero      = .false.
    logical                   :: run_rescld    = .false.
    logical                   :: verbose       = .false.
    real                      :: fg_dust_scale = -1.0
    type(aqm_species_type), pointer :: species => null()
  end type aqm_config_type

  private

  public :: aqm_config_type

  public :: aqm_config_init

contains

  subroutine aqm_config_read(model, config, rc)

    type(ESMF_GridComp),   intent(in)    :: model
    type(aqm_config_type), intent(inout) :: config
    integer, optional,     intent(out)   :: rc

    ! -- local variables
    integer                    :: localrc
    character(len=ESMF_MAXSTR) :: value
    type(ESMF_Config)          :: cf

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- get component's configuration
    call ESMF_GridCompGet(model, config=cf, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- read chemical mechanism definitions
    ! 1. aerosols
    call ESMF_ConfigGetAttribute(cf, config % ae_matrix_nml, &
      label="ae_matrix_nml:", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! 2. gas chemistry
    call ESMF_ConfigGetAttribute(cf, config % gc_matrix_nml, &
      label="gc_matrix_nml:", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! 3. non-reactive species
    call ESMF_ConfigGetAttribute(cf, config % nr_matrix_nml, &
      label="nr_matrix_nml:", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! 4. additional tracers
    call ESMF_ConfigGetAttribute(cf, config % tr_matrix_nml, &
      label="tr_matrix_nml:", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- mechanism table
    call ESMF_ConfigGetAttribute(cf, config % csqy_data, &
      label="csqy_data:", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- optics data
    call ESMF_ConfigGetAttribute(cf, config % optics_data, &
      label="optics_data:", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- OMI
    call ESMF_ConfigGetAttribute(cf, config % omi, &
      label="omi_data:", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- read run settings
    call ESMF_ConfigGetAttribute(cf, config % run_aero, &
      label="run_aerosol:", default=.true., rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_ConfigGetAttribute(cf, config % run_rescld, &
      label="run_rescld:", default=.true., rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_ConfigGetAttribute(cf, config % mie_optics, &
      label="mie_optics:", default=.false., rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_ConfigGetAttribute(cf, config % fg_dust_scale, &
      label="fugitive_dust_scale:", default=-1.0, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- read start up settings
    call ESMF_ConfigGetAttribute(cf, config % init_conc, &
      label="init_concentrations:", default=config % initial_run, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- read diagnostic settings
    call ESMF_ConfigGetAttribute(cf, config % ctm_aod, &
      label="ctm_aod:", default=.false., rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_ConfigGetAttribute(cf, config % ctm_pmdiag, &
      label="ctm_pmdiag:", default=.false., rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_ConfigGetAttribute(cf, config % ctm_wb_dust, &
      label="ctm_wb_dust:", default=.true., rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    
    call ESMF_ConfigGetAttribute(cf, value, &
      label="ctm_stdout:", default="all", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    config % ctm_stdout = ESMF_UtilStringLowerCase(value, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- microphysics tracer map
    call ESMF_ConfigGetAttribute(cf, config % mp_map, &
      label="mp_tracer_map:", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- CMAQ tracers start
    call ESMF_ConfigGetAttribute(cf, config % dy_map_beg, &
      label="dy_tracer_map_start:", default=0, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- set other default values
    config % ctm_depvfile  = .false.
    config % ctm_photodiag = .false.

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

    ! -- initialize species
    config % species % p_atm_qv   = 0
    config % species % p_atm_qc   = 0
    config % species % p_atm_qr   = 0
    config % species % p_atm_qi   = 0
    config % species % p_atm_qs   = 0
    config % species % p_atm_qg   = 0
    config % species % p_atm_o3   = 0
    config % species % p_aqm_beg  = 0
    config % species % p_diag_beg = 0
    config % species % ndiag      = 0

    ! -- map microphysics tracers based on the microphysics scheme
    ! -- used in the coupled atmospheric model
    select case (trim(config % mp_map))
      case ("gfdl")
        ! -- set hydrometeors pointers
        config % species % p_atm_qv = 1
        config % species % p_atm_qc = 2
        config % species % p_atm_qr = 3
        config % species % p_atm_qi = 4
        config % species % p_atm_qs = 5
        config % species % p_atm_qg = 6
        ! -- set ozone pointer
        config % species % p_atm_o3 = 7
      case ("thompson")
        ! -- set hydrometeors pointers
        config % species % p_atm_qv = 1
        config % species % p_atm_qc = 2
        config % species % p_atm_qi = 3
        config % species % p_atm_qr = 4
        config % species % p_atm_qs = 5
        config % species % p_atm_qg = 6
        ! -- set ozone pointer
        config % species % p_atm_o3 = 9
      case ("zhao-carr")
        ! -- set hydrometeors pointers
        config % species % p_atm_qv = 1
        config % species % p_atm_qc = 2
        ! -- set ozone pointer
        config % species % p_atm_o3 = 3
      case default
        call aqm_rc_set(AQM_RC_FAILURE, &
          msg="unknown mp_map", file=__FILE__, line=__LINE__, rc=rc)
        return
    end select

    ! -- starting index for CMAQ tracers
    if (config % dy_map_beg > 0) then
      ! -- use provided starting index
      if (aqm_rc_test(config % dy_map_beg <= config % species % p_atm_o3, &
        msg="dynamics tracer mapping must start after microphysics", &
        file=__FILE__, line=__LINE__, rc=rc)) return
      config % species % p_aqm_beg = config % dy_map_beg
    else
      ! -- assume CMAQ tracers follow the microphysics ones
      config % species % p_aqm_beg = config % species % p_atm_o3 + 1
    end if

    ! -- initialize diagnostic tracers
    if (config % ctm_pmdiag) config % species % ndiag = 4

  end subroutine aqm_config_species_init

  subroutine aqm_config_runtime_set(model, config, rc)

    type(ESMF_GridComp)                  :: model
    type(aqm_config_type), intent(inout) :: config
    integer, optional,     intent(out)   :: rc

    ! -- local variables
    integer                    :: localrc
    integer                    :: yy, julday, h, m, s
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime, startTime, stopTime
    type(ESMF_TimeInterval)    :: timeStep

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- get component's clock
    call NUOPC_ModelGet(model, modelClock=clock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- get clock information
    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, &
      stopTime=stopTime, timeStep=timeStep, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_TimeGet(startTime, yy=yy, dayOfYear=julday, h=h, m=m, s=s, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    config % ctm_stdate =  1000 * yy + julday
    config % ctm_sttime = 10000 * h + 100 * m + s

    call ESMF_TimeIntervalGet(timeStep, h=h, m=m, s=s, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    config % ctm_tstep = 10000 * h + 100 * m + s

    call ESMF_TimeIntervalGet(stopTime-currTime, h=h, m=m, s=s, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    config % ctm_runlen = 10000 * h + 100 * m + s

    config % initial_run = (currTime == startTime)

    config % init_conc   = config % initial_run

  end subroutine aqm_config_runtime_set

  subroutine aqm_config_init(model, config, rc)

    type(ESMF_GridComp)                  :: model
    type(aqm_config_type), intent(inout) :: config
    integer, optional,     intent(out)   :: rc

    ! -- local variables
    integer                    :: localrc
    integer                    :: diagnostic
    integer                    :: verbosity
    character(len=ESMF_MAXSTR) :: name
    character(len=ESMF_MAXSTR) :: msgString
    type(ESMF_Config)          :: cf

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- get component's information
    call NUOPC_CompGet(model, name=name, diagnostic=diagnostic, &
      verbosity=verbosity, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- set model runtime and start up mode
    call aqm_config_runtime_set(model, config, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- retrieve model settings
    call aqm_config_read(model, config, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- set up hydrometeors based on microphysics tracer map
    call aqm_config_species_init(config, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to initialize microphysics tracers for input map: " // config % mp_map, &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

    config % name    = name
    config % verbose = btest(diagnostic, 17)

    select case (trim(config % ctm_stdout))
      case ("all")
        m3io_enabled = .true.
      case ("maintask")
        m3io_enabled = aqm_logger_active()
      case ("none")
        m3io_enabled = .false.
    end select

    ! -- perform sanity check
    if (config % fg_dust_scale > 1.0 .or. config % fg_dust_scale < 0.) then
      config % fg_dust_scale = -1.0
      call aqm_logger_log("config: init: " &
        // "disabling fugitive dust correction (scale factor outside [0,1])", level=aqm_logger_warn)
    end if

    ! -- set model's name and verbosity level based on the component standard diagnostic level
    if (btest(verbosity,8)) then
      call aqm_config_log(config, name, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return
    end if

  end subroutine aqm_config_init

  subroutine aqm_config_log(config, name, rc)

    type(aqm_config_type), intent(in)  :: config
    character(len=*),      intent(in)  :: name
    integer, optional,     intent(out) :: rc

    ! -- local variables
    integer                    :: localrc
    character(len=ESMF_MAXSTR) :: msgString

    ! -- external functions
    character(len=10), external :: hhmmss
    character(len=14), external :: mmddyy

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_LogWrite(trim(name) // ": config: read: ae_matrix_nml: " &
      // config % ae_matrix_nml, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_LogWrite(trim(name) // ": config: read: gc_matrix_nml: " &
      // config % gc_matrix_nml, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_LogWrite(trim(name) // ": config: read: nr_matrix_nml: " &
      // config % nr_matrix_nml, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_LogWrite(trim(name) // ": config: read: tr_matrix_nml: " &
      // config % tr_matrix_nml, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_LogWrite(trim(name) // ": config: read: csqy_data: " &
      // config % csqy_data, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_LogWrite(trim(name) // ": config: read: optics_data: " &
      // config % optics_data, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_LogWrite(trim(name) // ": config: read: omi_data: " &
      // config % omi, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    if (config % ctm_aod) then
      call ESMF_LogWrite(trim(name) // ": config: read: ctm_aod: true", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    else
      call ESMF_LogWrite(trim(name) // ": config: read: ctm_aod: false", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if
    if (config % ctm_wb_dust) then
      call ESMF_LogWrite(trim(name) // ": config: read: ctm_wb_dust: true", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    else
      call ESMF_LogWrite(trim(name) // ": config: read: ctm_wb_dust: false", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if
    call ESMF_LogWrite(trim(name) // ": config: read: ctm_stdout: " &
      // config % ctm_stdout, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    if (config % run_aero) then
      call ESMF_LogWrite(trim(name) // ": config: read: run_aerosol: true", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    else
      call ESMF_LogWrite(trim(name) // ": config: read: run_aerosol: false", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if
    if (config % run_rescld) then
      call ESMF_LogWrite(trim(name) // ": config: read: run_rescld: true", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    else
      call ESMF_LogWrite(trim(name) // ": config: read: run_rescld: false", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if
    if (config % mie_optics) then
      call ESMF_LogWrite(trim(name) // ": config: read: mie_optics: true", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    else
      call ESMF_LogWrite(trim(name) // ": config: read: mie_optics: false", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if
    call ESMF_LogWrite(trim(name) // ": config: read: mp_tracer_map: " &
      // config % mp_map, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    write(msgString, '(a,": config: read: dy_tracer_map_start: ",i0)') &
      trim(name), config % dy_map_beg
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (config % fg_dust_scale > 0.0) then
      write(msgString, '(a,": config: read: fugitive_dust_scale: ",f8.5)') &
        trim(name), config % fg_dust_scale
      call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    else
      call ESMF_LogWrite(trim(name) // ": config: read: fugitive_dust_scale: (disabled)", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

    write(msgString, '(a,": config: init: mp_tracer_map: ",a,": ",i0," tracers")') &
      trim(name), trim(config % mp_map), config % species % p_aqm_beg - 1
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return
    call ESMF_LogWrite(trim(name) // ": config: init: start_time: " &
      // mmddyy(config % ctm_stdate) // " " // hhmmss(config % ctm_sttime), &
      ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return
    call ESMF_LogWrite(trim(name) // ": config: init: time_step : " &
      // hhmmss(config % ctm_tstep), ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return
    call ESMF_LogWrite(trim(name) // ": config: init: run_length: " &
      // hhmmss(config % ctm_runlen), ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return
    write(msgString, '(a,": config: init: run_type  :")') trim(name)
    if (config % initial_run) then
      msgString = trim(msgString) // " initial"
    else
      msgString = trim(msgString) // " restart"
    end if
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return
    write(msgString, '(a,": config: init: start_mode:")') trim(name)
    if (config % init_conc) then
      msgString = trim(msgString) // " cold (initialize concentrations)"
    else
      msgString = trim(msgString) // " warm"
    end if
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return
    write(msgString, '(a,": config: init: output_level:")') trim(name)
    if (config % verbose) then
      msgString = trim(msgString) // " verbose"
    else
      msgString = trim(msgString) // " normal"
    end if
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

  end subroutine aqm_config_log

end module aqm_config_mod
