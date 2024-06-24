module aqm_emis_mod

  use ESMF
  use NUOPC
  use aqmio
  use aqm_rc_mod
  use aqm_types_mod
  use aqm_tools_mod
  use aqm_internal_mod
  use aqm_model_mod, only : aqm_model_get, aqm_model_domain_get, aqm_state_type
  use aqm_const_mod, only : deg_to_rad, rad_to_deg

  implicit none

  ! -- parameters
  integer,            parameter :: emRefLen = 16
  real(ESMF_KIND_R4), parameter :: emAccept = 1.e+15_ESMF_KIND_R4
  character(len=*),   parameter :: rName    = "emissions"

  ! -- internal variables
  type(aqm_internal_emis_type), pointer :: aqm_emis_data(:) => null()

  private

  public :: aqm_emis_init
  public :: aqm_emis_finalize
  public :: aqm_emis_data_get
  public :: aqm_emis_ispresent
  public :: aqm_emis_get
  public :: aqm_emis_desc
  public :: aqm_emis_update
  public :: aqm_emis_read

  public :: aqm_internal_emis_type

contains

  subroutine aqm_emis_init(model, rc)
    type(ESMF_GridComp)            :: model
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                    :: localrc
    integer                    :: verbosity
    integer                    :: emisCount, item
    character(len=ESMF_MAXSTR) :: name
    character(len=ESMF_MAXSTR) :: msgString
    type(ESMF_Config)          :: config
    type(aqm_internal_state_type) :: is
    type(aqm_internal_data_type), pointer :: this

    character(len=*), parameter :: pName = "init"

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- initialize
    nullify(aqm_emis_data)

    ! -- get component's information
    call NUOPC_CompGet(model, name=name, verbosity=verbosity, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- get component's configuration
    call ESMF_GridCompGet(model, config=config, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- get component's internal state
    call ESMF_GridCompGetInternalState(model, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (associated(is % wrap % emis)) then
      call ESMF_LogSetError(ESMF_RC_PTR_ISALLOC, &
        msg="emissions already initialized", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return
    end if

    ! -- get emission sources
    call aqm_emis_src_create(config, is % wrap % emis, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (associated(is % wrap % emis)) then
      emisCount = size(is % wrap % emis)
      if (btest(verbosity,8)) then
        write(msgString,'(a,": ",a,": ",a,": types[",i0,"]: ")') trim(name), &
          trim(rName), trim(pName), emisCount
        do item = 1, emisCount
          msgString = trim(msgString) // " " // is % wrap % emis(item) % name
          if (item < emisCount) msgString = trim(msgString) // ","
        end do
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if

      do item = 1, emisCount
        ! -- set default log prefix label and verbosity
        is % wrap % emis(item) % logprefix = trim(name)//": "//rName &
          // ": " // is % wrap % emis(item) % name
        is % wrap % emis(item) % verbose   = btest(verbosity,8)
        ! -- initialize emission sources from config
        call aqm_emis_src_init(model, is % wrap % emis(item), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end do
      aqm_emis_data => is % wrap % emis

    else

      if (btest(verbosity,8)) then
        write(msgString,'(a,": ",a,": types : none")') trim(name), &
          trim(rName), trim(pName)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if
    end if

  end subroutine aqm_emis_init


  subroutine aqm_emis_src_create(config, em, rc)
    type(ESMF_Config)                     :: config
    type(aqm_internal_emis_type), pointer :: em(:)
    integer, optional,        intent(out) :: rc

    ! -- local variables
    integer                :: localrc, stat
    integer                :: emCount, item
    character(ESMF_MAXSTR) :: emName

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (associated(em)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="em pointer argument must not be associated", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return
    end if

    ! -- get number of sources
    emCount = ESMF_ConfigGetLen(config, label="emission_sources:", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    allocate(em(emCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    call ESMF_ConfigFindLabel(config, "emission_sources:", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    do item = 1, emCount
      ! -- initialize emission source
      em(item) % name = ""
      em(item) % type = ""
      em(item) % path = ""
      em(item) % file = ""
      em(item) % frequency   = ""
      em(item) % format      = "netcdf"
      em(item) % iomode      = "read"
      em(item) % iofmt       = AQMIO_FMT_NETCDF
      em(item) % irec        = 0
      em(item) % count       = 0
      em(item) % layers      = 1
      em(item) % scalefactor = 1.0
      em(item) % topfraction = -1.0
      em(item) % gridded     = .true.
      em(item) % sync        = .false.
      em(item) % verbose     = .false.
      em(item) % logprefix   = ""
      em(item) % period      = ""
      em(item) % plumerise   = ""
      em(item) % specfile    = ""
      em(item) % specprofile = ""
      em(item) % latname     = ""
      em(item) % lonname     = ""
      em(item) % stkdmname   = ""
      em(item) % stkhtname   = ""
      em(item) % stktkname   = ""
      em(item) % stkvename   = ""
      nullify(em(item) % species)
      nullify(em(item) % sources)
      nullify(em(item) % units)
      nullify(em(item) % factors)
      nullify(em(item) % fields)
      nullify(em(item) % lat)
      nullify(em(item) % lon)
      nullify(em(item) % stkdm)
      nullify(em(item) % stkht)
      nullify(em(item) % stktk)
      nullify(em(item) % stkve)
      nullify(em(item) % rates)
      nullify(em(item) % dens_flag)
      nullify(em(item) % ijmap)
      nullify(em(item) % table)

      call ESMF_ConfigGetAttribute(config, emName, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
      em(item) % name = ESMF_UtilStringLowerCase(emName, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end do

  end subroutine aqm_emis_src_create


  subroutine aqm_emis_src_init(model, em, rc)
    type(ESMF_GridComp)                     :: model
    type(aqm_internal_emis_type)            :: em
    integer, optional,          intent(out) :: rc

    ! -- local variables
    integer                    :: localrc, stat
    integer                    :: columnCount, rowCount
    integer                    :: fieldCount, spcsCount, item
    integer                    :: aqm_emis_num
    logical                    :: eolFlag, readFactors
    character(len=ESMF_MAXSTR) :: value
    character(len=ESMF_MAXSTR) :: msgString
    character(len=ESMF_MAXSTR), allocatable :: tmpSourceList(:)
    character(len=ESMF_MAXSTR), allocatable :: tmpSpeciesList(:)
    character(len=ESMF_MAXSTR), allocatable :: tmpUnitsList(:)
    real(ESMF_KIND_R4)         :: factor
    real(ESMF_KIND_R4), allocatable :: tmpFactorList(:)
    type(ESMF_Time)            :: startTime, currTime
    type(ESMF_TimeInterval)    :: timeInterval
    type(ESMF_Clock)           :: clock
    type(ESMF_Config)          :: config
    type(ESMF_Grid)            :: grid

    character(len=*), parameter :: pName = "init: source"


    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- initialize
    if (len_trim(em % name) == 0) then
      if (em % verbose) then
        write(msgString,'(a,": ",a,": ",a,": no emission name")') &
          trim(em % logprefix), trim(pName)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if
      return
    end if

    ! -- get component's configuration
    call ESMF_GridCompGet(model, clock=clock, config=config, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_ConfigGetAttribute(config, value, &
      label=trim(em % name)//"_type:", default="unknown", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    em % type = ESMF_UtilStringLowerCase(value, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_ConfigGetAttribute(config, value, &
      label=trim(em % name)//"_format:", default="netcdf", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    em % format = ESMF_UtilStringLowerCase(value, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    select case (trim(em % format))
      case ("binary")
        em % iofmt = AQMIO_FMT_BIN
      case ("netcdf")
        em % iofmt = AQMIO_FMT_NETCDF
      case default
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="- invalid emission format: "//trim(em % format), &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return
    end select
    if (em % verbose) then
      call ESMF_LogWrite(trim(em % logprefix)//": "//pName//": set format to "&
        //trim(em % format), ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

    call ESMF_ConfigGetAttribute(config, em % path, &
      label=trim(em % name) //"_path:", default="", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    if (em % verbose) then
      call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
        //": path: "//trim(em % path), ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

    if (trim(em % format) == "netcdf") then
      call ESMF_ConfigGetAttribute(config, em % file, &
        label=trim(em % name)//"_file:", rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
      if (em % verbose) then
        call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
          //": netCDF dataset: "//trim(em % file), ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
       end if
    end if

    call ESMF_ConfigGetAttribute(config, value, &
      label=trim(em % name)//"_frequency:", default="static", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    em % frequency = ESMF_UtilStringLowerCase(value, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (em % iofmt == AQMIO_FMT_BIN) then
      if (trim(em % frequency) /= "static") then
        em % frequency = "static"
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
          //": frequency set to static for binary input", &
          ESMF_LOGMSG_WARNING, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
      end if
    end if

    select case (trim(em % frequency))
      case ("hourly")
        call ESMF_TimeIntervalSet(timeInterval, h=1, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      case ("daily")
        call ESMF_TimeIntervalSet(timeInterval, d=1, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      case ("weekly")
        call ESMF_TimeIntervalSet(timeInterval, d=7, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      case ("monthly")
        call ESMF_TimeIntervalSet(timeInterval, mm=1, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      case ("static")
        call ESMF_TimeIntervalSet(timeInterval, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      case default
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="- unknown emission frequency: "//trim(em % frequency), &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        em % frequency = ""
        return
    end select

    call ESMF_ClockGet(clock, startTime=startTime, currTime=currTime, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- set input time record according to start type (startup/continue)
    em % irec = (currTime - startTime) / timeInterval

    em % alarm = ESMF_AlarmCreate(clock, ringTime=startTime, &
      ringInterval=timeInterval, name=trim(em % name)//"_alarm", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    if (em % verbose) then
      call ESMF_LogWrite(trim(em % logprefix)//": "//pName//": set to "&
        //trim(em % frequency)//" input", &
        ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

    readFactors = .true.

    select case (trim(em % type))
      case ("biogenic")
        call ESMF_ConfigGetAttribute(config, value, &
          label=trim(em % name)//"_period:", default="auto", rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        em % period = ESMF_UtilStringLowerCase(value, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        select case (trim(em % period))
          case ("summer", "winter")
            ! -- these are valid values
          case default
            ! -- not implemented yet
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg="- biogenic emission period: "//em % period, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)
            return  ! bail out
        end select
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": period set to: "//trim(em % period), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        call ESMF_ConfigGetAttribute(config, em % specfile, &
          label=trim(em % name)//"_speciation_file:", rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": speciation file: "//trim(em % specfile), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        call ESMF_ConfigGetAttribute(config, em % specprofile, &
          label=trim(em % name)//"_speciation_profile:", rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": speciation profile: "//trim(em % specprofile), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
      case ("fengsha")
        call ESMF_ConfigGetAttribute(config, em % scalefactor, &
          label=trim(em % name)//"_alpha:", default=1.0, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          write(msgString,'(g20.8)') em % scalefactor
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": alpha: "//adjustl(msgString), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
      case ("gbbepx")
        call ESMF_ConfigGetAttribute(config, value, &
          label=trim(em % name)//"_plume_rise:", default="none", rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        em % plumerise = ESMF_UtilStringLowerCase(value, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": plume_rise: "//trim(em % plumerise), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        if (trim(em % plumerise) /= "none") then
          call ESMF_ConfigGetAttribute(config, em % topfraction, &
            label=trim(em % name)//"_plume_top_fraction:", default=-1.0, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          if (em % verbose) then
            write(msgString,'(g20.8)') em % topfraction
            call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
              //": plume_top_fraction: "//adjustl(msgString), ESMF_LOGMSG_INFO, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
          end if
          if (em % topfraction > 1.0) then
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
              msg="plume top fraction must not exceed 1.0", &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)
            return  ! bail out
          end if
        end if
      case ("point-source")
        em % gridded = .false.
        ! -- get plumerise type
        call ESMF_ConfigGetAttribute(config, value, &
          label=trim(em % name)//"_plume_rise:", default="default", rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        em % plumerise = ESMF_UtilStringLowerCase(value, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": plume_rise: "//trim(em % plumerise), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        ! -- get emission layers
        call ESMF_ConfigGetAttribute(config, em % layers, &
          label=trim(em % name)//"_layers:", default=1, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % layers <= 0) then
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg="- layers must be greater than 0", &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)
          return  ! bail out
        end if
        if (em % verbose) then
          write(msgString, '(a,": ",a,": layers: ",i0)') trim(em % logprefix), &
            trim(pName), em % layers
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        ! -- get coordinate labels
        call ESMF_ConfigFindLabel(config, trim(em % name) // "_latlon_names:", rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        call ESMF_ConfigGetAttribute(config, em % latname, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        call ESMF_ConfigGetAttribute(config, em % lonname, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": latlon_names: "//trim(em % latname)//" "//trim(em % lonname), &
            ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        ! -- get stack parameters
        call ESMF_ConfigGetAttribute(config, em % stkdmname, &
          label=trim(em % name)//"_stack_diameter:", default='STKDM', rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": stack_diameter: "//trim(em % stkdmname), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        call ESMF_ConfigGetAttribute(config, em % stkhtname, &
          label=trim(em % name)//"_stack_height:", default='STKHT', rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": stack_height: "//trim(em % stkhtname), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        call ESMF_ConfigGetAttribute(config, em % stktkname, &
          label=trim(em % name)//"_stack_temperature:", default='STKTK', rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": stack_temperature: "//trim(em % stktkname), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        call ESMF_ConfigGetAttribute(config, em % stkvename, &
          label=trim(em % name)//"_stack_velocity:", default='STKVE', rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": stack_velocity: "//trim(em % stkvename), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
      case ("product")
        em % iomode = "create"
        readFactors = .false.
        call ESMF_ConfigGetAttribute(config, em % sync, &
          label=trim(em % name)//"_sync:", default=.false., rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          if (em % sync) then
            call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
              //": sync: true", ESMF_LOGMSG_INFO, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
          else
            call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
              //": sync: false", ESMF_LOGMSG_INFO, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
          end if
        end if
    end select

    call ESMF_ConfigGetDim(config, rowCount, columnCount, &
      label=trim(em % name)//"_species::", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (rowCount > 0) then
      allocate(tmpSpeciesList(rowCount), tmpUnitsList(rowCount), &
        tmpFactorList(rowCount), tmpSourceList(rowCount), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return

      ! -- initialize temporary arrays
      tmpSpeciesList = ""
      tmpUnitsList   = ""
      tmpSourceList  = ""
      tmpFactorList  = 0._ESMF_KIND_R4

      call ESMF_ConfigFindLabel(config, trim(em % name)//"_species::", rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out

      fieldCount = 0
      do item = 1, rowCount
        call ESMF_ConfigNextLine(config, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        ! -- species name
        call ESMF_ConfigGetAttribute(config, value, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        tmpSpeciesList(item) = ESMF_UtilStringUpperCase(value, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (readFactors) then
          ! -- scaling factor
          call ESMF_ConfigGetAttribute(config, value, default="1.0", &
            eolFlag=eolFlag, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          if (.not.eolFlag) then
            read(value, *, iostat=stat) tmpFactorList(item)
            if (stat /= 0) then
              call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                msg="- emission factor: "//trim(value), &
                line=__LINE__,  &
                file=__FILE__,  &
                rcToReturn=rc)
              return  ! bail out
            end if
          end if
        else
          eolFlag = .false.
        end if
        if (.not.eolFlag) then
          ! -- variable (netcdf) or file (binary) name
          call ESMF_ConfigGetAttribute(config, tmpSourceList(item), &
            default="", eolFlag=eolFlag, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        if (.not.eolFlag) then
          ! -- species units
          call ESMF_ConfigGetAttribute(config, value, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          tmpUnitsList(item) = ESMF_UtilStringUpperCase(value, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        if (.not.eolFlag) fieldCount = fieldCount + 1
        if (em % verbose) then
          if (eolFlag) then
            write(msgString, '("[",i0,"]: ",a," (not provided)")') &
              item, trim(tmpSpeciesList(item))
          else
            write(msgString, '("[",i0,"]: ",a," (",a,", ",a,", scale=",g0.5,")")') &
              item, trim(tmpSpeciesList(item)), trim(tmpSourceList(item)), &
              trim(tmpUnitsList(item)), tmpFactorList(item)
          end if
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": species"//trim(msgString), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
      end do

      if (fieldCount > 0) then
        ! -- create I/O component for emissions
        em % IO = AQMIO_Create(grid, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": created I/O component", ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if

        ! -- open single netCDF file if selected
        if (em % iofmt == AQMIO_FMT_NETCDF) then
          call AQMIO_Open(em % IO, em % file, filePath=em % path, iomode=em % iomode, &
            iofmt=em % iofmt, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          if (em % verbose) then
            call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
              //": opened: "//trim(em % file), ESMF_LOGMSG_INFO, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
          end if
        end if

        ! -- create arrays
        allocate(em % species(fieldCount),   &
                 em % factors(fieldCount),   &
                 em % sources(fieldCount),   &
                 em % units(fieldCount),     &
                 em % dens_flag(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return

        ! -- initialize arrays
        em % species = ""
        em % factors = 0._ESMF_KIND_R4
        em % sources = ""
        em % units   = ""

        ! -- assume emissions are provided as surface densities by default
        em % dens_flag = 0

        fieldCount = 0
        do item = 1, rowCount
          if (len_trim(tmpSourceList(item)) > 0) then
            fieldCount = fieldCount + 1
            em % species(fieldCount) = tmpSpeciesList(item)
            em % factors(fieldCount) = tmpFactorList(item)
            em % sources(fieldCount) = tmpSourceList(item)
            em % units(fieldCount)   = tmpUnitsList(item)
          end if
        end do

        deallocate(tmpFactorList, tmpSourceList, tmpUnitsList, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return

        ! -- create fields if needed
        if (em % gridded) then
          allocate(em % fields(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return
          do item = 1, fieldCount
            em % fields(item)  = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, &
              name=em % sources(item), rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
          end do
        else
          allocate(em % rates(fieldCount), stat=stat)
          if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return
          ! -- initialize ungridded emissions (point sources)
          call aqm_emis_pts_init(model, em, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if

      end if

      ! -- build unique list of species

      ! -- 1. sort species names
      call ESMF_UtilSort(tmpSpeciesList, ESMF_SORTFLAG_ASCENDING, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out

      ! -- 2. select unique species
      aqm_emis_num = 1
      do item = 2, rowCount
        if (tmpSpeciesList(item) /= tmpSpeciesList(item-1)) &
          aqm_emis_num = aqm_emis_num + 1
      end do

      allocate(em % table(aqm_emis_num,2), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return

      em % table = ""

      aqm_emis_num = 1
      em % table(aqm_emis_num, 1) = tmpSpeciesList(1)
      do item = 2, rowCount
        if (tmpSpeciesList(item) /= tmpSpeciesList(item-1)) then
          aqm_emis_num = aqm_emis_num + 1
          em % table(aqm_emis_num, 1) = tmpSpeciesList(item)
        end if
      end do

      deallocate(tmpSpeciesList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return

    end if

  end subroutine aqm_emis_src_init


  subroutine aqm_emis_finalize(model, rc)
    type(ESMF_GridComp)            :: model
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                       :: localrc, stat
    integer                       :: verbosity
    integer                       :: item, n
    logical                       :: isCreated
    character(len=ESMF_MAXSTR)    :: name
    type(aqm_internal_state_type) :: is
    type(aqm_internal_emis_type), pointer :: em

    character(len=*), parameter :: pName = "finalize"

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- get component's information
    call NUOPC_CompGet(model, name=name, verbosity=verbosity, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- get component's internal state
    call ESMF_GridCompGetInternalState(model, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    nullify(aqm_emis_data)

    if (.not.associated(is % wrap)) return

    if (associated(is % wrap % emis)) then
      do item = 1, size(is % wrap % emis)

        em => is % wrap % emis(item)

        if (associated(em % sources)) then
          deallocate(em % sources, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % sources)
        end if
        if (associated(em % species)) then
          deallocate(em % species, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % species)
        end if
        if (associated(em % units)) then
          deallocate(em % units, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % units)
        end if
        if (associated(em % factors)) then
          deallocate(em % factors, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % factors)
        end if
        if (associated(em % dens_flag)) then
          deallocate(em % dens_flag, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % dens_flag)
        end if
        if (associated(em % table)) then
          deallocate(em % table, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % table)
        end if
        if (associated(em % fields)) then
          do n = 1, size(em % fields)
            isCreated = ESMF_FieldIsCreated(em % fields(n), rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
            if (isCreated) then
              call ESMF_FieldDestroy(em % fields(n), rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__,  &
                rcToReturn=rc)) &
                return  ! bail out
            end if
          end do
          deallocate(em % fields, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % fields)
        end if
        if (associated(em % lat)) then
          deallocate(em % lat, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % lat)
        end if
        if (associated(em % lon)) then
          deallocate(em % lon, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % lon)
        end if
        if (associated(em % stkdm)) then
          deallocate(em % stkdm, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % stkdm)
        end if
        if (associated(em % stkht)) then
          deallocate(em % stkht, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % stkht)
        end if
        if (associated(em % stktk)) then
          deallocate(em % stktk, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % stktk)
        end if
        if (associated(em % stkve)) then
          deallocate(em % stkve, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % stkve)
        end if
        if (associated(em % ijmap)) then
          deallocate(em % ijmap, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % ijmap)
        end if
        if (associated(em % rates)) then
          do n = 1, size(em % rates)
            if (associated(em % rates(n) % values)) then
              deallocate(em % rates(n) % values, stat=stat)
              if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__,  &
                rcToReturn=rc)) &
                return
              nullify(em % rates(n) % values)
            end if
          end do
          deallocate(em % rates, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(em % rates)
        end if
        call ESMF_AlarmDestroy(em % alarm, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        call AQMIO_Destroy(em % IO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": released resources for dataset: "//trim(em % name), &
            ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
      end do
      nullify(em)
      deallocate(is % wrap % emis, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
      nullify(is % wrap % emis)
      if (btest(verbosity,8)) then
        call ESMF_LogWrite(trim(name)//": "//rName//": "//pName &
          //": all resources released", &
          ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if
    end if

  end subroutine aqm_emis_finalize


  subroutine aqm_emis_update(model, rc)
    type(ESMF_GridComp)            :: model
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                       :: localrc
    integer                       :: item, n
    logical                       :: isRinging
    character(len=ESMF_MAXSTR)    :: timeString
    type(ESMF_Clock)              :: clock
    type(ESMF_Time)               :: currTime
    type(aqm_internal_state_type) :: is
    type(aqm_internal_emis_type), pointer :: em

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- get component's configuration
    call ESMF_GridCompGet(model, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- get component's internal state
    call ESMF_GridCompGetInternalState(model, is, localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- return if no internal data set
    if (.not.associated(is % wrap)) return
    ! -- return if no internal emission data set
    if (.not.associated(is % wrap % emis)) return

    do item = 1, size(is % wrap % emis)

      em => is % wrap % emis(item)

      isRinging = (trim(em % iomode) == "read")

      if (isRinging) then
        isRinging = ESMF_AlarmIsRinging(em % alarm, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if

      if (isRinging) then
        call ESMF_ClockGet(clock, currTime=currTime, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        call ESMF_TimeGet(currTime, timeString=timeString, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (em % verbose) then
          call ESMF_LogWrite(trim(em % logprefix)//": reading "//&
            trim(em % name)//" @ "//trim(timeString), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        if (em % iofmt == AQMIO_FMT_BIN) then
          do n = 1, size(em % sources)
            call AQMIO_Read(em % IO, (/ em % fields(n) /), fileName=em % sources(n), &
              filePath=em % path, iofmt=em % iofmt, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
          end do
        else
          em % irec = em % irec + 1
          if (em % gridded) then
            call AQMIO_Read(em % IO, em % fields, timeSlice=em % irec, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
          else
            do n = 1, size(em % sources)
              call AQMIO_DataRead(em % IO, em % rates(n) % values, em % sources(n), timeSlice=em % irec, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__,  &
                rcToReturn=rc)) &
                return  ! bail out
            end do
          end if
        end if
        call ESMF_AlarmRingerOff(em % alarm, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if

    end do

  end subroutine aqm_emis_update


  function aqm_emis_get(etype) result (ep)
    character(len=*), intent(in) :: etype
    type(aqm_internal_emis_type), pointer :: ep

    integer :: item

    nullify(ep)

    if (associated(aqm_emis_data)) then
      do item = 1, size(aqm_emis_data)
        if (trim(aqm_emis_data(item) % type) == trim(etype)) then
          ep => aqm_emis_data(item)
          exit
        end if
      end do
    end if

  end function aqm_emis_get


  function aqm_emis_data_get() result (ep)
    type(aqm_internal_emis_type), pointer :: ep(:)

    nullify(ep)

    if (associated(aqm_emis_data)) ep => aqm_emis_data

  end function aqm_emis_data_get


  logical function aqm_emis_ispresent(etype)
    character(len=*), intent(in) :: etype

    aqm_emis_ispresent = associated(aqm_emis_get(etype))

  end function aqm_emis_ispresent


  subroutine aqm_emis_desc( etype, nlays, nvars, vnames, units )
    character(len=*),            intent(in)  :: etype
    integer,           optional, intent(out) :: nlays
    integer,           optional, intent(out) :: nvars
    character(len=16), optional, intent(out) :: vnames(:)
    character(len=16), optional, intent(out) :: units(:)

    ! -- local variables
    integer :: localrc
    integer :: item, nsrc
    type(aqm_internal_emis_type), pointer :: em

    ! -- begin
    ! -- get emission data
    nullify(em)
    em => aqm_emis_get(etype)

    if (associated(em)) then
      if (present(nlays)) then
        ! -- get number of vertical levels from model
        call aqm_model_domain_get(nl=nlays, rc=localrc)
        if (aqm_rc_check(localrc, msg="Failure to retrieve model layers", &
          file=__FILE__, line=__LINE__)) return
        select case (trim(em % plumerise))
          case ("briggs","default")
            nlays = min(nlays, em % layers)
          case ("sofiev")
            ! -- use full vertical column for fire emissions
          case default
            nlays = 1
        end select
      end if
      nsrc = size( em % table, dim=1 )
      if (present(nvars))  nvars = nsrc
      if (present(vnames)) vnames( 1:nsrc ) = em % table( 1:nsrc, 1 )
      if (present(units))  units ( 1:nsrc ) = em % table( 1:nsrc, 2 )
    else
      if (present(nlays))  nlays  = 0
      if (present(nvars))  nvars  = 0
      if (present(vnames)) vnames = ""
      if (present(units))  units  = ""
    end if

  end subroutine aqm_emis_desc


  subroutine aqm_emis_grd_read(em, spcname, buffer, localDe, rc)
    type(aqm_internal_emis_type)     :: em
    character(len=*),  intent(in)    :: spcname
    real,              intent(inout) :: buffer(*)
    integer, optional, intent(in)    :: localDe
    integer, optional, intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: item, i, j, k
    integer, dimension(2) :: lb, ub
    character(len=ESMF_MAXSTR)    :: msgString
    real(ESMF_KIND_R4),   pointer :: fptr(:,:)
    type(aqm_state_type), pointer :: stateIn

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- NOTE: input emissions need to be converted to surface densities here
    ! -- since grid cell area is set to 1 internally.

    ! -- bail out if this is a product
    if (trim(em % iomode) /= "read") return

    call aqm_model_get(stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    do item = 1, size(em % species)
      if (trim(spcname) == trim(em % species(item))) then
        nullify(fptr)
        call ESMF_FieldGet(em % fields(item), localDe=localDe, &
          computationalLBound=lb, computationalUBound=ub, &
          farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__)) then
          if (present(rc)) rc = AQM_RC_FAILURE
          return  ! bail out
        end if

        select case (em % dens_flag(item))
          case (:-1)
            ! -- this case indicates that input emissions are provided as totals/cell
            ! -- while surface densities are required and should never occur in CMAQ
            k = 0
            do j = lb(2), ub(2)
              do i = lb(1), ub(1)
                k = k + 1
                if (abs(fptr(i,j)) < emAccept) then
                  buffer(k) = buffer(k) &
                    + em % factors(item) * fptr(i,j) / stateIn % area(i,j) &
                                                     / stateIn % area(i,j)
                end if
              end do
            end do
          case (0)
            ! -- emissions are totals over each grid cell
            k = 0
            do j = lb(2), ub(2)
              do i = lb(1), ub(1)
                k = k + 1
                if (abs(fptr(i,j)) < emAccept) then
                  buffer(k) = buffer(k) &
                    + em % factors(item) * fptr(i,j) / stateIn % area(i,j)
                end if
              end do
            end do
          case (1:)
            ! -- emissions are already provided as surface densities, no need to normalize
            k = 0
            do j = lb(2), ub(2)
              do i = lb(1), ub(1)
                k = k + 1
                if (abs(fptr(i,j)) < emAccept) then
                  buffer(k) = buffer(k) &
                    + em % factors(item) * fptr(i,j)
                end if
              end do
            end do
          case default
            ! -- this case should never occur
            call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
              msg="uncategorized emission source", &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)
            return
        end select
        if (em % verbose) then
          write(msgString, '(a,": read: ",a16,": min/max = ",2g20.8)') &
            trim(em % logprefix), spcname, minval(fptr), maxval(fptr)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
      end if
    end do

  end subroutine aqm_emis_grd_read


  subroutine aqm_emis_pts_read(em, spcname, buffer, ijmap, localDe, rc)
    type(aqm_internal_emis_type)                    :: em
    character(len=*),                 intent(in)    :: spcname
    real,                             intent(inout) :: buffer(*)
    type(aqm_internal_psrc_type), optional, pointer :: ijmap(:)
    integer, optional,                intent(in)    :: localDe
    integer, optional,                intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: item, i, j, m, n
    character(len=ESMF_MAXSTR)    :: msgString
    real(ESMF_KIND_R4)            :: em_min, em_max
    type(aqm_state_type), pointer :: stateIn

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- NOTE: input emissions need to be converted to surface densities here
    ! -- since grid cell area is set to 1 internally.

    ! -- bail out if this is a product
    if (trim(em % iomode) /= "read") return

    if (em % count == 0) return

    call aqm_model_get(stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- return grid locations and map
    if (present(ijmap)) ijmap => em % ijmap

    do item = 1, size(em % species)
      if (trim(spcname) == trim(em % species(item))) then

        select case (em % dens_flag(item))
          case (:-1)
            ! -- this case indicates that input emissions are provided as totals/cell
            ! -- while surface densities are required and should never occur in CMAQ
            do m = 1, size(em % ijmap)
              n = em % ijmap(m) % nloc
              i = em % ijmap(m) % i
              j = em % ijmap(m) % j
              buffer(n) = buffer(n) &
                + em % factors(item) * em % rates(item) % values(n) / stateIn % area(i,j) &
                                                                    / stateIn % area(i,j)
            end do
          case (0)
            ! -- emissions are totals over each grid cell
            do m = 1, size(em % ijmap)
              n = em % ijmap(m) % nloc
              i = em % ijmap(m) % i
              j = em % ijmap(m) % j
              buffer(n) = buffer(n) &
                + em % factors(item) * em % rates(item) % values(n) / stateIn % area(i,j)
            end do
          case (1:)
            ! -- emissions are already provided as surface densities, no need to normalize
            do m = 1, size(em % ijmap)
              n = em % ijmap(m) % nloc
              buffer(n) = buffer(n) &
                + em % factors(item) * em % rates(item) % values(n)
            end do
          case default
            ! -- this case should never occur
            call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
              msg="uncategorized emission source", &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)
            return
        end select
        if (em % verbose) then
          em_min = huge(0._ESMF_KIND_R4)
          em_max = -em_min
          do m = 1, size(em % ijmap)
            n = em % ijmap(m) % nloc
            em_min = min( em_min, em % rates(item) % values(n) )
            em_max = max( em_max, em % rates(item) % values(n) )
          end do
          write(msgString, '(a,": read: ",a16,": min/max = ",2g20.8)') &
            trim(em % logprefix), spcname, em_min, em_max
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
      end if
    end do

  end subroutine aqm_emis_pts_read


  subroutine aqm_emis_read(etype, spcname, buffer, ijmap, localDe, rc)
    character(len=*),                     intent(in)    :: etype
    character(len=*),                     intent(in)    :: spcname
    real,                                 intent(inout) :: buffer(*)
    type(aqm_internal_psrc_type), optional, pointer     :: ijmap(:)
    integer,                      optional, intent(in)  :: localDe
    integer,                      optional, intent(out) :: rc

    ! -- local variables
    type(aqm_internal_emis_type), pointer :: em

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- retrieve emissions
    em => aqm_emis_get(etype)

    ! -- bail out if no emissions available
    if (.not.associated(em)) return

    ! -- bail out if this is a product
    if (trim(em % iomode) /= "read") return

    ! -- select emission reader
    if (em % gridded) then
      call aqm_emis_grd_read(em, spcname, buffer, localDe=localDe, rc=rc)
    else
      call aqm_emis_pts_read(em, spcname, buffer, ijmap=ijmap, localDe=localDe, rc=rc)
    end if

  end subroutine aqm_emis_read


  subroutine aqm_emis_pts_map(model, em, rc)

    type(ESMF_GridComp)                     :: model
    type(aqm_internal_emis_type)            :: em
    integer, optional,          intent(out) :: rc

    ! -- local variables
    integer :: localrc, stat
    integer :: deCount, localDeCount
    integer :: lb, ub
    integer :: i, ij, j, m, n, nx
    integer :: ids, ide, jds, jde, its, ite, jts
    integer,               dimension(:),   allocatable :: globalMap, localMap
    integer(ESMF_KIND_I4), dimension(:),   pointer     :: ploc
    integer(ESMF_KIND_I4), dimension(:,:), pointer     :: factorIndexList
    real(ESMF_KIND_R8),    dimension(:),   pointer     :: plat, plon
    type(ESMF_DistGrid)    :: distgrid
    type(ESMF_Field)       :: locField, grdField
    type(ESMF_Grid)        :: grid
    type(ESMF_Index_Flag)  :: indexflag
    type(ESMF_LocStream)   :: locstream
    type(ESMF_RouteHandle) :: rh
    type(ESMF_VM)          :: vm
    type(aqm_internal_psrc_type), dimension(:), allocatable :: pmap


    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- bail out if this is a product
    if (trim(em % iomode) /= "read") return

    ! -- bail out if emission type isn't point source
    if (trim(em % type) /= "point-source") return

    ! -- get model grid
    call ESMF_GridCompGet(model, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- get information on horizontal decomposition from grid
    call ESMF_GridGet(grid, distgrid=distgrid, indexflag=indexflag, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_DistGridGet(distgrid, deCount=deCount, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- create locstream only on active DEs to store emission locations
    locstream = ESMF_LocStreamCreate(regDecomp=deCount, minIndex=1, maxIndex=em % count, &
      indexflag=ESMF_INDEX_GLOBAL, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- add location's latitudes and longitudes
    ! -- add lat/lon keys
    call ESMF_LocStreamAddKey(locstream,                    &
                              keyName="ESMF:Lat",           &
                              keyTypeKind=ESMF_TYPEKIND_R8, &
                              keyUnits="Degrees",           &
                              keyLongName="Latitude",       &
                              rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_LocStreamAddKey(locstream,                    &
                              keyName="ESMF:Lon",           &
                              keyTypeKind=ESMF_TYPEKIND_R8, &
                              keyUnits="Degrees",           &
                              keyLongName="Longitude",      &
                              rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- now add coordinates if available
    if (localDeCount > 0) then

      call ESMF_LocStreamGetKey(locstream,                    &
                                keyName="ESMF:Lat",           &
                                farray=plat,                  &
                                rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out

      call ESMF_LocStreamGetKey(locstream,                    &
                                keyName="ESMF:Lon",           &
                                computationalLBound=lb,       &
                                computationalUBound=ub,       &
                                farray=plon,                  &
                                rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out

      do n = lb, ub
        plat(n) = em % lat(n)
        plon(n) = em % lon(n)
      end do

    end if

    ! -- create temporary fields to map point source locations to grid

    ! -- (1) location field

    locField = ESMF_FieldCreate(locstream, ESMF_TYPEKIND_I4, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- populate field with location index number

    if (localDeCount > 0) then

      call ESMF_FieldGet(locField, farrayPtr=ploc, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out

      do n = lb, ub
        ploc(n) = n
      end do

    end if

    ! -- (2) gridded field

    grdField = ESMF_FieldCreate(grid, ESMF_TYPEKIND_I4, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- regrid emissions from locstream to grid

    call ESMF_FieldRegridStore(locField, grdField, regridmethod=ESMF_REGRIDMETHOD_NEAREST_DTOS, &
      routehandle=rh, factorIndexList=factorIndexList, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_FieldRegrid(locField, grdField, rh, zeroregion=ESMF_REGION_TOTAL, &
      termorderflag=ESMF_TERMORDER_SRCSEQ, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- assemble internal map

    allocate(localMap(em % count), globalMap(em % count), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- store local map from location index to grid index as created by ESMF_FieldRegridStore()
    ! -- grid indices are assigned by interpreting the gridded field as a sequence of elements
    ! -- here we assume the default index rule is used to assign sequence indices to field elements
    ! -- see ESMF Reference Manual, Sect. 28.2.18 for details
    if (localDeCount > 0) then
      localMap  = 0
      globalMap = 0
      do n = 1, size(factorIndexList,2)
        localMap(factorIndexList(1,n)) = factorIndexList(2,n)
      end do
    end if

    deallocate(factorIndexList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- build global location map only on PETs with available data using I/O component's VM

    call ESMF_GridCompGet(em % IO, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_VMAllReduce(vm, localMap, globalMap, em % count, ESMF_REDUCE_SUM, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    deallocate(localMap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (localDeCount > 0) then

    ! -- retrieve index space information
      call aqm_model_domain_get(ids=ids, ide=ide, jds=jds, jde=jde, its=its, ite=ite, jts=jts, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
        file=__FILE__, &
        line=__LINE__, &
        rc=rc))        &
        return

      nx = ite - its + 1

      allocate(pmap(em % count), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out

      ! -- map to global grid
      ij = 0
      do m = 1, em % count
        n = globalMap(m)
        j = n / nx + jts
        if (j > jde .or. j < jds) cycle
        i = n - (j-1)*nx + its - 1
        if (i > ide .or. i < ids) cycle
        ij = ij + 1
        pmap(ij) % nloc = m
        pmap(ij) % i    = i
        pmap(ij) % j    = j
      end do

      if (indexflag == ESMF_INDEX_DELOCAL) then
        ! -- map to local domain
        i = ids - 1
        j = jds - 1
        do m = 1, ij
          pmap(m) % i = pmap(m) % i - i
          pmap(m) % j = pmap(m) % j - j
        end do
      end if

      allocate(em % ijmap(ij), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out

      em % ijmap = pmap(1:ij)

      ! -- set emission count to local mapped count
      em % count = ij

      deallocate(pmap, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out

    end if

    ! -- free up memory

    deallocate(globalMap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_FieldDestroy(locField, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_FieldDestroy(grdField, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_FieldRegridRelease(rh, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_LocStreamDestroy(locstream, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

  end subroutine aqm_emis_pts_map


  subroutine aqm_emis_pts_init(model, em, rc)
    type(ESMF_GridComp)                     :: model
    type(aqm_internal_emis_type)            :: em
    integer, optional,          intent(out) :: rc

    ! -- local variables
    integer :: localrc, item
    character(len=ESMF_MAXSTR) :: msgString

    character(len=*), parameter :: pName = "init: source"

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- bail out if gridded emissions
    if (em % gridded) return

    ! -- initialize pointers & counters
    do item = 1, size(em % sources)
      nullify(em % rates(item) % values)
    end do

    ! -- read stack locations
    call AQMIO_DataRead(em % IO, em % lat, em % latname, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    call AQMIO_DataRead(em % IO, em % lon, em % lonname, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- read stack parameters
    call AQMIO_DataRead(em % IO, em % stkdm, em % stkdmname, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    call AQMIO_DataRead(em % IO, em % stkht, em % stkhtname, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    call AQMIO_DataRead(em % IO, em % stktk, em % stktkname, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    call AQMIO_DataRead(em % IO, em % stkve, em % stkvename, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- set initial (maximum) count
    em % count = size(em % lat)

    ! -- map point source emissions to grid
    call aqm_emis_pts_map(model, em, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (em % verbose) then
      write(msgString, '("mapped to ",i0," local grid points")') em % count
      call ESMF_LogWrite(trim(em % logprefix)//": "//pName &
            //": "//trim(msgString), ESMF_LOGMSG_INFO, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

  end subroutine aqm_emis_pts_init

end module aqm_emis_mod
