module aqm_emis_mod

  use ESMF
  use NUOPC
  use aqmio
  use aqm_rc_mod
  use aqm_types_mod
  use aqm_tools_mod
  use aqm_model_mod, only : aqm_model_get, aqm_state_type

  implicit none

  ! -- parameters
  integer,          parameter :: emRefLen    = 16
  character(len=*), parameter :: emAlarmName = "emissions_alarm"
  character(len=*), parameter :: rName       = "emissions"

  character(len=ESMF_MAXPATHLEN)          :: emPath
  character(len=emRefLen),    allocatable :: aqm_emis_ref_table(:,:)
  character(len=ESMF_MAXSTR), allocatable :: aqm_emis_species(:)
  character(len=ESMF_MAXSTR), allocatable :: aqm_emis_sources(:)
  character(len=ESMF_MAXSTR), allocatable :: aqm_emis_units(:)
  integer,                    allocatable :: aqm_emis_dens_flag(:)
  real(ESMF_KIND_R4),         allocatable :: aqm_emis_factors(:)
  type(ESMF_Field),           allocatable :: aqm_emis_fields(:)
  type(ESMF_GridComp)                     :: emIO
  type(ESMF_IOFmt_flag)                   :: emIOFmt

  private

  public :: aqm_emis_ref_table
  public :: aqm_emis_species
  public :: aqm_emis_units
  public :: aqm_emis_factors
  public :: aqm_emis_dens_flag

  public :: aqm_emis_init
  public :: aqm_emis_finalize
  public :: aqm_emis_update
  public :: aqm_emis_read

contains

  subroutine aqm_emis_init(model, rc)
    type(ESMF_GridComp)            :: model
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                    :: localrc, stat
    integer                    :: verbosity
    integer                    :: columnCount, rowCount
    integer                    :: fieldCount, spcsCount, item
    integer                    :: aqm_emis_num
    logical                    :: eolFlag
    character(len=ESMF_MAXSTR) :: name
    character(len=ESMF_MAXSTR) :: value, emFormat, emFile, emFrequency
    character(len=ESMF_MAXSTR) :: msgString
    character(len=ESMF_MAXSTR), allocatable :: tmpSourceList(:)
    character(len=ESMF_MAXSTR), allocatable :: tmpSpeciesList(:)
    character(len=ESMF_MAXSTR), allocatable :: tmpUnitsList(:)
    real(ESMF_KIND_R4)         :: factor
    real(ESMF_KIND_R4), allocatable :: tmpFactorList(:)
    type(ESMF_Alarm)           :: alarm
    type(ESMF_Config)          :: config
    type(ESMF_Clock)           :: clock
    type(ESMF_Grid)            :: grid
    type(ESMF_Time)            :: startTime
    type(ESMF_TimeInterval)    :: timeInterval


    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- initialize
    aqm_emis_num = 0

    ! -- get component's information
    call NUOPC_CompGet(model, name=name, verbosity=verbosity, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- get component's configuration
    call ESMF_GridCompGet(model, clock=clock, config=config, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_ConfigGetAttribute(config, value, &
      label="emissions_format:", default="netcdf", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    emFormat = ESMF_UtilStringLowerCase(value, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    select case (trim(value))
      case ("binary")
        emIOFmt = ESMF_IOFMT_BIN
      case ("netcdf")
        emIOFmt = ESMF_IOFMT_NETCDF
      case default
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="- invalid emission format: "//trim(value), &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return
    end select
    if (btest(verbosity,8)) then
     call ESMF_LogWrite(trim(name)//": "//rName//": set format to "&
       //trim(value), ESMF_LOGMSG_INFO, rc=localrc)
     if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__,  &
       file=__FILE__,  &
       rcToReturn=rc)) &
       return  ! bail out
    end if

    call ESMF_ConfigGetAttribute(config, emPath, &
      label="emissions_path:", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    if (btest(verbosity,8)) then
     call ESMF_LogWrite(trim(name)//": "//rName//": path: "//trim(emPath), &
       ESMF_LOGMSG_INFO, rc=localrc)
     if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__,  &
       file=__FILE__,  &
       rcToReturn=rc)) &
       return  ! bail out
    end if

    if (trim(emFormat) == "netcdf") then
      call ESMF_ConfigGetAttribute(config, emFile, &
        label="emissions_file:", rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
      if (btest(verbosity,8)) then
       call ESMF_LogWrite(trim(name)//": "//rName//": netCDF dataset: "//trim(emFile), &
         ESMF_LOGMSG_INFO, rc=localrc)
       if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__,  &
         file=__FILE__,  &
         rcToReturn=rc)) &
         return  ! bail out
      end if
    end if

    call ESMF_ConfigGetAttribute(config, value, &
      label="emissions_frequency:", default="static", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    emFrequency = ESMF_UtilStringLowerCase(value, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (emIOFmt == ESMF_IOFMT_BIN) then
      if (trim(emFrequency) /= "static") then
        emFrequency = "static"
        if (btest(verbosity,8)) then
          call ESMF_LogWrite(trim(name)//": "//rName//": frequency set to static for binary input", &
          ESMF_LOGMSG_WARNING, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
      end if
    end if

    select case (trim(emFrequency))
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
          msg="- unknown emission frequency: "//trim(emFrequency), &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return
    end select

    call ESMF_ClockGet(clock, startTime=startTime, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    alarm = ESMF_AlarmCreate(clock, ringTime=startTime, &
      ringInterval=timeInterval, name="emissions_alarm", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    if (btest(verbosity,8)) then
     call ESMF_LogWrite(trim(name)//": "//rName//": set to "&
       //trim(emFrequency)//" input", &
       ESMF_LOGMSG_INFO, rc=localrc)
     if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__,  &
       file=__FILE__,  &
       rcToReturn=rc)) &
       return  ! bail out
    end if

    call ESMF_ConfigGetDim(config, rowCount, columnCount, &
      label="emissions_species::", rc=localrc)
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

      aqm_emis_ref_table  = ""
      tmpSourceList = ""
      tmpFactorList = 0._ESMF_KIND_R4

      call ESMF_ConfigFindLabel(config, "emissions_species::", rc=localrc)
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
        if (btest(verbosity,8)) then
          if (eolFlag) then
            write(msgString, '("[",i0,"]: ",a," (not provided)")') &
              item, trim(tmpSpeciesList(item))
          else
            write(msgString, '("[",i0,"]: ",a," (",a,", ",a,", scale=",g0.5,")")') &
              item, trim(tmpSpeciesList(item)), trim(tmpSourceList(item)), &
              trim(tmpUnitsList(item)), tmpFactorList(item)
          end if
          call ESMF_LogWrite(trim(name)//": "//rName//": species"//trim(msgString), &
            ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
      end do

      if (fieldCount > 0) then
        ! -- create I/O component for emissions
        emIO = AQMIO_Create(grid, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (btest(verbosity,8)) then
          call ESMF_LogWrite(trim(name)//": "//rName//": created I/O component", &
            ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if

        ! -- open single netCDF file if selected
        if (emIOFmt == ESMF_IOFMT_NETCDF) then
          call AQMIO_Open(emIO, emFile, filePath=emPath, iomode="read", &
            iofmt=emIOFmt, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          if (btest(verbosity,8)) then
            call ESMF_LogWrite(trim(name)//": "//rName//": opened: "//trim(emFile), &
              ESMF_LOGMSG_INFO, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
          end if
        end if

        ! -- create fields
        allocate(aqm_emis_species(fieldCount), aqm_emis_factors(fieldCount), &
          aqm_emis_sources(fieldCount), aqm_emis_units(fieldCount), &
          aqm_emis_dens_flag(fieldCount), aqm_emis_fields(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return

        aqm_emis_dens_flag = 0

        fieldCount = 0
        do item = 1, rowCount
          if (len_trim(tmpSourceList(item)) > 0) then
            fieldCount = fieldCount + 1
            aqm_emis_species(fieldCount) = tmpSpeciesList(item)
            aqm_emis_factors(fieldCount) = tmpFactorList(item)
            aqm_emis_sources(fieldCount) = tmpSourceList(item)
            aqm_emis_units(fieldCount)   = tmpUnitsList(item)
            aqm_emis_fields(fieldCount)  = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, &
              name=aqm_emis_sources(fieldCount), rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
          end if
        end do

        deallocate(tmpFactorList, tmpSourceList, tmpUnitsList, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return

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

      allocate(aqm_emis_ref_table(aqm_emis_num,2), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return

      aqm_emis_ref_table = ""

      aqm_emis_num = 1
      aqm_emis_ref_table(aqm_emis_num, 1) = tmpSpeciesList(1)
      do item = 2, rowCount
        if (tmpSpeciesList(item) /= tmpSpeciesList(item-1)) then
          aqm_emis_num = aqm_emis_num + 1
          aqm_emis_ref_table(aqm_emis_num, 1) = tmpSpeciesList(item)
        end if
      end do

      deallocate(tmpSpeciesList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return

    end if

  end subroutine aqm_emis_init

  subroutine aqm_emis_finalize(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc, stat
    integer :: item
    logical :: isCreated

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    call AQMIO_Destroy(emIO, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (allocated(aqm_emis_fields)) then
      do item = 1, size(aqm_emis_fields)
        isCreated = ESMF_FieldIsCreated(aqm_emis_fields(item), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        call ESMF_FieldDestroy(aqm_emis_fields(item), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end do
    end if

    if (allocated(aqm_emis_species)) then
      deallocate(aqm_emis_species, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

    if (allocated(aqm_emis_factors)) then
      deallocate(aqm_emis_factors, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

    if (allocated(aqm_emis_sources)) then
      deallocate(aqm_emis_sources, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

  end subroutine aqm_emis_finalize

  subroutine aqm_emis_update(model, rc)
    type(ESMF_GridComp)            :: model
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: verbosity
    integer :: item
    logical :: isRinging
    character(len=ESMF_MAXSTR) :: name
    character(len=ESMF_MAXSTR) :: timeString
    type(ESMF_Alarm) :: alarm
    type(ESMF_Clock) :: clock
    type(ESMF_Time)  :: currTime

    integer, save :: timeSlice = 0

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- get component's information
    call NUOPC_CompGet(model, name=name, verbosity=verbosity, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- get component's configuration
    call ESMF_GridCompGet(model, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_ClockGetAlarm(clock, emAlarmName, alarm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    isRinging = ESMF_AlarmIsRinging(alarm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

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
      if (btest(verbosity,8)) then
        call ESMF_LogWrite(trim(name)//": "//rName//": reading @ "//&
          trim(timeString), ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if
      if (emIOFmt == ESMF_IOFMT_BIN) then
        do item = 1, size(aqm_emis_sources)
          call AQMIO_Read(emIO, (/ aqm_emis_fields(item) /), fileName=aqm_emis_sources(item), &
            filePath=emPath, iofmt=emIOFmt, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end do
      else
        timeSlice = timeSlice + 1
        call AQMIO_Read(emIO, aqm_emis_fields, timeSlice=timeSlice, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if
      call ESMF_AlarmRingerOff(alarm, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

  end subroutine aqm_emis_update

  subroutine aqm_emis_read(spcname, buffer, localDe, rc)
    character(len=*),  intent(in)    :: spcname
    real,              intent(inout) :: buffer(*)
    integer, optional, intent(in)    :: localDe
    integer, optional, intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: item, i, j, k
    integer, dimension(2) :: lb, ub
    real(ESMF_KIND_R4),   pointer :: fptr(:,:)
    type(aqm_state_type), pointer :: stateIn

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- NOTE: input emissions need to be converted to surface densities here
    ! -- since grid cell area is set to 1 internally.

    call aqm_model_get(stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    do item = 1, size(aqm_emis_species)
      if (trim(spcname) == trim(aqm_emis_species(item))) then
        nullify(fptr)
        call ESMF_FieldGet(aqm_emis_fields(item), localDe=localDe, &
          computationalLBound=lb, computationalUBound=ub, &
          farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__)) then
          if (present(rc)) rc = AQM_RC_FAILURE
          return  ! bail out
        end if
        select case (aqm_emis_dens_flag(item))
          case (:-1)
            ! -- this case indicates that input emissions are provided as totals/cell
            ! -- while surface densities are required and should never occur in CMAQ
            k = 0
            do j = lb(2), ub(2)
              do i = lb(1), ub(1)
                k = k + 1
                if (abs(fptr(i,j)) < 1.e+15) & !! TEST
                buffer(k) = buffer(k) &
                  + aqm_emis_factors(item) * fptr(i,j) / stateIn % area(i,j) &
                                                       / stateIn % area(i,j)
              end do
            end do
          case (0)
            ! -- emissions are totals over each grid cell
            k = 0
            do j = lb(2), ub(2)
              do i = lb(1), ub(1)
                k = k + 1
                if (abs(fptr(i,j)) < 1.e+15) & !! TEST
                buffer(k) = buffer(k) + aqm_emis_factors(item) * fptr(i,j) / stateIn % area(i,j)
              end do
            end do
          case (1:)
            ! -- emissions are already provided as surface densities, no need to normalize
            k = 0
            do j = lb(2), ub(2)
              do i = lb(1), ub(1)
                k = k + 1
                if (abs(fptr(i,j)) < 1.e+15) & !! TEST
                buffer(k) = buffer(k) + aqm_emis_factors(item) * fptr(i,j)
              end do
            end do
        end select
      end if
    end do

  end subroutine aqm_emis_read
    
end module aqm_emis_mod
