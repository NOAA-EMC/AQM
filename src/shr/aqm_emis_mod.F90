module aqm_emis_mod

  use ESMF
  use NUOPC
  use aqmio
  use aqm_rc_mod
  use aqm_types_mod
  use aqm_io_mod
  use aqm_comm_mod
  use aqm_model_mod
  use aqm_config_mod

  implicit none

  ! -- parameters
  integer, parameter :: emNameLen = 16
  character(len=*), parameter :: emAlarmName = "emissions_alarm"
  character(len=*), parameter :: rName = "emissions"

  integer :: aqm_emis_num
  character(len=ESMF_MAXPATHLEN) :: emPath
  character(len=emNameLen),   allocatable :: aqm_emis_def(:,:)
  character(len=ESMF_MAXSTR), allocatable :: speciesList(:)
  character(len=ESMF_MAXSTR), allocatable :: sourceList(:)
  real(ESMF_KIND_R4),         allocatable :: factorList(:)
  type(ESMF_Field),           allocatable :: emFieldList(:)
  type(ESMF_GridComp)   :: emIO
  type(ESMF_IOFmt_flag) :: emIOFmt


  private

  public :: aqm_emis_num
  public :: aqm_emis_def
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
      label="emissions_frequency:", default="static", rc=rc)
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
      endif
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

      aqm_emis_def  = ""
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
        if (.not.eolFlag) fieldCount = fieldCount + 1
        if (btest(verbosity,8)) then
          if (eolFlag) then
            write(msgString, '("[",i0,"]: ",a," (not provided)")') &
              item, trim(tmpSpeciesList(item))
          else
            write(msgString, '("[",i0,"]: ",a," (",a,", scale=",g12.5,")")') &
              item, trim(tmpSpeciesList(item)), trim(tmpUnitsList(item)), &
              tmpFactorList(item)
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
        allocate(speciesList(fieldCount), factorList(fieldCount), &
          sourceList(fieldCount), emFieldList(fieldCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return

        fieldCount = 0
        do item = 1, rowCount
          if (len_trim(tmpSourceList(item)) > 0) then
            fieldCount = fieldCount + 1
            speciesList(fieldCount) = tmpSpeciesList(item)
            factorList(fieldCount)  = tmpFactorList(item)
            sourceList(fieldCount)  = tmpSourceList(item)
            emFieldList(fieldCount) = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, &
              name=sourceList(fieldCount), rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
          end if
        end do

        deallocate(tmpFactorList, tmpSourceList, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return

      end if

      ! -- build unique list of species

      ! -- 1. sort species names
      call aqm_emis_sort(tmpSpeciesList, tmpUnitsList)

      ! -- 2. select unique species
      aqm_emis_num = 1
      do item = 2, rowCount
        if (tmpSpeciesList(item) /= tmpSpeciesList(item-1)) &
          aqm_emis_num = aqm_emis_num + 1
      end do

      allocate(aqm_emis_def(aqm_emis_num,2), stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return

      aqm_emis_num = 1
      aqm_emis_def(aqm_emis_num, 1) = tmpSpeciesList(1)
      aqm_emis_def(aqm_emis_num, 2) = tmpUnitsList(1)
      do item = 2, rowCount
        if (tmpSpeciesList(item) /= tmpSpeciesList(item-1)) then
          aqm_emis_num = aqm_emis_num + 1
          aqm_emis_def(aqm_emis_num, 1) = tmpSpeciesList(item)
          aqm_emis_def(aqm_emis_num, 2) = tmpUnitsList(item)
        end if
      end do

      deallocate(tmpSpeciesList, tmpUnitsList, stat=stat)
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

    if (allocated(emFieldList)) then
      do item = 1, size(emFieldList)
        isCreated = ESMF_FieldIsCreated(emFieldList(item), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        call ESMF_FieldDestroy(emFieldList(item), rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end do
    end if

    if (allocated(speciesList)) then
      deallocate(speciesList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

    if (allocated(factorList)) then
      deallocate(factorList, stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

    if (allocated(sourceList)) then
      deallocate(sourceList, stat=stat)
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
        do item = 1, size(sourceList)
          call AQMIO_Read(emIO, (/ emFieldList(item) /), fileName=sourceList(item), &
            filePath=emPath, iofmt=emIOFmt, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end do
      else
        timeSlice = timeSlice + 1
        call AQMIO_Read(emIO, emFieldList, timeSlice=timeSlice, rc=localrc)
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

  subroutine aqm_emis_read(spcname, jdate, jtime, buffer, de, rc)
    character(len=*),  intent(in)  :: spcname
    integer,           intent(in)  :: jdate
    integer,           intent(in)  :: jtime
    real(AQM_KIND_R4), intent(out) :: buffer(:,:)
    integer, optional, intent(in)  :: de
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: item
    real(ESMF_KIND_R4), pointer :: fptr(:,:)

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    buffer = 0._AQM_KIND_R4

    do item = 1, size(speciesList)
      if (trim(spcname) == trim(speciesList(item))) then
        call ESMF_FieldGet(emFieldList(item), localDe=de, &
          farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__)) then
          if (present(rc)) rc = AQM_RC_FAILURE
          return  ! bail out
        end if
        buffer = buffer + factorList(item) * fptr
      end if
    end do

  end subroutine aqm_emis_read

  ! -- auxiliary methods

  subroutine aqm_emis_sort(list, auxlist)
    character(len=*), intent(inout) :: list(:)
    character(len=*), intent(inout) :: auxlist(:)

    ! -- local variables
    integer :: n

    ! -- begin
    n = size(list)

    if (size(auxlist) < n) return

    call aqm_emis_quicksort(n, list, auxlist, 1, n)

  end subroutine aqm_emis_sort

! ---------------------------------------------------------------------
!  The following methods (aqm_emis_partition, aqm_emis_quicksort) are
!  modified versions of the original FMS qksrt_partition and
!  qksrt_quicksort written by Magnus Lie Hetland and released under the
!  GNU Lesser General Public License as part of GFDL Flexible Modeling
!  System (FMS). Terms of the GNU LGPL are included below.
! ---------------------------------------------------------------------
!***********************************************************************
!*                   GNU Lesser General Public License
!*
!* This file is part of the GFDL Flexible Modeling System (FMS).
!*
!* FMS is free software: you can redistribute it and/or modify it under
!* the terms of the GNU Lesser General Public License as published by
!* the Free Software Foundation, either version 3 of the License, or (at
!* your option) any later version.
!*
!* FMS is distributed in the hope that it will be useful, but WITHOUT
!* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!* for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************

  function aqm_emis_partition(n, list, order, start, end) result(top)
    integer, intent(in) :: n
    character(len=*), intent(inout) :: list(n), order(n)
    integer, intent(in) :: start, end

    ! -- local variables
    character(len=len(list(1))) pivot
    character(len=len(order(1))) pvaux
    integer bottom, top
    logical done

    ! -- begin
    pivot = list(end)                   ! Partition around the last value
    pvaux = order(end)                  ! Partition around the last value
    bottom = start-1                    ! Start outside the area to be partitioned
    top = end                           ! Ditto

    done = .false.
    do while (.not. done)               ! Until all elements are partitioned...

      do while (.not. done)             ! Until we find an out of place element...
        bottom = bottom+1               ! ... move the bottom up.

        if (bottom == top) then         ! If we hit the top...
          done = .true.                 ! ... we are done.
          exit
        endif

        if (list(bottom) > pivot) then  ! Is the bottom out of place?
          list(top)  = list(bottom)     ! Then put it at the top...
          order(top) = order(bottom)    ! Then put it at the top...
          exit                          ! ... and start searching from the top.
        endif
      enddo

      do while (.not. done)             ! Until we find an out of place element...
          top = top-1                   ! ... move the top down.

        if (top == bottom) then         ! If we hit the bottom...
          done = .true.                 ! ... we are done.
          exit
        endif

        if (list(top) < pivot) then     ! Is the top out of place?
          list(bottom)  = list(top)     ! Then put it at the bottom...
          order(bottom) = order(top)    ! Then put it at the bottom...
          exit                          ! ...and start searching from the bottom.
        endif
      enddo
    enddo

    list(top)  = pivot                  ! Put the pivot in its place.
    order(top) = pvaux                  ! Put the pivot in its place.
     ! Return the split point

  end function aqm_emis_partition

  recursive subroutine aqm_emis_quicksort(n, list, order, start, end)
    integer, intent(in) :: n
    character(len=*), intent(inout) :: list(n)
    character(len=*), intent(inout) :: order(n)

    ! -- local variables
    integer, intent(in) :: start, end
    integer :: split

    ! -- begin
    if (start < end) then    ! If there are two or more elements...
      split = aqm_emis_partition(n, list, order, start, end)    ! ... partition the sublist...
      call aqm_emis_quicksort(n, list, order,  start, split-1)  ! ... and sort both halves.
      call aqm_emis_quicksort(n, list, order, split+1, end)
    endif
  end subroutine aqm_emis_quicksort
    
end module aqm_emis_mod
