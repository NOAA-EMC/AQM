module aqm_logger_mod

  use ESMF

  logical :: is_logger_on = .false.

  integer, parameter :: aqm_logger_task = 0
  integer, parameter :: aqm_logger_unit = 6

  private

  public :: aqm_logger_init
  public :: aqm_logger_log
  public :: aqm_logger_logstep
  public :: aqm_logger_active

contains

  logical function aqm_logger_active()

    aqm_logger_active = is_logger_on

  end function aqm_logger_active

  subroutine aqm_logger_init(model, rc)

    type(ESMF_GridComp), intent(in)  :: model
    integer, optional,   intent(out) :: rc

    ! -- local variables
    integer       :: localrc
    integer       :: localPet
    type(ESMF_VM) :: vm

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- retrieve component's VM
    call ESMF_GridCompGet(model, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- retrieve local PET
    call ESMF_VMGet(vm, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    is_logger_on = (localPet == aqm_logger_task)

  end subroutine aqm_logger_init

  subroutine aqm_logger_log(msg, logUnit, rc)

    character(len=*),           intent(in)  :: msg
    integer,          optional, intent(in)  :: logUnit
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer                    :: localrc
    integer                    :: unit

    ! print timestep details

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    unit = aqm_logger_unit
    if (present(logUnit)) unit = logUnit

    if (is_logger_on) write(unit, '(a)') trim(msg)

    call ESMF_LogWrite(trim(msg), rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

  end subroutine aqm_logger_log

  subroutine aqm_logger_logstep(model, msg, logUnit, rc)

    type(ESMF_GridComp),           intent(in)  :: model
    character(len=*),    optional, intent(in)  :: msg
    integer,             optional, intent(in)  :: logUnit
    integer,             optional, intent(out) :: rc

    ! -- local variables
    integer                    :: localrc
    integer                    :: unit
    character(len=ESMF_MAXSTR) :: timeString
    character(:), allocatable  :: msgString
    type(ESMF_Clock)           :: clock
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timeStep

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    unit = aqm_logger_unit
    if (present(logUnit)) unit = logUnit

    ! -- get component's clock
    call ESMF_GridCompGet(model, clock=clock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- get current time and time step
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    call ESMF_TimeGet(currTime, timeStringISOFrac=timeString, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    msgString = timeString
    call ESMF_TimeGet(currTime + timeStep, timeStringISOFrac=timeString, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    msgString = 'Advancing from '//trim(msgString) // ' to ' // timeString

    if (present(msg)) msgString = trim(msg) // ': ' // trim(msgString)

    if (is_logger_on) write(unit, '(a)') msgString

    call ESMF_LogWrite(msgString, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

  end subroutine aqm_logger_logstep

end module aqm_logger_mod
