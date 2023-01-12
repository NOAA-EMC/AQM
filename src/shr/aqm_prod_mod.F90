module aqm_prod_mod

  use ESMF
  use NUOPC
  use aqmio
  use aqm_rc_mod
  use aqm_emis_mod, only : aqm_internal_emis_type
  use aqm_internal_mod

  implicit none

  ! -- parameters
  character(len=*),   parameter :: rName    = "products"

  ! -- internal variables

  private

  public :: aqm_prod_init
  public :: aqm_prod_compute
  public :: aqm_prod_units_set
  public :: aqm_prod_update

contains

  subroutine aqm_prod_field_init(em, rc)
    type(aqm_internal_emis_type)   :: em
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer            :: n
    integer            :: localrc
    real(ESMF_KIND_R8) :: initValue

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- reset product fields
    do n = 1, size(em % fields)
      select case (trim(em % units(n)))
        case ("MIN")
          initValue = huge(1._ESMF_KIND_R4)    ! this prevents overflow converting from R8 to R4 in ESMF
        case default
          initValue = 0._ESMF_KIND_R8
      end select
      call ESMF_FieldFill(em % fields(n), dataFillScheme="const", &
        const1=initValue, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return
    end do

  end subroutine aqm_prod_field_init

  subroutine aqm_prod_units_set(field, units, rc)
    type(ESMF_Field)               :: field
    character(len=*),  intent(in)  :: units
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer         :: localrc
    type(ESMF_Info) :: info

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- add units to field
    call ESMF_InfoGetFromHost(field, info, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    call ESMF_InfoSet(info, "units", units, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

  end subroutine aqm_prod_units_set

  subroutine aqm_prod_init(model, rc)
    type(ESMF_GridComp)            :: model
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                    :: localrc
    integer                    :: verbosity
    integer                    :: prodCount, item, n
    integer(ESMF_KIND_I4)      :: dts, ts
    character(len=ESMF_MAXSTR) :: name
    character(len=ESMF_MAXSTR) :: msgString
    type(ESMF_Clock)           :: clock
    type(ESMF_Config)          :: config
    type(ESMF_TimeInterval)    :: timeInterval
    type(aqm_internal_state_type) :: is
    type(aqm_internal_emis_type), pointer :: em

    character(len=*), parameter :: pName = "init"

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
    call ESMF_GridCompGet(model, clock=clock, config=config, rc=localrc)
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

    prodCount = 0
    if (associated(is % wrap % emis)) then
      do item = 1, size(is % wrap % emis)
        em => is % wrap % emis(item)
        if (trim(em % type) == "product") then
          prodCount = prodCount + 1
          ! -- set default log prefix label and verbosity
          em % logprefix = trim(name)//": "//rName &
            // ": " // em % name
          em % verbose   = btest(verbosity,8)
          ! -- setup products
          ! 1. initialize field payload to zero
          call aqm_prod_field_init(em, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          ! 2. initialize weights
          do n = 1, size(em % species)
            if (trim(em % units(n)) == "AVERAGE") then
              call ESMF_AlarmGet(em % alarm, ringInterval=timeInterval, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__,  &
                rcToReturn=rc)) &
                return  ! bail out
              call ESMF_TimeIntervalGet(timeInterval, s=ts, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__,  &
                rcToReturn=rc)) &
                return  ! bail out
              call ESMF_ClockGet(clock, timeStep=timeInterval, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__,  &
                rcToReturn=rc)) &
                return  ! bail out
              call ESMF_TimeIntervalGet(timeInterval, s=dts, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__,  &
                rcToReturn=rc)) &
                return  ! bail out
              em % factors(n) = real(dts, kind=ESMF_KIND_R4) / ts
            else
              em % factors(n) = 1._ESMF_KIND_R4
            end if
          end do
        end if
      end do
    end if

    if (prodCount > 0) then

      if (btest(verbosity,8)) then
        write(msgString,'(a,": ",a,": ",a,": types[",i0,"]: ")') trim(name), &
          trim(rName), trim(pName), prodCount
        n = 0
        do item = 1, size(is % wrap % emis)
          if (trim(is % wrap % emis(item) % type) == "product") then
            n = n + 1
            msgString = trim(msgString) // " " // is % wrap % emis(item) % name
            if (n < prodCount) msgString = trim(msgString) // ","
          end if
        end do
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if

    else

      if (btest(verbosity,8)) then
        write(msgString,'(a,": ",a,": ",a,": types: none")') trim(name), &
          trim(rName), trim(pName)
        call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if

    end if

  end subroutine aqm_prod_init

  subroutine aqm_prod_compute(em, cgrid, n, spc, rc)
    type(aqm_internal_emis_type), pointer  :: em
    real,                      intent(in)  :: cgrid(:,:,:,:)
    integer,                   intent(in)  :: n
    integer,                   intent(in)  :: spc
    integer, optional,         intent(out) :: rc

    ! -- local variables
    integer                       :: localrc
    integer                       :: item
    integer                       :: i, j, c, r
    integer, dimension(2)         :: lb, ub
    real(ESMF_KIND_R4),   pointer :: fptr(:,:)

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- return if no internal emission data set
    if (.not.associated(em)) return

    if (trim(em % type) == "product") then

      call ESMF_FieldGet(em % fields(n), &
        computationalLBound=lb, computationalUBound=ub, &
        farrayPtr=fptr, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__)) then
        if (present(rc)) rc = AQM_RC_FAILURE
        return  ! bail out
      end if
      select case (trim(em % units(n)))
        case ("AVERAGE")
          r = 0
          do j = lb(2), ub(2)
            r = r + 1
            c = 0
            do i = lb(1), ub(1)
              c = c + 1
              fptr(i,j) = fptr(i,j) + em % factors(n) * cgrid(c,r,1,spc)
            end do
          end do
        case ("MIN")
          r = 0
          do j = lb(2), ub(2)
            r = r + 1
            c = 0
            do i = lb(1), ub(1)
              c = c + 1
              fptr(i,j) = min(fptr(i,j), cgrid(c,r,1,spc))
            end do
          end do
        case ("MAX")
          r = 0
          do j = lb(2), ub(2)
            r = r + 1
            c = 0
            do i = lb(1), ub(1)
              c = c + 1
              fptr(i,j) = max(fptr(i,j), cgrid(c,r,1,spc))
            end do
          end do
        case default
          r = 0
          do j = lb(2), ub(2)
            r = r + 1
            c = 0
            do i = lb(1), ub(1)
              c = c + 1
              fptr(i,j) = cgrid(c,r,1,spc)
            end do
          end do
      end select

    end if

  end subroutine aqm_prod_compute
    
  subroutine aqm_prod_update(model, rc)
    type(ESMF_GridComp)            :: model
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                       :: localrc
    integer                       :: item
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

      isRinging = (trim(em % type) == "product")

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
          call ESMF_LogWrite(trim(em % logprefix)//": writing "//&
            trim(em % name)//" @ "//trim(timeString), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        em % irec = em % irec + 1
        call AQMIO_Write(em % IO, em % fields, timeSlice=em % irec, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if ( em % sync ) then
          call AQMIO_Sync(em % IO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end if
        call ESMF_AlarmRingerOff(em % alarm, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        ! -- reset product fields
        call aqm_prod_field_init(em, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if

    end do

  end subroutine aqm_prod_update

end module aqm_prod_mod
