module aqm_comp_mod

  use ESMF
  use NUOPC
  use NUOPC_Model, only : NUOPC_ModelGet
  use aqm_rc_mod
  use aqm_comm_mod
  use aqm_types_mod, only : AQM_MAXSTR
  use aqm_model_mod
  use aqm_io_mod
  use aqm_iodata_mod
  use cmaq_model_mod

  implicit none

  public 

contains

  subroutine aqm_comp_create(model, rc)
    type(ESMF_GridComp) :: model
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                 :: localrc
    integer                 :: comm
    integer                 :: yy, mm, dd, h, m
    real(ESMF_KIND_R8)      :: dts
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: clock
    type(ESMF_VM)           :: vm
    type(ESMF_Time)         :: startTime
    type(ESMF_TimeInterval) :: TimeStep


    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out

    ! -- init model communication subsystem over ESMF communicator
    call ESMF_GridCompGet(model, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out

    call ESMF_VMGet(vm, mpiCommunicator=comm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out

    ! -- initialize model after creation to setup correct communication
    call aqm_model_init(comm=comm, isolate=.true., rc=localrc)
    if (aqm_rc_check(localrc)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to initialize air quality model", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

    ! -- initialize model I/O
    call aqm_io_init(rc=localrc)
    if (aqm_rc_check(localrc)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to initialize I/O model subsystem", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

    ! -- read-in emission and background fields, setup internal parameters
    call aqm_model_config_init(rc=localrc)
    if (aqm_rc_check(localrc)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to initialize model configuration", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

    ! -- initialize internal clock
    ! -- get clock information
    call ESMF_ClockGet(clock, startTime=startTime, timeStep=timeStep, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    ! -- get forecast initial time
    call ESMF_TimeGet(startTime, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    ! -- get time step
    call ESMF_TimeIntervalGet(timeStep, s_r8=dts, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    ! -- set internal clock
    call aqm_model_clock_create(rc=localrc)
    if (aqm_rc_check(localrc)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to create model clock", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if
    call aqm_model_clock_set(yy=yy, mm=mm, dd=dd, h=h, m=m, dts=dts, tz=0, rc=localrc)
    if (aqm_rc_check(localrc)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to initialize model clock", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

#if 0
    ! -- allocate memory for internal workspace
    call aqm_backgd_init(rc=localrc)
    if (aqm_rc_check(localrc)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to initialize I/O model subsystem", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

    ! -- read-in emission and background fields
    call aqm_backgd_read(rc=localrc)
    if (aqm_rc_check(localrc)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to initialize I/O model subsystem", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if
#endif

  end subroutine aqm_comp_create

  subroutine aqm_comp_init(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount
    type(aqm_config_type), pointer :: config

    ! -- begin
    if (present(rc)) rc = ESMF_FAILURE

    call aqm_model_get(deCount=deCount, config=config, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) return

    if (deCount > 0) then
      call cmaq_model_init(rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) then
        call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to initialize model", &
          line=__LINE__, file=__FILE__, rcToReturn=rc)
        return  ! bail out
      end if
    end if

    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine aqm_comp_init


  subroutine aqm_comp_advance(clock, rc)

    type(ESMF_Clock), intent(in) :: clock
    integer,         intent(out) :: rc

    ! -- local variables
    integer                 :: deCount
    integer                 :: julday, yy, mm, dd, h, m, s
    integer                 :: jdate, jtime, tstep(3)
    integer(ESMF_KIND_I8)   :: advanceCount
    real(ESMF_KIND_R8)      :: dts
    character(len=AQM_MAXSTR) :: tStamp
    type(ESMF_Time)         :: currTime
    type(ESMF_TimeInterval) :: timeStep
    type(aqm_config_type), pointer :: config

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- check if model is active on this PET, bail out if not
    call aqm_model_get(deCount=deCount, config=config, rc=rc)
    if (aqm_rc_check(rc, file=__FILE__, line=__LINE__)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to get model info", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

    if (deCount < 1) return

    ! -- get current time and set model's internal clock
    call ESMF_ClockPrint(clock, &
      preString="aqm: run(): time step : ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="aqm: run(): time stamp: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, &
      advanceCount=advanceCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_TimeIntervalGet(timeStep, h=h, m=m, s=s, s_r8=dts, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- set model internal timestep
    tstep( 1 ) = 0
    tstep( 2 ) = h * 100000 + m * 100 + s
    tstep( 3 ) = tstep( 2 )

    call ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, &
      dayOfYear=julday, timeString=tStamp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- set model internal date and time
    jdate = yy * 1000 + julday
    jtime = h * 100000 + m * 100 + s

    call cmaq_model_advance(jdate, jtime, tstep, rc=rc)
    if (aqm_rc_check(rc, file=__FILE__, line=__LINE__)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to advance model", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

  end subroutine aqm_comp_advance


  subroutine aqm_comp_finalize(rc)
    integer, intent(out) :: rc

    ! -- begin
    rc = ESMF_SUCCESS

    call aqm_model_destroy()

  end subroutine aqm_comp_finalize

  !-----------------------------------------------------------------------------

  subroutine aqm_comp_connect(stateType, state, fieldNames, rc)
    character(len=*),  intent(in)  :: stateType
    type(ESMF_State),  intent(in)  :: state
    character(len=*),  intent(in)  :: fieldNames(:)
    integer,           intent(out) :: rc

    ! -- begin
    rc = ESMF_RC_NOT_IMPL

    select case (trim(stateType))
      case('import','i')
        call aqm_comp_import(state, fieldNames, rc)
      case('export','e')
        call aqm_comp_export(state, fieldNames, rc)
      case default
        ! not implemented
    end select

  end subroutine aqm_comp_connect


  subroutine aqm_comp_export(state, fieldNames, rc)
    type(ESMF_State),               intent(in) :: state
    character(len=*), dimension(:), intent(in) :: fieldNames
    integer, intent(out) :: rc

    ! -- local variables
    type(aqm_state_type), pointer :: stateOut
    type(ESMF_Field)               :: field
    integer                        :: item, localDe, localDeCount

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- check if model is active on this PET, bail out if not
    call aqm_model_get(deCount=localDeCount, rc=rc)
    if (aqm_rc_check(rc, file=__FILE__, line=__LINE__)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to get model info", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

    if (localDeCount < 1) return

    do item = 1, size(fieldNames)

      call ESMF_StateGet(state, field=field, &
        itemName=trim(fieldNames(item)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail

      do localDe = 0, localDeCount-1

        call aqm_model_get(stateOut=stateOut, de=localDe, rc=rc)
        if (aqm_rc_check(rc)) then
          call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
            msg="Failed to retrieve model's export state", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
          return  ! bail out
        end if

        select case (trim(fieldNames(item)))
          case ("inst_tracer_mass_frac")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateOut % tr3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case default
            ! -- unused field
        end select

      end do
      call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail
    end do

  end subroutine aqm_comp_export

  subroutine aqm_comp_import(state, fieldNames, rc)
    type(ESMF_State), intent(in)  :: state
    character(len=*), intent(in)  :: fieldNames(:)
    integer,          intent(out) :: rc

    ! -- local variables
    type(aqm_state_type), pointer :: stateIn
    type(ESMF_Field)               :: field
    integer                        :: item, localDe, localDeCount

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- check if model is active on this PET, bail out if not
    call aqm_model_get(deCount=localDeCount, rc=rc)
    if (aqm_rc_check(rc, file=__FILE__, line=__LINE__)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to get model info", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

    if (localDeCount < 1) return

    do item = 1, size(fieldNames)

      call ESMF_StateGet(state, field=field, &
        itemName=trim(fieldNames(item)), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail

      do localDe = 0, localDeCount-1

        call aqm_model_get(stateIn=stateIn, de=localDe, rc=rc)
        if (aqm_rc_check(rc)) then
          call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
            msg="Failed to retrieve model's import state", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
          return  ! bail out
        end if

        select case (trim(fieldNames(item)))
          case ("inst_pres_interface")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % pr3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call aqm_model_set(numIntLayers=size(stateIn % pr3d,dim=3), de=localDe)
          case ("inst_pres_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % prl3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call aqm_model_set(numModLayers=size(stateIn % prl3d,dim=3), de=localDe)
          case ("inst_temp_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % tk3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("soil_type")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % stype2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_pbl_height")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % pb2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("surface_cell_area")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % area, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_convective_rainfall_amount")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % rc2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_exchange_coefficient_heat_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % exch, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_friction_velocity")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % us2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_geop_interface")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % ph3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_geop_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % phl3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_tracer_mass_frac")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % tr3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call aqm_model_set(numTracers=size(stateIn % tr3d, dim=4), de=localDe)
          case ("inst_omega_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % ws3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_rainfall_amount")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % rn2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_soil_moisture_content")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % sm3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call aqm_model_set(numSoilLayers=size(stateIn % sm3d, dim=3), de=localDe)
          case ("inst_down_sw_flx")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % rsds, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_land_sea_mask")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % slmsk2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_temp_height_surface")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % ts2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_up_sensi_heat_flx")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % hf2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_lwe_snow_thickness")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % snwdph2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("vegetation_type")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % vtype2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_vegetation_area_frac")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % vfrac2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_zonal_wind_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % us3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_merid_wind_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % vs3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_surface_roughness")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % zorl2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case default
            ! -- unused field
        end select
      end do
    end do

  end subroutine aqm_comp_import

  !-----------------------------------------------------------------------------

  subroutine fieldPrintMinMax(field, vm, global, rc)
    type(ESMF_Field),        intent(in)  :: field
    type(ESMF_VM), optional, intent(in)  :: vm
    logical,       optional, intent(in)  :: global
    integer,       optional, intent(out) :: rc

    ! local variables
    type(ESMF_VM)               :: localVM
    real(ESMF_KIND_R8), pointer :: fp1d(:), fp2d(:,:), fp3d(:,:,:), fp4d(:,:,:,:)
    real(ESMF_KIND_R8)          :: fieldMaxValue, fieldMinValue, maxValue, minValue
    real(ESMF_KIND_R8)          :: globalMaxValue(1), globalMinValue(1)
    integer                     :: localDe, localDeCount, localPet, localrc, rank
    logical                     :: addGlobal
    character(len=ESMF_MAXSTR)  :: fieldName

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    addGlobal = .false.
    if (present(global)) addGlobal = global

    if (present(vm)) then
      localVM = vm
    else
      call ESMF_VMGetCurrent(localVM, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return  ! bail out
    end if

    call ESMF_VMGet(localVM, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    call ESMF_FieldGet(field, rank=rank, localDeCount=localDeCount, &
      name=fieldName, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    fieldMinValue = huge(1.0_ESMF_KIND_R8)
    fieldMaxValue = -fieldMinValue

    do localDe = 0, localDeCount - 1
      select case(rank)
        case(1)
          call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fp1d, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return  ! bail out
          minValue = minval(fp1d)
          maxValue = maxval(fp1d)
        case(2)
          call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fp2d, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return  ! bail out
          minValue = minval(fp2d)
          maxValue = maxval(fp2d)
        case(3)
          call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fp3d, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return  ! bail out
          minValue = minval(fp3d)
          maxValue = maxval(fp3d)
        case(4)
          call ESMF_FieldGet(field, localDe=localDe, farrayPtr=fp4d, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return  ! bail out
          minValue = minval(fp4d)
          maxValue = maxval(fp4d)
        case default
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="Field rank not implemented.", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=localrc)
          return ! bail out
      end select
      fieldMinValue = min(fieldMinValue, minValue)
      fieldMaxValue = max(fieldMaxValue, maxValue)
      write(6,'(a,":",i0,2x,"DE: ",i0,2x,a," - checking  - min/max = ",2g16.6)') 'PET', &
         localPet, localDe, trim(fieldName), minValue, maxValue
    end do

    if (addGlobal) then

      globalMinValue(1) = 0._ESMF_KIND_R8
      globalMaxValue(1) = 0._ESMF_KIND_R8

      call ESMF_VMReduce(localVM, (/ fieldMinValue /), globalMinValue, 1, &
        reduceflag=ESMF_REDUCE_MIN, rootPet=0, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return  ! bail out
        return  ! bail out
      call ESMF_VMReduce(localVM, (/ fieldMaxValue /), globalMaxValue, 1, &
        reduceflag=ESMF_REDUCE_MAX, rootPet=0, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return  ! bail out

      if (localPet == 0) then
         write(6,'(a,":",a," - checking  - min/max = ",2g16.6)') 'Field', &
           trim(fieldName), globalMinValue, globalMaxValue
      end if

    end if

  end subroutine fieldPrintMinMax

  !-----------------------------------------------------------------------------

end module aqm_comp_mod
