module aqm_comp_mod

  use ESMF
  use NUOPC
  use NUOPC_Model, only : NUOPC_ModelGet
  use aqm_rc_mod
  use aqm_comm_mod
  use aqm_types_mod, only : AQM_MAXSTR
  use aqm_model_mod
  use aqm_io_mod
  use aqm_emis_mod
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

    ! -- initialize emission subsystem
    call aqm_emis_init(model, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, &
      msg="Failed to initialize emissions subsystem", &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out

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

  end subroutine aqm_comp_create

  subroutine aqm_comp_init(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount

    ! -- begin
    if (present(rc)) rc = ESMF_FAILURE

    call aqm_model_get(deCount=deCount, rc=localrc)
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


  subroutine aqm_comp_advance(model, rc)

    type(ESMF_GridComp), intent(in)  :: model
    integer,             intent(out) :: rc

    ! -- local variables
    integer                 :: localrc
    integer                 :: deCount
    integer                 :: julday, yy, mm, dd, h, m, s
    integer                 :: jdate, jtime, tstep(3)
    integer(ESMF_KIND_I8)   :: advanceCount
    real(ESMF_KIND_R8)      :: dts
    character(len=AQM_MAXSTR) :: tStamp
    type(ESMF_Clock)        :: clock
    type(ESMF_Time)         :: currTime
    type(ESMF_TimeInterval) :: timeStep
    type(aqm_config_type), pointer :: config => null()

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- check if model is active on this PET, bail out if not
    call aqm_model_get(deCount=deCount, config=config, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to get model info", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

    if (deCount < 1) return

    ! -- get component's clock
    call ESMF_GridCompGet(model, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__)) &
      return  ! bail out

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

    ! -- set model internal timestep vector (HHMMSS)
    tstep( 1 ) = h * 10000 + m * 100 + s    ! TSTEP(1) = local output step
    tstep( 2 ) = tstep( 1 )                 ! TSTEP(2) = sciproc sync. step (chem)
    tstep( 3 ) = tstep( 2 )                 ! TSTEP(3) = twoway model time step

    config % ctm_tstep = tstep( 1 )

    call ESMF_TimeGet(currTime, yy=yy, mm=mm, dd=dd, h=h, m=m, s=s, &
      dayOfYear=julday, timeString=tStamp, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- set model internal date and time
    jdate = yy * 1000 + julday
    jtime = h * 10000 + m * 100 + s

    ! -- update input emissions if needed
    call aqm_emis_update(model, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed to update emissions", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- advance CMAQ
    call cmaq_model_advance(jdate, jtime, tstep, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to advance model", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

  end subroutine aqm_comp_advance


  subroutine aqm_comp_finalize(rc)
    integer, intent(out) :: rc

    ! -- begin
    rc = ESMF_SUCCESS

    call aqm_emis_finalize(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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
    type(ESMF_Field)              :: field
    integer                       :: item, localDe, localDeCount
    integer                       :: localrc

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- check if model is active on this PET, bail out if not
    call aqm_model_get(deCount=localDeCount, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) then
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

        call aqm_model_get(stateOut=stateOut, de=localDe, rc=localrc)
        if (aqm_rc_check(localrc)) then
          call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
            msg="Failed to retrieve model's export state", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
          return  ! bail out
        end if

        select case (trim(fieldNames(item)))
          case ("inst_tracer_mass_frac")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateOut % tr, rc=rc)
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
    type(ESMF_Field)              :: field
    integer                       :: item, localDe, localDeCount
    integer                       :: localrc

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- check if model is active on this PET, bail out if not
    call aqm_model_get(deCount=localDeCount, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) then
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

        call aqm_model_get(stateIn=stateIn, de=localDe, rc=localrc)
        if (aqm_rc_check(localrc)) then
          call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
            msg="Failed to retrieve model's import state", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
          return  ! bail out
        end if

        select case (trim(fieldNames(item)))
          case ("canopy_moisture_storage")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % wr, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("height")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % ht, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_aerodynamic_conductance")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % cmm, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_canopy_resistance")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % rc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_cloud_frac_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % cldfl, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_convective_rainfall_amount")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % rainc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_friction_velocity")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % ustar, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_geop_interface")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % phii, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call aqm_model_set(numIntLayers=size(stateIn % phii,dim=3), de=localDe)
          case ("inst_geop_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % phil, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call aqm_model_set(numModLayers=size(stateIn % phil,dim=3), de=localDe)
          case ("inst_land_sea_mask")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % slmsk, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_laten_heat_flx")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % lh, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_merid_wind_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % uwind, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_net_sw_flx")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % rgrnd, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_pbl_height")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % hpbl, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_pres_height_surface")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % psfc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_pres_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % prl, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_rainfall_amount")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % rain, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_sensi_heat_flx")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % hfx, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_soil_moisture_content")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % smois, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call aqm_model_set(numSoilLayers=size(stateIn % smois, dim=3), de=localDe)
          case ("inst_surface_roughness")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % zorl, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_spec_humid_height2m")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % q2m, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_temp_height2m")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % t2m, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_temp_height_surface")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % tsfc, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_temp_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % temp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_tracer_mass_frac")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % tr, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
            call aqm_model_set(numTracers=size(stateIn % tr, dim=4), de=localDe)
          case ("inst_zonal_wind_height10m")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % u10m, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_vegetation_area_frac")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % vfrac, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_merid_wind_height10m")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % v10m, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("inst_zonal_wind_levels")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % vwind, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("leaf_area_index")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % xlai, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("sea_ice_area_fraction")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % fice, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("soil_type")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % stype, rc=rc)
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
          case ("surface_snow_area_fraction")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % sncov, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail
          case ("temperature_of_soil_layer")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % stemp, rc=rc)
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
