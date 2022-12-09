module aqm_comp_mod

  use ESMF
  use NUOPC
  use NUOPC_Model, only : NUOPC_ModelGet
  use aqm_rc_mod
  use aqm_config_mod
  use aqm_types_mod, only : AQM_MAXSTR
  use aqm_logger_mod
  use aqm_model_mod
  use aqm_emis_mod
  use aqm_prod_mod
  use aqm_internal_mod
  use cmaq_model_mod

  implicit none

  public

contains

  subroutine aqm_comp_create(model, rc)
    type(ESMF_GridComp) :: model
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                 :: localrc, stat
    integer                 :: deCount
    integer                 :: yy, mm, dd, h, m, s, julday
    real(ESMF_KIND_R8)      :: dts
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Clock)        :: clock
    type(ESMF_VM)           :: vm
    type(ESMF_Time)         :: startTime, stopTime
    type(ESMF_TimeInterval) :: TimeStep
    type(aqm_internal_state_type) :: is
    type(aqm_config_type), pointer :: config


    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- allocate memory for the internal state and store it into component
    allocate(is % wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out
    call ESMF_GridCompSetInternalState(model, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__)) &
      return  ! bail out

    ! -- query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out

    ! -- allocate model's internal configuration
    call aqm_model_config_create(rc=localrc)
    if (aqm_rc_check(localrc)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to initialize model configuration", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

    ! -- read model configuration
    nullify(config)
    call aqm_model_get(deCount=deCount, config=config, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg="Failed to get model config", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

    if (deCount > 0) then
      call aqm_config_init(model, config, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, &
        msg="Failed to read model configuration", &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
    end if

    ! -- initialize emission subsystem
    call aqm_emis_init(model, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, &
      msg="Failed to initialize emissions subsystem", &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out

    ! -- initialize products
    call aqm_prod_init(model, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, &
      msg="Failed to initialize products", &
      line=__LINE__, file=__FILE__, rcToReturn=rc)) return  ! bail out

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

  !-----------------------------------------------------------------------------

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
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- set model internal timestep vector (HHMMSS)
    tstep( 1 ) = config % ctm_tstep         ! TSTEP(1) = local output step
    tstep( 2 ) = tstep( 1 )                 ! TSTEP(2) = sciproc sync. step (chem)
    tstep( 3 ) = tstep( 2 )                 ! TSTEP(3) = twoway model time step

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

    ! -- update output products if needed
    call aqm_prod_update(model, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg="Failed to update products", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine aqm_comp_advance

  !-----------------------------------------------------------------------------

  subroutine aqm_comp_finalize(model, rc)
    type(ESMF_GridComp), intent(in)  :: model
    integer,             intent(out) :: rc

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- finalize CMAQ driver
    call cmaq_model_finalize(rc=rc)
    if (aqm_rc_check(rc, file=__FILE__, line=__LINE__)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to finalize CMAQ driver", &
        line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    end if

    ! -- finalize AQM emission configuration
    call aqm_emis_finalize(model, rc=rc)
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
          case ("inst_tracer_diag_aod")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateOut % aod, rc=rc)
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
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % vwind, rc=rc)
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
          case ("inst_pres_interface")
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % pri, rc=rc)
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
            call ESMF_FieldGet(field, localDe=localDe, farrayPtr=stateIn % uwind, rc=rc)
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

  subroutine aqm_field_diagnostics(model, rc)
    type(ESMF_GridComp)            :: model
    integer, optional, intent(out) :: rc

    ! -- local variables
    logical :: isConnected
    integer :: localrc
    integer :: item
    integer :: localDe, localDeCount, rank
    integer :: fieldCount, maxLength
    character(len=ESMF_MAXSTR)          :: msgString
    character(len=ESMF_MAXSTR)          :: name
    character(len=ESMF_MAXSTR), pointer :: connectedList(:)
    character(len=ESMF_MAXSTR), pointer :: fieldNames(:)
    real(ESMF_KIND_R8)                              :: minValue, maxValue
    real(ESMF_KIND_R8), dimension(:),       pointer :: localMin, localMax, globalMin, globalMax
    real(ESMF_KIND_R8), dimension(:),       pointer :: fp1d
    real(ESMF_KIND_R8), dimension(:,:),     pointer :: fp2d
    real(ESMF_KIND_R8), dimension(:,:,:),   pointer :: fp3d
    real(ESMF_KIND_R8), dimension(:,:,:,:), pointer :: fp4d
    type(ESMF_Field), pointer :: fieldList(:)
    type(ESMF_State)  :: importState
    type(ESMF_VM)     :: vm

    ! -- local parameters
    character(len=*), parameter :: rName = "diagnostics"

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- get component information
    call NUOPC_CompGet(model, name=name, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    ! -- query the Component for its VM and importState
    call ESMF_GridCompGet(model, vm=vm, importState=importState, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    nullify(fieldNames, connectedList, fieldList)
    call NUOPC_GetStateMemberLists(importState, StandardNameList=fieldNames, &
      ConnectedList=connectedList, fieldList=fieldList, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    isConnected = .false.
    if (associated(fieldNames)) isConnected = any(connectedList == "true")

    nullify(localMin, localMax, globalMin, globalMax)

    ! -- check values of imported fields, if requested
    if (isConnected) then

      fieldCount = size(fieldList)

      allocate(localMin(fieldCount), localMax(fieldCount), &
        globalMin(fieldCount), globalMax(fieldCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return

      localMin = huge(0._ESMF_KIND_R8)
      localMax = -localMin
      globalMin = 0._ESMF_KIND_R8
      globalMax = 0._ESMF_KIND_R8

      ! -- find longest field name for formatting purpose
      maxLength = 0
      do item = 1, fieldCount
        maxLength = max(maxLength, len_trim(fieldNames(item)))
      end do

      do item = 1, fieldCount
        if (connectedList(item) == "true") then
          ! --- get field data
          call ESMF_FieldGet(fieldList(item), rank=rank, &
            localDeCount=localDeCount, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out

          do localDe = 0, localDeCount - 1
            minValue = localMin(item)
            maxValue = localMax(item)
            select case(rank)
              case(1)
                call ESMF_FieldGet(fieldList(item), localDe=localDe, farrayPtr=fp1d, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__, &
                  rcToReturn=rc)) return  ! bail out
                minValue = minval(fp1d)
                maxValue = maxval(fp1d)
              case(2)
                call ESMF_FieldGet(fieldList(item), localDe=localDe, farrayPtr=fp2d, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__, &
                  rcToReturn=rc)) return  ! bail out
                minValue = minval(fp2d)
                maxValue = maxval(fp2d)
              case(3)
                call ESMF_FieldGet(fieldList(item), localDe=localDe, farrayPtr=fp3d, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__, &
                  rcToReturn=rc)) return  ! bail out
                minValue = minval(fp3d)
                maxValue = maxval(fp3d)
              case(4)
                call ESMF_FieldGet(fieldList(item), localDe=localDe, farrayPtr=fp4d, rc=localrc)
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
                  rcToReturn=rc)
                return ! bail out
            end select
            localMin(item) = min(minValue, localMin(item))
            localMax(item) = max(maxValue, localMax(item))
          end do
        end if
      end do

      ! -- compute global min and max values
      call ESMF_VMAllReduce(vm, localMin, globalMin, fieldCount, ESMF_REDUCE_MIN, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      call ESMF_VMAllReduce(vm, localMax, globalMax, fieldCount, ESMF_REDUCE_MAX, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      ! -- log results
      do item = 1, fieldCount
        if (connectedList(item) == "true") then
          write(msgString,'(a,": ",a,": ",a,"[",i0,"]: local/global min/max =",4g20.8)') &
            trim(name), rName, fieldNames(item)(1:maxLength), item, localMin(item), &
            localMax(item), globalMin(item), globalMax(item)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
        end if
      end do

      deallocate(localMin, localMax, globalMin, globalMax, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return

      if (associated(fieldNames)) then
        deallocate(fieldNames, stat=localrc)
        if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return
      end if
      if (associated(connectedList)) then
        deallocate(connectedList, stat=localrc)
        if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return
      end if
      if (associated(fieldList)) then
        deallocate(fieldList, stat=localrc)
        if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return
      end if
    end if

  end subroutine aqm_field_diagnostics

end module aqm_comp_mod
