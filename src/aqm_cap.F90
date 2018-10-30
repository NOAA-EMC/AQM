module AQM
  !-----------------------------------------------------------------------------
  ! AQM Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, inheritModel => SetServices

  use aqm_comp_mod
  
  implicit none

  ! -- import fields
  integer, parameter :: importFieldCount = 25
  character(len=*), dimension(importFieldCount), parameter :: &
    importFieldNames = (/ &
      "inst_pres_interface                  ", &
      "inst_pres_levels                     ", &
      "inst_geop_interface                  ", &
      "inst_geop_levels                     ", &
      "inst_temp_levels                     ", &
      "inst_zonal_wind_levels               ", &
      "inst_merid_wind_levels               ", &
      "inst_omega_levels                    ", &
      "inst_tracer_mass_frac                ", &
      "soil_type                            ", &
      "inst_pbl_height                      ", &
      "surface_cell_area                    ", &
      "inst_convective_rainfall_amount      ", &
      "inst_exchange_coefficient_heat_levels", &
      "inst_friction_velocity               ", &
      "inst_rainfall_amount                 ", &
      "inst_soil_moisture_content           ", &
      "inst_down_sw_flx                     ", &
      "inst_land_sea_mask                   ", &
      "inst_temp_height_surface             ", &
      "inst_up_sensi_heat_flx               ", &
      "inst_lwe_snow_thickness              ", &
      "vegetation_type                      ", &
      "inst_vegetation_area_frac            ", &
      "inst_surface_roughness               "  &
    /)
  ! -- export fields
  integer, parameter :: exportFieldCount = 1
  character(len=*), dimension(exportFieldCount), parameter :: &
    exportFieldNames = (/ &
      "inst_tracer_mass_frac                "  &
    /)

  ! -- verbosity
  integer :: verbosity

  private
  
  public SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! begin
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, inheritModel, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Provide InitializeP0 to switch from default IPDv00 to IPDv03
    call ESMF_GridCompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_MethodRemove(model, label=label_CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_CheckImport, &
      specRoutine=CheckImport, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(model, specLabel=label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! local variables
    character(len=5)     :: value

    ! begin
    rc = ESMF_SUCCESS

    ! get component verbosity
    call ESMF_AttributeGet(model, name="Verbosity", value=value, &
      defaultValue="min", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! convert value to verbosity
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min ","high", "max "/), &
      specialValueList=(/0,255,255/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! write to log files
    call ESMF_LogWrite("AQM: Verbosity = " // trim(value), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(model, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------
  subroutine InitializeP1(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! begin
    rc = ESMF_SUCCESS

    ! -- advertise imported fields
    if (importFieldCount > 0) then
      call NUOPC_Advertise(importState, importFieldNames, &
        TransferOfferGeomObject="cannot provide", &
        TransferOfferField="cannot provide", &
        SharePolicyField="share", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    end if

    ! -- advertise exported fields
    if (exportFieldCount > 0) then
      call NUOPC_Advertise(exportState, exportFieldNames, &
        TransferOfferGeomObject="cannot provide", &
        TransferOfferField="cannot provide", &
        SharePolicyField="share", &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    end if

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Field)              :: field
    type(ESMF_Clock)              :: clock
    type(ESMF_Grid)               :: grid
    type(ESMF_VM)                 :: vm
    type(ESMF_GeomType_flag)      :: geomtype
    type(ESMF_DistGrid)           :: distgrid
    type(ESMF_Array)              :: array
    integer                       :: de, item, localrc, localDe, tile
    integer                       :: comm, localPet
    real(ESMF_KIND_R8), dimension(:,:), pointer :: coord

    integer :: dimCount, tileCount, deCount, localDeCount
    integer, dimension(:),   allocatable :: deToTileMap, localDeToDeMap
    integer, dimension(:,:), allocatable :: minIndexPDe, maxIndexPDe, minIndexPTile, maxIndexPTile
    integer, dimension(:,:), allocatable :: computationalLBound, computationalUBound

    integer                    :: yy, mm, dd, h, m
    real(ESMF_KIND_R8)         :: dts
    type(ESMF_Time)            :: startTime
    type(ESMF_TimeInterval)    :: TimeStep
    character(len=255) :: msgString

    ! begin
    rc = ESMF_SUCCESS

    ! -- initialize air quality model
    call ESMF_GridCompGet(model, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- check if import fields are defined
    if (importFieldCount < 1) then 
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="This component requires import fields to be defined.", &
        line=__LINE__, file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

    ! -- check if export fields are defined
    if (exportFieldCount < 1) then 
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="This component requires export fields to be defined.", &
        line=__LINE__, file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

    ! -- query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, importState=importState, &
      exportState=exportState, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get coordinates from Grid object
    ! assume all fields on same grid
    ! use first field 
    call ESMF_StateGet(importState, field=field, &
      itemName=trim(importFieldNames(1)), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_FieldGet(field, geomtype=geomtype, localDeCount=localDeCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (geomtype == ESMF_GEOMTYPE_GRID) then
      call ESMF_FieldGet(field, grid=grid, array=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_ArrayGet(array, deCount=deCount, dimCount=dimCount, &
        tileCount=tileCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate(minIndexPDe(dimCount, deCount), maxIndexPDe(dimCount, deCount),  &
        minIndexPTile(dimCount, tileCount), maxIndexPTile(dimCount, tileCount), &
        computationalLBound(dimCount, localDeCount), computationalUBound(dimCount, localDeCount), &
        deToTileMap(deCount), localDeToDeMap(localDeCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
      call ESMF_ArrayGet(array, distgrid=distgrid, &
        deToTileMap=deToTileMap, localDeToDeMap=localDeToDeMap, &
        computationalLBound=computationalLBound, &
        computationalUBound=computationalUBound, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_DistGridGet(distgrid, &
        minIndexPDe=minIndexPDe, maxIndexPDe=maxIndexPDe, &
        minIndexPTile=minIndexPTile, maxIndexPTile=maxIndexPTile, &
        rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! -- init air quality model on local DEs
      call aqm_model_create(deCount=localDeCount, rc=rc)
      if (aqm_rc_check(rc)) then
        call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
          msg="Failed to initialize air quality model for localDeCount", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return  ! bail out
      end if

      call ESMF_VMGet(vm, localPet=localPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do localDe = 0, localDeCount-1
        de   = localDeToDeMap(localDe+1) + 1
        tile = deToTileMap(de)

        if (btest(verbosity,0)) then
          write(msgString,'("AQM: localDe: ",i4," DE: ",i4, " tile=",i2," minIndexPDe=",2i4,2x," maxIndexPDe=",2i4," minIndexPTile=",2i4," maxIndexPTile=",2i4,4i4)') &
            localDe, de-1, tile, minIndexPDe(:,de), maxIndexPDe(:,de), minIndexPTile(:,tile), maxIndexPTile(:,tile), &
            computationalLBound(:,localDe+1), computationalUBound(:,localDe+1)
          call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        end if

        ! -- set model domains for local DEs
        call aqm_model_domain_set(minIndexPDe=minIndexPDe(:,de), maxIndexPDe=maxIndexPDe(:,de), &
          minIndexPTile=minIndexPTile(:,tile), maxIndexPTile=maxIndexPTile(:,tile), &
          minIndexLocal=computationalLBound(:,localDe+1), maxIndexLocal=computationalUBound(:,localDe+1), &
          tile=deToTileMap(de), tileCount=tileCount, de=localDe, rc=rc)
        if (aqm_rc_check(rc)) then
          call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
            msg="Failed to set domain for air quality model", &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
          return  ! bail out
        end if

        ! -- get local coordinate arrays
        do item = 1, 2
          call ESMF_GridGetCoord(grid, coordDim=item, staggerloc=ESMF_STAGGERLOC_CENTER, &
            localDE=localDe, farrayPtr=coord, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call aqm_model_domain_coord_set(item, coord, de=localDe, rc=rc)
          if (aqm_rc_check(rc)) then
            call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
              msg="Failed to set coordinates for air quality model", &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)
            return  ! bail out
          end if
        end do

      end do
      deallocate(minIndexPDe, maxIndexPDe, minIndexPTile, maxIndexPTile, &
        computationalLBound, computationalUBound, &
        deToTileMap, localDeToDeMap, stat=localrc)
      if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out

    else
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="Imported fields can only be defined on Grid objects.", &
        line=__LINE__, file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

    ! -- init model communication subsystem over ESMF communicator
!   call ESMF_GridCompGet(model, vm=vm, rc=rc)
!   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!     line=__LINE__, &
!     file=__FILE__)) &
!     return  ! bail out
!   call ESMF_VMGet(vm, mpiCommunicator=comm, rc=rc)
!   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!     line=__LINE__, &
!     file=__FILE__)) &
!     return  ! bail out

!   ! -- initialize model after creation to setup correct communication
!   call aqm_model_init(comm=comm, isolate=.true., rc=rc)
!   if (aqm_rc_check(rc)) then
!     call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
!       msg="Failed to initialize air quality model", &
!       line=__LINE__, &
!       file=__FILE__, &
!       rcToReturn=rc)
!     return  ! bail out
!   end if

!   ! -- initialize model I/O
!   call aqm_io_init(rc=rc)
!   if (aqm_rc_check(rc)) then
!     call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
!       msg="Failed to initialize I/O model subsystem", &
!       line=__LINE__, &
!       file=__FILE__, &
!       rcToReturn=rc)
!     return  ! bail out
!   end if

!   ! -- read-in emission and background fields, setup internal parameters
    call aqm_model_config_init(rc=rc)
    if (aqm_rc_check(rc)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to initialize model configuration", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

    ! -- initialize internal clock
    ! -- get clock information
!   call ESMF_ClockGet(clock, startTime=startTime, timeStep=timeStep, rc=localrc)
!   if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
!     line=__LINE__,  &
!     file=__FILE__,  &
!     rcToReturn=rc)) &
!     return  ! bail out
!   ! -- get forecast initial time
!   call ESMF_TimeGet(startTime, yy=yy, mm=mm, dd=dd, h=h, m=m, rc=localrc)
!   if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
!     line=__LINE__,  &
!     file=__FILE__,  &
!     rcToReturn=rc)) &
!     return  ! bail out
    ! -- get time step
!   call ESMF_TimeIntervalGet(timeStep, s_r8=dts, rc=localrc)
!   if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
!     line=__LINE__,  &
!     file=__FILE__,  &
!     rcToReturn=rc)) &
!     return  ! bail out
!   ! -- set internal clock
!   call aqm_model_clock_create(rc=rc)
!   if (aqm_rc_check(rc)) then
!     call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
!       msg="Failed to create model clock", &
!       line=__LINE__, &
!       file=__FILE__, &
!       rcToReturn=rc)
!     return  ! bail out
!   end if
!   call aqm_model_clock_set(yy=yy, mm=mm, dd=dd, h=h, m=m, dts=dts, tz=0, rc=rc)
!   if (aqm_rc_check(rc)) then
!     call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
!       msg="Failed to initialize model clock", &
!       line=__LINE__, &
!       file=__FILE__, &
!       rcToReturn=rc)
!     return  ! bail out
!   end if

!   ! -- allocate memory for internal workspace
!   call aqm_backgd_init(rc=rc)
!   if (aqm_rc_check(rc)) then
!     call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
!       msg="Failed to initialize I/O model subsystem", &
!       line=__LINE__, &
!       file=__FILE__, &
!       rcToReturn=rc)
!     return  ! bail out
!   end if

!   ! -- read-in emission and background fields
!   call aqm_backgd_read(rc=rc)
!   if (aqm_rc_check(rc)) then
!     call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
!       msg="Failed to initialize I/O model subsystem", &
!       line=__LINE__, &
!       file=__FILE__, &
!       rcToReturn=rc)
!     return  ! bail out
!   end if

!if 0
!   ! -- diagnostics: write out emission and background fields
!   call aqm_backgd_write(rc=rc)
!   if (aqm_rc_check(rc)) then
!     call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
!       msg="Failed to initialize I/O model subsystem", &
!       line=__LINE__, &
!       file=__FILE__, &
!       rcToReturn=rc)
!     return  ! bail out
!   end if
!endif

    ! -- create & initialize model component (infrastructure)
    call aqm_comp_create(model, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- connect import fields to model
    ! -- this can be done only once since remote fields are accessed by reference
    call aqm_comp_connect('import', importState, importFieldNames, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- connect export fields to model
    ! -- this can be done only once since remote fields are accessed by reference
    call aqm_comp_connect('export', exportState, exportFieldNames, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- initialize internal component (CMAQ)
    call aqm_comp_init(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!   ! -- allocate arrays for tracer output
!   call aqm_output_init(rc=rc)
!   if (aqm_rc_check(rc)) then
!     call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
!       msg="Failed to allocate output arrays", &
!       line=__LINE__, &
!       file=__FILE__, &
!       rcToReturn=rc)
!     return  ! bail out
!   end if

    ! indicate that data initialization is complete (breaking out of init-loop)
    call NUOPC_CompAttributeSet(model, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState
    type(ESMF_Time)               :: currTime
    type(ESMF_TimeInterval)       :: timeStep
    type(ESMF_Field)              :: field
    type(ESMF_VM)                 :: vm
    integer                       :: item

    ! begin
    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in 
    ! multiple calls to the ModelAdvance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing AQM from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_TimePrint(currTime + timeStep, &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! print field diagnostics
    if (btest(verbosity,0)) then
      call ESMF_GridCompGet(model, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      do item = 1, importFieldCount
        call ESMF_StateGet(importState, field=field, &
          itemName=trim(importFieldNames(item)), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        call fieldPrintMinMax(field, vm=vm, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      end do
    end if

    ! advance model
    call aqm_comp_advance(clock, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! write output if it is time
    call aqm_output_write(rc)
    if (aqm_rc_check(rc)) then
      call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, &
        msg="Failed to write air quality output", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    end if

  end subroutine ModelAdvance

  !-----------------------------------------------------------------------------

  subroutine CheckImport(model, rc)
    type(ESMF_GridComp)   :: model
    integer, intent(out)  :: rc
    
    ! Enforce a time dependency on the imported fields to be a coupling
    ! timeStep ahead of the current time. This means that the air quality
    ! component is assumed to run sequentially after the ATM during the
    ! same timestep. An incorrect run sequence will be flagged here as an
    ! incompatibility.
    
    ! local variables
    type(ESMF_Clock)        :: clock
    type(ESMF_Time)         :: time
    type(ESMF_State)        :: importState
    logical                 :: allCurrent

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock and importState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get the current time out of the clock
    call ESMF_ClockGet(clock, stopTime=time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! check that Fields in the importState show correct timestamp
    allCurrent = NUOPC_IsAtTime(importState, time, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    if (.not.allCurrent) then
      !TODO: introduce and use INCOMPATIBILITY return codes!!!!
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="NUOPC INCOMPATIBILITY DETECTED: Import Fields not at the "// &
        "expected time", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return  ! bail out
    endif
    
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelFinalize(model, rc)
    type(ESMF_GridComp)   :: model
    integer, intent(out)  :: rc

    ! begin
    rc = ESMF_SUCCESS

    ! finalize model
    call aqm_comp_finalize(rc)

  end subroutine ModelFinalize

  !-----------------------------------------------------------------------------

end module AQM
