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
  integer, parameter :: importFieldCount = 35
  character(len=*), dimension(importFieldCount), parameter :: &
    importFieldNames = (/ &
      "canopy_moisture_storage                  ", &
      "height                                   ", &
      "inst_aerodynamic_conductance             ", &
      "inst_canopy_resistance                   ", &
      "inst_cloud_frac_levels                   ", &
      "inst_convective_rainfall_amount          ", &
      "inst_friction_velocity                   ", &
      "inst_geop_interface                      ", &
      "inst_geop_levels                         ", &
      "inst_land_sea_mask                       ", &
      "inst_laten_heat_flx                      ", &
      "inst_merid_wind_levels                   ", &
      "inst_net_sw_flx                          ", &
      "inst_pbl_height                          ", &
      "inst_pres_height_surface                 ", &
      "inst_pres_levels                         ", &
      "inst_rainfall_amount                     ", &
      "inst_sensi_heat_flx                      ", &
      "inst_soil_moisture_content               ", &
      "inst_surface_roughness                   ", &
      "inst_spec_humid_height2m                 ", &
      "inst_temp_height2m                       ", &
      "inst_temp_height_surface                 ", &
      "inst_temp_levels                         ", &
      "inst_tracer_mass_frac                    ", &
      "inst_zonal_wind_height10m                ", &
      "inst_vegetation_area_frac                ", &
      "inst_merid_wind_height10m                ", &
      "inst_zonal_wind_levels                   ", &
      "leaf_area_index                          ", &
      "sea_ice_area_fraction                    ", &
      "soil_type                                ", &
      "surface_cell_area                        ", &
      "surface_snow_area_fraction               ", &
      "temperature_of_soil_layer                "  &
    /)
  ! -- export fields
  integer, parameter :: exportFieldCount = 2
  character(len=*), dimension(exportFieldCount), parameter :: &
    exportFieldNames = (/ &
      "inst_tracer_mass_frac                ", &
      "inst_tracer_diag_aod                 "  &
    /)

  private

  real(ESMF_KIND_R8), parameter :: pi = 3.1415926535897931
  real(ESMF_KIND_R8), parameter :: rad2deg = 180./pi
  
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
    integer                    :: verbosity
    character(len=ESMF_MAXSTR) :: msgString, name, rcFile
    type(ESMF_Config)          :: config

    ! begin
    rc = ESMF_SUCCESS

    ! get component's information
    call NUOPC_CompGet(model, name=name, verbosity=verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! write to log files
    msgString = ""
    write(msgString, '("Verbosity = ",i0)') verbosity
    call ESMF_LogWrite(trim(name)//": "//trim(msgString), &
      ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! get name of config file
    call ESMF_AttributeGet(model, name="ResourceFile", value=rcFile, &
      defaultValue="aqm.rc", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__)) &
      return  ! bail out
    if (btest(verbosity,8)) then
      call ESMF_LogWrite(trim(name)//": ResourceFile = "//trim(rcFile), &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__)) &
        return  ! bail out
    end if

    ! load component's configuration
    config = ESMF_ConfigCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ConfigLoadFile(config, rcFile, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    ! store Config object into gridded component
    call ESMF_GridCompSet(model, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
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
    integer                    :: verbosity
    real(ESMF_KIND_R8)         :: dts
    type(ESMF_Time)            :: startTime
    type(ESMF_TimeInterval)    :: TimeStep
    type(ESMF_CoordSys_Flag)   :: aqmGridCoordSys
    character(len=ESMF_MAXSTR) :: msgString, name
!test:
    integer :: tlb(2), tub(2)


    ! begin
    rc = ESMF_SUCCESS

    ! get component's information
    call NUOPC_CompGet(model, name=name, verbosity=verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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

    ! -- get coordinates from Grid object
    ! -- assume all fields on same grid and use first field
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

      call ESMF_VMGet(vm, localPet=mylocalPet, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      do localDe = 0, localDeCount-1
        de   = localDeToDeMap(localDe+1) + 1
        tile = deToTileMap(de)

        if (btest(verbosity,8)) then
          write(msgString,'("DE[",i0,"]: domain : [",i0,",",i0,"] x [",i0,",",i0,' // &
            '"], compute: [",i0,",",i0,"] x [",i0,",",i0,"]")') localDe, &
            minIndexPDe(:,de), maxIndexPDe(:,de), &
            computationalLBound(:,localDe+1), computationalUBound(:,localDe+1)
          call ESMF_LogWrite(trim(name)//": decomp: local : "//trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          write(msgString,'("DE[",i0,"]: tile[",i0,"]: [",i0,",",i0,"] x [",i0,",",i0,"]")') &
            de-1, tile, minIndexPTile(:,tile), maxIndexPTile(:,tile)
          call ESMF_LogWrite(trim(name)//": decomp: global: "//trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
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
        call ESMF_GridGet(grid, coordSys=aqmGridCoordSys, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
         
        do item = 1, 2
          call ESMF_GridGetCoord(grid, coordDim=item, staggerloc=ESMF_STAGGERLOC_CENTER, &
            totalLBound=tlb, totalUBound=tub, &
            localDE=localDe, farrayPtr=coord, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (aqmGridCoordSys == ESMF_COORDSYS_SPH_RAD) then
            coord = coord * rad2deg
          endif

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

      call ESMF_GridCompSet(model, grid=grid, rc=localrc)
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

    ! -- indicate that data initialization is complete (breaking out of init-loop)
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
    integer                       :: diagnostic
    character(len=ESMF_MAXSTR)    :: name

    ! begin
    rc = ESMF_SUCCESS
    
    ! get component's information
    call NUOPC_CompGet(model, name=name, diagnostic=diagnostic, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

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
      preString="---------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! print field diagnostics
    if (btest(diagnostic,17)) then
      call aqm_field_diagnostics(model, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
    end if

    ! advance model
    call aqm_comp_advance(model, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine ModelAdvance

  !-----------------------------------------------------------------------------

  subroutine ModelFinalize(model, rc)
    type(ESMF_GridComp)   :: model
    integer, intent(out)  :: rc

    ! begin
    rc = ESMF_SUCCESS

    ! finalize model
    call aqm_comp_finalize(model, rc)

  end subroutine ModelFinalize

  !-----------------------------------------------------------------------------

end module AQM
