module aqm_model_mod

  use aqm_rc_mod
  use aqm_types_mod
  use aqm_config_mod
  use aqm_domain_mod,   only : aqm_domain_type
  use aqm_state_mod,    only : aqm_state_type

  implicit none

  type aqm_model_type
    type(aqm_config_type),  pointer :: config  => null()
    type(aqm_domain_type)   :: domain
    type(aqm_state_type)    :: stateIn, stateOut
  end type aqm_model_type

  type(aqm_model_type), dimension(:), allocatable, target :: aqm_model

  private

  public :: aqm_model_type
  ! -- also provide subtypes
  public :: aqm_config_type
  public :: aqm_domain_type
  public :: aqm_state_type

  public :: aqm_model_create
  public :: aqm_model_destroy
  public :: aqm_model_get
  public :: aqm_model_set
  public :: aqm_model_config_create
  public :: aqm_model_domain_get
  public :: aqm_model_domain_set
  public :: aqm_model_domain_coord_set

contains

  subroutine aqm_model_create(deCount, rc)

    integer, intent(in),  optional :: deCount
    integer, intent(out), optional :: rc

    !-- local variables
    integer :: localDeCount, localrc

    !-- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    localDeCount = 1
    if (present(deCount)) localDeCount = deCount

    ! -- AQM can only support up to 1 DE/PET
    if (localDeCount > 1) then
      call aqm_rc_set(AQM_RC_FAILURE, &
        msg="AQM only supports up to 1 DE/PET.", &
        file=__FILE__, line=__LINE__, rc=rc)
      return
    end if

    if (localDeCount > 0) then
      allocate(aqm_model(0:localDeCount-1), stat=localrc)
      if (aqm_rc_test((localrc /= 0), msg="Failure to create model", &
          file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    
  end subroutine aqm_model_create


  subroutine aqm_model_destroy(rc)

    integer, intent(out), optional :: rc

    !-- local variables
    integer :: localrc
    integer :: de

    !-- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    if (allocated(aqm_model)) then
      do de = 1, size(aqm_model)-1
        nullify(aqm_model(de) % config)
      end do
      de = 0
      if (associated(aqm_model(de) % config)) then
        if (associated(aqm_model(de) % config % species)) then
          deallocate(aqm_model(de) % config % species, stat=localrc)
          if (aqm_rc_test((localrc /= 0), &
            msg="Failure to deallocate model species memory", &
            file=__FILE__, line=__LINE__, rc=rc)) return
          nullify(aqm_model(de) % config % species)
        end if
        deallocate(aqm_model(de) % config, stat=localrc)
        if (aqm_rc_test((localrc /= 0), &
          msg="Failure to deallocate model config memory", &
          file=__FILE__, line=__LINE__, rc=rc)) return
        nullify(aqm_model(de) % config)
        if (associated(aqm_model(de) % domain % lon)) then
          deallocate(aqm_model(de) % domain % lon, stat=localrc)
          if (aqm_rc_test((localrc /= 0), &
            msg="Failure to deallocate model domain longitudes memory", &
            file=__FILE__, line=__LINE__, rc=rc)) return
          nullify(aqm_model(de) % domain % lon)
        end if
        if (associated(aqm_model(de) % domain % lat)) then
          deallocate(aqm_model(de) % domain % lat, stat=localrc)
          if (aqm_rc_test((localrc /= 0), &
            msg="Failure to deallocate model domain latitudes memory", &
            file=__FILE__, line=__LINE__, rc=rc)) return
          nullify(aqm_model(de) % domain % lat)
        end if
      end if
      deallocate(aqm_model, stat=localrc)
      if (aqm_rc_test((localrc /= 0), msg="Failure to allocate model memory", &
          file=__FILE__, line=__LINE__, rc=rc)) return
    end if

  end subroutine aqm_model_destroy


  subroutine aqm_model_config_create(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount
    type(aqm_model_type), pointer :: model

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- NOTE: config is allocated only on DE 0 on each PET
    ! --       config points to DE 0 on other DEs on the same PET
    deCount = 0
    call aqm_model_local_get(de=0, deCount=deCount, model=model, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (deCount > 0) then
      if (.not.associated(model % config)) then
        allocate(model % config, stat=localrc)
        if (aqm_rc_test((localrc /= 0), msg="Failure to allocate model configuration", &
          file=__FILE__, line=__LINE__, rc=rc)) return
      end if
      ! -- populate pointers on other local DEs
      do de = 1, deCount-1
        call aqm_model_set(de=de, config=model % config, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
      end do
    end if

  end subroutine aqm_model_config_create


  subroutine aqm_model_domain_set(minIndexPDe, maxIndexPDe, minIndexPTile, maxIndexPTile, &
    minIndexLocal, maxIndexLocal, tile, tileCount, de, rc)

    integer, dimension(2), optional, intent(in)  :: minIndexPDe,   maxIndexPDe
    integer, dimension(2), optional, intent(in)  :: minIndexPTile, maxIndexPTile
    integer, dimension(2), optional, intent(in)  :: minIndexLocal, maxIndexLocal
    integer,               optional, intent(in)  :: tile, tileCount
    integer,               optional, intent(in)  :: de
    integer,               optional, intent(out) :: rc

    !-- local variables
    integer                        :: deCount, localrc
    type(aqm_model_type), pointer :: model

    !-- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_local_get(de=de, deCount=deCount, model=model, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model", &
        file=__FILE__, line=__LINE__, rc=rc)) return

    if (deCount < 1) return

    model % domain % tile = 0
    if (present(tile)) model % domain % tile = tile

    model % domain % tileCount = 0
    if (present(tileCount)) model % domain % tileCount = tileCount

    if (present(minIndexPDe)) then
      model % domain % ids = minIndexPDe(1)
      model % domain % jds = minIndexPDe(2)
    end if
    if (present(maxIndexPDe)) then
      model % domain % ide = maxIndexPDe(1)
      model % domain % jde = maxIndexPDe(2)
    end if
    if (present(minIndexPTile)) then
      model % domain % its = minIndexPTile(1)
      model % domain % jts = minIndexPTile(2)
    end if
    if (present(maxIndexPTile)) then
      model % domain % ite = maxIndexPTile(1)
      model % domain % jte = maxIndexPTile(2)
    end if
    if (present(minIndexLocal)) then
      model % domain % ims = minIndexLocal(1)
      model % domain % jms = minIndexLocal(2)
    end if
    if (present(maxIndexLocal)) then
      model % domain % ime = maxIndexLocal(1)
      model % domain % jme = maxIndexLocal(2)
    end if

  end subroutine aqm_model_domain_set


  subroutine aqm_model_domain_coord_set(coordDim, coord, scale, de, rc)

    integer,                     intent(in)  :: coordDim
    real(AQM_KIND_R8),           intent(in)  :: coord(:,:)
    real(AQM_KIND_R8), optional, intent(in)  :: scale
    integer,           optional, intent(in)  :: de
    integer,           optional, intent(out) :: rc

    !-- local variables
    integer                       :: deCount, localrc
    real(AQM_KIND_R8)             :: fscale
    type(aqm_model_type), pointer :: model

    !-- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_local_get(de=de, deCount=deCount, model=model, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (deCount < 1) return

    fscale = 1._AQM_KIND_R8
    if (present(scale)) fscale = scale

    select case (coordDim)
      case(1)
        allocate(model % domain % lon, source=fscale * coord, stat=localrc)
        if (aqm_rc_test((localrc /= 0), &
          msg="Failure to allocate model config memory", &
          file=__FILE__, line=__LINE__, rc=rc)) return
      case(2)
        allocate(model % domain % lat, source=fscale * coord, stat=localrc)
        if (aqm_rc_test((localrc /= 0), &
          msg="Failure to allocate model config memory", &
          file=__FILE__, line=__LINE__, rc=rc)) return
      case default
        call aqm_rc_set(AQM_RC_FAILURE, &
          msg="coordDim can only be 1 or 2.", &
          file=__FILE__, line=__LINE__, rc=rc)
        return
    end select

  end subroutine aqm_model_domain_coord_set


  subroutine aqm_model_domain_get(de, &
   ids, ide, jds, jde,  &
   its, ite, jts, jte,  &
   ims, ime, jms, jme,  &
   ni,  nl,  nt,  tile, &
   lon, lat, rc)
    integer, optional, intent(in)  :: de
    integer, optional, intent(out) :: ids, ide, jds, jde
    integer, optional, intent(out) :: its, ite, jts, jte
    integer, optional, intent(out) :: ims, ime, jms, jme
    integer, optional, intent(out) :: ni, nl, nt
    integer, optional, intent(out) :: tile
    real(AQM_KIND_R8), optional, pointer :: lon(:,:)
    real(AQM_KIND_R8), optional, pointer :: lat(:,:)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                        :: localrc
    type(aqm_model_type), pointer :: model

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_local_get(de=de, model=model, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (present(ids)) ids = model % domain % ids
    if (present(ide)) ide = model % domain % ide
    if (present(jds)) jds = model % domain % jds
    if (present(jde)) jde = model % domain % jde
    if (present(its)) its = model % domain % its
    if (present(ite)) ite = model % domain % ite
    if (present(jts)) jts = model % domain % jts
    if (present(jte)) jte = model % domain % jte
    if (present(ims)) ims = model % domain % ims
    if (present(ime)) ime = model % domain % ime
    if (present(jms)) jms = model % domain % jms
    if (present(jme)) jme = model % domain % jme
    if (present(ni))  ni  = model % domain % ni
    if (present(nl))  nl  = model % domain % nl
    if (present(nt))  nt  = model % domain % nt
    if (present(tile)) tile = model % domain % tile
    if (present(lon)) lon => model % domain % lon
    if (present(lat)) lat => model % domain % lat

  end subroutine aqm_model_domain_get


  subroutine aqm_model_set(de, numIntLayers, numModLayers, numSoilLayers, numTracers, &
    config, rc)

    integer, optional, intent(in)  :: de
    integer, optional, intent(in)  :: numIntLayers
    integer, optional, intent(in)  :: numModLayers
    integer, optional, intent(in)  :: numSoilLayers
    integer, optional, intent(in)  :: numTracers
    type(aqm_config_type), optional, pointer :: config
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                        :: deCount, localrc
    type(aqm_model_type), pointer :: model
    
    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_local_get(de=de, deCount=deCount, model=model, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (deCount > 0) then
      if (present(numIntLayers)) model % domain % ni = numIntLayers
      if (present(numModLayers)) model % domain % nl = numModLayers
      if (present(numSoilLayers)) model % domain % ns = numSoilLayers
      if (present(numTracers))   model % domain % nt = numTracers
      if (present(config))       model % config => config
    end if

  end subroutine aqm_model_set


  subroutine aqm_model_get(de, deCount, stateIn, stateOut, config, &
    domain, tile, tileCount, rc)

    integer,               optional,  intent(in)  :: de
    integer,               optional,  intent(out) :: deCount
    type(aqm_state_type),  optional,  pointer     :: stateIn, stateOut
    type(aqm_config_type), optional,  pointer     :: config
    type(aqm_domain_type), optional,  pointer     :: domain
    integer,               optional,  intent(out) :: tile
    integer,               optional,  intent(out) :: tileCount
    integer,               optional,  intent(out) :: rc

    !-- local variables
    integer                        :: localrc, localDeCount
    type(aqm_model_type), pointer :: model
    
    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    localDeCount = 0
    call aqm_model_local_get(de=de, deCount=localDeCount, model=model, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (present(deCount)) deCount = localDeCount

    if (localDeCount > 0) then
      if (present(stateIn))     stateIn     => model % stateIn
      if (present(stateOut))    stateOut    => model % stateOut
      if (present(config))      config      => model % config
      if (present(domain))      domain      => model % domain
      if (present(tile))        tile        =  model % domain % tile
      if (present(tileCount))   tileCount   =  model % domain % tileCount
    end if

  end subroutine aqm_model_get


  subroutine aqm_model_local_get(de, deCount, model, rc)

    integer,               optional,  intent(in)  :: de
    integer,               optional,  intent(out) :: deCount
    type(aqm_model_type), optional,  pointer     :: model
    integer,               optional,  intent(out) :: rc

    !-- local variables
    integer :: localDe

    !-- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    if (present(model)) nullify(model)

    localDe = 0
    if (present(de)) localDe = de

    if (aqm_rc_test((localDe < 0), &
      msg="DE must be >= 0", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (allocated(aqm_model)) then
      if (present(deCount)) deCount = size(aqm_model)
      if (present(model)) then
        if (localDe <= ubound(aqm_model, dim=1)) then
          model => aqm_model(localDe)
        else
          call aqm_rc_set(AQM_RC_FAILURE, msg="Model undefined on local DE", &
            file=__FILE__, line=__LINE__, rc=rc)
          return
        end if
      end if
    else
      if (present(deCount)) deCount = 0
    end if

  end subroutine aqm_model_local_get


end module aqm_model_mod
