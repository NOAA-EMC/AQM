module cmaq_model_mod

  use aqm_rc_mod
  use aqm_types_mod
  use aqm_model_mod
  use cmaq_mod

  implicit none

  private

  public :: cmaq_model_init
  public :: cmaq_model_advance
  public :: cmaq_model_finalize

contains

  ! -- public methods

  subroutine cmaq_model_init(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount
    integer :: numSpecies
    integer :: is, ie, js, je, ni, nl, nt
    type(aqm_config_type), pointer :: config => null()

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- get model config
    call aqm_model_get(deCount=deCount, config=config, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (deCount < 1) return

    ! -- initialize CMAQ
    ! -- NOTE: CMAQ can only run on 1DE/PET domain decomposition (DE 0)

    ! -- initialize species from namelists on DE 0
    call cmaq_species_read(numSpecies, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to initialize CMAQ species", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- verify that coupling tracer arrays are large enough to include
    ! -- all required species
    config % species % p_diag_beg = config % species % p_aqm_beg + numSpecies
    numSpecies = numSpecies + config % species % ndiag
    call aqm_model_domain_get(nt=nt, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model domain on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return
    if (aqm_rc_test((config % species % p_aqm_beg + numSpecies - 1 > nt), &
      msg="Coupling tracer fields cannot hold all the required species", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    call aqm_model_set(config=config, numTracers=numSpecies, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to set number of model species on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- set domain size
    call aqm_model_domain_get(ids=is, ide=ie, jds=js, jde=je, nl=nl, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model domain on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- initialize CMAQ's internal workspace
    call cmaq_init(rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to initialize CMAQ", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (config % verbose) call cmaq_domain_log(config % name)

    ! -- initialize emissions
    call cmaq_emis_init(rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to initialize CMAQ", &
      file=__FILE__, line=__LINE__, rc=rc)) return

  end subroutine cmaq_model_init


  subroutine cmaq_model_advance(jdate, jtime, tstep, rc)
    integer,           intent(in)  :: jdate, jtime, tstep(3)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount
    type(aqm_config_type), pointer :: config   => null()
    type(aqm_state_type),  pointer :: stateIn  => null()
    type(aqm_state_type),  pointer :: stateOut => null()

    logical, save :: first_step = .true.

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(deCount=deCount, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (deCount < 1) return

    ! -- run CMAQ
    ! -- NOTE: CMAQ can only run on 1DE/PET domain decomposition (DE 0)
    call aqm_model_get(config=config, &
      stateIn=stateIn, stateOut=stateOut, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- import advected species mixing ratios
    if (config % init_conc .and. first_step) then
      call cmaq_conc_init(jdate, jtime, tstep, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failed to initialize concentrations", &
        file=__FILE__, line=__LINE__, rc=rc)) return
      first_step = .false.
      if (config % verbose) call cmaq_conc_log(trim(config % name) // ": init")
    else
      call cmaq_import(stateIn % tr, stateIn % prl, stateIn % phii, stateIn % temp, config % species % p_aqm_beg)
      if (config % verbose) call cmaq_conc_log(trim(config % name) // ": import")
    end if

    ! -- advance model
    call cmaq_advance(jdate, jtime, tstep, config % run_aero, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to advance CMAQ on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- export updated species mixing ratios
    call cmaq_export(stateOut % tr, stateIn % prl, stateIn % temp, config % species % p_aqm_beg)
    if (config % verbose) call cmaq_conc_log(trim(config % name) // ": export")

  end subroutine cmaq_model_advance


  subroutine cmaq_model_finalize(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- finalize fire emissions
    call cmaq_emis_finalize(rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to finalize CMAQ emissions", &
      file=__FILE__, line=__LINE__, rc=rc)) return

  end subroutine cmaq_model_finalize

end module cmaq_model_mod
