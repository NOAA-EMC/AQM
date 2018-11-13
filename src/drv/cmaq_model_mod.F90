module cmaq_model_mod

  use aqm_rc_mod
  use aqm_types_mod
  use aqm_comm_mod
  use aqm_model_mod
  use aqm_tracers_mod
  use cmaq_mod

  implicit none

  private

  public :: cmaq_model_init
  public :: cmaq_model_advance

contains

  ! -- public methods

  subroutine cmaq_model_init(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount
    integer :: numSpecies
    integer :: is, ie, js, je, ni, nl
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

    call aqm_model_set(config=config, numTracers=numSpecies, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to set number of model species on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- set domain size
    call aqm_model_domain_get(ids=is, ide=ie, jds=js, jde=je, nl=nl, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model domain on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    NCOLS = ie - is + 1
    NROWS = je - js + 1
    NLAYS = nl

    ! -- initialize CMAQ's internal workspace
    call cmaq_init(rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to initialize CMAQ", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- print out model configuration
    if (aqm_comm_isroot()) then
      write(cmaq_logdev,'(28("-"))')
      write(cmaq_logdev,'("CMAQ configuration:")')
      write(cmaq_logdev,'(28("-"))')
      write(cmaq_logdev,'(2x,"ae_matrix_nml: ",a)') trim(config % ae_matrix_nml)
      write(cmaq_logdev,'(2x,"gc_matrix_nml: ",a)') trim(config % gc_matrix_nml)
      write(cmaq_logdev,'(2x,"nr_matrix_nml: ",a)') trim(config % nr_matrix_nml)
      write(cmaq_logdev,'(2x,"tr_matrix_nml: ",a)') trim(config % tr_matrix_nml)
      write(cmaq_logdev,'(2x,"csqy_data    : ",a)') trim(config % csqy_data)
      write(cmaq_logdev,'(2x,"optics_data  : ",a)') trim(config % optics_data)
      write(cmaq_logdev,'(2x,"omi          : ",a)') trim(config % omi)
      write(cmaq_logdev,'(2x,"ctm_depvfile : ",l7)') config % ctm_depvfile
      write(cmaq_logdev,'(2x,"ctm_photodiag: ",l7)') config % ctm_photodiag
      write(cmaq_logdev,'(2x,"ctm_pmdiag   : ",l7)') config % ctm_pmdiag
      write(cmaq_logdev,'(2x,"run_aero     : ",l7)') config % run_aero
      write(cmaq_logdev,'(2x,"N. species   : ",i0)') numSpecies
      write(cmaq_logdev,'(28("-"))')
    end if

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

    integer, save :: advanceCount = 0

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(deCount=deCount, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (deCount < 1) return

    print *,'cmaq_model_advance: count = ',advanceCount

    ! -- CMAQ time steps start from 1, while model time steps start from 0
    advanceCount = advanceCount + 1

    ! -- run CMAQ
    ! -- NOTE: CMAQ can only run on 1DE/PET domain decomposition (DE 0)
    call aqm_model_get(config=config, &
      stateIn=stateIn, stateOut=stateOut, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- import advected species mixing ratios
    if (advanceCount > 1) &
      call cmaq_import(stateIn % tr, config % spcs_start_index)

    ! -- advance model
    call cmaq_advance(jdate, jtime, tstep, config % run_aero, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to advance CMAQ on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- export updated species mixing ratios
    call cmaq_export(stateOut % tr, config % spcs_start_index)

  end subroutine cmaq_model_advance

end module cmaq_model_mod
