module cmaq_model_mod

  use aqm_rc_mod
  use aqm_types_mod
  use aqm_comm_mod
  use aqm_model_mod
  use aqm_tracers_mod
  use cmaq_mod

  use cmaq_shr_mod

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
    integer :: nspecies
    integer :: is, ie, js, je, ni, nl
    type(aqm_config_type), pointer :: config => null()
    type(aqm_data_type),   pointer :: data   => null()

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- get model config
    call aqm_model_get(deCount=deCount, config=config, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (deCount < 1) return

    ! -- initialize species pointers for CMAQ internals
!   call aqm_tracers_set(config, rc=localrc)
!   if (aqm_rc_check(localrc, msg="Failed to set tracer pointers", &
!     file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- print out model configuration
    if (aqm_comm_isroot()) then
      write(6,'(28("-"))')
      write(6,'("CMAQ configuration:")')
      write(6,'(28("-"))')
      write(6,'("    aqm_opt         = ",i0)') config % aqm_opt
      write(6,'("    aqm_in_opt      = ",i0)') config % aqm_in_opt
      write(6,'("    dust_opt         = ",i0)') config % dust_opt
      write(6,'("    dmsemis_opt      = ",i0)') config % dmsemis_opt
      write(6,'("    seas_opt         = ",i0)') config % seas_opt
      write(6,'("    biomass_burn_opt = ",i0)') config % biomass_burn_opt
      write(6,'(28("-"))')
    end if

    ! -- initialize CMAQ
    !  * initialize species from namelists on DE 0
    call cmaq_species_read(nspecies, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to initialize CMAQ species", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    do de = 0, deCount-1

      call aqm_model_get(de=de, config=config, data=data, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failed to retrieve model on local DE", &
        file=__FILE__, line=__LINE__, rc=rc)) return

      call aqm_model_domain_get(de=de, ids=is, ide=ie, jds=js, jde=je, ni=ni, nl=nl, &
        rc=localrc)
      if (aqm_rc_check(localrc, msg="Failed to retrieve model domain on local DE", &
        file=__FILE__, line=__LINE__, rc=rc)) return

      ! -- allocate CMAQ internal workspace
      if (.not.allocated(data % cgrid)) then
        allocate(data % cgrid(ie-is+1,je-js+1,ni,nspecies), stat=localrc)
        if (aqm_rc_test((localrc /= 0), &
          msg="Failed to allocate CMAQ workspace on local DE", &
          file=__FILE__, line=__LINE__, rc=rc)) return
        data % cgrid = 0._AQM_KIND_R4
      end if

      ! -- initialize CMAQ's internal workspace
      call cmaq_init(data % cgrid, config % run_aero, rc=localrc)

      ! -- set CMAQ internal clock?

    end do

  end subroutine cmaq_model_init


  subroutine cmaq_model_advance(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount
    integer :: advanceCount, julday, mm, tz
    integer :: jdate, jtime, tstep(3)
    integer :: is, ie, js, je, ni, nl
    real(AQM_KIND_R8) :: dts
    real(AQM_KIND_R8), dimension(:,:), pointer :: lat, lon
    type(aqm_config_type), pointer :: config => null()
    type(aqm_data_type),   pointer :: data   => null()

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(deCount=deCount, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (deCount < 1) return

    call aqm_model_clock_get(advanceCount=advanceCount, dts=dts, mm=mm, tz=tz, julday=julday, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model clock on local DE", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- CMAQ time steps start from 1, while model time steps start from 0
    advanceCount = advanceCount + 1

    do de = 0, deCount-1
      call aqm_model_get(de=de, data=data, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failed to retrieve model on local DE", &
        file=__FILE__, line=__LINE__, rc=rc)) return

      call aqm_model_domain_get(de=de, ids=is, ide=ie, jds=js, jde=je, ni=ni, nl=nl, &
        lon=lon, lat=lat, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failed to retrieve model domain on local DE", &
        file=__FILE__, line=__LINE__, rc=rc)) return

      ! -- set array dimensions on this DE
      CMAQ_NROWS = ie-is+1
      CMAQ_NCOLS = je-js+1
      CMAQ_NLAYS = ni

      ! -- set pointers to meteorological data structures
      call cmaq_model_setpointers(data=data, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failed to set pointers for CMAQ on local DE", &
        file=__FILE__, line=__LINE__, rc=rc)) return

      jdate = 0
      jtime = 0
      call cmaq_advance(data % cgrid, jdate, jtime, tstep, config % run_aero, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failed to advance CMAQ on local DE", &
        file=__FILE__, line=__LINE__, rc=rc)) return

    end do

  end subroutine cmaq_model_advance


  subroutine cmaq_model_setpointers(config, data, rc)
    type(aqm_config_type), optional, pointer :: config
    type(aqm_data_type),   optional, pointer :: data
    integer,               optional, intent(out) :: rc

    ! -- local variables

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    if (present(data)) then
      ! -- vdiff
      vdiffp => data % vdiff
      ! -- MET
      CMAQ_Met_Data  => data % Met_Data
      CMAQ_Grid_Data => data % Grid_Data
    end if

  end subroutine cmaq_model_setpointers

end module cmaq_model_mod
