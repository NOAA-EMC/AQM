module aqm_iodata_mod

  use aqm_rc_mod
  use aqm_types_mod
  use aqm_io_mod
  use aqm_comm_mod
  use aqm_model_mod
  use aqm_config_mod

  implicit none

  private

  public :: aqm_backgd_init
  public :: aqm_backgd_read
  public :: aqm_backgd_write
  public :: aqm_output_init
  public :: aqm_output_write

contains

  subroutine aqm_backgd_init(rc)
    integer, optional, intent(out) :: rc
    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS
  end subroutine aqm_backgd_init

  subroutine aqm_backgd_read(rc)
    integer, optional, intent(out) :: rc
    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS
  end subroutine aqm_backgd_read

  subroutine aqm_backgd_write(rc)
    integer, optional, intent(out) :: rc
    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS
  end subroutine aqm_backgd_write

  subroutine aqm_output_init(rc)
    integer, optional, intent(out) :: rc
    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS
  end subroutine aqm_output_init

  subroutine aqm_output_write(rc)
    integer, optional, intent(out) :: rc
    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS
  end subroutine aqm_output_write


#if 0
  subroutine aqm_backgd_init(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount
    integer :: ids, ide, jds, jde
    type(aqm_data_type),   pointer :: data   => null()
    type(aqm_config_type), pointer :: config => null()

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(deCount=deCount, config=config, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    do de = 0, deCount-1
      call aqm_model_get(de=de, data=data, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
      call aqm_model_domain_get(de=de, ids=ids, ide=ide, jds=jds, jde=jde, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      ! -- pressure levels for GOCART
      if (.not.allocated(data % p_gocart)) then
        allocate(data % p_gocart(config % nvl_gocart+1), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % p_gocart = (/ 1013.250, 998.1627, 968.4935, 914.7947, &
          841.1536, 752.8976, 655.9680, 556.8575, 472.6497, 401.1463, 340.4388, 288.9272, 245.2461,&
          208.2443, 176.9300, 150.3930, 127.8370, 108.6634, 92.36566, 78.51230, 66.60338, 56.38794,&
          47.64393, 40.17542, 33.80996, 28.36782, 23.73036, 19.79155, 16.45707, 13.64339, 11.27689,&
          9.292943, 7.619839, 6.216800, 5.046805, 4.076567, 3.276433, 2.620212, 2.084972, 1.650792,&
          1.300508, 1.019442, 0.7951340, 0.6167790, 0.4758060, 0.3650410, 0.2785260, 0.2113490,&
          0.1594950, 0.1197030, 8.9345001E-02, 6.6000000E-02, 4.7584999E-02, 3.2699998E-02,&
          2.0000000E-02,  9.9999998E-03 /)
      end if

      ! -- dust 
      if (.not.allocated(data % dm0)) then
        allocate(data % dm0(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % dm0 = 0._AQM_KIND_R4
      end if

      ! -- dust erosion factors
      if (.not.allocated(data % ero1)) then
        allocate(data % ero1(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % ero1 = 0._AQM_KIND_R4
      end if
      if (.not.allocated(data % ero2)) then
        allocate(data % ero2(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % ero2 = 0._AQM_KIND_R4
      end if
      if (.not.allocated(data % ero3)) then
        allocate(data % ero3(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % ero3 = 0._AQM_KIND_R4
      end if

      ! -- aqmical species background
      if (.not.allocated(data % h2o2_backgd)) then
        allocate(data % h2o2_backgd(ids:ide,jds:jde,config % nvl_gocart), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % h2o2_backgd = 0._AQM_KIND_R4
      end if
      if (.not.allocated(data % no3_backgd)) then
        allocate(data % no3_backgd(ids:ide,jds:jde,config % nvl_gocart), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % no3_backgd = 0._AQM_KIND_R4
      end if
      if (.not.allocated(data % oh_backgd)) then
        allocate(data % oh_backgd(ids:ide,jds:jde,config % nvl_gocart), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % oh_backgd = 0._AQM_KIND_R4
      end if

      ! -- emissions
      if (.not.allocated(data % emiss_ab)) then
        allocate(data % emiss_ab(ids:ide,jds:jde,config % num_emis_ant), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emiss_ab = 0._AQM_KIND_R4
      end if

      ! -- volcanic ash
      if (.not.allocated(data % emiss_ash_height)) then
        allocate(data % emiss_ash_height(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emiss_ash_height = 0._AQM_KIND_R4
      end if
      if (.not.allocated(data % emiss_ash_mass)) then
        allocate(data % emiss_ash_mass(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emiss_ash_mass = 0._AQM_KIND_R4
      end if
      if (.not.allocated(data % emiss_ash_dt)) then
        allocate(data % emiss_ash_dt(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emiss_ash_dt = 0._AQM_KIND_R4
      end if

      ! -- emission tr
      if (.not.allocated(data % emiss_tr_height)) then
        allocate(data % emiss_tr_height(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emiss_tr_height = 0._AQM_KIND_R4
      end if
      if (.not.allocated(data % emiss_tr_mass)) then
        allocate(data % emiss_tr_mass(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emiss_tr_mass = 0._AQM_KIND_R4
      end if
      if (.not.allocated(data % emiss_tr_dt)) then
        allocate(data % emiss_tr_dt(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emiss_tr_dt = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % th_pvsrf)) then
        allocate(data % th_pvsrf(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % th_pvsrf = 0._AQM_KIND_R4
      end if

      ! -- additional dust quantities for AFWA
      if (config % dust_opt == DUST_OPT_AFWA) then
        if (.not.allocated(data % clayfrac)) then
          allocate(data % clayfrac(ids:ide,jds:jde), stat=localrc)
          if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
          data % clayfrac = 0._AQM_KIND_R4
        end if
        if (.not.allocated(data % sandfrac)) then
          allocate(data % sandfrac(ids:ide,jds:jde), stat=localrc)
          if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
          data % sandfrac = 0._AQM_KIND_R4
        end if
      end if

      ! -- emission from burning biomass
      if (config % biomass_burn_opt > 0) then
        if (.not.allocated(data % emiss_abu)) then
          allocate(data % emiss_abu(ids:ide,jds:jde,config % num_ebu_in), stat=localrc)
          if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
          data % emiss_abu = 0._AQM_KIND_R4
        end if
        if (.not.allocated(data % plumestuff)) then
          allocate(data % plumestuff(ids:ide,jds:jde,config % num_plumestuff), stat=localrc)
          if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
          data % plumestuff = 0._AQM_KIND_R4
        end if
      end if

    end do

  end subroutine aqm_backgd_init

  subroutine aqm_backgd_read(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount, localpe, tile
    type(aqm_data_type),   pointer :: data => null()
    type(aqm_config_type), pointer :: config => null()

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(deCount=deCount, config=config, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    call aqm_comm_get(localpe=localpe, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    do de = 0, deCount-1
      call aqm_model_get(de=de, data=data, tile=tile, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      if ((config % aqm_opt == AQM_OPT_RACM_SOA_VBS) .or.  &
          (config % aqm_opt >= AQM_OPT_GOCART)       .and. &
          (config % aqm_opt < 500)) then

        ! -- dust erosion factors
        call aqm_io_read('dm0.dat', data % dm0, path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," dm0 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % dm0)

        ! -- dust erosion factors
        call aqm_io_read('erod1.dat', data % ero1, path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ero1 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % ero1)
        call aqm_io_read('erod2.dat', data % ero2, path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ero2 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % ero2)
        call aqm_io_read('erod3.dat', data % ero3, path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ero3 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % ero3)

        ! -- bacground values for aqmical species
        call aqm_io_read('h2o2.dat', data % h2o2_backgd, path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," h2o2 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % h2o2_backgd)
        call aqm_io_read('no3.dat', data % no3_backgd, path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," no3 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % no3_backgd)
        call aqm_io_read('oh.dat', data % oh_backgd, path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," oh - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % oh_backgd)

        ! -- emissions
        call aqm_io_read('e_bc.dat', data % emiss_ab(:,:,config % species % p_e_bc), &
          path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_bc - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_bc)), maxval(data % emiss_ab(:,:,config % species % p_e_bc))

        call aqm_io_read('e_oc.dat', data % emiss_ab(:,:,config % species % p_e_oc), &
          path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_oc - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_oc)), maxval(data % emiss_ab(:,:,config % species % p_e_oc))

        call aqm_io_read('e_pm_10.dat', data % emiss_ab(:,:,config % species % p_e_pm_10), &
          path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_pm_10 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_pm_10)), maxval(data % emiss_ab(:,:,config % species % p_e_pm_10))

        call aqm_io_read('e_pm_25.dat', data % emiss_ab(:,:,config % species % p_e_pm_25), &
          path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_pm_25 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_pm_25)), maxval(data % emiss_ab(:,:,config % species % p_e_pm_25))

        call aqm_io_read('e_so2.dat', data % emiss_ab(:,:,config % species % p_e_so2), &
          path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_so2 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_so2)), maxval(data % emiss_ab(:,:,config % species % p_e_so2))

        call aqm_io_read('e_sulf.dat', data % emiss_ab(:,:,config % species % p_e_sulf), &
          path=trim(config % emi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_sulf - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_sulf)), maxval(data % emiss_ab(:,:,config % species % p_e_sulf))
        
        if (config % dust_opt == DUST_OPT_AFWA) then
           ! -- DUST_OPT_AFWA
          call aqm_io_read('clay.dat', data % clayfrac, path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," clayfrac - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % dm0), maxval(data % clayfrac)
          call aqm_io_read('sand.dat', data % sandfrac, path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," sandfrac - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % dm0), maxval(data % sandfrac)
        end if

        if ((config % aqm_opt == AQM_OPT_GOCART_RACM) .or. &
            (config % aqm_opt == AQM_OPT_RACM_SOA_VBS)) then

          call aqm_io_read('e_ald.dat', data % emiss_ab(:,:,config % species % p_e_ald), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_ald - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_ald)), maxval(data % emiss_ab(:,:,config % species % p_e_ald))

          call aqm_io_read('e_co.dat', data % emiss_ab(:,:,config % species % p_e_co), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_co - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_co)), maxval(data % emiss_ab(:,:,config % species % p_e_co))

          call aqm_io_read('e_csl.dat', data % emiss_ab(:,:,config % species % p_e_csl), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_csl - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_csl)), maxval(data % emiss_ab(:,:,config % species % p_e_csl))

          call aqm_io_read('e_dms.dat', data % emiss_ab(:,:,config % species % p_e_dms), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_dms - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_dms)), maxval(data % emiss_ab(:,:,config % species % p_e_dms))

          call aqm_io_read('e_eth.dat', data % emiss_ab(:,:,config % species % p_e_eth), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_eth - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_eth)), maxval(data % emiss_ab(:,:,config % species % p_e_eth))

          call aqm_io_read('e_hc3.dat', data % emiss_ab(:,:,config % species % p_e_hc3), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_hc3 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_hc3)), maxval(data % emiss_ab(:,:,config % species % p_e_hc3))

          call aqm_io_read('e_hc5.dat', data % emiss_ab(:,:,config % species % p_e_hc5), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_hc5 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_hc5)), maxval(data % emiss_ab(:,:,config % species % p_e_hc5))

          call aqm_io_read('e_hc8.dat', data % emiss_ab(:,:,config % species % p_e_hc8), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_hc8 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_hc8)), maxval(data % emiss_ab(:,:,config % species % p_e_hc8))

          call aqm_io_read('e_hcho.dat', data % emiss_ab(:,:,config % species % p_e_hcho), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_hcho - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_hcho)), maxval(data % emiss_ab(:,:,config % species % p_e_hcho))

          call aqm_io_read('e_iso.dat', data % emiss_ab(:,:,config % species % p_e_iso), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_iso - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_iso)), maxval(data % emiss_ab(:,:,config % species % p_e_iso))

          call aqm_io_read('e_ket.dat', data % emiss_ab(:,:,config % species % p_e_ket), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_ket - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_ket)), maxval(data % emiss_ab(:,:,config % species % p_e_ket))

          call aqm_io_read('e_nh3.dat', data % emiss_ab(:,:,config % species % p_e_nh3), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_nh3 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_nh3)), maxval(data % emiss_ab(:,:,config % species % p_e_nh3))

          call aqm_io_read('e_no2.dat', data % emiss_ab(:,:,config % species % p_e_no2), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_no2 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_no2)), maxval(data % emiss_ab(:,:,config % species % p_e_no2))

          call aqm_io_read('e_no.dat', data % emiss_ab(:,:,config % species % p_e_no), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_no - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_no)), maxval(data % emiss_ab(:,:,config % species % p_e_no))

          call aqm_io_read('e_oli.dat', data % emiss_ab(:,:,config % species % p_e_oli), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_oli - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_oli)), maxval(data % emiss_ab(:,:,config % species % p_e_oli))

          call aqm_io_read('e_olt.dat', data % emiss_ab(:,:,config % species % p_e_olt), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_olt - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_olt)), maxval(data % emiss_ab(:,:,config % species % p_e_olt))

          call aqm_io_read('e_ora2.dat', data % emiss_ab(:,:,config % species % p_e_ora2), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_ora2 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_ora2)), maxval(data % emiss_ab(:,:,config % species % p_e_ora2))

          call aqm_io_read('e_tol.dat', data % emiss_ab(:,:,config % species % p_e_tol), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_tol - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_tol)), maxval(data % emiss_ab(:,:,config % species % p_e_tol))

          call aqm_io_read('e_xyl.dat', data % emiss_ab(:,:,config % species % p_e_xyl), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," e_xyl - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_xyl)), maxval(data % emiss_ab(:,:,config % species % p_e_xyl))

        end if
      end if

      ! -- emissions from burning biomass
      if (config % biomass_burn_opt > 0) then
        ! -- emissions
        call aqm_io_read('ebu_bc.dat', data % emiss_abu(:,:,config % species % p_e_bc), &
          path=trim(config % fireemi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_bc - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_bc)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_bc))

        call aqm_io_read('ebu_oc.dat', data % emiss_abu(:,:,config % species % p_e_oc), &
          path=trim(config % fireemi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_oc - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_oc)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_oc))

        call aqm_io_read('ebu_pm_10.dat', data % emiss_abu(:,:,config % species % p_e_pm_10), &
          path=trim(config % fireemi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_pm_10 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_pm_10)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_pm_10))

        call aqm_io_read('ebu_pm_25.dat', data % emiss_abu(:,:,config % species % p_e_pm_25), &
          path=trim(config % fireemi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_pm_25 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_pm_25)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_pm_25))

        call aqm_io_read('ebu_so2.dat', data % emiss_abu(:,:,config % species % p_e_so2), &
          path=trim(config % fireemi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_so2 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_so2)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_so2))

        call aqm_io_read('ebu_sulf.dat', data % emiss_abu(:,:,config % species % p_e_sulf), &
          path=trim(config % fireemi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_sulf - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_sulf)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_sulf))

        call aqm_io_read('plumestuff.dat', data % plumestuff, recrange=(/ 1, config % num_plumestuff /), &
          path=trim(config % fireemi_inname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," plumestuff - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % plumestuff), maxval(data % plumestuff)


        if ((config % aqm_opt == AQM_OPT_GOCART_RACM) .or. &
            (config % aqm_opt == AQM_OPT_RACM_SOA_VBS)) then

          call aqm_io_read('ebu_ald.dat', data % emiss_abu(:,:,config % species % p_e_ald), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_ald - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_ald)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_ald))

          call aqm_io_read('ebu_co.dat', data % emiss_abu(:,:,config % species % p_e_co), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_co - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_co)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_co))

          call aqm_io_read('ebu_csl.dat', data % emiss_abu(:,:,config % species % p_e_csl), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_csl - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_csl)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_csl))

          call aqm_io_read('ebu_dms.dat', data % emiss_abu(:,:,config % species % p_e_dms), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_dms - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_dms)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_dms))

          call aqm_io_read('ebu_eth.dat', data % emiss_abu(:,:,config % species % p_e_eth), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_eth - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_eth)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_eth))

          call aqm_io_read('ebu_hc3.dat', data % emiss_abu(:,:,config % species % p_e_hc3), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_hc3 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_hc3)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_hc3))

          call aqm_io_read('ebu_hc5.dat', data % emiss_abu(:,:,config % species % p_e_hc5), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_hc5 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_hc5)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_hc5))

          call aqm_io_read('ebu_hc8.dat', data % emiss_abu(:,:,config % species % p_e_hc8), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_hc8 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_hc8)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_hc8))

          call aqm_io_read('ebu_hcho.dat', data % emiss_abu(:,:,config % species % p_e_hcho), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_hcho - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_hcho)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_hcho))

          call aqm_io_read('ebu_iso.dat', data % emiss_abu(:,:,config % species % p_e_iso), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_iso - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_iso)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_iso))

          call aqm_io_read('ebu_ket.dat', data % emiss_abu(:,:,config % species % p_e_ket), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_ket - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_ket)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_ket))

          call aqm_io_read('ebu_nh3.dat', data % emiss_abu(:,:,config % species % p_e_nh3), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_nh3 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_nh3)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_nh3))

          call aqm_io_read('ebu_no2.dat', data % emiss_abu(:,:,config % species % p_e_no2), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_no2 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_no2)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_no2))

          call aqm_io_read('ebu_no.dat', data % emiss_abu(:,:,config % species % p_e_no), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_no - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_no)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_no))

          call aqm_io_read('ebu_oli.dat', data % emiss_abu(:,:,config % species % p_e_oli), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_oli - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_oli)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_oli))

          call aqm_io_read('ebu_olt.dat', data % emiss_abu(:,:,config % species % p_e_olt), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_olt - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_olt)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_olt))

          call aqm_io_read('ebu_ora2.dat', data % emiss_abu(:,:,config % species % p_e_ora2), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_ora2 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_ora2)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_ora2))

          call aqm_io_read('ebu_tol.dat', data % emiss_abu(:,:,config % species % p_e_tol), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_tol - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_tol)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_tol))

          call aqm_io_read('ebu_xyl.dat', data % emiss_abu(:,:,config % species % p_e_xyl), &
            path=trim(config % fireemi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," ebu_xyl - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_xyl)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_xyl))

        end if
      end if

      ! -- volcanic stuff
      if (config % aqm_opt == AQM_OPT_GOCART) then
         ! -- also for aqm_opt = 316, 317, 502

        if (config % ash_mass > -900._AQM_KIND_R4) then
          ! -- TODO
          call aqm_io_read('volcanic.dat', data % emiss_ash_mass,recrange=(/ 4, 4 /), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," emiss_ash_mass - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ash_mass), &
            maxval(data % emiss_ash_mass)

          call aqm_io_read('volcanic.dat', data % emiss_ash_height,recrange=(/ 5, 5 /), &
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," emiss_ash_height - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ash_height), &
            maxval(data % emiss_ash_height)

          call aqm_io_read('volcanic.dat', data % emiss_ash_dt, recrange=(/ 6, 6 /),&
            path=trim(config % emi_inname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_read: PET:",i4," DE:",i2," tile=",i2," emiss_ash_dt - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ash_dt), &
            maxval(data % emiss_ash_dt)

        end if
        ! -- overwrite ash_mass if namelist value exists
        if (config % ash_mass > -100._AQM_KIND_R4)then
          write(0,*)'using namelist value for ash_mass'
          where(data % emiss_ash_mass > 0._AQM_KIND_R4) data % emiss_ash_mass = config % ash_mass
        endif
        ! -- overwrite ash_height if namelist value exists
        if (config % ash_height > 0._AQM_KIND_R4) then

          write(0,*)'using namelist value for ash_height'
          where(data % emiss_ash_height > 0._AQM_KIND_R4) data % emiss_ash_height = config % ash_height
          where(data % emiss_ash_height < 1._AQM_KIND_R4) data % emiss_ash_dt     = 0._AQM_KIND_R4

        else if (config % ash_height < -990._AQM_KIND_R4) then

          write(0,*)'resetting all ash variables to zero'
          data % emiss_ash_mass   = 0._AQM_KIND_R4
          data % emiss_ash_height = 0._AQM_KIND_R4
          data % emiss_ash_dt     = 0._AQM_KIND_R4

        endif
      end if

    end do

  end subroutine aqm_backgd_read


  subroutine aqm_backgd_write(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount, localpe, tile
    type(aqm_data_type),   pointer :: data => null()
    type(aqm_config_type), pointer :: config => null()

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(deCount=deCount, config=config, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    call aqm_comm_get(localpe=localpe, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    do de = 0, deCount-1
      call aqm_model_get(de=de, data=data, tile=tile, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      if ((config % aqm_opt == AQM_OPT_RACM_SOA_VBS) .or.  &
          (config % aqm_opt >= AQM_OPT_GOCART)       .and. &
          (config % aqm_opt < 500)) then

        ! -- dust erosion factors
        call aqm_io_write('dm0.dat', data % dm0, path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," dm0 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % dm0)

        ! -- dust erosion factors
        call aqm_io_write('erod1.dat', data % ero1, path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ero1 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % ero1)
        call aqm_io_write('erod2.dat', data % ero2, path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ero2 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % ero2)
        call aqm_io_write('erod3.dat', data % ero3, path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ero3 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % ero3)

        ! -- bacground values for aqmical species
        call aqm_io_write('h2o2.dat', data % h2o2_backgd, path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," h2o2 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % h2o2_backgd)
        call aqm_io_write('no3.dat', data % no3_backgd, path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," no3 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % no3_backgd)
        call aqm_io_write('oh.dat', data % oh_backgd, path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," oh - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % dm0), maxval(data % oh_backgd)

        ! -- emissions
        call aqm_io_write('e_bc.dat', data % emiss_ab(:,:,config % species % p_e_bc), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_bc - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_bc)), maxval(data % emiss_ab(:,:,config % species % p_e_bc))

        call aqm_io_write('e_oc.dat', data % emiss_ab(:,:,config % species % p_e_oc), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_oc - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_oc)), maxval(data % emiss_ab(:,:,config % species % p_e_oc))

        call aqm_io_write('e_pm_10.dat', data % emiss_ab(:,:,config % species % p_e_pm_10), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_pm_10 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_pm_10)), maxval(data % emiss_ab(:,:,config % species % p_e_pm_10))

        call aqm_io_write('e_pm_25.dat', data % emiss_ab(:,:,config % species % p_e_pm_25), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_pm_25 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_pm_25)), maxval(data % emiss_ab(:,:,config % species % p_e_pm_25))

        call aqm_io_write('e_so2.dat', data % emiss_ab(:,:,config % species % p_e_so2), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_so2 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_so2)), maxval(data % emiss_ab(:,:,config % species % p_e_so2))

        call aqm_io_write('e_sulf.dat', data % emiss_ab(:,:,config % species % p_e_sulf), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_sulf - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_ab(:,:,config % species % p_e_sulf)), maxval(data % emiss_ab(:,:,config % species % p_e_sulf))
        
        if (config % dust_opt == DUST_OPT_AFWA) then
           ! -- DUST_OPT_AFWA
          call aqm_io_write('clay.dat', data % clayfrac, path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," clayfrac - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % dm0), maxval(data % clayfrac)
          call aqm_io_write('sand.dat', data % sandfrac, path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," sandfrac - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % dm0), maxval(data % sandfrac)
        end if

        if ((config % aqm_opt == AQM_OPT_GOCART_RACM) .or. &
            (config % aqm_opt == AQM_OPT_RACM_SOA_VBS)) then

          call aqm_io_write('e_ald.dat', data % emiss_ab(:,:,config % species % p_e_ald), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_ald - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_ald)), maxval(data % emiss_ab(:,:,config % species % p_e_ald))

          call aqm_io_write('e_co.dat', data % emiss_ab(:,:,config % species % p_e_co), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_co - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_co)), maxval(data % emiss_ab(:,:,config % species % p_e_co))

          call aqm_io_write('e_csl.dat', data % emiss_ab(:,:,config % species % p_e_csl), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_csl - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_csl)), maxval(data % emiss_ab(:,:,config % species % p_e_csl))

          call aqm_io_write('e_dms.dat', data % emiss_ab(:,:,config % species % p_e_dms), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_dms - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_dms)), maxval(data % emiss_ab(:,:,config % species % p_e_dms))

          call aqm_io_write('e_eth.dat', data % emiss_ab(:,:,config % species % p_e_eth), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_eth - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_eth)), maxval(data % emiss_ab(:,:,config % species % p_e_eth))

          call aqm_io_write('e_hc3.dat', data % emiss_ab(:,:,config % species % p_e_hc3), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_hc3 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_hc3)), maxval(data % emiss_ab(:,:,config % species % p_e_hc3))

          call aqm_io_write('e_hc5.dat', data % emiss_ab(:,:,config % species % p_e_hc5), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_hc5 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_hc5)), maxval(data % emiss_ab(:,:,config % species % p_e_hc5))

          call aqm_io_write('e_hc8.dat', data % emiss_ab(:,:,config % species % p_e_hc8), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_hc8 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_hc8)), maxval(data % emiss_ab(:,:,config % species % p_e_hc8))

          call aqm_io_write('e_hcho.dat', data % emiss_ab(:,:,config % species % p_e_hcho), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_hcho - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_hcho)), maxval(data % emiss_ab(:,:,config % species % p_e_hcho))

          call aqm_io_write('e_iso.dat', data % emiss_ab(:,:,config % species % p_e_iso), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_iso - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_iso)), maxval(data % emiss_ab(:,:,config % species % p_e_iso))

          call aqm_io_write('e_ket.dat', data % emiss_ab(:,:,config % species % p_e_ket), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_ket - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_ket)), maxval(data % emiss_ab(:,:,config % species % p_e_ket))

          call aqm_io_write('e_nh3.dat', data % emiss_ab(:,:,config % species % p_e_nh3), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_nh3 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_nh3)), maxval(data % emiss_ab(:,:,config % species % p_e_nh3))

          call aqm_io_write('e_no2.dat', data % emiss_ab(:,:,config % species % p_e_no2), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_no2 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_no2)), maxval(data % emiss_ab(:,:,config % species % p_e_no2))

          call aqm_io_write('e_no.dat', data % emiss_ab(:,:,config % species % p_e_no), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_no - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_no)), maxval(data % emiss_ab(:,:,config % species % p_e_no))

          call aqm_io_write('e_oli.dat', data % emiss_ab(:,:,config % species % p_e_oli), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_oli - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_oli)), maxval(data % emiss_ab(:,:,config % species % p_e_oli))

          call aqm_io_write('e_olt.dat', data % emiss_ab(:,:,config % species % p_e_olt), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_olt - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_olt)), maxval(data % emiss_ab(:,:,config % species % p_e_olt))

          call aqm_io_write('e_ora2.dat', data % emiss_ab(:,:,config % species % p_e_ora2), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_ora2 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_ora2)), maxval(data % emiss_ab(:,:,config % species % p_e_ora2))

          call aqm_io_write('e_tol.dat', data % emiss_ab(:,:,config % species % p_e_tol), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_tol - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_tol)), maxval(data % emiss_ab(:,:,config % species % p_e_tol))

          call aqm_io_write('e_xyl.dat', data % emiss_ab(:,:,config % species % p_e_xyl), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," e_xyl - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_ab(:,:,config % species % p_e_xyl)), maxval(data % emiss_ab(:,:,config % species % p_e_xyl))

        end if
      end if

      ! -- emissions from burning biomass
      if (config % biomass_burn_opt > 0) then
        ! -- emissions
        call aqm_io_write('ebu_bc.dat', data % emiss_abu(:,:,config % species % p_e_bc), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_bc - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_bc)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_bc))

        call aqm_io_write('ebu_oc.dat', data % emiss_abu(:,:,config % species % p_e_oc), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_oc - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_oc)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_oc))

        call aqm_io_write('ebu_pm_10.dat', data % emiss_abu(:,:,config % species % p_e_pm_10), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_pm_10 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_pm_10)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_pm_10))

        call aqm_io_write('ebu_pm_25.dat', data % emiss_abu(:,:,config % species % p_e_pm_25), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_pm_25 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_pm_25)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_pm_25))

        call aqm_io_write('ebu_so2.dat', data % emiss_abu(:,:,config % species % p_e_so2), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_so2 - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_so2)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_so2))

        call aqm_io_write('ebu_sulf.dat', data % emiss_abu(:,:,config % species % p_e_sulf), &
          path=trim(config % emi_outname), de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_sulf - min/max = "2g16.6)') localpe, de, &
          tile, minval(data % emiss_abu(:,:,config % species % p_e_sulf)), &
          maxval(data % emiss_abu(:,:,config % species % p_e_sulf))

!       call aqm_io_write('plumestuff.dat', data % plumestuff, path=trim(config % emi_outname), de=de, rc=localrc)
!       if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
!       write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," plumestuff - min/max = "2g16.6)') localpe, de, &
!         tile, minval(data % plumestuff), maxval(data % plumestuff)


        if ((config % aqm_opt == AQM_OPT_GOCART_RACM) .or. &
            (config % aqm_opt == AQM_OPT_RACM_SOA_VBS)) then

          call aqm_io_write('ebu_ald.dat', data % emiss_abu(:,:,config % species % p_e_ald), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_ald - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_ald)), maxval(data % emiss_abu(:,:,config % species % p_e_ald))

          call aqm_io_write('ebu_co.dat', data % emiss_abu(:,:,config % species % p_e_co), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_co - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_co)), maxval(data % emiss_abu(:,:,config % species % p_e_co))

          call aqm_io_write('ebu_csl.dat', data % emiss_abu(:,:,config % species % p_e_csl), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_csl - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_csl)), maxval(data % emiss_abu(:,:,config % species % p_e_csl))

          call aqm_io_write('ebu_dms.dat', data % emiss_abu(:,:,config % species % p_e_dms), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_dms - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_dms)), maxval(data % emiss_abu(:,:,config % species % p_e_dms))

          call aqm_io_write('ebu_eth.dat', data % emiss_abu(:,:,config % species % p_e_eth), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_eth - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_eth)), maxval(data % emiss_abu(:,:,config % species % p_e_eth))

          call aqm_io_write('ebu_hc3.dat', data % emiss_abu(:,:,config % species % p_e_hc3), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_hc3 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_hc3)), maxval(data % emiss_abu(:,:,config % species % p_e_hc3))

          call aqm_io_write('ebu_hc5.dat', data % emiss_abu(:,:,config % species % p_e_hc5), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_hc5 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_hc5)), maxval(data % emiss_abu(:,:,config % species % p_e_hc5))

          call aqm_io_write('ebu_hc8.dat', data % emiss_abu(:,:,config % species % p_e_hc8), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_hc8 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_hc8)), maxval(data % emiss_abu(:,:,config % species % p_e_hc8))

          call aqm_io_write('ebu_hcho.dat', data % emiss_abu(:,:,config % species % p_e_hcho), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_hcho - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_hcho)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_hcho))

          call aqm_io_write('ebu_iso.dat', data % emiss_abu(:,:,config % species % p_e_iso), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_iso - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_iso)), maxval(data % emiss_abu(:,:,config % species % p_e_iso))

          call aqm_io_write('ebu_ket.dat', data % emiss_abu(:,:,config % species % p_e_ket), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_ket - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_ket)), maxval(data % emiss_abu(:,:,config % species % p_e_ket))

          call aqm_io_write('ebu_nh3.dat', data % emiss_abu(:,:,config % species % p_e_nh3), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_nh3 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_nh3)), maxval(data % emiss_abu(:,:,config % species % p_e_nh3))

          call aqm_io_write('ebu_no2.dat', data % emiss_abu(:,:,config % species % p_e_no2), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_no2 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_no2)), maxval(data % emiss_abu(:,:,config % species % p_e_no2))

          call aqm_io_write('ebu_no.dat', data % emiss_abu(:,:,config % species % p_e_no), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_no - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_no)), maxval(data % emiss_abu(:,:,config % species % p_e_no))

          call aqm_io_write('ebu_oli.dat', data % emiss_abu(:,:,config % species % p_e_oli), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_oli - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_oli)), maxval(data % emiss_abu(:,:,config % species % p_e_oli))

          call aqm_io_write('ebu_olt.dat', data % emiss_abu(:,:,config % species % p_e_olt), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_olt - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_olt)), maxval(data % emiss_abu(:,:,config % species % p_e_olt))

          call aqm_io_write('ebu_ora2.dat', data % emiss_abu(:,:,config % species % p_e_ora2), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_ora2 - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_ora2)), &
            maxval(data % emiss_abu(:,:,config % species % p_e_ora2))

          call aqm_io_write('ebu_tol.dat', data % emiss_abu(:,:,config % species % p_e_tol), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_tol - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_tol)), maxval(data % emiss_abu(:,:,config % species % p_e_tol))

          call aqm_io_write('ebu_xyl.dat', data % emiss_abu(:,:,config % species % p_e_xyl), &
            path=trim(config % emi_outname), de=de, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
          write(6,'("aqm_backgd_write: PET:",i4," DE:",i2," tile=",i2," ebu_xyl - min/max = "2g16.6)') localpe, de, &
            tile, minval(data % emiss_abu(:,:,config % species % p_e_xyl)), maxval(data % emiss_abu(:,:,config % species % p_e_xyl))

        end if
      end if

    end do

  end subroutine aqm_backgd_write

  subroutine aqm_output_init(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount
    integer :: ids, ide, jds, jde, nvl, nt
    type(aqm_data_type),   pointer :: data   => null()
    type(aqm_config_type), pointer :: config => null()

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(deCount=deCount, config=config, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    do de = 0, deCount-1
      call aqm_model_get(de=de, data=data, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
      call aqm_model_domain_get(de=de, ids=ids, ide=ide, jds=jds, jde=jde, nl=nvl, nt=nt, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      ! -- dust 
      if (.not.allocated(data % emi_d1)) then
        allocate(data % emi_d1(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emi_d1 = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % emi_d2)) then
        allocate(data % emi_d2(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emi_d2 = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % emi_d3)) then
        allocate(data % emi_d3(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emi_d3 = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % emi_d4)) then
        allocate(data % emi_d4(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emi_d4 = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % emi_d5)) then
        allocate(data % emi_d5(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % emi_d5 = 0._AQM_KIND_R4
      end if
      ! column burden
      if (.not.allocated(data % intaer)) then
        allocate(data % intaer(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % intaer = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % intsea)) then
        allocate(data % intsea(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % intsea = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % intbc)) then
        allocate(data % intbc(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % intbc = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % intdust)) then
        allocate(data % intdust(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % intdust = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % intoc)) then
        allocate(data % intoc(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % intoc = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % intsulf)) then
        allocate(data % intsulf(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % intsulf = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % aod2d)) then
        allocate(data % aod2d(ids:ide,jds:jde), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % aod2d = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % ext_cof)) then
        allocate(data % ext_cof(ids:ide,jds:jde,nvl,config % nbands), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % ext_cof = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % sscal)) then
        allocate(data % sscal(ids:ide,jds:jde,nvl,config % nbands), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % sscal = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % asymp)) then
        allocate(data % asymp(ids:ide,jds:jde,nvl,config % nbands), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % asymp = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % pm10)) then
        allocate(data % pm10(ids:ide,jds:jde,nvl), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % pm10 = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % pm25)) then
        allocate(data % pm25(ids:ide,jds:jde,nvl), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % pm25 = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % ebu_oc)) then
        allocate(data % ebu_oc(ids:ide,jds:jde,nvl), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % ebu_oc = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % oh_bg)) then
        allocate(data % oh_bg(ids:ide,jds:jde,nvl), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % oh_bg = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % h2o2_bg)) then
        allocate(data % h2o2_bg(ids:ide,jds:jde,nvl), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % h2o2_bg = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % no3_bg)) then
        allocate(data % no3_bg(ids:ide,jds:jde,nvl), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % no3_bg = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % wet_dep)) then
        allocate(data % wet_dep(ids:ide,jds:jde,config % num_aqm), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % wet_dep = 0._AQM_KIND_R4
      end if

      if (.not.allocated(data % trdp)) then
        allocate(data % trdp(ids:ide,jds:jde,nvl,nt), stat=localrc)
        if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
        data % trdp = 0._AQM_KIND_R4
      end if

    end do

  end subroutine aqm_output_init

  subroutine aqm_output_write(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount
    integer :: ids, ide, jds, jde
    integer :: advanceCount
    type(aqm_config_type),  pointer :: config   => null()
    type(aqm_species_type), pointer :: s        => null()
    type(aqm_data_type),    pointer :: data     => null()
    type(aqm_state_type),   pointer :: stateOut => null()

    character(len=*), parameter :: filepos = 'append'

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(deCount=deCount, config=config, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    if (deCount < 1) return

    if (config % archive_step < 0) return

    call aqm_model_clock_get(advanceCount=advanceCount, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    if (mod(advanceCount, config % archive_step) == 0) then

      s => config % species

      do de = 0, deCount-1
        call aqm_model_get(de=de, data=data, stateOut=stateOut, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

        select case (config % aqm_opt)
          case (AQM_OPT_GOCART, AQM_OPT_GOCART_RACM)

            call aqm_io_write('d1st', stateOut % tr3d(:,:,:,config % ntra + s % p_dust_1), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('d2st', stateOut % tr3d(:,:,:,config % ntra + s % p_dust_2), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('d3st', stateOut % tr3d(:,:,:,config % ntra + s % p_dust_3), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('d4st', stateOut % tr3d(:,:,:,config % ntra + s % p_dust_4), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('d5st', stateOut % tr3d(:,:,:,config % ntra + s % p_dust_5), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

            call aqm_io_write('dms1', stateOut % tr3d(:,:,:,config % ntra + s % p_dms), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('pmsa', stateOut % tr3d(:,:,:,config % ntra + s % p_msa), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('s1ea', stateOut % tr3d(:,:,:,config % ntra + s % p_seas_1), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('s2ea', stateOut % tr3d(:,:,:,config % ntra + s % p_seas_2), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('s3ea', stateOut % tr3d(:,:,:,config % ntra + s % p_seas_3), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('s4ea', stateOut % tr3d(:,:,:,config % ntra + s % p_seas_4), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

            call aqm_io_write('pbc1', stateOut % tr3d(:,:,:,config % ntra + s % p_bc1), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('pbc2', stateOut % tr3d(:,:,:,config % ntra + s % p_bc2), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('poc1', stateOut % tr3d(:,:,:,config % ntra + s % p_oc1), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('poc2', stateOut % tr3d(:,:,:,config % ntra + s % p_oc2), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('pso2', stateOut % tr3d(:,:,:,config % ntra + s % p_so2), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('sulf', stateOut % tr3d(:,:,:,config % ntra + s % p_sulf), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('pp25', stateOut % tr3d(:,:,:,config % ntra + s % p_p25), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('pp10', stateOut % tr3d(:,:,:,config % ntra + s % p_p10), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

            call aqm_io_write('pm10', data % pm10, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('pm25', data % pm25, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

            call aqm_io_write('emd1', data % emi_d1, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('emd2', data % emi_d2, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('emd3', data % emi_d3, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('emd4', data % emi_d4, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('emd5', data % emi_d5, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            !column burden
            call aqm_io_write('cbae', data % intaer, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('cbbc', data % intbc, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('cboc', data % intoc, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('cbsf', data % intsulf, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('cbdt', data % intdust, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('cbss', data % intsea, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            
            call aqm_io_write('ao2D', data % aod2d, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

            call aqm_io_write('wbc2', data % wet_dep(:,:,s % p_bc2), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('woc2', data % wet_dep(:,:,s % p_oc2), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wp10', data % wet_dep(:,:,s % p_p10), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wp25', data % wet_dep(:,:,s % p_p25), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wso4', data % wet_dep(:,:,s % p_sulf), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wdt1', data % wet_dep(:,:,s % p_dust_1), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wdt2', data % wet_dep(:,:,s % p_dust_2), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wdt3', data % wet_dep(:,:,s % p_dust_3), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wdt4', data % wet_dep(:,:,s % p_dust_4), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wdt5', data % wet_dep(:,:,s % p_dust_5), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wse1', data % wet_dep(:,:,s % p_seas_1), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wse2', data % wet_dep(:,:,s % p_seas_2), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wse3', data % wet_dep(:,:,s % p_seas_3), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('wse4', data % wet_dep(:,:,s % p_seas_4), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

            call aqm_io_write('aso2', data % emiss_ab(:,:,s % p_e_so2), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('anbc', data % emiss_ab(:,:,s % p_e_bc), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('anoc', data % emiss_ab(:,:,s % p_e_oc), &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

            if (config % aqm_opt == AQM_OPT_GOCART) then

              call aqm_io_write('ohbg', data % oh_bg, &
                path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
              if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
              call aqm_io_write('hobg', data % h2o2_bg, &
                path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
              if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
              call aqm_io_write('no3b', data % no3_bg, &
                path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
              if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
              call aqm_io_write('ocbb', data % ebu_oc, &
                path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
              if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

            end if
            
          case (AQM_OPT_RACM_SOA_VBS)

            call aqm_io_write('ao2D', data % aod2d, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('pm10', data % pm10, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
            call aqm_io_write('pm25', data % pm25, &
              path=trim(config % emi_outname), pos=filepos, de=de, rc=localrc)
            if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

          case default

            ! -- no output

        end select

      end do

    end if

  end subroutine aqm_output_write
#endif
end module aqm_iodata_mod
