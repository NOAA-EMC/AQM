module aqm_config_mod

  use aqm_rc_mod
  use aqm_types_mod,   only : AQM_MAXSTR, AQM_KIND_R4
  use aqm_species_mod, only : aqm_species_type
  use aqm_comm_mod,    only : aqm_comm_bcast, aqm_comm_isroot

  implicit none

  ! -- Currently available modules
  integer, parameter :: AQM_OPT_NONE         = 0
  integer, parameter :: AQM_OPT_GOCART       = 300
  integer, parameter :: AQM_OPT_GOCART_RACM  = 301
  integer, parameter :: AQM_OPT_RACM_SOA_VBS = 108
  integer, parameter :: AQM_OPT_MAX          = 500

  integer, parameter :: DUST_OPT_GOCART = 1
  integer, parameter :: DUST_OPT_AFWA   = 3

  character(len=*), parameter :: aqm_file_nml = 'input.nml'

  ! -- data structure for configuration options
  type aqm_config_type
    sequence
    character(len=AQM_MAXSTR) :: emi_inname         = ''
    character(len=AQM_MAXSTR) :: fireemi_inname     = ''
    character(len=AQM_MAXSTR) :: emi_outname        = ''
    character(len=AQM_MAXSTR) :: fireemi_outname    = ''
    character(len=AQM_MAXSTR) :: input_aqm_inname  = ''
    character(len=AQM_MAXSTR) :: input_aqm_outname = ''
    character(len=AQM_MAXSTR) :: aqm_hist_outname  = 'fim_out_'
    integer :: io_style_emissions = 0
    real(AQM_KIND_R4) :: bioemdt
    real(AQM_KIND_R4) :: photdt
    real(AQM_KIND_R4) :: aqmdt
    integer :: kemit
    integer :: kfuture
    integer :: aqm_conv_tr
    integer :: aqm_opt
    integer :: gasaqm_onoff
    integer :: aeraqm_onoff
    integer :: wetscav_onoff
    integer :: cldaqm_onoff
    integer :: vertmix_onoff
    integer :: aqm_in_opt
    integer :: phot_opt
    integer :: drydep_opt
    real(AQM_KIND_R4) :: depo_fact
    integer :: emiss_opt
    integer :: dust_opt
    integer :: dmsemis_opt
    integer :: seas_opt
    integer :: bio_emiss_opt
    integer :: biomass_burn_opt
    integer :: plumerisefire_frq
    integer :: emiss_inpt_opt
    integer :: gas_bc_opt
    integer :: gas_ic_opt
    integer :: aer_bc_opt
    integer :: aer_ic_opt
    logical :: have_bcs_aqm
    integer :: aer_ra_feedback    = 0
    integer :: aer_op_opt
    integer :: conv_tr_aqaqm
    integer :: call_biomass       = 1
    integer :: call_aqmistry     = 1
    integer :: call_radiation     = 1
    logical :: readrestart        = .false.
    integer :: archive_step
    real(AQM_KIND_R4):: ash_mass
    real(AQM_KIND_R4):: ash_height
    ! -- control variables
    integer :: ntra                = 3      ! # of tracers advected on small dt: FV3 only has 3 tracers
    integer :: ntrb                = 0      ! # of tracers advected on large dt: will include aqmistry
    integer :: num_aqm            = 0
    integer :: num_moist           = 0
    integer :: num_emis_ant        = 0
    integer :: num_emis_vol        = 0
    integer :: num_ebu             = 0
    integer :: num_ebu_in          = 0
    integer :: numgas              = 0
    integer :: num_emis_season_bb  = 0
    integer :: num_emis_season_ant = 0
    integer :: nbegin              = 0
    ! -- parameters
    integer :: nbands   = 14
    integer :: nbandlw  = 16
    integer :: num_soil_layers = 4
    integer :: num_scalar      = 1
    integer :: nvl_gocart      = 55  ! number of input levels from gocart file
    integer :: num_ext_coef    = 5
    integer :: num_bscat_coef  = 3
    integer :: num_asym_par    = 3
    INTEGER :: num_emis_dust   = 5
    INTEGER :: num_emis_seas   = 4
    INTEGER :: num_plumestuff  = 8
    INTEGER :: ne_area         = 41
    INTEGER :: nmegan          = 1
    INTEGER :: mp_physics      = 0
  ! -- configuration variables for output:
  !  . grid level defined in FIM Makefile
    integer :: glvl            = 0
  ! -- control variables
    logical :: call_gocart     = .false.

  ! -- CMAQ configuration options
    logical :: run_aero

  ! -- species
    type(aqm_species_type), pointer :: species => null()

  end type aqm_config_type

  private

  public :: aqm_config_type
  ! -- provide subtypes
  public :: aqm_species_type

  public :: aqm_config_read
  public :: aqm_config_control_init
  public :: aqm_config_species_init
  public :: AQM_OPT_NONE,         &
            AQM_OPT_GOCART,       &
            AQM_OPT_GOCART_RACM,  &
            AQM_OPT_RACM_SOA_VBS, &
            AQM_OPT_MAX
  public :: DUST_OPT_GOCART, &
            DUST_OPT_AFWA

contains

  subroutine aqm_config_read(config, rc)

    type(aqm_config_type), intent(inout) :: config
    integer, optional,     intent(out)   :: rc

    ! -- local variables
    integer, parameter :: unit = 200

    integer                :: localrc, iostat
    integer                :: buffer(24)
    real(AQM_KIND_R4)      :: rbuffer(6)
    character(AQM_MAXSTR)  :: sbuffer(4)

    ! -- variables in input namelist
    character(len=AQM_MAXSTR) :: emi_inname
    character(len=AQM_MAXSTR) :: fireemi_inname
    character(len=AQM_MAXSTR) :: emi_outname
    character(len=AQM_MAXSTR) :: fireemi_outname
    character(len=AQM_MAXSTR) :: input_aqm_inname
    character(len=AQM_MAXSTR) :: input_aqm_outname
    character(len=AQM_MAXSTR) :: aqm_hist_outname
    integer :: io_style_emissions
    real(AQM_KIND_R4)   :: bioemdt
    real(AQM_KIND_R4)   :: photdt
    real(AQM_KIND_R4)   :: aqmdt
    integer :: kemit
    integer :: kfuture
    integer :: aqm_conv_tr
    integer :: aqm_opt
    integer :: gasaqm_onoff
    integer :: aeraqm_onoff
    integer :: wetscav_onoff
    integer :: cldaqm_onoff
    integer :: vertmix_onoff
    integer :: aqm_in_opt
    integer :: phot_opt
    integer :: drydep_opt
    real(AQM_KIND_R4)   :: depo_fact
    integer :: emiss_opt
    integer :: dust_opt
    integer :: dmsemis_opt
    integer :: seas_opt
    integer :: bio_emiss_opt
    integer :: biomass_burn_opt
    integer :: plumerisefire_frq
    integer :: emiss_inpt_opt
    integer :: gas_bc_opt
    integer :: gas_ic_opt
    integer :: aer_bc_opt
    integer :: aer_ic_opt
    logical :: have_bcs_aqm
    integer :: aer_ra_feedback
    integer :: aer_op_opt
    integer :: conv_tr_aqaqm
    integer :: archive_step
    real(AQM_KIND_R4) :: ash_mass
    real(AQM_KIND_R4) :: ash_height


    namelist /aqm_nml/          &
      emi_inname,               &
      fireemi_inname,           &
      emi_outname,              &
      fireemi_outname,          &
      input_aqm_inname,         &
      input_aqm_outname,        &
      aqm_hist_outname,         &
      io_style_emissions,       &
      bioemdt,                  &
      photdt,                   &
      aqmdt,                    &
      kemit,                    &
      kfuture,                  &
      aqm_conv_tr,              &
      aqm_opt,                  &
      gasaqm_onoff,             &
      aeraqm_onoff,             &
      wetscav_onoff,            &
      cldaqm_onoff,             &
      vertmix_onoff,            &
      aqm_in_opt,               &
      phot_opt,                 &
      drydep_opt,               &
      depo_fact,                &
      emiss_opt,                &
      dust_opt,                 &
      dmsemis_opt,              &
      seas_opt,                 &
      bio_emiss_opt,            &
      biomass_burn_opt,         &
      plumerisefire_frq,        &
      emiss_inpt_opt,           &
      gas_bc_opt,               &
      gas_ic_opt,               &
      aer_bc_opt,               &
      aer_ic_opt,               &
      have_bcs_aqm,             &
      aer_ra_feedback,          &
      aer_op_opt,               &
      conv_tr_aqaqm,            &
      archive_step,             &
      ash_mass,                 &
      ash_height

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- set defaults
    aqm_opt           = AQM_OPT_NONE
    aqmdt             = 3._AQM_KIND_R4
    bioemdt           = 3._AQM_KIND_R4
    kemit             = 1
    kfuture           = 0
    phot_opt          = 1
    photdt            = 60._AQM_KIND_R4
    ash_mass          = -999._AQM_KIND_R4
    ash_height        = -999._AQM_KIND_R4
    depo_fact         = 0._AQM_KIND_R4
    drydep_opt        = 0
    DUST_OPT          = 3
    DMSEMIS_OPT       = 1
    SEAS_OPT          = 1
    EMISS_OPT         = 5
    BIO_EMISS_OPT     = 0
    BIOMASS_BURN_OPT  = 1
    PLUMERISEFIRE_FRQ = 60
    EMISS_INPT_OPT    = 1
    GAS_BC_OPT        = 1
    GAS_IC_OPT        = 1
    AER_BC_OPT        = 1
    AER_IC_OPT        = 1
    gasaqm_onoff      = 1
    aeraqm_onoff      = 1
    wetscav_onoff     = 0
    cldaqm_onoff      = 0
    vertmix_onoff     = 1
    aqm_conv_tr       = 1
    aer_ra_feedback   = 0
    aer_op_opt        = 0
    aqm_in_opt        = 0
    archive_step      = 1
    conv_tr_aqaqm     = 0
    have_bcs_aqm      = .false.
    io_style_emissions = 0
    emi_inname         = ""
    fireemi_inname     = ""
    emi_outname        = ""
    fireemi_outname    = ""
    input_aqm_inname  = ""
    input_aqm_outname = ""
    aqm_hist_outname  = "aqm_out_"

    ! -- read aqm configuration namelist

    if (aqm_comm_isroot()) then
      open(unit, file=aqm_file_nml, form='formatted', status='old', iostat=iostat)
    end if
    call aqm_comm_bcast(iostat, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
    if (aqm_rc_test((iostat /= 0), msg="Failed to open namelist file: "//aqm_file_nml, &
        file=__FILE__, line=__LINE__, rc=rc)) return
    if (aqm_comm_isroot()) then
      rewind(unit)
      read(unit, nml=aqm_nml, iostat=iostat )
      close(unit)
      write(6, nml=aqm_nml)
    end if
    call aqm_comm_bcast(iostat, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
    if (aqm_rc_test((iostat /= 0), msg="Failed to read &aqm namelist in: "//aqm_file_nml, &
        file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- pack integer values in buffer
    buffer = (/ &
      aqm_opt,          &
      kemit,             &
      phot_opt,          &
      DUST_OPT,          &
      DMSEMIS_OPT,       &
      SEAS_OPT,          &
      EMISS_OPT,         &
      BIO_EMISS_OPT,     &
      BIOMASS_BURN_OPT,  &
      PLUMERISEFIRE_FRQ, &
      EMISS_INPT_OPT,    &
      GAS_BC_OPT,        &
      GAS_IC_OPT,        &
      AER_BC_OPT,        &
      AER_IC_OPT,        &
      gasaqm_onoff,     &
      aeraqm_onoff,     &
      wetscav_onoff,     &
      cldaqm_onoff,     &
      vertmix_onoff,     &
      aqm_conv_tr,      &
      aer_ra_feedback,   &
      aqm_in_opt,       &
      archive_step       &
      /)
    ! -- broadcast integer buffer
    call aqm_comm_bcast(buffer, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
    ! -- load integer values in local config
    config % aqm_opt          = buffer( 1 )
    config % kemit             = buffer( 2 )
    config % phot_opt          = buffer( 3 )
    config % dust_opt          = buffer( 4 )
    config % dmsemis_opt       = buffer( 5 )
    config % seas_opt          = buffer( 6 )
    config % emiss_opt         = buffer( 7 )
    config % bio_emiss_opt     = buffer( 8 )
    config % biomass_burn_opt  = buffer( 9 )
    config % plumerisefire_frq = buffer( 10 )
    config % emiss_inpt_opt    = buffer( 11 )
    config % gas_bc_opt        = buffer( 12 )
    config % gas_ic_opt        = buffer( 13 )
    config % aer_bc_opt        = buffer( 14 )
    config % aer_ic_opt        = buffer( 15 )
    config % gasaqm_onoff     = buffer( 16 )
    config % aeraqm_onoff     = buffer( 17 )
    config % wetscav_onoff     = buffer( 18 )
    config % cldaqm_onoff     = buffer( 19 )
    config % vertmix_onoff     = buffer( 20 )
    config % aqm_conv_tr      = buffer( 21 )
    config % aer_ra_feedback   = buffer( 22 )
    config % aqm_in_opt       = buffer( 23 )
    config % archive_step      = buffer( 24 )

    ! -- pack real variables in buffer
    rbuffer = (/ bioemdt, photdt, aqmdt, ash_mass, ash_height, depo_fact /)
    ! -- broadcast real buffer
    call aqm_comm_bcast(rbuffer, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
    ! -- local real values in local buffer
    config % bioemdt    = rbuffer(1)
    config % photdt     = rbuffer(2)
    config % aqmdt     = rbuffer(3)
    config % ash_mass   = rbuffer(4)
    config % ash_height = rbuffer(5)
    config % depo_fact  = rbuffer(6)

    ! -- pack strings into buffer
    sbuffer = (/ aqm_hist_outname, emi_inname, fireemi_inname, emi_outname /)
    ! -- broadcast string variable
    call aqm_comm_bcast(sbuffer, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
    ! -- set string values to config
    config % aqm_hist_outname = sbuffer(1)
    config % emi_inname        = sbuffer(2)
    config % fireemi_inname    = sbuffer(3)
    config % emi_outname       = sbuffer(4)

  end subroutine aqm_config_read

  subroutine aqm_config_control_init(config, rc)

    type(aqm_config_type), intent(inout) :: config
    integer, optional,     intent(out)   :: rc

    ! -- local variables

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    config % ntrb = 0

    config % numgas     = 1
    config % num_ebu    = 0
    config % num_ebu_in = 0

    if (config % mp_physics == 0) then

      select case (config % aqm_opt)
        case(AQM_OPT_GOCART)
          config % num_aqm            = 19
          config % num_ebu             = 7
          config % num_ebu_in          = 7
          config % num_emis_ant        = 7
          config % num_emis_vol        = 0
          config % num_emis_season_ant = 7
          config % num_emis_season_bb  = 7
          config % num_moist           = 3
          ! compute total # of tracers - no ice variable transported
          config % ntrb = config % ntrb + config % num_moist + config % num_aqm - 3

        case(AQM_OPT_GOCART_RACM)
          config % num_aqm            = 66
          config % num_ebu             = 25
          config % num_ebu_in          = 25
          config % num_emis_ant        = 25
          config % num_emis_vol        = 0
          config % num_emis_season_ant = 0
          config % num_emis_season_bb  = 0
          config % numgas              = 49
          config % num_moist           = 3
          ! compute total # of tracers - no ice variable transported
          config % ntrb = config % ntrb + config % num_moist + config % num_aqm - 3

        case(AQM_OPT_RACM_SOA_VBS)
          config % num_aqm            = 103
          config % num_ebu             = 25
          config % num_ebu_in          = 25
          config % num_emis_ant        = 25
          config % num_emis_vol        = 0
          config % num_emis_season_ant = 0
          config % num_emis_season_bb  = 0
          config % numgas              = 65
          config % num_moist           = 3
          ! compute total # of tracers - no ice variable transported
          config % ntrb = config % ntrb + config % num_moist + config % num_aqm - 3

        case default
          call aqm_rc_set(AQM_RC_FAILURE, msg="aqm_opt not implemented", &
            file=__FILE__, line=__LINE__, rc=rc)
          return

      end select

    else

      call aqm_rc_set(AQM_RC_FAILURE, msg="mp_physics option not implemented", &
        file=__FILE__, line=__LINE__, rc=rc)
      return

    end if

    ! -- nbegin is the start address (-1) of the first aqm variable in tr3d
    if (config % num_moist > 3) then
      config % nbegin = config % ntra + config % num_moist - 2
    else
      config % nbegin = config % ntra + config % num_moist - 3
    end if

  end subroutine aqm_config_control_init


  subroutine aqm_config_species_init(config, rc)

    type(aqm_config_type), intent(inout) :: config
    integer, optional,      intent(out)   :: rc

    ! -- local variables
    integer :: localrc

    ! -- begin
    if (.not.associated(config % species)) then
      allocate(config % species, stat=localrc)
      if (aqm_rc_test((localrc /= 0), msg="Failed to allocate species data container", &
        file=__FILE__, line=__LINE__, rc=rc)) return
    end if

    ! -- set pointers to predefined atmospheric tracers
    ! -- NOTE: this is model-dependent
    config % species % p_atm_shum = 1
    config % species % p_atm_cldq = 2
    config % species % p_atm_o3mr = 3

    if (config % aer_ra_feedback == 1) then
      config % species % p_extcof3    = 1
      config % species % p_extcof55   = 2
      config % species % p_extcof106  = 3
      config % species % p_extcof3_5  = 4
      config % species % p_extcof8_12 = 5
      config % species % p_bscof3     = 1
      config % species % p_bscof55    = 2
      config % species % p_bscof106   = 3
      config % species % p_asympar3   = 1
      config % species % p_asympar55  = 2
      config % species % p_asympar106 = 3
    endif

    select case (config % aqm_opt)
      case (AQM_OPT_GOCART)
        ! -- gocart simple
        if (aqm_rc_test((config % num_aqm    /= 19), &
          msg="num_aqm is not equal to 19", &
          file=__FILE__, line=__LINE__, rc=rc)) return
        if (aqm_rc_test((config % num_emis_ant < 4), &
          msg="num_emis_ant smaller than 4", &
          file=__FILE__, line=__LINE__, rc=rc)) return
        if (aqm_rc_test(((config % num_emis_ant < 6) .and. (config % biomass_burn_opt == 1)), &
          msg="num_emis_ant smaller than 6 with biomass_burn_opt=1", &
          file=__FILE__, line=__LINE__, rc=rc)) return
        config % species % p_qv=1
        config % species % p_qc=2
        config % species % p_qi=3
        config % species % p_so2=1
        config % numgas=4
        config % species % p_sulf=2
        config % species % p_dms=3
        config % species % p_msa=4
        config % species % p_p25=5
        config % species % p_bc1=6
        config % species % p_bc2=7
        config % species % p_oc1=8
        config % species % p_oc2=9
        config % species % p_dust_1=10
        config % species % p_dust_2=11
        config % species % p_dust_3=12
        config % species % p_dust_4=13
        config % species % p_dust_5=14
        config % species % p_seas_1=15
        config % species % p_seas_2=16
        config % species % p_seas_3=17
        config % species % p_seas_4=18
        config % species % p_p10   =19
        config % species % p_e_bc  =1
        config % species % p_e_oc  =2
        config % species % p_e_sulf=3
        config % species % p_e_pm_25=4
        config % species % p_e_so2=5
        config % species % p_e_pm_10=6
        config % species % p_e_dms=7

        config % species % p_ebu_bc  =1
        config % species % p_ebu_oc  =2
        config % species % p_ebu_sulf=3
        config % species % p_ebu_pm25=4
        config % species % p_ebu_so2=5
        config % species % p_ebu_pm10=6
        config % species % p_ebu_dms=7

        config % species % p_ebu_in_bc  =1
        config % species % p_ebu_in_oc  =2
        config % species % p_ebu_in_sulf=3
        config % species % p_ebu_in_pm25=4
        config % species % p_ebu_in_so2=5
        config % species % p_ebu_in_pm10=6
        config % species % p_ebu_in_dms=7

        ! -- diagnostic dust and seasale stuff
        config % species % p_edust1=1
        config % species % p_edust2=2
        config % species % p_edust3=3
        config % species % p_edust4=4
        config % species % p_edust5=5
        config % species % p_eseas1=1
        config % species % p_eseas2=2
        config % species % p_eseas3=3
        config % species % p_eseas4=4

      case (AQM_OPT_GOCART_RACM)
        ! -- gocart + racm
        if (aqm_rc_test((config % num_aqm    /= 66), &
          msg="num_aqm is not equal to 66", &
          file=__FILE__, line=__LINE__, rc=rc)) return
        if (aqm_rc_test((config % num_emis_ant < 25), &
          msg="num_emis_ant smaller than 25", &
          file=__FILE__, line=__LINE__, rc=rc)) return

        ! -- initialize pointers for gas phase and aerosol stuff
        config % conv_tr_aqaqm = 1
        call gocartracm_pointers_init(config)
        ! -- initializing photolysis pointers so far done in aqm_alloc (no pointers yet)
        ! -- hydrometeors
        config % species % p_qv=1
        config % species % p_qc=2
        config % species % p_qi=3

      case (AQM_OPT_RACM_SOA_VBS)
        ! -- racm + soa
        if (aqm_rc_test((config % num_aqm   /= 103), &
          msg="num_aqm is not equal to 103", &
          file=__FILE__, line=__LINE__, rc=rc)) return
        if (aqm_rc_test((config % num_emis_ant < 25), &
          msg="num_emis_ant smaller than 25", &
          file=__FILE__, line=__LINE__, rc=rc)) return

        ! -- initialize pointers for gas phase and aerosol stuff
        config % conv_tr_aqaqm = 1
        call racmsoavbs_pointers_init(config)
        ! -- initializing photolysis pointers so far done in aqm_alloc (no pointers yet)
        ! -- hydrometeors
        config % species % p_qv=1
        config % species % p_qc=2
        config % species % p_qi=3
      case default

        call aqm_rc_set(AQM_RC_FAILURE, msg="aqm_opt not implemented", &
          file=__FILE__, line=__LINE__, rc=rc)
        return

    end select


  contains

!package   gocartracm_kpp     aqm_opt==301                   -
!(aqm:so2,sulf,no2,no,o3,hno3,h2o2,ald,hcho,op1,op2,paa,ora1,ora2,nh3,n2o5,no3,pan,hc3,hc5,hc8,eth,co,ete,olt,oli,tol,xyl,aco3,tpan,hono,hno4,ket,gly,mgly,dcb,onit,csl,iso,co2,ch4,udd,hket,api,lim,dien,macr,ho,ho2,dms,msa,p25,bc1,bc2,oc1,oc2,dust_1,dust_2,dust_3,dust_4,dust_5,seas_1,seas_2,seas_3,seas_4,p10
    subroutine gocartracm_pointers_init(config)

      type(aqm_config_type), intent(inout) :: config
    ! -- this module will initialize variable pointers for aqm_opt "301"
    ! -- June 2015

    ! -- 51 variabkes for RACM gas phase
      config % species % p_so2  = 1
      config % species % p_sulf  = 2
      config % species % p_no2  = 3
      config % species % p_no  = 4
      config % species % p_o3  = 5
      config % species % p_hno3  = 6
      config % species % p_h2o2  = 7
      config % species % p_ald  = 8
      config % species % p_hcho  = 9
      config % species % p_op1  = 10
      config % species % p_op2  = 11
      config % species % p_paa  = 12
      config % species % p_ora1  = 13
      config % species % p_ora2  = 14
      config % species % p_nh3  = 15
      config % species % p_n2o5  = 16
      config % species % p_no3  = 17
      config % species % p_pan  = 18
      config % species % p_hc3  = 19
      config % species % p_hc5  = 20
      config % species % p_hc8  = 21
      config % species % p_eth  = 22
      config % species % p_co  = 23
      config % species % p_ete  = 24
      config % species % p_olt  = 25
      config % species % p_oli  = 26
      config % species % p_tol  = 27
      config % species % p_xyl  = 28
      config % species % p_aco3  = 29
      config % species % p_tpan  = 30
      config % species % p_hono  = 31
      config % species % p_hno4  = 32
      config % species % p_ket  = 33
      config % species % p_gly  = 34
      config % species % p_mgly  = 35
      config % species % p_dcb  = 36
      config % species % p_onit  = 37
      config % species % p_csl  = 38
      config % species % p_iso  = 39
      config % species % p_co2  = 40
      config % species % p_ch4  = 41
      config % species % p_udd  = 42
      config % species % p_hket  = 43
      config % species % p_api  = 44
      config % species % p_lim  = 45
      config % species % p_dien  = 46
      config % species % p_macr  = 47
      config % species % p_ho  = 48
      config % species % p_ho2  = 49
      config % species % p_dms  = 50
      config % species % p_msa  = 51
!
! 15 more for GOCART
!
      config % species % p_p25  = 52
      config % species % p_bc1  = 53
      config % species % p_bc2  = 54
      config % species % p_oc1  = 55
      config % species % p_oc2  = 56
      config % species % p_dust_1  = 57
      config % species % p_dust_2  = 58
      config % species % p_dust_3  = 59
      config % species % p_dust_4  = 60
      config % species % p_dust_5  = 61
      config % species % p_seas_1  = 62
      config % species % p_seas_2  = 63
      config % species % p_seas_3  = 64
      config % species % p_seas_4  = 65
      config % species % p_p10  = 66
! emissions
!package   ecptec          emiss_opt==5                   -
!emis_ant:e_iso,e_so2,e_no,e_no2,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt,e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,e_pm_25,e_pm_10,e_oc,e_sulf,e_bc

      config % species % p_e_iso = 1
      config % species % p_e_so2 = 2
      config % species % p_e_no = 3
      config % species % p_e_no2 = 4
      config % species % p_e_co = 5
      config % species % p_e_eth = 6
      config % species % p_e_hc3 = 7
      config % species % p_e_hc5 = 8
      config % species % p_e_hc8 = 9
      config % species % p_e_xyl = 10
      config % species % p_e_olt = 11
      config % species % p_e_oli = 12
      config % species % p_e_tol = 13
      config % species % p_e_csl = 14
      config % species % p_e_hcho = 15
      config % species % p_e_ald = 16
      config % species % p_e_ket = 17
      config % species % p_e_ora2 = 18
      config % species % p_e_nh3 = 19
      config % species % p_e_pm_25 = 20
      config % species % p_e_pm_10 = 21
      config % species % p_e_oc = 22
      config % species % p_e_sulf = 23
      config % species % p_e_bc = 24
      config % species % p_e_dms = 25
! biomass burning
      config % species % p_ebu_iso = 1
      config % species % p_ebu_so2 = 2
      config % species % p_ebu_no = 3
      config % species % p_ebu_no2=4
      config % species % p_ebu_co =5
      config % species % p_ebu_eth=6
      config % species % p_ebu_hc3=7
      config % species % p_ebu_hc5=8
      config % species % p_ebu_hc8=9
      config % species % p_ebu_xyl=10
      config % species % p_ebu_olt=11
      config % species % p_ebu_oli=12
      config % species % p_ebu_tol=13
      config % species % p_ebu_csl=14
      config % species % p_ebu_hcho=15
      config % species % p_ebu_ald=16
      config % species % p_ebu_ket=17
      config % species % p_ebu_ora2=18
      config % species % p_ebu_nh3=19
      config % species % p_ebu_pm25=20
      config % species % p_ebu_pm10=21
      config % species % p_ebu_oc=22
      config % species % p_ebu_sulf=23
      config % species % p_ebu_bc=24
      config % species % p_ebu_dms=25

      config % species % p_ebu_in_iso = 1
      config % species % p_ebu_in_so2 = 2
      config % species % p_ebu_in_no = 3
      config % species % p_ebu_in_no2=4
      config % species % p_ebu_in_co =5
      config % species % p_ebu_in_eth=6
      config % species % p_ebu_in_hc3=7
      config % species % p_ebu_in_hc5=8
      config % species % p_ebu_in_hc8=9
      config % species % p_ebu_in_xyl=10
      config % species % p_ebu_in_olt=11
      config % species % p_ebu_in_oli=12
      config % species % p_ebu_in_tol=13
      config % species % p_ebu_in_csl=14
      config % species % p_ebu_in_hcho=15
      config % species % p_ebu_in_ald=16
      config % species % p_ebu_in_ket=17
      config % species % p_ebu_in_ora2=18
      config % species % p_ebu_in_nh3=19
      config % species % p_ebu_in_pm25=20
      config % species % p_ebu_in_pm10=21
      config % species % p_ebu_in_oc=22
      config % species % p_ebu_in_sulf=23
      config % species % p_ebu_in_bc=24
      config % species % p_ebu_in_dms=25

!dust and seasalt
! diagnostic dust and seasale stuff
      config % species % p_edust1=1
      config % species % p_edust2=2
      config % species % p_edust3=3
      config % species % p_edust4=4
      config % species % p_edust5=5
      config % species % p_eseas1=1
      config % species % p_eseas2=2
      config % species % p_eseas3=3
      config % species % p_eseas4=4

    end subroutine gocartracm_pointers_init
!
!package   gocartracm_kpp     aqm_opt==108                   -
!(aqm:so2,sulf,no2,no,o3,hno3,h2o2,ald,hcho,op1,op2,paa,ora1,ora2,nh3,n2o5,no3,pan,hc3,hc5,hc8,eth,co,ete,olt,oli,tol,xyl,aco3,tpan,hono,hno4,ket,gly,mgly,dcb,onit,csl,iso,co2,ch4,udd,hket,api,lim,dien,macr,ho,ho2,dms,msa,p25,bc1,bc2,oc1,oc2,dust_1,dust_2,dust_3,dust_4,dust_5,seas_1,seas_2,seas_3,seas_4,p10
    subroutine racmsoavbs_pointers_init(config)

      type(aqm_config_type), intent(inout) :: config

    ! -- this subroutine will initialize variable pointers for aqm_opt "108"
    ! -- June 2015

    ! -- 51 variabkes for RACM gas phase
      config % species % p_so2  = 1
      config % species % p_sulf  = 2
      config % species % p_no2  = 3
      config % species % p_no  = 4
      config % species % p_o3  = 5
      config % species % p_hno3  = 6
      config % species % p_h2o2  = 7
      config % species % p_ald  = 8
      config % species % p_hcho  = 9
      config % species % p_op1  = 10
      config % species % p_op2  = 11
      config % species % p_paa  = 12
      config % species % p_ora1  = 13
      config % species % p_ora2  = 14
      config % species % p_nh3  = 15
      config % species % p_n2o5  = 16
      config % species % p_no3  = 17
      config % species % p_pan  = 18
      config % species % p_hc3  = 19
      config % species % p_hc5  = 20
      config % species % p_hc8  = 21
      config % species % p_eth  = 22
      config % species % p_co  = 23
      config % species % p_ete  = 24
      config % species % p_olt  = 25
      config % species % p_oli  = 26
      config % species % p_tol  = 27
      config % species % p_xyl  = 28
      config % species % p_aco3  = 29
      config % species % p_tpan  = 30
      config % species % p_hono  = 31
      config % species % p_hno4  = 32
      config % species % p_ket  = 33
      config % species % p_gly  = 34
      config % species % p_mgly  = 35
      config % species % p_dcb  = 36
      config % species % p_onit  = 37
      config % species % p_csl  = 38
      config % species % p_iso  = 39
      config % species % p_co2  = 40
      config % species % p_ch4  = 41
      config % species % p_udd  = 42
      config % species % p_hket  = 43
      config % species % p_api  = 44
      config % species % p_lim  = 45
      config % species % p_dien = 46
      config % species % p_macr = 47
      config % species % p_hace = 48
      config % species % p_ishp = 49
      config % species % p_ison = 50
      config % species % p_mahp = 51
      config % species % p_mpan = 52
      config % species % p_nald = 53
      config % species % p_sesq = 54
      config % species % p_mbo  = 55
      config % species % p_cvasoa1 = 56
      config % species % p_cvasoa2 = 57
      config % species % p_cvasoa3 = 58
      config % species % p_cvasoa4 = 59
      config % species % p_cvbsoa1 = 60
      config % species % p_cvbsoa2 = 61
      config % species % p_cvbsoa3 = 62
      config % species % p_cvbsoa4 = 63
      config % species % p_ho  = 64
      config % species % p_ho2 = 65
      config % species % p_so4aj = 66
      config % species % p_so4ai = 67
      config % species % p_nh4aj = 68
      config % species % p_nh4ai = 69
      config % species % p_no3aj = 70
      config % species % p_no3ai = 71
      config % species % p_naaj  = 72
      config % species % p_naai  = 73
      config % species % p_claj  = 74
      config % species % p_clai  = 75
      config % species % p_asoa1j = 76
      config % species % p_asoa1i = 77
      config % species % p_asoa2j = 78
      config % species % p_asoa2i = 79
      config % species % p_asoa3j = 80
      config % species % p_asoa3i = 81
      config % species % p_asoa4j = 82
      config % species % p_asoa4i = 83
      config % species % p_bsoa1j = 84
      config % species % p_bsoa1i = 85
      config % species % p_bsoa2j = 86
      config % species % p_bsoa2i = 87
      config % species % p_bsoa3j = 88
      config % species % p_bsoa3i = 89
      config % species % p_bsoa4j = 90
      config % species % p_bsoa4i = 91
      config % species % p_orgpaj = 92
      config % species % p_orgpai = 93
      config % species % p_ecj    = 94
      config % species % p_eci    = 95
      config % species % p_p25j   = 96
      config % species % p_p25i   = 97
      config % species % p_antha  = 98
      config % species % p_seas   = 99
      config % species % p_soila  = 100
      config % species % p_nu0    = 101
      config % species % p_ac0    = 102
      config % species % p_corn   = 103
!
! emissions
!package   ecptec          emiss_opt==5                   -
!emis_ant:e_iso,e_so2,e_no,e_no2,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt,e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,e_pm_25,e_pm_10,e_oc,e_sulf,e_bc
      config % species % p_e_iso = 1
      config % species % p_e_so2 = 2
      config % species % p_e_no = 3
      config % species % p_e_no2 = 4
      config % species % p_e_co = 5
      config % species % p_e_eth = 6
      config % species % p_e_hc3 = 7
      config % species % p_e_hc5 = 8
      config % species % p_e_hc8 = 9
      config % species % p_e_xyl = 10
      config % species % p_e_olt = 11
      config % species % p_e_oli = 12
      config % species % p_e_tol = 13
      config % species % p_e_csl = 14
      config % species % p_e_hcho = 15
      config % species % p_e_ald = 16
      config % species % p_e_ket = 17
      config % species % p_e_ora2 = 18
      config % species % p_e_nh3 = 19
      config % species % p_e_pm_25 = 20
      config % species % p_e_pm_10 = 21
      config % species % p_e_oc = 22
      config % species % p_e_sulf = 23
      config % species % p_e_bc = 24
      config % species % p_e_dms = 25

! biomass burning
      config % species % p_ebu_iso = 1
      config % species % p_ebu_so2 = 2
      config % species % p_ebu_no = 3
      config % species % p_ebu_no2=4
      config % species % p_ebu_co =5
      config % species % p_ebu_eth=6
      config % species % p_ebu_hc3=7
      config % species % p_ebu_hc5=8
      config % species % p_ebu_hc8=9
      config % species % p_ebu_xyl=10
      config % species % p_ebu_olt=11
      config % species % p_ebu_oli=12
      config % species % p_ebu_tol=13
      config % species % p_ebu_csl=14
      config % species % p_ebu_hcho=15
      config % species % p_ebu_ald=16
      config % species % p_ebu_ket=17
      config % species % p_ebu_ora2=18
      config % species % p_ebu_nh3=19
      config % species % p_ebu_pm25=20
      config % species % p_ebu_pm10=21
      config % species % p_ebu_oc=22
      config % species % p_ebu_sulf=23
      config % species % p_ebu_bc=24
      config % species % p_ebu_dms=25

      config % species % p_ebu_in_iso = 1
      config % species % p_ebu_in_so2 = 2
      config % species % p_ebu_in_no = 3
      config % species % p_ebu_in_no2=4
      config % species % p_ebu_in_co =5
      config % species % p_ebu_in_eth=6
      config % species % p_ebu_in_hc3=7
      config % species % p_ebu_in_hc5=8
      config % species % p_ebu_in_hc8=9
      config % species % p_ebu_in_xyl=10
      config % species % p_ebu_in_olt=11
      config % species % p_ebu_in_oli=12
      config % species % p_ebu_in_tol=13
      config % species % p_ebu_in_csl=14
      config % species % p_ebu_in_hcho=15
      config % species % p_ebu_in_ald=16
      config % species % p_ebu_in_ket=17
      config % species % p_ebu_in_ora2=18
      config % species % p_ebu_in_nh3=19
      config % species % p_ebu_in_pm25=20
      config % species % p_ebu_in_pm10=21
      config % species % p_ebu_in_oc=22
      config % species % p_ebu_in_sulf=23
      config % species % p_ebu_in_bc=24
      config % species % p_ebu_in_dms=25
!dust and seasalt
! diagnostic dust and seasale stuff
      config % species % p_edust1=1
      config % species % p_edust2=2
      config % species % p_edust3=3
      config % species % p_edust4=4
      config % species % p_edust5=5
      config % species % p_eseas1=1
      config % species % p_eseas2=2
      config % species % p_eseas3=3
      config % species % p_eseas4=4

    end subroutine racmsoavbs_pointers_init

  end subroutine aqm_config_species_init

end module aqm_config_mod
