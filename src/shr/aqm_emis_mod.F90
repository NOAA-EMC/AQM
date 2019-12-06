module aqm_emis_mod

  use aqm_rc_mod
  use aqm_types_mod
  use aqm_io_mod
  use aqm_comm_mod
  use aqm_model_mod
  use aqm_config_mod

  implicit none

  integer, parameter :: aqm_emis_num = 36

  character(len=16), dimension(aqm_emis_num, 3), parameter :: &
    aqm_emis_def = reshape((/ &
      'AACD       ', 'MOL/S      ', 'e_aacd.dat ',  &
      'ACET       ', 'MOL/S      ', 'e_acet.dat ',  &
      'ACROLEIN   ', 'MOL/S      ', 'e_acro.dat ',  &
      'AECI       ', 'G/S        ', 'e_aeci.dat ',  &
      'AECJ       ', 'G/S        ', 'e_aecj.dat ',  &
      'ALD2       ', 'MOL/S      ', 'e_ald2.dat ',  &
      'ALDX       ', 'MOL/S      ', 'e_aldx.dat ',  &
      'AORGCJ     ', 'MOL/S      ', 'e_aorg.dat ',  & !!! wrong name
      'BENZ       ', 'MOL/S      ', 'e_benz.dat ',  &
      'BUTADIENE13', 'MOL/S      ', 'e_buta.dat ',  &
      'CH4        ', 'MOL/S      ', 'e_ch4.dat  ',  &
      'CO         ', 'MOL/S      ', 'e_co.dat   ',  &
      'ETHA       ', 'MOL/S      ', 'e_etha.dat ',  &
      'ETH        ', 'MOL/S      ', 'e_eth.dat  ',  &
      'ETHY       ', 'MOL/S      ', 'e_ethy.dat ',  &
      'ETOH       ', 'MOL/S      ', 'e_etoh.dat ',  &
      'FACD       ', 'MOL/S      ', 'e_facd.dat ',  &
      'FORM       ', 'MOL/S      ', 'e_form.dat ',  &
      'GLYXL      ', 'MOL/S      ', 'e_glyx.dat ',  &
      'HONO       ', 'MOL/S      ', 'e_hono.dat ',  &
      'IOLE       ', 'MOL/S      ', 'e_iole.dat ',  &
      'KET        ', 'MOL/S      ', 'e_ket.dat  ',  &
      'NAPH       ', 'MOL/S      ', 'e_naph.dat ',  &
      'MEOH       ', 'MOL/S      ', 'e_meoh.dat ',  &
      'NH3        ', 'MOL/S      ', 'e_nh3.dat  ',  &
      'NO2        ', 'MOL/S      ', 'e_no2.dat  ',  &
      'NO         ', 'MOL/S      ', 'e_no.dat   ',  &
      'OLE        ', 'MOL/S      ', 'e_ole.dat  ',  &
      'PACD       ', 'MOL/S      ', 'e_pacd.dat ',  &
      'PAR        ', 'MOL/S      ', 'e_par.dat  ',  &
      'PRPA       ', 'MOL/S      ', 'e_prpa.dat ',  &
      'SO2        ', 'MOL/S      ', 'e_so2.dat  ',  &
      'SULF       ', 'MOL/S      ', 'e_sulf.dat ',  &
      'TOL        ', 'MOL/S      ', 'e_tol.dat  ',  &
      'TOLU       ', 'MOL/S      ', 'e_tolu.dat ',  &
      'XYLMN      ', 'MOL/S      ', 'e_xyl.dat  '   &
    /), (/aqm_emis_num, 3/), order=(/2, 1/))

  integer, parameter :: aqm_emis_sup_num = 35
  character(len=16), dimension(aqm_emis_sup_num, 2), parameter :: &
    aqm_emis_sup_def = reshape((/ &
      ! -- gas-phase emissions
      'ISOP        ', 'MOL/S       ', &
      'TERP        ', 'MOL/S       ', &
      'CL2         ', 'MOL/S       ', &
      'HCL         ', 'MOL/S       ', &
      'SESQ        ', 'MOL/S       ', &
      'SOAALK      ', 'MOL/S       ', &
      'FORM_PRIMARY', 'MOL/S       ', &
      'ALD2_PRIMARY', 'MOL/S       ', &
      'HGNRVA      ', 'MOL/S       ', &
      'HGIIGAS     ', 'MOL/S       ', &
      ! -- point-source emissions
      'PSO4        ', 'KG/HR       ', &
      'PNO3        ', 'KG/HR       ', &
      'PCL         ', 'KG/HR       ', &
      'PNH4        ', 'KG/HR       ', &
      'PNA         ', 'KG/HR       ', &
      'PMG         ', 'KG/HR       ', &
      'PK          ', 'KG/HR       ', &
      'PCA         ', 'KG/HR       ', &
      'PEC         ', 'KG/HR       ', &
      'PFE         ', 'KG/HR       ', &
      'PAL         ', 'KG/HR       ', &
      'PSI         ', 'KG/HR       ', &
      'PTI         ', 'KG/HR       ', &
      'PMN         ', 'KG/HR       ', &
      'PH2O        ', 'KG/HR       ', &
      'PMOTHR      ', 'KG/HR       ', &
      'PMC         ', 'KG/HR       ', &
      'POC         ', 'KG/HR       ', &
      ! -- aerosols
      'ASO4J       ', 'KG/HR       ', &
      'ANH4J       ', 'KG/HR       ', &
      'ANO3K       ', 'KG/HR       ', &
      'ACLJ        ', 'KG/HR       ', &
      'AH2OJ       ', 'KG/HR       ', &
      'ATOL1J      ', 'KG/HR       ', &
      'ATOL2J      ', 'KG/HR       '  &
    /), (/aqm_emis_sup_num, 2/), order=(/2, 1/))

  logical, dimension(aqm_emis_num) :: is_emis_read = .false.

  private

  public :: aqm_emis_num
  public :: aqm_emis_def
  public :: aqm_emis_sup_num
  public :: aqm_emis_sup_def
  public :: aqm_emis_read

contains

  subroutine aqm_emis_read(spcname, jdate, jtime, buffer, de, rc)
    character(len=*),  intent(in)  :: spcname
    integer,           intent(in)  :: jdate
    integer,           intent(in)  :: jtime
    real(AQM_KIND_R4), intent(out) :: buffer(:,:)
    integer, optional, intent(in)  :: de
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: localDe
    integer :: i, ispc, localpe, tile
    type(aqm_config_type), pointer :: config => null()

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    buffer = 0._AQM_KIND_R4

    ispc = 0
    do i = 1, aqm_emis_num
      if (trim(spcname) == trim(aqm_emis_def(i,1))) then
        ispc = i
        exit
      end if
    end do

    if (ispc > 0) then
      ! -- read in emissions
      call aqm_model_get(de=de, config=config, tile=tile, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      call aqm_io_read(aqm_emis_def(ispc, 3), buffer, &
        path=config % emi_inname, de=de, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
    end if

  end subroutine aqm_emis_read
    
end module aqm_emis_mod
