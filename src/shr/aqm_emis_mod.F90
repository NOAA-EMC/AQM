module aqm_emis_mod

  use aqm_rc_mod
  use aqm_types_mod
  use aqm_io_mod
  use aqm_comm_mod
  use aqm_model_mod
  use aqm_config_mod

  implicit none

  integer, parameter :: aqm_emis_num = 62

  character(len=16), dimension(aqm_emis_num, 3), parameter :: &
    aqm_emis_def = reshape((/ &
      ! Emis. name     Units           File (empty if n/a)
      !-------------+---------------+---------------+
      ! -------------- gas chemistry ----------------
      'AACD        ', 'MOL/S       ', 'e_aacd.dat  ',  &
      'ACET        ', 'MOL/S       ', 'e_acet.dat  ',  &
      'ACROLEIN    ', 'MOL/S       ', 'e_acro.dat  ',  &
      'ALD2        ', 'MOL/S       ', 'e_ald2.dat  ',  &
      'ALD2_PRIMARY', 'MOL/S       ', '            ',  &
      'ALDX        ', 'MOL/S       ', 'e_aldx.dat  ',  &
      'BENZ        ', 'MOL/S       ', 'e_benz.dat  ',  &
      'BUTADIENE13 ', 'MOL/S       ', 'e_buta.dat  ',  &
      'CH4         ', 'MOL/S       ', 'e_ch4.dat   ',  &
      'CL2         ', 'MOL/S       ', '            ',  &
      'CO          ', 'MOL/S       ', 'e_co.dat    ',  &
      'ETH         ', 'MOL/S       ', 'e_eth.dat   ',  &
      'ETHA        ', 'MOL/S       ', 'e_etha.dat  ',  &
      'ETHY        ', 'MOL/S       ', 'e_ethy.dat  ',  &
      'ETOH        ', 'MOL/S       ', 'e_etoh.dat  ',  &
      'FACD        ', 'MOL/S       ', 'e_facd.dat  ',  &
      'FORM        ', 'MOL/S       ', 'e_form.dat  ',  &
      'FORM_PRIMARY', 'MOL/S       ', '            ',  &
      'GLYXL       ', 'MOL/S       ', 'e_glyx.dat  ',  &
      'HCL         ', 'MOL/S       ', '            ',  &
      'HGIIGAS     ', 'MOL/S       ', '            ',  &
      'HGNRVA      ', 'MOL/S       ', '            ',  &
      'HONO        ', 'MOL/S       ', 'e_hono.dat  ',  &
      'IOLE        ', 'MOL/S       ', 'e_iole.dat  ',  &
      'ISOP        ', 'MOL/S       ', '            ',  &
      'KET         ', 'MOL/S       ', 'e_ket.dat   ',  &
      'MEOH        ', 'MOL/S       ', 'e_meoh.dat  ',  &
      'NAPH        ', 'MOL/S       ', 'e_naph.dat  ',  &
      'NO          ', 'MOL/S       ', 'e_no.dat    ',  &
      'NO2         ', 'MOL/S       ', 'e_no2.dat   ',  &
      'OLE         ', 'MOL/S       ', 'e_ole.dat   ',  &
      'PACD        ', 'MOL/S       ', 'e_pacd.dat  ',  &
      'PAR         ', 'MOL/S       ', 'e_par.dat   ',  &
      'PRPA        ', 'MOL/S       ', 'e_prpa.dat  ',  &
      'SESQ        ', 'MOL/S       ', '            ',  &
      'SOAALK      ', 'MOL/S       ', '            ',  &
      'SO2         ', 'MOL/S       ', 'e_so2.dat   ',  &
      'SULF        ', 'MOL/S       ', 'e_sulf.dat  ',  &
      'TERP        ', 'MOL/S       ', '            ',  &
      'TOL         ', 'MOL/S       ', 'e_tol.dat   ',  &
      'TOLU        ', 'MOL/S       ', 'e_tolu.dat  ',  &
      'XYLMN       ', 'MOL/S       ', 'e_xyl.dat   ',  &
      ! -------------- point sources ----------------
      'PAL         ', 'G/S         ', '            ',  &
      'PCA         ', 'G/S         ', '            ',  &
      'PCL         ', 'G/S         ', '            ',  &
      'PEC         ', 'G/S         ', 'e_pec.dat   ',  &
      'PFE         ', 'G/S         ', '            ',  &
      'PH2O        ', 'G/S         ', '            ',  &
      'PK          ', 'G/S         ', '            ',  &
      'PMC         ', 'G/S         ', '            ',  &
      'PMG         ', 'G/S         ', '            ',  &
      'PMN         ', 'G/S         ', '            ',  &
      'PMOTHR      ', 'G/S         ', '            ',  &
      'PNA         ', 'G/S         ', '            ',  &
      'PNCOM       ', 'G/S         ', '            ',  &
      'PNH4        ', 'G/S         ', '            ',  &
      'PNO3        ', 'G/S         ', '            ',  &
      'POC         ', 'G/S         ', 'e_poc.dat   ',  &
      'PSI         ', 'G/S         ', '            ',  &
      'PSO4        ', 'G/S         ', '            ',  &
      'PTI         ', 'G/S         ', '            ',  &
      ! -------------- non reactive  ----------------
      'NH3         ', 'MOL/S       ', 'e_nh3.dat   '   &
    /), (/aqm_emis_num, 3/), order=(/2, 1/))

  logical, dimension(aqm_emis_num) :: is_emis_read = .false.

  private

  public :: aqm_emis_num
  public :: aqm_emis_def
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
        if (len_trim(aqm_emis_def(i,3)) > 0) ispc = i
        exit
      end if
    end do

    if (ispc > 0) then
      ! -- read in emissions
      call aqm_model_get(de=de, config=config, tile=tile, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      call aqm_io_read(aqm_emis_def(ispc,3), buffer, &
        path=config % emi_inname, de=de, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
    end if

  end subroutine aqm_emis_read
    
end module aqm_emis_mod
