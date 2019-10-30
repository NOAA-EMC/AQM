module aqm_bkgd_mod

  use aqm_rc_mod
  use aqm_types_mod
  use aqm_io_mod
  use aqm_model_mod
  use aqm_config_mod

  implicit none

  integer, parameter :: aqm_bkgd_num = 36
  character(len=16), dimension(aqm_bkgd_num, 2), parameter :: &
    aqm_bkgd_def = reshape((/ &
      'AACD       ', 'e_aacd.dat ',  &
      'ACET       ', 'e_acet.dat ',  &
      'ACROLEIN   ', 'e_acro.dat ',  &
      'AECI       ', 'e_aeci.dat ',  &
      'AECJ       ', 'e_aecj.dat ',  &
      'ALD2       ', 'e_ald2.dat ',  &
      'ALDX       ', 'e_aldx.dat ',  &
      'AORGCJ     ', 'e_aorg.dat ',  &
      'BENZ       ', 'e_benz.dat ',  &
      'BUTADIENE13', 'e_buta.dat ',  &
      'CH4        ', 'e_ch4.dat  ',  &
      'CO         ', 'e_co.dat   ',  &
      'ETHA       ', 'e_etha.dat ',  &
      'ETH        ', 'e_eth.dat  ',  &
      'ETHY       ', 'e_ethy.dat ',  &
      'ETOH       ', 'e_etoh.dat ',  &
      'FACD       ', 'e_facd.dat ',  &
      'FORM       ', 'e_form.dat ',  &
      'GLYXL      ', 'e_glyx.dat ',  &
      'HONO       ', 'e_hono.dat ',  &
      'IOLE       ', 'e_iole.dat ',  &
      'KET        ', 'e_ket.dat  ',  &
      'MEOH       ', 'e_meoh.dat ',  &
      'NAPH       ', 'e_naph.dat ',  &
      'NH3        ', 'e_nh3.dat  ',  &
      'NO2        ', 'e_no2.dat  ',  &
      'NO         ', 'e_no.dat   ',  &
      'OLE        ', 'e_ole.dat  ',  &
      'PACD       ', 'e_pacd.dat ',  &
      'PAR        ', 'e_par.dat  ',  &
      'PRPA       ', 'e_prpa.dat ',  &
      'SO2        ', 'e_so2.dat  ',  &
      'SULF       ', 'e_sulf.dat ',  &
      'TOL        ', 'e_tol.dat  ',  &
      'TOLU       ', 'e_tolu.dat ',  &
      'XYLMN      ', 'e_xyl.dat  '   &
    /), (/aqm_bkgd_num, 2/), order=(/2, 1/))

  private

  public :: aqm_bkgd_num
  public :: aqm_bkgd_def
  public :: aqm_bkgd_read

contains

  subroutine aqm_bkgd_read(spcname, buffer, de, rc)
    character(len=*),  intent(in)  :: spcname
    real(AQM_KIND_R4), intent(out) :: buffer(:,:)
    integer, optional, intent(in)  :: de
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: i, ispc
    type(aqm_config_type), pointer :: config => null()

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    buffer = 0._AQM_KIND_R4

    do i = 1, aqm_bkgd_num
      if (trim(spcname) == trim(aqm_bkgd_def(i,1))) then
        ! -- read in emissions
        call aqm_model_get(de=de, config=config, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

        call aqm_io_read(aqm_bkgd_def(i,2), buffer, &
          path=config % emi_inname, de=de, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        exit
      end if
    end do

  end subroutine aqm_bkgd_read
    
end module aqm_bkgd_mod
