module aqm_data_mod

  use aqm_rc_mod
  use aqm_types_mod
  use ASX_DATA_MOD, only : GRID_Type, MET_Type

! use cmaq_shr_mod

  implicit none

  type aqm_data_type
    ! -- CMAQ workspace
!   type(cmaq_vdiff_type) :: vdiff
    ! -- cmaq workspace
    type(GRID_Type) :: Grid_Data
    type(MET_Type)  :: Met_Data
    real(AQM_KIND_R4), dimension(:,:,:,:), allocatable :: cgrid
  end type aqm_data_type

  private

  public :: aqm_data_type
  public :: aqm_data_destroy

contains

  subroutine aqm_data_destroy(data, rc)
    type(aqm_data_type)           :: data
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    if (allocated(data % cgrid)) then
      deallocate(data % cgrid, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if

  end subroutine aqm_data_destroy

end module aqm_data_mod
