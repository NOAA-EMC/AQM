! This module contains CMAQ work arrays needed
! to be exposed to the NUOPC interface
module cmaq_shr_mod

  use cmaq_types_mod

  implicit none

  ! -- rc flag
  integer :: cmaq_rc
  character(len=120) :: cmaq_xmsg = ''

  ! -- log unit
  integer :: cmaq_logdev

  ! -- vdiff
  type(cmaq_vdiff_type), pointer :: vdiffp    => null()

  public

end module cmaq_shr_mod
