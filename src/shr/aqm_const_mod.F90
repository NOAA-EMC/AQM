module aqm_const_mod

  use aqm_types_mod

  implicit none

  ! --- basic mathematical constants

  real(AQM_KIND_R8), parameter :: zero = 0.0_AQM_KIND_R8
  real(AQM_KIND_R8), parameter :: one  = 1.0_AQM_KIND_R8
  real(AQM_KIND_R8), parameter :: half = 0.5_AQM_KIND_R8

  ! -- include CMAQ constants

  include SUBST_CONST

  real, parameter :: EPS1 = RWVAP/RDGAS - 1.

  ! -- include GFS constants

  ! molecular weight of dry air (g/mol)
  real(AQM_KIND_R8), parameter :: con_amd  = 28.9644_AQM_KIND_R8
  ! molecular weight of o3 (g/mol)
  real(AQM_KIND_R8), parameter :: con_amo3 = 47.9982_AQM_KIND_R8
  ! gravity (m/s2)
  real(AQM_KIND_R8), parameter :: con_g    =  9.80665e+0_AQM_KIND_R8
  ! inverse gravity (m/s2)
  real(AQM_KIND_R8), parameter :: onebg    =  one / con_g
  ! ozone conversion factor from kg/kg to ppmV
  real(AQM_KIND_R8), parameter :: con_mr2ppm_o3 = 1.e+06_AQM_KIND_R8 * con_amd / con_amo3

  ! -- thresholds

  ! approximate stratosphere lower boundary (Pa)
  real(AQM_KIND_R8), parameter :: thrs_p_strato = 1.e+04_AQM_KIND_R8

  public

end module aqm_const_mod
