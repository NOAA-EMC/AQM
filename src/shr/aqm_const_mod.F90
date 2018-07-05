module aqm_const_mod

  use aqm_types_mod

  implicit none

  real, parameter :: RGASUNIV   = 8.314510e0    ! universal gas constant [ J/mol-K ]
! real, parameter :: airmw      = 28.97
  real, parameter :: epsilc     = 1.e-16

  !..................................................................
  !       Sec. 1  Math and Physics Constants
  !..................................................................

  real, parameter    :: pi         = 3.14159265
  real(AQM_KIND_R8)  :: pi_r8      = 3.141592653559_AQM_KIND_R8
  real, parameter    :: degrad     = pi/180.
  real, parameter    :: raddeg     = 180./pi
  real, parameter    :: earthrad   = 6371220.      ! earth radius (m)
  real, parameter    :: omegx2     = 1.4584e-4     ! 2 x earth rotation rate (s^-1)
  real, parameter    :: g          = 9.80796       ! acceleration from earth gravity (m/(s^2))
  real, parameter    :: grvity     = 9.80665       ! acceleration from earth gravity (m/(s^2))
  real, parameter    :: grav98     = 9.8           ! gravity value for calc of geopotential (m/(s^2))
  real, parameter    :: p1000      = 100000.       ! p at 1000mb (pascals)
  real, parameter    :: cp         = 1004.6855     ! specific heat at const pres
  real, parameter    :: rd         = 287.0586      ! spec gas constant for dry air
  real, parameter    :: rv         = 461.50        ! gas constant for H2O
 
  real, parameter    :: qvmin         = 1.e-10
  real, parameter    :: qwmin         = 1.e-10
  real, parameter    :: ratio_h20_dry = 0.62197
  real, parameter    :: mwdry         = 28.
!  <mw>d is the molecular weight of dry air (28.966), <mw>w/<mw>d = 0.62197, and
!  (<mw>d - <mw>w)/<mw>d = 0.37803
!  http://atmos.nmsu.edu/education_and_outreach/encyclopedia/humidity.htm
  real, parameter     :: ROVRM1_P  = 0.6078
!  RVOVRM1_P         R  ND           RV/RD-1 = MD/MV-1
!                                    RVOVRM1 = 0.60778 (0.6078 usually used)
  REAL,     PARAMETER :: XLV       = 2.5E6

  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------  
  ! tracer info
  !-----------------------------------------------------------------------
  ! Tracer index:
  ! default initialization for all sulfur and carbon species is 0 (undefined)
  !     1. DMS       = Dimethyl sulfide            = CH3SCH3  
  !     2. SO2       = Sulfur dioxide              = SO2               
  !     3. SO4       = Sulfate                     = SO4            
  !     4. MSA       = Methane sulfonic acid       = CH3SO3H             
! INTEGER               :: NDMS=1, NSO2=2, NSO4=3, NMSA=4
  REAL,    PARAMETER :: airmw      = 28.97
  REAL,    PARAMETER :: mw_so2_aer = 64.066
  REAL,    PARAMETER :: mw_so4_aer = 96.066
  REAL,    PARAMETER :: smw        = 32.00
  REAL,    PARAMETER :: nh4_mfac = 1.375   ! increase sulf (output ond AOD only)
                                           ! to account for missing nh4
  REAL,    PARAMETER :: oc_mfac = 1.8      ! increase oc (output ond AOD nly)
                                           ! to account for Carbon to Organic ma
  public

end module aqm_const_mod
