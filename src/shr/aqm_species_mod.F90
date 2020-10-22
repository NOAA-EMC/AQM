module aqm_species_mod

  implicit none

  type aqm_species_type
    sequence
    ! -- hydrometeors in atmospheric microphysics scheme
    integer :: p_atm_qv      = 0    ! QV: water vapor mixing ratio (kg kg-1)
    integer :: p_atm_qc      = 0    ! QC: cloud water mixing ratio (kg kg-1)
    integer :: p_atm_qr      = 0    ! QR: rain water mixing ratio (kg kg-1)
    integer :: p_atm_qi      = 0    ! QI: ice mixing ratio (kg kg-1)
    integer :: p_atm_qs      = 0    ! QS: snow mixing ratio (kg kg-1)
    integer :: p_atm_qg      = 0    ! QG: graupel mixing ratio (kg kg-1)
    ! -- ozone
    integer :: p_atm_o3      = 0    ! O3: ozone mixing ratio (kg kg-1)
    ! -- CMAQ species
    integer :: p_aqm_beg     = 0    ! start index of CMAQ species
    ! -- CMAQ diagnostic tracers
    integer :: p_diag_beg    = 0    ! start index of CMAQ diagnostic species
    integer :: ndiag         = 0    ! number of CMAQ diagnostic tracers
  end type aqm_species_type

  private

  public :: aqm_species_type

end module aqm_species_mod
