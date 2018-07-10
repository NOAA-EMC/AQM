module cmaq_types_mod

  implicit none

  ! -- CMAQ return codes
  integer, parameter :: CMAQ_RC_SUCCESS = 0, &
                        CMAQ_RC_FAILURE = -1

  ! -- vdiff workspace
  type cmaq_vdiff_type
    LOGICAL :: FIRSTIME = .TRUE.
!   REAL, ALLOCATABLE :: FCJACMF( :,:,: )  ! 1/ mid-full layer vert Jac factor
    REAL, ALLOCATABLE :: CNGRD( :,:,:,: )    ! cgrid aero in mixing ratio
    REAL, ALLOCATABLE :: DDEP     ( :,:,: )   ! ddep accumulator
    REAL, ALLOCATABLE :: ICMP     ( :,:,: )   ! component flux accumlator
    REAL, allocatable :: WRDD( :,: )                 ! ddep write buffer
    REAL, ALLOCATABLE :: DDEP_PA  ( :,:,: )   ! ddep for process analysis
    REAL, ALLOCATABLE :: EMIS_PA( :,:,:,: )   ! emis for process analysis
    REAL, ALLOCATABLE :: EDDYV ( :,:,: )   ! from EDYINTB
    REAL, ALLOCATABLE :: SEDDY ( :,:,: )   ! flipped EDDYV
!   REAL, ALLOCATABLE :: VSED_AE( :,:,:,: )
  end type cmaq_vdiff_type

  ! -- MET
  

end module cmaq_types_mod
