module aqm_internal_mod

  use ESMF

  implicit none

  type aqm_internal_emis_type
    character(len=ESMF_MAXSTR)     :: name
    character(len=ESMF_MAXSTR)     :: type
    character(len=ESMF_MAXSTR)     :: format
    character(len=ESMF_MAXPATHLEN) :: path
    character(len=ESMF_MAXSTR)     :: file
    character(len=ESMF_MAXSTR)     :: frequency
    character(len=ESMF_MAXSTR)     :: logprefix
    character(len=ESMF_MAXSTR)     :: plumerise
    character(len=ESMF_MAXSTR)     :: specfile
    character(len=ESMF_MAXSTR)     :: specprofile
    character(len=6)               :: period
    integer                        :: irec
    logical                        :: verbose
    type(ESMF_IOFmt_flag)          :: iofmt
    type(ESMF_GridComp)            :: IO
    type(ESMF_Alarm)               :: alarm
    character(len=ESMF_MAXSTR), dimension(:),   pointer :: sources   => null()
    character(len=ESMF_MAXSTR), dimension(:),   pointer :: species   => null()
    character(len=ESMF_MAXSTR), dimension(:),   pointer :: units     => null()
    integer,                    dimension(:),   pointer :: dens_flag => null()
    real(ESMF_KIND_R4),         dimension(:),   pointer :: factors   => null()
    type(ESMF_Field),           dimension(:),   pointer :: fields    => null()
    character(len=ESMF_MAXSTR), dimension(:,:), pointer :: table     => null()
  end type

  type aqm_internal_data_type
    type(aqm_internal_emis_type), pointer :: emis(:) => null()
  end type

  type aqm_internal_state_type
    type(aqm_internal_data_type), pointer :: wrap => null()
  end type

  private

  public :: aqm_internal_data_type
  public :: aqm_internal_emis_type
  public :: aqm_internal_state_type

end module aqm_internal_mod
