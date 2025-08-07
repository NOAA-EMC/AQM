module aqm_internal_mod

  use ESMF

  implicit none

  type aqm_internal_rate_type
    real(ESMF_KIND_R4), dimension(:), pointer :: values => null()
  end type aqm_internal_rate_type

  type aqm_internal_psrc_type
    integer :: nloc
    integer :: i
    integer :: j
  end type aqm_internal_psrc_type

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
    character(len=ESMF_MAXSTR)     :: latname
    character(len=ESMF_MAXSTR)     :: lonname
    character(len=ESMF_MAXSTR)     :: stkdmname
    character(len=ESMF_MAXSTR)     :: stkhtname
    character(len=ESMF_MAXSTR)     :: stktkname
    character(len=ESMF_MAXSTR)     :: stkvename
    character(len=6)               :: period
    logical                        :: gridded
    logical                        :: sync
    logical                        :: verbose
    real                           :: scalefactor
    real                           :: topfraction
    integer(ESMF_KIND_I4)          :: layers
    integer                        :: count
    integer                        :: irec
    integer                        :: iofmt
    character(len=ESMF_MAXSTR)     :: iomode
    type(ESMF_GridComp)            :: IO
    type(ESMF_Alarm)               :: alarm
    character(len=ESMF_MAXSTR), dimension(:),   pointer :: sources   => null()
    character(len=ESMF_MAXSTR), dimension(:),   pointer :: species   => null()
    character(len=ESMF_MAXSTR), dimension(:),   pointer :: units     => null()
    integer,                    dimension(:),   pointer :: dens_flag => null()
    real(ESMF_KIND_R4),         dimension(:),   pointer :: lat       => null()
    real(ESMF_KIND_R4),         dimension(:),   pointer :: lon       => null()
    real(ESMF_KIND_R4),         dimension(:),   pointer :: stkdm     => null()
    real(ESMF_KIND_R4),         dimension(:),   pointer :: stkht     => null()
    real(ESMF_KIND_R4),         dimension(:),   pointer :: stktk     => null()
    real(ESMF_KIND_R4),         dimension(:),   pointer :: stkve     => null()
    real(ESMF_KIND_R4),         dimension(:),   pointer :: factors   => null()
    type(ESMF_Field),           dimension(:),   pointer :: fields    => null()
    type(aqm_internal_psrc_type), dimension(:), pointer :: ijmap     => null()
    type(aqm_internal_rate_type), dimension(:), pointer :: rates     => null()
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
  public :: aqm_internal_psrc_type
  public :: aqm_internal_state_type

end module aqm_internal_mod
