module aqm_internal_mod

  use ESMF

  implicit none

  type aqm_internal_rate_type
    real(ESMF_KIND_R4), dimension(:), pointer :: values => null()  ! Array of rate values
  end type aqm_internal_rate_type

  type aqm_internal_emis_type
    character(len=ESMF_MAXSTR)     :: name           ! Name of the emission source or dataset
    character(len=ESMF_MAXSTR)     :: type           ! Type of emission (e.g., area, point, mobile)
    character(len=ESMF_MAXSTR)     :: format         ! Data format (e.g., netCDF, ASCII)
    character(len=ESMF_MAXPATHLEN) :: path          ! Directory path to emission files
    character(len=ESMF_MAXSTR)     :: file           ! Base filename for emission data
    character(len=ESMF_MAXSTR)     :: frequency     ! Update frequency (e.g., hourly, daily)
    character(len=ESMF_MAXSTR)     :: logprefix     ! Prefix for log file names
    character(len=ESMF_MAXSTR)     :: plumerise     ! Plume rise model configuration
    character(len=ESMF_MAXSTR)     :: specfile       ! Species definition file
    character(len=ESMF_MAXSTR)     :: specprofile    ! Vertical profile specification for species
    character(len=ESMF_MAXSTR)     :: latname        ! Variable name for latitude in input files
    character(len=ESMF_MAXSTR)     :: lonname        ! Variable name for longitude in input files
    character(len=ESMF_MAXSTR)     :: stkdmname      ! Variable name for stack diameter
    character(len=ESMF_MAXSTR)     :: stkhtname      ! Variable name for stack height
    character(len=ESMF_MAXSTR)     :: stktkname      ! Variable name for stack temperature
    character(len=ESMF_MAXSTR)     :: stkvename      ! Variable name for stack velocity
    character(len=6)               :: period         ! Time period identifier (e.g., YYYYMM)
    logical                        :: gridded        ! Flag for gridded (true) vs point source (false) emissions
    logical                        :: sync           ! Flag to synchronize emission reads with model time
    logical                        :: verbose        ! Flag for verbose logging output
    real                           :: fires_surface_frac    ! Fraction of fire emissions at surface
    real                           :: fires_adjacent_frac   ! Fraction of fire emissions in adjacent cells
    real                           :: scalefactor    ! Scaling factor for emission values
    real                           :: topfraction    ! Fraction of emissions released at model top
    integer(ESMF_KIND_I4)          :: layers         ! Number of vertical layers for emission distribution
    integer                        :: count          ! Count of emission sources or records
    integer                        :: irec           ! Current record index for file reading
    integer                        :: iofmt          ! I/O format flag (e.g., binary, unformatted)
    character(len=ESMF_MAXSTR)     :: iomode         ! Mode for I/O operations (e.g., read, write)
    type(ESMF_GridComp)            :: IO             ! ESMF Grid Component for I/O handling
    type(ESMF_Alarm)               :: alarm          ! ESMF Alarm for scheduling emission updates
    character(len=ESMF_MAXSTR), dimension(:), pointer :: sources    => null()  ! Array of emission source names
    character(len=ESMF_MAXSTR), dimension(:), pointer :: species    => null()  ! Array of chemical species names
    character(len=ESMF_MAXSTR), dimension(:), pointer :: units      => null()  ! Array of units for emission values
    integer,                   dimension(:), pointer :: dens_flag   => null()  ! Flags for density-based calculations
    integer,                   dimension(:), pointer :: ip           => null()  ! I-index (longitude) for point sources
    integer,                   dimension(:), pointer :: jp           => null()  ! J-index (latitude) for point sources
    integer,                   dimension(:), pointer :: ijmap        => null()  ! Mapping indices for grid cells
    real(ESMF_KIND_R4),        dimension(:), pointer :: lat          => null()  ! Latitude values for sources
    real(ESMF_KIND_R4),        dimension(:), pointer :: lon          => null()  ! Longitude values for sources
    real(ESMF_KIND_R4),        dimension(:), pointer :: stkdm        => null()  ! Stack diameter values
    real(ESMF_KIND_R4),        dimension(:), pointer :: stkht        => null()  ! Stack height values
    real(ESMF_KIND_R4),        dimension(:), pointer :: stktk        => null()  ! Stack temperature values
    real(ESMF_KIND_R4),        dimension(:), pointer :: stkve        => null()  ! Stack velocity values
    real(ESMF_KIND_R4),        dimension(:), pointer :: factors      => null()  ! Scaling or conversion factors
    type(ESMF_Field),          dimension(:), pointer :: fields       => null()  ! Array of ESMF Fields for emissions
    type(aqm_internal_rate_type), dimension(:), pointer :: rates     => null()  ! Array of emission rate data structures
    character(len=ESMF_MAXSTR), dimension(:,:), pointer :: table     => null()  ! Lookup table for emission parameters
  end type

  type aqm_internal_data_type
    type(aqm_internal_emis_type), pointer :: emis(:) => null()  ! Array of emission configurations
  end type

  type aqm_internal_state_type
    type(aqm_internal_data_type), pointer :: wrap => null()  ! Wrapper for internal data structures
  end type

  private

  public :: aqm_internal_data_type
  public :: aqm_internal_emis_type
  public :: aqm_internal_state_type

end module aqm_internal_mod
