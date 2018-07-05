module aqm_iolayout_mod

  use mpi

  implicit none

  type aqm_iolayout_type
    integer :: mpicomm     = MPI_COMM_NULL
    integer :: modelComm   = MPI_COMM_NULL
    integer :: tileComm    = MPI_COMM_NULL
    logical :: localIOflag = .false.
  end type aqm_iolayout_type

  private

  public :: aqm_iolayout_type

end module aqm_iolayout_mod
