module aqm_comm_mod

  use mpi
  use aqm_types_mod
  use aqm_rc_mod

  implicit none

  integer :: mpi_comm_aqm    = MPI_COMM_NULL
  integer :: mpi_group_aqm   = MPI_GROUP_NULL
  integer :: aqm_comm_rootpe = 0

  integer, parameter :: AQM_COMM_MAX     = MPI_MAX     ! return the maximum
  integer, parameter :: AQM_COMM_MIN     = MPI_MIN     ! return the minumum
  integer, parameter :: AQM_COMM_SUM     = MPI_SUM     ! return the sum
  integer, parameter :: AQM_COMM_PROD    = MPI_PROD    ! return the product
  integer, parameter :: AQM_COMM_LAND    = MPI_LAND    ! return the logical and
  integer, parameter :: AQM_COMM_BAND    = MPI_BAND    ! return the bitwise and
  integer, parameter :: AQM_COMM_LOR     = MPI_LOR     ! return the logical or
  integer, parameter :: AQM_COMM_BOR     = MPI_BOR     ! return the bitwise of
  integer, parameter :: AQM_COMM_LXOR    = MPI_LXOR    ! return the logical exclusive or
  integer, parameter :: AQM_COMM_BXOR    = MPI_BXOR    ! return the bitwise exclusive or
  integer, parameter :: AQM_COMM_MINLOC  = MPI_MINLOC  ! return the minimum and the location (actually, the value of
                                                        ! the second element of the structure where the minimum of
                                                        ! the first is found)
  integer, parameter :: AQM_COMM_MAXLOC  = MPI_MAXLOC  ! return the maximum and the location
  integer, parameter :: AQM_COMM_REPLACE = MPI_REPLACE ! replace b with a
  integer, parameter :: AQM_COMM_NO_OP   = MPI_NO_OP   ! perform no operation


  interface aqm_comm_bcast
    module procedure aqm_comm_bcast_i0
    module procedure aqm_comm_bcast_i1
    module procedure aqm_comm_bcast_r0
    module procedure aqm_comm_bcast_r1
    module procedure aqm_comm_bcast_s0
    module procedure aqm_comm_bcast_s1
  end interface aqm_comm_bcast

  interface aqm_comm_allgather
    module procedure aqm_comm_allgather_i1
  end interface aqm_comm_allgather

  interface aqm_comm_reduce
    module procedure aqm_comm_reduce_r1
    module procedure aqm_comm_reduce_r2
    module procedure aqm_comm_reduce_r3
  end interface aqm_comm_reduce

  interface aqm_comm_create
    module procedure aqm_comm_create_group
    module procedure aqm_comm_create_comm
  end interface aqm_comm_create

  private

  public :: aqm_comm_rootpe
  public :: AQM_RC_SUCCESS
  public :: AQM_RC_FAILURE
  public :: AQM_COMM_MAX
  public :: AQM_COMM_MIN
  public :: AQM_COMM_SUM
  public :: AQM_COMM_PROD
  public :: AQM_COMM_LAND
  public :: AQM_COMM_BAND
  public :: AQM_COMM_LOR
  public :: AQM_COMM_BOR
  public :: AQM_COMM_LXOR
  public :: AQM_COMM_BXOR
  public :: AQM_COMM_MINLOC
  public :: AQM_COMM_MAXLOC
  public :: AQM_COMM_REPLACE
  public :: AQM_COMM_NO_OP

  public :: aqm_comm_abort
  public :: aqm_comm_allgather
  public :: aqm_comm_bcast
  public :: aqm_comm_create
  public :: aqm_comm_get
  public :: aqm_comm_init
  public :: aqm_comm_inquire
  public :: aqm_comm_isroot
  public :: aqm_comm_log
  public :: aqm_comm_reduce
  public :: aqm_comm_set

contains

  subroutine aqm_comm_init(rc, comm, isolate)
    integer,           intent(out) :: rc
    integer, optional, intent(in)  :: comm
    logical, optional, intent(in)  :: isolate

    ! -- local variables
    integer :: ierr
    logical :: flag

    ! -- begin
    rc = AQM_RC_FAILURE

    call mpi_initialized(flag, ierr)
    if (.not.flag) then
      call mpi_init(ierr)
      if (ierr /= MPI_SUCCESS) return
    end if

    flag = .false.
    if (present(isolate)) flag = isolate
      
    if (present(comm)) then
      if (flag) then
        call mpi_comm_dup(comm, mpi_comm_aqm, ierr)
        if (ierr /= MPI_SUCCESS) return
      else
        mpi_comm_aqm = comm
      end if
    end if

    ! -- change MPI default error handler to return
    call mpi_errhandler_set(mpi_comm_aqm, MPI_ERRORS_RETURN, ierr)
    if (ierr /= MPI_SUCCESS) return

    ! -- get group handle
    call mpi_comm_group(mpi_comm_aqm, mpi_group_aqm, ierr)
    if (ierr /= MPI_SUCCESS) return

    rc = AQM_RC_SUCCESS
   
  end subroutine aqm_comm_init

  logical function aqm_comm_isroot()
    ! -- local variables
    integer :: ierr, rank
    ! -- begin
    aqm_comm_isroot = .false.
    call mpi_comm_rank(mpi_comm_aqm, rank, ierr)
    if (ierr /= MPI_SUCCESS) return
    
    aqm_comm_isroot = (rank == aqm_comm_rootpe)

  end function aqm_comm_isroot

  subroutine aqm_comm_get(localpe, pecount, comm, group, rc)
    integer, optional, intent(out) :: localpe
    integer, optional, intent(out) :: pecount
    integer, optional, intent(out) :: comm
    integer, optional, intent(out) :: group
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: ierr

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    if (present(localpe)) then
      localpe = -1
      call mpi_comm_rank(mpi_comm_aqm, localpe, ierr) 
      if (aqm_rc_test((ierr /= MPI_SUCCESS), &
        file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (present(pecount)) then
      pecount = -1
      call mpi_comm_size(mpi_comm_aqm, pecount, ierr) 
      if (aqm_rc_test((ierr /= MPI_SUCCESS), &
        file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (present(comm)) comm = mpi_comm_aqm

    if (present(group)) then
      call mpi_comm_group(mpi_comm_aqm, group, ierr)
      if (aqm_rc_test((ierr /= MPI_SUCCESS), &
        file=__FILE__, line=__LINE__, rc=rc)) return
    end if

  end subroutine aqm_comm_get

  subroutine aqm_comm_inquire(comm, localpe, pecount, rc)
    integer,           intent(in)  :: comm
    integer, optional, intent(out) :: localpe
    integer, optional, intent(out) :: pecount
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    if (present(localpe)) then
      localpe = -1
      call mpi_comm_rank(comm, localpe, localrc)
      if (aqm_rc_test((localrc /= MPI_SUCCESS), &
        file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (present(pecount)) then
      pecount = -1
      call mpi_comm_size(comm, pecount, localrc)
      if (aqm_rc_test((localrc /= MPI_SUCCESS), &
        file=__FILE__, line=__LINE__, rc=rc)) return
    end if

  end subroutine aqm_comm_inquire

  subroutine aqm_comm_set(comm, rc)
    integer, optional, intent(in)  :: comm
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    if (present(comm)) then
      mpi_comm_aqm = comm
      ! -- update group handle
      call mpi_comm_group(mpi_comm_aqm, mpi_group_aqm, localrc)
      if (aqm_rc_test((localrc /= MPI_SUCCESS), &
        file=__FILE__, line=__LINE__, rc=rc)) return
    end if

  end subroutine aqm_comm_set

  subroutine aqm_comm_log(msg, sync)
    character(len=*),  intent(in) :: msg
    logical, optional, intent(in) :: sync

    write(0,'("aqm_comm_log:",a)') trim(msg)
    if (present(sync)) then
      if (sync) call flush(0)
    end if

  end subroutine aqm_comm_log

  subroutine aqm_comm_abort(errcode, msg)
    integer,          optional, intent(in) :: errcode
    character(len=*), optional, intent(in) :: msg

    ! -- local variables
    integer :: ierr, localerrcode

    ! -- begin
    localerrcode = AQM_RC_FAILURE
    if (present(msg)) write(0,'("aqm_comm_abort:",a)') trim(msg)
    if (present(errcode)) localerrcode = errcode
    call mpi_abort(mpi_comm_aqm, localerrcode, ierr)

  end subroutine aqm_comm_abort

  ! -- allgather

  subroutine aqm_comm_allgather_i1(sendbuf, recvbuf, count, comm, rc)
    integer,           intent(in)    :: sendbuf(:)
    integer,           intent(inout) :: recvbuf(:)
    integer, optional, intent(in)    :: count
    integer, optional, intent(in)    :: comm
    integer, optional, intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: localcomm, localcount

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    localcomm = mpi_comm_aqm
    if (present(comm)) localcomm = comm

    if (present(count)) then
      if (count > size(sendbuf)) return
      localcount = count
    else
      localcount = size(sendbuf)
    end if

    call mpi_allgather(sendbuf, localcount, MPI_INTEGER, recvbuf, localcount, &
      MPI_INTEGER, localcomm, localrc)
    if (aqm_rc_test((localrc /= MPI_SUCCESS), &
      file=__FILE__, line=__LINE__, rc=rc)) return
    
  end subroutine aqm_comm_allgather_i1

  ! -- reduce

  subroutine aqm_comm_reduce_r1(sendbuf, recvbuf, op, rootpe, comm, rc)
    real(AQM_KIND_R4),  intent(in)    :: sendbuf(:)
    real(AQM_KIND_R4),  intent(inout) :: recvbuf(:)
    integer,            intent(in)    :: op
    integer, optional,  intent(in)    :: rootpe
    integer, optional,  intent(in)    :: comm
    integer, optional,  intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: localcomm, localroot

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    localcomm = mpi_comm_aqm
    if (present(comm)) localcomm = comm

    localroot = aqm_comm_rootpe
    if (present(rootpe)) localroot = rootpe

    call mpi_reduce(sendbuf, recvbuf, size(sendbuf), MPI_REAL, op, localroot, localcomm, localrc)
    if (aqm_rc_test((localrc /= MPI_SUCCESS), &
      file=__FILE__, line=__LINE__, rc=rc)) return

  end subroutine aqm_comm_reduce_r1

  subroutine aqm_comm_reduce_r2(sendbuf, recvbuf, op, rootpe, comm, rc)
    real(AQM_KIND_R4),  intent(in)    :: sendbuf(:,:)
    real(AQM_KIND_R4),  intent(inout) :: recvbuf(:,:)
    integer,            intent(in)    :: op
    integer, optional,  intent(in)    :: rootpe
    integer, optional,  intent(in)    :: comm
    integer, optional,  intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: localcomm, localroot

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    localcomm = mpi_comm_aqm
    if (present(comm)) localcomm = comm

    localroot = aqm_comm_rootpe
    if (present(rootpe)) localroot = rootpe

    call mpi_reduce(sendbuf, recvbuf, size(sendbuf), MPI_REAL, op, localroot, localcomm, localrc)
    if (aqm_rc_test((localrc /= MPI_SUCCESS), &
      file=__FILE__, line=__LINE__, rc=rc)) return

  end subroutine aqm_comm_reduce_r2

  subroutine aqm_comm_reduce_r3(sendbuf, recvbuf, op, rootpe, comm, rc)
    real(AQM_KIND_R4),  intent(in)    :: sendbuf(:,:,:)
    real(AQM_KIND_R4),  intent(inout) :: recvbuf(:,:,:)
    integer,            intent(in)    :: op
    integer, optional,  intent(in)    :: rootpe
    integer, optional,  intent(in)    :: comm
    integer, optional,  intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: localcomm, localroot

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    localcomm = mpi_comm_aqm
    if (present(comm)) localcomm = comm

    localroot = aqm_comm_rootpe
    if (present(rootpe)) localroot = rootpe

    call mpi_reduce(sendbuf, recvbuf, size(sendbuf), MPI_REAL, op, localroot, localcomm, localrc)
    if (aqm_rc_test((localrc /= MPI_SUCCESS), &
      file=__FILE__, line=__LINE__, rc=rc)) return

  end subroutine aqm_comm_reduce_r3

  ! -- communicators and groups

  subroutine aqm_comm_create_comm(newcomm, color, comm, rc)
    integer,           intent(out) :: newcomm
    integer,           intent(in)  :: color
    integer, optional, intent(in)  :: comm
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: localcomm

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    localcomm  = mpi_comm_aqm
    if (present(comm)) localcomm = comm

    call mpi_comm_split(localcomm, color, 0, newcomm, localrc)
    if (aqm_rc_test((localrc /= MPI_SUCCESS), &
      file=__FILE__, line=__LINE__, rc=rc)) return

  end subroutine aqm_comm_create_comm

  subroutine aqm_comm_create_group(newcomm, peList, comm, rc)
    integer,           intent(out) :: newcomm
    integer,           intent(in)  :: peList(:)
    integer, optional, intent(in)  :: comm
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: localcomm, localgroup, newgroup

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    localcomm  = mpi_comm_aqm
    localgroup = mpi_group_aqm
    if (present(comm)) then
      localcomm = comm
      call mpi_comm_group(localcomm, localgroup, localrc)
      if (aqm_rc_test((localrc /= MPI_SUCCESS), &
        file=__FILE__, line=__LINE__, rc=rc)) return
    end if

    call mpi_group_incl(localgroup, size(peList), peList, newgroup, localrc)
    if (aqm_rc_test((localrc /= MPI_SUCCESS), &
      file=__FILE__, line=__LINE__, rc=rc)) return

    call mpi_comm_create_group(localcomm, newgroup, 0, newcomm, localrc)
    if (aqm_rc_test((localrc /= MPI_SUCCESS), &
      file=__FILE__, line=__LINE__, rc=rc)) return
    
  end subroutine aqm_comm_create_group

  ! -- broadcast

  subroutine aqm_comm_bcast_i0(data, rootpe, comm, rc)
    integer,           intent(inout) :: data
    integer, optional, intent(in)    :: rootpe
    integer, optional, intent(in)    :: comm
    integer, optional, intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: buffer(1)

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS
    buffer(1) = data
    call aqm_comm_bcast_i1(buffer, rootpe=rootpe, comm=comm, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
    data = buffer(1)
    
  end subroutine aqm_comm_bcast_i0

  subroutine aqm_comm_bcast_i1(buffer, count, rootpe, comm, rc)
    integer,           intent(inout) :: buffer(:)
    integer, optional, intent(in)    :: count
    integer, optional, intent(in)    :: rootpe
    integer, optional, intent(in)    :: comm
    integer, optional, intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: localcomm, localcount, root

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    if (present(count)) then
      if (count > size(buffer)) return
      localcount = count
    else
      localcount = size(buffer)
    end if
    root = aqm_comm_rootpe
    if (present(rootpe)) root = rootpe
    localcomm = mpi_comm_aqm
    if (present(comm)) localcomm = comm
    call mpi_bcast(buffer, localcount, MPI_INTEGER, root, localcomm, localrc)
    if (aqm_rc_test((localrc /= MPI_SUCCESS), &
      file=__FILE__, line=__LINE__, rc=rc)) return
    
  end subroutine aqm_comm_bcast_i1

  subroutine aqm_comm_bcast_r0(data, rootpe, comm, rc)
    real(AQM_KIND_R4),  intent(inout) :: data
    integer, optional,  intent(in)    :: rootpe
    integer, optional,  intent(in)    :: comm
    integer, optional,  intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    real    :: buffer(1)

    ! -- begin
    buffer(1) = data
    call aqm_comm_bcast_r1(buffer, rootpe=rootpe, comm=comm, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
    
  end subroutine aqm_comm_bcast_r0

  subroutine aqm_comm_bcast_r1(buffer, count, rootpe, comm, rc)
    real(AQM_KIND_R4),  intent(inout) :: buffer(:)
    integer, optional,  intent(in)    :: count
    integer, optional,  intent(in)    :: rootpe
    integer, optional,  intent(in)    :: comm
    integer, optional,  intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: localcomm, localcount, root

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS
    if (present(count)) then
      if (count > size(buffer)) return
      localcount = count
    else
      localcount = size(buffer)
    end if
    root = aqm_comm_rootpe
    if (present(rootpe)) root = rootpe
    localcomm = mpi_comm_aqm
    if (present(comm)) localcomm = comm
    call mpi_bcast(buffer, localcount, MPI_REAL, root, localcomm, localrc)
    if (aqm_rc_test((localrc /= MPI_SUCCESS), &
      file=__FILE__, line=__LINE__, rc=rc)) return
    
  end subroutine aqm_comm_bcast_r1

  subroutine aqm_comm_bcast_s0(data, rootpe, comm, rc)
    character(len=*),  intent(inout) :: data
    integer, optional, intent(in)    :: rootpe
    integer, optional, intent(in)    :: comm
    integer, optional, intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: localcomm, root

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS
    root = aqm_comm_rootpe
    if (present(rootpe)) root = rootpe
    localcomm = mpi_comm_aqm
    if (present(comm)) localcomm = comm
    call mpi_bcast(data, len(data), MPI_CHARACTER, root, localcomm, localrc)
    if (aqm_rc_test((localrc /= MPI_SUCCESS), &
      file=__FILE__, line=__LINE__, rc=rc)) return
    
  end subroutine aqm_comm_bcast_s0

  subroutine aqm_comm_bcast_s1(buffer, count, rootpe, comm, rc)
    character(len=*),  intent(inout) :: buffer(:)
    integer, optional, intent(in)    :: count
    integer, optional, intent(in)    :: rootpe
    integer, optional, intent(in)    :: comm
    integer, optional, intent(out)   :: rc

    ! -- local variables
    integer :: localrc
    integer :: localcomm, localcount, root

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS
    if (present(count)) then
      if (count > size(buffer)) return
      localcount = count
    else
      localcount = size(buffer)
    end if
    root = aqm_comm_rootpe
    if (present(rootpe)) root = rootpe
    localcomm = mpi_comm_aqm
    if (present(comm)) localcomm = comm
    call mpi_bcast(buffer, localcount*len(buffer(1)), MPI_CHARACTER, root, localcomm, localrc)
    if (aqm_rc_test((localrc /= MPI_SUCCESS), &
      file=__FILE__, line=__LINE__, rc=rc)) return
    
  end subroutine aqm_comm_bcast_s1

end module aqm_comm_mod
