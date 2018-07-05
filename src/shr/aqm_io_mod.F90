module aqm_io_mod

  use mpi
  use aqm_rc_mod
  use aqm_types_mod
  use aqm_comm_mod
  use aqm_model_mod

  implicit none

  integer, parameter :: ioUnit = 100

  interface aqm_io_read
    module procedure aqm_io_read_2DR4
    module procedure aqm_io_read_3DR4
  end interface aqm_io_read

  interface aqm_io_write
    module procedure aqm_io_write_2DR4
    module procedure aqm_io_write_3DR4
    module procedure aqm_io_write_3DR8
  end interface aqm_io_write

  private

  public :: aqm_io_init
  public :: aqm_io_read
  public :: aqm_io_write

contains

  subroutine aqm_io_init(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: de, deCount, tile, tileCount
    integer :: pe, peCount
    integer :: i, localpe, npe
    integer :: comm, tileComm
    integer, dimension(:), allocatable :: localTile, tileToPet, pes

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(deCount=deCount, tileCount=tileCount, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- if no model on this PET, bail out
    if (deCount < 1) return

    call aqm_comm_get(comm=comm, localpe=localpe, pecount=peCount, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    allocate(localTile(tileCount), tileToPet(tileCount*peCount), stat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Unable to allocate memory", &
        file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- store which tiles are assigned to this PET
    localTile = -1
    do de = 0, deCount-1
      call aqm_model_get(de=de, tile=tile, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
      localTile(tile) = localpe
    end do

    ! -- build a global tile-to-PET map
    call aqm_comm_allgather(localTile, tileToPet, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    deallocate(localTile, stat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Unable to free memory", &
        file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- extract the list of PETs assigned to each tile and create MPI groups
    allocate(pes(peCount), stat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Unable to allocate memory", &
        file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- gather PET list for each tile and create tile-specific communicator
    pes = -1
    do tile = 1, tileCount
      npe = 0
      do i = tile, tileCount*peCount, tileCount
        if (tileToPet(i) > -1) then
          npe = npe + 1
          pes(npe) = tileToPet(i)
        end if
      end do

      ! -- create new communicator
      call aqm_comm_create(tileComm, pes(1:npe), rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      do de = 0, deCount-1
        call aqm_model_get(de=de, tile=i, rc=localrc)
        if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        if (tile == i) then
          call aqm_model_set(de=de, tileComm=tileComm, rc=localrc)
          if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
        end if
      end do
    end do

    deallocate(pes, tileToPet, stat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Unable to free memory", &
        file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- store I/O layout into model
    do de = 0, deCount-1
      call aqm_model_get(de=de, tile=tile, tileComm=tileComm, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
      ! -- get local PET for tile 
      call aqm_comm_inquire(tileComm, localpe=pe, pecount=npe, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
      ! -- mark local root PET as I/O PET
      call aqm_model_set(de=de, localIOflag=(pe == 0), rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return
      write(6,'("aqm_io_init: PET:",i2," DE:",i02," tile=",i0," - comm=",i0," PE:",i0,"/",i0)') &
        localpe, de, tile, tileComm, pe, npe
      flush(6)
    end do

  end subroutine aqm_io_init


  subroutine aqm_io_file_name(fullname, filename, tile, pathname)
    character(len=*),           intent(out) :: fullname
    character(len=*),           intent(in)  :: filename
    integer,                    intent(in)  :: tile
    character(len=*), optional, intent(in)  :: pathname

    ! -- local variables
    integer :: lstr
    character(len=AQM_MAXSTR) :: fname

    ! -- begin
    fname = ""
    fullname = ""

    lstr = len_trim(filename)
    if (lstr > 4) then
      print *, 'filename = ', filename(lstr-3:lstr)
      if (filename(lstr-3:lstr) == ".dat") then
        write(fname, '("tile",i0,"/",a)') tile, trim(filename)
      else
        write(fname, '(a,".tile",i0,".dat")') trim(filename), tile
      end if
    else
      write(fname, '(a,".tile",i0,".dat")') trim(filename), tile
    end if

    if (present(pathname)) then
      lstr = len_trim(pathname)
      if (pathname(lstr:lstr) == "/") then
        fullname = trim(pathname) // trim(fname)
      else
        fullname = trim(pathname) // "/" // trim(fname)
      end if
    else
      fullname = trim(fname)
    end if

  end subroutine aqm_io_file_name
  

  subroutine aqm_io_file_read(datafile, buffer, recrange, recsize, recstride, rc)
    character(len=*),   intent(in)  :: datafile
    real(AQM_KIND_R4), intent(out) :: buffer(:)
    integer, optional,  intent(in)  :: recrange(2)
    integer, optional,  intent(in)  :: recsize
    integer, optional,  intent(in)  :: recstride
    integer, optional,  intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: irec, is, ie
    integer :: rcount, rsize, rstride
    integer :: rrange(2)

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    buffer = 0._AQM_KIND_R4

    if (present(recrange)) then
      rrange = recrange
    else
      rrange = 1
    end if
    rcount = rrange(2) - rrange(1) + 1

    if (present(recsize)) then
      rsize = recsize
    else
      rsize = size(buffer) / rcount
    end if

    if (present(recstride)) then
      rstride = recstride
    else
      rstride = rsize
    end if

    if (aqm_rc_test((size(buffer) < rcount * max(rsize, rstride)), &
        msg="insufficient buffer size", &
        file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- open file
    open(unit=ioUnit, file=trim(datafile), form='unformatted', action='read', position='rewind', iostat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Failure opening file: "//trim(datafile), &
        file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- advance to first record
    do irec = 2, rrange(1)
      read(unit=ioUnit, iostat=localrc)
      if (aqm_rc_test((localrc /= 0), msg="Unable to locate record in file: "//trim(datafile), &
          file=__FILE__, line=__LINE__, rc=rc)) then
        close(unit=iounit)
        return
      end if
    end do

    ! -- read records
    is = 1
    ie = rsize
    do irec = 1, rcount
      read(unit=iounit, iostat=localrc) buffer(is:ie)
      if (aqm_rc_test((localrc /= 0), msg="Failure reading data from file: "//trim(datafile), &
          file=__FILE__, line=__LINE__, rc=rc)) then
        close(unit=iounit)
        return
      end if
      is = is + rstride
      ie = ie + rstride
    end do

    ! -- close file
    close(unit=ioUnit)

  end subroutine aqm_io_file_read


  subroutine aqm_io_file_write(datafile, buffer, pos, rc)
    character(len=*),           intent(in)  :: datafile
    real(AQM_KIND_R4),         intent(in)  :: buffer(:)
    character(len=*), optional, intent(in)  :: pos
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    character(len=6) :: filepos

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    filepos = 'rewind'
    if (present(pos)) then
      select case (trim(pos))
        case ('a', 'append')
          filepos = 'append'
        case default
          filepos = 'rewind'
      end select
    end if

    open(unit=ioUnit, file=trim(datafile), form='unformatted', action='write', &
      position=trim(filepos), iostat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Failure opening file: "//trim(datafile), &
        file=__FILE__, line=__LINE__, rc=rc)) return
    write(unit=ioUnit, iostat=localrc) buffer
    if (aqm_rc_test((localrc /= 0), msg="Failure reading data from file: "//trim(datafile), &
        file=__FILE__, line=__LINE__, rc=rc)) return
    close(unit=ioUnit)

  end subroutine aqm_io_file_write


  subroutine aqm_io_read_2DR4(filename, farray, path, recrange, recsize, recstride, de, rc)
    character(len=*),           intent(in)  :: filename
    real(AQM_KIND_R4),         intent(out) :: farray(:,:)
    character(len=*), optional, intent(in)  :: path
    integer,          optional, intent(in)  :: recrange(2)
    integer,          optional, intent(in)  :: recsize
    integer,          optional, intent(in)  :: recstride
    integer,          optional, intent(in)  :: de
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: tile, tileComm
    integer :: ids, ide, jds, jde, its, ite, jts, jte
    integer :: bsize(2)
    logical :: localIOflag
    character(len=AQM_MAXSTR) :: datafile
    real(AQM_KIND_R4), dimension(:), allocatable :: buffer
    real(AQM_KIND_R4), dimension(:,:), allocatable, target :: buf2d

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(de=de, tile=tile, tileComm=tileComm, &
      localIOflag=localIOflag, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    call aqm_model_domain_get(de=de, ids=ids, ide=ide, jds=jds, jde=jde, &
      its=its, ite=ite, jts=jts, jte=jte, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- check size consistency
    if (aqm_rc_test((size(farray) /= (ide-ids+1)*(jde-jds+1)), &
      msg="size of input array inconsistent with domain decomposition", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    bsize = (/ ite-its+1, jte-jts+1 /)
    allocate(buffer(bsize(1)*bsize(2)), stat=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Cannot allocate read buffer", &
      file=__FILE__, line=__LINE__, rc=rc)) return
    buffer = 0._AQM_KIND_R4

    if (localIOflag) then

      call aqm_io_file_name(datafile, filename, tile, pathname=path)

      call aqm_io_file_read(datafile, buffer, recrange=recrange, recsize=recsize, recstride=recstride, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      write(6,'("aqm_data_read: tile=",i2,2x,a," - min/max = "2g16.6)') tile, &
        trim(datafile), minval(buffer), maxval(buffer)
    end if

    call aqm_comm_bcast(buffer, comm=tileComm, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    allocate(buf2d(its:ite,jts:jte), stat=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Cannot allocate read buffer", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    buf2d = reshape(buffer, bsize)

    farray = buf2d(ids:ide, jds:jde) 
     
    deallocate(buffer, buf2d, stat=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Cannot deallocate read buffer", &
      file=__FILE__, line=__LINE__, rc=rc)) return

  end subroutine aqm_io_read_2DR4


  subroutine aqm_io_read_3DR4(filename, farray, path, recrange, recsize, recstride, de, rc)
    character(len=*),           intent(in)  :: filename
    real(AQM_KIND_R4),         intent(out) :: farray(:,:,:)
    character(len=*), optional, intent(in)  :: path
    integer,          optional, intent(in)  :: recrange(2)
    integer,          optional, intent(in)  :: recsize
    integer,          optional, intent(in)  :: recstride
    integer,          optional, intent(in)  :: de
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: tile, tileComm
    integer :: ids, ide, jds, jde, its, ite, jts, jte
    integer :: bsize(3)
    logical :: localIOflag
    character(len=AQM_MAXSTR) :: datafile
    real(AQM_KIND_R4), dimension(:),     allocatable :: buffer
    real(AQM_KIND_R4), dimension(:,:,:), allocatable, target :: buf3d

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(de=de, tile=tile, tileComm=tileComm, &
      localIOflag=localIOflag, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    call aqm_model_domain_get(de=de, ids=ids, ide=ide, jds=jds, jde=jde, &
      its=its, ite=ite, jts=jts, jte=jte, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    bsize = (/ ite-its+1, jte-jts+1, size(farray, dim=3) /)

    ! -- check size consistency
    if (aqm_rc_test((size(farray) /= (ide-ids+1)*(jde-jds+1)*bsize(3)), &
      msg="size of input array inconsistent with domain decomposition", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    allocate(buffer(bsize(1)*bsize(2)*bsize(3)), stat=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Cannot allocate read buffer", &
      file=__FILE__, line=__LINE__, rc=rc)) return
    buffer = 0._AQM_KIND_R4

    if (localIOflag) then

      call aqm_io_file_name(datafile, filename, tile, pathname=path)

      call aqm_io_file_read(datafile, buffer, recrange=recrange, recsize=recsize, recstride=recstride, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      write(6,'("aqm_data_read: tile=",i2,2x,a," - min/max = "2g16.6)') tile, &
        trim(datafile), minval(buffer), maxval(buffer)
    end if

    call aqm_comm_bcast(buffer, comm=tileComm, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    allocate(buf3d(its:ite,jts:jte,bsize(3)), stat=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Cannot allocate read buffer", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    buf3d = reshape(buffer, bsize)

    farray = buf3d(ids:ide, jds:jde, :)
     
    deallocate(buffer, buf3d, stat=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Cannot deallocate read buffer", &
      file=__FILE__, line=__LINE__, rc=rc)) return

  end subroutine aqm_io_read_3DR4


  subroutine aqm_io_write_2DR4(filename, farray, path, pos, de, rc)
    character(len=*),           intent(in)  :: filename
    real(AQM_KIND_R4),         intent(in)  :: farray(:,:)
    character(len=*), optional, intent(in)  :: path
    character(len=*), optional, intent(in)  :: pos
    integer,          optional, intent(in)  :: de
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: tile, tileComm
    integer :: ids, ide, jds, jde, its, ite, jts, jte
    logical :: localIOflag
    character(len=AQM_MAXSTR) :: datafile
    real(AQM_KIND_R4), dimension(:,:), allocatable, target :: buf2d, recvbuf

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(de=de, tile=tile, tileComm=tileComm, &
      localIOflag=localIOflag, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    call aqm_model_domain_get(de=de, ids=ids, ide=ide, jds=jds, jde=jde, &
      its=its, ite=ite, jts=jts, jte=jte, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- check size consistency
    if (aqm_rc_test((size(farray) /= (ide-ids+1)*(jde-jds+1)), &
      msg="size of input array inconsistent with domain decomposition", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    allocate(buf2d(its:ite,jts:jte), stat=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Cannot allocate read buffer", &
      file=__FILE__, line=__LINE__, rc=rc)) return
    buf2d = 0._AQM_KIND_R4

    buf2d(ids:ide, jds:jde) = farray

    allocate(recvbuf(its:ite,jts:jte), stat=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Cannot allocate read buffer", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    recvbuf = 0._AQM_KIND_R4

    call aqm_comm_reduce(buf2d, recvbuf, AQM_COMM_SUM, comm=tileComm, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    if (localIOflag) then

      call aqm_io_file_name(datafile, filename, tile, pathname=path)

      call aqm_io_file_write(datafile, reshape(recvbuf, (/size(buf2d)/)), &
        pos=pos, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      write(6,'("aqm_data_write: tile=",i2,2x,a," - min/max = "2g16.6)') tile, &
        trim(datafile), minval(recvbuf), maxval(recvbuf)
    end if

    deallocate(buf2d, recvbuf, stat=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Cannot deallocate read buffer", &
      file=__FILE__, line=__LINE__, rc=rc)) return

  end subroutine aqm_io_write_2DR4


  subroutine aqm_io_write_3DR4(filename, farray, order, path, pos, de, rc)
    character(len=*),           intent(in)  :: filename
    real(AQM_KIND_R4),         intent(in)  :: farray(:,:,:)
    character(len=*), optional, intent(in)  :: order
    character(len=*), optional, intent(in)  :: path
    character(len=*), optional, intent(in)  :: pos
    integer,          optional, intent(in)  :: de
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: tile, tileComm
    integer :: i, j, k, id, jd, lbuf
    integer :: ids, ide, jds, jde, its, ite, jts, jte, nk
    logical :: localIOflag
    character(len=3) :: localOrder
    character(len=AQM_MAXSTR) :: datafile
    real(AQM_KIND_R4), dimension(:),     allocatable :: recvbuf
    real(AQM_KIND_R4), dimension(:,:,:), allocatable :: buf3d

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(de=de, tile=tile, tileComm=tileComm, &
      localIOflag=localIOflag, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    call aqm_model_domain_get(de=de, ids=ids, ide=ide, jds=jds, jde=jde, &
      its=its, ite=ite, jts=jts, jte=jte, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    localOrder = "ijk"
    if (present(order)) localOrder = order

    select case (trim(localOrder))
      case("ikj")
        ! -- (i,k,j)
        nk = size(farray,dim=2)
      case default
        ! -- default to (i,j,k)
        nk = size(farray,dim=3)
    end select

    ! -- check consistency in decomposition
    if (aqm_rc_test((size(farray) /= (ide-ids+1)*(jde-jds+1)*nk), &
      msg="size of input array inconsistent with domain decomposition", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    allocate(buf3d(its:ite,jts:jte,nk), stat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Cannot allocate read buffer", &
        file=__FILE__, line=__LINE__, rc=rc)) return

    buf3d = 0._AQM_KIND_R4

    select case (trim(localOrder))
      case("ikj")
        do k = 1, nk
          j = 0
          do jd = jds, jde
            j = j + 1
            i = 0
            do id = ids, ide
              i = i + 1
              buf3d(id, jd, k) = farray(i, k, j)
            end do
          end do
        end do
      case default
        buf3d(ids:ide, jds:jde, 1:nk) = farray
    end select

    lbuf = (ite-its+1)*(jte-jts+1)*nk
    allocate(recvbuf(lbuf), stat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Cannot allocate read buffer", &
        file=__FILE__, line=__LINE__, rc=rc)) return

    recvbuf = 0._AQM_KIND_R4

    call aqm_comm_reduce(reshape(buf3d, (/lbuf/)), recvbuf, AQM_COMM_SUM, comm=tileComm, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    if (localIOflag) then

      call aqm_io_file_name(datafile, filename, tile, pathname=path)

      call aqm_io_file_write(datafile, recvbuf, pos=pos, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      write(6,'("aqm_io_write: tile=",i2,2x,a," - min/max = "2g16.6)') tile, &
        trim(datafile), minval(recvbuf), maxval(recvbuf)
    end if

    deallocate(buf3d, recvbuf, stat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Cannot deallocate read buffer", &
        file=__FILE__, line=__LINE__, rc=rc)) return

  end subroutine aqm_io_write_3DR4

  subroutine aqm_io_write_3DR8(filename, farray, order, path, pos, de, rc)
    character(len=*),           intent(in)  :: filename
    real(AQM_KIND_R8),         intent(in)  :: farray(:,:,:)
    character(len=*), optional, intent(in)  :: order
    character(len=*), optional, intent(in)  :: path
    character(len=*), optional, intent(in)  :: pos
    integer,          optional, intent(in)  :: de
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: tile, tileComm
    integer :: i, j, k, id, jd, lbuf
    integer :: ids, ide, jds, jde, its, ite, jts, jte, nk
    logical :: localIOflag
    character(len=3) :: localOrder
    character(len=AQM_MAXSTR) :: datafile
    real(AQM_KIND_R4), dimension(:),     allocatable :: recvbuf
    real(AQM_KIND_R4), dimension(:,:,:), allocatable :: buf3d

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    call aqm_model_get(de=de, tile=tile, tileComm=tileComm, &
      localIOflag=localIOflag, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    call aqm_model_domain_get(de=de, ids=ids, ide=ide, jds=jds, jde=jde, &
      its=its, ite=ite, jts=jts, jte=jte, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    localOrder = "ijk"
    if (present(order)) localOrder = order

    select case (trim(localOrder))
      case("ikj")
        ! -- (i,k,j)
        nk = size(farray,dim=2)
      case default
        ! -- default to (i,j,k)
        nk = size(farray,dim=3)
    end select

    ! -- check consistency in decomposition
    if (aqm_rc_test((size(farray) /= (ide-ids+1)*(jde-jds+1)*nk), &
      msg="size of input array inconsistent with domain decomposition", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    allocate(buf3d(its:ite,jts:jte,nk), stat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Cannot allocate read buffer", &
        file=__FILE__, line=__LINE__, rc=rc)) return

    buf3d = 0._AQM_KIND_R4

    select case (trim(localOrder))
      case("ikj")
        do k = 1, nk
          j = 0
          do jd = jds, jde
            j = j + 1
            i = 0
            do id = ids, ide
              i = i + 1
              buf3d(id, jd, k) = real(farray(i, k, j), kind=AQM_KIND_R4)
            end do
          end do
        end do
      case default
        buf3d(ids:ide, jds:jde, 1:nk) = real(farray, kind=AQM_KIND_R4)
    end select

    lbuf = (ite-its+1)*(jte-jts+1)*nk
    allocate(recvbuf(lbuf), stat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Cannot allocate read buffer", &
        file=__FILE__, line=__LINE__, rc=rc)) return

    recvbuf = 0._AQM_KIND_R4

    call aqm_comm_reduce(reshape(buf3d, (/lbuf/)), recvbuf, AQM_COMM_SUM, comm=tileComm, rc=localrc)
    if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

    if (localIOflag) then

      call aqm_io_file_name(datafile, filename, tile, pathname=path)

      call aqm_io_file_write(datafile, recvbuf, pos=pos, rc=localrc)
      if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__, rc=rc)) return

      write(6,'("aqm_io_write: tile=",i2,2x,a," - min/max = "2g16.6)') tile, &
        trim(datafile), minval(recvbuf), maxval(recvbuf)
    end if

    deallocate(buf3d, recvbuf, stat=localrc)
    if (aqm_rc_test((localrc /= 0), msg="Cannot deallocate read buffer", &
        file=__FILE__, line=__LINE__, rc=rc)) return

  end subroutine aqm_io_write_3DR8

end module aqm_io_mod
