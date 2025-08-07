module aqm_tools_mod

  use aqm_types_mod, only : AQM_KIND_R4, &
                            AQM_KIND_R8

  implicit none

  private

  public :: aqm_units_conv

contains

! ---------------------------------------------------------------------
! unit conversion
! ---------------------------------------------------------------------

  real(AQM_KIND_R4) function aqm_units_conv(fromUnits, toUnits, molWeight, areaFlag)

    character(len=*),  intent(in) :: fromUnits
    character(len=*),  intent(in) :: toUnits
    real(AQM_KIND_R4), intent(in) :: molWeight
    integer,          intent(out) :: areaFlag

    ! -- local variables
    integer :: isrc, idst, psrc, pdst
    integer :: i, j
    character(len=len(fromUnits)) :: srcUnits
    character(len=len(toUnits))   :: dstUnits

    ! -- local parameters
    real(AQM_KIND_R4), parameter :: timeMatrix(2,2) = reshape( &
      (/    1._AQM_KIND_R4, 1._AQM_KIND_R4/3600._AQM_KIND_R4, &
         3600._AQM_KIND_R4, 1._AQM_KIND_R4                    /), (/2,2/))

    real(AQM_KIND_R4), parameter :: massMatrix(3,3) = reshape( &
       (/  1._AQM_KIND_R4,    -1._AQM_KIND_R4,    -1.E+03_AQM_KIND_R4, &
          -1._AQM_KIND_R4,     1._AQM_KIND_R4,     1.E+03_AQM_KIND_R4, &
          -1.E-03_AQM_KIND_R4, 1.E-03_AQM_KIND_R4, 1._AQM_KIND_R4     /), (/3,3/))

    character(len=*), parameter :: timeUnits(2) = (/ "/S ", "/HR" /)
    character(len=*), parameter :: massUnits(3) = (/ "MOL", "G  ", "KG " /)
    character(len=*), parameter :: areaUnits(1) = (/ "/M2" /)

    ! areaFlag has the following meaning:
    !   +1        to convert from src to dst, multiply by area
    !    0        src and dst are consistent, nothing to do
    !   -1        to convert from src to dst, divide by area
    areaFlag         = 0

    ! this function returns 0 if src units are invalid
    aqm_units_conv = 1._AQM_KIND_R4

    if (trim(fromUnits) == trim(toUnits)) return

    srcUnits = adjustl(fromUnits)
    dstUnits = adjustl(toUnits)

    psrc = 1
    pdst = 1
    call get_index(srcUnits, dstUnits, massUnits, isrc, idst, psrc, pdst)

    ! -- check mass
    if (isrc /= 0 .and. idst /= 0) then
      aqm_units_conv = aqm_units_conv * massMatrix(isrc,idst)
      if (aqm_units_conv < 0.) then
        if (isrc < idst) then
          aqm_units_conv = -aqm_units_conv * molWeight
        else if (isrc > idst) then
          aqm_units_conv = -aqm_units_conv / molWeight
        end if
      end if
    else
      aqm_units_conv = 0._AQM_KIND_R4
      return
    end if

    ! -- check area
    call get_index(srcUnits, dstUnits, areaUnits, isrc, idst, psrc, pdst)
    areaFlag = isrc - idst

    ! -- check time
    call get_index(srcUnits, dstUnits, timeUnits, isrc, idst, psrc, pdst)
    if (isrc /= 0 .and. idst /= 0) then
      aqm_units_conv = aqm_units_conv * timeMatrix(isrc,idst)
    else
      aqm_units_conv = 0._AQM_KIND_R4
      return
    end if

    ! -- check if src units include unparsed items (invalid)
    if (psrc <= len_trim(srcUnits)) then
      aqm_units_conv = 0._AQM_KIND_R4
      return
    end if

  end function aqm_units_conv

  subroutine get_index(src, dst, list, isrc, idst, psrc, pdst)
    character(len=*), intent(in)    :: src
    character(len=*), intent(in)    :: dst
    character(len=*), intent(in)    :: list(:)
    integer,          intent(out)   :: isrc, idst
    integer,          intent(inout) :: psrc, pdst

    integer :: l, n

    isrc = 0
    idst = 0
    do n = 1, size(list)
      l = len_trim(list(n))
      if (src(psrc:psrc + l - 1) == list(n)(1:l)) then
        isrc = n
        psrc = psrc + l
      end if
      if (dst(pdst:pdst + l - 1) == list(n)(1:l)) then
        idst = n
        pdst = pdst + l
      end if
    end do

  end subroutine get_index

end module aqm_tools_mod
