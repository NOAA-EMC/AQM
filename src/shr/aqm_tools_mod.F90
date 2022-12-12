module aqm_tools_mod

  use aqm_types_mod, only : AQM_KIND_R4, &
                            AQM_KIND_R8

  implicit none

  private

  public :: aqm_units_conv
  public :: aqm_gridloc_get

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

! ---------------------------------------------------------------------------------
! grid mapping (spherical geometry)
! ---------------------------------------------------------------------------------
!
!  The following source code has been adapted from portions of NASA's MAPL library:
!
!  https://github.com/GEOS-ESM/MAPL
!
!  MAPL is licensed under the Apache license version 2.0. See notice below:
!
! ---------------------------------------------------------------------------------
!
!  NASA Docket No. GSC-15,354-1, and identified as "GEOS-5 GCM Modeling Software
!
!  Copyright (C) 2008 United States Government as represented by the Administrator
!  of the National Aeronautics and Space Administration. All Rights Reserved.
!
!  Licensed under the Apache License, Version 2.0 (the "License"); you may not use
!  this file except in compliance with the License. You may obtain a copy of the
!  License at
!
!  http://www.apache.org/licenses/LICENSE-2.0
!
!  Unless required by applicable law or agreed to in writing, software distributed
!  under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
!  CONDITIONS OF ANY KIND, either express or implied. See the License for the
!  specific language governing permissions and limitations under the License.
!
! ---------------------------------------------------------------------------------

 subroutine aqm_gridloc_get(center_lons,center_lats,corner_lons,corner_lats,lons,lats,ii,jj,ijcount,rc)
    real(AQM_KIND_R8), intent(in)  :: center_lats(:,:),center_lons(:,:)
    real(AQM_KIND_R8), intent(in)  :: corner_lats(:,:),corner_lons(:,:)
    real(AQM_KIND_R8), intent(in)  :: lons(:),lats(:)
    integer,           intent(out) :: ii(:),jj(:)
    integer,           intent(out) :: ijcount
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: npts,i,n,niter,im,jm,ilb,jlb,iub,jub,ifound,jfound
    integer :: lold,uold,lnew,unew
    logical :: in_region,in_sub_region

    ! -- begin
    ii = -1
    jj = -1
    ijcount = 0

    npts = size(lats)
    if (npts /= size(lons)) return
    if (npts /= size(ii)) return
    if (npts /= size(jj)) return

    im=size(corner_lons,1)-1
    jm=size(corner_lons,2)-1
    niter = max(im,jm)

    do i=1,npts
       ifound=-1
       jfound=-1
       ilb=1
       iub=im
       jlb=1
       jub=jm
       in_region = point_in_polygon([lons(i),lats(i)],[center_lons(ilb,jlb),center_lats(ilb,jlb)],  &
           [corner_lons(ilb,jlb),corner_lats(ilb,jlb)], &
           [corner_lons(iub+1,jlb),corner_lats(iub+1,jlb)], &
           [corner_lons(iub+1,jub+1),corner_lats(iub+1,jub+1)], &
           [corner_lons(ilb,jub+1),corner_lats(ilb,jub+1)])
       if (in_region) then
          ! bisect first dimension
          lnew=ilb
          unew=iub
          do n = 1,niter
             lold=lnew
             uold=unew
             unew=lold+(uold-lold)/2
             in_sub_region = point_in_polygon([lons(i),lats(i)], [center_lons(lnew,jlb),center_lats(lnew,jlb)], &
                 [corner_lons(lnew,jlb),corner_lats(lnew,jlb)], &
                 [corner_lons(unew+1,jlb),corner_lats(unew+1,jlb)], &
                 [corner_lons(unew+1,jub+1),corner_lats(unew+1,jub+1)], &
                 [corner_lons(lnew,jub+1),corner_lats(lnew,jub+1)])
             if (in_sub_region) then
               lnew=lold
               unew=unew
             else
               lnew=unew+1
               unew=uold
             end if
             if (unew==lnew) then
                ifound=unew
                exit
             end if
          enddo
          ! bisect 2nd dimension
          lnew=jlb
          unew=jub
          do n = 1,niter
             lold=lnew
             uold=unew
             unew=lold+(uold-lold)/2
             in_sub_region = point_in_polygon([lons(i),lats(i)], [center_lons(ifound,lnew),center_lats(ifound,lnew)] , &
                 [corner_lons(ifound,lnew),corner_lats(ifound,lnew)], &
                 [corner_lons(ifound+1,lnew),corner_lats(ifound+1,lnew)], &
                 [corner_lons(ifound+1,unew+1),corner_lats(ifound+1,unew+1)], &
                 [corner_lons(ifound,unew+1),corner_lats(ifound,unew+1)])
             if (in_sub_region) then
               lnew=lold
               unew=unew
             else
               lnew=unew+1
               unew=uold
             end if
             if (unew==lnew) then
                jfound=unew
                exit
             end if
          enddo
       end if
       ii(i)=ifound
       jj(i)=jfound
       if (ifound > 0 .and. jfound > 0) ijcount = ijcount + 1
    enddo

 end subroutine aqm_gridloc_get

 function point_in_polygon(p0,pinside,a1,a2,a3,a4) result(in_poly)
    real(AQM_KIND_R8), intent(in) :: p0(2),pinside(2),a1(2),a2(2),a3(2),a4(2)
    logical :: in_poly

    real(AQM_KIND_R8) :: p1c(3),p2c(3),a1c(3),a2c(3),a3c(3),a4c(3)
    logical :: intersect(4)
    p1c=convert_to_cart(p0)
    p2c=convert_to_cart(pinside)
    a1c=convert_to_cart(a1)
    a2c=convert_to_cart(a2)
    a3c=convert_to_cart(a3)
    a4c=convert_to_cart(a4)

    intersect(1) = lines_intersect(p1c,p2c,a1c,a2c)
    intersect(2) = lines_intersect(p1c,p2c,a2c,a3c)
    intersect(3) = lines_intersect(p1c,p2c,a3c,a4c)
    intersect(4) = lines_intersect(p1c,p2c,a4c,a1c)
    if (mod(count(intersect),2)==0) then
       in_poly=.true.
    else
       in_poly=.false.
    end if

 end function point_in_polygon

 function lines_intersect(b0,b1,a0,a1)  result(intersect)
    real(AQM_KIND_R8), intent(in) :: b0(3),b1(3),a0(3),a1(3)
    logical :: intersect
    real(AQM_KIND_R8) :: p(3),q(3),t(3)
    real(AQM_KIND_R8) :: s1,s2,s3,s4
    logical :: signs(4)

    intersect=.false.
    q=cross_prod(b0,b1)
    p=cross_prod(a0,a1)
    t=normal_vect(cross_prod(p,q))

    s1=dot_product(cross_prod(a0,p),t)
    s2=dot_product(cross_prod(a1,p),t)
    s3=dot_product(cross_prod(b0,q),t)
    s4=dot_product(cross_prod(b1,q),t)

    signs(1) = -s1 <0.d0
    signs(2) = s2 <0.d0
    signs(3) = -s3 < 0.d0
    signs(4) = s4 < 0.d0

    intersect = ((count(signs)==0) .or. (count(signs)==4))

 end function lines_intersect

 function normal_vect(vin) result(vout)
    real(AQM_KIND_R8), intent(in) :: vin(3)
    real(AQM_KIND_R8) :: vout(3)
    vout=vin/sqrt(vin(1)*vin(1)+vin(2)*vin(2)+vin(3)*vin(3))

 end function normal_vect

 function cross_prod(v1,v2) result(vout)
    real(AQM_KIND_R8), intent(in) :: v1(3),v2(3)
    real(AQM_KIND_R8) :: vout(3)
    vout(1)=v1(2)*v2(3)-v1(3)*v2(2)
    vout(2)=v1(3)*v2(1)-v1(1)*v2(3)
    vout(3)=v1(1)*v2(2)-v1(2)*v2(1)
 end function cross_prod

 function convert_to_cart(v) result(xyz)
    real(AQM_KIND_R8), intent(in) :: v(2)
    real(AQM_KIND_R8) :: xyz(3)

    xyz(1)=cos(v(2))*cos(v(1))
    xyz(2)=cos(v(2))*sin(v(1))
    xyz(3)=sin(v(2))

 end function convert_to_cart

 function vect_mag(v) result(mag)
    real(AQM_KIND_R8), intent(in) :: v(3)
    real :: mag
    mag = sqrt(v(1)*v(1)+v(2)*v(2)+v(3)*v(3))
 end function vect_mag

end module aqm_tools_mod
