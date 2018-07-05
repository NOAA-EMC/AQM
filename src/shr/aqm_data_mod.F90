module aqm_data_mod

  use aqm_rc_mod
  use aqm_types_mod

  implicit none

  type aqm_data_type
    ! -- input
    real(AQM_KIND_R4), dimension(:),     allocatable :: p_gocart          ! GOCART pressure levels
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: clayfrac          ! clay fraction (AFWA dust saqme)
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: dm0               ! dms reference emissions
    real(AQM_KIND_R4), dimension(:,:,:), allocatable :: emiss_ab          ! emissions for all available species
    real(AQM_KIND_R4), dimension(:,:,:), allocatable :: emiss_abu         ! emissions for all available species
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: emiss_ash_dt      ! ash emissions
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: emiss_ash_height  ! ash emissions
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: emiss_ash_mass    ! ash emissions
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: emiss_tr_dt      ! ash emissions
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: emiss_tr_height  ! ash emissions
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: emiss_tr_mass    ! ash emissions
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: ero1              ! dust erosion factor
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: ero2              ! dust erosion factor
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: ero3              ! dust erosion factor
    real(AQM_KIND_R4), dimension(:,:,:), allocatable :: h2o2_backgd       ! H2O2 background for GOCART
    real(AQM_KIND_R4), dimension(:,:,:), allocatable :: no3_backgd        ! NO3 background for GOCART
    real(AQM_KIND_R4), dimension(:,:,:), allocatable :: oh_backgd         ! OH background for GOCART
    real(AQM_KIND_R4), dimension(:,:,:), allocatable :: plumestuff        ! fire info
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: sandfrac          ! sand fraction (AFWA dust saqme)
    real(AQM_KIND_R4), dimension(:,:),   allocatable :: th_pvsrf
    ! -- output
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: emi_d1
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: emi_d2
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: emi_d3
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: emi_d4
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: emi_d5
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: intaer
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: intbc
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: intoc
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: intsulf
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: intdust
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: intsea
    real(AQM_KIND_R4), dimension(:,:),     allocatable :: aod2d
    real(AQM_KIND_R4), dimension(:,:,:),   allocatable :: pm10
    real(AQM_KIND_R4), dimension(:,:,:),   allocatable :: pm25
    real(AQM_KIND_R4), dimension(:,:,:),   allocatable :: ebu_oc
    real(AQM_KIND_R4), dimension(:,:,:),   allocatable :: oh_bg
    real(AQM_KIND_R4), dimension(:,:,:),   allocatable :: h2o2_bg
    real(AQM_KIND_R4), dimension(:,:,:),   allocatable :: no3_bg
    real(AQM_KIND_R4), dimension(:,:,:),   allocatable :: wet_dep
    real(AQM_KIND_R4), dimension(:,:,:,:), allocatable :: ext_cof
    real(AQM_KIND_R4), dimension(:,:,:,:), allocatable :: sscal
    real(AQM_KIND_R4), dimension(:,:,:,:), allocatable :: asymp
    real(AQM_KIND_R4), dimension(:,:,:,:), allocatable :: tr3d
    real(AQM_KIND_R4), dimension(:,:,:,:), allocatable :: trdp
  end type aqm_data_type

  private

  public :: aqm_data_type
  public :: aqm_data_destroy

contains

  subroutine aqm_data_destroy(data, rc)
    type(aqm_data_type)           :: data
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    if (allocated(data % p_gocart)) then
      deallocate(data % p_gocart, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % clayfrac)) then
      deallocate(data % clayfrac, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % dm0)) then
      deallocate(data % dm0, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emiss_ab)) then
      deallocate(data % emiss_ab, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emiss_abu)) then
      deallocate(data % emiss_abu, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emiss_ash_dt)) then
      deallocate(data % emiss_ash_dt, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emiss_ash_height)) then
      deallocate(data % emiss_ash_height, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emiss_ash_mass)) then
      deallocate(data % emiss_ash_mass, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emiss_tr_dt)) then
      deallocate(data % emiss_tr_dt, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emiss_tr_height)) then
      deallocate(data % emiss_tr_height, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emiss_tr_mass)) then
      deallocate(data % emiss_tr_mass, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % ero1)) then
      deallocate(data % ero1, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % ero2)) then
      deallocate(data % ero2, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % ero3)) then
      deallocate(data % ero3, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % h2o2_backgd)) then
      deallocate(data % h2o2_backgd, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % no3_backgd)) then
      deallocate(data % no3_backgd, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % oh_backgd)) then
      deallocate(data % oh_backgd, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % plumestuff)) then
      deallocate(data % plumestuff, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % sandfrac)) then
      deallocate(data % sandfrac, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % th_pvsrf)) then
      deallocate(data % th_pvsrf, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emi_d1)) then
      deallocate(data % emi_d1, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emi_d2)) then
      deallocate(data % emi_d2, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emi_d3)) then
      deallocate(data % emi_d3, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emi_d4)) then
      deallocate(data % emi_d4, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % emi_d5)) then
      deallocate(data % emi_d5, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % intaer)) then
      deallocate(data % intaer, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % intbc)) then
      deallocate(data % intbc, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % intoc)) then
      deallocate(data % intoc, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % intsulf)) then
      deallocate(data % intsulf, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % intdust)) then
      deallocate(data % intdust, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % intsea)) then
      deallocate(data % intsea, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % aod2d)) then
      deallocate(data % aod2d, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % pm10)) then
      deallocate(data % pm10, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % pm25)) then
      deallocate(data % pm25, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % ebu_oc)) then
      deallocate(data % ebu_oc, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % oh_bg)) then
      deallocate(data % oh_bg, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % h2o2_bg)) then
      deallocate(data % h2o2_bg, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % no3_bg)) then
      deallocate(data % no3_bg, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % wet_dep)) then
      deallocate(data % wet_dep, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % ext_cof)) then
      deallocate(data % ext_cof, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % sscal)) then
      deallocate(data % sscal, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % asymp)) then
      deallocate(data % asymp, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % tr3d)) then
      deallocate(data % tr3d, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if
    if (allocated(data % trdp)) then
      deallocate(data % trdp, stat=localrc)
      if (aqm_rc_test((localrc /= 0), file=__FILE__, line=__LINE__, rc=rc)) return
    end if

  end subroutine aqm_data_destroy

end module aqm_data_mod
