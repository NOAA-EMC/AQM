module aqm_fires_mod

  use aqm_const_mod, only : grav, onebg
  use aqm_emis_mod
  use aqm_model_mod, only : aqm_model_get, aqm_model_domain_get
  use aqm_state_mod
  use aqm_rc_mod

  private

  public :: aqm_plume_sofiev

contains

  subroutine aqm_plume_sofiev(em, frp, profile, rc)

    type(aqm_internal_emis_type), pointer :: em
    real,                     intent(in)  :: frp(:)
    real,                     intent(out) :: profile(:,:,:)
    integer, optional,        intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: c, r, l
    integer :: lev0, lev1
    integer :: is, ie, js, je, nl, nx, ny
    real    :: Hp, pblh, th0, th1, dz
    real    :: tfrac, w
    real :: fixed_surface, remaining_w
    real :: dz_local, sigma, fixed_below, fixed_above, central_frac, gauss_sum, total_sum
    real, dimension(:), allocatable :: gauss_weights
    real(AQM_KIND_R8) :: hbl, dist
    real(AQM_KIND_R8),    pointer :: phi(:)
    type(aqm_state_type), pointer :: state

    ! -- local parameters
    real, parameter :: rcp = 2./7.
    real, parameter :: p_ref = 1.e+05

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    nullify(phi)
    nullify(state)

    profile = 0.0

    ! -- get model info
    call aqm_model_get(stateIn=state, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model state", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- get domain info
    call aqm_model_domain_get(ids=is, ide=ie, jds=js, jde=je, nl=nl, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve grid coordinates", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    nx = ie - is + 1
    ny = je - js + 1

    allocate(gauss_weights(nl))

    ! -- compute layer empirical weights
    if (aqm_rc_test((em % topfraction > 1.0), &
      msg="Plume top fraction must be between 0.0 and 1.0", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    w = min( 1.0 - em % topfraction, 1.0 )

    ! -- select free-troposphere vertical level
    k = 0
    do r = 1, ny
      do c = 1, nx
        k = k + 1
        phi => state % phil(c,r,:)
        hbl =  2 * grav * state % hpbl(c,r)
        lev0 = minloc(phi, 1, mask=phi >= hbl)
        if (aqm_rc_test((phi(lev0) < hbl), &
          msg="Could not find first free-troposphere layer", &
          file=__FILE__, line=__LINE__,rc=rc)) return
        lev1 = lev0 + 1
        if (aqm_rc_test((lev1 > nl), &
          msg="Not enough vertical levels", &
          file=__FILE__, line=__LINE__,rc=rc)) return

        dz   = onebg * ( phi(lev1) - phi(lev0) )
        th0  = state % temp(c,r,lev0) * (p_ref / state % prl(c,r,lev0)) ** rcp
        th1  = state % temp(c,r,lev1) * (p_ref / state % prl(c,r,lev1)) ** rcp
        pblh = state % hpbl(c,r)

        ! -- call Sofiev's algorithm to compute height of plume top
        call plumeRiseSofiev(th0, th1, dz, frp(k), pblh, Hp)

        ! -- distribute linearly between surface and plume top height
        lev0 = 1
        lev1 = maxloc(phi, 1, mask = phi <= grav * Hp)

        ! Allocate fires_surface_frac of emissions to surface layer
        fixed_surface = em % fires_surface_frac * w
        if (fixed_surface > w) fixed_surface = w
        profile(c,r,1) = fixed_surface
        remaining_w = w - fixed_surface

        if (lev1 >= lev0) then         

          ! Gaussian distribution around plume height
          phi1 = phi(lev1)
          if (lev1 > 1) then
            phi2 = phi(lev1 - 1)
          else
            phi2 = phi(lev1)
          end if
          dz_local = onebg * (phi1 - phi2)
          if (lev1 == 1) then
            if (nl > 1) then
              dz_local = onebg * (phi(2) - phi(1))
            else
              dz_local = onebg * phi(1)
            end if
          else if (lev1 == nl) then
            dz_local = onebg * (phi(nl) - phi(nl-1))
          end if
      
          sigma = dz_local / 2.0

          fixed_below = 0.0
          fixed_above = 0.0
          central_frac = remaining_w

          if (lev1 > 1) then
            fixed_below = em % fires_adjacent_frac * remaining_w
            central_frac = central_frac - fixed_below
          end if
          if (lev1 < nl) then
            fixed_above = em % fires_adjacent_frac * remaining_w
            central_frac = central_frac - fixed_above
          end if
      
          gauss_sum = 0.0
          do l = 1, nl
            dist = real(l - lev1, AQM_KIND_R8)
            gauss_weights(l) = exp(-0.5 * (dist / sigma)**2)
            gauss_sum = gauss_sum + gauss_weights(l)
          end do
      
          if (gauss_sum > 0.0) then
            do l = 1, nl
              gauss_weights(l) = (gauss_weights(l) / gauss_sum) * central_frac
            end do
          end if
      
          if (lev1 > 1) then
            profile(c,r,lev1-1) = fixed_below
          end if
          if (lev1 < nl) then
            profile(c,r,lev1+1) = fixed_above
          end if
      
          do l = 1, nl
            profile(c,r,l) = profile(c,r,l) + gauss_weights(l)
          end do
      
          ! Renormalize to ensure total sums to w
          total_sum = sum(profile(c,r,1:nl))
          if (abs(total_sum - w) > 1e-10) then
            profile(c,r,1:nl) = profile(c,r,1:nl) * (w / total_sum)
          end if
       
          ! Special case for single layer
          if (nl == 1 .and. lev1 == 1) then
            profile(c,r,1) = w
          end if

        else
          profile(c,r,lev0) = w
        end if
       
        ! Ensure non-negative profile values
        do l = 1, nl
          profile(c,r,l) = max(0.0, profile(c,r,l))
        end do

      end do
    end do

  end subroutine aqm_plume_sofiev


  subroutine plumeRiseSofiev(PT1, PT2,laydepth,frp,pblh,Hp)

!  This subroutine implements the Sofiev plume rise algorithm
!  History: 09/16/2019: Prototype by Daniel Tong (DT)
!           10/15/2019: bug fix based on feedback from M. Sofiev, DT
!           11/2020: parameterization options, Yunyao Li (YL)
!
!  Ref: M. Sofiev et al., Evaluation of the smoke-injection
!    height from wild-land fires using remote sensing data.
!    Atmos. Chem. Phys., 12, 1995-2006, 2012.

      real, intent(in)  :: pblh         ! PBL height (m)
      real, intent(in)  :: frp          ! fire radiative power (W)
      real, intent(in)  :: PT1, PT2     ! Potential Temperature right below and above PBL height
      real, intent(in)  :: laydepth     ! depth of the layer at the PBL height
      real, intent(out) :: Hp           ! plume height (m)

      real NFT_sq       ! N square in Free Troposphere (@ z = 2pblh)
      real Pf0          ! reference fire power (W)
      real N0_sq        ! Brunt-Vaisala frequency (s-2)
      real alpha        ! part of ABL passed freely
      real beta         ! weights contribution of fire intensity
      real gama         ! power-law dependence on FRP
      real delta        ! dependence on stability in the FT

! ... Initial values.
! ... predefined values parameter set 3 to estimate whether hp higher
! than abl
      alpha     = 0.15
      beta      = 102
      gama      = 0.49
      delta     = 0

      Pf0       = 1000000.0
      N0_sq     = 0.00025

! ! ... calculate PT from T and P
!       PT1 = T1 * (1000/P1)**0.286
!       PT2 = T2 * (1000/P2)**0.286

! ... calculate Brunt-Vaisala frequency
      NFT_sq = grav/PT1*abs(PT1-PT2)/laydepth

! ... calculate first guess plume rise top height
      Hp = alpha*pblh + beta*(frp/Pf0)**gama * exp(-delta*NFT_sq/N0_sq)
! ... compare Hp with ABL
      if (Hp .lt. pblh) then
        alpha     = 0.24
        beta      = 170
        gama      = 0.35
        delta     = 0.6
        Hp = alpha*pblh + beta*(frp/Pf0)**gama*exp(-delta*NFT_sq/N0_sq)
      else
        alpha     = 0.93
        beta      = 298
        gama      = 0.13
        delta     = 0.7
        Hp = alpha*pblh + beta*(frp/Pf0)**gama*exp(-delta*NFT_sq/N0_sq)
      end if

  end subroutine plumeRiseSofiev

end module aqm_fires_mod
