module aqm_fires_mod

  use aqm_const_mod, only : grav, onebg
  use aqm_emis_mod
  use aqm_model_mod, only : aqm_model_get, aqm_model_domain_get
  use aqm_state_mod
  use aqm_rc_mod

  implicit none

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
    integer :: c, r, l, k
    integer :: lev0, lev1
    integer :: is, ie, js, je, nl, nx, ny
    real    :: Hp, pblh, th0, th1, dz
    real    :: tfrac, w
    real :: fixed_surface, remaining_w
    real :: dz_local, sigma, fixed_below, fixed_above, central_frac, gauss_sum, total_sum
    real    :: can_frac, weight_32, weight_153
    real    :: h_canopy, p_ratio, p_pen, Hp_adj, Hp_eff
    real    :: x_low, x_high, weight, hgt_prev, layer_top
    real    :: phi1, phi2, N2, avg_U, stab_penalty
    integer :: plm_idx
    real, dimension(:), allocatable :: gauss_weights
    real(AQM_KIND_R8) :: hbl, dist
    real(AQM_KIND_R8),    pointer :: phi(:)
    type(aqm_state_type), pointer :: state

    ! -- Advanced Physics Toggles
    logical, parameter :: use_beta_dist = .true.  !< Use Beta PDF instead of linear/Gaussian
    logical, parameter :: use_wind_adj  = .true.  !< Use wind-shear/stability adjustment

    ! -- local parameters
    real, parameter :: rcp = 2./7.
    real, parameter :: p_ref = 1.e+05
    real, parameter :: N2_ref = 2.5e-4 ! Reference N2 for stability scaling
    real, parameter :: U_ref  = 5.0    ! Reference wind speed (m/s)

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
        pblh = state % hpbl(c,r)

        ! -- 0. Extra forest canopy turbulence dampening (Heilman 2023)
        if (associated(state % cfrt)) then
          can_frac = max(0.0, min(1.0, real(state % cfrt(c,r))))
        else
          can_frac = 0.0
        end if

        hbl =  2 * grav * pblh
        lev0 = minloc(phi, 1, mask=phi >= hbl)
        if (lev0 == 0) then
          lev0 = nl
        end if
        if (aqm_rc_test((phi(lev0) < hbl .and. lev0 /= nl), &
          msg="Could not find first free-troposphere layer", &
          file=__FILE__, line=__LINE__,rc=rc)) return
        lev1 = min(lev0 + 1, nl)
        if (aqm_rc_test((lev1 > nl), &
          msg="Not enough vertical levels", &
          file=__FILE__, line=__LINE__,rc=rc)) return

        dz   = onebg * ( phi(lev1) - phi(lev0) )
        th0  = state % temp(c,r,lev0) * (p_ref / state % prl(c,r,lev0)) ** rcp
        th1  = state % temp(c,r,lev1) * (p_ref / state % prl(c,r,lev1)) ** rcp
        pblh = state % hpbl(c,r)

        ! -- call Sofiev's algorithm to compute height of plume top
        call plumeRiseSofiev(th0, th1, dz, frp(k), pblh, Hp)

        ! Safety Check: Maintain a minimum height
        Hp = max(Hp, 10.0)

        ! -- Stability-Dependent Wind Entrainment Adjustment
        Hp_eff = Hp
        if (use_wind_adj) then
          ! Calculate local Brunt-Vaisala frequency (N2)
          N2 = (grav / th0) * abs(th1 - th0) / max(dz, 1.0)

          ! Calculate column-average horizontal wind speed magnitude
          plm_idx = 1
          do l = 1, nl
            if (phi(l) * onebg >= Hp) exit
            plm_idx = l
          end do
          avg_U = sum(sqrt(state%uwind(c,r,1:plm_idx)**2 + state%vwind(c,r,1:plm_idx)**2)) / max(1.0, real(plm_idx))

          ! Penalty increases in stable environments (high N2)
          stab_penalty = 1.0 + (max(0.0, N2) / N2_ref)
          if (avg_U > 2.0) then
            Hp_eff = Hp * (U_ref / max(U_ref, avg_U))**(0.5 * stab_penalty)
          end if
        end if

        ! -- Canopy Penetration Adjustment (GEM-MACH style suppression/enhancement)
        if (associated(state%cfch) .and. associated(state%cfrt)) then
          if (state%cfch(c,r) > 0.0 .and. can_frac > 0.0) then
            h_canopy = real(state%cfch(c,r))
            if (Hp_eff > 0.0) then
              ! Penetration parameter P relative to canopy top H
              ! hs (source height) assumed 0 for surface fires
              p_ratio = h_canopy / Hp_eff
              p_pen = max(0.0, min(1.0, 1.5 - p_ratio))

              ! Adjusted rise height within/near canopy
              Hp_adj = (0.62 + 0.38 * p_pen) * h_canopy

              ! Final effective rise height blended by canopy fraction
              Hp_eff = (1.0 - can_frac) * Hp_eff + can_frac * Hp_adj
            end if
          end if
        end if

        if (use_beta_dist) then
          ! -- Vertical Mass Distribution (Beta PDF)
          hgt_prev = 0.0
          ! Safety clamp for division
          Hp_eff = max(Hp_eff, 10.0)
          do l = 1, nl
            layer_top = min(phi(l) * onebg, Hp_eff)
            if (hgt_prev >= Hp_eff) exit

            x_low  = hgt_prev / Hp_eff
            x_high = layer_top / Hp_eff

            ! Beta(3,2) Analytical Integral: 4x^3 - 3x^4
            weight_32 = (4.0*x_high**3 - 3.0*x_high**4) - (4.0*x_low**3 - 3.0*x_low**4)

            ! Heilman (2023) Beta(1.5, 3) Dampened Profile: 4.375x^1.5 - 5.25x^2.5 + 1.875x^3.5
            weight_153 = (4.375*x_high**1.5 - 5.25*x_high**2.5 + 1.875*x_high**3.5) - &
                         (4.375*x_low**1.5 - 5.25*x_low**2.5 + 1.875*x_low**3.5)

            ! Linearly weight the two profiles by canopy fraction
            weight = (1.0 - can_frac) * weight_32 + can_frac * weight_153

            profile(c,r,l) = max(0.0, weight * w)
            hgt_prev = phi(l) * onebg
          end do
        else
          ! -- distribute linearly/Gaussian between surface and plume top height
          lev0 = 1
          lev1 = maxloc(phi, 1, mask = phi <= grav * Hp_eff)
          if (lev1 == 0) lev1 = 1

          ! Allocate fires_surface_frac of emissions to surface layer
          fixed_surface = em % fires_surface_frac * w
          if (fixed_surface > w) fixed_surface = w
          profile(c,r,1) = fixed_surface
          remaining_w = w - fixed_surface

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
            ! Physical Gaussian using height distance in meters
            dist = real(abs(phi(l) * onebg - Hp_eff), AQM_KIND_R8)
            gauss_weights(l) = exp(-0.5 * (dist / max(sigma, 1.0))**2)
            gauss_sum = gauss_sum + gauss_weights(l)
          end do

          if (gauss_sum > 0.0) then
            do l = 1, nl
              gauss_weights(l) = (gauss_weights(l) / gauss_sum) * central_frac
            end do
          end if

          if (lev1 > 1) then
            profile(c,r,lev1-1) = profile(c,r,lev1-1) + fixed_below
          end if
          if (lev1 < nl) then
            profile(c,r,lev1+1) = profile(c,r,lev1+1) + fixed_above
          end if

          do l = 1, nl
            profile(c,r,l) = profile(c,r,l) + gauss_weights(l)
          end do
        end if

        ! Renormalize to ensure total sums to w
        total_sum = sum(profile(c,r,1:nl))
        if (abs(total_sum - w) > 1e-10) then
          profile(c,r,1:nl) = profile(c,r,1:nl) * (w / total_sum)
        end if

        ! Special case for single layer
        if (nl == 1 .and. lev1 == 1) then
          profile(c,r,1) = w
        end if

        ! Ensure non-negative profile values
        do l = 1, nl
          profile(c,r,l) = max(0.0, profile(c,r,l))
        end do

      end do
    end do

  end subroutine aqm_plume_sofiev


  !> @brief Implements the Sofiev plume rise algorithm
  !> @param PT1 Potential Temperature below PBL top
  !> @param PT2 Potential Temperature above PBL top
  !> @param laydepth Thickness of the layer at PBL top (m)
  !> @param frp Fire Radiative Power (W)
  !> @param pblh Planetary Boundary Layer height (m)
  !> @param Hp Output plume top height (m)
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
      Hp = max(Hp, 10.0) ! Maintain a small minimum height

  end subroutine plumeRiseSofiev

end module aqm_fires_mod
