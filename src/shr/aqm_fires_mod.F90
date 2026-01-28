!> @file aqm_fires_mod.f90
!> @brief Advanced Plume Rise and Vertical Distribution for AQM
!> @details Implements Sofiev (2012) buoyancy-driven plume rise with
!> empirical wind-shear suppression and Beta-distribution vertical mapping.
!> forest turbulence effects on dampening distribution
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

  !> @brief Main subroutine to calculate vertical fire emission profiles
  !> @param em Pointer to internal emission parameters
  !> @param frp Array of Fire Radiative Power (W) per grid cell
  !> @param profile Output 3D array of vertical emission weights
  !> @param rc Optional return code
  subroutine aqm_plume_sofiev(em, frp, profile, rc)

    type(aqm_internal_emis_type), pointer :: em
    real,                     intent(in)  :: frp(:)
    real,                     intent(out) :: profile(:,:,:)
    integer, optional,        intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: c, r, l, k, plm_idx
    integer :: lev0, lev1
    integer :: is, ie, js, je, nl, nx, ny
    real    :: Hp, pblh, th0, th1, dz, N2
    real    :: w, Hp_eff, avg_U, stab_penalty
    real    :: hgt_prev, layer_top, x_low, x_high, weight, total_sum
    real    :: frp_phys, model_top_m

    real(AQM_KIND_R8),    pointer :: phi(:)
    type(aqm_state_type), pointer :: state

    ! -- Advanced Physics Toggles
    logical, parameter :: use_beta_dist = .true.  !< Use Beta PDF instead of linear
    logical, parameter :: use_wind_adj  = .true.  !< Use wind-shear/stability adjustment

    ! -- Local Physical Parameters
    real, parameter :: rcp    = 2.0/7.0
    real, parameter :: p_ref  = 1.e+05
    real, parameter :: N2_ref = 2.5e-4 ! Reference N2 for stability scaling
    real, parameter :: U_ref  = 5.0    ! Reference wind speed (m/s)

    if (present(rc)) rc = AQM_RC_SUCCESS

    nullify(phi)
    nullify(state)

    profile = 0.0

    ! -- get model info
    call aqm_model_get(stateIn=state, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve model state", rc=rc)) return

    ! -- get domain info
    call aqm_model_domain_get(ids=is, ide=ie, jds=js, jde=je, nl=nl, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve grid coordinates", rc=rc)) return

    nx = ie - is + 1
    ny = je - js + 1

    ! -- total fraction to distribute vertically (1.0 - surface fraction)
    w = min( 1.0 - em % topfraction, 1.0 )

    k = 0
    do r = 1, ny
      do c = 1, nx
        k = k + 1
        phi => state % phil(c,r,:)
        pblh = state % hpbl(c,r)

        ! -- 1. Identify levels for stability calculation (approx 2x PBLH)
        lev0 = 1
        do l = 1, nl
          if (phi(l) * onebg >= 2.0 * pblh) then
            lev0 = l
            exit
          end if
        end do
        lev1 = min(lev0 + 1, nl)

        dz   = onebg * ( phi(lev1) - phi(lev0) )
        th0  = state % temp(c,r,lev0) * (p_ref / state % prl(c,r,lev0)) ** rcp
        th1  = state % temp(c,r,lev1) * (p_ref / state % prl(c,r,lev1)) ** rcp

        ! -- 2. Apply Physical Clamping to FRP (1MW to 100GW)
        frp_phys = max(1.0e6, min(frp(k), 1.0e11))

        ! -- 3. Compute buoyancy-driven plume rise (Sofiev 2012)
        ! Interface maintained: (PT1, PT2, laydepth, frp, pblh, Hp)
        call plumeRiseSofiev(th0, th1, dz, frp_phys, pblh, Hp)

        ! -- 4. Stability-Dependent Wind Entrainment Adjustment
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
          avg_U = sum(sqrt(state%u(c,r,1:plm_idx)**2 + state%v(c,r,1:plm_idx)**2)) / max(1.0, real(plm_idx))

          ! Penalty increases in stable environments (high N2)
          stab_penalty = 1.0 + (max(0.0, N2) / N2_ref)
          if (avg_U > 2.0) then
            Hp_eff = Hp * (U_ref / max(U_ref, avg_U))**(0.5 * stab_penalty)
          end if
        end if

        ! -- 5. Safety Check: Cap effective height at model top
        model_top_m = phi(nl) * onebg
        Hp_eff = min(Hp_eff, model_top_m - 10.0)

        ! -- 6. Vertical Mass Distribution (Beta PDF or Linear)
        hgt_prev = 0.0
        do l = 1, nl
          layer_top = min(phi(l) * onebg, Hp_eff)

          if (hgt_prev >= Hp_eff) exit

          x_low  = hgt_prev / Hp_eff
          x_high = layer_top / Hp_eff

          if (use_beta_dist) then
            ! Beta(3,2) Analytical Integral: 4x^3 - 3x^4
            ! Places peak injection at ~66% of plume height
            weight = (4.0*x_high**3 - 3.0*x_high**4) - (4.0*x_low**3 - 3.0*x_low**4)
          else
            ! Standard Linear/Uniform mapping
            weight = (layer_top - hgt_prev) / Hp_eff
          end if

          profile(c,r,l) = max(0.0, weight * w)
          hgt_prev = phi(l) * onebg
        end do

        ! -- 7. Final Renormalization for Mass Conservation
        total_sum = sum(profile(c,r,:))
        if (total_sum > 1.e-9) then
          profile(c,r,:) = profile(c,r,:) * (w / total_sum)
        end if

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
  subroutine plumeRiseSofiev(PT1, PT2, laydepth, frp, pblh, Hp)

      real, intent(in)  :: pblh, frp, PT1, PT2, laydepth
      real, intent(out) :: Hp

      real :: NFT_sq, alpha, beta, gama, delta
      real, parameter :: Pf0 = 1.e6, N0_sq = 0.00025

      ! Brunt-Vaisala frequency at the injection interface
      NFT_sq = grav / PT1 * abs(PT1 - PT2) / max(laydepth, 1.0)

      ! Initial guess (Sofiev Parameter Set 3)
      alpha = 0.15; beta = 102.0; gama = 0.49; delta = 0.0
      Hp = alpha*pblh + beta*(frp/Pf0)**gama * exp(-delta*NFT_sq/N0_sq)

      ! Refine based on initial guess vs PBL height
      if (Hp < pblh) then
        ! Case: Plume trapped in ABL
        alpha = 0.24; beta = 170.0; gama = 0.35; delta = 0.6
      else
        ! Case: Plume penetrates into Free Troposphere
        alpha = 0.93; beta = 298.0; gama = 0.13; delta = 0.7
      end if

      Hp = alpha*pblh + beta*(frp/Pf0)**gama * exp(-delta*NFT_sq/N0_sq)
      Hp = max(Hp, 10.0) ! Maintain a small minimum height

  end subroutine plumeRiseSofiev

end module aqm_fires_mod
