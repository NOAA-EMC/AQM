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
    integer :: is, ie, js, je, nl, nx, ny
    integer :: c, r, l
    integer :: lev0, lev1
    real    :: Hp, pblh, th0, th1, dz
    real(AQM_KIND_R8) :: hbl
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
      file=__FILE__, line=__LINE__)) return

    ! -- get domain info
    call aqm_model_domain_get(ids=is, ide=ie, jds=js, jde=je, nl=nl, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failed to retrieve grid coordinates", &
      file=__FILE__, line=__LINE__)) return

    nx = ie - is + 1
    ny = je - js + 1

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
          file=__FILE__, line=__LINE__)) return
        lev1 = lev0 + 1
        if (aqm_rc_test((lev1 > nl), &
          msg="Not enough vertical levels", &
          file=__FILE__, line=__LINE__)) return

        dz   = onebg * ( phi(lev1) - phi(lev0) )
        th0  = state % temp(c,r,lev0) * (p_ref / state % prl(c,r,lev0)) ** rcp
        th1  = state % temp(c,r,lev1) * (p_ref / state % prl(c,r,lev1)) ** rcp
        pblh = state % hpbl(c,r)

        ! -- call Sofiev's algorithm to compute height of plume top
        call plumeRiseSofiev(th0, th1, dz, frp(k), pblh, Hp)

        ! -- distribute linearly between surface and plume top height
        lev0 = 1
        lev1 = maxloc(phi, 1, mask = phi <= grav * Hp)
         
        if (lev1 > lev0) then
          dz = 0.0
          do l = lev0, lev1
            profile(c,r,l) = ( phi(l) - dz ) / phi(lev1)
            dz = phi(l)
          end do
        else
          profile(c,r,lev0) = 1.0
        end if

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
