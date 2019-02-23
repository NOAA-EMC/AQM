module aqm_domain_mod

  use aqm_types_mod

  implicit none

  type aqm_domain_type
    integer :: tile = 0
    integer :: tileCount = 0
    ! -- DE bounds
    integer :: ids   = 0
    integer :: ide   = 0
    integer :: jds   = 0
    integer :: jde   = 0
    ! -- memory bounds
    integer :: ims  = 0
    integer :: ime  = 0
    integer :: jms  = 0
    integer :: jme  = 0
    ! -- tile boundaries
    integer :: its  = 0
    integer :: ite  = 0
    integer :: jts  = 0
    integer :: jte  = 0
    ! -- number of vertical levels (i: interface, l: model)
    integer :: ni   = 0
    integer :: nl   = 0
    integer :: ns   = 0
    integer :: nt   = 0
    real(AQM_KIND_R8), pointer :: lon(:,:)  => null()
    real(AQM_KIND_R8), pointer :: lat(:,:)  => null()
    real(AQM_KIND_R8), pointer :: area(:,:) => null()
  end type aqm_domain_type

  ! -- domain decomposition
  integer :: ids, ide, jds, jde, kds, kde
  integer :: ims, ime, jms, jme, kms, kme
  integer :: its, ite, jts, jte, kts, kte

  public

contains

  subroutine aqm_domain_get(domain, &
    ids, ide, jds, jde,  &
    its, ite, jts, jte,  &
    ims, ime, jms, jme,  &
    ni,  nl,  ns, nt,  tile)
    type(aqm_domain_type), intent(in)  :: domain
    integer, optional,     intent(out) :: ids, ide, jds, jde
    integer, optional,     intent(out) :: its, ite, jts, jte
    integer, optional,     intent(out) :: ims, ime, jms, jme
    integer, optional,     intent(out) :: ni, nl, ns, nt
    integer, optional,     intent(out) :: tile

    ! -- local variables

    ! -- begin
    if (present(ids))  ids  = domain % ids
    if (present(ide))  ide  = domain % ide
    if (present(jds))  jds  = domain % jds
    if (present(jde))  jde  = domain % jde
    if (present(its))  its  = domain % its
    if (present(ite))  ite  = domain % ite
    if (present(jts))  jts  = domain % jts
    if (present(jte))  jte  = domain % jte
    if (present(ims))  ims  = domain % ims
    if (present(ime))  ime  = domain % ime
    if (present(jms))  jms  = domain % jms
    if (present(jme))  jme  = domain % jme
    if (present(ni))   ni   = domain % ni
    if (present(nl))   nl   = domain % nl
    if (present(ns))   ns   = domain % ns
    if (present(nt))   nt   = domain % nt
    if (present(tile)) tile = domain % tile

  end subroutine aqm_domain_get

end module aqm_domain_mod
