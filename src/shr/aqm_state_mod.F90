module aqm_state_mod

  use aqm_types_mod

  implicit none

  type aqm_state_type

    real(AQM_KIND_R8), dimension(:,:),     pointer :: area     => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: cmm      => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: fice     => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: hfx      => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: hpbl     => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: ht       => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: lh       => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: psfc     => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: q2m      => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: rain     => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: rainc    => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: rgrnd    => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: rc       => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: slmsk    => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: sncov    => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: stype    => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: t2m      => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: tsfc     => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: ustar    => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: u10m     => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: v10m     => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: vfrac    => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: wr       => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: xlai     => null()
    real(AQM_KIND_R8), dimension(:,:),     pointer :: zorl     => null()

    real(AQM_KIND_R8), dimension(:,:,:),   pointer :: cldfl    => null()
    real(AQM_KIND_R8), dimension(:,:,:),   pointer :: dkt      => null()
    real(AQM_KIND_R8), dimension(:,:,:),   pointer :: phii     => null()
    real(AQM_KIND_R8), dimension(:,:,:),   pointer :: phil     => null()
    real(AQM_KIND_R8), dimension(:,:,:),   pointer :: pri      => null()
    real(AQM_KIND_R8), dimension(:,:,:),   pointer :: prl      => null()
    real(AQM_KIND_R8), dimension(:,:,:),   pointer :: smois    => null()
    real(AQM_KIND_R8), dimension(:,:,:),   pointer :: stemp    => null()
    real(AQM_KIND_R8), dimension(:,:,:),   pointer :: temp     => null()
    real(AQM_KIND_R8), dimension(:,:,:),   pointer :: uwind    => null()
    real(AQM_KIND_R8), dimension(:,:,:),   pointer :: vwind    => null()

    real(AQM_KIND_R8), dimension(:,:,:,:), pointer :: tr       => null()

    ! -- diagnostics
    real(AQM_KIND_R8), dimension(:,:),     pointer :: aod      => null()

  end type aqm_state_type

  public

end module aqm_state_mod
