       module coupler_module

         use mydata_module

         implicit none

! for 2d variable
         integer, parameter :: n2d_data   = 29

         integer, parameter :: lon_ind      =  1
         integer, parameter :: lat_ind      =  2
         integer, parameter :: lwmask_ind   =  3
         integer, parameter :: open_ind     =  4
         integer, parameter :: szone_ind    =  5
         integer, parameter :: surf_ind     =  6
         integer, parameter :: seaice_ind   =  7
         integer, parameter :: prsfc_ind    =  8
         integer, parameter :: snocov_ind   =  9
         integer, parameter :: temp2_ind    = 10
         integer, parameter :: tempg_ind    = 11
         integer, parameter :: ht_ind       = 12
         integer, parameter :: rmol_ind     = 13
         integer, parameter :: rs_ind       = 14     ! surface resistance
         integer, parameter :: ra_ind       = 15     ! aerodynamic resistance
         integer, parameter :: pbl_ind      = 16
         integer, parameter :: ustar_ind    = 17
         integer, parameter :: hfx_ind      = 18
         integer, parameter :: lh_ind       = 19
         integer, parameter :: canwat_ind   = 20 
         integer, parameter :: lai_ind      = 21
         integer, parameter :: vegpx_ind    = 22
         integer, parameter :: znt_ind      = 23
         integer, parameter :: q2_ind       = 24
         integer, parameter :: rainc_ind    = 25     ! time-step convective precipitation
         integer, parameter :: rainnc_ind   = 26     ! time-step total grid-scale precipitation
         integer, parameter :: cfrac2dr_ind = 27
         integer, parameter :: cfrac2dt_ind = 28
         integer, parameter :: cellArea_ind = 29     ! cell area, m**2

! for 3d variable
         integer, parameter :: n3d_data     = 17

         integer, parameter :: cfrac3d_ind    =  1
         integer, parameter :: cldfracwcu_ind =  2
         integer, parameter :: pres_ind       =  3
         integer, parameter :: zh_ind         =  4
         integer, parameter :: zf_ind         =  5
         integer, parameter :: dens_ind       =  6
         integer, parameter :: temp_ind       =  7
         integer, parameter :: qv_ind         =  8
         integer, parameter :: qc_ind         =  9
         integer, parameter :: qr_ind         = 10
         integer, parameter :: qg_ind         = 11
         integer, parameter :: qi_ind         = 12
         integer, parameter :: qs_ind         = 13
         integer, parameter :: qc_cu_ind      = 14
         integer, parameter :: qi_cu_ind      = 15
         integer, parameter :: densa_j_ind    = 16
         integer, parameter :: eddy_ind       = 17

! for cgrid data
         integer, parameter :: my_n_gc = 127
         integer, parameter :: my_n_ae =  83
         integer, parameter :: my_n_nr =  17

         integer, parameter :: num_cmaq_species = my_n_gc + my_n_ae + my_n_nr

         integer  :: my_gc_adj
         integer  :: my_ae_adj
         integer  :: my_nr_adj

         real, allocatable :: g2ddata(:,:,:)
         real, allocatable :: g3ddata(:,:,:,:)
         real, allocatable :: lufrac_data(:,:,:)
         real, allocatable :: smois_data(:,:)         ! surface layer
         real, allocatable :: cmaq_species(:,:,:,:)
!        real, allocatable :: gc_species(:,:,:,:)
!        real, allocatable :: ae_species(:,:,:,:)
!        real, allocatable :: nr_species(:,:,:,:)
!        real, allocatable :: tr_species(:,:,:,:)

         real, allocatable :: my_emis_buffer (:, :, :, :, :)

         integer :: my_emis_buffer_ind, my_emis_tstep 

         logical :: mpas_cmaq_last_step = .false.

         character (16) :: mminlu_mpas

       end module coupler_module
