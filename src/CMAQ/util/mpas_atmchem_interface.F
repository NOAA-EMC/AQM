 module mpas_atmchem_interface

   use mpas_kind_types
   use mpas_derived_types
   use mpas_pool_routines
 
   use mpas_atmphys_vars

   use get_env_module

   implicit none

   private :: convert_time

   contains

   subroutine mpas_cmaq_coupler (config_dt, domain, mesh, state, time_lev,  &
                                 diag, diag_physics, sfc_input)

      use mpas_timekeeping
!     use mpas_kind_types
      use coupler_module

!    include 'mpif.h'

     real, parameter :: rad_to_deg_factor = 57.295779513

     real(kind=RKIND), pointer           :: config_dt
     type(domain_type),    intent(in)    :: domain
     type(mpas_pool_type), intent(in)    :: mesh
     type(mpas_pool_type), intent(inout) :: state
     type(mpas_pool_type), intent(in)    :: diag
     type(mpas_pool_type), intent(in)    :: diag_physics
     type(mpas_pool_type), intent(in)    :: sfc_input
     integer,              intent(in)    :: time_lev

! local pointers:
     integer, pointer :: index_qv, index_qc, index_qr, index_qi, index_qs, index_qg

     real(kind=RKIND), dimension(:),   pointer :: latCell, lonCell, xland, xice, snowc, &
                                                  t2m, skintemp, ter, prsfc, rs, ra,    &
                                                  rmol, hpbl, ustar, hfx, lh, canwat,   &
                                                  lai, vegpx, znt, q2, rainc, rainnc,   &
                                                  cldfract, cldfracwcut, areaCell
                                                  
     real(kind=RKIND), dimension(:,:), pointer :: zgrid, cldfrac, pressure, rho, theta, &
                                                  landusef, smois, qv, qc, qr, qi, qs,  &
                                                  qg, eddy, pressure_p, pressure_base,  &
                                                  cldfracwcu, qc_cu, qi_cu, gfs_o3
     real(kind=RKIND), dimension(:,:,:), pointer :: scalars

     real(kind=RKIND), allocatable :: ta(:,:)

     integer, dimension(:), pointer :: ivgtyp, lw

     character(len=StrKIND), pointer :: mminlu

     integer, pointer :: num_scalars
 
     character(len=StrKIND)          :: timeStamp
     type (MPAS_Time_Type)           :: currTime
     type (MPAS_TimeInterval_Type)   :: xtimeTime
     type (MPAS_Clock_type), pointer :: clock

     integer :: ierr, i, v, k 
     integer :: tt2(2)
     integer, save :: mpas_date, mpas_time
     logical, save :: firstime = .true.
     logical, save :: initialized = .false.
     logical, save :: called_convert_time = .false.
     integer, save :: counter = -1
     integer, save :: mpas_cmaq_freq
     logical, save :: run_cmaq_driver, mpas_restart
     logical       :: cmaq_step

     counter = counter + 1

     if (firstime) then
        call get_env (mpas_cmaq_freq, 'mpas_cmaq_freq', 1)

        call get_env (mpas_restart, 'mpas_restart', .false.)

        call get_env (run_cmaq_driver, 'run_cmaq_driver', .false.)

!print *, ' ==d== coupler a ', ite, kte

        allocate (g2ddata(ite, 1, n2d_data),                          &
                  g3ddata(ite, 1, kte, n3d_data),                     &
                  smois_data(ite, 1),                                 &
                  cmaq_species(ite, 1, kte, num_cmaq_species),        &
                  stat=ierr)
        if (ierr .ne. 0) then
           print *, ' Error: Cannot allocate memory in mpas_to_chem_var_mapping'
           stop
        end if 

     end if

!print *, ' ==d== coupler a ', ite, kte, mpas_cmaq_freq, mpas_restart, initialized, firstime

     call mpas_pool_get_dimension (state, 'num_scalars', num_scalars)

     if ( .not. mpas_restart .and. .not. initialized) then
        cmaq_species = 1.0e-20
        cmaq_species(:, 1, 1:18  , 4 ) = 20.0    * 1.0e-3
        cmaq_species(:, 1, 19:40 , 4)  = 50.0    * 1.0e-3
        cmaq_species(:, 1, 41    , 4)  = 70.0    * 1.0e-3
        cmaq_species(:, 1, 42    , 4)  = 100.0   * 1.0e-3
        cmaq_species(:, 1, 43    , 4)  = 200.0   * 1.0e-3
        cmaq_species(:, 1, 44    , 4)  = 500.0   * 1.0e-3
        cmaq_species(:, 1, 45    , 4)  = 1000.0  * 1.0e-3
        cmaq_species(:, 1, 46    , 4)  = 2000.0  * 1.0e-3
        cmaq_species(:, 1, 47    , 4)  = 3000.0  * 1.0e-3
        cmaq_species(:, 1, 48    , 4)  = 4000.0  * 1.0e-3
        cmaq_species(:, 1, 49    , 4)  = 6000.0  * 1.0e-3
        cmaq_species(:, 1, 50    , 4)  = 8000.0  * 1.0e-3
     end if

     print *, ' ==d== mpas ', counter

     if (run_cmaq_driver) then

        cmaq_step = (mod(counter, mpas_cmaq_freq) .eq. 0)

        if (cmaq_step) then

           call mpas_pool_get_array (mesh, 'latCell', latCell)
           call mpas_pool_get_array (mesh, 'lonCell', lonCell)
           call mpas_pool_get_array (mesh, 'zgrid', zgrid)
           call mpas_pool_get_array (mesh, 'areaCell', areaCell)

!print *, ' ==d== lat ', minval(latcell(1:ite)), maxval(latcell(1:ite))
!print *, ' ==d== lon ', minval(loncell(1:ite)), maxval(loncell(1:ite))
!print *, ' ==d== zgrid ', minval(zgrid(:,1:ite)), maxval(zgrid(:,1:ite))

           call mpas_pool_get_array (sfc_input, 'xland', xland, time_lev)
           call mpas_pool_get_array (sfc_input, 'landmask', lw)
           call mpas_pool_get_array (sfc_input, 'xice', xice, time_lev)
           call mpas_pool_get_array (sfc_input, 'snowc', snowc, time_lev)
           call mpas_pool_get_array (sfc_input, 'skintemp', skintemp, time_lev)
           call mpas_pool_get_array (sfc_input, 'ter', ter)

!write (6, '(a13, 3e12.5)') ' ==d== xland ', minval(xland(1:ite)), maxval(xland(1:ite)), sum(xland(1:ite))
!write (6, '(a13, 3i7)') ' ==d== lw    ', minval(lw(1:ite)), maxval(lw(1:ite)), sum(lw(1:ite))
!write (6, '(a13, 3e12.5)') ' ==d== xice  ', minval(xice(1:ite)), maxval(xice(1:ite)), sum(xice(1:ite))
!write (6, '(a13, 3e12.5)') ' ==d== snowc ', minval(snowc(1:ite)), maxval(snowc(1:ite)), sum(snowc(1:ite))
!print *, ' ==d== skintemp ', minval(skintemp), maxval(skintemp)
!print *, ' ==d== ter ', minval(ter(1:ite)), maxval(ter(1:ite))

           call mpas_pool_get_array (sfc_input, 'ivgtyp', ivgtyp)
           call mpas_pool_get_array (sfc_input, 'mminlu', mminlu)
           call mpas_pool_get_array (sfc_input, 'landusef', landusef)
           call mpas_pool_get_array (sfc_input, 'smois', smois)

!write (6, '(a14, 2i8, a16)') ' ==d== ivgtyp ', minval(ivgtyp(1:ite)), maxval(ivgtyp(1:ite)), trim(mminlu)
!write (6, '(a13, 3e12.5)') ' ==d== lw    ', minval(landusef(:,1:ite)), maxval(landusef(:,1:ite)), sum(landusef(:,1:ite))
!write (6, '(a13, 3e12.5)') ' ==d== lw    ', minval(smois(:,1:ite)), maxval(smois(:,1:ite))

           call mpas_pool_get_array (diag_physics, 't2m', t2m, time_lev)
           call mpas_pool_get_array (diag_physics, 'rs', rs, time_lev)
           call mpas_pool_get_array (diag_physics, 'ra', ra, time_lev)
           call mpas_pool_get_array (diag_physics, 'rmol', rmol, time_lev)
           call mpas_pool_get_array (diag_physics, 'hpbl', hpbl, time_lev)
           call mpas_pool_get_array (diag_physics, 'ust', ustar, time_lev)
           call mpas_pool_get_array (diag_physics, 'hfx', hfx, time_lev)
           call mpas_pool_get_array (diag_physics, 'lai', lai, time_lev)

!print *, ' ==d== t2m ', minval(t2m(1:ite)), maxval(t2m(1:ite))
!print *, ' ==d== rs ', minval(rs(1:ite)), maxval(rs(1:ite))
!print *, ' ==d== ra ', minval(ra(1:ite)), maxval(ra(1:ite))
!print *, ' ==d== rmol ', minval(rmol(1:ite)), maxval(rmol(1:ite))
!print *, ' ==d== hpbl ', minval(hpbl(1:ite)), maxval(hpbl(1:ite))
!print *, ' ==d== ust ', minval(ustar(1:ite)), maxval(ustar(1:ite))
!print *, ' ==d== hfx ', minval(hfx(1:ite)), maxval(hfx(1:ite))
!print *, ' ==d== lai ', minval(lai(1:ite)), maxval(lai(1:ite))

           call mpas_pool_get_array (diag_physics, 'vegpx', vegpx, time_lev)
           call mpas_pool_get_array (diag_physics, 'znt', znt, time_lev)
           call mpas_pool_get_array (diag_physics, 'q2', q2, time_lev)
           call mpas_pool_get_array (diag_physics, 'lh', lh, time_lev)
           call mpas_pool_get_array (diag_physics, 'canwat', canwat, time_lev)
           call mpas_pool_get_array (diag_physics, 'raincv', rainc, time_lev)
           call mpas_pool_get_array (diag_physics, 'rainncv', rainnc, time_lev)
           call mpas_pool_get_array (diag_physics, 'cldfract', cldfract, time_lev)
           call mpas_pool_get_array (diag_physics, 'cldfracwcut', cldfracwcut, time_lev)

!write (6, '(a13, 3e12.5)') ' ==d== vegpx a ', minval(vegpx(1:ite)), maxval(vegpx(1:ite))
!print *, ' ==d== znt ', minval(znt(1:ite)), maxval(znt(1:ite))
!print *, ' ==d== q2 ', minval(q2(1:ite)), maxval(q2(1:ite))
!print *, ' ==d== lh ', minval(lh(1:ite)), maxval(lh(1:ite))
!print *, ' ==d== canwat ', minval(canwat(1:ite)), maxval(canwat(1:ite))

           call mpas_pool_get_array (diag_physics, 'cldfrac', cldfrac, time_lev)
           call mpas_pool_get_array (diag_physics, 'cldfracwcu', cldfracwcu, time_lev)
           call mpas_pool_get_array (diag_physics, 'qc_cu', qc_cu, time_lev)
           call mpas_pool_get_array (diag_physics, 'qi_cu', qi_cu, time_lev)
           call mpas_pool_get_array (diag, 'pressure', pressure, time_lev)
           call mpas_pool_get_array (diag, 'pressure_p', pressure_p, time_lev)
           call mpas_pool_get_array (diag, 'pressure_base', pressure_base, time_lev)
           call mpas_pool_get_array (diag, 'rho', rho, time_lev)
           call mpas_pool_get_array (diag, 'theta', theta, time_lev)
           call mpas_pool_get_array (diag, 'surface_pressure', prsfc, time_lev)
           call mpas_pool_get_array (diag_physics, 'exch_h', eddy, time_lev)

!write (6, '(a15, 3e12.5)') ' ==d== cldfrac ', minval(cldfrac(:,1:ite)), maxval(cldfrac(:,1:ite)), sum(cldfrac(:,1:ite))
!print *, ' ==d== pressure ', minval(pressure(:,1:ite)), maxval(pressure(:,1:ite))
!print *, ' ==d== pressure_p ', minval(pressure_p(:,1:ite)), maxval(pressure_p(:,1:ite))
!print *, ' ==d== pressure ', pressure(1,555), pressure_p(1,555), pressure_base(1,555)
           pressure = pressure_p + pressure_base
!print *, ' ==d== pressure ', minval(pressure(:,1:ite)), maxval(pressure(:,1:ite)), pressure(1,555)
!print *, ' ==d== rho ', minval(rho(:,1:ite)), maxval(rho(:,1:ite))
!print *, ' ==d== theta ', minval(theta(:,1:ite)), maxval(theta(:,1:ite))
!write (6, '(a13, 3e12.5)') ' ==d== prsfc ', minval(prsfc(1:ite)), maxval(prsfc(1:ite)), sum(prsfc(1:ite))
!write (6, '(a13, 3e12.5)') ' ==d== eddy ', minval(eddy(:,1:ite)), maxval(eddy(:,1:ite))

           allocate (ta(size(theta,1), size(theta, 2)), stat=ierr)

!print *, ' ==d== size a ', size(scalars,1), size(scalars,2), size(scalars,3), num_scalars

!    call mpas_pool_get_dimension (state, 'num_gc_species', num_gc_species)
!    call mpas_pool_get_dimension (state, 'num_ae_species', num_ae_species)
!    call mpas_pool_get_dimension (state, 'num_nr_species', num_nr_species)
!    call mpas_pool_get_dimension (state, 'num_tr_species', num_tr_species)

           call mpas_pool_get_array (state, 'scalars', scalars, time_lev)

           call mpas_pool_get_dimension (state, 'index_qv', index_qv )
           call mpas_pool_get_dimension (state, 'index_qc', index_qc )
           call mpas_pool_get_dimension (state, 'index_qr', index_qr )
           call mpas_pool_get_dimension (state, 'index_qi', index_qi )
           call mpas_pool_get_dimension (state, 'index_qs', index_qs )
           call mpas_pool_get_dimension (state, 'index_qg', index_qg )

!print *, ' ==d== qv ', minval(scalars(1,1,1:ite)), maxval(scalars(1,1,1:ite))

           qv => scalars(index_qv , :,: )
           qc => scalars(index_qc , :,: )
           qr => scalars(index_qr , :,: )
           qi => scalars(index_qi , :,: )
           qs => scalars(index_qs , :,: )
           qg => scalars(index_qg , :,: )

!      print *, ' ==d== sp1 ',minval(scalars( 9,1,1:ite)), maxval(scalars( 9,1,1:ite))
!      print *, ' ==d== sp2 ',minval(scalars(10,1,1:ite)), maxval(scalars(11,1,1:ite))
!      print *, ' ==d== sp3 ',minval(scalars(11,1,1:ite)), maxval(scalars(12,1,1:ite))
!      print *, ' ==d== sp4 ',minval(scalars(12,1,1:ite)), maxval(scalars(13,1,1:ite))
!      print *, ' ==d== sp5 ',minval(scalars(13,1,1:ite)), maxval(scalars(14,1,1:ite))
  
!      print *, ' ==d== qv ', index_qv, minval(qv), maxval(qv), minval(qv(1,1:ite)), maxval(qv(1,1:ite))
!      print *, ' ==d== qc ', index_qc, minval(qc), maxval(qc)
!      print *, ' ==d== qr ', index_qr, minval(qr), maxval(qr)
!      print *, ' ==d== qi ', index_qi, minval(qi), maxval(qi)
!      print *, ' ==d== qs ', index_qs, minval(qs), maxval(qs)
!      print *, ' ==d== qg ', index_qg, minval(qg), maxval(qg), sum(qg)

           ! convert potential temperature to temperature which CMAQ requires
           ta = theta / ( ( 1.0 + 0.608 * qv ) * ( ( 100000.0 / pressure ) ** 0.286 ))

!print *, ' ==d== ta ', minval(ta(:,1:ite)), maxval(ta(:,1:ite))

           clock => domain % clock

           currTime = mpas_get_clock_time(clock, MPAS_NOW, ierr)

           call mpas_get_time(curr_time=currTime, dateTimeString=timeStamp, ierr=ierr)

!          if (.not. called_convert_time) then
              call convert_time (timeStamp, mpas_date, mpas_time)
!             called_convert_time = .true.
!          end if

           mminlu_mpas = mminlu

           my_gc_adj = num_scalars - num_cmaq_species

 print *, ' ==d== call cmaq driver a ', counter, trim(timeStamp), ' == ', mpas_date, mpas_time, num_cmaq_species, num_scalars, my_gc_adj

           if (.not. allocated(lufrac_data)) then
              allocate (lufrac_data(size(landusef,1), size(landusef,2), 1), stat=ierr)
              if (ierr .ne. 0) then
                 print *, ' Error: Cannot allocate lufrac_data'
                 stop
              end if

              lufrac_data(:,:,1) = landusef
           end if

!k = my_ae_adj - my_gc_adj - 2
!print *, ' ==d== couple d ', 1, k, cmaq_species(1,1,1,1:5), cmaq_species(1,1,1,k-4:k)

!i = my_nr_adj - my_gc_adj - 2
!print *, ' ==d== couple d ', k+1, i, cmaq_species(1,1,1,k+1:k+5), cmaq_species(1,1,1,i-4:i)

!k = num_cmaq_species
!print *, ' ==d== couple d ', i+1, k, cmaq_species(1,1,1,i+1:i+5), cmaq_species(1,1,1,k-4:k)

           smois_data(:,1) = smois(1,:)

           g2ddata(:, 1, lon_ind) = lonCell * rad_to_deg_factor       ! convert to degree which CMAQ requires
           g2ddata(:, 1, lat_ind) = latCell * rad_to_deg_factor       ! convert to degree which CMAQ requires
           ! in MPAS lonCell is 0 - 360 counter clockwise, in CMAQ longitude is 0 - 180 counter clockwise 
           ! and 0 - -180 clockwise
           where (g2ddata(:, 1, lon_ind) > 180.0) g2ddata(:, 1, lon_ind) = g2ddata(:, 1, lon_ind) - 360.0

           g2ddata(:, 1, lwmask_ind) = xland

!print *, ' ==d== coupler e data lw ', minval(g2ddata(:, 1, lwmask_ind)), minval(g2ddata(:, 1, lwmask_ind)), &
!   sum(g2ddata(:, 1, lwmask_ind)), size(g2ddata, 1)

!          g2ddata(:, 1, open_ind)   = open_in
           g2ddata(:, 1, open_ind)   = 0
!          g2ddata(:, 1, szone_ind)  = szone_in
           g2ddata(:, 1, szone_ind)  = 1
!          g2ddata(:, 1, surf_ind)   = surf_in
           g2ddata(:, 1, surf_ind)   = 1

           g2ddata(:, 1, seaice_ind) = xice
           g2ddata(:, 1, prsfc_ind)  = prsfc
           g2ddata(:, 1, snocov_ind) = snowc
           g2ddata(:, 1, temp2_ind)  = t2m
           g2ddata(:, 1, tempg_ind)  = skintemp
           g2ddata(:, 1, ht_ind)     = ter
           g2ddata(:, 1, rmol_ind)   = rmol
           g2ddata(:, 1, rs_ind)     = rs
           g2ddata(:, 1, ra_ind)     = ra
           g2ddata(:, 1, pbl_ind)    = hpbl
           g2ddata(:, 1, ustar_ind)  = ustar
           g2ddata(:, 1, hfx_ind)    = hfx
           g2ddata(:, 1, lh_ind)     = lh
           g2ddata(:, 1, canwat_ind) = canwat
           g2ddata(:, 1, lai_ind)    = lai
           g2ddata(:, 1, vegpx_ind)  = vegpx

!  print *, ' ==d== coupler vegpx f ', size(g2ddata,1), size(vegpx), minval(vegpx), maxval(vegpx)
!  print *, ' ==d== coupler vegpx g ', size(g2ddata,1), minval(g2ddata(:, 1, vegpx_ind)), maxval(g2ddata(:, 1, vegpx_ind))
!  print *, ' ==d== coupler vegpx h ', size(g2ddata,1), minval(g2ddata(:, :, vegpx_ind)), maxval(g2ddata(:, :, vegpx_ind))

           g2ddata(:, 1, znt_ind)    = znt
           g2ddata(:, 1, q2_ind)     = q2

           g2ddata(:, 1, rainc_ind)    = rainc
           g2ddata(:, 1, cfrac2dr_ind) = cldfract
           g2ddata(:, 1, cfrac2dt_ind) = cldfracwcut 
           g2ddata(:, 1, cellArea_ind) = areaCell

!     write (6, *) ' ==d== grid ', kte
!     do k = 1, kte, 5
!        write (6, '(a12, i5,5e20.12)') ' ==d== grid ', k, zgrid(k:k+4,651)
!     end do
 
           do k = 1, kte
              do i = 1, ite
                 g3ddata(i, 1, k, cfrac3d_ind)    = cldfrac(k, i)
                 g3ddata(i, 1, k, cldfracwcu_ind) = cldfracwcu(k, i)
                 g3ddata(i, 1, k, pres_ind)       = pressure(k, i)
                 g3ddata(i, 1, k, zf_ind)         = (zgrid(k+1, i) - zgrid(1, i))
                 g3ddata(i, 1, k, zh_ind)         = g3ddata(i, 1, k, zf_ind) - (zgrid(k+1, i) - zgrid(k, i)) * 0.5
                 g3ddata(i, 1, k, dens_ind)       = rho(k, i)
                 g3ddata(i, 1, k, temp_ind)       = ta(k, i)
                 g3ddata(i, 1, k, qv_ind)         = qv(k, i)
                 g3ddata(i, 1, k, qc_ind)         = qc(k, i)
                 g3ddata(i, 1, k, qr_ind)         = qr(k, i)
                 g3ddata(i, 1, k, qg_ind)         = qg(k, i)
                 g3ddata(i, 1, k, qi_ind)         = qi(k, i)
                 g3ddata(i, 1, k, qs_ind)         = qs(k, i)
                 g3ddata(i, 1, k, qc_cu_ind)      = qc_cu(k, i)
                 g3ddata(i, 1, k, qi_cu_ind)      = qi_cu(k, i)
                 g3ddata(i, 1, k, densa_j_ind)    = 1.0 / rho(k, i)
                 g3ddata(i, 1, k, eddy_ind)       = eddy(k, i)
              end do
           end do

           tt2 = minloc(zgrid)

!    print *, ' ==d== coupler m zgrid ', kte, cell_num, zgrid(:,cell_num)
!    print *, ' ==d== coupler n zf    ', g3ddata(cell_num,1,:,zf_ind)
!    print *, ' ==d== coupler p zh    ', g3ddata(cell_num,1,:,zh_ind)

!    print *, ' ==d== coupler q zf    ', g3ddata(tt2(2),1,:,zf_ind)
!    print *, ' ==d== coupler q zh    ', g3ddata(tt2(2),1,:,zh_ind)

!    print *, ' ==d== coupler q zh ', minval(g3ddata(:,:,:,zh_ind)), maxval(g3ddata(:,:,:,zh_ind))
!    print *, ' ==d== coupler q zh ', g3ddata(cell_num,1,:,zh_ind)

           deallocate (ta)

           if ( .not. mpas_restart .and. .not. initialized) then
              initialized = .true.
           else
              do k = 1, kte
                 do i = 1, ite
                    do v = 1, num_cmaq_species
                       cmaq_species(i, 1, k, v) = scalars(v+my_gc_adj, k, i)
                    end do
                 end do
              end do
           end if

           if (counter >= 1) then

 write (6, '(a24, 3i8, f8.2)') ' ==d== call cmaq driver ', counter, mpas_date, mpas_time, config_dt

              call cmaq_driver (ite, kte, mpas_date, mpas_time, config_dt * mpas_cmaq_freq)

              ! transfer species information back to MPAS
              do k = 1, kte
                 do i = 1, ite
                    do v = 1, num_cmaq_species
                       scalars(v+my_gc_adj, k, i) = cmaq_species(i, 1, k, v)
                    end do
                 end do
              end do
           end if

!print *, ' ==d== sp end 1 ',minval(scalars( 1,1,1:ite)), maxval(scalars( 1,1,1:ite))
!print *, ' ==d== sp end 2 ',minval(scalars( 2,1,1:ite)), maxval(scalars( 2,1,1:ite))
!print *, ' ==d== sp end 3 ',minval(scalars( 3,1,1:ite)), maxval(scalars( 3,1,1:ite))
!print *, ' ==d== sp end 4 ',minval(scalars( 4,1,1:ite)), maxval(scalars( 4,1,1:ite))
!print *, ' ==d== sp end 5 ',minval(scalars( 5,1,1:ite)), maxval(scalars( 5,1,1:ite))
!print *, ' ==d== sp end 6 ',minval(scalars( 6,1,1:ite)), maxval(scalars( 6,1,1:ite))
!print *, ' ==d== sp end 7 ',minval(scalars( 7,1,1:ite)), maxval(scalars( 7,1,1:ite))
!print *, ' ==d== sp end 8 ',minval(scalars( 8,1,1:ite)), maxval(scalars( 8,1,1:ite))
!print *, ' ==d== sp end 9 ',minval(scalars( 9,1,1:ite)), maxval(scalars( 9,1,1:ite))
!print *, ' ==d== sp end10 ',minval(scalars(10,1,1:ite)), maxval(scalars(10,1,1:ite))
!print *, ' ==d== sp end11 ',minval(scalars(11,1,1:ite)), maxval(scalars(11,1,1:ite))
!print *, ' ==d== sp end12 ',minval(scalars(12,1,1:ite)), maxval(scalars(12,1,1:ite))
!print *, ' ==d== sp end13 ',minval(scalars(13,1,1:ite)), maxval(scalars(13,1,1:ite))
!print *, ' ==d== sp end14 ',minval(scalars(14,1,1:ite)), maxval(scalars(14,1,1:ite))
!print *, ' ==d== sp end15 ',minval(scalars(15,1,1:ite)), maxval(scalars(15,1,1:ite))

        end if  ! end of cmaq_step
     end if  ! end of run_cmaq_driver

     if (firstime) then
        firstime = .false.
        ! this initialize CMAQ data in scalar array at the very first step
        if ( .not. mpas_restart ) then
           do k = 1, kte
              do i = 1, ite
                 do v = 1, num_cmaq_species
                    scalars(v+my_gc_adj, k, i) = cmaq_species(i, 1, k, v)
                 end do
              end do
           end do
        end if
     end if

   end subroutine mpas_cmaq_coupler

! -------------------------------------------------------------------

   subroutine convert_time (timeStamp, mpas_date, mpas_time)

     character(len=StrKIND), intent(in) :: timeStamp
     integer, intent(out) :: mpas_date, mpas_time

     integer, parameter :: ly_days(12) = (/0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 /)
     integer, parameter :: ry_days(12) = (/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 /)

     integer :: year, month, day, hour, min, sec, tday
     logical :: leap_year

     read (timeStamp, '(i4, 5(1x, i2))') year, month, day, hour, min, sec

     if (mod(year, 4) .ne. 0) then
        leap_year = .false.
     else if (mod(year, 400) .eq. 0) then
        leap_year = .true.
     else if (mod(year, 100) .eq. 0) then
        leap_year = .false.
     else
        leap_year = .true.
     endif

     if (leap_year) then
        tday = ly_days(month) + day
     else
        tday = ry_days(month) + day
     end if

     mpas_date = year * 1000 + tday
     mpas_time = hour * 10000 + min * 100 + sec

   end subroutine convert_time

 end module mpas_atmchem_interface
