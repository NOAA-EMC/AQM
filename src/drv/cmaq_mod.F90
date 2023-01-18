module cmaq_mod

  use aqm_rc_mod
  use aqm_types_mod
  use aqm_const_mod, only : onebg, rdgas, grav, mwair, zero
  use aqm_emis_mod
  use aqm_prod_mod
  use aqm_tools_mod, only : aqm_units_conv

  use PAGRD_DEFN
  USE PA_DEFN, Only: LIPR, LIRR
  use PCGRID_DEFN

  use cgrid_spcs

  use AERO_DATA,   only : aerolist, n_aerolist,  &
                          aerospc_mw, n_emis_pm, &
                          map_pmemis, pmem_map,  &
                          pmem_map_name, pmem_units

  use M3UTILIO,    only : M3MESG
  use UTILIO_DEFN, only : INDEX1, INIT3, MXVARS3

  implicit none

  integer :: cmaq_logdev

  ! -- pointer to CMAQ concentration array
  real, pointer :: CGRID(:,:,:,:) => null()

  ! -- fire emissions work arrays
  real, allocatable :: em_buffer(:)
  real, allocatable :: em_vfrac(:,:,:)

  private

  public :: cmaq_logdev

  public :: cmaq_advance
  public :: cmaq_init
  public :: cmaq_conc_init
  public :: cmaq_conc_log
  public :: cmaq_domain_log
  public :: cmaq_emis_init
  public :: cmaq_emis_finalize
  public :: cmaq_emis_print
  public :: cmaq_prod_init
  public :: cmaq_prod_units_get
  public :: cmaq_prod_update
  public :: cmaq_species_read
  public :: cmaq_export
  public :: cmaq_import
  public :: cmaq_pm_export

contains

  subroutine cmaq_species_read(nspecies, rc)

    integer,           intent(out) :: nspecies
    integer, optional, intent(out) :: rc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- read from namelist CGRID gas chem, aerosol, non-reactive, 
    ! -- and tracer species definitions
    ! -- This is done only on DE 0 and shared with other DEs on this PET
    nspecies = 0
    if (aqm_rc_test(.not.cgrid_spcs_init(), &
      msg="Error in CGRID_SPCS:CGRID_SPCS_INIT", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (aqm_rc_test(( nspcsd > mxvars3 ), &
      msg="Number of species exceeds MXVARS3", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    nspecies = n_gc_trns + n_ae_trns + n_nr_trns

  end subroutine cmaq_species_read

  subroutine cmaq_init(rc)

    integer, optional, intent(out)   :: rc

    ! -- local variables
    integer :: mype, nprocs

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- set CMAQ log unit
    cmaq_logdev = INIT3()

    ! -- set up horizontal domain and define vertical layer structure
    mype   = 0
    nprocs = 1
    if (aqm_rc_test(.not.grid_init( nprocs, mype ), &
      msg="Failure defining horizontal domain", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    ! -- set I/O flag
    IO_PE_INCLUSIVE = ( MYPE .EQ. 0 )

    ! -- Generate the process analysis data: load PA_DEFN module
!   CALL PA_DATAGEN( )

    ! -- Set up horizontal domain and calculate processor-to-subdomain maps for
    ! -- process analysis, if required
    IF ( LIPR .OR. LIRR ) THEN
      IF (aqm_rc_test( .NOT. PAGRD_INIT( NPROCS, MYPE ), &
        msg="Failure defining PA domain configuration",  &
        FILE=__FILE__, LINE=__LINE__, rc=rc)) RETURN
    END IF

    ! -- Initialize PCGRID
    if (aqm_rc_test(.not.pcgrid_init(), &
      msg="Failure defining horizontal domain", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    CGRID => PCGRID( 1:MY_NCOLS,1:MY_NROWS,:,: )   ! required for PinG

  end subroutine cmaq_init

  subroutine cmaq_advance(jdate, jtime, tstep, run_aero, run_rescld, rc)

    integer,           intent(in)    :: jdate, jtime, tstep(3)
    logical,           intent(in)    :: run_aero, run_rescld
    integer, optional, intent(out)   :: rc

    ! -- local variables

    ! -- external methods
    INTERFACE
      SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP )
        REAL, POINTER             :: CGRID( :,:,:,: )
        INTEGER                   :: JDATE, JTIME
        INTEGER                   :: TSTEP( 3 )
      END SUBROUTINE VDIFF
      SUBROUTINE CLDPROC ( CGRID, JDATE, JTIME, TSTEP )
        REAL, POINTER             :: CGRID( :,:,:,: )
        INTEGER                   :: JDATE, JTIME
        INTEGER                   :: TSTEP( 3 )
      END SUBROUTINE CLDPROC
      SUBROUTINE CHEM ( CGRID, JDATE, JTIME, TSTEP )
        REAL, POINTER             :: CGRID( :,:,:,: )
        INTEGER                   :: JDATE, JTIME
        INTEGER                   :: TSTEP( 3 )
      END SUBROUTINE CHEM
      SUBROUTINE AERO ( CGRID, JDATE, JTIME, TSTEP )
        REAL, POINTER             :: CGRID( :,:,:,: )
        INTEGER                   :: JDATE, JTIME
        INTEGER                   :: TSTEP( 3 )
      END SUBROUTINE AERO
    END INTERFACE

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- advance all physical and chemical processes on a grid
    CALL VDIFF ( CGRID, JDATE, JTIME, TSTEP )
    
    if (run_rescld) then
      CALL CLDPROC ( CGRID, JDATE, JTIME, TSTEP )
    end if

    CALL CHEM ( CGRID, JDATE, JTIME, TSTEP )

    if (run_aero) then
      CALL AERO ( CGRID, JDATE, JTIME, TSTEP )
    end if

  end subroutine cmaq_advance

  subroutine cmaq_import(tracers, prl, phii, temp, start_index, rc)

    real(AQM_KIND_R8), intent(in)  :: tracers(:,:,:,:)
    real(AQM_KIND_R8), intent(in)  :: prl(:,:,:)
    real(AQM_KIND_R8), intent(in)  :: phii(:,:,:)
    real(AQM_KIND_R8), intent(in)  :: temp(:,:,:)
    integer,           intent(in)  :: start_index
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: c, r, l, n, off, spc, v
    real(AQM_KIND_R8) :: dens

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    n = max(0, start_index - 1)

    ! -- gas chemistry
    if ( n_gc_spc > 0 ) then
      off = gc_strt - 1
      do v = 1, n_gc_trns
         spc = off + gc_trns_map( v )
         n = n + 1
         do l = 1, nlays
            do r = 1, my_nrows
               do c = 1, my_ncols
                  cgrid( c,r,l,spc ) = max( zero, tracers( c,r,l,n ) )
               end do
            end do
         end do
      end do
    end if

    ! -- aerosols
    if ( n_ae_spc > 0 ) then
      off = ae_strt - 1
      do v = 1, n_ae_trns
         spc = off + ae_trns_map( v )
         n = n + 1
         ! -- convert from x/kg to x/m3 (x = ug, number, m2)
         do l = 1, nlays
            do r = 1, my_nrows
               do c = 1, my_ncols
                  dens = prl( c,r,l ) / ( rdgas * temp( c,r,l ) )
                  cgrid( c,r,l,spc ) = dens * max( zero, tracers( c,r,l,n ) )
               end do
            end do
         end do
      end do
    end if

    ! -- non reactive species
    if ( n_nr_spc > 0 ) then
      off = nr_strt - 1
      do v = 1, n_nr_trns
         spc = off + nr_trns_map( v )
         n = n + 1
         do l = 1, nlays
            do r = 1, my_nrows
               do c = 1, my_ncols
                  cgrid( c,r,l,spc ) = max( zero, tracers( c,r,l,n ) )
               end do
            end do
         end do
      end do
    end if

    ! -- Jacobian x air density for process analysis
    spc = gc_strt - 1 + n_gc_spcd

    do l = 1, nlays
      do r = 1, my_nrows
        do c = 1, my_ncols
          dens = prl( c,r,l ) / ( rdgas * temp( c,r,l ) )
          cgrid( c,r,l,spc ) = onebg * dens * ( phii( c,r,l+1 ) - phii( c,r,l ) )
        end do
      end do
    end do

  end subroutine cmaq_import

  subroutine cmaq_export(tracers, prl, temp, start_index, rc)

    real(AQM_KIND_R8), intent(out) :: tracers(:,:,:,:)
    real(AQM_KIND_R8), intent(in)  :: prl(:,:,:)
    real(AQM_KIND_R8), intent(in)  :: temp(:,:,:)
    integer,           intent(in)  :: start_index
    integer, optional, intent(out) :: rc
    

    ! -- local variables
    integer :: c, r, l, n, off, spc, v
    real(AQM_KIND_R8) :: rdens
    real :: pm25(my_ncols,my_nrows,nlays,1)
    integer :: localrc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    n = max(0, start_index - 1)

    ! -- gas chemistry
    if ( n_gc_spc > 0 ) then
      off = gc_strt - 1
      do v = 1, n_gc_trns
         spc = off + gc_trns_map( v )
         n = n + 1
         do l = 1, nlays
            do r = 1, my_nrows
               do c = 1, my_ncols
                  tracers( c,r,l,n ) = cgrid( c,r,l,spc )
               end do
            end do
         end do
      end do
    end if

    ! -- aerosols
    if ( n_ae_spc > 0 ) then
      off = ae_strt - 1
      do v = 1, n_ae_trns
         spc = off + ae_trns_map( v )
         n = n + 1
         ! -- convert from x/m3 to x/kg (x = ug, number, m2)
         do l = 1, nlays
            do r = 1, my_nrows
               do c = 1, my_ncols
                  rdens = rdgas * temp( c,r,l ) / prl( c,r,l )
                  tracers( c,r,l,n ) = rdens * cgrid( c,r,l,spc )
               end do
            end do
         end do
      end do
    end if

    ! -- non reactive species
    if ( n_nr_spc > 0 ) then
      off = nr_strt - 1
      do v = 1, n_nr_trns
         spc = off + nr_trns_map( v )
         n = n + 1
         do l = 1, nlays
            do r = 1, my_nrows
               do c = 1, my_ncols
                  tracers( c,r,l,n ) = cgrid( c,r,l,spc )
               end do
            end do
         end do
      end do
    end if

  end subroutine cmaq_export

  subroutine cmaq_pm_export(tracers, diag_index, rc)

    real(AQM_KIND_R8), intent(out) :: tracers(:,:,:,:)
    integer,           intent(in)  :: diag_index
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: c, r, l, n
    real    :: pm25(my_ncols,my_nrows,nlays,1)

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- pm2.5
    call cmaq_prod_pm25( pm25, cgrid, tracers, diag_index, nlays )
    n = diag_index + 3
    do l = 1, nlays
      do r = 1, my_nrows
        do c = 1, my_ncols
          tracers( c,r,l,n ) = pm25( c,r,l,1 )
        end do
      end do
    end do

  end subroutine cmaq_pm_export

  subroutine cmaq_conc_init(jdate, jtime, tstep, rc)

    integer,           intent(in)  :: jdate, jtime, tstep(3)
    integer, optional, intent(out) :: rc

    ! -- external methods
    INTERFACE
      SUBROUTINE INITSCEN ( CGRID, STDATE, STTIME, TSTEP, NSTEPS )
        REAL, POINTER             :: CGRID( :,:,:,: )
        INTEGER                   :: STDATE, STTIME
        INTEGER                   :: TSTEP( 3 )
        INTEGER                   :: NSTEPS
      END SUBROUTINE INITSCEN
    END INTERFACE

    ! -- local variables
    integer :: stdate, sttime, nsteps

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    STDATE = JDATE
    STTIME = JTIME
    CALL INITSCEN ( CGRID, STDATE, STTIME, TSTEP, NSTEPS )

  end subroutine cmaq_conc_init

  subroutine cmaq_conc_log(label)

    character(len=*), intent(in) :: label

    ! -- local variables
    integer :: off, spc, v
    character(len=AQM_MAXSTR) :: msgString

    ! -- gas chemistry
    if ( n_gc_spc > 0 ) then
      off = gc_strt - 1
      do v = 1, n_gc_trns
         spc = off + gc_trns_map( v )
         write(msgString, '(a,": ",a16,"[",i0,"]: min/max = ",2g20.8)') &
           trim(label), gc_trns( v ), spc, &
           minval(cgrid(:,:,:,spc)), maxval(cgrid(:,:,:,spc))
         call m3mesg(msgString)
      end do
    end if

    ! -- aerosols
    if ( n_ae_spc > 0 ) then
      off = ae_strt - 1
      do v = 1, n_ae_trns
         spc = off + ae_trns_map( v )
         write(msgString, '(a,": ",a16,"[",i0,"]: min/max = ",2g20.8)') &
           trim(label), ae_trns( v ), spc, &
           minval(cgrid(:,:,:,spc)), maxval(cgrid(:,:,:,spc))
         call m3mesg(msgString)
      end do
    end if

    ! -- non reactive species
    if ( n_nr_spc > 0 ) then
      off = nr_strt - 1
      do v = 1, n_nr_trns
         spc = off + nr_trns_map( v )
         write(msgString, '(a,": ",a16,"[",i0,"]: min/max = ",2g20.8)') &
           trim(label), nr_trns( v ), spc, &
           minval(cgrid(:,:,:,spc)), maxval(cgrid(:,:,:,spc))
         call m3mesg(msgString)
      end do
    end if

    ! -- reciprocal Jacobian x air density for process analysis
    spc = gc_strt - 1 + n_gc_spcd
    write(msgString, '(a,": RHOJ",12x,"[",i0,"]: min/max = ",2g20.8)') &
      trim(label), spc, minval(cgrid(:,:,:,spc)), maxval(cgrid(:,:,:,spc))
    call m3mesg(msgString)

  end subroutine cmaq_conc_log

  subroutine cmaq_emis_init(rc)

    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc, stat
    integer :: item
    integer :: ltable, n, spc
    integer, allocatable :: umap(:)
    real(AQM_KIND_R4)    :: ucnv
    type(aqm_internal_emis_type), pointer :: em

    ! -- local parameters
    character(len=*), parameter :: etype(5) = (/ &
      "anthropogenic", &
      "biogenic     ", &
      "fengsha      ", &
      "gbbepx       ", &
      "point-source "  &
    /)

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- set proper units for input emissions
    ! -- performing conversions if necessary

    ! -- check if internal emissions reference table was allocated
    do item = 1, size(etype)

      nullify(em)
      em => aqm_emis_get(trim(etype(item)))

      if (associated(em)) then

        select case (trim(etype(item)))
          case ("anthropogenic", "gbbepx", "point-source")

          ! -- check if internal emissions reference table was allocated
          if (aqm_rc_test(.not.associated(em % table), &
            msg="Internal emissions table not allocated", &
            file=__FILE__, line=__LINE__, rc=rc)) return

          ! -- add internal units to emissions reference table
          ltable = size(em % table, dim=1)

          ! -- define mapping of CMAQ aerosol and related emission species
          call map_pmemis()

          ! -- set destination units for PM emissions for all species
          pmem_units = "G/S"

          ! -- set internal units for all species
          ! -- (a) gas species
          do n = 1, n_gc_emis
            spc = index1( gc_emis( n ), ltable, em % table(:,1) )
            if (spc > 0) em % table(spc,2) = "MOL/S"
          end do
          ! -- (b) non reactive
          do n = 1, n_nr_emis
            spc = index1( nr_emis( n ), ltable, em % table(:,1) )
            if (spc > 0) em % table(spc,2) = "MOL/S"
          end do
          spc = index1( "NH3_FERT", ltable, em % table(:,1) )
          if (spc > 0) em % table(spc,2) = "MOL/S"
          ! -- (c) aerosols
          do n = 1, n_emis_pm
            spc = index1( pmem_map_name( n ), ltable, em % table(:,1) )
            if (spc > 0) em % table(spc,2) = pmem_units
          end do

          ! -- perform unit conversion for input species, if needed
          ! -- (a) map input species to internal species
          allocate(umap(size(em % species)), stat=stat)
          if (aqm_rc_test(stat /= 0, &
            msg="Unable to allocate memory", &
            file=__FILE__, line=__LINE__, rc=rc)) return

          do n = 1, size(em % species)
            umap(n) = 0
            if ( trim(em % units(n)) == "1" ) then
              em % dens_flag(n) = 1
            else
              em % dens_flag(n) = 0
              spc = index1( em % species( n ), ltable, em % table(:,1) )
              if (spc > 0) umap(n) = spc
            end if
          end do

          ! -- (b) perform unit conversion for input species
          ! ---    1. gas species
          do n = 1, size(em % species)
            if (umap(n) > 0) then
              spc = index1( em % species(n), n_gc_emis, gc_emis )
              if (spc > 0) then
                ucnv = aqm_units_conv( em % units(n), em % table(umap(n),2), gc_molwt(gc_emis_map(spc)), em % dens_flag(n) )
                if (aqm_rc_test(ucnv == 0._AQM_KIND_R4, &
                  msg=trim(em % species(n))//": invalid input units ("//trim(em % units(n))//")", &
                  file=__FILE__, line=__LINE__, rc=rc)) return
                em % factors(n) = ucnv * em % factors(n)
                umap(n) = 0
              end if
            end if
          end do
          ! ---    2. non reactive
          do n = 1, size(em % species)
            if (umap(n) > 0) then
              if ( trim(em % species(n)) == "NH3_FERT" ) then
                spc = index1( "NH3", n_nr_emis, nr_emis )
              else
                spc = index1( em % species(n), n_nr_emis, nr_emis )
              end if
              if (spc > 0) then
                ucnv = aqm_units_conv( em % units(n), em % table(umap(n),2), nr_molwt(nr_emis_map(spc)), em % dens_flag(n) )
                if (aqm_rc_test(ucnv == 0._AQM_KIND_R4, &
                  msg=trim(em % species(n))//": invalid input units ("//trim(em % units(n))//")", &
                  file=__FILE__, line=__LINE__, rc=rc)) return
                em % factors(n) = ucnv * em % factors(n)
                umap(n) = 0
              end if
            end if
          end do
          ! ---    3. aerosols
          do n = 1, size(em % species)
            if (umap(n) > 0) then
              spc = index1( em % species(n), n_emis_pm, pmem_map_name )
              if (spc > 0) then
                ucnv = aqm_units_conv( em % units(n), em % table(umap(n),2), aerospc_mw(pmem_map(spc)), em % dens_flag(n) )
                if (aqm_rc_test(ucnv == 0._AQM_KIND_R4, &
                  msg=trim(em % species(n))//": invalid input units ("//trim(em % units(n))//")", &
                  file=__FILE__, line=__LINE__, rc=rc)) return
                em % factors(n) = ucnv * em % factors(n)
                umap(n) = 0
              end if
            end if
          end do

          ! -- (c) free up memory
          deallocate(umap, stat=stat)
          if (aqm_rc_test(stat /= 0, &
            msg="Unable to deallocate memory", &
            file=__FILE__, line=__LINE__, rc=rc)) return

          case ("biogenic")

          ! -- include conversion factors from source units to internal units
          do n = 1, size(em % species)
            em % dens_flag(n) = 0
            select case (trim(em % units(n)))
              case ("GMC/HR", "GMN/HR")
                em % dens_flag(n) = 0
              case ("1")
                em % dens_flag(n) = 1
              case default
                call aqm_rc_set(AQM_RC_SUCCESS, &
                  msg="Biogenics: unknown units: "//trim(em % units(n)), &
                  file=__FILE__, line=__LINE__, rc=rc)
                return
            end select
          end do

          case ("fengsha")

          ! -- inputs are already provided as surface densities, no need to normalize
          do n = 1, size(em % species)
            em % dens_flag(n) = 1
          end do

        end select

        ! -- log final scaling factors, if requested
        if (em % verbose) call cmaq_emis_log(em)

      end if
    enddo

  end subroutine cmaq_emis_init

  subroutine cmaq_emis_finalize(rc)

    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

  end subroutine cmaq_emis_finalize

  subroutine cmaq_emis_log(em)

    type(aqm_internal_emis_type), intent(in) :: em

    ! -- local variables
    integer :: maxlen, n
    character(len=AQM_MAXSTR) :: msgString

    ! -- begin
    ! -- determine species label's maximum length
    maxlen = 0
    do n = 1, size(em % species)
      maxlen = max(maxlen, len_trim(em % species( n )))
    end do

    ! -- write header
    call m3mesg(trim(em % logprefix) // ": compute from sources as follows:")

    ! -- write emission species, sources, and scaling factors
    do n = 1, size(em % species)
      write(msgString, '(a,": ",a,"[",i0,"] = ",g16.8," x [",a,"]")') &
        trim(em % logprefix), em % species( n )(1:maxlen), n, &
        em % factors( n ), trim(em % sources( n ))
      call m3mesg(msgString)
    end do

  end subroutine cmaq_emis_log

  subroutine cmaq_emis_print(etype, unit)

    character(len=*), intent(in) :: etype
    integer,          intent(in) :: unit

    ! -- local variables
    integer :: ltable, n, spc
    type(aqm_internal_emis_type), pointer :: em

    ! -- begin
    nullify(em)
    em => aqm_emis_get(etype)

    if (associated(em)) then

      ltable = size(em % table, dim=1)

      write(unit,'(2x,58("-"))')
      write(unit,'(21x,"Emission Table for ",a)') em % type
      write(unit,'(2x,58("-"))')
      do n = 1, size(em % species)
        spc = index1( em % species(n), ltable, em % table(:,1) )
        if (spc > 0) &
          write(unit,'(2x,i4,2x,a13,4x,a8,1x,"->",1x,a8,4x,g12.5)') n, trim(em % species(n)), &
            trim(em % units(n)), trim(em % table(spc,2)), em % factors(n)
      end do
      write(unit,'(2x,58("-"))')
    end if

  end subroutine cmaq_emis_print

  subroutine cmaq_domain_log(label)

    character(len=*), intent(in) :: label

    ! -- local variables
    character(len=AQM_MAXSTR) :: msgString

    ! -- begin
    write(msgString, '(a,": cgrid: init: NPCOL: ",i0,", NPROW: ",i0,' // &
      '", GL_NCOLS: ",i0,", GL_NROWS: ",i0,", NLAYS: ",i0,", NSPCSD: ",i0)') &
      trim(label), npcol, nprow, gl_ncols, gl_nrows, nlays, nspcsd
    call m3mesg(msgString)

  end subroutine cmaq_domain_log

  subroutine cmaq_prod_init(rc)

    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                               :: item, n
    character(len=AQM_MAXSTR)             :: units
    type(aqm_internal_emis_type), pointer :: pdata(:)
    type(aqm_internal_emis_type), pointer :: prod

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    nullify(pdata)
    pdata => aqm_emis_data_get()

    if (associated(pdata)) then

      do item = 1, size(pdata)
        nullify(prod)
        if (trim(pdata(item) % type) == "product") then
          prod => pdata(item)
          do n = 1, size(prod % species)
            call cmaq_prod_units_get( prod % species(n), units )
            call aqm_prod_units_set ( prod % fields(n),  units )
          end do
        end if
      end do

    end if

  end subroutine cmaq_prod_init

  subroutine cmaq_prod_units_get( species, units )

    character(len=*), intent(in)  :: species
    character(len=*), intent(out) :: units

    ! -- local variables
    integer :: spc

    ! -- begin
    units = "n/a"
    select case ( trim(species) )
      case ("PM2.5")
        ! --- diagnostic PM2.5
        units = "ug/m3"
      case default
        ! --- CMAQ species
        ! --- 1. gas species
        spc = index1( trim(species), n_gc_spc, gc_spc )
        if (spc > 0) then
          units = "ppmV"
        else
          ! --- 2. aerosol species
          spc = index1( trim(species), n_ae_spc, ae_spc )
          if (spc > 0) units = "ug/m3"
        end if
    end select

  end subroutine cmaq_prod_units_get

  subroutine cmaq_prod_update(tracers, start_index, pmdiag, rc)

    real(AQM_KIND_R8), intent(in)  :: tracers(:,:,:,:)
    integer,           intent(in)  :: start_index
    logical,           intent(in)  :: pmdiag
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: item, n, spc
    real    :: pm25(my_ncols,my_nrows,1,1)
    type(aqm_internal_emis_type), pointer :: pdata(:)
    type(aqm_internal_emis_type), pointer :: prod

    ! -- begin
    nullify(pdata)
    pdata => aqm_emis_data_get()

    if (associated(pdata)) then

      do item = 1, size(pdata)
        nullify(prod)
        if (trim(pdata(item) % type) == "product") then
          prod => pdata(item)
          do n = 1, size(prod % species)
            select case ( trim(prod % species(n)) )
              case ("PM2.5")
                ! --- diagnostic PM2.5
                if ( n_ae_spc > 0 .and. pmdiag ) then
                  call cmaq_prod_pm25( pm25, cgrid, tracers, start_index, 1)
                  call aqm_prod_compute( prod, pm25, n, 1 )
                end if
              case default
                ! --- CMAQ species
                spc = 0
                ! --- 1. gas species
                if ( n_gc_spc > 0 ) spc = index1( prod % species(n), n_gc_spc, gc_spc )
                if (spc > 0) then
                  spc = spc + gc_strt - 1
                  call aqm_prod_compute( prod, cgrid, n, spc )
                else
                  ! --- 2. aerosol species
                  if ( n_ae_spc > 0 ) spc = index1( prod % species(n), n_ae_spc, ae_spc )
                  if (spc > 0) then
                    spc = spc + ae_strt - 1
                    call aqm_prod_compute( prod, cgrid, n, spc )
                  end if
                end if
            end select
          end do
        end if
      end do

    end if

  end subroutine cmaq_prod_update

  subroutine cmaq_prod_pm25( pm25, cgrid, frac, idx, nlays_in)

    real,              intent(out) :: pm25(:,:,:,:)
    real,              intent(in)  :: cgrid(:,:,:,:)
    real(AQM_KIND_R8), intent(in)  :: frac(:,:,:,:)
    integer,           intent(in)  :: idx
    integer,           intent(in)  :: nlays_in

    ! -- local variables
    integer :: i, ibeg, iend, imod, mode, spc
    integer :: c, r, l

    ! -- local parameters
    character(len=*), parameter :: pm25_species(*) = &
      (/ "ASO4I  ", "ANO3I  ", "ANH4I  ", "ANAI   ", "ACLI   ", "AECI   ", "AOTHRI ",            & ! I-mode (Atken)
         "ALVPO1I", "ASVPO1I", "ASVPO2I", "ALVOO1I", "ALVOO2I", "ASVOO1I", "ASVOO2I",            &
         "ASO4J  ", "ANO3J  ", "ANH4J  ", "ANAJ   ", "ACLJ   ", "AECJ   ", "AOTHRJ ",            & ! J-mode (accum)
         "ALVPO1J", "ASVPO1J", "ASVPO2J", "ASVPO3J", "AIVPO1J",                                  &
         "AXYL1J ", "AXYL2J ", "AXYL3J ", "ATOL1J ", "ATOL2J ", "ATOL3J ", "ABNZ1J ", "ABNZ2J ", &
         "ABNZ3J ", "AISO1J ", "AISO2J ", "AISO3J ", "ATRP1J ", "ATRP2J ", "ASQTJ  ", "AALK1J ", &
         "AALK2J ", "APAH1J ", "APAH2J ", "APAH3J ", "AORGCJ ", "AOLGBJ ", "AOLGAJ ",            &
         "ALVOO1J", "ALVOO2J", "ASVOO1J", "ASVOO2J", "ASVOO3J", "APCSOJ ",                       &
         "AFEJ   ", "ASIJ   ", "ATIJ   ", "ACAJ   ", "AMGJ   ", "AMNJ   ", "AALJ   ", "AKJ    ", &
         "ASOIL  ", "ACORS  ", "ASEACAT", "ACLK   ", "ASO4K  ", "ANO3K  ", "ANH4K  " /)            ! K-mode (coarse)

    integer, parameter :: nspc(3) = (/ 14, 49, 7 /)

    ! -- begin
    pm25 = 0.

    if ( n_ae_spc > 0 ) then
      ibeg = 0
      iend = 0

      ! -- loop over I, J, K modes
      imod = idx
      do mode = 1, 3
        ibeg = iend + 1
        iend = iend + nspc(mode)
        do i = ibeg, iend
          spc = index1( pm25_species(i), n_ae_spc, ae_spc )
          if (spc > 0) then
            spc = spc + ae_strt - 1
            do l = 1, nlays_in
              do r = 1, my_nrows
                do c = 1, my_ncols
                  pm25( c,r,l,1 ) = pm25( c,r,l,1 ) + frac( c,r,l,imod ) * cgrid( c,r,l,spc )
                end do
              end do
            end do
          end if
        end do
        imod = imod + 1
      end do
    end if

  end subroutine cmaq_prod_pm25

end module cmaq_mod
