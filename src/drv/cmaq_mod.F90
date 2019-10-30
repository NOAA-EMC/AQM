module cmaq_mod

  use aqm_rc_mod
  use aqm_types_mod

  use PAGRD_DEFN
  USE PA_DEFN, Only: LIPR, LIRR
  use PCGRID_DEFN

  use cgrid_spcs,  only : cgrid_spcs_init, nspcsd

  use UTILIO_DEFN, only : INIT3, MXVARS3

  implicit none

  integer :: cmaq_logdev

  real, pointer :: CGRID(:,:,:,:) => null()

  private

  public :: cmaq_logdev

  public :: cmaq_advance
  public :: cmaq_init
  public :: cmaq_species_read
  public :: cmaq_export
  public :: cmaq_import

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
      msg="cmaq_init: Error in CGRID_SPCS:CGRID_SPCS_INIT", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    if (aqm_rc_test(( nspcsd > mxvars3 ), &
      msg="cmaq_init: Number of species exceeds MXVARS3", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    nspecies = nspcsd

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
      msg="cmaq_init: Failure defining horizontal domain", &
      file=__FILE__, line=__LINE__, rc=rc)) return

#ifdef verbose_driver
    write( cmaq_logdev,* ) ' MYPE -> NPROCS:   ', mype, nprocs
    write( cmaq_logdev,* ) ' MYPE -> NPCOL:    ', mype, npcol
    write( cmaq_logdev,* ) ' MYPE -> NPROW:    ', mype, nprow
    write( cmaq_logdev,* ) ' MYPE -> GL_NCOLS: ', mype, gl_ncols
    write( cmaq_logdev,* ) ' MYPE -> GL_NROWS: ', mype, gl_nrows
    write( cmaq_logdev,* ) ' MYPE -> NLAYS:    ', mype, nlays
    write( cmaq_logdev,* ) ' MYPE -> NSPCS:    ', mype, nspcsd
#endif

    ! -- set I/O flag
    IO_PE_INCLUSIVE = ( MYPE .EQ. 0 )

    ! -- Generate the process analysis data: load PA_DEFN module
!   CALL PA_DATAGEN( )

    ! -- Set up horizontal domain and calculate processor-to-subdomain maps for
    ! -- process analysis, if required
    IF ( LIPR .OR. LIRR ) THEN
      IF (aqm_rc_test( .NOT. PAGRD_INIT( NPROCS, MYPE ),      &
        msg = 'cmaq_init: *** Failure defining PA domain configuration', &
        FILE=__FILE__, LINE=__LINE__, rc=rc)) RETURN
    END IF

    ! -- Initialize PCGRID
    if (aqm_rc_test(.not.pcgrid_init(), &
      msg="cmaq_init: Failure defining horizontal domain", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    CGRID => PCGRID( 1:MY_NCOLS,1:MY_NROWS,:,: )   ! required for PinG

  end subroutine cmaq_init

  subroutine cmaq_advance(jdate, jtime, tstep, run_aero, rc)

    integer,           intent(in)    :: jdate, jtime, tstep(3)
    logical,           intent(in)    :: run_aero
    integer, optional, intent(out)   :: rc

    ! -- local variables
    logical, save :: first_run = .true.
    integer :: sdate, stime, nsteps
    CHARACTER( 36 ) :: NMSG = 'After NEXTIME: returned JDATE, JTIME'

    ! -- external methods
    INTERFACE
      SUBROUTINE INITSCEN ( CGRID, STDATE, STTIME, TSTEP, NSTEPS )
        REAL, POINTER             :: CGRID( :,:,:,: )
        INTEGER                   :: STDATE, STTIME
        INTEGER                   :: TSTEP( 3 )
        INTEGER                   :: NSTEPS
      END SUBROUTINE INITSCEN

      SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP )
        REAL, POINTER             :: CGRID( :,:,:,: )
        INTEGER                   :: JDATE, JTIME
        INTEGER                   :: TSTEP( 3 )
      END SUBROUTINE VDIFF
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

    SDATE = JDATE
    STIME = JTIME

    if (first_run) then
      CALL INITSCEN ( CGRID, SDATE, STIME, TSTEP, NSTEPS )
      first_run = .false.
    end if

    ! -- advance all physical and chemical processes on a grid
    CALL VDIFF ( CGRID, JDATE, JTIME, TSTEP )
    
!   CALL NEXTIME ( SDATE, STIME, TSTEP( 2 ) )

    CALL CHEM ( CGRID, JDATE, JTIME, TSTEP )

    if (run_aero) then
      CALL AERO ( CGRID, JDATE, JTIME, TSTEP )
    end if

!   CALL NEXTIME ( JDATE, JTIME, TSTEP( 2 ) )
    WRITE( cmaq_logdev,'(/ 5X, A, I8, I7.6)' ) NMSG, JDATE, JTIME

  end subroutine cmaq_advance

  subroutine cmaq_import(tracers, start_index, rc)
    real(AQM_KIND_R8), intent(in)  :: tracers(:,:,:,:)
    integer,           intent(in)  :: start_index
    integer, optional, intent(out) :: rc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    CGRID = tracers(:,:,:, start_index:start_index + nspcsd - 1)
    write(cmaq_logdev,'("cmaq_import: cgrid - min/max ",2i8,2g16.6)') &
      start_index,start_index + nspcsd - 1, minval(CGRID), maxval(CGRID)

  end subroutine cmaq_import

  subroutine cmaq_export(tracers, start_index, rc)
    real(AQM_KIND_R8), intent(out) :: tracers(:,:,:,:)
    integer,           intent(in)  :: start_index
    integer, optional, intent(out) :: rc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    tracers(:,:,:, start_index:start_index + nspcsd - 1) = CGRID
    write(cmaq_logdev,'("cmaq_export: cgrid - min/max ",2i8,2g16.6)') &
      start_index,start_index + nspcsd - 1, minval(CGRID), maxval(CGRID)

  end subroutine cmaq_export

end module cmaq_mod
