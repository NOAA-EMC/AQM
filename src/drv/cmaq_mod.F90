module cmaq_mod

  use aqm_rc_mod
  use aqm_types_mod

  use PCGRID_DEFN
  use HGRD_DEFN, ONLY : NCOLS, NROWS, MY_NCOLS, MY_NROWS
  use VGRD_DEFN, ONLY : NLAYS

  use ASX_DATA_MOD, only : CMAQ_Grid_Data => Grid_Data, &
                           CMAQ_Met_Data  => Met_Data

  use cgrid_spcs,  only : cgrid_spcs_init, nspcsd

  use UTILIO_DEFN, only : INIT3

  implicit none

  integer :: cmaq_logdev

  real, pointer :: CGRID(:,:,:,:) => null()

  private

  public :: CMAQ_Grid_Data, CMAQ_Met_Data
  public :: cmaq_logdev
  public :: NCOLS, NROWS, NLAYS

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

    nspecies = nspcsd

  end subroutine cmaq_species_read

  subroutine cmaq_init(rc)

    integer, optional, intent(out)   :: rc

    ! -- local variables
    real, parameter :: CMIN = 1.0E-30

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- set CMAQ log unit
    cmaq_logdev = INIT3()

    ! -- local computational domain same as local domain
    MY_NROWS = NROWS
    MY_NCOLS = NCOLS

    ! -- Initialize PCGRID
    if (aqm_rc_test(.not.pcgrid_init(), &
      msg="cmaq_init: Failure defining horizontal domain", &
      file=__FILE__, line=__LINE__, rc=rc)) return

    CGRID => PCGRID   ! required for PinG

   ! -- Initialize conc field: Copy IC's to CONC file as step 0
   ! -- Convention: the input file concentration units are always ppmV.
   CGRID = CMIN

  end subroutine cmaq_init

  subroutine cmaq_advance(jdate, jtime, tstep, run_aero, rc)

    integer,           intent(in)    :: jdate, jtime, tstep(3)
    logical,           intent(in)    :: run_aero
    integer, optional, intent(out)   :: rc

    ! -- local variables
    integer :: sdate, stime
    CHARACTER( 36 ) :: NMSG = 'After NEXTIME: returned JDATE, JTIME'

    ! -- external methods
    INTERFACE
      SUBROUTINE VDIFF ( CGRID, JDATE, JTIME, TSTEP )
        REAL, INTENT(INOUT)       :: CGRID( :,:,:,: )
        INTEGER, INTENT( IN )     :: JDATE, JTIME
        INTEGER, INTENT( IN )     :: TSTEP( 3 )
      END SUBROUTINE VDIFF
      SUBROUTINE CHEM ( CGRID, JDATE, JTIME, TSTEP )
        REAL, INTENT(INOUT)       :: CGRID( :,:,:,: )
        INTEGER, INTENT( IN )     :: JDATE, JTIME
        INTEGER, INTENT( IN )     :: TSTEP( 3 )
      END SUBROUTINE CHEM
      SUBROUTINE AERO ( CGRID, JDATE, JTIME, TSTEP )
        REAL, INTENT(INOUT)       :: CGRID( :,:,:,: )
        INTEGER, INTENT( IN )     :: JDATE, JTIME
        INTEGER, INTENT( IN )     :: TSTEP( 3 )
      END SUBROUTINE AERO
    END INTERFACE

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- advance all physical and chemical processes on a grid
    CALL VDIFF ( CGRID, JDATE, JTIME, TSTEP )
    
    SDATE = JDATE
    STIME = JTIME
    CALL NEXTIME ( SDATE, STIME, TSTEP( 2 ) )

    CALL CHEM ( CGRID, JDATE, JTIME, TSTEP )

    if (run_aero) then
      CALL AERO ( CGRID, JDATE, JTIME, TSTEP )
    end if

    CALL NEXTIME ( JDATE, JTIME, TSTEP( 2 ) )
    WRITE( cmaq_logdev,'(/ 5X, A, I8, I7.6)' ) NMSG, JDATE, JTIME

  end subroutine cmaq_advance

  subroutine cmaq_import(tracers, start_index, rc)
    real(AQM_KIND_R8), intent(in)  :: tracers(:,:,:,:)
    integer,           intent(in)  :: start_index
    integer, optional, intent(out) :: rc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    CGRID = tracers(:,:,:, start_index:start_index + nspcsd - 1)

  end subroutine cmaq_import

  subroutine cmaq_export(tracers, start_index, rc)
    real(AQM_KIND_R8), intent(out) :: tracers(:,:,:,:)
    integer,           intent(in)  :: start_index
    integer, optional, intent(out) :: rc

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    tracers(:,:,:, start_index:start_index + nspcsd - 1) = CGRID

  end subroutine cmaq_export

end module cmaq_mod
