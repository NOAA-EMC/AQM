module cmaq_mod

  use aqm_rc_mod
  use aqm_types_mod
! use aqm_const_mod,  only : cp, grvity, mwdry, mw_so2_aer, mw_so4_aer, &
!                             p1000, rd, epsilc
! use aqm_tracers_mod

  use VGRD_DEFN, ONLY : CMAQ_NLAYS => NLAYS
  use HGRD_DEFN, ONLY : CMAQ_NCOLS => NCOLS, &
                        CMAQ_NROWS => NROWS

  use ASX_DATA_MOD, only : CMAQ_Grid_Data => Grid_Data, &
                           CMAQ_Met_Data  => Met_Data

  use cgrid_spcs,  only : cgrid_spcs_init, nspcsd
  use UTILIO_DEFN, only : INIT3

  implicit none

  integer :: cmaq_logdev

  private

  public :: cmaq_advance
  public :: cmaq_init
  public :: cmaq_species_read
  public :: CMAQ_NCOLS, CMAQ_NROWS, CMAQ_NLAYS
  public :: CMAQ_Grid_Data, CMAQ_Met_Data
  public :: cmaq_logdev

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


  subroutine cmaq_init(cgrid, run_aero, rc)

    real(AQM_KIND_R4), intent(inout) :: cgrid(:,:,:,:)
    logical,           intent(in)    :: run_aero
    integer, optional, intent(out)   :: rc

    ! -- local variables
    real(AQM_KIND_R4), parameter :: cmin = 1.0E-30

    ! -- begin
    if (present(rc)) rc = AQM_RC_SUCCESS

    ! -- set CMAQ log unit
    cmaq_logdev = INIT3()

    ! -- set initial concentrations
    cgrid = cmin

    ! -- initialize processes
    ! -- ... see driver.F
!   CALL VDIFF ( CGRID, JDATE, JTIME, TSTEP, INIT=.TRUE. )

    ! -- initialize met data
  ! call INIT_MET( JDATE, JTIME )
    
  end subroutine cmaq_init

  subroutine cmaq_advance(cgrid, jdate, jtime, tstep, run_aero, rc)

    real(AQM_KIND_R4), intent(inout) :: cgrid(:,:,:,:)
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
!   if (aqm_rc_test((cmaq_rc /= CMAQ_RC_SUCCESS), &
!     msg=CMAQ_XMSG, file=__FILE__, line=__LINE__, rc=rc)) return
    
    SDATE = JDATE
    STIME = JTIME
    CALL NEXTIME ( SDATE, STIME, TSTEP( 2 ) )

    CALL CHEM ( CGRID, JDATE, JTIME, TSTEP )
!   if (aqm_rc_test((cmaq_rc /= CMAQ_RC_SUCCESS), &
!     msg=CMAQ_XMSG, file=__FILE__, line=__LINE__, rc=rc)) return

    if (run_aero) then
      CALL AERO ( CGRID, JDATE, JTIME, TSTEP )
!     if (aqm_rc_test((cmaq_rc /= CMAQ_RC_SUCCESS), &
!       msg=CMAQ_XMSG, file=__FILE__, line=__LINE__, rc=rc)) return
    end if

    CALL NEXTIME ( JDATE, JTIME, TSTEP( 2 ) )
    WRITE( cmaq_logdev,'(/ 5X, A, I8, I7.6)' ) NMSG, JDATE, JTIME

  end subroutine cmaq_advance

end module cmaq_mod
