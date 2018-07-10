
!------------------------------------------------------------------------!
!  The Community Multiscale Air Quality (CMAQ) system software is in     !
!  continuous development by various groups and is based on information  !
!  from these groups: Federal Government employees, contractors working  !
!  within a United States Government contract, and non-Federal sources   !
!  including research institutions.  These groups give the Government    !
!  permission to use, prepare derivative works of, and distribute copies !
!  of their work in the CMAQ system to the public and to permit others   !
!  to do so.  The United States Environmental Protection Agency          !
!  therefore grants similar permission to use the CMAQ system software,  !
!  but users are requested to provide copies of derivative works or      !
!  products designed to operate in the CMAQ system to the United States  !
!  Government without restrictions as to use by others.  Software        !
!  that is used with the CMAQ system but distributed under the GNU       !
!  General Public License or the GNU Lesser General Public License is    !
!  subject to their copyright restrictions.                              !
!------------------------------------------------------------------------!


!     SUBROUTINE CMAQ_DRIVER ( ncols_in, nlays_in, mpas_date_in, mpas_time_in, dt_in)
  SUBROUTINE CMAQ_DRIVER ()

    USE RXNS_DATA             ! chemical mechanism data
    USE CGRID_SPCS, ONLY: CGRID_SPCS_INIT, NSPCSD  ! CGRID mechanism species

    IMPLICIT NONE

! Include Files:
    INCLUDE SUBST_FILES_ID    ! I/O definitions and declarations

! External Functions:
    INTEGER, EXTERNAL :: SETUP_LOGDEV

! Local variables:

    INTEGER, SAVE :: LOGDEV   ! FORTRAN unit number for log file
    INTEGER      STDATE       ! starting date,    format YYYYDDD
    INTEGER      STTIME       ! starting time,    format HHMMSS
    INTEGER, SAVE :: TSTEP( 3 ) ! time step vector (HHMMSS)
                                  ! TSTEP(1) = local output step
                                  ! TSTEP(2) = sciproc sync. step (chem)
                                  ! TSTEP(3) = twoway model time step w.r.t. wrf time
                                  !            step and wrf/cmaq call frequency

    INTEGER, ALLOCATABLE, SAVE :: ASTEP( : )
    INTEGER          NSTEPS   ! run duration: number of output time steps
    INTEGER, SAVE :: NREPS    ! number of model time steps per output step
    INTEGER          ISTEP    ! current output time step number
    INTEGER          IREP     ! model step number within this output step
    INTEGER, SAVE :: JDATE    ! current model date, coded YYYYDDD
    INTEGER, SAVE :: JTIME    ! current model time, coded HHMMSS
    INTEGER          C, R, L, K, S, V     ! loop induction variables
    INTEGER          ALLOCSTAT

    CHARACTER(  2 ) :: COLROW = 'CR'  ! col/row arg list order
    CHARACTER( 16 ) :: PNAME = 'DRIVER'
    CHARACTER( 96 ) :: XMSG = ' '

!   REAL, SAVE, POINTER     :: CGRID( :,:,:,: )
    REAL, ALLOCATABLE, SAVE :: CGRID( :,:,:,: )
    REAL, ALLOCATABLE, SAVE :: AGRID( :,:,:,: )
    REAL    DIVFAC      ! trapezoidal average factor
    INTEGER A_NLYS

    INTEGER NPROCS      ! number of processors
    INTEGER ELAPTIME    ! ENDTIME-BEGTIME

    LOGICAL, SAVE :: FIRST_RUN = .TRUE.  ! used for twoway model
    LOGICAL       :: WFLG                ! turn on write subdmap in pio_init

    INTEGER, SAVE :: STEP_COUNT = 0, TOTAL_STEPS = 0
    INTEGER       :: STATUS, RUNLEN
    INTEGER       :: TIME2SEC, SEC2TIME

    integer :: io_mode
    logical :: ncd_64bit_offset

    CHARACTER( 16 ), SAVE :: CTM_PMDIAG   = 'CTM_PMDIAG'
    LOGICAL, SAVE :: PMDIAG

    INTERFACE

         SUBROUTINE INITSCEN ( CGRID, STDATE, STTIME, TSTEP, NSTEPS )
!           REAL, POINTER            :: CGRID( :,:,:,: )
            REAL, INTENT(INOUT)      :: CGRID( :,:,:,: )
            INTEGER, INTENT( OUT )   :: STDATE, STTIME, TSTEP( 3 )
            INTEGER, INTENT( OUT )   :: NSTEPS
         END SUBROUTINE INITSCEN

         SUBROUTINE SCIPROC ( CGRID, JDATE, JTIME, TSTEP, ASTEP )
!           REAL, POINTER            :: CGRID( :,:,:,: )
            REAL, INTENT(INOUT)      :: CGRID( :,:,:,: )
            INTEGER, INTENT( INOUT ) :: JDATE, JTIME
            INTEGER, INTENT( IN )    :: TSTEP( 3 ), ASTEP( : )
         END SUBROUTINE SCIPROC

         SUBROUTINE LOAD_CGRID ( FNAME, STDATE, STTIME, SPC_CAT, CMIN, CGRID )
            CHARACTER( 16 ), INTENT( IN ) :: FNAME
            INTEGER, INTENT( IN )         :: STDATE, STTIME
            CHARACTER(  2 ), INTENT( IN ) :: SPC_CAT
            REAL,    INTENT( IN )         :: CMIN
            REAL, INTENT(INOUT)           :: CGRID( :,:,:,: )
         END SUBROUTINE LOAD_CGRID

         SUBROUTINE UNLOAD_CGRID ( CGRID )
            REAL, INTENT(IN)  :: CGRID( :,:,:,: )
         END SUBROUTINE UNLOAD_CGRID

    END INTERFACE

    ! -- begin

    STEP_COUNT = STEP_COUNT + 1

      IF ( FIRST_RUN ) THEN

         NCOLS = NCOLS_IN
         NROWS = 1
         NLAYS = NLAYS_IN
         TSTEP( 2 ) = SEC2TIME( int(dt_in) )
         TSTEP( 3 ) = SEC2TIME( int(dt_in) )
         STDATE     = MPAS_DATE_IN
         STTIME     = MPAS_TIME_IN

         call get_env (RUNLEN, 'CTM_RUNLEN', 10000)
         TOTAL_STEPS = TIME2SEC( RUNLEN ) / int(dt_in) - 1

         call get_env( cell_num, 'cell_num', 1)

         call mpi_allreduce (ncols, ncols_gl, 1, mpi_int, mpi_sum, mpi_comm_world, status)

         call finit(NPROCS, 1, ncols_gl, nrows)

         JDATE = 0
         JTIME = 0

! Start I/O-API and set up log file(s)
         LOGDEV = SETUP_LOGDEV()

! Set up horizontal domain, calculate processor-to-subdomain maps
! and define vertical layer structure (in module GRID_CONF)
!        IF ( .NOT. GRID_INIT ( NPROCS, MYPE ) ) THEN
!           XMSG = '*** Failure defining domain configuration'
!           CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
!        END IF

! Set CGRID mechanism
! --- NOTE: this must be done only on DE 0

         IF ( .NOT. CGRID_SPCS_INIT() ) THEN
            XMSG = 'Error in CGRID_SPCS:CGRID_SPCS_INIT'
            print *, trim(xmsg); stop
         END IF

         ALLOCATE (CGRID ( NCOLS,NROWS,NLAYS,NSPCSD ), STAT = ALLOCSTAT )

! Initialize conc field: Copy IC's to CONC file as step 0
! Convention: the input file concentration units are always ppmV.

! --- NOTE: the call below is equivelent to:
! ---  CGRID = CMIN = 1.E-30
! ---  TSTEP(1) = 010000 (default) or get_env(TSTEP(1))

         CALL INITSCEN ( CGRID, STDATE, STTIME, TSTEP, NSTEPS )

      STDATE = MPAS_DATE_IN
      STTIME = 0

         JDATE = STDATE; JTIME = STTIME

         ALLOCATE ( ASTEP( NLAYS ), STAT = ALLOCSTAT )
         IF ( ALLOCSTAT .NE. 0 ) THEN
            XMSG = 'ASTEP memory allocation failed'
            print *, trim(XMSG); stop
         END IF

         FIRST_RUN = .FALSE.

        if (.not. lus_init (mminlu_mpas, lufrac_data(:,:,1)) ) then
           print *, ' Error: Cannot initialize Land Use category'
           stop
        end if

        call get_env (ncd_64bit_offset, 'ncd_64bit_offset', .false.)

        if (ncd_64bit_offset) then
!           io_mode = ior (nf90_noclobber, nf90_64bit_offset)
         else
!           io_mode = nf90_noclobber
         end if

        call setfile (EMIS_1)

        call get_env (PMDIAG, CTM_PMDIAG, .true.)

        if (PMDIAG) then
           call fcreate (CTM_PMDIAG_1, io_mode)
        end if
        call fcreate (CTM_DRY_DEP_1, io_mode)
        call fcreate (CTM_DEPV_DIAG, io_mode)
        call fcreate (CTM_RJ_1, io_mode)
        call fcreate (CTM_RJ_2, io_mode)

      END IF ! first_run

      write (logdev, *) ' ==d== cmaq driver b ', STEP_COUNT, TOTAL_STEPS, jdate, jtime, tstep

      if (STEP_COUNT .eq. TOTAL_STEPS) then
         mpas_cmaq_last_step = .true.
      end if

      v = size(cmaq_species, 4)

! --- NOTE: this could be replaced with methods cmaq_import()
      DO L = 1, NLAYS
         DO C = 1, NCOLS
            CGRID( C,1,L,1:V ) = cmaq_species( c,1,l,1:v )
         END DO
      END DO

! Main processing loop:

! Get synchronization and advection time steps, TSTEP(2), ASTEP(L) and NREPS
         NREPS = 1 

! science process sequence:

         DIVFAC = 0.5 / FLOAT( NREPS )

         DO IREP = 1, NREPS

         write (6, '(a25, 10i8)') ' ==d== call cmaq sciproc ', jdate, jtime, tstep, 
     &    STEP_COUNT, TOTAL_STEPS

! --- NOTE: the call below is equivalent to:
! ---  CALL VDIFF ( CGRID, JDATE, JTIME, TSTEP )
! ---  SDATE = JDATE
! ---  STIME = JTIME
! ---  CALL NEXTIME ( SDATE, STIME, TSTEP( 2 ) )
! ---  CALL CHEM ( CGRID, JDATE, JTIME, TSTEP )
! ---  if (run_aero) then
! ---    CALL AERO ( CGRID, JDATE, JTIME, TSTEP )
! ---  end if
! ---  CALL NEXTIME ( JDATE, JTIME, TSTEP( 2 ) )
! ---  WRITE( LOGDEV,'(/ 5X, A, I8, I7.6)' ) NMSG, JDATE, JTIME
! ---  
! ---  ASTEP NOT REQUIRED

            CALL SCIPROC ( CGRID, JDATE, JTIME, TSTEP, ASTEP )

         END DO

! write conc fields

! --- NOTE: this could be replaced with methods cmaq_export()
         CALL UNLOAD_CGRID (CGRID)

  END SUBROUTINE CMAQ_DRIVER

2015  FORMAT( 5X , 'Date and time ', A, ' (', I7, ':', I6.6, ')' )
2021  FORMAT( //5X, 'The elapsed time for this job was', I7.6, ' seconds.' / )
