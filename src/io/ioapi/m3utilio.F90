module m3utilio

  implicit none

!.......   Dimensioning parameters:

  integer, parameter :: mxvars3 = 540      !  max number of variables per file

!...........   Token-values ("magic numbers"):

  integer, parameter :: default_logdev = 6

  integer, parameter :: fsread3 = 1        !  OPEN3() flag:  "old read-only" file

  integer, parameter :: xstat0 = 0, &      !  Normal, successful completion
                        xstat1 = 1, &      !  File I/O error
                        xstat2 = 2, &      !  Execution error
                        xstat3 = 3         !  Special  error

!...........   Scalars:

  integer            :: nvars3d            ! number of species

!...........   Arrays:

  integer            :: vtype3d( mxvars3 ) ! variable type:  M3(INT|REAL|DBLE)
  character(len=16)  :: vname3d( mxvars3 ) ! variable names (length MXDLEN3=80)
  character(len=16)  :: units3d( mxvars3 ) !   "   units or 'none' (MXDLEN3=80)
  character(len=80)  :: vdesc3d( mxvars3 ) !   "      descriptions (MXDLEN3=80)

!...........   EXTERNAL Functions:

  INTEGER           :: INTERP3
  INTEGER, EXTERNAL :: INDEX1, SEC2TIME, TIME2SEC, GETEFILE
  INTEGER, EXTERNAL :: SECSDIFF
  LOGICAL, EXTERNAL :: ENVYN

!...........   INTERFACE Blocks:

  INTERFACE INTERPX
    logical function interpx_2d( fname, vname, pname, &
      col0, col1, row0, row1, lay0, lay1, jdate, jtime, buffer )
      character(len=*), intent(in)  :: fname, vname, pname
      integer,          intent(in)  :: col0, col1, row0, row1
      integer,          intent(in)  :: lay0, lay1
      integer,          intent(in)  :: jdate, jtime
      real,             intent(out) :: buffer(:,:)
    end function interpx_2d
    logical function interpx_3d( fname, vname, pname, &
      col0, col1, row0, row1, lay0, lay1, jdate, jtime, buffer )
      character(len=*), intent(in)  :: fname, vname, pname
      integer,          intent(in)  :: col0, col1, row0, row1
      integer,          intent(in)  :: lay0, lay1
      real,             intent(out) :: buffer(:,:,:)
    end function interpx_3d
  END INTERFACE INTERPX

  public

contains

  LOGICAL FUNCTION DESC3( FNAME )
    CHARACTER(LEN=*), INTENT(IN) :: FNAME

    INCLUDE SUBST_FILES_ID

    NVARS3D = 0
    VNAME3D = ""
    UNITS3D = ""

    IF      ( TRIM( FNAME ) .EQ. TRIM( GRID_DOT_2D ) ) THEN
      NVARS3D = 1
      VNAME3D( 1:NVARS3D ) = &
      (/ 'MSFD2           '                      /)
      UNITS3D( 1:NVARS3D ) = &
      (/ '(M/M)**2        '                      /)

    ELSE IF ( TRIM( FNAME ) .EQ. TRIM( GRID_CRO_2D ) ) THEN
      NVARS3D = 7
      VNAME3D( 1:NVARS3D ) = &
      (/ 'LAT             ', 'LON             ',            &
         'MSFX2           ', 'HT              ',            &
         'LWMASK          ', 'PURB            ',            &
         'DLUSE           '                      /)
      UNITS3D( 1:NVARS3D ) = &
      (/ 'DEGREES         ', 'DEGREES         ',            &
         '(M/M)**2        ', 'M               ',            &
         '-               ', 'PERCENT         ',            &
         'CATEGORY        '                      /)

    ELSE IF ( TRIM( FNAME ) .EQ. TRIM( MET_CRO_2D ) ) THEN
      NVARS3D = 32
      VNAME3D( 1:NVARS3D ) = &
      (/ 'PRSFC           ', 'USTAR           ',            &
         'WSTAR           ', 'PBL             ',            &
         'ZRUF            ', 'MOLI            ',            &
         'HFX             ', 'RA              ',            &
         'RS              ', 'WSPD10          ',            &
         'GSW             ', 'RGRND           ',            &
         'RNA             ', 'RCA             ',            &
         'CFRAC           ', 'CLDT            ',            &
         'CLDB            ', 'WBAR            ',            &
         'SNOCOV          ', 'VEG             ',            &
         'TEMP2           ', 'WR              ',            &
         'TEMPG           ', 'LAI             ',            &
         'SLTYP           ', 'Q2              ',            &
         'SEAICE          ', 'SOIM1           ',            &
         'SOIM2           ', 'SOIT1           ',            &
         'SOIT2           ', 'LH              ' /)
      UNITS3D( 1:NVARS3D ) = &
      (/ 'Pascal          ', 'M/S             ',            &
         'M/S             ', 'M               ',            &
         'M               ', '1/M             ',            &
         'WATTS/M**2      ', 'S/M             ',            &
         'S/M             ', 'M/S             ',            &
         'WATTS/M**2      ', 'WATTS/M**2      ',            &
         'CM              ', 'CM              ',            &
         'FRACTION        ', 'M               ',            &
         'M               ', 'G/M**3          ',            &
         'NODIM           ', 'NO UNIT         ',            &
         'K               ', 'M               ',            &
         'K               ', 'AREA/AREA       ',            &
         '-               ', 'KG/KG           ',            &
         'FRACTION        ', 'M**3/M**3       ',            &
         'M**3/M**3       ', 'K               ',            &
         'K               ', 'WATTS/M**2      ' /)

    ELSE IF ( TRIM( FNAME ) .EQ. TRIM( MET_CRO_3D ) ) THEN
      NVARS3D = 15
      VNAME3D( 1:NVARS3D ) = &
      (/ 'JACOBF          ', 'JACOBM          ',            &
         'DENSA_J         ', 'TA              ',            &
         'QV              ', 'QC              ',            &
         'QR              ', 'QI              ',            &
         'QS              ', 'QG              ',            &
         'PRES            ', 'DENS            ',            &
         'ZH              ', 'ZF              ',            &
         'PV              '                      /)
      UNITS3D( 1:NVARS3D ) = &
      (/ 'M               ', 'M               ',            &
         'KG/M**2         ', 'K               ',            &
         'KG/KG           ', 'KG/KG           ',            &
         'KG/KG           ', 'KG/KG           ',            &
         'KG/KG           ', 'KG/KG           ',            &
         'Pa              ', 'KG/M**3         ',            &
         'M               ', 'M               ',            &
         'M^2*K/KG/S * E-6'                      /)

    ELSE IF ( TRIM( FNAME ) .EQ. TRIM( MET_DOT_3D ) ) THEN
      NVARS3D = 4
      VNAME3D( 1:NVARS3D ) = &
      (/ 'UWINDC          ', 'VWINDC          ',            &
         'UHAT_JD         ', 'VHAT_JD         '  /)
      UNITS3D( 1:NVARS3D ) = &
      (/ 'M/S             ', 'M/S             ',            &
         'KG/(M*S)        ', 'KG/(M*S)        '  /)

    END IF

    DESC3 = ( NVARS3D > 0 )

  END FUNCTION DESC3

  integer function init3()
    init3 = default_logdev
  end function init3

  LOGICAL FUNCTION  OPEN3( FNAME, FSTATUS, PGNAME )
    CHARACTER(LEN=*), INTENT(IN) :: FNAME
    INTEGER,          INTENT(IN) :: FSTATUS
    CHARACTER(LEN=*), INTENT(IN) :: PGNAME
    OPEN3 = .TRUE.
  END FUNCTION  OPEN3

  subroutine m3exit( caller, jdate, jtime, msgtxt, xstat )
    use ESMF
    character(len=*),  intent(in) :: caller
    integer,           intent(in) :: jdate, jtime
    character(len=*),  intent(in) :: msgtxt
    integer, optional, intent(in) :: xstat

    call ESMF_LogSetError(ESMF_RC_INTNRL_BAD, msg=trim(caller) // ':' // trim(msgtxt))
    call ESMF_Finalize(endflag=ESMF_END_ABORT)
  end subroutine m3exit

  subroutine m3mesg( message )
    use ESMF
    character(len=*), intent(in) :: message

    call ESMF_LogWrite(trim(message), ESMF_LOGMSG_INFO)
  end subroutine m3mesg

  subroutine m3parag( nmesg, msgs )
    integer,          intent(in) :: nmesg
    character(len=*), intent(in) :: msgs( nmesg )

    ! -- local variables
    integer :: n

    do n = 1, nmesg
      call m3mesg( msgs(n) )
    end do
  end subroutine m3parag

  subroutine m3warn( caller, jdate, jtime, msgtxt )
    use ESMF
    character(len=*), intent(in) :: caller
    integer,          intent(in) :: jdate, jtime
    character(len=*), intent(in) :: msgtxt

    call ESMF_LogWrite(trim(caller) // ':' // trim(msgtxt), ESMF_LOGMSG_WARNING)
  end subroutine m3warn

end module m3utilio
