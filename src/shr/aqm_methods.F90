LOGICAL FUNCTION CHKGRID( FNAME )

  IMPLICIT NONE

  CHARACTER( * ), INTENT( IN )  :: FNAME   ! File name

  CHKGRID = .TRUE.

END FUNCTION CHKGRID


LOGICAL FUNCTION  DSCGRID( GNAME, CNAME,                             &
                           CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT, &
                           XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK )

  USE M3UTILIO,      ONLY : LATGRD3
  USE aqm_model_mod, ONLY : aqm_model_domain_get
  USE aqm_rc_mod,    ONLY : aqm_rc_check

  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN   ) :: GNAME   !  grid  sys name
  CHARACTER*(*), INTENT(  OUT) :: CNAME   !  coord sys name
  INTEGER,       INTENT(  OUT) :: CTYPE   !  coord sys type
  REAL*8 ,       INTENT(  OUT) :: P_ALP   !  first, second, third map
  REAL*8 ,       INTENT(  OUT) :: P_BET   !  projection descriptive
  REAL*8 ,       INTENT(  OUT) :: P_GAM   !  parameters
  REAL*8 ,       INTENT(  OUT) :: XCENT   !  lon for coord-system X=0
  REAL*8 ,       INTENT(  OUT) :: YCENT   !  lat for coord-system Y=0
  REAL*8 ,       INTENT(  OUT) :: XORIG   !  X-coordinate origin of grid (map units)
  REAL*8 ,       INTENT(  OUT) :: YORIG   !  Y-coordinate origin of grid
  REAL*8 ,       INTENT(  OUT) :: XCELL   !  X-coordinate cell dimension
  REAL*8 ,       INTENT(  OUT) :: YCELL   !  Y-coordinate cell dimension
  INTEGER,       INTENT(  OUT) :: NCOLS   !  number of grid columns
  INTEGER,       INTENT(  OUT) :: NROWS   !  number of grid rows
  INTEGER,       INTENT(  OUT) :: NTHIK   !  BOUNDARY:  perimeter thickness (cells)

  ! -- local variables
  integer :: localrc
  integer :: is, ie, js, je

  ! -- begin
  DSCGRID = .FALSE.

  IF ( TRIM(GNAME) .EQ. 'Cubed-Sphere' ) THEN

    call aqm_model_domain_get(ids=is, ide=ie, jds=js, jde=je, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve grid coordinates", &
      file=__FILE__, line=__LINE__)) return

    NCOLS = ie - is + 1
    NROWS = je - js + 1
    NTHIK = 0

    CNAME = ' '
    CTYPE = 0  ! set to zero (unknown) as this is a column model with variable DX1 & DX2
    P_ALP = 0.
    P_BET = 0.
    P_GAM = 0.
    XCENT = 0.
    YCENT = 0.
    XORIG = 0.
    YORIG = 0.
    ! C96 has a 1-degree resolution. Needs to be revised.
    XCELL = 1.
    YCELL = 1.

    DSCGRID = .TRUE.
  END IF

END FUNCTION  DSCGRID


LOGICAL FUNCTION DESC3( FNAME )

  USE M3UTILIO,      ONLY : &
    GDNAM3D, NLAYS3D, NVARS3D, VDESC3D, VGLVS3D, &
    VGSGPN3, VGTOP3D, VGTYP3D, VNAME3D, UNITS3D, &
    NCOLS3D, NROWS3D, SDATE3D, STIME3D, TSTEP3D

  USE aqm_emis_mod
  USE aqm_model_mod, ONLY : aqm_config_type, &
                            aqm_model_get, aqm_model_domain_get
  USE aqm_rc_mod,    ONLY : aqm_rc_check

  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN) :: FNAME

  INCLUDE SUBST_FILES_ID
 
  integer :: localrc
  integer :: is, ie, js, je
  integer :: EMLAYS
  type(aqm_config_type), pointer :: config

  ! -- begin
  nullify(config)

  NVARS3D = 0
  VNAME3D = ""
  UNITS3D = ""
  VDESC3D = ""

  SDATE3D = 0
  STIME3D = 0
  TSTEP3D = 0

  IF ( (TRIM(FNAME) .EQ. TRIM(INIT_GASC_1)) .OR. &
       (TRIM(FNAME) .EQ. TRIM(INIT_AERO_1)) .OR. &
       (TRIM(FNAME) .EQ. TRIM(INIT_NONR_1)) .OR. &
       (TRIM(FNAME) .EQ. TRIM(INIT_TRAC_1)) ) THEN

    ! -- Input initial background values for the following species
    NVARS3D = 3
    VNAME3D( 1:NVARS3D ) = (/ 'ECH4   ', 'CO     ', 'O3     ' /)

    call aqm_model_domain_get(ids=is, ide=ie, jds=js, jde=je, nl=NLAYS3D, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model coordinates", &
      file=__FILE__, line=__LINE__)) return

    NCOLS3D = ie - is + 1
    NROWS3D = je - js + 1

  ELSE IF ( TRIM( FNAME ) .EQ. 'B3GRD' ) THEN

    call aqm_emis_desc("biogenic", NLAYS3D, NVARS3D, VNAME3D, UNITS3D)

  ELSE IF ( TRIM( FNAME ) .EQ. TRIM( EMIS_1 ) ) THEN

    NLAYS3D = 0

    call aqm_emis_desc("gbbepx",        NLAYS=EMLAYS)
    NLAYS3D = MAX(EMLAYS, NLAYS3D)

    call aqm_emis_desc("point-source",  NLAYS=EMLAYS)
    NLAYS3D = MAX(EMLAYS, NLAYS3D)

    call aqm_emis_desc("anthropogenic", NLAYS=EMLAYS, NVARS=NVARS3D, VNAMES=VNAME3D, UNITS=UNITS3D)
    NLAYS3D = MAX(EMLAYS, NLAYS3D)

  ELSE IF ( TRIM( FNAME ) .EQ. TRIM( GRID_DOT_2D ) ) THEN
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
    VDESC3D( NVARS3D ) = 'MODIS NOAH      '

  ELSE IF ( TRIM( FNAME ) .EQ. TRIM( MET_CRO_2D ) ) THEN

    NVARS3D = 35
    VNAME3D( 1:NVARS3D ) = &
    (/ 'PRSFC           ', 'USTAR           ',            &
       'WSTAR           ', 'PBL             ',            &
       'ZRUF            ',                                &
       'HFX             ', 'WSPD10          ',            &
       'GSW             ', 'RGRND           ',            &
       'RN              ', 'RC              ',            &
       'CFRAC           ', 'CLDT            ',            &
       'CLDB            ', 'WBAR            ',            &
       'RA              ', 'RS              ',            &
       'SNOCOV          ', 'VEG             ',            &
       'TEMP2           ', 'WR              ',            &
       'TEMPG           ', 'LAI             ',            &
       'SLTYP           ', 'Q2              ',            &
       'SEAICE          ', 'SOIM1           ',            &
       'SOIM2           ', 'SOIT1           ',            &
       'SOIT2           ', 'LH              ',            &
       'CLAYF           ', 'SANDF           ',            &
       'DRAG            ', 'UTHR            ' /)
    UNITS3D( 1:NVARS3D ) = &
    (/ 'Pascal          ', 'M/S             ',            &
       'M/S             ', 'M               ',            &
       'M               ',                                &
       'WATTS/M**2      ', 'M/S             ',            &
       'WATTS/M**2      ', 'WATTS/M**2      ',            &
       'CM              ', 'CM              ',            &
       'FRACTION        ', 'M               ',            &
       'M               ', 'G/M**3          ',            &
       'S/M             ', 'S/M             ',            &
       'NODIM           ', 'NO UNIT         ',            &
       'K               ', 'M               ',            &
       'K               ', 'AREA/AREA       ',            &
       '-               ', 'KG/KG           ',            &
       'FRACTION        ', 'M**3/M**3       ',            &
       'M**3/M**3       ', 'K               ',            &
       'K               ', 'WATTS/M**2      ',            &
       '1               ', '1               ',            &
       '1               ', 'M/S             ' /)

    call aqm_model_get(config=config, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
      file=__FILE__, line=__LINE__)) return

    SDATE3D = config % ctm_stdate
    STIME3D = config % ctm_sttime
    TSTEP3D = config % ctm_tstep

  ELSE IF ( TRIM( FNAME ) .EQ. TRIM( MET_CRO_3D ) ) THEN

    CALL aqm_model_domain_get(nl=NLAYS3D, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve vertical levels", &
      file=__FILE__, line=__LINE__)) return

    GDNAM3D = 'Cubed-Sphere'
    VGTYP3D = VGSGPN3 ! non-hydrostatic sigma-P
    VGTOP3D = 20. * 101.325
    ! -- actual sigma levels are not required for AQM since transport
    ! -- is performed in the atmosphere. Bogus sigma levels are set
    ! -- to satisfy d(sigma) = 1
    DO is = 1, NLAYS3D+1
      VGLVS3D( is ) = DBLE(NLAYS3D + 1 - is)
    END DO

    NVARS3D = 14
    VNAME3D( 1:NVARS3D ) = &
    (/ 'JACOBF          ', 'JACOBM          ',            &
       'DENSA_J         ', 'TA              ',            &
       'QV              ', 'QC              ',            &
       'PRES            ', 'DENS            ',            &
       'UWINDA          ', 'VWINDA          ',            &
       'ZH              ', 'ZF              ',            &
       'CFRAC_3D        ', 'PRESF           '             &
    /)
    UNITS3D( 1:NVARS3D ) = &
    (/ 'M               ', 'M               ',            &
       'KG/M**2         ', 'K               ',            &
       'KG/KG           ', 'KG/KG           ',            &
       'Pa              ', 'KG/M**3         ',            &
       'M/S             ', 'M/S             ',            &
       'M               ', 'M               ',            &
       'FRACTION        ', 'Pa              '             &
    /)

    call aqm_model_get(config=config, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
      file=__FILE__, line=__LINE__)) return

    SDATE3D = config % ctm_stdate
    STIME3D = config % ctm_sttime
    TSTEP3D = config % ctm_tstep

    if (config % species % p_atm_qr > 0) then
      NVARS3D = NVARS3D + 1
      VNAME3D( NVARS3D ) = 'QR'
      UNITS3D( NVARS3D ) = 'KG/KG'
    end if

    if (config % species % p_atm_qi > 0) then
      NVARS3D = NVARS3D + 1
      VNAME3D( NVARS3D ) = 'QI'
      UNITS3D( NVARS3D ) = 'KG/KG'
    end if

    if (config % species % p_atm_qs > 0) then
      NVARS3D = NVARS3D + 1
      VNAME3D( NVARS3D ) = 'QS'
      UNITS3D( NVARS3D ) = 'KG/KG'
    end if

    if (config % species % p_atm_qg > 0) then
      NVARS3D = NVARS3D + 1
      VNAME3D( NVARS3D ) = 'QG'
      UNITS3D( NVARS3D ) = 'KG/KG'
    end if

  ELSE IF ( TRIM( FNAME ) .EQ. TRIM( MET_DOT_3D ) ) THEN
    NVARS3D = 4
    VNAME3D( 1:NVARS3D ) = &
    (/ 'UWINDC          ', 'VWINDC          ',            &
       'UHAT_JD         ', 'VHAT_JD         '  /)
    UNITS3D( 1:NVARS3D ) = &
    (/ 'M/S             ', 'M/S             ',            &
       'KG/(M*S)        ', 'KG/(M*S)        '  /)

    call aqm_model_get(config=config, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
      file=__FILE__, line=__LINE__)) return

    SDATE3D = config % ctm_stdate
    STIME3D = config % ctm_sttime
    TSTEP3D = config % ctm_tstep

  ELSE IF ( TRIM( FNAME ) .EQ. 'MODIS_FPAR' ) THEN
    NVARS3D = 1
    VNAME3D( 1:NVARS3D ) = &
    (/ 'UWINDC          ', 'VWINDC          ',            &
       'UHAT_JD         ', 'VHAT_JD         '  /)
    UNITS3D( 1:NVARS3D ) = &
    (/ 'M/S             ', 'M/S             ',            &
       'KG/(M*S)        ', 'KG/(M*S)        '  /)

    DESC3 = .TRUE.
    RETURN

  END IF

  DESC3 = ( NVARS3D > 0 )

END FUNCTION DESC3


logical function envyn(name, description, defaultval, status)

  use aqm_emis_mod,  only : aqm_internal_emis_type, &
                            aqm_emis_get, aqm_emis_ispresent
  use aqm_model_mod, only : aqm_config_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check

  implicit none

  character(len=*), intent(in)  :: name
  character(len=*), intent(in)  :: description
  logical,          intent(in)  :: defaultval
  integer,          intent(out) :: status

  ! -- local variables
  integer :: deCount, localrc
  type(aqm_config_type), pointer :: config
  type(aqm_internal_emis_type), pointer :: em

  ! -- begin
  envyn = defaultval
  status = 0

  nullify(config)

  call aqm_model_get(deCount=deCount, config=config, rc=localrc)
  if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) then
    status = -2
    return
  end if

  if (deCount < 1) return

  select case (trim(name))
    case ('BIOSW_YN')
      envyn = config % biosw_yn
    case ('SUMMER_YN')
      envyn = .false.
      em => aqm_emis_get("biogenic")
      if (associated(em)) envyn = (trim(em % period) == "summer")
    case ('CTM_AOD')
      envyn = config % ctm_aod
    case ('CTM_BIOGEMIS')
      envyn = aqm_emis_ispresent("biogenic")
    case ('CTM_DEPVFILE')
      envyn = config % ctm_depvfile
    case ('CTM_PMDIAG')
      envyn = config % ctm_pmdiag
    case ('CTM_PHOTODIAG')
      envyn = config % ctm_photodiag
    case ('CTM_PT3DEMIS')
      envyn = aqm_emis_ispresent("gbbepx") .or. &
              aqm_emis_ispresent("point-source")
    case ('CTM_GRAV_SETL')
      envyn = .false.
    case ('CTM_WBDUST_FENGSHA')
      envyn = aqm_emis_ispresent("fengsha")
    case ('CTM_WB_DUST')
      envyn = config % ctm_wb_dust
    case ('MIE_OPTICS')
      envyn = config % mie_optics
    case ('INITIAL_RUN')
      envyn = .true.
    case default
      status = -2
  end select

end function envyn


integer function envint(name, description, defaultval, status)

  use aqm_emis_mod,  only : aqm_internal_emis_type, aqm_emis_get
  use aqm_model_mod, only : aqm_config_type, aqm_model_get, &
                            aqm_model_domain_get
  use aqm_rc_mod,    only : aqm_rc_check
  use m3utilio,      only : xstat0

  implicit none

  character(len=*), intent(in)  :: name
  character(len=*), intent(in)  :: description
  integer,          intent(in)  :: defaultval
  integer,          intent(out) :: status

  ! -- local variables
  integer :: deCount, localrc
  type(aqm_config_type), pointer :: config
  type(aqm_internal_emis_type), pointer :: em

  ! -- begin

  envint = defaultval
  status = xstat0

  nullify(config)

  call aqm_model_get(deCount=deCount, config=config, rc=localrc)
  if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) then
    status = -2
    return
  end if

  if (deCount < 1) return

  select case (trim(name))
    case ('CTM_TSTEP')
      envint = config % ctm_tstep
    case ('CTM_EMLAYS')
      envint = 1
      em => aqm_emis_get("gbbepx")
      if (associated(em)) then
        if (trim(em % plumerise) /= "none") then
          call aqm_model_domain_get(nl=envint, rc=localrc)
          if (aqm_rc_check(localrc, msg="Failure to retrieve model coordinates", &
            file=__FILE__, line=__LINE__)) return
        end if
      end if
    case default
      ! -- use default
  end select

end function envint


REAL FUNCTION ENVREAL( LNAME, DESC, DEFAULT, STAT )

  USE AQM_EMIS_MOD,  ONLY : AQM_INTERNAL_EMIS_TYPE, AQM_EMIS_GET
  USE AQM_MODEL_MOD, ONLY : AQM_CONFIG_TYPE, AQM_MODEL_GET, &
                            AQM_MODEL_DOMAIN_GET
  USE AQM_RC_MOD,    ONLY : AQM_RC_CHECK

  IMPLICIT NONE

  CHARACTER*(*), INTENT(IN   ) :: LNAME
  CHARACTER*(*), INTENT(IN   ) :: DESC
  REAL         , INTENT(IN   ) :: DEFAULT
  INTEGER      , INTENT(  OUT) :: STAT

  ! -- local variables
  INTEGER                               :: LOCALRC
  TYPE(AQM_INTERNAL_EMIS_TYPE), POINTER :: EM
  TYPE(AQM_CONFIG_TYPE),        POINTER :: CONFIG

  ! -- begin
  ENVREAL = DEFAULT
  STAT = 0

  SELECT CASE ( TRIM(LNAME) )
    CASE ( 'CTM_WBDUST_FENGSHA_ALPHA' )
      EM => AQM_EMIS_GET("fengsha")
      IF (ASSOCIATED(EM)) ENVREAL = EM % SCALEFACTOR
    CASE ( 'FG_DUST_SCALE' )
      NULLIFY(CONFIG)
      CALL AQM_MODEL_GET(CONFIG=CONFIG, RC=LOCALRC)
      IF (AQM_RC_CHECK(LOCALRC, FILE=__FILE__, LINE=__LINE__)) THEN
         STAT = -2
         RETURN
      END IF
      ENVREAL = CONFIG % FG_DUST_SCALE
    CASE DEFAULT
      ! Nothing to do
  END SELECT

END FUNCTION ENVREAL


SUBROUTINE ENVSTR( LNAME, DESC, DEFAULT, EQNAME, STAT )
  USE m3utilio, ONLY : XSTAT0
  use aqm_emis_mod, ONLY : aqm_internal_emis_type, aqm_emis_get
  IMPLICIT NONE
  CHARACTER*(*), INTENT(IN   ) :: LNAME
  CHARACTER*(*), INTENT(IN   ) :: DESC
  CHARACTER*(*), INTENT(IN   ) :: DEFAULT
  CHARACTER*(*), INTENT(  OUT) :: EQNAME
  INTEGER      , INTENT(  OUT) :: STAT
  TYPE(aqm_internal_emis_type), POINTER :: em

  EQNAME = ""
  STAT = XSTAT0

  SELECT CASE ( TRIM(LNAME) )
    CASE ( 'GRID_NAME' )
      EQNAME = 'Cubed-Sphere'
    CASE ( 'BIOG_SPRO' )
      NULLIFY(em)
      em => aqm_emis_get("biogenic")
      IF (ASSOCIATED(em)) EQNAME = em % specprofile
    CASE DEFAULT
      EQNAME = DEFAULT
  END SELECT

END SUBROUTINE ENVSTR


INTEGER FUNCTION PROMPTFFILE( PROMPT, RDONLY, FMTTED, DEFAULT, CALLER )

  use aqm_emis_mod,  ONLY : aqm_internal_emis_type, aqm_emis_get
  USE aqm_model_mod, ONLY : aqm_config_type, aqm_model_get
  USE aqm_rc_mod,    ONLY : aqm_rc_check, aqm_rc_test

  IMPLICIT NONE

! ARGUMENTS and their descriptions:

  CHARACTER*(*), INTENT(IN   ) :: PROMPT         !  prompt for user
  LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
  LOGICAL      , INTENT(IN   ) :: FMTTED         !  TRUE iff file should be formatted
  CHARACTER*(*), INTENT(IN   ) :: DEFAULT        !  default logical file name
  CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging messages

  INTEGER :: IDEV
  INTEGER :: deCount, localrc
  TYPE(aqm_config_type), pointer :: config
  TYPE(aqm_internal_emis_type), POINTER :: em

  INTEGER, EXTERNAL :: GETEFILE

  PROMPTFFILE = -1

  IF ( TRIM(DEFAULT) .EQ. 'GSPRO' ) THEN

    NULLIFY(em)
    em => aqm_emis_get("biogenic")
    IF (ASSOCIATED(em)) THEN

      IDEV = GETEFILE( DEFAULT, RDONLY, FMTTED, CALLER )
      IF ( aqm_rc_test( ( IDEV .LT. 0 ), &
        MSG = 'Could not open input file "' // TRIM( em % specfile ) // '".', &
        file=__FILE__, line=__LINE__)) RETURN

        PROMPTFFILE = IDEV
      END IF

  END IF

END FUNCTION PROMPTFFILE


subroutine nameval(name, eqname)

  use aqm_emis_mod,  only : aqm_internal_emis_type, aqm_emis_get
  use aqm_model_mod, only : aqm_config_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check

  implicit none

  character(len=*), intent(in)  :: name
  character(len=*), intent(out) :: eqname

  ! -- local variables
  integer :: deCount, localrc
  type(aqm_config_type), pointer :: config
  type(aqm_internal_emis_type), pointer :: em

  ! -- begin
  eqname = ""

  nullify(config)

  call aqm_model_get(deCount=deCount, config=config, rc=localrc)
  if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) return

  if (deCount < 1) return

  select case (trim(name))
    case ('gc_matrix_nml')
      eqname = config % gc_matrix_nml
    case ('ae_matrix_nml')
      eqname = config % ae_matrix_nml
    case ('nr_matrix_nml')
      eqname = config % nr_matrix_nml
    case ('tr_matrix_nml')
      eqname = config % tr_matrix_nml
    case ('CSQY_DATA')
      eqname = config % csqy_data
    case ('GSPRO')
      nullify(em)
      em => aqm_emis_get("biogenic")
      if (associated(em)) eqname = em % specfile
    case ('OPTICS_DATA')
      eqname = config % optics_data
    case ('OMI')
      eqname = config % omi
    case default
      ! -- nothing to do
  end select
  
end subroutine nameval


logical function interpx( fname, vname, pname, &
  col0, col1, row0, row1, lay0, lay1, jdate, jtime, buffer )

  use aqm_types_mod, only : AQM_KIND_R4, AQM_KIND_R8, AQM_MAXSTR
  use aqm_rc_mod,    only : aqm_rc_check, aqm_rc_test
  use aqm_emis_mod,  only : aqm_emis_read
  use aqm_model_mod, only : aqm_config_type, aqm_state_type, &
                            aqm_model_get, aqm_model_domain_get
  use aqm_const_mod, only : eps1, grav, onebg, rdgas

  USE M3UTILIO,      ONLY : M3MESG

  implicit none

  character(len=*), intent(in)  :: fname, vname, pname
  integer,          intent(in)  :: col0, col1, row0, row1, lay0, lay1
  integer,          intent(in)  :: jdate, jtime
  real,             intent(out) :: buffer(*)

  ! -- local variables
  integer :: localrc
  integer :: c, r, l, k, n, nl
  integer :: lbuf, lu_index
  logical :: set_non_neg
  character(len=16)         :: varname
  character(len=AQM_MAXSTR) :: msgString
  real(AQM_KIND_R8), dimension(:,:),   pointer     :: lat, lon
  real(AQM_KIND_R8), dimension(:,:),   pointer     :: p2d
  real(AQM_KIND_R8), dimension(:,:,:), pointer     :: p3d
  type(aqm_config_type),               pointer     :: config
  type(aqm_state_type),                pointer     :: stateIn

  ! -- constants
  include SUBST_FILES_ID

  logical, parameter :: debug = .true.

  ! -- begin
  interpx = .false.

  lbuf = (col1-col0+1) * (row1-row0+1) * (lay1-lay0+1)
  buffer(1:lbuf) = 0.

  nullify(p2d)
  nullify(p3d)
  nullify(config)
  nullify(stateIn)
  set_non_neg = .false.

  if (trim(fname) == trim(GRID_CRO_2D)) then

    call aqm_model_get(stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
      file=__FILE__, line=__LINE__)) return

    call aqm_model_domain_get(lon=lon, lat=lat, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve grid coordinates", &
      file=__FILE__, line=__LINE__)) return

    if (vname(1:7) == 'LUFRAC_') then
      lu_index = 0
      read(vname(8:9), *, iostat=localrc) lu_index
      if (aqm_rc_test(localrc /= 0, msg="Failure to identify LU_INDEX", &
        file=__FILE__, line=__LINE__)) return
      k = 0
      do r = row0, row1
        do c = col0, col1
          k = k + 1
          if (int(stateIn % stype(c,r)) == lu_index) buffer(k) = 1.0
        end do
      end do
    else
      select case (trim(vname))
        case ('HT')
          p2d => stateIn % ht
        case ('LAT')
          p2d => lat
        case ('LON')
          k = 0
          do r = row0, row1
            do c = col0, col1
              k = k + 1
              if (lon(c,r) > 180.) then
                buffer(k) = lon(c,r) - 360.
              else
                buffer(k) = lon(c,r)
              end if
            end do
          end do
        case ('LWMASK')
          k = 0
          do r = row0, row1
           do c = col0, col1
             k = k + 1
             buffer(k) = stateIn % slmsk(c,r)
             if (nint(buffer(k)) == 2) buffer(k) = 0.  ! set sea ice points as water
           end do
          end do
        case ('MSFX2')
          buffer(1:lbuf) = 1.
        case ('PURB')
        case default
          return
      end select
    end if

  else if (trim(fname) == trim(MET_CRO_2D)) then

    call aqm_model_get(stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
      file=__FILE__, line=__LINE__)) return
    
    call aqm_model_get(config=config, stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
      file=__FILE__, line=__LINE__)) return

    select case (trim(vname))
      case ("HFX")
        p2d => stateIn % hfx
      case ("LAI")
        p2d => stateIn % xlai
      case ("LH")
        p2d => stateIn % lh
      case ("PRSFC")
        p2d => stateIn % psfc
      case ("PBL")
        p2d => stateIn % hpbl
      case ("Q2")
        p2d => stateIn % q2m
      case ("RADYNI")
        p2d => stateIn % cmm
      case ("RSTOMI")
        k = 0
        do r = row0, row1
         do c = col0, col1
           k = k + 1
           if ( stateIn % rc(c,r) /= 0.0 ) buffer(k) = 1.0 / stateIn % rc(c,r)
         end do
        end do
      case ("RA")
        k = 0
        do r = row0, row1
         do c = col0, col1
           k = k + 1
           buffer(k) = sqrt(stateIn % uwind(c,r,1) * stateIn % uwind(c,r,1) +  &
                            stateIn % vwind(c,r,1) * stateIn % vwind(c,r,1)) / &
                       ( stateIn % ustar(c,r) * stateIn % ustar(c,r) )
         end do
        end do
      case ("RS")
        p2d => stateIn % rc
      case ("RC")
        k = 0
        do r = row0, row1
         do c = col0, col1
           k = k + 1
           buffer(k) = 100. * stateIn % rainc(c,r)
         end do
        end do
      case ("RGRND")
        p2d => stateIn % rgrnd
      case ("RN")
        k = 0
        do r = row0, row1
         do c = col0, col1
           k = k + 1
           buffer(k) = 100. * (stateIn % rain(c,r) - stateIn % rainc(c,r))
         end do
        end do
        set_non_neg = .true.
      case ("SEAICE")
        p2d => stateIn % fice
      case ("SLTYP")
        p2d => stateIn % stype
      case ("SNOCOV")
        p2d => stateIn % sncov
      case ("SOIM1")
        p2d => stateIn % smois(:,:,1)
      case ("SOIM2")
        p2d => stateIn % smois(:,:,2)
      case ("SOIT1")
        p2d => stateIn % stemp(:,:,1)
      case ("SOIT2")
        p2d => stateIn % stemp(:,:,2)
      case ("TEMPG")
        p2d => stateIn % tsfc
      case ("TEMP2")
        p2d => stateIn % t2m
      case ("USTAR")
        p2d => stateIn % ustar
      case ("VEG")
        p2d => stateIn % vfrac
      case ("WR")
        p2d => stateIn % wr
      case ("WSPD10")
        k = 0
        do r = row0, row1
         do c = col0, col1
           k = k + 1
           buffer(k) = sqrt(stateIn % u10m(c,r) * stateIn % u10m(c,r) &
                          + stateIn % v10m(c,r) * stateIn % v10m(c,r))
         end do
        end do
      case ("ZRUF")
        k = 0
        do r = row0, row1
         do c = col0, col1
           k = k + 1
           buffer(k) = 0.01 * stateIn % zorl(c,r)
         end do
        end do
      case ("CLAYF","DRAG","SANDF","UTHR")
        ! -- read in fengsha variables
        call aqm_emis_read("fengsha", vname, buffer, rc=localrc)
        if (aqm_rc_test((localrc /= 0), &
          msg="Failure to read fengsha input for " // vname, &
          file=__FILE__, line=__LINE__)) return
      case default
    !   return
    end select

  else if (trim(fname) == trim(OCEAN_1)) then

    select case (trim(vname))
      case ("OPEN")
        call aqm_model_get(stateIn=stateIn, rc=localrc)
        if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
          file=__FILE__, line=__LINE__)) return
        ! -- set to complement to land mask
        k = 0
        do r = row0, row1
         do c = col0, col1
           k = k + 1
           buffer(k) = 1.0 - stateIn % slmsk(c,r)
           if (nint(stateIn % slmsk(c,r)) == 2) buffer(k) = 1.0  ! set sea ice points as water
         end do
        end do
      case ("SURF")
        ! -- zero
      case default
        return
    end select

  else if (trim(fname) == trim(EMIS_1)) then

    ! -- read in emissions
    call aqm_emis_read("anthropogenic", vname, buffer, rc=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Failure to read emissions for " // vname, &
      file=__FILE__, line=__LINE__)) return

  else if (trim(fname) == trim(MET_CRO_3D)) then

    call aqm_model_get(config=config, stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
      file=__FILE__, line=__LINE__)) return

    select case (trim(vname))
      case ("JACOBF")
        call aqm_model_domain_get(nl=nl, rc=localrc)
        if (aqm_rc_check(localrc, msg="Failure to retrieve model coordinates", &
          file=__FILE__, line=__LINE__)) return
        k = 0
        do l = lay0, lay1
          n = min(l + 1, nl)
          do r = row0, row1
            do c = col0, col1
              k = k + 1
              buffer(k) = onebg * (stateIn % phil(c,r,n) - stateIn % phil(c,r,n-1))
            end do
          end do
        end do
      case ("JACOBM")
        k = 0
        do l = lay0, lay1
          do r = row0, row1
            do c = col0, col1
              k = k + 1
              buffer(k) = onebg * (stateIn % phii(c,r,l+1) - stateIn % phii(c,r,l))
            end do
          end do
        end do
      case ("DENS")
        k = 0
        do l = lay0, lay1
          do r = row0, row1
            do c = col0, col1
              k = k + 1
              buffer(k) = stateIn % prl(c,r,l) / ( rdgas * stateIn % temp(c,r,l) )
            end do
          end do
        end do
      case ("DENSA_J")
        k = 0
        do l = lay0, lay1
          do r = row0, row1
            do c = col0, col1
              k = k + 1
              ! -- rho
              buffer(k) = stateIn % prl(c,r,l) / ( rdgas * stateIn % temp(c,r,l) )
              ! -- Jacobian
              buffer(k) = buffer(k) &
                          * onebg * (stateIn % phii(c,r,l+1) - stateIn % phii(c,r,l))
            end do
          end do
        end do
      case ("PRES")
        p3d => stateIn % prl
      case ("PRESF")
        p3d => stateIn % pri
      case ("CFRAC_3D")
        p3d => stateIn % cldfl
      case ("PV")
        buffer(1:lbuf) = 1.0
      case ("QV")
        p3d => stateIn % tr(:,:,:,config % species % p_atm_qv)
        set_non_neg = .true.
      case ("QC")
        p3d => stateIn % tr(:,:,:,config % species % p_atm_qc)
        set_non_neg = .true.
      case ("QR")
        if (config % species % p_atm_qr > 0) then
          p3d => stateIn % tr(:,:,:,config % species % p_atm_qr)
          set_non_neg = .true.
        end if
      case ("QI")
        if (config % species % p_atm_qi > 0) then
          p3d => stateIn % tr(:,:,:,config % species % p_atm_qi)
          set_non_neg = .true.
        end if
      case ("QS")
        if (config % species % p_atm_qs > 0) then
          p3d => stateIn % tr(:,:,:,config % species % p_atm_qs)
          set_non_neg = .true.
        end if
      case ("QG")
        if (config % species % p_atm_qg > 0) then
          p3d => stateIn % tr(:,:,:,config % species % p_atm_qg)
          set_non_neg = .true.
        end if
      case ("UWINDA")
        p3d => stateIn % uwind
      case ("VWINDA")
        p3d => stateIn % vwind
      case ("ZF")
        k = 0
        do l = lay0, lay1
          do r = row0, row1
            do c = col0, col1
              k = k + 1
              buffer(k) = onebg * stateIn % phii(c,r,l+1)
            end do
          end do
        end do
        set_non_neg = .true.
      case ("ZH")
        k = 0
        do l = lay0, lay1
          do r = row0, row1
            do c = col0, col1
              k = k + 1
              buffer(k) = onebg * stateIn % phil(c,r,l)
            end do
          end do
        end do
        set_non_neg = .true.
      case ("TA")
        p3d => stateIn % temp
      case default
        ! set to 0
    end select

  else if (trim(fname) == trim(MET_DOT_3D)) then

    select case (trim(vname))
      case ("UWINDC")
        ! u-wind is on C grid, while imported wind component are on A grid
        ! this needs to be fixed
        ! set to 0 for now
      case ("VWINDC")
        ! set to 0 for now
    end select

  else

    return

  end if

  if (associated(p2d)) then
    k = 0
    do r = row0, row1
      do c = col0, col1
        k = k + 1
        buffer(k) = p2d(c,r)
      end do
    end do
  else if (associated(p3d)) then
    k = 0
    do l = lay0, lay1
      do r = row0, row1
        do c = col0, col1
          k = k + 1
          buffer(k) = p3d(c,r,l)
        end do
      end do
    end do
  end if

  if (set_non_neg) buffer(1:lbuf) = max( 0., buffer(1:lbuf) )

  interpx = .true.

  call aqm_model_get(config=config, rc=localrc)
  if (aqm_rc_check(localrc, msg="Failure to retrieve model configuration", &
    file=__FILE__, line=__LINE__)) return

  if (config % verbose) then
    varname = vname
    write(msgString, '(a,": interpx: ",a16,": ",a16,": min/max = ",2g20.8)') &
      trim(config % name), fname, varname, &
      minval(buffer(1:lbuf)), maxval(buffer(1:lbuf))
    call m3mesg(msgString)
  end if

end function interpx


LOGICAL FUNCTION  XTRACT3 ( FNAME, VNAME,                           &
                            LAY0, LAY1, ROW0, ROW1, COL0, COL1,     &
                            JDATE, JTIME, BUFFER )

  use aqm_types_mod, only : AQM_KIND_R4
  use aqm_model_mod, only : aqm_state_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check, aqm_rc_test
  use aqm_const_mod, only : con_mr2ppm_o3, thrs_p_strato
  use aqm_emis_mod,  only : aqm_emis_read
  use aqm_config_mod

  implicit none

  !!...........   ARGUMENTS and their descriptions:
  CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name
  CHARACTER*(*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
  INTEGER      , INTENT(IN   ) :: LAY0            !  lower layer bound
  INTEGER      , INTENT(IN   ) :: LAY1            !  upper layer bound
  INTEGER      , INTENT(IN   ) :: ROW0            !  lower row bound
  INTEGER      , INTENT(IN   ) :: ROW1            !  upper row bound
  INTEGER      , INTENT(IN   ) :: COL0            !  lower col bound
  INTEGER      , INTENT(IN   ) :: COL1            !  upper col bound
  INTEGER      , INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
  INTEGER      , INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
  REAL         , INTENT(  OUT) :: BUFFER(*)       !  output buffer array

  ! -- local variables
  integer :: localrc
  integer :: c, r, l, k, lbuf, lu_index
  type(aqm_config_type),  pointer :: config
  type(aqm_state_type),   pointer :: stateIn

  include SUBST_FILES_ID

  ! -- begin

  nullify(config)
  nullify(stateIn)

  lbuf = (LAY1-LAY0+1)*(ROW1-ROW0+1)*(COL1-COL0+1)
  BUFFER(1:lbuf) = 0.
  XTRACT3 = .TRUE.

  IF (TRIM(FNAME) == TRIM(GRID_CRO_2D)) THEN

    call aqm_model_get(stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
      file=__FILE__, line=__LINE__)) return

    if (vname(1:7) == 'LUFRAC_') then
      if (aqm_rc_test((LAY0.NE.1).OR.(LAY1.NE.1), &
        msg=TRIM(VNAME)//" is 2D. LAY0 and LAY1 must be 1", &
        file=__FILE__, line=__LINE__)) return
      lu_index = 0
      read(vname(8:), *, iostat=localrc) lu_index
      if (aqm_rc_test(localrc /= 0, msg="Failure to identify LU_INDEX", &
        file=__FILE__, line=__LINE__)) return
      k = 0
      do r = row0, row1
        do c = col0, col1
          k = k + 1
          if (int(stateIn % stype(c,r)) == lu_index) buffer(k) = 1.0
        end do
      end do
    end if

  ELSE IF (TRIM(FNAME) .EQ. 'MODIS_FPAR') THEN

    IF (TRIM(VNAME) .EQ. 'MODIS_FPAR_T') THEN

      if (aqm_rc_test((LAY0.NE.1).OR.(LAY1.NE.1), &
        msg=TRIM(VNAME)//" is 2D. LAY0 and LAY1 must be 1", &
        file=__FILE__, line=__LINE__)) return

      call aqm_model_get(stateIn=stateIn, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrieve model input state", &
        file=__FILE__, line=__LINE__)) return

      k = 0
      do r = row0, row1
        do c = col0, col1
          k = k + 1
          buffer(k) = real(stateIn % vfrac(c,r))
        end do
      end do

    END IF

  ELSE IF ( TRIM(FNAME) .EQ. TRIM(INIT_GASC_1) ) THEN

    ! -- initialize gas-phase species (ppmV)
    SELECT CASE (TRIM(VNAME))
      CASE ('ECH4')
        BUFFER(1:lbuf) = 1.80
      CASE ('CO')
        BUFFER(1:lbuf) = 60.E-03
      CASE ('O3')
        call aqm_model_get(config=config, stateIn=stateIn, rc=localrc)
        if (aqm_rc_check(localrc, msg="Failure to retrieve model config", &
          file=__FILE__, line=__LINE__)) return
        k = 0
        do l = lay0, lay1
          do r = row0, row1
            do c = col0, col1
              k = k + 1
              if (stateIn % prl(c,r,l) < thrs_p_strato) then
                buffer(k) = con_mr2ppm_o3 * stateIn % tr(c,r,l,config % species % p_atm_o3)
              else
                buffer(k) = 40.E-03
              end if
            end do
          end do
        end do
    END SELECT

  ELSE IF ( TRIM(FNAME) .EQ. 'B3GRD' ) THEN

    ! -- read in biogenic emissions
    call aqm_emis_read("biogenic", vname, buffer, rc=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Failure to read emissions for " // vname, &
      file=__FILE__, line=__LINE__)) return

  END IF

END FUNCTION  XTRACT3


SUBROUTINE SUBHFILE ( FNAME, GXOFF, GYOFF, &
                      STRTCOL, ENDCOL, STRTROW, ENDROW )

  USE aqm_model_mod, ONLY : aqm_model_domain_get
  USE aqm_rc_mod,    ONLY : aqm_rc_check

  IMPLICIT NONE

  CHARACTER( 16 ), INTENT( IN )  :: FNAME
  INTEGER,         INTENT( OUT ) :: GXOFF ! X global origin offset from file (.ge. 0)
  INTEGER,         INTENT( OUT ) :: GYOFF ! Y global origin offset from file (.ge. 0)
  INTEGER,         INTENT( OUT ) :: STRTCOL ! local processor start colum in file
  INTEGER,         INTENT( OUT ) ::  ENDCOL ! local processor end colum in file
  INTEGER,         INTENT( OUT ) :: STRTROW ! local processor start row in file
  INTEGER,         INTENT( OUT ) ::  ENDROW ! local processor end row in file

  INTEGER :: localrc
  INTEGER :: is, ie, js, je

  GXOFF = 0
  GYOFF = 0

  STRTCOL = 0
   ENDCOL = 0
  STRTROW = 0
   ENDROW = 0

  call aqm_model_domain_get(ids=is, ide=ie, jds=js, jde=je, rc=localrc)
  if (aqm_rc_check(localrc, msg="Failure to retrieve grid coordinates", &
    file=__FILE__, line=__LINE__)) return

  STRTCOL = 1
   ENDCOL = ie - is + 1
  STRTROW = 1
   ENDROW = je - js + 1

END SUBROUTINE SUBHFILE

LOGICAL FUNCTION WRITE3_REAL2D( FNAME, VNAME, JDATE, JTIME, BUFFER )

  USE M3UTILIO,      ONLY : ALLVAR3
  USE aqm_model_mod, ONLY : aqm_state_type, &
                            aqm_model_get
  USE aqm_rc_mod,    ONLY : aqm_rc_check

  INCLUDE SUBST_FILES_ID

  CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
  CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
  INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
  INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
  REAL         , INTENT(IN   ) :: BUFFER(:,:)  !  output buffer array

  integer :: localrc
  type(aqm_state_type), pointer :: stateOut

  WRITE3_REAL2D = .TRUE.

  IF ( TRIM( FNAME ) .EQ. TRIM( CTM_AOD_1 ) ) THEN

    WRITE3_REAL2D = .FALSE.

    IF ( TRIM( VNAME ) .EQ. TRIM( ALLVAR3 ) ) THEN

      nullify(stateOut)
      call aqm_model_get(stateOut=stateOut, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrieve model output state", &
        file=__FILE__, line=__LINE__)) return

      stateOut % aod = BUFFER

    END IF

    WRITE3_REAL2D = .TRUE.

  END IF

END FUNCTION WRITE3_REAL2D

LOGICAL FUNCTION WRITE3_REAL4D( FNAME, VNAME, JDATE, JTIME, BUFFER )

  USE M3UTILIO,      ONLY : ALLVAR3
  USE aqm_model_mod, ONLY : aqm_config_type, &
                            aqm_state_type, &
                            aqm_model_get
  USE aqm_rc_mod,    ONLY : aqm_rc_check

  INCLUDE SUBST_FILES_ID

  CHARACTER*(*), INTENT(IN   ) :: FNAME      !  logical file name
  CHARACTER*(*), INTENT(IN   ) :: VNAME      !  logical file name
  INTEGER      , INTENT(IN   ) :: JDATE      !  date, formatted YYYYDDD
  INTEGER      , INTENT(IN   ) :: JTIME      !  time, formatted HHMMSS
  REAL         , INTENT(IN   ) :: BUFFER(:,:,:,:)  !  output buffer array

  integer :: localrc
  integer :: s
  type(aqm_state_type),  pointer :: stateOut
  type(aqm_config_type), pointer :: config

  integer, parameter :: p_pm25at = 23

  WRITE3_REAL4D = .TRUE.

  IF ( TRIM( FNAME ) .EQ. TRIM( CTM_PMDIAG_1 ) ) THEN

    WRITE3_REAL4D = .FALSE.

    IF ( TRIM( VNAME ) .EQ. TRIM( ALLVAR3 ) ) THEN

      nullify(config)
      nullify(stateOut)
      call aqm_model_get(config=config, stateOut=stateOut, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrieve model output state", &
        file=__FILE__, line=__LINE__)) return

      do s = 0, config % species % ndiag - 2
        stateOut % tr(:,:,:,config % species % p_diag_beg + s) = &
          buffer(:,:,:,p_pm25at + s)
      end do

    END IF

    WRITE3_REAL4D = .TRUE.

  END IF

END FUNCTION WRITE3_REAL4D

! -- dummy subroutines

SUBROUTINE DUMMY_AQCHEM ( JDATE, JTIME, TEMP, PRES_PA, TAUCLD, PRCRATE, &
                          WCAVG, WTAVG, AIRM, ALFA0, ALFA2, ALFA3, GAS, &
                          AEROSOL, GASWDEP, AERWDEP, HPWDEP, BETASO4, DARK )
  INTEGER,   INTENT( IN )  :: JDATE
  INTEGER,   INTENT( IN )  :: JTIME
  REAL,      INTENT( IN )  :: AIRM
  REAL,      INTENT( IN )  :: ALFA0
  REAL,      INTENT( IN )  :: ALFA2
  REAL,      INTENT( IN )  :: ALFA3
  REAL,      INTENT( OUT ) :: HPWDEP
  REAL( 8 ), INTENT( OUT ) :: BETASO4
  REAL,      INTENT( IN )  :: PRCRATE
  REAL,      INTENT( IN )  :: PRES_PA
  REAL,      INTENT( IN )  :: TAUCLD
  REAL,      INTENT( IN )  :: TEMP
  REAL,      INTENT( IN )  :: WCAVG
  REAL,      INTENT( IN )  :: WTAVG
  REAL( 8 ), INTENT( INOUT ) :: GAS    ( : )
  REAL( 8 ), INTENT( INOUT ) :: AEROSOL( :,: )
  REAL( 8 ), INTENT( INOUT ) :: GASWDEP( : )
  REAL( 8 ), INTENT( INOUT ) :: AERWDEP( :,: )
  LOGICAL,   INTENT( IN )    :: DARK
  BETASO4 = 0.0D0
  HPWDEP  = 0.0
END SUBROUTINE DUMMY_AQCHEM

SUBROUTINE DUMMY_CONVCLD_ACM ( CGRID, JDATE, JTIME, TSTEP, &
                               N_SPC_WDEP, WDEP_MAP, CONV_DEP, SUBTRANS )
  REAL, POINTER            :: CGRID( :,:,:,: )
  INTEGER, INTENT( IN )    :: JDATE
  INTEGER, INTENT( IN )    :: JTIME
  INTEGER, INTENT( IN )    :: TSTEP( 3 )
  INTEGER, INTENT( IN )    :: N_SPC_WDEP
  INTEGER, INTENT( IN )    :: WDEP_MAP( : )
  REAL,    INTENT( INOUT ) :: CONV_DEP( :,:,: )
  REAL,    INTENT( OUT )   :: SUBTRANS( :,:,: )
  CONV_DEP = 0.0
  SUBTRANS = 1.0
END SUBROUTINE DUMMY_CONVCLD_ACM

SUBROUTINE DUMMY_EDDYX ( EDDYV )
  REAL,   INTENT( OUT ) :: EDDYV ( :,:,: )
  EDDYV = 0.0
END SUBROUTINE DUMMY_EDDYX

SUBROUTINE DUMMY_VDIFFACMX( dtsec, seddy, ddep, icmp, ddepj, ddepj_fst, cngrd )
  IMPLICIT NONE
  REAL, INTENT( IN )    :: dtsec
  REAL, INTENT( INOUT ) :: seddy( :,:,: )
  REAL, INTENT( INOUT ) :: ddep ( :,:,: )
  REAL, INTENT( INOUT ) :: icmp ( :,:,: )
  REAL, INTENT( INOUT ), OPTIONAL :: ddepj    ( :,:,:,: )
  REAL, INTENT( INOUT ), OPTIONAL :: ddepj_fst( :,:,:,: )
  REAL, INTENT( INOUT ) :: cngrd( :,:,:,: )
END SUBROUTINE DUMMY_VDIFFACMX

SUBROUTINE DUMMY_OPCONC( JDATE, JTIME, JSTEP )
  INTEGER      JDATE        ! starting date (YYYYDDD)
  INTEGER      JTIME        ! starting time (HHMMSS)
  INTEGER      TSTEP        ! output timestep (HHMMSS)
END SUBROUTINE DUMMY_OPCONC

SUBROUTINE DUMMY_OPACONC( JDATE, JTIME, JSTEP )
  INTEGER, INTENT (IN ) :: JDATE        ! current model date, coded YYYYDDD
  INTEGER, INTENT (IN ) :: JTIME        ! current model time, coded HHMMSS
  INTEGER, INTENT (IN ) :: JSTEP        ! output timestep (HHMMSS)
END SUBROUTINE DUMMY_OPACONC

SUBROUTINE DUMMY_OPWDEP( JDATE, JTIME, JSTEP )
  INTEGER, INTENT (IN ) :: JDATE        ! current model date, coded YYYYDDD
  INTEGER, INTENT (IN ) :: JTIME        ! current model time, coded HHMMSS
  INTEGER, INTENT (IN ) :: JSTEP        ! output timestep (HHMMSS)
END SUBROUTINE DUMMY_OPWDEP

SUBROUTINE DUMMY_WR_INIT ( CGRID, STDATE, STTIME, TSTEP )
  REAL, POINTER         :: CGRID(:,:,:,:)
  INTEGER, INTENT( IN ) :: STDATE, STTIME, TSTEP
END SUBROUTINE DUMMY_WR_INIT
