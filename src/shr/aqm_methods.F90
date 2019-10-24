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
    CTYPE = LATGRD3
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
    VGSGPN3, VGTOP3D, VGTYP3D, VNAME3D, UNITS3D
   
  USE aqm_emis_mod
  USE aqm_model_mod, ONLY : aqm_config_type, &
                            aqm_model_get, aqm_model_domain_get
  USE aqm_rc_mod,    ONLY : aqm_rc_check

  IMPLICIT NONE

  CHARACTER(LEN=*), INTENT(IN) :: FNAME

  INCLUDE SUBST_FILES_ID
 
  integer :: localrc
  type(aqm_config_type), pointer :: config => null()

  NVARS3D = 0
  VNAME3D = ""
  UNITS3D = ""
  VDESC3D = ""

  IF      ( TRIM( FNAME ) .EQ. TRIM( EMIS_1 ) ) THEN

    ! -- These species are used in MPAS 5.1 EPA-modified
    ! NVARS3D = 31
    ! VNAME3D( 1:NVARS3D ) = &
    !   (/ 'CO     ', 'NO     ', 'NO2    ', 'NH3    ', 'SO2    ', &
    !      'SULF   ', 'CH4    ', 'ALD2   ', 'ALDX   ', 'ETH    ', &
    !      'ETHA   ', 'ETOH   ', 'FORM   ', 'IOLE   ', 'ISOP   ', &
    !      'MEOH   ', 'NVOL   ', 'OLE    ', 'PAR    ', 'TERP   ', &
    !      'TOL    ', 'UNK    ', 'UNR    ', 'XYL    ', 'BENZENE', &
    !      'SESQ   ', 'CL2    ', 'HCL    ', 'HONO   ', 'HGNRVA ', &
    !      'PHGI   ', 'HGIIGAS', 'PMC    ', 'PEC    ', 'PMFINE ', &
    !      'PNO3   ', 'POC    ', 'PSO4   ', 'PCL    ', 'PNH4   ', &
    !      'PNA    ', 'PMG    ', 'PK     ', 'PCA    ', 'PNCOM  ', &
    !      'PFE    ', 'PAL    ', 'PSI    ', 'PMN    ', 'PH2O   ', &
    !      'PMOTHR ', 'PTI    ', 'PCL_B  ' /)
    ! UNITS3D( 1:NVARS3D ) = 'MOL/S'

    NVARS3D = aqm_emis_num
    VNAME3D( 1:NVARS3D ) = aqm_emis_def( 1:NVARS3D, 1 )
    UNITS3D( 1:NVARS3D ) = aqm_emis_def( 1:NVARS3D, 2 )

   ! -- missing emission species
    NVARS3D = NVARS3D + aqm_emis_sup_num
    VNAME3D( aqm_emis_num+1:NVARS3D ) = aqm_emis_sup_def( 1:aqm_emis_sup_num, 1)
    UNITS3D( aqm_emis_num+1:NVARS3D ) = aqm_emis_sup_def( 1:aqm_emis_sup_num, 2)

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

    NVARS3D = 31
    VNAME3D( 1:NVARS3D ) = &
    (/ 'PRSFC           ', 'USTAR           ',            &
       'WSTAR           ', 'PBL             ',            &
       'ZRUF            ', &
       'HFX             ', 'WSPD10          ',            &
       'GSW             ', 'RGRND           ',            &
       'RNA             ', 'RCA             ',            &
       'CFRAC           ', 'CLDT            ',            &
       'CLDB            ', 'WBAR            ',            &
       'RA              ', 'RS              ',            &
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
       'M               ', &
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
       'K               ', 'WATTS/M**2      ' /)

  ELSE IF ( TRIM( FNAME ) .EQ. TRIM( MET_CRO_3D ) ) THEN

    CALL aqm_model_domain_get(nl=NLAYS3D, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrieve vertical levels", &
      file=__FILE__, line=__LINE__)) return

    GDNAM3D = 'Cubed-Sphere'
    VGTYP3D = VGSGPN3 ! non-hydrostatic sigma-P
    VGTOP3D = 20.
    VGLVS3D( 1:NLAYS3D+1) = &
    (/ &
       1.00000000000000,        0.994670137967030,       &
       0.988630030551305,       0.981795355947387,       &
       0.974074829179211,       0.965370569372193,       &
       0.955579045414836,       0.944592287197078,       &
       0.932299594647352,       0.918589853166181,       &
       0.903354556653176,       0.886491478900351,       &
       0.867909171686985,       0.847532021746212,       &
       0.825305882291101,       0.801203957859928,       &
       0.775232608753270,       0.747436709629337,       &
       0.717903956645773,       0.686767568199990,       &
       0.654206972126252,       0.620446045471596,       &
       0.585748535469621,       0.550410960071072,       &
       0.514753123680963,       0.479106813575342,       &
       0.443803755579685,       0.409163370230986,       &
       0.375481639620453,       0.343021632871033,       &
       0.312006338485267,       0.282614364066433,       &
       0.254978018417156,       0.229183937796752,       &
       0.205273975252949,       0.183250541893293,       &
       0.163082923261438,       0.144713204521001,       &
       0.128062075677410,       0.113034552990968,       &
       9.952492552934208E-002,  8.742129366270174E-002,  &
       7.660920966635407E-002,  6.697471990523667E-002,  &
       5.840827204975076E-002,  5.080841024628596E-002,  &
       4.407969004491388E-002,  3.813288583979074E-002,  &
       3.288544494348749E-002,  2.826156655643848E-002,  &
       2.419217215339815E-002,  2.061463896155175E-002,  &
       1.747249395390158E-002,  1.471507822911011E-002,  &
       1.229704358126450E-002,  1.017797739499531E-002,  &
       8.321978184689799E-003,  6.697171906618627E-003,  &
       5.275396081141108E-003,  4.031725976013030E-003,  &
       2.944237698040571E-003,  1.993563989931395E-003,  &
       1.162726420216179E-003,  4.367701495483935E-004,  &
       0.000000000000000E+000 &
    /)

    NVARS3D = 11
    VNAME3D( 1:NVARS3D ) = &
    (/ 'JACOBF          ', 'JACOBM          ',            &
       'DENSA_J         ', 'TA              ',            &
       'QV              ', 'QC              ',            &
       'PRES            ', 'DENS            ',            &
       'ZH              ', 'ZF              ',            &
       'CFRAC_3D        '                                 &
    /)
    UNITS3D( 1:NVARS3D ) = &
    (/ 'M               ', 'M               ',            &
       'KG/M**2         ', 'K               ',            &
       'KG/KG           ', 'KG/KG           ',            &
       'Pa              ', 'KG/M**3         ',            &
       'M               ', 'M               ',            &
       'FRACTION        '                                 &
    /)

    call aqm_model_get(config=config, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
      file=__FILE__, line=__LINE__)) return

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

  use aqm_model_mod, only : aqm_config_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check

  implicit none

  character(len=*), intent(in)  :: name
  character(len=*), intent(in)  :: description
  logical,          intent(in)  :: defaultval
  integer,          intent(out) :: status

  ! -- local variables
  integer :: deCount, localrc
  type(aqm_config_type), pointer :: config => null()

  ! -- begin
  envyn = defaultval
  status = 0

  call aqm_model_get(deCount=deCount, config=config, rc=localrc)
  if (aqm_rc_check(localrc, file=__FILE__, line=__LINE__)) then
    status = -2
    return
  end if

  if (deCount < 1) return

  select case (trim(name))
    case ('CTM_DEPVFILE')
      envyn = config % ctm_depvfile
    case ('CTM_PMDIAG')
      envyn = config % ctm_pmdiag
    case ('CTM_PHOTODIAG')
      envyn = config % ctm_photodiag
    case ('CTM_GRAV_SETL')
      envyn = .false.
    case default
      status = -2
  end select

end function envyn


integer function envint(name, description, defaultval, status)
  use m3utilio, only : xstat0
  implicit none
  character(len=*), intent(in)  :: name
  character(len=*), intent(in)  :: description
  integer,          intent(in)  :: defaultval
  integer,          intent(out) :: status
  envint = 0
  status = xstat0
end function envint


REAL FUNCTION ENVREAL( LNAME, DESC, DEFAULT, STAT )
  IMPLICIT NONE
  CHARACTER*(*), INTENT(IN   ) :: LNAME
  CHARACTER*(*), INTENT(IN   ) :: DESC
  REAL         , INTENT(IN   ) :: DEFAULT
  INTEGER      , INTENT(  OUT) :: STAT
  ENVREAL = DEFAULT
  STAT = 0
END FUNCTION ENVREAL


SUBROUTINE ENVSTR( LNAME, DESC, DEFAULT, EQNAME, STAT )
  USE m3utilio, ONLY : XSTAT0
  IMPLICIT NONE
  CHARACTER*(*), INTENT(IN   ) :: LNAME
  CHARACTER*(*), INTENT(IN   ) :: DESC
  CHARACTER*(*), INTENT(IN   ) :: DEFAULT
  CHARACTER*(*), INTENT(  OUT) :: EQNAME
  INTEGER      , INTENT(  OUT) :: STAT

  EQNAME = ''
  STAT = XSTAT0

  IF ( TRIM(LNAME) .EQ. 'GRID_NAME' ) EQNAME = 'Cubed-Sphere'

END SUBROUTINE ENVSTR


subroutine nameval(name, eqname)

  use aqm_model_mod, only : aqm_config_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check

  implicit none

  character(len=*), intent(in)  :: name
  character(len=*), intent(out) :: eqname

  ! -- local variables
  integer :: deCount, localrc
  type(aqm_config_type), pointer :: config => null()

  ! -- begin
  eqname = ""

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

  use aqm_types_mod, only : AQM_KIND_R4, AQM_KIND_R8
  use aqm_rc_mod,    only : aqm_rc_check, aqm_rc_test
  use aqm_emis_mod,  only : aqm_emis_read
  use aqm_model_mod, only : aqm_config_type, aqm_state_type, &
                            aqm_model_get, aqm_model_domain_get

  implicit none

  character(len=*), intent(in)  :: fname, vname, pname
  integer,          intent(in)  :: col0, col1, row0, row1, lay0, lay1
  integer,          intent(in)  :: jdate, jtime
  real,             intent(out) :: buffer(*)

  ! -- local variables
  integer :: localrc
  integer :: c, r, l, k
  integer :: lbuf, lu_index
  logical :: set_non_neg
  real(AQM_KIND_R4), dimension(:,:),   allocatable :: buf2d
  real(AQM_KIND_R8), dimension(:,:),   pointer     :: lat, lon
  real(AQM_KIND_R8), dimension(:,:),   pointer     :: p2d     => null()
  real(AQM_KIND_R8), dimension(:,:,:), pointer     :: p3d     => null()
  type(aqm_config_type),               pointer     :: config  => null()
  type(aqm_state_type),                pointer     :: stateIn => null()

  ! -- constants
  include SUBST_CONST
  include SUBST_FILES_ID

  logical, parameter :: debug = .true.
  real,    parameter :: EPS1 = RWVAP/RDGAS - 1.

  ! -- begin
  interpx = .false.

  lbuf = (col1-col0+1) * (row1-row0+1) * (lay1-lay0+1)
  buffer(1:lbuf) = 0.

  nullify(p2d)
  nullify(p3d)
  set_non_neg = .false.

  if (trim(fname) == trim(GRID_CRO_2D)) then

    call aqm_model_get(stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
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
    if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
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
      case default
    !   return
    end select

  else if (trim(fname) == trim(OCEAN_1)) then

    select case (trim(vname))
      case ("OPEN")
        ! -- zero
      case ("SZONE")
        buffer(1:lbuf) = 1.0
      case ("SURF")
        buffer(1:lbuf) = 1.0
      case default
        return
    end select

  else if (trim(fname) == trim(EMIS_1)) then

    ! -- read in emissions
    allocate(buf2d(col0:col1,row0:row1), stat=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Failure to allocate emission buffer", &
      file=__FILE__, line=__LINE__)) return

    call aqm_emis_read(vname, jdate, jtime, buf2d, rc=localrc)
    if (.not.aqm_rc_check(localrc, &
      msg="Failure to read emissions for " // vname, &
      file=__FILE__, line=__LINE__)) then

      k = 0
      do r = row0, row1
        do c = col0, col1
          k = k + 1
          buffer(k) = buf2d(c,r)
        end do
      end do

    end if

    deallocate(buf2d, stat=localrc)
    if (aqm_rc_test((localrc /= 0), &
      msg="Failure to deallocate emission buffer", &
      file=__FILE__, line=__LINE__)) return

  else if (trim(fname) == trim(MET_CRO_3D)) then

    call aqm_model_get(config=config, stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
      file=__FILE__, line=__LINE__)) return

    select case (trim(vname))
      case ("JACOBF")
        buffer(1:lbuf) = 1.0
      case ("JACOBM")
        buffer(1:lbuf) = 1.0
      case ("DENS")
        k = 0
        do l = lay0, lay1
          do r = row0, row1
            do c = col0, col1
              k = k + 1
              buffer(k) = stateIn % prl(c,r,l) / ( RDGAS * stateIn % temp(c,r,l) &
                * ( 1.0 + EPS1 * stateIn % tr(c,r,l,config % species % p_atm_qv) ) )
            end do
          end do
        end do
      case ("DENSA_J")
        buffer(1:lbuf) = 1.0
      case ("PRES")
        p3d => stateIn % prl
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
      case ("ZF")
        k = 0
        do l = lay0, lay1
          do r = row0, row1
            do c = col0, col1
              k = k + 1
              buffer(k) = stateIn % phil(c,r,l) / GRAV
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
              buffer(k) = stateIn % phii(c,r,l) / GRAV
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

    call aqm_model_get(stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
      file=__FILE__, line=__LINE__)) return

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

  if (debug) write(6,'(a,": ",a10," - min/max ",2g20.6)') &
    trim(fname), trim(vname), minval(buffer(1:lbuf)), maxval(buffer(1:lbuf))

end function interpx


LOGICAL FUNCTION  XTRACT3 ( FNAME, VNAME,                           &
                            LAY0, LAY1, ROW0, ROW1, COL0, COL1,     &
                            JDATE, JTIME, BUFFER )

  use aqm_model_mod, only : aqm_state_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check, aqm_rc_test

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
  integer :: c, r, k, lbuf, lu_index
  type(aqm_state_type),  pointer :: stateIn => null()

  include SUBST_FILES_ID

  ! -- begin

  lbuf = (LAY1-LAY0+1)*(ROW1-ROW0+1)*(COL1-COL0+1)
  BUFFER(1:lbuf) = 0.
  XTRACT3 = .TRUE.

  IF (TRIM(FNAME) == TRIM(GRID_CRO_2D)) THEN

    call aqm_model_get(stateIn=stateIn, rc=localrc)
    if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
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
      if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
        file=__FILE__, line=__LINE__)) return

      k = 0
      do r = row0, row1
        do c = col0, col1
          k = k + 1
          buffer(k) = real(stateIn % vfrac(c,r))
        end do
      end do

    END IF

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

! -- dummy subroutines

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
