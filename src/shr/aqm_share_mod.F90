logical function envyn(name, description, defaultval, status)

  use aqm_model_mod, only : aqm_config_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check

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
    case default
      status = -2
  end select

end function envyn

subroutine nameval(name, eqname)

  use aqm_model_mod, only : aqm_config_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check

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

logical function interpx_2d( fname, vname, pname, &
  col0, col1, row0, row1, lay0, lay1, jdate, jtime, buffer )

  use aqm_types_mod, only : AQM_KIND_R8
  use aqm_model_mod, only : aqm_state_type, &
                            aqm_model_get, aqm_model_domain_get
  use aqm_rc_mod,    only : aqm_rc_check

  implicit none

  character(len=*), intent(in)  :: fname, vname, pname
  integer,          intent(in)  :: col0, col1, row0, row1, lay0, lay1
  integer,          intent(in)  :: jdate, jtime
  real,             intent(out) :: buffer(:,:)

  ! -- local variables
  integer :: localrc
  real(AQM_KIND_R8), dimension(:,:), pointer :: lat, lon
  type(aqm_state_type), pointer :: stateIn
  logical, parameter :: debug = .true.

  ! -- begin
  interpx_2d = .false.
  buffer = 0.

  select case (trim(fname))

    case ("GRID_CRO_2D")
      call aqm_model_get(stateIn=stateIn, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
        file=__FILE__, line=__LINE__)) return

      call aqm_model_domain_get(lon=lon, lat=lat, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrieve grid coordinates", &
        file=__FILE__, line=__LINE__)) return

      select case (trim(vname))
        case ('HT')
          buffer = stateIn % ht
        case ('LAT')
          buffer = lat
        case ('LON')
          buffer = lon
          where(buffer > 180.) buffer = buffer - 360.
        case ('LWMASK')
          buffer = stateIn % slmsk
        case ('PURB')
        case default
          return
      end select

    case ("MET_CRO_2D")

      call aqm_model_get(stateIn=stateIn, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
        file=__FILE__, line=__LINE__)) return

      select case (trim(vname))
        case ("HFX")
          buffer = stateIn % hfx
        case ("LAI")
          buffer = stateIn % xlai
        case ("LH")
          buffer = stateIn % lh
        case ("PRSFC")
          buffer = stateIn % psfc
        case ("PBL")
          buffer = stateIn % hpbl
        case ("Q2")
          buffer = stateIn % q2m
        case ("RC")
          buffer = 100. * stateIn % rainc
        case ("RGRND")
          buffer = stateIn % rgrnd
        case ("RN")
          buffer = max(0., 100. * (stateIn % rain - stateIn % rainc))
        case ("SEAICE")
          buffer = stateIn % fice
        case ("SLTYP")
          buffer = stateIn % stype
        case ("SOIM1")
          buffer = stateIn % smois(:,:,1)
        case ("SOIM2")
          buffer = stateIn % smois(:,:,2)
        case ("SOIT1")
          buffer = stateIn % stemp(:,:,1)
        case ("SOIT2")
          buffer = stateIn % stemp(:,:,2)
        case ("TEMPG")
          buffer = stateIn % tsfc
        case ("TEMP2")
          buffer = stateIn % t2m
        case ("USTAR")
          buffer = stateIn % ustar
        case ("VEG")
          buffer = stateIn % vfrac
        case ("WR")
          buffer = stateIn % wr
        case ("WSPD10")
          buffer = sqrt(stateIn % u10m * stateIn % u10m + stateIn % v10m * stateIn % v10m)
        case ("ZRUF")
          buffer = 0.01 * stateIn % zorl
        case default
      !   return
      end select

    case ("OCEAN_1")

      select case (trim(vname))
        case ("OPEN")
          buffer = 0.0
        case ("SZONE")
          buffer = 1.0
        case ("SURF")
          buffer = 1.0
        case default
          return
      end select

    case default
      return

  end select

  interpx_2d = .true.

  if (debug) write(6,'("INTERPX: ",a," - min/max ",2g20.6)') &
    trim(vname), minval(buffer), maxval(buffer)

end function interpx_2d


logical function interpx_3d( fname, vname, pname, &
  col0, col1, row0, row1, lay0, lay1, jdate, jtime, buffer )

  use aqm_model_mod, only : aqm_config_type, aqm_state_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check

  implicit none

  character(len=*), intent(in)  :: fname, vname, pname
  integer,          intent(in)  :: col0, col1, row0, row1, lay0, lay1
  integer,          intent(in)  :: jdate, jtime
  real,             intent(out) :: buffer(:,:,:)

  ! -- local variables
  integer :: localrc
  integer :: nx, ny
  type(aqm_config_type), pointer :: config  => null()
  type(aqm_state_type),  pointer :: stateIn => null()
  logical, parameter :: debug = .true.

  ! -- constants
  include SUBST_CONST

  real, parameter :: EPS1 = RWVAP/RDGAS - 1.

  ! -- begin
  interpx_3d = .false.
  buffer = 0.

  select case (trim(fname))

    case ("MET_CRO_3D")

      call aqm_model_get(config=config, stateIn=stateIn, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
        file=__FILE__, line=__LINE__)) return

      select case (trim(vname))
        case ("JACOBF")
          buffer = 1.0
        case ("JACOBM")
          buffer = 1.0
        case ("DENS")
          buffer = stateIn % temp &
                 * ( 1.0 + EPS1 * stateIn % tr(:,:,:,config % species % p_atm_qv) )
          buffer = stateIn % prl / ( RDGAS * buffer )
        case ("DENSA_J")
          buffer = 1.0
        case ("PRES")
          buffer = stateIn % prl
        case ("PV")
          buffer = 1.0
        case ("QV")
          buffer = stateIn % tr(:,:,:,config % species % p_atm_qv)
        case ("QC")
          buffer = stateIn % tr(:,:,:,config % species % p_atm_qc)
        case ("QR")
          if (config % species % p_atm_qr > 0) &
            buffer = stateIn % tr(:,:,:,config % species % p_atm_qr)
        case ("QI")
          if (config % species % p_atm_qi > 0) &
            buffer = stateIn % tr(:,:,:,config % species % p_atm_qi)
        case ("QS")
          if (config % species % p_atm_qs > 0) &
            buffer = stateIn % tr(:,:,:,config % species % p_atm_qs)
        case ("QG")
          if (config % species % p_atm_qg > 0) &
            buffer = stateIn % tr(:,:,:,config % species % p_atm_qg)
        case ("ZF")
          buffer = max(0., stateIn % phil / GRAV)
        case ("ZH")
          buffer = max(0., stateIn % phii(:,:,1:size(buffer,3)) / GRAV)
        case ("TA")
          buffer = stateIn % temp
        case default
      !   return
      end select

    case ("MET_DOT_3D")
      call aqm_model_get(stateIn=stateIn, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
        file=__FILE__, line=__LINE__)) return

      select case (trim(vname))
        case ("UWINDC")
          ! u-wind is on C grid, while imported wind component are on A grid
          ! this needs to be fixed
          nx = size(stateIn % uwind, dim=1)
          ny = size(stateIn % uwind, dim=2)
          buffer(1:nx,1:ny,:) = stateIn % uwind
        case ("VWINDC")
          nx = size(stateIn % vwind, dim=1)
          ny = size(stateIn % vwind, dim=2)
          buffer(1:nx,1:ny,:) = stateIn % vwind
      end select

    case default
      return

  end select

  interpx_3d = .true.

  if (debug) write(6,'("INTERPX: ",a," - min/max ",2g20.6)') &
    trim(vname), minval(buffer), maxval(buffer)

end function interpx_3d

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

SUBROUTINE EDDYX ( EDDYV )

  use aqm_model_mod, only : aqm_state_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check

  IMPLICIT NONE

  ! -- arguments
  REAL, INTENT( OUT ) :: EDDYV ( :,:,: ) ! eddy diffusivity (m**2/s)

  ! -- local variables
  integer :: localrc
  type(aqm_state_type), pointer :: stateIn => null()

  ! -- begin
  call aqm_model_get(stateIn=stateIn, rc=localrc)
  if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
        file=__FILE__, line=__LINE__)) return

  EDDYV = stateIn % dkt

END SUBROUTINE EDDYX
