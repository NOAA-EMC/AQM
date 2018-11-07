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
          ! -- placeholder for FV3 surface height (m)
        case ('LAT')
          buffer = lat
        case ('LON')
          buffer = lon
          where(buffer > 180.) buffer = buffer - 360.
        case ('LWMASK')
          buffer = stateIn % slmsk2d
        case ('PURB')
        case ('SLTYP')
          buffer = stateIn % stype2d
        case default
          return
      end select

    case ("MET_CRO_2D")

      call aqm_model_get(stateIn=stateIn, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
        file=__FILE__, line=__LINE__)) return

      select case (trim(vname))
        case ("HFX")
          buffer = stateIn % hf2d
        case ("PRSFC")
          buffer = stateIn % pr3d(:,:,1) ! TDB
        case ("PBL")
          buffer = stateIn % pb2d
        case ("RC")
          buffer = 100. * stateIn % rc2d
        case ("RGRND")
          buffer = stateIn % rsds
        case ("RN")
          buffer = max(0., 100. * (stateIn % rn2d - stateIn % rc2d))
        case ("TEMPG")
          buffer = stateIn % ts2d
        case ("USTAR")
          buffer = stateIn % us2d
        case ("ZRUF")
          buffer = 0.01 * stateIn % zorl2d
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

  use aqm_model_mod, only : aqm_state_type, aqm_model_get
  use aqm_rc_mod,    only : aqm_rc_check

  implicit none

  character(len=*), intent(in)  :: fname, vname, pname
  integer,          intent(in)  :: col0, col1, row0, row1, lay0, lay1
  integer,          intent(in)  :: jdate, jtime
  real,             intent(out) :: buffer(:,:,:)

  ! -- local variables
  integer :: localrc
  type(aqm_state_type), pointer :: stateIn
  logical, parameter :: debug = .true.

  ! -- begin
  interpx_3d = .false.
  buffer = 0.

  select case (trim(fname))

    case ("MET_CRO_3D")

      call aqm_model_get(stateIn=stateIn, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrive model input state", &
        file=__FILE__, line=__LINE__)) return

      select case (trim(vname))
        case ("DENS")
          buffer = stateIn % tk3d * ( 1.0 + 0.608 * stateIn % tr3d(:,:,:,1) )
          buffer = stateIn % prl3d / ( 287.0586 * buffer )
        case ("PRES")
          buffer = stateIn % prl3d
        case ("QV")
          buffer = stateIn % tr3d(:,:,:,1)
        case ("QC")
          buffer = stateIn % tr3d(:,:,:,2)
        case ("TA")
          buffer = stateIn % tk3d
        case ("WWIND")
          buffer = stateIn % tk3d * ( 1.0 + 0.608 * stateIn % tr3d(:,:,:,1) )
          buffer = stateIn % prl3d / ( 287.0586 * buffer )
          buffer = -stateIn % ws3d / ( 9.81 * buffer )
        case ("ZF")
          buffer = max(0., stateIn % phl3d / 9.81)
        case ("ZH")
          buffer = max(0., stateIn % ph3d(:,:,1:size(buffer,3)) / 9.81)
        case default
      !   return
      end select

    case ("MET_DOT_3D")
      ! all null

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
