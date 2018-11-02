module ASX_DATA_MOD

  use aqm_model_mod
  use m3utilio

  implicit none

  include SUBST_CONST

  type MET_Type
    real,    Allocatable :: RDEPVHT  ( :,: )  ! air dens / dep vel ht
    real,    Allocatable :: DENS1    ( :,: )  ! layer 1 air density
    real,    Allocatable :: PRSFC    ( :,: )  ! surface pressure [Pa]
    real,    Allocatable :: Q2       ( :,: )  ! 2 meter water vapor mixing ratio [kg/kg]
    real,    Allocatable :: QSS_GRND ( :,: )  ! ground saturation water vapor mixing ratio [kg/kg]
    real,    Allocatable :: RH       ( :,: )  ! relative humidity [ratio]
    real,    Allocatable :: RA       ( :,: )  ! aerodynamic resistnace [s/m]
    real,    Allocatable :: RS       ( :,: )  ! stomatal resistnace [s/m]
    real,    Allocatable :: RGRND    ( :,: )  ! Solar radiation at the ground [W/m**2]
    real,    Allocatable :: HFX      ( :,: )  ! Sensible heat flux [W/m**2]
    real,    Allocatable :: LH       ( :,: )  ! Latent heat flux [W/m**2]
    real,    Allocatable :: SNOCOV   ( :,: )  ! Snow cover [1=yes, 0=no]
    real,    Allocatable :: TEMP2    ( :,: )  ! two meter temperature [K]
    real,    Allocatable :: TEMPG    ( :,: )  ! skin temperature [K]
    real,    Allocatable :: USTAR    ( :,: )  ! surface friction velocity [m/s]
    real,    Allocatable :: VEG      ( :,: )  ! fractional vegetation coverage [ratio]
    real,    Allocatable :: LAI      ( :,: )  ! grid cell leaf area index [m**2/m**2]
    real,    Allocatable :: WR       ( :,: )  ! precip intercepted by canopy [m]
    real,    Allocatable :: WSTAR    ( :,: )  ! convective velocity scale [m/s]
    real,    Allocatable :: Z0       ( :,: )  ! roughness length [m]
    real,    Allocatable :: SOIM1    ( :,: )  ! 1 cm soil moisture [m**3/m**3]
    real,    Allocatable :: SOIM2    ( :,: )  ! 1 m soil moisture  [m**3/m**3]
    real,    Allocatable :: SOIT1    ( :,: )  ! 1 cm soil temperature [K]
    real,    Allocatable :: SOIT2    ( :,: )  ! 1 m soil temperature [K]
    real,    Allocatable :: SEAICE   ( :,: )  ! Sea ice coverage [%]
    real,    Allocatable :: MOL      ( :,: )  ! Monin-Obukhov length [m]
    real,    Allocatable :: MOLI     ( :,: )  ! inverse of Monin-Obukhov length [m]
    real,    Allocatable :: HOL      ( :,: )  ! PBL over Obukhov length
    integer, Allocatable :: LPBL     ( :,: )  ! PBL layer
    logical, Allocatable :: CONVCT   ( :,: )  ! convection flag
    real,    Allocatable :: PBL      ( :,: )  ! pbl height (m)
!> U and V wind components on the cross grid points
    real,    Allocatable :: UWIND    ( :,:,: )  ! [m/s]
    real,    Allocatable :: VWIND    ( :,:,: )  ! [m/s]
!> 3-D meteorological fields:
    real,    Allocatable :: KZMIN    ( :,:,: )  ! minimum Kz [m**2/s]
    real,    Allocatable :: PRES     ( :,:,: )  ! layer 1 pressure [Pa]
    real,    Allocatable :: QV       ( :,:,: )  ! water vapor mixing ratio
    real,    Allocatable :: QC       ( :,:,: )  ! cloud water mixing ratio
    real,    Allocatable :: THETAV   ( :,:,: )  ! potential temp
    real,    Allocatable :: TA       ( :,:,: )  ! temperature (K)
    real,    Allocatable :: ZH       ( :,:,: )  ! mid-layer height above ground [m]
    real,    Allocatable :: ZF       ( :,:,: )  ! layer height [m]
    real,    Allocatable :: DZF      ( :,:,: )  ! layer surface thickness
    real,    Allocatable :: DENS     ( :,:,: )  ! air density
    real,    Allocatable :: RJACM    ( :,:,: )  ! reciprocal mid-layer Jacobian
    real,    Allocatable :: RJACF    ( :,:,: )  ! reciprocal full-layer Jacobian
    real,    Allocatable :: RRHOJ    ( :,:,: )  ! reciprocal density X Jacobian
  end type MET_Type

  type GRID_Type
    real,    Allocatable :: RDX3F  ( :,:,: )    ! reciprocal sigma layer thickness ! EMIS_DEFN.F, sedi.F, vdiffacmx.F, vdiffproc.F
!> Horizontal Information:
    real,    Allocatable :: RMSFX4 ( :,: )  ! inverse map scale factor ** 4
    real,    Allocatable :: LON    ( :,: )  ! longitude
    real,    Allocatable :: LAT    ( :,: )  ! latitude
    real,    Allocatable :: LWMASK ( :,: )  ! land water mask
    real,    Allocatable :: OCEAN  ( :,: )  ! Open ocean
    real,    Allocatable :: SZONE  ( :,: )  ! Surf zone
    real,    Allocatable :: PURB   ( :,: )  ! percent urban [%]
    integer, Allocatable :: SLTYP  ( :,: )  ! soil type [category]
    real,    Allocatable :: WWLT   ( :,: )  ! soil wilting point
    real,    Allocatable :: BSLP   ( :,: )  ! B Slope
    real,    Allocatable :: WRES   ( :,: )  ! Soil residual moisture point
    real,    Allocatable :: WFC    ( :,: )  ! soil field capacity
!   real,    Allocatable :: RHOB   ( :,: )  ! soil bulk density
    real, Allocatable :: LUFRAC  ( :,:,: ) ! land use fraction (col,row,lu_type)[ratio]
! Land use information:
    character( 16 ), allocatable   :: NAME    ( : )     ! LU name
    character( 16 ), allocatable   :: LU_Type ( : )     ! general land use type e.g. water, forest, etc.
  end type GRID_Type

  type :: MOSAIC_Type                 ! (col,row,lu)
    Character( 16 ), Allocatable :: NAME    ( : ) ! LU name
    Character( 16 ), Allocatable :: LU_Type ( : ) ! general land use type e.g. water, forest, etc.
!> Sub grid cell meteorological variables:
    Real, Allocatable :: USTAR ( :,:,: )   ! surface friction velocity [m/s]
    Real, Allocatable :: LAI   ( :,:,: )   ! leaf area index [m**2/m**2]
    Real, Allocatable :: VEG   ( :,:,: )   ! vegetation fraction [ratio]
    Real, Allocatable :: Z0    ( :,:,: )   ! vegetation fraction [ratio]
    Real, Allocatable :: DELTA ( :,:,: )   ! Surface wetness [ratio]
!> Sub grid cell resistances
    Real, Allocatable :: RA    ( :,:,: )    ! aerodynamic resistance [s/m]
    Real, Allocatable :: RSTW  ( :,:,: )    ! Stomatal Resistance of water [s/m]
    Real, Allocatable :: RINC  ( :,:,: )    ! In-canopy resistance [s/m]
  end type MOSAIC_Type

  Type :: ChemMos_Type                 ! (col,row,lu,spc)
     Character( 16 ), Allocatable :: NAME    ( : )  ! LU name
     Character( 16 ), Allocatable :: Lu_Type ( : )  ! general land use type e.g. water, forest, etc.
     Character( 16 ), Allocatable :: SubName ( : )  ! Deposition species name
!> Sub grid cell chemically dependent resistances
     Real, Allocatable :: Rb   ( :,:,:,: ) ! quasi-laminar boundary layer resistance [s/m]
     Real, Allocatable :: Rst  ( :,:,:,: ) ! stomatal resistance [s/m]
     Real, Allocatable :: Rgc  ( :,:,:,: ) ! Canopy covered soil resistance [s/m]
     Real, Allocatable :: Rgb  ( :,:,:,: ) ! Barron soil resistance [s/m]
     Real, Allocatable :: Rcut ( :,:,:,: ) ! soil resistance [s/m]
     Real, Allocatable :: Rwat ( :,:,:,: ) ! surface water resistance [s/m]
!> Sub grid cell compensation point
     Real, Allocatable :: Catm ( :,:,:,: ) ! Atmospheric [ppm]
     Real, Allocatable :: CZ0  ( :,:,:,: ) ! compensation point at Z0 [ppm]
     Real, Allocatable :: Cleaf( :,:,:,: ) ! Leaf compensation point [ppm]
     Real, Allocatable :: Cstom( :,:,:,: ) ! Stomatal compensation point [ppm]
     Real, Allocatable :: Ccut ( :,:,:,: ) ! Cuticular compensation point [ppm]
     Real, Allocatable :: Csoil( :,:,:,: ) ! Soil compensation point [ppm]
  End Type ChemMos_Type

  type(MET_Type)     :: MET_Data
  type(GRID_Type)    :: GRID_Data
  type(MOSAIC_Type)  :: MOSAIC_Data
  type(ChemMos_Type) :: ChemMos_Data

  integer, parameter :: ltotg = 108 ! from DEPVVARS module
  integer, parameter :: n_spc_m3dry = ltotg
!> M3 asx constants
  real, parameter :: a0         = 8.0        ! [dim'less]
  real, parameter :: d3         = 1.38564e-2 ! [dim'less]
  real, parameter :: dwat       = 0.2178     ! [cm^2/s] at 273.15K
  real, parameter :: hplus_ap   = 1.0e-6     ! pH=6.0 leaf apoplast solution Ph (Massad et al 2008)
  real, parameter :: hplus_def  = 1.0e-5     ! pH=5.0
  real, parameter :: hplus_east = 1.0e-5     ! pH=5.0
  real, parameter :: hplus_h2o  = 7.94328e-9 ! 10.0**(-8.1)
  real, parameter :: hplus_west = 3.16228e-6 ! 10.0**(-5.5)
  real, parameter :: kvis       = 0.132      ! [cm^2 / s] at 273.15K
  real, parameter :: pr         = 0.709      ! [dim'less]
  real, parameter :: rcut0      = 3000.0     ! [s/m]
  real, parameter :: rcw0       = 125000.0   ! acc'd'g to Padro and
  real, parameter :: resist_max = 1.0e30     ! maximum resistance
  real, parameter :: rg0        = 1000.0     ! [s/m]
  real, parameter :: rgwet0     = 25000.0    ! [s/m]
  real, parameter :: rsndiff    = 10.0       ! snow diffusivity fac
  real, parameter :: rsnow0     = 1000.0
  real, parameter :: svp2       = 17.67      ! from MM5 and WRF
  real, parameter :: svp3       = 29.65      ! from MM5 and WRF
  real, parameter :: rt25inK    = 1.0/(stdtemp + 25.0) ! 298.15K = 25C
  real, parameter :: twothirds  = 2.0 / 3.0
  real, parameter :: betah      = 5.0       ! WRF 3.6 px uses Dyer
  real, parameter :: gamah      = 16.0
  real, parameter :: pr0        = 0.95
  real, parameter :: karman     = 0.40
  real, parameter :: f3min      = 0.25
  real, parameter :: ftmin      = 0.0000001  ! m/s
  real, parameter :: nscat      = 16.0
  real, parameter :: rsmax      = 5000.0     ! s/m

  real            :: ar       ( ltotg )        ! reactivity relative to HNO3
  real            :: dif0     ( ltotg )        ! molecular diffusivity [cm2/s]
  real            :: lebas    ( ltotg )        ! Le Bas molar volume [cm3/mol ]
  real            :: meso     ( ltotg )        ! Exception for species that
                                               ! react with cell walls. fo in
                                               ! Wesely 1989 eq 6.
  character( 16 ) :: subname  ( ltotg )        ! for subroutine HLCONST


  DATA subname(  1), dif0(  1), ar(  1), meso(  1), lebas(  1) / 'SO2             ', 0.1089,   10.0,      0.0,  35.0/
  DATA subname(  2), dif0(  2), ar(  2), meso(  2), lebas(  2) / 'H2SO4           ', 0.1091, 8000.0,      0.0,  49.0/
  DATA subname(  3), dif0(  3), ar(  3), meso(  3), lebas(  3) / 'NO2             ', 0.1361,    2.0,      0.1,  21.0/
  DATA subname(  4), dif0(  4), ar(  4), meso(  4), lebas(  4) / 'NO              ', 0.1802,    2.0,      0.0,  14.0/
  DATA subname(  5), dif0(  5), ar(  5), meso(  5), lebas(  5) / 'O3              ', 0.1444,   12.0,      1.0,  21.0/
  DATA subname(  6), dif0(  6), ar(  6), meso(  6), lebas(  6) / 'HNO3            ', 0.1067, 8000.0,      0.0,  35.0/
  DATA subname(  7), dif0(  7), ar(  7), meso(  7), lebas(  7) / 'H2O2            ', 0.1300,34000.0,      1.0,  28.0/   !ar=34,000 such that r_cut=0.7 s/m as in Nguyen et al. 2015
  DATA subname(  8), dif0(  8), ar(  8), meso(  8), lebas(  8) / 'ACETALDEHYDE    ', 0.1111,   10.0,      0.0,  56.0/
  DATA subname(  9), dif0(  9), ar(  9), meso(  9), lebas(  9) / 'FORMALDEHYDE    ', 0.1554,   10.0,      0.0,  35.0/
  DATA subname( 10), dif0( 10), ar( 10), meso( 10), lebas( 10) / 'METHYLHYDROPEROX', 0.1179,   10.0,      0.3,  49.0/   !meso change from 0.1 to 0.3, Wolfe and Thornton 2011 ACP per J. Bash
  DATA subname( 11), dif0( 11), ar( 11), meso( 11), lebas( 11) / 'PEROXYACETIC_ACI', 0.0868,   20.0,      0.1,  70.0/
  DATA subname( 12), dif0( 12), ar( 12), meso( 12), lebas( 12) / 'ACETIC_ACID     ', 0.0944,   20.0,      0.0,  63.0/
  DATA subname( 13), dif0( 13), ar( 13), meso( 13), lebas( 13) / 'NH3             ', 0.1978,   20.0,      0.0,  28.0/
  DATA subname( 14), dif0( 14), ar( 14), meso( 14), lebas( 14) / 'PAN             ', 0.0687,   16.0,      0.1,  91.0/
  DATA subname( 15), dif0( 15), ar( 15), meso( 15), lebas( 15) / 'HNO2            ', 0.1349,   20.0,      0.1,  28.0/
  DATA subname( 16), dif0( 16), ar( 16), meso( 16), lebas( 16) / 'CO              ', 0.1807,    5.0,      0.0,  14.0/
  DATA subname( 17), dif0( 17), ar( 17), meso( 17), lebas( 17) / 'METHANOL        ', 0.1329,    2.0,      0.0,  42.0/
  DATA subname( 18), dif0( 18), ar( 18), meso( 18), lebas( 18) / 'N2O5            ', 0.0808, 5000.0,      0.0,  49.0/
  DATA subname( 19), dif0( 19), ar( 19), meso( 19), lebas( 19) / 'NO3             ', 0.1153, 5000.0,      0.0,  28.0/
  DATA subname( 20), dif0( 20), ar( 20), meso( 20), lebas( 20) / 'GENERIC_ALDEHYDE', 0.0916,   10.0,      0.0,  56.0/
  DATA subname( 21), dif0( 21), ar( 21), meso( 21), lebas( 21) / 'CL2             ', 0.1080,   10.0,      0.0,  49.0/
  DATA subname( 22), dif0( 22), ar( 22), meso( 22), lebas( 22) / 'HOCL            ', 0.1300,   10.0,      0.0,  38.5/
  DATA subname( 23), dif0( 23), ar( 23), meso( 23), lebas( 23) / 'HCL             ', 0.1510, 8000.0,      0.0,  31.5/
  DATA subname( 24), dif0( 24), ar( 24), meso( 24), lebas( 24) / 'FMCL            ', 0.1094,   10.0,      0.0,  45.5/
  DATA subname( 25), dif0( 25), ar( 25), meso( 25), lebas( 25) / 'HG              ', 0.1194,    0.1,      0.0,  14.8/   ! lebas not used
  DATA subname( 26), dif0( 26), ar( 26), meso( 26), lebas( 26) / 'HGIIGAS         ', 0.0976, 8000.0,      0.0,  95.0/   ! estimation from back calculating to get dw25 = 1.04e-5 (Garland et al, 1965)
  DATA subname( 27), dif0( 27), ar( 27), meso( 27), lebas( 27) / 'TECDD_2378      ', 0.0525,    2.0,      0.0, 217.0/
  DATA subname( 28), dif0( 28), ar( 28), meso( 28), lebas( 28) / 'PECDD_12378     ', 0.0508,    2.0,      0.0, 234.5/
  DATA subname( 29), dif0( 29), ar( 29), meso( 29), lebas( 29) / 'HXCDD_123478    ', 0.0494,    2.0,      0.0, 252.0/
  DATA subname( 30), dif0( 30), ar( 30), meso( 30), lebas( 30) / 'HXCDD_123678    ', 0.0494,    2.0,      0.0, 252.0/
  DATA subname( 31), dif0( 31), ar( 31), meso( 31), lebas( 31) / 'HXCDD_123478    ', 0.0494,    2.0,      0.0, 252.0/
  DATA subname( 32), dif0( 32), ar( 32), meso( 32), lebas( 32) / 'HPCDD_1234678   ', 0.0480,    2.0,      0.0, 269.5/
  DATA subname( 33), dif0( 33), ar( 33), meso( 33), lebas( 33) / 'OTCDD           ', 0.0474,    2.0,      0.0, 287.0/
  DATA subname( 34), dif0( 34), ar( 34), meso( 34), lebas( 34) / 'TECDF_2378      ', 0.0534,    2.0,      0.0, 210.0/
  DATA subname( 35), dif0( 35), ar( 35), meso( 35), lebas( 35) / 'PECDF_12378     ', 0.0517,    2.0,      0.0, 227.5/
  DATA subname( 36), dif0( 36), ar( 36), meso( 36), lebas( 36) / 'PECDF_23478     ', 0.0517,    2.0,      0.0, 227.5/
  DATA subname( 37), dif0( 37), ar( 37), meso( 37), lebas( 37) / 'HXCDF_123478    ', 0.0512,    2.0,      0.0, 245.0/
  DATA subname( 38), dif0( 38), ar( 38), meso( 38), lebas( 38) / 'HXCDF_123678    ', 0.0512,    2.0,      0.0, 245.0/
  DATA subname( 39), dif0( 39), ar( 39), meso( 39), lebas( 39) / 'HXCDF_234678    ', 0.0512,    2.0,      0.0, 245.0/
  DATA subname( 40), dif0( 40), ar( 40), meso( 40), lebas( 40) / 'HXCDF_123789    ', 0.0512,    2.0,      0.0, 245.0/
  DATA subname( 41), dif0( 41), ar( 41), meso( 41), lebas( 41) / 'HPCDF_1234678   ', 0.0487,    2.0,      0.0, 262.5/
  DATA subname( 42), dif0( 42), ar( 42), meso( 42), lebas( 42) / 'HPCDF_1234789   ', 0.0487,    2.0,      0.0, 262.5/
  DATA subname( 43), dif0( 43), ar( 43), meso( 43), lebas( 43) / 'OTCDF           ', 0.0474,    2.0,      0.0, 280.0/
  DATA subname( 44), dif0( 44), ar( 44), meso( 44), lebas( 44) / 'NAPHTHALENE     ', 0.0778,    4.0,      0.0, 119.0/
  DATA subname( 45), dif0( 45), ar( 45), meso( 45), lebas( 45) / '1NITRONAPHTHALEN', 0.0692,    4.0,      0.0, 133.0/
  DATA subname( 46), dif0( 46), ar( 46), meso( 46), lebas( 46) / '2NITRONAPHTHALEN', 0.0692,    4.0,      0.0, 133.0/
  DATA subname( 47), dif0( 47), ar( 47), meso( 47), lebas( 47) / '14NAPHTHOQUINONE', 0.0780,    4.0,      0.0, 119.0/
  DATA subname( 48), dif0( 48), ar( 48), meso( 48), lebas( 48) / 'HEXAMETHYLE_DIIS', 0.0380,   10.0,      0.0, 196.0/
  DATA subname( 49), dif0( 49), ar( 49), meso( 49), lebas( 49) / 'HYDRAZINE       ', 0.4164,   20.0,      0.0,  42.0/
  DATA subname( 50), dif0( 50), ar( 50), meso( 50), lebas( 50) / 'MALEIC_ANHYDRIDE', 0.0950,   10.0,      0.0,  70.0/
  DATA subname( 51), dif0( 51), ar( 51), meso( 51), lebas( 51) / '24-TOLUENE_DIIS ', 0.0610,   10.0,      0.0, 154.0/
  DATA subname( 52), dif0( 52), ar( 52), meso( 52), lebas( 52) / 'TRIETHYLAMINE   ', 0.0881,   20.0,      0.0, 154.0/
  DATA subname( 53), dif0( 53), ar( 53), meso( 53), lebas( 53) / 'ORG_NTR         ', 0.0607,   16.0,      0.0, 160.0/  ! assumes 58.2% C5H11O4N and 41.8% C5H11O3N
  DATA subname( 54), dif0( 54), ar( 54), meso( 54), lebas( 54) / 'HYDROXY_NITRATES', 0.0609,   16.0,      0.0, 156.1/
  DATA subname( 55), dif0( 55), ar( 55), meso( 55), lebas( 55) / 'MPAN            ', 0.0580,   16.0,      0.1, 133.0/
  DATA subname( 56), dif0( 56), ar( 56), meso( 56), lebas( 56) / 'PPN             ', 0.0631,   16.0,      0.1, 118.2/
  DATA subname( 57), dif0( 57), ar( 57), meso( 57), lebas( 57) / 'MVK             ', 0.0810,    8.0,      1.0,  88.8/
  DATA subname( 58), dif0( 58), ar( 58), meso( 58), lebas( 58) / 'DINTR           ', 0.0617,   16.0,      0.1, 169.8/
  DATA subname( 59), dif0( 59), ar( 59), meso( 59), lebas( 59) / 'NTR_ALK         ', 0.0688,   16.0,      0.1, 133.0/
  DATA subname( 60), dif0( 60), ar( 60), meso( 60), lebas( 60) / 'NTR_OH          ', 0.0665,   16.0,      0.1, 140.4/
  DATA subname( 61), dif0( 61), ar( 61), meso( 61), lebas( 61) / 'HYDROXY_NITRATES', 0.0646,   16.0,      0.0, 147.8/
  DATA subname( 62), dif0( 62), ar( 62), meso( 62), lebas( 62) / 'PROPNN          ', 0.0677,   16.0,      0.0, 133.0/
  DATA subname( 63), dif0( 63), ar( 63), meso( 63), lebas( 63) / 'NITRYL_CHLORIDE ', 0.0888,    8.0,      0.0,  45.5/  ! dif0 estimated following Erickson III et al., JGR, 104, D7, 8347-8372, 1999
  DATA subname( 64), dif0( 64), ar( 64), meso( 64), lebas( 64) / 'ISOPNN          ',0.0457,    8.0,      0.0,  206.8/
  DATA subname( 65), dif0( 65), ar( 65), meso( 65), lebas( 65) / 'MTNO3           ',0.0453,    8.0,      0.0,  251.2/
  DATA subname( 66), dif0( 66), ar( 66), meso( 66), lebas( 66) / 'IEPOX           ',0.0579,    8.0,      0.0,  110.8/
  DATA subname( 67), dif0( 67), ar( 67), meso( 67), lebas( 67) / 'HACET           ',0.1060,    8.0,      0.0,   72.6/  ! dif0 from Nguyen 2015 PNAS
  DATA subname( 68), dif0( 68), ar( 68), meso( 68), lebas( 68) / 'SVALK1          ',0.0514,   20.0,      0.0,  280.5/
  DATA subname( 69), dif0( 69), ar( 69), meso( 69), lebas( 69) / 'SVALK2          ',0.0546,   20.0,      0.0,  275.6/
  DATA subname( 70), dif0( 70), ar( 70), meso( 70), lebas( 70) / 'SVBNZ1          ',0.0642,   20.0,      0.0,  134.1/
  DATA subname( 71), dif0( 71), ar( 71), meso( 71), lebas( 71) / 'SVBNZ2          ',0.0726,   20.0,      0.0,  127.5/
  DATA subname( 72), dif0( 72), ar( 72), meso( 72), lebas( 72) / 'SVISO1          ',0.0733,   20.0,      0.0,  126.3/
  DATA subname( 73), dif0( 73), ar( 73), meso( 73), lebas( 73) / 'SVISO2          ',0.0729,   20.0,      0.0,  123.8/
  DATA subname( 74), dif0( 74), ar( 74), meso( 74), lebas( 74) / 'SVPAH1          ',0.0564,   20.0,      0.0,  235.7/
  DATA subname( 75), dif0( 75), ar( 75), meso( 75), lebas( 75) / 'SVPAH2          ',0.0599,   20.0,      0.0,  231.5/
  DATA subname( 76), dif0( 76), ar( 76), meso( 76), lebas( 76) / 'SVSQT           ',0.0451,   20.0,      0.0,  346.5/
  DATA subname( 77), dif0( 77), ar( 77), meso( 77), lebas( 77) / 'SVTOL1          ',0.0637,   20.0,      0.0,  153.7/
  DATA subname( 78), dif0( 78), ar( 78), meso( 78), lebas( 78) / 'SVTOL2          ',0.0607,   20.0,      0.0,  194.1/
  DATA subname( 79), dif0( 79), ar( 79), meso( 79), lebas( 79) / 'SVTRP1          ',0.0603,   20.0,      0.0,  194.9/
  DATA subname( 80), dif0( 80), ar( 80), meso( 80), lebas( 80) / 'SVTRP2          ',0.0559,   20.0,      0.0,  218.8/
  DATA subname( 81), dif0( 81), ar( 81), meso( 81), lebas( 81) / 'SVXYL1          ',0.0610,   20.0,      0.0,  154.6/
  DATA subname( 82), dif0( 82), ar( 82), meso( 82), lebas( 82) / 'SVXYL2          ',0.0585,   20.0,      0.0,  194.6/
  DATA subname( 83), dif0( 83), ar( 83), meso( 83), lebas( 83) / 'IO              ',0.1002,    8.0,      0.0,  44.4/
  DATA subname( 84), dif0( 84), ar( 84), meso( 84), lebas( 84) / 'OIO             ',0.0938,    8.0,      0.0,  51.8/
  DATA subname( 85), dif0( 85), ar( 85), meso( 85), lebas( 85) / 'I2O2            ',0.0732,    8.0,      0.0,  88.8/
  DATA subname( 86), dif0( 86), ar( 86), meso( 86), lebas( 86) / 'I2O3            ',0.0707,    8.0,      0.0,  96.2/
  DATA subname( 87), dif0( 87), ar( 87), meso( 87), lebas( 87) / 'I2O4            ',0.0684,    8.0,      0.0, 103.6/
  DATA subname( 88), dif0( 88), ar( 88), meso( 88), lebas( 88) / 'HI              ',0.1045,    8.0,      0.0,  40.7/
  DATA subname( 89), dif0( 89), ar( 89), meso( 89), lebas( 89) / 'HOI             ',0.0972,    8.0,      0.0,  48.1/
  DATA subname( 90), dif0( 90), ar( 90), meso( 90), lebas( 90) / 'INO             ',0.0882,    8.0,      0.0,  60.9/
  DATA subname( 91), dif0( 91), ar( 91), meso( 91), lebas( 91) / 'INO2            ',0.0883,   20.0,      0.0,  69.2/
  DATA subname( 92), dif0( 92), ar( 92), meso( 92), lebas( 92) / 'IONO2           ',0.0792,    8.0,      0.0,  77.5/
  DATA subname( 93), dif0( 93), ar( 93), meso( 93), lebas( 93) / 'BRO             ',0.1144,    1.0,      0.0,  34.4/
  DATA subname( 94), dif0( 94), ar( 94), meso( 94), lebas( 94) / 'HOBR            ',0.1101,    1.0,      0.0,  38.1/
  DATA subname( 95), dif0( 95), ar( 95), meso( 95), lebas( 95) / 'HBR             ',0.1216,    2.0,      0.0,  30.7/
  DATA subname( 96), dif0( 96), ar( 96), meso( 96), lebas( 96) / 'BRONO2          ',0.0855,    1.0,      0.0,  67.5/
  DATA subname( 97), dif0( 97), ar( 97), meso( 97), lebas( 97) / 'BRNO2           ',0.0909,    1.0,      0.0,  59.2/
  DATA subname( 98), dif0( 98), ar( 98), meso( 98), lebas( 98) / 'BRCL            ',0.0966,    1.0,      0.0,  51.6/
  DATA subname( 99), dif0( 99), ar( 99), meso( 99), lebas( 99) / 'DMS             ',0.0926,    2.0,      0.0,  77.4/
  DATA subname(100), dif0(100), ar(100), meso(100), lebas(100) / 'MSA             ',0.0896,    2.0,      0.0,  77.4/
  DATA subname(101), dif0(101), ar(101), meso(101), lebas(101) / 'METHANE         ',0.2107,    2.0,      0.0,  29.6/ ! dif0, equation 9-22. Scwarzenbach et. (1993) Env. Org. Chem.
  DATA subname(102), dif0(102), ar(102), meso(102), lebas(102) / 'ACRYACID        ',0.0908,    2.0,      0.0,  63.2/ 
  DATA subname(103), dif0(103), ar(103), meso(103), lebas(103) / 'CARBSULFIDE     ',0.1240,    5.0,      0.0,  51.5/ 
  DATA subname(104), dif0(104), ar(104), meso(104), lebas(104) / 'ACETONITRILE    ',0.1280,    5.0,      0.0,  52.3/ 
  DATA subname(105), dif0(105), ar(105), meso(105), lebas(105) / '6_NITRO_O_CRESOL',0.0664,   16.0,      0.0, 155.0/ ! dif0, equation 9-22. Scwarzenbach et. (1993) Env. Org. Chem.

  logical :: MET_INITIALIZED = .false.
  logical :: ifwr            = .true.
  logical :: CSTAGUV         = .false.   ! Winds are available with C stagger?
  logical :: MINKZ           = .false.   ! Winds are available with C stagger?
  real    :: CONVPA          = 1.0

  integer, parameter :: p_qv = 1

  public

  private :: ltotg

contains

  subroutine INIT_MET ( JDATE, JTIME, MOSAIC, ABFLUX, HGBIDI )

    integer, intent(in) :: JDATE, JTIME
    logical, intent(in) :: MOSAIC, ABFLUX, HGBIDI

    ! -- local variables
    integer :: allocstat, localrc
    integer :: NROWS, RCOLS, NLAYS

    type(aqm_state_type), pointer :: stateIn

    character(len=*), parameter :: PNAME = 'INIT_MET'

    

    ! -- begin

    if (.not.MET_INITIALIZED) then

      call aqm_model_get(stateIn=stateIn, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrieve input state", &
        file=__FILE__, line=__LINE__)) &
        call m3exit("INIT_MET", 0, 0, "Cannot retrieve input state")
      call aqm_model_domain_get(ids=is, ide=ie, jds=js, jde=je, &
        nl=NLAYS, lon=lon, lat=lat, rc=localrc)
      if (aqm_rc_check(localrc, msg="Failure to retrieve domain information", &
        file=__FILE__, line=__LINE__)) &
        call m3exit("INIT_MET", 0, 0, "Cannot retrieve domain information")

      NROWS = ie - is + 1
      NCOLS = je - js + 1

      ! -- grid data
      allocate( &
        Grid_Data%RDX3F   ( NCOLS,NROWS,NLAYS ), &
        Grid_Data%RMSFX4  ( NCOLS,NROWS ),       &
        Grid_Data%LON     ( NCOLS,NROWS ),       &
        Grid_Data%LAT     ( NCOLS,NROWS ),       &
        Grid_Data%LWMASK  ( NCOLS,NROWS ),       &
        Grid_Data%OCEAN   ( NCOLS,NROWS ),       &
        Grid_Data%SZONE   ( NCOLS,NROWS ),       &
        Grid_Data%PURB    ( NCOLS,NROWS ),       &
        Grid_Data%SLTYP   ( NCOLS,NROWS ),       &
        Grid_Data%NAME    ( n_lufrac ),          &
        Grid_Data%LU_Type ( n_lufrac ),          &
        STAT = ALLOCSTAT )

      If ( ALLOCSTAT .Ne. 0 ) Call M3EXIT( PNAME, 0, 0, 'Failure allocating grid vars')

      Grid_Data % LAT = lat
      ! -- CMAQ requires longitude to be expressed within (-180,180)
      Grid_Data % LON = lon
      where(Grid_Data % LON > 180.) Grid_Data % LON = Grid_Data % LON - 360.
      print *,'INIT_MET: LON: min/max = ',minval(lon),maxval(lon),minval(Grid_Data % LON),maxval(Grid_Data % LON)

      ! CMAQ: 0: water, 1: land
      ! FV3 : 0: water, 1: land, 2: sea-ice
      Grid_Data % LWMASK = stateIn % slmsk2d

      allocate( &
        Met_Data%RDEPVHT  ( NCOLS,NROWS ),            &
        Met_Data%DENS1    ( NCOLS,NROWS ),            &
        Met_Data%PRSFC    ( NCOLS,NROWS ),            &
        Met_Data%Q2       ( NCOLS,NROWS ),            &
        Met_Data%QSS_GRND ( NCOLS,NROWS ),            &
        Met_Data%RH       ( NCOLS,NROWS ),            &
        Met_Data%RA       ( NCOLS,NROWS ),            &
        Met_Data%RS       ( NCOLS,NROWS ),            &
        Met_Data%RGRND    ( NCOLS,NROWS ),            &
        Met_Data%HFX      ( NCOLS,NROWS ),            &
        Met_Data%LH       ( NCOLS,NROWS ),            &
        Met_Data%SNOCOV   ( NCOLS,NROWS ),            &
        Met_Data%TEMP2    ( NCOLS,NROWS ),            &
        Met_Data%TEMPG    ( NCOLS,NROWS ),            &
        Met_Data%USTAR    ( NCOLS,NROWS ),            &
        Met_Data%VEG      ( NCOLS,NROWS ),            &
        Met_Data%LAI      ( NCOLS,NROWS ),            &
        Met_Data%WR       ( NCOLS,NROWS ),            &
        Met_Data%WSTAR    ( NCOLS,NROWS ),            &
        Met_Data%Z0       ( NCOLS,NROWS ),            &
        Met_Data%SOIM1    ( NCOLS,NROWS ),            &
        Met_Data%SEAICE   ( NCOLS,NROWS ),            &
        Met_Data%MOL      ( NCOLS,NROWS ),            &
        Met_Data%MOLI     ( NCOLS,NROWS ),            &
        Met_Data%HOL      ( NCOLS,NROWS ),            &
        Met_Data%LPBL     ( NCOLS,NROWS ),            &
        Met_Data%CONVCT   ( NCOLS,NROWS ),            &
        Met_Data%PBL      ( NCOLS,NROWS ),            &
        Met_Data%UWIND    ( NCOLS+1,NROWS+1,NLAYS ),  &
        Met_Data%VWIND    ( NCOLS+1,NROWS+1,NLAYS ),  &
        Met_Data%KZMIN    ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%PRES     ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%QV       ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%QC       ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%THETAV   ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%TA       ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%ZH       ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%ZF       ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%DZF      ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%DENS     ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%RJACM    ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%RJACF    ( NCOLS,NROWS,NLAYS ),      &
        Met_Data%RRHOJ    ( NCOLS,NROWS,NLAYS ),      &
        STAT = ALLOCSTAT )
      If ( ALLOCSTAT .Ne. 0 ) Call M3EXIT( PNAME, 0, 0, 'Failure allocating met vars')



      Met_Data % SEAICE = 0.0
      where(stateIn % slmsk2d > 1.9) Met_Data % SEAICE = 1.0

      Met_Data % PRES = stateIn % prl3d
      Met_Data % QV   = stateIn % tr3d(:,:,:,p_qv)
      Met_Data % TA   = stateIn % tk3d
      
      ! -- 3D meteorological fields
      KZMIN    ( :,:,: )  ! minimum Kz [m**2/s]
      PRES    => stateIn % prl3d   ( :,:,: )  ! layer 1 pressure [Pa]
      QV       ( :,:,: )  ! water vapor mixing ratio
      QC       ( :,:,: )  ! cloud water mixing ratio
      THETAV   ( :,:,: )  ! potential temp
      TA      => stateIn % tk3d ( :,:,: )  ! temperature (K)
      ZH       ( :,:,: )  ! mid-layer height above ground [m]
      ZF       ( :,:,: )  ! layer height [m]
      DZF      ( :,:,: )  ! layer surface thickness
      DENS     ( :,:,: )  ! air density
      RJACM    ( :,:,: )  ! reciprocal mid-layer Jacobian
      RJACF    ( :,:,: )  ! reciprocal full-layer Jacobian
      RRHOJ    ( :,:,: )  ! reciprocal density X Jacobian

      MET_INITIALIZED = .true.
    end if

  end subroutine INIT_MET

  subroutine GET_MET ( JDATE, JTIME, TSTEP, MOSAIC, ABFLUX, HGBIDI )

    integer, intent(in) :: JDATE, JTIME, TSTEP
    logical, intent(in) :: MOSAIC, ABFLUX, HGBIDI

    ! -- local parameters
    real, parameter :: cond_min = 1.0 / resist_max ! minimum conductance [m/s]
    real, parameter :: KZMAXL = 500.0    ! upper limit for min Kz [m]

    real, parameter :: KZ0UT  = 1.0      ! minimum eddy diffusivity [m**2/sec] KZ0

    real, parameter :: KZL    = 0.01     ! lowest KZ
    real, parameter :: KZU    = 1.0      ! 2.0  ! highest KZ
    real, parameter :: EPS    = 1.0E-08  ! small number for temperature difference


  end subroutine GET_MET

end module ASX_DATA_MOD

#if 0
      Use GRID_CONF           ! horizontal & vertical domain specifications
      Use LSM_MOD             ! Land surface data
      Use DEPVVARS, Only: ltotg

      Implicit None

      Include SUBST_CONST     ! constants

      Type :: MET_Type
         Real,    Allocatable :: RDEPVHT  ( :,: )  ! air dens / dep vel ht
         Real,    Allocatable :: DENS1    ( :,: )  ! layer 1 air density
         Real,    Allocatable :: PRSFC    ( :,: )  ! surface pressure [Pa]
         Real,    Allocatable :: Q2       ( :,: )  ! 2 meter water vapor mixing ratio [kg/kg]
         Real,    Allocatable :: QSS_GRND ( :,: )  ! ground saturation water vapor mixing ratio [kg/kg]
         Real,    Allocatable :: RH       ( :,: )  ! relative humidity [ratio]
         Real,    Allocatable :: RA       ( :,: )  ! aerodynamic resistnace [s/m]
         Real,    Allocatable :: RS       ( :,: )  ! stomatal resistnace [s/m]
         Real,    Allocatable :: RGRND    ( :,: )  ! Solar radiation at the ground [W/m**2]
         Real,    Allocatable :: HFX      ( :,: )  ! Sensible heat flux [W/m**2]
         Real,    Allocatable :: LH       ( :,: )  ! Latent heat flux [W/m**2]
         Real,    Allocatable :: SNOCOV   ( :,: )  ! Snow cover [1=yes, 0=no]
         Real,    Allocatable :: TEMP2    ( :,: )  ! two meter temperature [K]
         Real,    Allocatable :: TEMPG    ( :,: )  ! skin temperature [K]
         Real,    Allocatable :: USTAR    ( :,: )  ! surface friction velocity [m/s]
         Real,    Allocatable :: VEG      ( :,: )  ! fractional vegetation coverage [ratio]
         Real,    Allocatable :: LAI      ( :,: )  ! grid cell leaf area index [m**2/m**2]
         Real,    Allocatable :: WR       ( :,: )  ! precip intercepted by canopy [m]
         Real,    Allocatable :: WSTAR    ( :,: )  ! convective velocity scale [m/s]
         Real,    Allocatable :: Z0       ( :,: )  ! roughness length [m]
         Real,    Allocatable :: SOIM1    ( :,: )  ! 1 cm soil moisture [m**3/m**3]
         Real,    Allocatable :: SOIM2    ( :,: )  ! 1 m soil moisture  [m**3/m**3]
         Real,    Allocatable :: SOIT1    ( :,: )  ! 1 cm soil temperature [K]
         Real,    Allocatable :: SOIT2    ( :,: )  ! 1 m soil temperature [K]
         Real,    Allocatable :: SEAICE   ( :,: )  ! Sea ice coverage [%]
         Real,    Allocatable :: MOL      ( :,: )  ! Monin-Obukhov length [m]
         Real,    Allocatable :: MOLI     ( :,: )  ! inverse of Monin-Obukhov length [m]
         Real,    Allocatable :: HOL      ( :,: )  ! PBL over Obukhov length
         Integer, Allocatable :: LPBL     ( :,: )  ! PBL layer
         Logical, Allocatable :: CONVCT   ( :,: )  ! convection flag
         Real,    Allocatable :: PBL      ( :,: )  ! pbl height (m)
!> U and V wind components on the cross grid points
         Real,    Allocatable :: UWIND    ( :,:,: )  ! [m/s]
         Real,    Allocatable :: VWIND    ( :,:,: )  ! [m/s]
!> 3-D meteorological fields:
         Real,    Allocatable :: KZMIN    ( :,:,: )  ! minimum Kz [m**2/s]
         Real,    Allocatable :: PRES     ( :,:,: )  ! layer 1 pressure [Pa]
         Real,    Allocatable :: QV       ( :,:,: )  ! water vapor mixing ratio
         Real,    Allocatable :: QC       ( :,:,: )  ! cloud water mixing ratio
         Real,    Allocatable :: THETAV   ( :,:,: )  ! potential temp
         Real,    Allocatable :: TA       ( :,:,: )  ! temperature (K)
         Real,    Allocatable :: ZH       ( :,:,: )  ! mid-layer height above ground [m]
         Real,    Allocatable :: ZF       ( :,:,: )  ! layer height [m]
         Real,    Allocatable :: DZF      ( :,:,: )  ! layer surface thickness
         Real,    Allocatable :: DENS     ( :,:,: )  ! air density
         Real,    Allocatable :: RJACM    ( :,:,: )  ! reciprocal mid-layer Jacobian
         Real,    Allocatable :: RJACF    ( :,:,: )  ! reciprocal full-layer Jacobian
         Real,    Allocatable :: RRHOJ    ( :,:,: )  ! reciprocal density X Jacobian
      End Type MET_Type
      
      Type :: GRID_Type
!> Grid infomation:
!> Vertical information

         Real,    Allocatable :: RDX3F  ( :,:,: )    ! reciprocal sigma layer thickness ! EMIS_DEFN.F, sedi.F, vdiffacmx.F, vdiffproc.F
# 165

!> Horizontal Information:
         Real,    Allocatable :: RMSFX4 ( :,: )  ! inverse map scale factor ** 4
         Real,    Allocatable :: LON    ( :,: )  ! longitude
         Real,    Allocatable :: LAT    ( :,: )  ! latitude
         Real,    Allocatable :: LWMASK ( :,: )  ! land water mask
         Real,    Allocatable :: OCEAN  ( :,: )  ! Open ocean
         Real,    Allocatable :: SZONE  ( :,: )  ! Surf zone
         Real,    Allocatable :: PURB   ( :,: )  ! percent urban [%]
         Integer, Allocatable :: SLTYP  ( :,: )  ! soil type [category]
# 177

         Real,    Allocatable :: WWLT   ( :,: )  ! soil wilting point
         Real,    Allocatable :: BSLP   ( :,: )  ! B Slope
         Real,    Allocatable :: WRES   ( :,: )  ! Soil residual moisture point
         Real,    Allocatable :: WFC    ( :,: )  ! soil field capacity
!        Real,    Allocatable :: RHOB   ( :,: )  ! soil bulk density
         Real, Allocatable :: LUFRAC  ( :,:,: ) ! land use fraction (col,row,lu_type)[ratio]
C Land use information:
         Character( 16 ), Allocatable   :: NAME    ( : )     ! LU name
         Character( 16 ), Allocatable   :: LU_Type ( : )     ! general land use type e.g. water, forest, etc.
      End Type GRID_Type

      Type :: MOSAIC_Type                 ! (col,row,lu)
         Character( 16 ), Allocatable :: NAME    ( : ) ! LU name
         Character( 16 ), Allocatable :: LU_Type ( : ) ! general land use type e.g. water, forest, etc.
!> Sub grid cell meteorological variables:
         Real, Allocatable :: USTAR ( :,:,: )   ! surface friction velocity [m/s]
         Real, Allocatable :: LAI   ( :,:,: )   ! leaf area index [m**2/m**2]
         Real, Allocatable :: VEG   ( :,:,: )   ! vegetation fraction [ratio]
         Real, Allocatable :: Z0    ( :,:,: )   ! vegetation fraction [ratio]
         Real, Allocatable :: DELTA ( :,:,: )   ! Surface wetness [ratio]
!> Sub grid cell resistances
         Real, Allocatable :: RA    ( :,:,: )    ! aerodynamic resistance [s/m]
         Real, Allocatable :: RSTW  ( :,:,: )    ! Stomatal Resistance of water [s/m]
         Real, Allocatable :: RINC  ( :,:,: )    ! In-canopy resistance [s/m]
      End Type MOSAIC_Type

      Type :: ChemMos_Type                 ! (col,row,lu,spc)
         Character( 16 ), Allocatable :: NAME    ( : )  ! LU name
         Character( 16 ), Allocatable :: Lu_Type ( : )  ! general land use type e.g. water, forest, etc.
         Character( 16 ), Allocatable :: SubName ( : )  ! Deposition species name
!> Sub grid cell chemically dependent resistances
         Real, Allocatable :: Rb   ( :,:,:,: ) ! quasi-laminar boundary layer resistance [s/m]
         Real, Allocatable :: Rst  ( :,:,:,: ) ! stomatal resistance [s/m]
         Real, Allocatable :: Rgc  ( :,:,:,: ) ! Canopy covered soil resistance [s/m]
         Real, Allocatable :: Rgb  ( :,:,:,: ) ! Barron soil resistance [s/m]
         Real, Allocatable :: Rcut ( :,:,:,: ) ! soil resistance [s/m]
         Real, Allocatable :: Rwat ( :,:,:,: ) ! surface water resistance [s/m]
!> Sub grid cell compensation point
         Real, Allocatable :: Catm ( :,:,:,: ) ! Atmospheric [ppm]
         Real, Allocatable :: CZ0  ( :,:,:,: ) ! compensation point at Z0 [ppm]
         Real, Allocatable :: Cleaf( :,:,:,: ) ! Leaf compensation point [ppm]
         Real, Allocatable :: Cstom( :,:,:,: ) ! Stomatal compensation point [ppm]
         Real, Allocatable :: Ccut ( :,:,:,: ) ! Cuticular compensation point [ppm]
         Real, Allocatable :: Csoil( :,:,:,: ) ! Soil compensation point [ppm]
      End Type ChemMos_Type

      Type( MET_Type ),     Save :: Met_Data 
      Type( GRID_Type ),    Save :: Grid_Data 
      Type( MOSAIC_Type ),  Save :: Mosaic_Data
      Type( ChemMos_Type ), Save :: ChemMos_Data

      Integer, Save   :: n_spc_m3dry = ltotg       ! from DEPVVARS module
!> M3 asx constants
      Real, Parameter :: a0         = 8.0        ! [dim'less]
      Real, Parameter :: d3         = 1.38564e-2 ! [dim'less]
      Real, Parameter :: dwat       = 0.2178     ! [cm^2/s] at 273.15K
      Real, Parameter :: hplus_ap   = 1.0e-6     ! pH=6.0 leaf apoplast solution Ph (Massad et al 2008)
      Real, Parameter :: hplus_def  = 1.0e-5     ! pH=5.0
      Real, Parameter :: hplus_east = 1.0e-5     ! pH=5.0
      Real, Parameter :: hplus_h2o  = 7.94328e-9 ! 10.0**(-8.1)
      Real, Parameter :: hplus_west = 3.16228e-6 ! 10.0**(-5.5)
      Real, Parameter :: kvis       = 0.132      ! [cm^2 / s] at 273.15K
      Real, Parameter :: pr         = 0.709      ! [dim'less]
      Real, Parameter :: rcut0      = 3000.0     ! [s/m]
      Real, Parameter :: rcw0       = 125000.0   ! acc'd'g to Padro and
      Real, Parameter :: resist_max = 1.0e30     ! maximum resistance
      Real, Parameter :: rg0        = 1000.0     ! [s/m]
      Real, Parameter :: rgwet0     = 25000.0    ! [s/m]
      Real, Parameter :: rsndiff    = 10.0       ! snow diffusivity fac
      Real, Parameter :: rsnow0     = 1000.0
      Real, Parameter :: svp2       = 17.67      ! from MM5 and WRF
      Real, Parameter :: svp3       = 29.65      ! from MM5 and WRF
      Real, Parameter :: rt25inK    = 1.0/(stdtemp + 25.0) ! 298.15K = 25C
      Real, Parameter :: twothirds  = 2.0 / 3.0
      Real, Parameter :: betah      = 5.0       ! WRF 3.6 px uses Dyer
      Real, Parameter :: gamah      = 16.0
      Real, Parameter :: pr0        = 0.95
      Real, Parameter :: karman     = 0.40
      Real, Parameter :: f3min      = 0.25
      Real, Parameter :: ftmin      = 0.0000001  ! m/s
      Real, Parameter :: nscat      = 16.0
      Real, Parameter :: rsmax      = 5000.0     ! s/m

      Real            :: ar       ( ltotg )        ! reactivity relative to HNO3
      Real            :: dif0     ( ltotg )        ! molecular diffusivity [cm2/s]
      Real            :: lebas    ( ltotg )        ! Le Bas molar volume [cm3/mol ]
      Real            :: meso     ( ltotg )        ! Exception for species that
                                                   ! react with cell walls. fo in
                                                   ! Wesely 1989 eq 6.
      Character( 16 ) :: subname  ( ltotg )        ! for subroutine HLCONST

      Logical, Save :: MET_INITIALIZED = .false.
      Real,    Save :: CONVPA             ! Pressure conversion factor file units to Pa
      Logical, Save :: MINKZ     
      Logical, Save :: CSTAGUV            ! Winds are available with C stagger?

      Logical, Save :: ifwr     = .true.
# 277

      Public        :: INIT_MET

      Logical, Private, Save :: ifsst    = .false.
      Logical, Private, Save :: ifq2     = .false.
      Logical, Private, Save :: rinv     = .True.
      Logical, Private, Save :: iflh     = .false.
      
      Integer,         Private :: C, R, L, S               ! loop induction variables
      Integer,         Private :: SPC
      Character( 16 ), Private, Save :: vname_rc, vname_rn, vname_uc, vname_vc
      Real,            Private, Save :: P0    ! reference pressure (100000.0 Pa) for Potential Temperature, note that in meteorology they do not use the SI 1 ATM.

      Integer, Private, Save :: LOGDEV
      Integer, Private, Save :: GXOFF, GYOFF              ! global origin offset from file
      Integer, Private, Save :: STRTCOLMC3, ENDCOLMC3, STRTROWMC3, ENDROWMC3 ! MET_CRO_3D
      Integer, Private, Save :: STRTCOLMD3, ENDCOLMD3, STRTROWMD3, ENDROWMD3 ! MET_DOT_3D
      Integer, Private, Save :: STRTCOLMC2, ENDCOLMC2, STRTROWMC2, ENDROWMC2 ! MET_CRO_2D
      Integer, Private, Save :: STRTCOL_O1, ENDCOL_O1, STRTROW_O1, ENDROW_O1 ! OCEAN_1

# 299

      Real, Pointer, Private :: BUFF2D( :,: )   ! 2D temp var
      Real, Pointer, Private :: BUFF3D( :,:,: ) ! 3D temp var

      DATA subname(  1), dif0(  1), ar(  1), meso(  1), lebas(  1) / 'SO2             ', 0.1089,   10.0,      0.0,  35.0/
      DATA subname(  2), dif0(  2), ar(  2), meso(  2), lebas(  2) / 'H2SO4           ', 0.1091, 8000.0,      0.0,  49.0/
      DATA subname(  3), dif0(  3), ar(  3), meso(  3), lebas(  3) / 'NO2             ', 0.1361,    2.0,      0.1,  21.0/
      DATA subname(  4), dif0(  4), ar(  4), meso(  4), lebas(  4) / 'NO              ', 0.1802,    2.0,      0.0,  14.0/
      DATA subname(  5), dif0(  5), ar(  5), meso(  5), lebas(  5) / 'O3              ', 0.1444,   12.0,      1.0,  21.0/
      DATA subname(  6), dif0(  6), ar(  6), meso(  6), lebas(  6) / 'HNO3            ', 0.1067, 8000.0,      0.0,  35.0/
      DATA subname(  7), dif0(  7), ar(  7), meso(  7), lebas(  7) / 'H2O2            ', 0.1300,34000.0,      1.0,  28.0/   !ar=34,000 such that r_cut=0.7 s/m as in Nguyen et al. 2015
      DATA subname(  8), dif0(  8), ar(  8), meso(  8), lebas(  8) / 'ACETALDEHYDE    ', 0.1111,   10.0,      0.0,  56.0/
      DATA subname(  9), dif0(  9), ar(  9), meso(  9), lebas(  9) / 'FORMALDEHYDE    ', 0.1554,   10.0,      0.0,  35.0/
      DATA subname( 10), dif0( 10), ar( 10), meso( 10), lebas( 10) / 'METHYLHYDROPEROX', 0.1179,   10.0,      0.3,  49.0/   !meso change from 0.1 to 0.3, Wolfe and Thornton 2011 ACP per J. Bash
      DATA subname( 11), dif0( 11), ar( 11), meso( 11), lebas( 11) / 'PEROXYACETIC_ACI', 0.0868,   20.0,      0.1,  70.0/
      DATA subname( 12), dif0( 12), ar( 12), meso( 12), lebas( 12) / 'ACETIC_ACID     ', 0.0944,   20.0,      0.0,  63.0/
      DATA subname( 13), dif0( 13), ar( 13), meso( 13), lebas( 13) / 'NH3             ', 0.1978,   20.0,      0.0,  28.0/
      DATA subname( 14), dif0( 14), ar( 14), meso( 14), lebas( 14) / 'PAN             ', 0.0687,   16.0,      0.1,  91.0/
      DATA subname( 15), dif0( 15), ar( 15), meso( 15), lebas( 15) / 'HNO2            ', 0.1349,   20.0,      0.1,  28.0/
      DATA subname( 16), dif0( 16), ar( 16), meso( 16), lebas( 16) / 'CO              ', 0.1807,    5.0,      0.0,  14.0/
      DATA subname( 17), dif0( 17), ar( 17), meso( 17), lebas( 17) / 'METHANOL        ', 0.1329,    2.0,      0.0,  42.0/
      DATA subname( 18), dif0( 18), ar( 18), meso( 18), lebas( 18) / 'N2O5            ', 0.0808, 5000.0,      0.0,  49.0/
      DATA subname( 19), dif0( 19), ar( 19), meso( 19), lebas( 19) / 'NO3             ', 0.1153, 5000.0,      0.0,  28.0/
      DATA subname( 20), dif0( 20), ar( 20), meso( 20), lebas( 20) / 'GENERIC_ALDEHYDE', 0.0916,   10.0,      0.0,  56.0/
      DATA subname( 21), dif0( 21), ar( 21), meso( 21), lebas( 21) / 'CL2             ', 0.1080,   10.0,      0.0,  49.0/
      DATA subname( 22), dif0( 22), ar( 22), meso( 22), lebas( 22) / 'HOCL            ', 0.1300,   10.0,      0.0,  38.5/
      DATA subname( 23), dif0( 23), ar( 23), meso( 23), lebas( 23) / 'HCL             ', 0.1510, 8000.0,      0.0,  31.5/
      DATA subname( 24), dif0( 24), ar( 24), meso( 24), lebas( 24) / 'FMCL            ', 0.1094,   10.0,      0.0,  45.5/
      DATA subname( 25), dif0( 25), ar( 25), meso( 25), lebas( 25) / 'HG              ', 0.1194,    0.1,      0.0,  14.8/   ! lebas not used
      DATA subname( 26), dif0( 26), ar( 26), meso( 26), lebas( 26) / 'HGIIGAS         ', 0.0976, 8000.0,      0.0,  95.0/   ! estimation from back calculating to get dw25 = 1.04e-5 (Garland et al, 1965)
      DATA subname( 27), dif0( 27), ar( 27), meso( 27), lebas( 27) / 'TECDD_2378      ', 0.0525,    2.0,      0.0, 217.0/
      DATA subname( 28), dif0( 28), ar( 28), meso( 28), lebas( 28) / 'PECDD_12378     ', 0.0508,    2.0,      0.0, 234.5/
      DATA subname( 29), dif0( 29), ar( 29), meso( 29), lebas( 29) / 'HXCDD_123478    ', 0.0494,    2.0,      0.0, 252.0/
      DATA subname( 30), dif0( 30), ar( 30), meso( 30), lebas( 30) / 'HXCDD_123678    ', 0.0494,    2.0,      0.0, 252.0/
      DATA subname( 31), dif0( 31), ar( 31), meso( 31), lebas( 31) / 'HXCDD_123478    ', 0.0494,    2.0,      0.0, 252.0/
      DATA subname( 32), dif0( 32), ar( 32), meso( 32), lebas( 32) / 'HPCDD_1234678   ', 0.0480,    2.0,      0.0, 269.5/
      DATA subname( 33), dif0( 33), ar( 33), meso( 33), lebas( 33) / 'OTCDD           ', 0.0474,    2.0,      0.0, 287.0/
      DATA subname( 34), dif0( 34), ar( 34), meso( 34), lebas( 34) / 'TECDF_2378      ', 0.0534,    2.0,      0.0, 210.0/
      DATA subname( 35), dif0( 35), ar( 35), meso( 35), lebas( 35) / 'PECDF_12378     ', 0.0517,    2.0,      0.0, 227.5/
      DATA subname( 36), dif0( 36), ar( 36), meso( 36), lebas( 36) / 'PECDF_23478     ', 0.0517,    2.0,      0.0, 227.5/
      DATA subname( 37), dif0( 37), ar( 37), meso( 37), lebas( 37) / 'HXCDF_123478    ', 0.0512,    2.0,      0.0, 245.0/
      DATA subname( 38), dif0( 38), ar( 38), meso( 38), lebas( 38) / 'HXCDF_123678    ', 0.0512,    2.0,      0.0, 245.0/
      DATA subname( 39), dif0( 39), ar( 39), meso( 39), lebas( 39) / 'HXCDF_234678    ', 0.0512,    2.0,      0.0, 245.0/
      DATA subname( 40), dif0( 40), ar( 40), meso( 40), lebas( 40) / 'HXCDF_123789    ', 0.0512,    2.0,      0.0, 245.0/
      DATA subname( 41), dif0( 41), ar( 41), meso( 41), lebas( 41) / 'HPCDF_1234678   ', 0.0487,    2.0,      0.0, 262.5/
      DATA subname( 42), dif0( 42), ar( 42), meso( 42), lebas( 42) / 'HPCDF_1234789   ', 0.0487,    2.0,      0.0, 262.5/
      DATA subname( 43), dif0( 43), ar( 43), meso( 43), lebas( 43) / 'OTCDF           ', 0.0474,    2.0,      0.0, 280.0/
      DATA subname( 44), dif0( 44), ar( 44), meso( 44), lebas( 44) / 'NAPHTHALENE     ', 0.0778,    4.0,      0.0, 119.0/
      DATA subname( 45), dif0( 45), ar( 45), meso( 45), lebas( 45) / '1NITRONAPHTHALEN', 0.0692,    4.0,      0.0, 133.0/
      DATA subname( 46), dif0( 46), ar( 46), meso( 46), lebas( 46) / '2NITRONAPHTHALEN', 0.0692,    4.0,      0.0, 133.0/
      DATA subname( 47), dif0( 47), ar( 47), meso( 47), lebas( 47) / '14NAPHTHOQUINONE', 0.0780,    4.0,      0.0, 119.0/
      DATA subname( 48), dif0( 48), ar( 48), meso( 48), lebas( 48) / 'HEXAMETHYLE_DIIS', 0.0380,   10.0,      0.0, 196.0/
      DATA subname( 49), dif0( 49), ar( 49), meso( 49), lebas( 49) / 'HYDRAZINE       ', 0.4164,   20.0,      0.0,  42.0/
      DATA subname( 50), dif0( 50), ar( 50), meso( 50), lebas( 50) / 'MALEIC_ANHYDRIDE', 0.0950,   10.0,      0.0,  70.0/
      DATA subname( 51), dif0( 51), ar( 51), meso( 51), lebas( 51) / '24-TOLUENE_DIIS ', 0.0610,   10.0,      0.0, 154.0/
      DATA subname( 52), dif0( 52), ar( 52), meso( 52), lebas( 52) / 'TRIETHYLAMINE   ', 0.0881,   20.0,      0.0, 154.0/
      DATA subname( 53), dif0( 53), ar( 53), meso( 53), lebas( 53) / 'ORG_NTR         ', 0.0607,   16.0,      0.0, 160.0/  ! assumes 58.2% C5H11O4N and 41.8% C5H11O3N
      DATA subname( 54), dif0( 54), ar( 54), meso( 54), lebas( 54) / 'HYDROXY_NITRATES', 0.0609,   16.0,      0.0, 156.1/
      DATA subname( 55), dif0( 55), ar( 55), meso( 55), lebas( 55) / 'MPAN            ', 0.0580,   16.0,      0.1, 133.0/
      DATA subname( 56), dif0( 56), ar( 56), meso( 56), lebas( 56) / 'PPN             ', 0.0631,   16.0,      0.1, 118.2/
      DATA subname( 57), dif0( 57), ar( 57), meso( 57), lebas( 57) / 'MVK             ', 0.0810,    8.0,      1.0,  88.8/
      DATA subname( 58), dif0( 58), ar( 58), meso( 58), lebas( 58) / 'DINTR           ', 0.0617,   16.0,      0.1, 169.8/
      DATA subname( 59), dif0( 59), ar( 59), meso( 59), lebas( 59) / 'NTR_ALK         ', 0.0688,   16.0,      0.1, 133.0/
      DATA subname( 60), dif0( 60), ar( 60), meso( 60), lebas( 60) / 'NTR_OH          ', 0.0665,   16.0,      0.1, 140.4/
      DATA subname( 61), dif0( 61), ar( 61), meso( 61), lebas( 61) / 'HYDROXY_NITRATES', 0.0646,   16.0,      0.0, 147.8/
      DATA subname( 62), dif0( 62), ar( 62), meso( 62), lebas( 62) / 'PROPNN          ', 0.0677,   16.0,      0.0, 133.0/
      DATA subname( 63), dif0( 63), ar( 63), meso( 63), lebas( 63) / 'NITRYL_CHLORIDE ', 0.0888,    8.0,      0.0,  45.5/  ! dif0 estimated following Erickson III et al., JGR, 104, D7, 8347-8372, 1999
      DATA subname( 64), dif0( 64), ar( 64), meso( 64), lebas( 64) / 'ISOPNN          ',0.0457,    8.0,      0.0,  206.8/
      DATA subname( 65), dif0( 65), ar( 65), meso( 65), lebas( 65) / 'MTNO3           ',0.0453,    8.0,      0.0,  251.2/
      DATA subname( 66), dif0( 66), ar( 66), meso( 66), lebas( 66) / 'IEPOX           ',0.0579,    8.0,      0.0,  110.8/
      DATA subname( 67), dif0( 67), ar( 67), meso( 67), lebas( 67) / 'HACET           ',0.1060,    8.0,      0.0,   72.6/  ! dif0 from Nguyen 2015 PNAS
      DATA subname( 68), dif0( 68), ar( 68), meso( 68), lebas( 68) / 'SVALK1          ',0.0514,   20.0,      0.0,  280.5/
      DATA subname( 69), dif0( 69), ar( 69), meso( 69), lebas( 69) / 'SVALK2          ',0.0546,   20.0,      0.0,  275.6/
      DATA subname( 70), dif0( 70), ar( 70), meso( 70), lebas( 70) / 'SVBNZ1          ',0.0642,   20.0,      0.0,  134.1/
      DATA subname( 71), dif0( 71), ar( 71), meso( 71), lebas( 71) / 'SVBNZ2          ',0.0726,   20.0,      0.0,  127.5/
      DATA subname( 72), dif0( 72), ar( 72), meso( 72), lebas( 72) / 'SVISO1          ',0.0733,   20.0,      0.0,  126.3/
      DATA subname( 73), dif0( 73), ar( 73), meso( 73), lebas( 73) / 'SVISO2          ',0.0729,   20.0,      0.0,  123.8/
      DATA subname( 74), dif0( 74), ar( 74), meso( 74), lebas( 74) / 'SVPAH1          ',0.0564,   20.0,      0.0,  235.7/
      DATA subname( 75), dif0( 75), ar( 75), meso( 75), lebas( 75) / 'SVPAH2          ',0.0599,   20.0,      0.0,  231.5/
      DATA subname( 76), dif0( 76), ar( 76), meso( 76), lebas( 76) / 'SVSQT           ',0.0451,   20.0,      0.0,  346.5/
      DATA subname( 77), dif0( 77), ar( 77), meso( 77), lebas( 77) / 'SVTOL1          ',0.0637,   20.0,      0.0,  153.7/
      DATA subname( 78), dif0( 78), ar( 78), meso( 78), lebas( 78) / 'SVTOL2          ',0.0607,   20.0,      0.0,  194.1/
      DATA subname( 79), dif0( 79), ar( 79), meso( 79), lebas( 79) / 'SVTRP1          ',0.0603,   20.0,      0.0,  194.9/
      DATA subname( 80), dif0( 80), ar( 80), meso( 80), lebas( 80) / 'SVTRP2          ',0.0559,   20.0,      0.0,  218.8/
      DATA subname( 81), dif0( 81), ar( 81), meso( 81), lebas( 81) / 'SVXYL1          ',0.0610,   20.0,      0.0,  154.6/
      DATA subname( 82), dif0( 82), ar( 82), meso( 82), lebas( 82) / 'SVXYL2          ',0.0585,   20.0,      0.0,  194.6/
      DATA subname( 83), dif0( 83), ar( 83), meso( 83), lebas( 83) / 'IO              ',0.1002,    8.0,      0.0,  44.4/
      DATA subname( 84), dif0( 84), ar( 84), meso( 84), lebas( 84) / 'OIO             ',0.0938,    8.0,      0.0,  51.8/
      DATA subname( 85), dif0( 85), ar( 85), meso( 85), lebas( 85) / 'I2O2            ',0.0732,    8.0,      0.0,  88.8/
      DATA subname( 86), dif0( 86), ar( 86), meso( 86), lebas( 86) / 'I2O3            ',0.0707,    8.0,      0.0,  96.2/
      DATA subname( 87), dif0( 87), ar( 87), meso( 87), lebas( 87) / 'I2O4            ',0.0684,    8.0,      0.0, 103.6/
      DATA subname( 88), dif0( 88), ar( 88), meso( 88), lebas( 88) / 'HI              ',0.1045,    8.0,      0.0,  40.7/
      DATA subname( 89), dif0( 89), ar( 89), meso( 89), lebas( 89) / 'HOI             ',0.0972,    8.0,      0.0,  48.1/
      DATA subname( 90), dif0( 90), ar( 90), meso( 90), lebas( 90) / 'INO             ',0.0882,    8.0,      0.0,  60.9/
      DATA subname( 91), dif0( 91), ar( 91), meso( 91), lebas( 91) / 'INO2            ',0.0883,   20.0,      0.0,  69.2/
      DATA subname( 92), dif0( 92), ar( 92), meso( 92), lebas( 92) / 'IONO2           ',0.0792,    8.0,      0.0,  77.5/
      DATA subname( 93), dif0( 93), ar( 93), meso( 93), lebas( 93) / 'BRO             ',0.1144,    1.0,      0.0,  34.4/
      DATA subname( 94), dif0( 94), ar( 94), meso( 94), lebas( 94) / 'HOBR            ',0.1101,    1.0,      0.0,  38.1/
      DATA subname( 95), dif0( 95), ar( 95), meso( 95), lebas( 95) / 'HBR             ',0.1216,    2.0,      0.0,  30.7/
      DATA subname( 96), dif0( 96), ar( 96), meso( 96), lebas( 96) / 'BRONO2          ',0.0855,    1.0,      0.0,  67.5/
      DATA subname( 97), dif0( 97), ar( 97), meso( 97), lebas( 97) / 'BRNO2           ',0.0909,    1.0,      0.0,  59.2/
      DATA subname( 98), dif0( 98), ar( 98), meso( 98), lebas( 98) / 'BRCL            ',0.0966,    1.0,      0.0,  51.6/
      DATA subname( 99), dif0( 99), ar( 99), meso( 99), lebas( 99) / 'DMS             ',0.0926,    2.0,      0.0,  77.4/
      DATA subname(100), dif0(100), ar(100), meso(100), lebas(100) / 'MSA             ',0.0896,    2.0,      0.0,  77.4/
      DATA subname(101), dif0(101), ar(101), meso(101), lebas(101) / 'METHANE         ',0.2107,    2.0,      0.0,  29.6/ ! dif0, equation 9-22. Scwarzenbach et. (1993) Env. Org. Chem.
      DATA subname(102), dif0(102), ar(102), meso(102), lebas(102) / 'ACRYACID        ',0.0908,    2.0,      0.0,  63.2/ 
      DATA subname(103), dif0(103), ar(103), meso(103), lebas(103) / 'CARBSULFIDE     ',0.1240,    5.0,      0.0,  51.5/ 
      DATA subname(104), dif0(104), ar(104), meso(104), lebas(104) / 'ACETONITRILE    ',0.1280,    5.0,      0.0,  52.3/ 
      DATA subname(105), dif0(105), ar(105), meso(105), lebas(105) / '6_NITRO_O_CRESOL',0.0664,   16.0,      0.0, 155.0/ ! dif0, equation 9-22. Scwarzenbach et. (1993) Env. Org. Chem.

      CONTAINS

C=======================================================================
         Subroutine INIT_MET ( JDATE, JTIME, MOSAIC, ABFLUX, HGBIDI )

C-----------------------------------------------------------------------
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX;
C                      allocatable RDEPVHT, RJACM, RRHOJ
C   14 Nov 03 J.Young: add reciprocal vertical Jacobian product for full and
C                      mid-layer
C   Tanya took JACOBF out of METCRO3D! Improvise
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C-----------------------------------------------------------------------

         Use UTILIO_DEFN

         Implicit None
 
         Include SUBST_FILES_ID  ! file name parameters
         Include SUBST_CONST     ! constants

C Arguments:
         Integer, Intent( IN ) :: JDATE, JTIME      ! internal simulation date&time
         Logical, Intent( IN ) :: MOSAIC
         Logical, Intent( IN ) :: ABFLUX
         Logical, Intent( IN ) :: HGBIDI
C File variables:
         Real, Pointer    :: MSFX2   ( :,: )
         Real, Pointer    :: SOILCAT ( :,: )
# 442


C Local variables:
         Character( 16 ) :: PNAME = 'INIT_MET'
         Character( 16 ) :: VNAME
         CHARACTER( 16 ) :: UNITSCK
         CHARACTER( 30 ) :: MSG1 = ' Error interpolating variable '
         Character( 96 ) :: XMSG = ' '

C for INTERPX
         Integer STRTCOLGC2, ENDCOLGC2, STRTROWGC2, ENDROWGC2
         Integer V
         Integer ALLOCSTAT

C-----------------------------------------------------------------------

         LOGDEV = INIT3()
# 461

!> Allocate buffers

         ALLOCATE (
# 467

     &              BUFF2D( NCOLS,NROWS ),
     &              BUFF3D( NCOLS,NROWS,NLAYS ), STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating Buffers'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

# 477

         BUFF2D = 0.0
         BUFF3D = 0.0

!> Allocate shared arrays
!> Met_Data
         ALLOCATE( Met_Data%RDEPVHT  ( NCOLS,NROWS ),
     &             Met_Data%DENS1    ( NCOLS,NROWS ),
     &             Met_Data%PRSFC    ( NCOLS,NROWS ),
     &             Met_Data%Q2       ( NCOLS,NROWS ),
!#ifndef 1
     *             Met_Data%QSS_GRND ( NCOLS,NROWS ),
!#endif
     &             Met_Data%RH       ( NCOLS,NROWS ),
     &             Met_Data%RA       ( NCOLS,NROWS ),
     &             Met_Data%RS       ( NCOLS,NROWS ),
# 496

     &             Met_Data%RGRND    ( NCOLS,NROWS ),
     &             Met_Data%HFX      ( NCOLS,NROWS ),
     &             Met_Data%LH       ( NCOLS,NROWS ),
     &             Met_Data%SNOCOV   ( NCOLS,NROWS ),
     &             Met_Data%TEMP2    ( NCOLS,NROWS ),
     &             Met_Data%TEMPG    ( NCOLS,NROWS ),
# 505

     &             Met_Data%USTAR    ( NCOLS,NROWS ),
     &             Met_Data%VEG      ( NCOLS,NROWS ),
     &             Met_Data%LAI      ( NCOLS,NROWS ),
     &             Met_Data%WR       ( NCOLS,NROWS ),
# 512

     &             Met_Data%WSTAR    ( NCOLS,NROWS ),
     &             Met_Data%Z0       ( NCOLS,NROWS ),
     &             Met_Data%SOIM1    ( NCOLS,NROWS ),
# 518

     &             Met_Data%SEAICE   ( NCOLS,NROWS ),
     &             Met_Data%MOL      ( NCOLS,NROWS ),
     &             Met_Data%MOLI     ( NCOLS,NROWS ),
     &             Met_Data%HOL      ( NCOLS,NROWS ),
# 525

     &             Met_Data%LPBL     ( NCOLS,NROWS ),
     &             Met_Data%CONVCT   ( NCOLS,NROWS ),
     &             Met_Data%PBL      ( NCOLS,NROWS ),
# 531

     &             Met_Data%UWIND    ( NCOLS+1,NROWS+1,NLAYS ),
     &             Met_Data%VWIND    ( NCOLS+1,NROWS+1,NLAYS ),
     &             Met_Data%KZMIN    ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%PRES     ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%QV       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%QC       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%THETAV   ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%TA       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%ZH       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%ZF       ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%DZF      ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%DENS     ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%RJACM    ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%RJACF    ( NCOLS,NROWS,NLAYS ),
     &             Met_Data%RRHOJ    ( NCOLS,NROWS,NLAYS ),
     &             STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating met vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If


         ALLOCATE(
     &             Grid_Data%RDX3F   ( NCOLS,NROWS,NLAYS ),
# 560

     &             Grid_Data%RMSFX4  ( NCOLS,NROWS ),
     &             Grid_Data%LON     ( NCOLS,NROWS ),
     &             Grid_Data%LAT     ( NCOLS,NROWS ),
     &             Grid_Data%LWMASK  ( NCOLS,NROWS ),
     &             Grid_Data%OCEAN   ( NCOLS,NROWS ),
     &             Grid_Data%SZONE   ( NCOLS,NROWS ),
     &             Grid_Data%PURB    ( NCOLS,NROWS ),
     &             Grid_Data%SLTYP   ( NCOLS,NROWS ),
     &             Grid_Data%NAME    ( n_lufrac ),
     &             Grid_Data%LU_Type ( n_lufrac ),
     &             STAT = ALLOCSTAT )
         If ( ALLOCSTAT .Ne. 0 ) Then
            XMSG = 'Failure allocating grid vars'
            Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
         End If

         If ( ABFLUX .Or. HGBIDI .Or. MOSAIC ) Then
            ALLOCATE( Met_Data%SOIM2    ( NCOLS,NROWS ),
     &                Met_Data%SOIT2    ( NCOLS,NROWS ),
     &                STAT = ALLOCSTAT )
            If ( ALLOCSTAT .Ne. 0 ) Then
               XMSG = 'Failure allocating mosaic met vars'
               Call M3EXIT( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
            End If
         End If
# 675

!> ccccccccccccccccccccc enable backward compatiblity ccccccccccccccccccccc

         CONVPA = 1.0
         P0     = 100000.0
         MINKZ = .False.   ! Don't have Purb yet
# 779

         If ( .Not. MINKZ ) Then
            XMSG = 'This run uses Kz0UT, *NOT* KZMIN in subroutine edyintb.'
            Write( LOGDEV,'(/5X, A, /)' ) XMSG
         End If


!        Do C = 1, NCOLS
!           Do L = 1, NLAYS
!              Grid_Data%RDX3M( C,1,L ) = 1.0 / g3ddata(C,1,L,zf_ind)
!              Grid_Data%RDX3M( C,1,L ) = 1.0
!           End Do
!        End Do
         Grid_Data%RMSFX4 = 1.0
!        Grid_Data%LON = g2ddata(:,:,lon_ind)
!        Grid_Data%LAT = g2ddata(:,:,lat_ind)
!        Grid_Data%LWMASK = g2ddata(:,:,lwmask_ind)
!        Grid_Data%OCEAN = g2ddata(:,:,open_ind)
!        Grid_Data%SZONE = g2ddata(:,:,szone_ind)
         Grid_Data%LON = 1.0
         Grid_Data%LAT = 1.0
         Grid_Data%LWMASK = 1.0
         Grid_Data%OCEAN = 1.0
         Grid_Data%SZONE = 1.0
# 933

         Return
         End Subroutine INIT_MET

C=======================================================================
         Subroutine GET_MET ( JDATE, JTIME, TSTEP, MOSAIC, ABFLUX, HGBIDI )

C-----------------------------------------------------------------------
C   30 Mar 01 J.Young: dyn alloc - Use HGRD_DEFN; replace INTERP3 with INTERPX;
C                      allocatable RDEPVHT, RJACM, RRHOJ
C   14 Nov 03 J.Young: add reciprocal vertical Jacobian product for full and
C                      mid-layer
C   Tanya took JACOBF out of METCRO3D! Improvise
C   31 Jan 05 J.Young: dyn alloc - establish both horizontal & vertical
C                      domain specifications in one module
C   16 Feb 11 S.Roselle: replaced I/O API include files with UTILIO_DEFN
C-----------------------------------------------------------------------

         USE GRID_CONF       ! horizontal & vertical domain specifications
         Use UTILIO_DEFN
# 959

         Implicit None
# 965

C Arguments:

         Integer, Intent( IN ) :: JDATE, JTIME, TSTEP      ! internal simulation date&time
         Logical, Intent( IN ) :: MOSAIC
         Logical, Intent( IN ) :: ABFLUX
         Logical, Intent( IN ) :: HGBIDI

C Parameters:
         Real, Parameter :: cond_min = 1.0 / resist_max ! minimum conductance [m/s]
         Real, Parameter :: KZMAXL = 500.0    ! upper limit for min Kz [m]

         Real, Parameter :: KZ0UT  = 1.0      ! minimum eddy diffusivity [m**2/sec] KZ0
# 980

         Real, Parameter :: KZL    = 0.01     ! lowest KZ
         Real, Parameter :: KZU    = 1.0      ! 2.0  ! highest KZ
         Real, Parameter :: EPS    = 1.0E-08  ! small number for temperature difference

C Local variables:
         Real    FINT
         Real    CPAIR, LV, QST 
         Real    TMPFX, TMPVTCON, TST, TSTV
         Real, Pointer    :: Es_Grnd ( :,: )
         Real, Pointer    :: Es_Air  ( :,: )
         Real, Pointer    :: TV      ( :,:,: )
         Integer LP
         Integer C, R, L         ! loop induction variables

         Character( 16 ) :: PNAME = 'GET_MET'
         Character( 16 ) :: VNAME
         CharactER( 30 ) :: MSG1 = ' Error interpolating variable '
         Character( 96 ) :: XMSG = ' '

C-----------------------------------------------------------------------
C Interpolate file input variables and format for output
C-------------------------------- MET_CRO_3D --------------------------------

!        Met_Data%ZH = g3ddata(:,:,:,zh_ind)
!        Met_Data%PRES = g3ddata(:,:,:,pres_ind)
!        Met_Data%ZF = g3ddata(:,:,:,zf_ind)
!        Met_Data%DENS = g3ddata(:,:,:,dens_ind)
         Met_Data%ZH = 1.0
         Met_Data%PRES = 1.0
         Met_Data%ZF = 1.0
         Met_Data%DENS = 1.0
         Met_Data%DENS1 = Met_Data%DENS( :,:,1 )
         Met_Data%RJACM = 1.0
         Met_Data%RJACM = 1.0 / Met_Data%RJACM
         Met_Data%RJACF = 1.0
         Met_Data%RJACF = 1.0 / Met_Data%RJACF
!        Met_Data%RRHOJ = g3ddata(:,:,:,densa_j_ind)
         Met_Data%RRHOJ = 1.0
         Met_Data%RRHOJ = 1.0 / Met_Data%RRHOJ
!        Met_Data%Ta = g3ddata(:,:,:,temp_ind)
!        Met_Data%QV = g3ddata(:,:,:,qv_ind)
!        Met_Data%QC = g3ddata(:,:,:,qc_ind)
         Met_Data%Ta = 1.0
         Met_Data%QV = 1.0
         Met_Data%QC = 1.0
!        Met_Data%LAI = g2ddata(:,:,lai_ind)
!        Met_Data%VEG = g2ddata(:,:,vegpx_ind)
!        Met_Data%Z0  = g2ddata(:,:,znt_ind)
!        Met_Data%SOIM1 = smois_data
         Met_Data%LAI = 1.0
         Met_Data%VEG = 1.0
         Met_Data%Z0  = 1.0
         Met_Data%SOIM1 = 1.0
!        Met_Data%SEAICE = g2ddata(:,:,seaice_ind)
!        Met_Data%PRSFC = g2ddata(:,:,prsfc_ind)
!        Met_Data%SNOCOV = g2ddata(:,:,snocov_ind)
         Met_Data%SEAICE = 1.0
         Met_Data%PRSFC = 1.0
         Met_Data%SNOCOV = 1.0
         Where( Met_Data%SNOCOV .Lt. 0.0 )
            Met_Data%SNOCOV = 0.0
         End Where
!        Met_Data%TEMP2 = g2ddata(:,:,temp2_ind)
!        Met_Data%TEMPG = g2ddata(:,:,tempg_ind)
!        Met_Data%USTAR = g2ddata(:,:,ustar_ind)
!        Met_Data%HFX = g2ddata(:,:,hfx_ind)
!        Met_Data%LH = g2ddata(:,:,lh_ind)
!        Met_Data%PBL = g2ddata(:,:,pbl_ind)
!        Met_Data%WR = g2ddata(:,:,canwat_ind)
!        Met_Data%Ra = g2ddata(:,:,ra_ind)
!        Met_Data%RS = g2ddata(:,:,rs_ind)
!        Met_Data%Q2 = g2ddata(:,:,q2_ind)
         Met_Data%TEMP2 = 1.0
         Met_Data%TEMPG = 1.0
         Met_Data%USTAR = 1.0
         Met_Data%HFX = 1.0
         Met_Data%LH = 1.0
         Met_Data%PBL = 1.0
         Met_Data%WR = 1.0
         Met_Data%Ra = 1.0
         Met_Data%RS = 1.0
         Met_Data%Q2 = 1.0
         Met_Data%RGRND = 1.0
# 1417

         Es_Grnd => BUFF2D
         Where( Met_Data%TEMPG .Lt. stdtemp )
            Es_Grnd = vp0 *Exp( 22.514 - ( 6.15e3 / Met_Data%TEMPG ) )
         Elsewhere
            Es_Grnd = vp0 *Exp( svp2 * ( Met_Data%TEMPG -stdtemp ) / ( Met_Data%TEMPG -svp3 ) ) 
         End Where
         Met_Data%QSS_GRND = Es_Grnd * 0.622 / ( Met_Data%PRSFC - Es_Grnd )
         Nullify( Es_Grnd )
!#endif
         Es_Air => BUFF2D
         Where( Met_Data%TEMP2 .Lt. stdtemp )
            Es_Air = vp0 *Exp( 22.514 - ( 6.15e3 / Met_Data%TEMP2 ) )
         Elsewhere
            Es_Air = vp0 *Exp( svp2 * ( Met_Data%TEMP2 -stdtemp ) / ( Met_Data%TEMP2 -svp3 ) ) 
         End Where
         Met_Data%RH = Met_Data%Q2 / ( Es_Air * 0.622 / ( Met_Data%PRSFC - Es_Air ) ) * 100.0
         Where( Met_Data%RH .Gt. 100.0 )
            Met_Data%RH = 100.0
         Elsewhere( Met_Data%RH .lt. 0.0 )
            Met_Data%RH = 0.0
         End Where
         Nullify( Es_Air )

C-------------------------------- MET_DOT_3D --------------------------------
# 1460

C-------------------------------- Calculated Variables --------------------------------
         Met_Data%DZF = Met_Data%ZF - EOSHIFT(Met_Data%ZF, Shift = -1, Boundary = 0.0, Dim = 3)

         Met_Data%RDEPVHT = 1.0 / Met_Data%ZF( :,:,1 )

         IF ( MINKZ ) THEN
            Met_Data%KZMIN = KZL
            DO L = 1, NLAYS
               Where( Met_Data%ZF( :,:,L ) .LE. KZMAXL )
                  Met_Data%KZMIN( :,:,L ) = KZL + ( KZU - KZL ) * 0.01 * Grid_data%PURB
               End Where
            End Do
         ELSE
            Met_Data%KZMIN = KZ0UT
         END IF

         TV => BUFF3D
         TV = Met_Data%TA * ( 1.0 + 0.608 * Met_Data%QV )
         Met_Data%THETAV = TV * ( P0 / Met_Data%PRES ) ** 0.286
         Nullify( TV )


!        Met_Data%MOL = 1.0 / g2ddata(:,:,rmol_ind)
         Met_Data%MOL = 1.0


C------  Updating MOL, then WSTAR, MOLI, HOL
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
# 1509

               IF ( Met_Data%MOL( C,R ) .LT. 0.0 ) THEN
                  Met_Data%WSTAR( C,R ) = Met_Data%USTAR( C,R ) * ( Met_Data%PBL( C,R )
     &                                  / ( karman * ABS( Met_Data%MOL( C,R ) ) ) ) ** 0.333333
               ELSE
                  Met_Data%WSTAR( C,R ) = 0.0
               END IF

            END DO
         END DO
   
         Met_Data%MOLI  = 1.0 / Met_Data%MOL 
         Met_Data%HOL   = Met_Data%PBL / Met_Data%MOL
C------

         Met_Data%CONVCT = .FALSE.
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS
               DO L = 1, NLAYS
                  IF ( Met_Data%PBL( C,R ) .LT. Met_Data%ZF( C,R,L ) ) THEN
                     LP = L; EXIT
                  END IF
               END DO

               Met_Data%LPBL( C,R ) = LP
# 1546

            END DO
         END DO
         Where( Met_Data%THETAV( :,:,1 ) - Met_Data%THETAV( :,:,2 ) .Gt. EPS .And.
     &          Met_Data%HOL .Lt. -0.02 .And. Met_Data%LPBL .Gt. 3 )
            Met_Data%CONVCT = .True.
         End Where

         Return
         End Subroutine GET_MET

      End Module ASX_DATA_MOD
#endif
