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

! RCS file, release, date & time of last delta, author, state, [and locker]
! $Header$

! what(1) key, module and SID; SCCS file; date and time of last delta:
! %W% %P% %G% %U%

   module can_levs_defn

!**********************************************************************
!
!  Function: Initializes fields and species concentrations for the explicit
!     3 sub-layer canopy shading effect, and allocates arrays.
!     Uses canopy height (fch), and fractions of canopy height to
!     gather meteorological and geophysical data into vertical
!     columns containing combined canopy + no-canopy columns
!     (these have the original, "resolved scale" model layers NLAYS interleaved
!     with NLAYC canopy layers). For the canopy layers, various algorithms are used
!     to fill in the missing meteorological data.
!
!     On the first timestep, the subroutine also initializes the
!     concentrations of the canopy layers using the value of the
!     lowest resolved scale model layer.
!
!  Preconditions: GRID_CONF defines the vertical canopy layer structure
!     For the CB6R3 family of gas-phase chemical mechanisms performed by
!     either the Euler Backward Iterative (EBI) solver or the vectorized Rosenbrock (ROS3) solver
!     Activated with config namelist parameter do_aqm_canopy= .true.
!
!  Key Subroutines/Functions Called: GET_PHOT_MET
!
!  Revision History: Created by I. Ivanova March 2024.
!     Based on Makar, P. A. et al. "The effects of forest canopy shading
!     and turbulence on boundary layer ozone." Nat. Commun. 8, 15243
!     doi: 10.1038/ncomms15243 (2017).
!**********************************************************************

   implicit none

   integer(kind=4) :: kcan_top
   real   (kind=4) :: hcan

! Vertical index, kmod: location of original ungathered model layer in combined canopy + resolved scale column
   integer(kind=4), dimension(:,:,:), allocatable, save :: kmod
   real   (kind=4), dimension(:), allocatable, save :: zmid, zmom            !Paul's zt is our zmid  and Paul's zmom is our zmom
! Original model arrays of z which include the surface, to allow interpolation of met vars
   real   (kind=4), dimension(:), allocatable :: z2, sigmid2, sigmom   ! Paul's sigt2 is our sigmid2


! Vertical index, kcan3: location of canopy ungathered model layer in combined canopy + resolved scale column
   integer(kind=4), dimension(:,:,:), allocatable, save :: kcan3
   real   (kind=4), dimension(:), allocatable :: zcan3, ta_can3, ws_can3, qv_can3, pres_can3, dens_can3, &
                                                        ta3, qv3, ws3, pres3, dens3
   integer(kind=4), dimension(:), allocatable :: klower_can


! Full-layer heights & sigma levels in gathered canopy + resolved scale columns
   real   (kind=4), dimension(:,:,:), allocatable, save :: zmom_can   !Paul's zmomcan is our   zmom_can ! rename to full
   real   (kind=4), dimension(:,:,:), allocatable, save :: sigmom_can !Paul's sigmcan is our sigmom_can ! rename to full
! Mid-layer heights & sigma levels in gathered forest canopy columns
   real   (kind=4), dimension(:,:,:), allocatable, save :: zmid_can   !Paul's zthrmcan is our   zmid_can
   real   (kind=4), dimension(:,:,:), allocatable, save :: sigmid_can ! Paul's sigtcan is our sigmid_can
! Grid area
   real   (kind=4), dimension(:,:), allocatable, save :: dxdy

! met3d arrays
   real  (kind=4), dimension( :, :, : ), allocatable, save :: ZH_CAN
   real  (kind=4), dimension( :, :, : ), allocatable, save :: ZF_CAN
   real  (kind=4), dimension( :, :, : ), allocatable, save :: TA_CAN
   real  (kind=4), dimension( :, :, : ), allocatable, save :: WS_CAN
   real  (kind=4), dimension( :, :, : ), allocatable, save :: QV_CAN
   real  (kind=4), dimension( :, :, : ), allocatable, save :: PRES_CAN       ! pressure (Pa)
   real  (kind=4), dimension( :, :, : ), allocatable, save :: DENS_CAN       ! mass density (Kg/m^3)
! gas-phase arrays
   real (kind=8), dimension( :, :, :, : ), allocatable, save :: KHETERO_CAN ! aerosols heterogeneous rx rates
   real (kind=4), dimension( :, :, :, : ), allocatable, save :: CONC_CAN   ! concentrations (including gas and aerosols)
   real (kind=4), dimension( :, :, :, : ), allocatable, save :: CONC_MOD   ! concentrations (including gas and aerosols)
! gas-phase tendencies
   real  (kind=4), dimension( :, :, : ), allocatable, save :: o3_new_can,  o3_old_can,  o3_tend_can
   real  (kind=4), dimension( :, :, : ), allocatable, save :: no2_new_can, no2_old_can, no2_tend_can
! gas-phase conc. 2m diagnostics
   real  (kind=4), dimension( :, :, : ), allocatable, save :: CONC_2M

   integer(kind=4), dimension(:,:), allocatable :: ka, kl


   public ::  ka, kl, dxdy, &
              kmod, zmid, zmom, z2, sigmom, sigmid2, kcan3, zcan3, ta_can3, qv_can3, ws_can3, &
              pres_can3, dens_can3, &
              ta3, qv3, ws3, pres3, dens3, klower_can, zmom_can, zmid_can, sigmom_can, sigmid_can, &
              ZH_CAN, ZF_CAN, &
              TA_CAN, QV_CAN, WS_CAN, PRES_CAN, DENS_CAN, &
              KHETERO_CAN, CONC_CAN, CONC_MOD, CONC_2M, &
              o3_new_can, o3_old_can, o3_tend_can, &
              no2_new_can, no2_old_can, no2_tend_can, &
              init_can_levs, get_can_levs

   contains

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


   subroutine init_can_levs(CONC, JDATE, JTIME)

   USE RUNTIME_VARS, only: LOGDEV
   USE GRID_CONF               ! horizontal & vertical domain specifications
   USE UTILIO_DEFN             ! IO routines & utilties

   use can_mask   !FRT_mask
   USE CGRID_SPCS                ! CGRID mechanism species, NSPCSD number of CGRID species
   USE RXNS_DATA, ONLY : NPHOTAB, NHETERO, NUMB_MECH_SPC, CGRID_INDEX
! NHETERO = Number of unique heteorogenous rate constants aerosols
! NSPCSD = n_gc_spcd + n_ae_spc + n_nr_spc + n_tr_spc

   IMPLICIT NONE

! *** CONC is concentration field (including gas and aerosol variables)
   REAL, POINTER :: CONC( :,:,:,: )              !  concentrations volume mixing ratio ppmv

!...Arguments:

   INTEGER, INTENT( IN ) :: JDATE         ! current Julian date (YYYYDDD)
   INTEGER, INTENT( IN ) :: JTIME         ! current time (HHMMSS)

!...local variables

   allocate (                                         &
             kmod      (NCOLS, NROWS, NLAYS),         &
! gas-phase array
             CONC_MOD (NCOLS, NROWS, NLAYS, NSPCSD), &
             CONC_CAN (NCOLS, NROWS, NLAYT, NSPCSD), &
             CONC_2M  (NCOLS, NROWS,        NSPCSD), &
    KHETERO_CAN(NHETERO,NCOLS, NROWS, NLAYT), &
                                  zmid     (NLAYS)  , &
                                  zmom     (NLAYS)  , & ! Same as zfull !
                                  sigmom   (NLAYS)  , &
                                  z2       (NLAYS+1), &
                                  sigmid2  (NLAYS+1), &
             kcan3     (NCOLS, NROWS, NLAYC),         &
                                  zcan3    (NLAYC)  , &
             ta_can3   (NLAYT),   ta3      (NLAYS)  , &
             qv_can3   (NLAYT),   qv3      (NLAYS)  , &
             ws_can3   (NLAYT),   ws3      (NLAYS)  , &
             pres_can3 (NLAYT),   pres3    (NLAYS)  , &
             dens_can3 (NLAYT),   dens3    (NLAYS)  , &
             klower_can(NLAYC)                      , &
             ka        (NCOLS, NROWS)               , &
             kl        (NCOLS, NROWS)               , &
             dxdy      (NCOLS, NROWS)               , &
             zmom_can  (NCOLS, NROWS, NLAYT+1)      , &
             zmid_can  (NCOLS, NROWS, NLAYT)        , &
             sigmom_can(NCOLS, NROWS, NLAYT)        , &
             sigmid_can(NCOLS, NROWS, NLAYT)        , &
! met3d arrays
             ZH_CAN    (NCOLS, NROWS, NLAYT)        , &
             ZF_CAN    (NCOLS, NROWS, NLAYT+1)      , &
             TA_CAN    (NCOLS, NROWS, NLAYT)        , &
             QV_CAN    (NCOLS, NROWS, NLAYT)        , &
             WS_CAN    (NCOLS, NROWS, NLAYT)        , &
             PRES_CAN  (NCOLS, NROWS, NLAYT)        , &
             DENS_CAN  (NCOLS, NROWS, NLAYT)        , &
! gas-phase tendencies
             o3_new_can (NCOLS, NROWS, NLAYT)       , &
             o3_old_can (NCOLS, NROWS, NLAYT)       , &
             o3_tend_can(NCOLS, NROWS, NLAYT)       , &
             no2_new_can (NCOLS, NROWS, NLAYT)      , &
             no2_old_can (NCOLS, NROWS, NLAYT)      , &
             no2_tend_can(NCOLS, NROWS, NLAYT)      )


   ka (:,:) = 0
   kl (:,:) = 0
   kmod (:,:,:) = 0
   kcan3(:,:,:) = 0
   kcan_top= 0

   hcan     = 0.
   zmid (:) = 0.
   zmom (:) = 0.
   sigmom (:) = 0.
   z2   (:) = 0.
   sigmid2(:) = 0.
   zcan3(:) = 0.
   ta_can3(:) = 0.
   qv_can3(:) = 0.
   ws_can3(:) = 0.
   pres_can3(:) = 0.
   dens_can3(:) = 0.
   ta3    (:) = 0.
   qv3    (:) = 0.
   ws3    (:) = 0.
   pres3  (:) = 0.
   dens3  (:) = 0.
   dxdy (:,:) = 0.
   zmom_can (:,:,:) = 0.
   zmid_can (:,:,:) = 0.
   sigmom_can(:,:,:) = 0.
   sigmid_can(:,:,:) = 0.
! met3d arrays
   ZH_CAN   (:,:,:) = 0.
   ZF_CAN   (:,:,:) = 0.
   TA_CAN   (:,:,:) = 0.
   QV_CAN   (:,:,:) = 0.
   PRES_CAN (:,:,:) = 0.
   DENS_CAN (:,:,:) = 0.
   WS_CAN   (:,:,:) = 0.
! gas-phase arrays
   KHETERO_CAN(:,:,:,:) = 0.0D0

! Initialize FIRSTIME only!
   CONC_MOD (:,:,:,:) = CONC (:,:,:,:) ! FIRSTIME

   CONC_CAN(:,:,NLAYC+1:NLAYT,:) = CONC(:,:,1:NLAYS,:) ! FIRSTIME
   CONC_CAN(:,:,3            ,:) = CONC(:,:,1,      :)       ! FIRSTIME
   CONC_CAN(:,:,2            ,:) = CONC(:,:,1,      :)       ! FIRSTIME
   CONC_CAN(:,:,1            ,:) = CONC(:,:,1,      :)       ! FIRSTIME

   CONC_2M (:,:,              :) = CONC(:,:,1,      :)       ! FIRSTIME
! gas-phase tendencies
   o3_old_can (:,:,:) = 0.
   o3_new_can (:,:,:) = 0.
   o3_tend_can(:,:,:) = 0.
   no2_old_can (:,:,:) = 0.
   no2_new_can (:,:,:) = 0.
   no2_tend_can(:,:,:) = 0.

   end subroutine init_can_levs

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

   subroutine get_can_levs(CONC, JDATE, JTIME, MDATE, MTIME)

   USE RUNTIME_VARS, only: LOGDEV
   USE GRID_CONF                               ! horizontal & vertical domain
! NB. TA alsos assigned in HRDATA
!   USE PHOT_MET_DATA, ONLY : ZFULL, ZM, TA     ! Met and Grid data
   USE PHOT_MET_DATA        ! Met and Grid data
   USE ASX_DATA_MOD, ONLY : MET_DATA, GRID_DATA ! Uses met data: Q2, TEMP2 grid data: area
   USE UTILIO_DEFN

   use can_mask
   USE RXNS_DATA, ONLY : NPHOTAB
   use CGRID_SPCS              ! CGRID mechanism species, NSPCSD number of CGRID species
   USE RXNS_DATA, ONLY : NPHOTAB, NHETERO, NUMB_MECH_SPC, CGRID_INDEX
! NHETERO = Number of unique heteorogenous rate constants aerosols
! NSPCSD = n_gc_spcd + n_ae_spc + n_nr_spc + n_tr_spc

   IMPLICIT NONE

! *** CONC is concentration field (including gas and aerosol variables)
   REAL, POINTER :: CONC( :,:,:,: )              !  concentrations volume mixing ratio ppmv

! Includes:
   INCLUDE SUBST_CONST     ! physical and mathematical constants

!...Arguments:

   INTEGER, INTENT( IN ) :: MDATE         ! "centered" Julian date (YYYYDDD)
   INTEGER, INTENT( IN ) :: MTIME         ! "centered" time (HHMMSS)
   INTEGER, INTENT( IN ) :: JDATE         ! current Julian date (YYYYDDD)
   INTEGER, INTENT( IN ) :: JTIME         ! current time (HHMMSS)

!...local variables

   INTEGER          :: ALLOCSTAT

   INTEGER          :: COL, ROW

   INTEGER          :: LEV, L

   logical(kind=4) :: flag_error
   integer(kind=4) :: k, kk, kc, k2, II, npass
   real   (kind=4) :: tmp

! del:  Minimum allowable distance between a resolved model layer and a canopy
! layer
!       (fraction of canopy layer height)
   real(kind=4),    parameter :: del = 0.2
   real(kind=4)    :: zm2, zr, td, hd, ddel

!  Assign the fractional heights of the canopy layers (fraction of canopy
!  height)
   real(kind=4), dimension(3), parameter   :: can_frac = (/1.0, 0.5, 0.2/)

   logical(kind=4)                         :: local_dbg

   local_dbg = (.false.)

   kmod (:,:,:) = -999
   kcan3(:,:,:) = -999

! Initialize times after FIRSTIME
! Here initialize all canopy & non-canopy columns
! Canopy colums over FRT_MASK get overwritten below

!... First, carry over original model values for the matching layers
   TA_CAN   (:,:,NLAYC+1:NLAYT)   = TA  (:,:,1:NLAYS)          ! K

!... Initialize the canopy layers with 1hy layer values
   TA_CAN(:,:,3) = TA( :,:,1)
   TA_CAN(:,:,2) = TA( :,:,1)
   TA_CAN(:,:,1) = TA( :,:,1)

!... First, carry over original model values for the matching layers
   QV_CAN   (:,:,NLAYC+1:NLAYT)   = Met_Data%QV(:,:,1:NLAYS)   !

!... Initialize the canopy layers with 1hy layer values
   QV_CAN(:,:,3) = Met_Data%QV( :,:,1)
   QV_CAN(:,:,2) = Met_Data%QV( :,:,1)
   QV_CAN(:,:,1) = Met_Data%QV( :,:,1)

!... First, carry over original model values for the matching layers
   PRES_CAN (:,:,NLAYC+1:NLAYT)   = Met_Data%PRES(:,:,1:NLAYS) ! Pa

!... Initialize the canopy layers with 1hy layer values
   PRES_CAN(:,:,3) = Met_Data%PRES( :,:,1)
   PRES_CAN(:,:,2) = Met_Data%PRES( :,:,1)
   PRES_CAN(:,:,1) = Met_Data%PRES( :,:,1)

!... First, carry over original model values for the matching layers
   DENS_CAN (:,:,NLAYC+1:NLAYT)   = Met_Data%DENS(:,:,1:NLAYS) ! kg m-3

!... Initialize the canopy layers with 1hy layer values
   DENS_CAN(:,:,3) = Met_Data%DENS( :,:,1)
   DENS_CAN(:,:,2) = Met_Data%DENS( :,:,1)
   DENS_CAN(:,:,1) = Met_Data%DENS( :,:,1)

! Initialize times after FIRSTIME
   CONC_MOD (:,:,:,:) = CONC (:,:,:,:)

!... First, carry over original model values for the matching layers
   CONC_CAN(:,:,NLAYC+1:NLAYT,:) = CONC(:,:,1:NLAYS,:)

!... Initialize the canopy layers with 1hy layer values
   CONC_CAN(:,:,3            ,:) = CONC(:,:,1,      :)
   CONC_CAN(:,:,2            ,:) = CONC(:,:,1,      :)
   CONC_CAN(:,:,1            ,:) = CONC(:,:,1,      :)

   CONC_2M (:,:,              :) = CONC(:,:,1,      :)

! zero gas-phase tendencies
   o3_old_can (:,:,:) = CONC_CAN(:,:,:,4) ! O3 = 4
   o3_new_can (:,:,:) = CONC_CAN(:,:,:,4) ! O3 = 4
   o3_tend_can(:,:,:) = 0.
   no2_old_can (:,:,:) = CONC_CAN(:,:,:,1) ! NO2 = 1
   no2_new_can (:,:,:) = CONC_CAN(:,:,:,1) ! NO2 = 1
   no2_tend_can(:,:,:) = 0.


   CALL GET_PHOT_MET( JDATE, JTIME, MDATE, MTIME )
! ===
! Out: ZFULL, ZM, TA
! ===

   DO ROW = 1, NROWS  !J-index
   DO COL = 1, NCOLS  !I-index

      dxdy(COL,ROW) = Grid_Data%AREA( COL,ROW ) !  dx*dy ~1.6E+8 m2

   ! Continuous forest canopy
   IF (FRT_mask(COL,ROW) > 0.) THEN

!      write(logdev,*) 'get_can_levs: ZFULL ZM TA = ', ZFULL(COL,ROW,1) , ZM(COL,ROW,1), TA(COL,ROW,1), COL, ROW

      hcan = Met_Data%FCH( COL,ROW )
!!! Extract the canopy height (FCH)

! Generate initial canopy levels, as altitude above sea level
!
      do kc = 1, NLAYC
         zcan3(kc) = hcan * can_frac(kc)  ! Paul's hc is our hcan
                                          ! Paul's zcan is our zcan3
!!! Set the initial values of the heights of the inserted canopy layers to hc, 0.5 hc, and 0.2 hc
!!!
!!! NB. zcan3(1) is hc, top of canopy
!!!     zcan3(2) is 0.5 * hc
!!!     zcan3(3) is 0.2 * hc (bottom canopy level)

!         write(logdev,*) 'get_can_levs: ZCAN = ', zcan3(kc), COL, ROW
      end do

! 1     = bottom (1st) model layer
! NLAYS = top model layer
      do k = 1, NLAYS       ! from bottom to top
         II = NLAYS + 1 - k  ! from top to bottom of resolved model layers
! zmid(1)     = ZM(NLAYS) is top    model  layer height
! zmid(NLAYS) = ZM(1)     is bottom model  layer height
        ! Paul's zt is our zmid
        zmid(II) = ZM(COL,ROW,k) ! mid layer height [m]
!!! Heights of the original model layers for the canopy columns are extracted to the zmid array.

        ! Paul's sigt2 is our sigmid2
        sigmid2(II) = Met_Data%PRES(COL,ROW,k)/ Met_Data%PRSFC(COL,ROW)

!         write(logdev,*) 'get_can_levs: ZMID = ', zmid(L), COL, ROW
      end do
      sigmid2(NLAYS+1) = 1.0

      do k = 1, NLAYS        ! from bottom to top
         II = NLAYS + 1 - k  ! from top to bottom of resolved model layers NLAYS+1 ???
!!! Paul's zmom is our zmom
! zmom(1)     = ZFULL(NLAYS) is top    model  layer height
! zmom(NLAYS) = ZFULL(1)     is bottom model  layer height
         zmom(II) = ZFULL(COL,ROW,k)
!! Heights of the original model layers for the canopy columns are extracted to the zmom array.

!        sigmom(II) = ?? PRES_FULL(COL,ROW, k) / Met_Data%PRSFC(COL,ROW) ??

         ! Create temperature & humidity array on reversed layer order for interpolation
         ta3(II) = TA(COL,ROW,k)               ! K
         qv3(II) = Met_Data%QV(COL,ROW,k)      !
         pres3(II) = Met_Data%PRES(COL,ROW, k) ! Pa
         dens3(II) = Met_Data%DENS(COL,ROW,k)  ! kg m-3
         !ws3(II) = WS(COL,ROW,k) ! wind speed ??
      end do

!!! Find the resolved model level which lies above the top of the forest canopy,
!!! in each canopy column.  Usually the canopy is within the NLAYS or NLAYS-1
!!! level of the original model structure.
!
! The model level above the tallest canopy in grid
      kcan_top = 2         ! initialize to 2nd top model layer
      do L = NLAYS, 3, -1  ! from bottom to top model layer (going up)
         ! Mid-layer height m, zmid
         if (zmid(L) > hcan) then ! Paul's zt is our zmid
            kcan_top = L - 1  ! level above the tallest canopy
            exit
         end if
      end do
!      write(logdev,*) 'get_can_levs: kcan_top = ', kcan_top

! Temporarily output kcan_top in ta_can array:
!     TA_CAN(COL,ROW, 1 ) = kcan_top ! middle canopy layer

!
! Adjust the canopy levels: we don't want canopy levels to get closer than del (0.2m)
! to the model levels to prevent possible differencing errors in the diffusion.
! If zcan3 > zmid but is too close to zmid, move zcan3 up by ddel.  If zcan3 < zmid
! but is too close to zmid, move zcan3 down by ddel.  The net result will be that the
! canopy levels are never closer than del from the original model levels.
      do k = kcan_top, NLAYS ! from model layer above the canopy to bottom of model layer
         do kc = 1, NLAYC   ! from top to bottom of canopy
            if (abs(zmid(k) - zcan3(kc)) < del) then
               ddel = max(0.0, del - abs(zcan3(kc) - zmid(k)))
               zcan3(kc) = zcan3(kc) + sign(ddel, zcan3(kc) - zmid(k))

!!! The reason why this section is necessary:  while it would be preferable for
!!! the canopy levels to stick with values of hc, 0.5 hc and 0.2 hc, somewhere in a
!!! large domain, there may be an overlap where one of these canopy levels is very
!!! close to or on top of an existing model level.  Which we dont want!
!!! What is done here, if the canopy levels come within "ddel" of an original model level,
!!! is to shift the canopy level in question a bit, to avoid overlaps.
            end if
         end do
      end do

!!! Starts the creation of the local array with the heights of the thermodynamic
!!! levels (layer midpoints) for the combined canopy + no canopy layers.
!!! Note that zmid_can at this point does not have these layers sorted in the
!!! correct order - the canopy layers have been tacked onto the bottom of the
!!! zmid_can array, but the values of zmid_can are not monotonically increasing with
!!! decreasing height index.
!
!  Set the initial values of the combined height array:
!
! Note that here, zmid_can is created, but the heights within each column have
! yet to be sorted to rearrange the layers in the correct order.
      do k = 1, NLAYS        ! from top to bottom model layers
         zmid_can(COL,ROW,k)    = zmid(k)
         ! Paul's zthrmcan is our zmid_can
!
      end do

! Add zcan3 additional thermo levels into zmid_can array for later sorting
      do kc = 1, NLAYC ! from top to bottom canopy layers
         zmid_can (COL,ROW,NLAYS+kc) = zcan3(kc)
      end do

!
!  Determine locations of canopy and resolved model levels within
!  the combined array for the canopy columns:
!
!!! This section sorts the zmid_can array to make sure that the new layers are
!!! all ordered so they monotonically increase with decreasing height.

! Top canopy layer height (NLAYS+1) is higher than the bottom model layer height (NLAYS)
      if (zmid_can(COL,ROW, NLAYS) < zmid_can (COL,ROW,NLAYS+1)) then
!
!  Non-trivial case:  the ancilliary and original array levels intermingle.
!  Sort the combined height array to get the right order of the the heights:
!
!  zmid_can is the height locations of the combined array, which needs to be  sorted:
!  since there are only NLAYC levels in the canopy, and both zcan3 and z
!  decrease monotonically, only NLAYC+1 passes are needed to sort the combined array:
         do npass = 1, NLAYC+1
            flag_error = .false.
            do k = NLAYT, 2, -1
! Top    canopy layer height (NLAYT-2) is larger than the bottom model  layer height (NLAYT-3 = NLAYS)
! Middle canopy layer height (NLAYT-1) is larger than the top    canopy layer height (NLAYT-2)
! Bottom canopy layer height (NLAYT)   is larger than the middle canopy layer height (NLAYT-1)
               if (zmid_can(COL,ROW, k) > zmid_can(COL,ROW, k-1)) then
!  The combined array heights are out of order, sort them:
                  tmp = zmid_can(COL,ROW, k-1)
                  zmid_can(COL,ROW, k-1) = zmid_can(COL,ROW,k)
                  zmid_can(COL,ROW, k)   = tmp
                  flag_error = .true.
               end if
            end do
         end do
         if (flag_error) then
            write(LOGDEV,*) 'NLAYC+1 passes insufficient to sort canopy array '
            write(LOGDEV,*) 'in can_levs_defn.F90.  Scream and die.'
! ABORT!
            return
         end if
      end if

!
!  Heights in zmid_can should now be monotonically decreasing.
! zmid_can(NLAYS-1) is 2nd model  layer <75.183m
! zmid_can(NLAYS)   is 1st model  layer <52.78m
! zmid_can(NLAYS+1) is top canopy layer <26.39m
! zmid_can(NLAYS+2) is 2nd canopy layer <22.73m
! zmid_can(NLAYS+3) is 1st canopy layer <10.56m

!  Next, identify the locations of the vertical levels in the combined
!  array relative to the resolved model array and canopy array
!
!!! Now that the heights in zmid_can are in the right order, we can use them to
!!! identify the values of kcan and kmod:  the vertical locations of the canopy and
!!! original model layers in the augmented canopy layer code.
      do kc = 1, NLAYC         ! from top to bottom canopy layers
         do kk = NLAYT, 1, -1  ! from bottom to top of combined canopy and resolved model levels
            if (zmid_can (COL,ROW, kk) == zcan3(kc)) then
               kcan3(COL,ROW,kc) = kk
               exit
            endif
         end do
      end do

! k=1        is top    model layer
! k=NLAYS    is bottom model layer
      do k = 1, NLAYS     ! from top to bottom model layers
         do kk = k, NLAYT ! from bottom to top of combined resolved plus canopy layers

! zmid_can(1)    = zmid(1)     is top    model  layer height
! ...
! zmid_can(NLAYS)= zmid(NLAYS) is bottom model  layer height
            if (zmid_can(COL,ROW, kk) == zmid(k)) then

! kmod(1)       is 1      , top model  layer
! kmod(NLAYS-1) is NLAYS-1, 2nd model  layer
! kmod(NLAYS)   is          top canopy layer (modified after monotonic adj.)
               kmod(COL,ROW,k) = kk
               exit
            endif
         end do
      end do

      if (local_dbg) then
      do kc = 1, NLAYC
         if (kcan3(COL,ROW,kc) < 1) then
            write(LOGDEV,*) 'get_can_levs: kcan undefined: ', kc, kcan3(COL,ROW,kc)
            !ABORT
            return
         end if
      end do
      do k = 1, NLAYS
         if (kmod(COL,ROW,k) < 1) then
            write(LOGDEV,*) 'get_can_levs: kmod undefined: ',k, kmod(COL,ROW,k)
            !ABORT
            return
         end if
      end do
      end if


!  Create the corresponding momentum height array
!
!  The original methodology adopted made use of the at2m array and the  thermodynamic heights determined above.
!  However, this methodology resulted in momentum levels which did not match the original model levels
!  above the region modified for canopy layers.  Here, the thermodynamic layers will be used to
!  (1) Determine whether the original model and canopy thermodynamic layers coincide, and if so,
!  (2) Use the existing model layer values for the momentum layers, while if not,
!  (3) Assign the new momentum layers as being 1/2 way between the canopy layers
! Note that these changes only exist inside the chemistry part of GEM-MACH and do not affect the model physics
!!!
!!! Create the momentum height (layer interface) array.  The original momentum layers are used above the canopy height.
!!! Below the canopy height, the "momentum"layers are assumed to be ½ way between the thermodynamiclayers.

! Default case:  all added canopy thermodynamic layers are below the lowest resolved model thermodynamic layer
! kcan_top is either 2nd or 3rd (63 or 62) resolved model layer
      do k = 1, kcan_top - 1 ! from top model layer to model layer above the canopy
!          zmom(1) is top    model  layer height
! zmom(kcan_top-1) is model layer above the canopy < 234.061m
         zmom_can(COL,ROW,k) = zmom(k) ! full layer height [m]
      end do

! Temporarily output
!     TA_CAN(COL,ROW, 2 ) = zmom_can(COL,ROW,kcan_top)   ! model layer above canopy

      ka(COL,ROW) = NLAYS
      inner0: do k = kcan_top, NLAYS-1  ! from resolved model layer above the canopy to top model layer
!  Starting from the top, scan down through the original and combined mid layer heights, to see when
!  they first deviate from each other
         !Paul's zthrmcan is our zmid_can
         !Paul's zt is our zmid
         if (zmid_can(COL,ROW,k) == zmid(k) .and. zmid_can(COL,ROW,k+1) == zmid(k+1)) then
            ! Paul's zmom is our zmom
            ! Paul's zmomcan(nkt+1) is our zmom_can
            zmom_can(COL,ROW,k) = zmom(k)  ! full layer height [m]
         else
            ka(COL,ROW) = k
            exit inner0
         end if
      end do  inner0

! Temporarily output
! ka is 63 or 64 (so NLAYS-1 or NLAYS)
!     TA_CAN(COL,ROW, 3 ) = ka(COL,ROW)

! ka  is the lower-most layer for which the combined layer zmom_can = zmom resolved model layer
      ! Paul's zmom is our zmom
      ! Paul's zmomcan is our zmom_can
      zmom_can(COL,ROW,ka(COL,ROW)) = zmom(ka(COL,ROW))
      do k = ka(COL,ROW)+1, NLAYT  ! from ka to bottom combined canopy and resolved layers
         zmom_can(COL,ROW,k) = (zmid_can(COL,ROW,k-1) + zmid_can(COL,ROW,k)) * 0.5
      end do
! Oct31:      zmom_can(COL,ROW, NLAYT + 1) = 0.

! Temporarily output
!     TA_CAN(COL,ROW, 1 ) = zmom_can(COL,ROW,NLAYS)   !1st model  layer <61.66m
!     TA_CAN(COL,ROW, 3 ) = zmom_can(COL,ROW,NLAYS+3) !1st canopy layer <16.51m

!########################################################################

!  create original model arrays of z which include the surface, to allow interpolation:
      !Paul's sigtcan is our sigmid_can
      do k = 1, NLAYS        ! from top to bottom of resolved model layers

! zmid(1)     is top    model  layer height
! zmid(NLAYS) is bottom model  layer height
         z2(k)    = zmid(k)

!  Fill in the thermodynamic sigma levels (Pre-existing levels first):
! kmod(1)     is 1  top    (last) model layer
! kmod(NLAYS) is 64 bottom (1st)  model layer
         kk = kmod(COL,ROW,k)
         sigmid_can(COL,ROW, kk) = sigmid2(k)

      end do
      klower_can(:) = -999
      z2(NLAYS+1) = 0.0

!
!  fill in the remaining sigma levels by interpolating in z:
      do kc = 1, NLAYC   ! from top to bottom canopy layers
         do k2 = kcan_top, NLAYS+1  ! from resolved model layer above the canopy top top model layer
            if (zcan3(kc) > z2(k2) .and. zcan3(kc) <= z2(k2-1)) then

! Interpolate in sigma
               sigmid_can(COL,ROW, kcan3(COL,ROW,kc)) = sigmid2(k2-1)  +     &
                                 (sigmid2(k2) - sigmid2(k2-1)) /  &
                                   (   z2(k2) -    z2(k2-1)) *        &
                                   (zcan3(kc) -    z2(k2-1))

! Store grid locations for use in later interpolations
               klower_can(kc) = k2
            end if
         end do
!
!
         if (klower_can(kc) < 1) then
            write(LOGDEV,*) 'get_can_levs:  klower_can is unassigned at col, row, kc: ', COL, ROW, kc
            write(LOGDEV,*) 'get_can_levs:  zcan3(kc): ',zcan3(kc)
            do kk = kcan_top, NLAYS+1
               write(LOGDEV,*) 'get_can_levs: kk z2(kk) which should bracket the above zcan3: ',kk, z2(kk)
            end do
            do kk = 1, NLAYS+1
               write(LOGDEV,*) 'get_can_levs:  kk z2(kk) full set of z2 values: ', kk, z2(kk)
            end do
            do kk = 1, NLAYC
               write(LOGDEV,*) 'get_can_levs:  kc zcan3(kc) hcan fr(kc) for full set of zcan3 values: ',kk, zcan3(kk), hcan, can_frac(kk)
            end do
            return
         end if
      end do

! Temporarily output
! klower_can(1) is 64 or 65
! klower_can(2) is 65 except for individual grid points near West coast
! klower_can(3) is 65 uniformly
!     TA_CAN(COL,ROW,1) = klower_can(1)   !JO3O1D

!
!
   if (local_dbg) then
!  Check on klower_can for NaN or out of bounds:
   do kc = 1, NLAYC
      if ((klower_can(kc) /= klower_can(kc)) .or. &
          (klower_can(kc) <= 0)              .or. &
          (klower_can(kc) > NLAYS+ 1) ) then
         write(LOGDEV,*) 'get_can_levs: klower_can after creation NaN or <=0 or >NLAYS+1 : ', &
                       kc, klower_can(kk)
         return
      end if
   end do
   end if
!
!  Create sigma coordinate  momentum levels:
!
!  As above, the existing momentum levels and the canopy values are used to create SIGM levels
!
!  (1) Determine whether the original model and canopy thermodynamic layers coincide, and if so,
!  (2) Use the existing model layer values for the momentum layers, while if not,
!  (3) Assign the new momentum layers as being 1/2 way between the canopy layers
! Note that these changes only exist inside the chemistry part of GEM-MACH and do not affect the
! model physics

! To do: calculate sigmom() using pres_full ???
! In the meantime, take an average of sigmid2 values
     do k = 1, NLAYS   ! from top to bottom of resolved model layers
         ! sigmid2(NLAYS+1) = 1.
         sigmom(k) = (sigmid2(k) + sigmid2(k+1)) * 0.5
     end do


!      Default case:  all added canopy half layers are
!      below the lowest resolved model half layer
      ka(COL,ROW) = NLAYS
      inner2:   do k = 1, NLAYS-1
         if (sigmid_can(COL,ROW, k) == sigmid2(k) .and. sigmid_can(COL,ROW, k+1) == sigmid2(k+1) ) then
            sigmom_can(COL,ROW, k) = sigmom(k)
         else
            ka(COL,ROW) = k
            exit inner2
         end if
      end do inner2
! ka  is the last layer for which sigmom_can= sigmid2(k)
      sigmom_can(COL,ROW, ka(COL,ROW)) = sigmid2(ka(COL,ROW))
      do k = ka(COL,ROW)+1, NLAYT
         sigmom_can(COL,ROW, k) = (sigmid_can(COL,ROW, k-1) + sigmid_can(COL,ROW, k)) * 0.5
      end do

!
!  Next, do a sort of all of the variables in the original METV3D array into canopy.  Note that
!  the declaration of the met arrays for the new canopy subdomain has occurred earlie in the code.
!  Three-D variables are a bit more complicated, in that one must make decisions regarding
!  the values of the met variables in the canopy region.
!  The code which follows is based on chm_load_metvar.ftn90
!
!  First, carry over original model values for the matching layers
   do k = 1, NLAYS        ! from bottom to top of resolved model layers
      ! kmod(1)     is 1  top model  layer
      ! kmod(NLAYS) is 65 top canopy layer (modified after mono adj.)
      kk = kmod(COL,ROW,k)
      ta_can3  (kk) = ta3(k)   !          TA  (COL,ROW, k) ! temperature [K]
      qv_can3  (kk) = qv3(k)   ! Met_Data%QV  (COL,ROW, k) ! N/A ?? QV(COL,ROW, k) ! spec. humidity
      pres_can3(kk) = pres3(k) ! Met_Data%PRES(COL,ROW, k) ! Pa
      dens_can3(kk) = dens3(k) ! Met_Data%DENS(COL,ROW, k) ! kg m-3
   end do

! Temporarily output
!     TA_CAN(COL,ROW,3) = ta_can3(NLAYS)     !COSZENS

!----------------------------------------------------------------------------
!  Canopy region:  next, go through each variable to work out canopy values.
!
!  (1) Do those variables for which special canopy formulae will NOT be used:
      do kc = 1, NLAYC       ! from top to bottom of canopy layers

!  Each of the following 2 variables have a screen height (2m) value in the 2D met arrays
!          Temperature:         TA, TEMP2
!          Specific humidity:   Q,  Q2

!   kcan3(1) = NLAYS + 1 = 65
!   kcan3(2) = NLAYS + 2 = 66
!   kcan3(3) = NLAYS + 3 = 67
         kk = kcan3(COL,ROW,kc)
         if (klower_can(kc) <= NLAYS ) then
!  Level is above first resolved model level

            k2 = klower_can(kc)
            zm2 = (zcan3(kc) - z2(k2-1)) / (z2(k2) - z2(k2-1))

            td = ( ta3(k2)  - ta3(k2-1)) * zm2
            hd = ( qv3(k2)  - qv3(k2-1)) * zm2
            ta_can3(kk) = ta3(k2-1) + td
            qv_can3(kk) = qv3(k2-1) + hd

         else
!  Level is below first resolved model level

            if (zcan3(kc) - z2(NLAYS+1) >= 2.0) then
            !  Level is below first resolved model level but above screen height

               zm2 = (zcan3(kc) - z2(NLAYS+1) - 2.0) / (z2(NLAYS) - z2(NLAYS+1) - 2.0)

               td = (ta3(NLAYS)  - Met_Data%TEMP2( COL,ROW ) )  * zm2
               hd = (qv3(NLAYS)  - Met_Data%Q2   ( COL,ROW ) )  * zm2
               ta_can3(kk)  = Met_Data%TEMP2(COL,ROW) + td
               qv_can3(kk)  = Met_Data%Q2   (COL,ROW) + hd

            else
            ! Level in canopy is below screen height; assume constant values below screen height

               ta_can3(kk)  = Met_Data%TEMP2(COL,ROW) ! 2-m  temperature [K]
               qv_can3(kk)  = Met_Data%Q2   (COL,ROW) ! 2-m  spec. humidity
            end if

         end if

!  Evaluate the air density in canopy columns using values determined above
!
! NB. PRES  is air pressure on ZH (mid-layers)
!     PRESF is air pressure on ZF (interfaces)
!     PRSFC is surface air pressure psfc

! get pressure from sigma levels in Pa
         pres_can3(kk) = sigmid_can(COL,ROW, kk) * Met_Data%PRSFC ( COL,ROW ) ! Pa

! aqm_methods: dens: buffer(k) = stateIn % prl(c,r,l) / ( rdgas * stateIn % temp(c,r,l) )
         ! dens_can3(1)       is top model  layer
         ! ...
         ! dens_can3(NLAYS)   is 1hy model  layer
         ! dens_can3(NLAYS+1) is top canopy layer
         ! dens_can3(NLAYT)   is 1st canopy layer
         dens_can3(kk) = pres_can3(kk) / ( RDGAS * ta_can3(kk))  ! kg m-3

! Temporarily output k2 = klower_can(kc)
!        if (kc == 1) TA_CAN(COL,ROW,1) = ta3(k2) !JO3O1D

!  The following variables are assumed to have uniform values throughout the
!  lowest resolved model layer:
!
!    Cloud liquid water mass mixing ratio (QCPLUS)
!    Total cloud fraction (FTOT)
!    Stratospheric cloud fraction (FXP)
!    Convective cloud fraction (FDC)
!    Total liquid water flux (RNFLX)
!    Total solid water flux (SNOFLX)
!    Precipitation evaporation (FEVP)
!    Cloud to rain collection tendency (PPRO)
!  Search over the original model layers (k).  Note that the outer loop above this
!  one is over the canopy layers kc:  we are looking for the values to assign the
!  canopy layers in the combined canopy+resolved scale space.  For these variables,
!  the resolved scale values will be used, hence the aim is to determine the
!  resolved scale layer in which the canopy layer resides, and assign the
!  corresponding values to the locations of the canopy layers in the combined
!  canopy + resolved scale space (kk).


      end do ! kc = 1, NLAYC

   if (local_dbg) then
! Several checks for suspicious values:
      do kk = 1, NLAYT
         if ( ta_can3(kk) < 150.0) then
            write(LOGDEV,*) 'get_can_levs:  suspicious temperature detected in get_can_levs after creation (kk value): ',&
                        COL, ROW, kk, ta_can3(kk)
            do kc = 1, NLAYC
               write(LOGDEV,*) 'get_can_levs: value of zcan(kc) z2(NLAYS+1) and difference  at this value of ic for kk: ',&
                            kc,' are: ',zcan3(kc),z2(NLAYS+1), zcan3(kc)-z2(NLAYS+1)
            end do

            do k = 1, NLAYT
               write(LOGDEV,*) 'get_can_levs: value of zmid_can for = ',COL, ROW,' at k = ',k,' is: ',zmid_can(COL,ROW,k)
            end do

            do kc = 1, NLAYC
               write(LOGDEV,*) 'get_can_levs:  values of kcan zcan and original zcan for = ',COL, ROW,' at kc = ',kc,' are: ',&
                           kcan3(COL,ROW,kc), zcan3(kc), hcan * can_frac(kc)
            end do

            do k = 1,NLAYS
               write(LOGDEV,*) 'get_can_levs:  value of kmod and z for = ', COL, ROW,' at k = ',k,' are: ',kmod(COL,ROW,k), zmid(k)
            end do

            do kc = 1, NLAYC
               write(LOGDEV,*) 'get_can_levs: value of klower_can at this grid point for kc: ',kc,' is: ',klower_can(kc)
            end do

            return
         end if
      end do
   end if

!  (2) For the last few variables, the value at the lowest resolved model layer and typical profiles for that variable
!  within the canopy will be used to create the canopy values:
      do kc = 1, NLAYC
         kk = kcan3(COL,ROW,kc)
!  Ratio of lowest model level to canopy height:
!
         zr = (zmid(NLAYS) - z2(NLAYS+1)) / hcan
!
!  Horizontal wind and KT profiles are from Raupach, Quarterly Journal
!  of the Royal Meteorological Society, vol 115, pp 609-632, 1989, examples
!  from page 626, equations (48) through (51).
!
!  Wind speed (equation 51), assumed to scale similarly in each horizontal dimension:
!
!   U(z) = ustar/karman * ln((z - d) / z0), where
!   k = 0.4
!   d = 0.75 hc
!   z0 = 0.07530 hc
! The next few lines calculate the average value of u(z), v(z), Raupach's eqn 51,
! at the first resolved level model height

! ...  wndr
! ...  uspr
         !ws_can(kk) = wndr * uspr

      end do !kc = 1, NLAYC

      do k = 1, NLAYT       ! from top to bottom of combined layers
         II = NLAYT + 1 - k  ! from bottom to top of combined layer

         ! Flip back meteo arrays on combined layers in same layer order as original model layer
         ! NLAYT is top model layer         <= 1
         ! ...
         ! (4) is 1st (bottom) model layer   <= NLAYS
         ! (3) is 3rd (top) canopy layer     <= NLAYT-2
         ! (2) is 2nd canopy layer           <= NLAYT-1
         ! (1) is 1st (bottom) canopy layer  <= NLAYT
         ZH_CAN  (COL,ROW,II) = zmid_can(COL,ROW, k)
         TA_CAN  (COL,ROW,II) = ta_can3  (k)
         QV_CAN  (COL,ROW,II) = qv_can3  (k)
         PRES_CAN(COL,ROW,II) = pres_can3(k)
         DENS_CAN(COL,ROW,II) = dens_can3(k)

      end do

      do k = 1, NLAYT        ! from top to bottom of combined layers
         II = NLAYT + 1 - k  ! from bottom to top of combined layer
         ZF_CAN  (COL,ROW,II) = zmom_can(COL,ROW, k)
      end do

   END IF ! Continuous forest canopy: FRT_MASK == 1.

! ... have not finished Paul's code ...


   END DO  !COL = 1, NCOLS  !I-index
   END DO  !ROW = 1, NROWS  !J-index

   end subroutine get_can_levs

   end module can_levs_defn
