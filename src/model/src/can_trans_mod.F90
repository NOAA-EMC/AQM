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

module can_trans_mod

!**********************************************************************
!  Function: Transfers resolved scale model layers mass to canopy layers
!     and vice versa, while conserving mass, and allocates arrays.
!
!  Preconditions: GRID_CONF defines the vertical canopy layer structure
!     CAN_MASK defines the canopy grid cells and CAN_LEVS_DEFN initializes
!     input meteorological and geophysical fields.
!     For the CB6R3 family of gas-phase chemical mechanisms performed by
!     either the Euler Backward Iterative (EBI) solver or the vectorized
!     Rosenbrock (ROS3) solver.
!     Activated with config namelist parameter do_aqm_canopy= .true.
!
!  Key Subroutines/Functions Called: None
!
!  Revision History: Created by I. Ivanova March 2024.
!     Based on Makar, P. A. et al. "The effects of forest canopy shading
!     and turbulence on boundary layer ozone." Nat. Commun. 8, 15243
!     doi: 10.1038/ncomms15243 (2017).
!**********************************************************************

   implicit none

   real(kind=4), dimension(:,:,:), allocatable, save :: massair_can, massair
   real(kind=4), dimension(:), allocatable :: mass_resolved, mass_canopy, mmr_canopy, mmr_resolved, vmr_resolved, &
                                              conc3, conc_can3, vmr_canopy
   integer(kind=4), dimension(:, :, :), allocatable, save    :: nfrct
   integer(kind=4), dimension(:, :, :, :), allocatable, save :: ifrct
   real(kind=4),    dimension(:, :, :, :), allocatable, save :: frctr2c, frctc2r


   public :: massair_can, massair, mass_resolved, mass_canopy, mmr_canopy, mmr_resolved, vmr_resolved, &
       conc3, conc_can3, vmr_canopy, &
       nfrct, ifrct, frctr2c, frctc2r, init_can_trans, canopy_transfer

   INTEGER, PRIVATE                :: LOGDEV                 ! unit number for the log file

   contains

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 subroutine init_can_trans( CONC, JDATE, JTIME)

!  Input/Output variables, original horizontal coordinate
!
!  Local variables:
!  massair_can(:,:, NLAYT)  :  mass of air in canopy layers (kg)
!  massair    (:,:, NLAYS)  :  mass of air in model layers (kg)
!                                        (gathered canopy + resolved scale columns)
!   nfrct  (NLAYT,   :,:)   :  Number of original model levels contributing to canopy level k
!   ifrct  (NLAYT, 2,:,:)   :  Index of the original model level contributing to canopy level k
!   frctr2c(NLAYT, 2,:,:)   :  Fractional contribution of the original model level to canopy level k
!   frctc2r(NLAYT, 2,:,:)   :  Fractional contribution of the canopy level to the original model level
!
!=============================================================================

   USE GRID_CONF               ! horizontal & vertical domain specifications
   USE UTILIO_DEFN

   use can_mask   !FRT_mask
   USE RXNS_DATA  !, ONLY : NPHOTAB, NHETERO, NUMB_MECH_SPC, CGRID_INDEX
   USE CGRID_SPCS        ! CGRID mechanism species, NSPCSD number of CGRID species
! NSPCSD = nspcsd = n_gc_spcd + n_ae_spc + n_nr_spc + n_tr_spc
! NPHOTAB:
! NHETERO: Number of unique heteorogenous rate constants aerosols
! NUMB_MECH_SPC:

   IMPLICIT NONE

!...Arguments:

! *** CONC is concentration field (including gas and aerosol variables)
   REAL, POINTER :: CONC( :,:,:,: )              !  concentrations volume mixing ratio ppmv

   INTEGER, INTENT( IN ) :: JDATE         ! current Julian date (YYYYDDD)
   INTEGER, INTENT( IN ) :: JTIME         ! current time (HHMMSS)

   LOGDEV   = INIT3()

   allocate ( massair_can(NCOLS, NROWS, NLAYT), &
              massair    (NCOLS, NROWS, NLAYS), &
              mass_canopy  (NLAYT), &
              mmr_canopy   (NLAYT), &
              vmr_canopy   (NLAYT), &
              mmr_resolved (NLAYS + 1), &
              vmr_resolved (NLAYS + 1), &
              mass_resolved(NLAYS), &
              conc3        (NLAYS), &
              conc_can3    (NLAYC), &
              nfrct  (NLAYT,    NCOLS, NROWS), &
              ifrct  (NLAYT, 2, NCOLS, NROWS), &
              frctr2c(NLAYT, 2, NCOLS, NROWS), &
              frctc2r(NLAYT, 2, NCOLS, NROWS) )

   massair_can(:,:,:) = 0.
   massair    (:,:,:) = 0.

   conc_can3(:)=0.
   conc3    (:)=0.
   mass_canopy(:) = 0.
   mmr_canopy (:) = 0.
   vmr_canopy (:) = 0.
   mmr_resolved(:) = 0.
   vmr_resolved(:) = 0.
   mass_resolved(:) = 0.

   nfrct  (:,  :,:) = 0
   ifrct  (:,:,:,:) = 0
   frctr2c(:,:,:,:) = 0.
   frctc2r(:,:,:,:) = 0.

 end subroutine init_can_trans

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

 subroutine canopy_transfer(CONC, JDATE, JTIME, FLAG)

! Arguments:
! Input variables
!-----------------------------------------------------------------------------
! Array dimensions:
!  NLAYC                              :  number of canopy levels
!  NLAYT = NLAYS + NLAYC              :  number of levels in gathered canopy + resolved scale columns
!  met???_CAN(:.:, NLAYT):            :  met 3d variables, gathered canopy + resolved scale columns
!  kmod(NLAYS)                        :  Vertical index location of original ungathered model layer in combined
!                                        canopy + resolved scale column
!  flag                               : 0 -> resolved_to_canopy
!                                       1 -> canopy_to_resolved
!
!  Input/Output variables, original horizontal coordinate
!  CONC_CAN(:,:, NLAYT, NSPCSD)     :  Chemical tracers concentrations ppmv combined canopy and resolved model layers
!  CONC_MOD(:,:, NLAYS, NSPCSD)     :  Chemical tracers concentrations ppmv on model levels (copy of CONC)
!  CONC    (:,:, NLAYS, NSPCSD)     :  Chemical tracers concentrations ppmv on model levels
!  CONC_2M (:,:,        NSPCSD)
!
!  Local variables:
!  massair_can(:,:, NLAYT)    :  mass of air in canopy layers (kg)
!  massair    (:,:, NLAYS)    :  mass of air in model layers (kg)
!                                        (gathered canopy + resolved scale columns)
!   nfrct  (NLAYT,  :,:)        :  Number of original model levels contributing to canopy level k
!   ifrct  (NLAYT,2,:,:)        :  Index of the original model level contributing to canopy level k
!   frctr2c(NLAYT,2,:,:)        :  Fractional contribution of the original model level to canopy level k
!   frctc2r(NLAYT,2,:,:)        :  Fractional contribution of the canopy level to the original model level
!
!=============================================================================

   USE GRID_CONF                     ! horizontal & vertical domain
   USE CGRID_SPCS                    ! CGRID mechanism species, NSPCSD number of CGRID species

! NB: TA also assigned in HRDATA
   USE PHOT_MET_DATA, ONLY : ZFULL, ZM          ! Met and Grid data
   USE ASX_DATA_MOD, ONLY : MET_DATA, GRID_DATA ! Uses met data: Q2, TEMP2
   USE RXNS_DATA   ! , ONLY : NUMB_MECH_SPC, CGRID_INDEX ! SPECIES_MOLWT

! hrinit: for ebi solver
!  USE HRDATA             ! FORWARD_CONV, REVERSE_CONV
! rbinit: for Rosenbrook solver
   USE RBDATA
   USE UTILIO_DEFN

   use can_mask
!     Initialized in can_mask (called by phot: call init_can_mask)
!       NLAYC = 3
!       NLAYT = NLAYS + NLAYC
!       FRT_MASK

   use can_levs_defn
!
!     Initilized in can_levs_defn (called by phot: call init_can_levs)
!
! 3D arrays dimensions:
!  KMOD      (:,:,NLAYS)
!  CONC_MOD (:,:,NLAYS, NSPCSD)  ppmv volume mixing ratio
!  CONC_2M  (:,:,       NSPCSD)
!  kcan3     (:,:,NLAYC)                    :  Vertical index location of canopy ungathered model layer in combined
!                                          canopy + resolved scale column
!  DXDY     (:,:)
!  ZMID_CAN (:,:,NLAYT)
!  ZMOM_CAN (:,:,NLAYT+1)
!
!  QV_CAN    "
!  TA_CAN
!  PRES_CAN  "
!  DENS_CAN  "
!
!  KHETERO_CAN(NHETERO,:,:)          => updated in HRDRIVER
!  CONC_CAN          (:,:,NLAYT)    => updated in HRDRIVER

   IMPLICIT NONE

!...Arguments:

! *** CONC is concentration field (including gas and aerosol variables)
   REAL, POINTER :: CONC( :,:,:,: )              !  concentrations volume mixing ratio ppmv

   INTEGER, INTENT( IN ) :: JDATE         ! current Julian date (YYYYDDD)
   INTEGER, INTENT( IN ) :: JTIME         ! current time (HHMMSS)
   INTEGER, INTENT( IN ) :: FLAG

!...local variables

   INTEGER          :: ALLOCSTAT

   INTEGER          :: COL, ROW, ISP, S

   INTEGER          :: LEV, L

   INTEGER          :: KOUNT

! Diagnostic height is the assumed height above ground of the sampling for observations
   real(kind=4),    parameter              :: diag_hgt = 2.0

!   REAL, PARAMETER :: DENS_CONV = ( 1.0E+03 * AVO / MWAIR ) * 1.0E-06  ! convert from kg/m**3 to #/cc
!   REAL, PARAMETER :: PPM_MCM3  = 1.0E-06  ! convert from ppm to molecules / cc mol_Spec/mol_Air = ppm * 1E-06

   real(kind=4) :: mmr_diag

   logical(kind=4) :: chm_error_l = .false.

   logical(kind=4) :: flag_error
   integer(kind=4) :: k, kk, kc, k2, II, npass
   real   (kind=4) :: tmp

   logical(kind=4)                         :: local_dbg
   local_dbg = .true.

   DO ROW = 1, NROWS  !J-index
   DO COL = 1, NCOLS  !I-index

   ! Continuous forest canopy
   IF (FRT_mask(COL,ROW) > 0.) THEN

! Put vars on combined layers in layer order as in Paul's code (GEM-MACH)
! 1      <=  NLAYT  is top model layer
! ...
! NLAYS   <= (4) is 1st (bottom) model layer
! NLAYT-2 <= (3) is 3rd (top) canopy layer
! NLAYT-1 <= (2) is 2nd canopy layer
! NLAYT   <= (1) is 1st (bottom) canopy layer

      do k = 1, NLAYT
         II = NLAYT + 1 - k
         pres_can3(II) = PRES_CAN(COL,ROW,k)
         dens_can3(II) = DENS_CAN(COL,ROW,k)  ! kg/m**3
      end do

! Calculate mass of air on combined levels
      !Paul's zmomcan is our zmom_can(NCOLS,NROWS, NLAYT + 1)
      ! zmom_can(:,:,1)       is top resolved layer
      ! zmom_can(:,:,NLAYS)   is 1hy resolved layer
      ! zmom_can(:,:,NLAYT)   is 1st canopy layer
      zmom_can(COL,ROW, NLAYT + 1) = 0.0
      do k = NLAYT, 1, -1
         ! Paul's massaircan is our massair_can
         massair_can(COL,ROW, k) = dens_can3(k) * Grid_Data%AREA (COL,ROW) * &
                              (zmom_can(COL,ROW, k) - zmom_can(COL,ROW, k + 1))
      end do

      do k = 1, NLAYS       ! from bottom to top
         II = NLAYS + 1 - k  ! from top to bottom of resolved model layers NLAYS+1 ???
!!! Paul's zmom is our zmom
! zmom(1)     = ZFULL(NLAYS) is top    model  layer height
! zmom(NLAYS) = ZFULL(1)     is bottom model  layer height
         zmom (II) = ZFULL(COL,ROW,k)
         dens3(II) = Met_Data%DENS(COL,ROW,k)  ! kg/m**3
!! Heights of the original model layers for the canopy columns are extracted to the zmom array.
      end do

!  Calculate mass of air in model levels
      !Paul's zmom is our zmom
      zmom(NLAYS + 1) = 0.0
      do k = NLAYS, 1, -1
         ! Paul's massairmod is our massair
         massair(COL,ROW, k) = dens3(k) * Grid_Data%AREA (COL,ROW) * &
                             (zmom(k) - zmom(k + 1))
      end do

!  Next, we need a set of arrays which track mass transfer from resolved to model layers;
!  how much of the original (aka "resolved") model layer mass goes into each canopy layer,
!  given the above level structure.  The three arrays are:
!    nfrct(k,  :,:) : the number of resolved model levels contributing to canopy level k
!    ifrct(k,n,:,:) : the index of the resolved model level contributing to canopy level k (n is at most 2)
!  frctr2c(k,n,:,:) : the fractional contribution of the resolved model level to canopy level k
!  frctc2r(k,n,:,:) : the fractional contribution of the canopy model level to the resolved model level
!
!  Check for coincident layers first:
!
      inner: do k = 1, NLAYS
!  If the following IF statement is true, then the canopy and resolved
!  model layer upper and lower boundaries coincide, and the entire resolved model
!  model layer contributes to the combined model layer (trivial case).
         if (zmom_can(COL,ROW, k) == zmom(k) .and. zmom_can(COL,ROW, k+1) == zmom(k+1)) then
            nfrct(k,    COL,ROW) = 1
            ifrct(k, 1, COL,ROW) = k
            frctr2c(k, 1, COL,ROW) = 1.0
            frctc2r(k, 1, COL,ROW) = 1.0
         else
            exit inner
         end if
      end do inner
!
!  "k" is the first layer where boundary levels do not match on output from the above loops.
! Determine fractions of original model layer structure contributing to canopy model layers.
      k2 = k
      do k = k2, NLAYT
         do kk = k2, NLAYS
! (1) Upper boundaries of combined and resolved model layers coincide,
! lower boundary of combined layer is within resolved layer, so canopy
! layer resides entirely within resolved layer, and shares an upper boundary
! with the resolved layer:
            if ((zmom_can(COL,ROW, k) == zmom(kk) .and. zmom_can(COL,ROW, k+1) > zmom(kk+1)) .or. &
! (2) Lower boundaries coincide, upper boundary of combined layer is within resolved layer,
! so canopy layer resides entirely within the resolved layer, and shares a lower boundary
! with the canopy layer.
                (zmom_can(COL,ROW, k+1) == zmom(kk+1) .and. zmom_can(COL,ROW, k) > zmom(kk)) .or. &
! (3) Both canopy layer boundaries exist inside a resolved layer, with no shared boundaries:
!  nfrct(NLAYS + 1) = 1
!  ifrct(NLAYS + 1) = 64
                (zmom_can(COL,ROW, k) < zmom(kk) .and. zmom_can(COL,ROW, k+1) >= zmom(kk+1))) then
               nfrct(k,    COL,ROW) = 1
               ifrct(k, 1, COL,ROW) = kk
               frctr2c(k, 1, COL,ROW) = (zmom_can(COL,ROW, k) - zmom_can(COL,ROW, k+1)) / (zmom(kk) - zmom(kk+1))
               frctc2r(k, 1, COL,ROW) = 1.0  ! canopy layer resides within resolved model layer
            end if
!  Resolved layer boundary splits a combined canopy layer:
!  This case arises if, due to the use of the momentum levels in the canopy column
!  sometimes being half-way between the thermodynamic levels, a resolved model
!  momentum layer falls within the canopy layer.  Since the resolved model layers are
!  defacto thicker than the canopy layers, this means that there can at most be two
!  resolved model layers contributing to the canopy layer (only case where nfrct = 2).
            if (zmom_can(COL,ROW, k+1) < zmom(kk) .and. zmom_can(COL,ROW, k) > zmom(kk)) then
               nfrct(k,    COL,ROW) = 2
               ifrct(k, 1, COL,ROW) = kk
               ifrct(k, 2, COL,ROW) = kk-1
!  Fraction of resolved model layer contributing to canopy layer:
               frctr2c(k, 1, COL,ROW) = (zmom(kk) - zmom_can(COL,ROW, k+1)) / (zmom(kk) - zmom(kk+1))
               frctr2c(k, 2, COL,ROW) = (zmom_can(COL,ROW, k) - zmom(kk)) / (zmom(kk-1) - zmom(kk))
!  Fraction of canopy layer contributing to resolved model layer:
               frctc2r(k, 1, COL,ROW) = (zmom(kk) - zmom_can(COL,ROW, k+1)) / (zmom_can(COL,ROW, k) - zmom_can(COL,ROW, k+1))
               frctc2r(k, 2, COL,ROW) = (zmom_can(COL,ROW, k) - zmom(kk)) / (zmom_can(COL,ROW, k) - zmom_can(COL,ROW, k+1))
            end if
         end do
      end do

!
!  massair_can thus contains the mass of air in the canopy layers in kg, while massair contains the
!  mass of air in the original model layers, at the canopy columns (COL,ROW)
!
   END IF ! Continuous forest canopy: FRT_MASK == 1.


   END DO  !COL = 1, NCOLS  !I-index
   END DO  !ROW = 1, NROWS  !J-index


!  return tracers to resolved scale model layers:

   if (flag == 1) then  ! "canopy_to_resolved"

!  At this point, the model mass is distributed over the combined layers,
!  and the tracer concentration arrays are both in the combined layer system.
!
   DO ROW = 1, NROWS  !J-index
   DO COL = 1, NCOLS  !I-index

   KOUNT = 0

   !  loop over canopy columns
   IF (FRT_mask(COL,ROW) > 0.) THEN

! CONC_MOD/CONC_CAN:
!   Assigned/Initilized in can_levs_defn FIRSTIME
!   Updated in hrdriver after gas-phase tendencies are applied

!...fetch all species (ozone = 4 no2 =1) and convert to kg kg-1 mass mixing ratio
      do ISP = 1, NUMB_MECH_SPC    ! NSPCSD
!     do ISP = 1, 1     ! NO2 = 1  O3 = 4

            S = CGRID_INDEX( ISP )

! Flip resolved layer arrays into a new array for use here
         do k = 1, NLAYS        ! from bottom to top
            II = NLAYS + 1 - k  ! from top to bottom of resolved model layers
            ! conc3(1)     is top model layer
            ! conc3(NLAYS) is 1st (bottom) model layer
            ! Paul's chem_tr is our conc3 = vmr_resolved
            ! conc3(II) = CONC(COL,ROW, k, S) ! ppm
! Oct9:     ! conc3(II) = CONC_MOD(COL,ROW, k, S) ! ppm
            ! Paul's chem_tr is our vmr_resolved =conc3
            vmr_resolved(II) = CONC_MOD(COL,ROW, k, S) ! ppm
! Feb10: use CONC instead of CONC_MOD
!           vmr_resolved(II) = CONC(COL,ROW, k, S) ! ppm
         end do

! Flip combined layer arrays into a new array for use here
         do k = 1, NLAYT        ! from top to bottom
            II = NLAYT + 1 - k  ! from bottom to top of resolved model layers
            ! Paul's trppm is our vmr_canopy (CONC_can)
            ! (NLAYS) is top model layer
            ! (1)     is 1hy model layer
            vmr_canopy(II) = CONC_CAN(COL,ROW, k, S) ! ppm
         end do

! (ii): Canopy shaded layers
      do kc = 1, NLAYC
         k = kcan3(COL,ROW, kc) ! kcan3(1,2,3) = 65,66,67
         ! Paul's tracers_can is our conc_can3 array
         conc_can3 (kc) = vmr_canopy(k)
      end do

!--------------
!hrinit.F: ...set scale factor for [ppm] -> [kg/kg]
!
! CONC to CHEM  Species conversion factor
!         FORWARD_CONV( N ) = 1.0E-3 * MWAIR / SPECIES_MOLWT( N )  ! ug kg-1 to ppm
!
! CHEM to CONC Species conversion factor
!         REVERSE_CONV( N ) = 1.0E+3 / MWAIR * SPECIES_MOLWT( N )  ! ppm    to ug kg-1
!--------------

!  (1) We start off by converting these volume mixing ratio ppm to mass in ug:

         do k = 1, NLAYS
            ! kmod(1)     is 1  top model  layer
            ! kmod(NLAYS) is 65 top canopy layer (modified after mono adj.)
            kk = kmod(COL,ROW, k)

! ...fetch gas volume mix. ratios [ppm] and convert to mass mixing ratios [ug kg-1]
            ! Paul's conc is our mmr_canopy
            !mmr_canopy(kk) = REVERSE_CONV(isp) * conc3(k)      ! ug kg-1
            mmr_canopy(kk) = REVERSE_CONV(isp) * vmr_resolved(k)
         end do

         do k = 1, NLAYC
            ! kcan3(k=1,2,3) = 65,66,67
            kc = kcan3(COL,ROW, k)

! ...fetch gas volume mix. ratios [ppm] and convert to mass mixing ratios [ug kg-1]
            mmr_canopy(kc) = REVERSE_CONV(isp) * conc_can3(k)  ! ug kg-1
         end do

! Temporary diagnostic output
!        CONC_MOD(COL,ROW, NLAYS  , S) = mmr_canopy(NLAYS  ) ! 1st model  layer  "COSZENS"  ~85 ug kg-1

! (2) Array "mass_canopy" now holds the mass of the tracer in each of the combined levels.
! This mass must be added back to the resolved levels:
         ! Paul's masscan is our mass_canopy
         ! Paul's mass_resolved is our mass_resolved
         mass_resolved(:) = 0.
         do k = 1, NLAYT
            mass_canopy(k) = mmr_canopy(k) * massair_can(COL,ROW, k)  ! ug
            do kk = 1, nfrct(k, COL,ROW)
               kc = ifrct(k, kk, COL,ROW)
               mass_resolved(kc) = mass_resolved(kc) + mass_canopy(k) * frctc2r(k,kk,COL,ROW)  ! ug
            end do
         end do

! Temporary diagnostic output
!           CONC_MOD(COL,ROW, NLAYS, S) = mass_resolved(NLAYS)   ! 'COSZENS'  E+11
! Print
         IF(.FALSE.) THEN
         IF ( KOUNT < 3 )  THEN
            do k = 1, NLAYS
               if (k > 62) &
                 print*,'CAN_TRANS C2R: ISP ', S, k, &
                 mass_resolved(k), mass_canopy(k),   &
              massair(COL,ROW, k), massair_can(COL,ROW, k)
            end do
            print*,'CAN_TRANS C2R: ISP ', S, k, &
            mass_canopy(NLAYS+1), mass_canopy(NLAYS+2), mass_canopy(NLAYT), &
            massair_can(COL,ROW, NLAYS+1), massair_can(COL,ROW, NLAYS+2), massair_can(COL,ROW, NLAYT)
         END IF ! KOUNT
         END IF ! .FALSE.
! End Print

!
!  Check:  total mass in the column should be the same
         if (local_dbg) then
             call canopy_mass_check(mass_canopy, mass_resolved, COL,ROW, flag)
             if (chm_error_l) return
         end if
!
!  (3) The masses in ug need to be converted back to ug/kg and then back to v.m.r [ppm]
         do k = 1, NLAYS
!
            ! Paul's massairmod is our massair
            ! Paul's mass_resolved is our mass_resolved
            mmr_resolved(k) = mass_resolved(k) / massair(COL,ROW, k)  ! ug kg-1

! (3a) Convert back m.m.r. [ug kg-1] to volume mix. ratios [ppm]
            ! NB. This is CONC_MOD to be used in gas-phase hrdriver call on canopy columns
            ! Paul's chem_tr is our conc3 = vmr_resolved
            vmr_resolved(k)            = FORWARD_CONV(isp) *  mmr_resolved(k)    ! ppm

         end do

         do k = 1, NLAYS       ! from bottom to top
            II = NLAYS + 1 - k  ! from top to bottom of resolved model layers
            ! zmid(1)     = ZM(NLAYS) is top    model  layer height
            ! zmid(NLAYS) = ZM(1)     is bottom model  layer height
            ! Paul's zt (or ZPLUS) is our zmid
            zmid(II) = ZM(COL,ROW,k) ! mid layer height [m]
!!! Heights of the original model layers for the canopy columns are extracted to the zmid array.
!         write(logdev,*) 'canopy_transfer: ZMID = ', zmid(L), COL, ROW
         end do

!
!  (4) Evaluate the diagnostic level concentration
!  Find the bounding layers above and below the diagnostic height:
!  kk'th layer is the layer above the inlet height
            kk = NLAYT
            do k = NLAYT, NLAYT-8, -1
               ! Paul's zt (MV3D_ZPLUS) is our zmid
               if (diag_hgt <= zmid(k-1) .and. &
                  diag_hgt > zmid(k)) then
                  kk = k - 1
               end if
            end do
!  If the diagnostic height is less than the lowest level, then use that level
!  for the concentration.
            if (kk == NLAYT) then
               mmr_diag =  mmr_canopy(NLAYT) ! ug kg-1
               vmr_resolved      (NLAYS + 1)      = FORWARD_CONV(isp) * mmr_canopy(NLAYT) ! ppm
            else
! Diagnostic height 2m is always above the lowest model hybrid level ~42m
               mmr_diag =  &
                        mmr_canopy(kk) +                    &
                       (mmr_canopy(kk) - mmr_canopy(kk + 1)) / &
                          (zmid(kk) -    zmid(kk + 1)) * &
                          (diag_hgt -    zmid(kk + 1))        ! ug kg-1
               vmr_resolved      (NLAYS + 1)      = FORWARD_CONV(isp) * mmr_diag
            end if

! Flip back resolved layers arrays for gas-phase integration (hrdriver)
         do k = 1, NLAYS        ! from top to bottom
            II = NLAYS + 1 - k  ! from bottom to top of resolved model layers
            ! Paul's trppm is our CONC_can (vmr_canopy)
            ! (NLAYS) is top model layer
            ! (1)     is 1hy model layer
            CONC_MOD(COL,ROW, II, S) = vmr_resolved(k) ! ppm
         end do

! 2M Diagnostics
         CONC_2M (COL,ROW,     S) = vmr_resolved(NLAYS+1)! ppm

      end do ! number of species loop isp = 1, NUMB_MECH_SPC

! Print up to KOUNT number of canopy columns
      KOUNT = KOUNT + 1
!
   END IF ! loop over canopy columns FRT_MASK == 1.


   END DO  !COL = 1, NCOLS  !I-index
   END DO  !ROW = 1, NROWS  !J-index

!  Done transfering from combined canopy + resolved scale back to resolved scale.  :)
!
! ========================================================================
   else ! if (flag == 0) then (can_transfer == "resolved_to_canopy") then
!
! In: CONC_MOD
!
! Out: CONC_CAN (vmr_canopy)
! NB. ! Paul's trppm (mach_gas_canopy) is our vmr_canopy
! ========================================================================
!

! Assigned in hrdriver after gas-phase tendencies are pplied
!   Temporarily set array to zero (for testing)
!   CONC_CAN(:,:,:,:) = 0.0

   DO ROW = 1, NROWS  !J-index
   DO COL = 1, NCOLS  !I-index

   KOUNT = 0

   IF (FRT_mask(COL,ROW) > 0.) THEN

!...fetch all species (ozone = 4 no2 =1) and convert to kg kg-1 mass mixing ratio
      DO ISP = 1, NUMB_MECH_SPC   !NSPCSD
!     DO ISP = 1, 1     ! NO2 = 1  O3 = 4

         S = CGRID_INDEX( ISP )

!        write(logdev,*) 'canopy_transfer: CGRID_INDEX = ', ISP, S, CONC_MOD(COL,ROW, :, S), COL, ROW

! Flip resolved layer arrays into a new array for use here
! (i): Model resolved layers
      do k = 1, NLAYS        ! from bottom to top
         II = NLAYS + 1 - k  ! from top to bottom of resolved model layers NLAYS+1 ???
         ! Paul's chem_tr is our conc3 = vmr_resolved (CONC_mod)
         ! conc3(1)     is top model layer
         ! conc3(NLAYS) is 1st (bottom) model layer
         ! conc3(II) = CONC     (COL,ROW, k, S) ! ppm
         conc3(II) = CONC_MOD(COL,ROW, k, S) ! ppm
         vmr_resolved(II) = CONC_MOD(COL,ROW, k, S) ! ppm
      end do

!  (1) We start off by converting these volume mixing ratio ppm to mass in ug:
      do k = 1, NLAYS
         ! kmod(1)     is 1  top model  layer
         ! kmod(NLAYS) is 65 top canopy layer (modified after mono adj.)
         !kk = kmod(COL,ROW, k)

! ...fetch gas volume mix. ratios [ppm] and convert to mass mixing ratios [ug kg-1]
         ! Paul's conc is our mmr_resolved
! Oct9:  mmr_resolved(kk) = REVERSE_CONV(isp) * conc3(k)      ! ug kg-1
         mmr_resolved(k) = REVERSE_CONV(isp) * conc3(k)      ! ug kg-1
      end do

!  (1) Convert the original model domain values in the current column to mass from mass mixing ratio:
!  mass_resolved = Mass mixing ratio * (density) / (volume of original model layer)  (ug)
      do k = 1, NLAYS
         mass_resolved(k) = mmr_resolved(k) * massair(COL,ROW, k) ! ug
      end do

! Temporary diagnostic output
!        CONC_CAN(COL,ROW, 2, S) = mass_resolved(NLAYS-1) ! ug 2hy model layer
!        CONC_CAN(COL,ROW, 1, S) = mass_resolved(NLAYS)   ! ug 1hy model layer
!
!  (2) Use the array fractions defined earlier to divide the resolved layer masses into the canopy layers,
!  and convert back to mixing ratios.  Note that the frctr2c fractions are vertical extent of the
!  contribution of the resolved layer into the canopy layer, hence the mass/volume can be divided up
!  this way:
!  mmr_canopy = sum of masses contributed / (density * volume of canopy model layeri)
            ! Paul's mmr_canopy is our mmr_canopy in ug kg-1
            ! Paul's masscan is our mass_canopy
            mmr_canopy(:) = 0.
            mass_canopy(:) = 0.
            do k = 1, NLAYT
               do kk = 1, nfrct(k, COL,ROW)
                  kc = ifrct(k, kk, COL,ROW)
                  mass_canopy(k) = mass_canopy(k) + mass_resolved(kc) * frctr2c(k, kk, COL,ROW) ! ug
               end do
            end do

! Temporary diagnostic output
!       CONC_CAN(COL,ROW, 4, S) = mass_canopy(NLAYS  ) ! ug 1hy model  layer
!       CONC_CAN(COL,ROW, 3, S) = mass_canopy(NLAYS+1) ! ug 3rd canopy layer
!       CONC_CAN(COL,ROW, 1, S) = mass_canopy(NLAYT)   ! ug 1st canopy layer

!
!  Check:  total mass in the column should be the same
            if (local_dbg) then
               call canopy_mass_check(mass_canopy, mass_resolved, COL,ROW, flag)
               if (chm_error_l) return
            end if
!
            do k = 1, NLAYT
               ! Paul's massaircan is our massair_can
               mmr_canopy(k) = mass_canopy(k) / massair_can(COL,ROW, k)  ! ug kg-1
            end do

! Temporary diagnostic output
!        CONC_CAN(COL,ROW, 3, S) = mmr_canopy(NLAYS  ) ! ug kg -1 1hy model  layer ~85  ug kg-1
!        CONC_CAN(COL,ROW, 1, S) = mmr_canopy(NLAYT)   ! ug 1st canopy layer

! Print
         IF(.FALSE.) THEN
         IF ( KOUNT < 3 )  THEN
            do k = 1, NLAYS
               if (k > 62) &
               print*,'CAN_TRANS R2C: SPC ', S, k, &
                 mass_resolved(k), mass_canopy(k), &
              massair(COL,ROW, k), massair_can(COL,ROW, k)
            end do
            print*,'CAN_TRANS R2C: SPC ', S, k, &
              mass_canopy(NLAYS+1), mass_canopy(NLAYS+2), mass_canopy(NLAYT), &
           massair_can(COL,ROW, NLAYS+1), massair_can(COL,ROW, NLAYS+2),massair_can(COL,ROW, NLAYT)
         END IF ! KOUNT
         END IF
! End Print

!
!  (3) Replace the original model layer values with the corresponding canopy layer values, when
!  a canopy exists:
            do kk = 1, NLAYS
               k = kmod(COL, ROW, kk)
               ! Paul's chem_tr is our conc3 = vmr_resolved (CONC_mod) <================
!              conc3(kk)         = FORWARD_CONV(isp) * mmr_canopy(k)  ! ppm
               vmr_resolved (kk) = FORWARD_CONV(isp) * mmr_canopy(k)  ! ppm
            end do

! (i): Model resolved layers: for hrdriver (trppm from mach_gas_canopy)
            do kk = 1, NLAYS
               ! kmod(1)     is 1  top model  layer
               ! kmod(NLAYS) is 65 top canopy layer (modified after mono adj.)
               k = kmod(COL, ROW, kk)

               ! Paul's trppm is our vmr_canopy (CONC_can)
!              vmr_canopy(k) = conc3(kk)         !ppm
               vmr_canopy(k) = vmr_resolved(kk)  !ppm
            end do
!
!  (4) Fill the canopy layers with the new mass mixing ratios
            do kc = 1, NLAYC
               k  = kcan3(COL,ROW, kc)
               ! Paul's tracers_can is our conc_can3              <====================
               conc_can3(kc)  = FORWARD_CONV(isp) * mmr_canopy(k) ! ppm
            end do

! (ii): Canopy shaded layers (for hrdriver) (trppm from mach_gas_canopy)
            do kc = 1, NLAYC
               ! Paul's trppm is our vmr_canopy (CONC_can)
               ! kcan3(1) = 65
               ! kcan3(2) = 66
               ! kcan3(3) = 67
               k = kcan3(COL,ROW, kc)
               vmr_canopy(k) = conc_can3(kc)                      !ppm
            end do

! Temporary diagnostic output
!       CONC_CAN(COL,ROW, 3, S) = vmr_canopy(NLAYS)     ! ~50-51 ppm
!       CONC_CAN(COL,ROW, 1, S) = vmr_canopy(NLAYT)

! Prepare array for gas-phase chemical integration. (Paul's mach_gas_canopy)
!
! Flip back augmented canopy+resolved arrays for gas-phase integration (hrdriver)
         do k = 1, NLAYT        ! from top to bottom
            II = NLAYT + 1 - k  ! from bottom to top of resolved model layers
            ! (NLAYT) is top model layer
            ! (4)     is 1hy model layer
            ! (1-3)   are canopy layers
            ! Paul's trppm is our vmr_canopy (CONC_can)
            CONC_CAN(COL,ROW, II, S) = vmr_canopy(k)! ppm
         end do

      end do !species index loop isp

! Print
!     print*, 'RESOLVED_TO_CANOPY: 1HY 1-2-3CY = ', CONC_MOD(COL,ROW,1, 4), & ! O3 = 4
!       CONC_CAN(COL,ROW,1, 4), CONC_CAN(COL,ROW,2, 4), CONC_CAN(COL,ROW,3, 4)

! Print up to KOUNT number of canopy columns
      KOUNT = KOUNT + 1

!  loop over canopy columns
   END IF ! loop over canopy columns FRT_MASK == 1.
!
   END DO  !COL = 1, NCOLS  !I-index
   END DO  !ROW = 1, NROWS  !J-index
!
   end if  ! 1="canopy_to_resolved" 0= "resolved_to_canopy"

   return

 contains

   subroutine canopy_mass_check(mass_canopy, mass_model, COL,ROW, flag)
      implicit none
      integer(kind=4),   intent(in) :: flag, COL,ROW
      real(kind=4),      intent(in) :: mass_canopy(NLAYT), mass_model(NLAYS)

      character(len=18) :: mode_transfer
      real(kind=4) :: masstotcan, masstotres, massrat
      real(kind=4) :: sum2can(NLAYT), sum2res(NLAYT)

      masstotcan = 0.
      masstotres = 0.
      do k = 1, NLAYT
         masstotcan = masstotcan + mass_canopy(k)
      end do
      do k = 1,NLAYS
         masstotres = masstotres + mass_model(k)
      end do

      if (flag == 1) then
         mode_transfer = "canopy_to_resolved"
      else
         mode_transfer = "resolved_to_canopy"
      end if

      if (masstotres > 0.0) then
         massrat = masstotcan / masstotres
         if (massrat > 1.001 .or. massrat < 0.999) then
            write(*, *) 'Conversion of mass in mach_canopy_transfer not conserved'
            write(*, *) 'during ', mode_transfer, 'evaluation.  Stopping '
            write(*, *) 'code with masstotcan = ',masstotcan,' and masstotres = ', &
                              masstotres
            write(*, *) 'Values of mass_canopy: ',(mass_canopy(k), k=1, NLAYT)
            write(*, *) 'Values of mass_resolved: ',(mass_model(k), k=1, NLAYS)
            do k = 1, NLAYT
               write(*, *) 'canopy layer ',k,'has ',nfrct(k, COL,ROW),' contributions'
               do kk = 1, nfrct(k, COL,ROW)
                  write(*, *) 'Resolved # ',ifrct(k,kk,COL,ROW),' with mass: ',mass_model(ifrct(k,kk,COL,ROW)),&
                             ' contributes ',frctr2c(k,kk,COL,ROW),' to canopy layer ',k,&
                             ' with mass_canopy ',mass_canopy(k)
               end do
            end do

            chm_error_l = .true.
            return
         end if
      end if
!
!  Check on the values of the fractions:  they should sum to unity across the number
!  of original model levels!
      sum2can = 0.
      sum2res = 0.
      do k = NLAYT, 1, -1
         do kk = 1, nfrct(k, COL,ROW)
            kc = ifrct(k, kk, COL,ROW)
            sum2can(kc) = sum2can(kc) + frctr2c(k, kk, COL,ROW)
            sum2res(k) = sum2res(k) + frctc2r(k, kk, COL,ROW)
         end do
      end do

      do k = NLAYS, 1, -1
         if (sum2can(k) < 0.999 .or. sum2can(k) > 1.001) then
            write(*, *) 'layer mismatch in canopy level setup in resolved to canopy indexing'
            write(*, 20) 'sum of non-zero contributions from column ',COL,ROW, &
               ' layer ',k,' is ',sum2can(k),' (should be unity).'
            chm_error_l = .true.
            return
         end if
      end do
      do k = NLAYT, 1, -1
         if (sum2res(k) < 0.999 .or. sum2res(k) > 1.001) then
            write(*, *) 'layer mismatch in canopy level setup in canopy to resolved indexing'
            write(*, 20) 'sum of non-zero contributions from column ',COL,ROW, &
               ' layer ',k,' is ',sum2res(k),' (should be unity).'
            write(*, *) 'k nfrct(k COL ROW) frctc2r'
            write(*, *) k, nfrct(k,COL,ROW),(frctc2r(k,kk,COL,ROW), kk = 1,nfrct(k,COL,ROW))
            chm_error_l = .true.
            return
         end if
      end do

 20   format(a42, i6, a7, i3, a5, 1pe10.3, a18)
!
      return
   end subroutine canopy_mass_check

 end subroutine canopy_transfer

end module can_trans_mod
