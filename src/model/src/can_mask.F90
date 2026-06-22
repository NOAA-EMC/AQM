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
! C %W% %P% %G% %U%

   module can_mask

!*************************************************************************
!
!  Function: Define the vertical canopy layer structure and determine the grid cells
!     of continuous forest canopy, where canopy shading effect is applied,
!     based on novel vegetative canopy data (forest canopy height FCH,
!     forest fraction, leaf area index LAI, clumping index, and population density)
!
!  Preconditions: GRID_CONF defines the vertical canopy layer structure
!     For the CB6R3 family of gas-phase chemical mechanisms performed by
!     either the Euler Backward Iterative (EBI) solver or the vectorized Rosenbrock (ROS3) solver
!     Activated with config namelist parameter do_aqm_canopy= .true.
!
!  Key Subroutines/Functions Called: None
!
!  Revision History: Created by I. Ivanova March 2024.
!     Based on Makar, P. A. et al. "The effects of forest canopy shading
!     and turbulence on boundary layer ozone." Nat. Commun. 8, 15243
!     doi: 10.1038/ncomms15243 (2017).
!
!  15 Dec 24 I.Ivanova: Revised the canopy vertical layer defintions in GRID_CONFIG.
!*************************************************************************

   implicit none

   real, allocatable, save, public :: FRT_mask(:,:)     ! Continuos Forest Canopy mask

   public :: init_can_mask, get_can_mask

   CONTAINS

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   subroutine init_can_mask(JDATE, JTIME)


   USE GRID_CONF, ONLY: NROWS, NCOLS, NLAYS, NLAYC, NLAYT  ! horizontal & vertical domain specifications
   USE UTILIO_DEFN

   IMPLICIT NONE

!...Arguments:

   INTEGER, INTENT( IN ) :: JDATE         ! current Julian date (YYYYDDD)
   INTEGER, INTENT( IN ) :: JTIME         ! current time (HHMMSS)

!...local variables

   CHARACTER(  32 ) :: PNAME             = 'INIT_CAN_MASK'
   CHARACTER( 120 ) :: XMSG

   INTEGER          :: ALLOCSTAT

!...Allocate and initialize new canopy arrays
   ALLOCATE( FRT_MASK (NCOLS,NROWS), STAT = ALLOCSTAT )
   IF ( ALLOCSTAT .NE. 0 ) THEN
      XMSG = 'Failure allocating FRT_MASK canopy array'
      CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
   END IF

! Initializations
   FRT_mask(:,:)=0.0

   end subroutine init_can_mask

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

   subroutine get_can_mask (JDATE, JTIME)

   USE RUNTIME_VARS, only: LOGDEV
   USE GRID_CONF, ONLY: NROWS, NCOLS, NLAYS, NLAYC, NLAYT  ! horizontal & vertical domain specifications
   USE  PHOT_MET_DATA        ! Met and Grid data
!Used for canopy shade calculation
   USE ASX_DATA_MOD, ONLY : MET_DATA        !use met data
   USE UTILIO_DEFN

   IMPLICIT NONE

!...Arguments:

   INTEGER, INTENT( IN ) :: JDATE         ! current Julian date (YYYYDDD)
   INTEGER, INTENT( IN ) :: JTIME         ! current time (HHMMSS)

!...local variables

   CHARACTER(  32 ) :: PNAME             = 'GET_CAN_MASK'
   CHARACTER( 120 ) :: XMSG

   INTEGER          :: ALLOCSTAT

   INTEGER          :: COL, ROW

!   INTEGER :: LOGDEV = -1  ! File Unit for Ascii Log File

   DO ROW = 1, NROWS  !J-index
   DO COL = 1, NCOLS  !I-index

      !NOT a Continuos forest canopy
      IF (                     Met_Data%LAIE(COL,ROW)  .LT. 0.1     &
           .OR.                Met_Data%FCH (COL,ROW)  .LT. 0.5     &
           .OR. MAX(0.0, 1.0 - Met_Data%FRT (COL,ROW)) .GT. 0.75    &  ! 0.5 for old canopy data
           .OR.                Met_Data%POPU(COL,ROW)  .GT. 10000.0 &
           .OR.      (EXP(-0.5*Met_Data%LAIE(COL,ROW) *             &
                               Met_Data%CLU (COL,ROW)) .GT. 0.45    &
                          .AND.Met_Data%FCH (COL,ROW)  .LT. 18.0) ) THEN

!   WRITE( LOGDEV, 5003 ) COL, ROW,
!     Met_Data%LAIE(COL,ROW), Met_Data%FCH (COL,ROW), &
!     Met_Data%FRT (COL,ROW), Met_Data%POPU(COL,ROW)

         FRT_mask(COL,ROW) = -1.0

      ! Continuous forest canopy
      ELSE

         FRT_mask(COL,ROW) = 1.0

      END IF ! Forest Canopy Mask

   END DO
   END DO


   DO COL = 1, NCOLS  !I-index
   DO ROW = 1, NROWS  !J-index

      !NOT a Continuos forest canopy
      IF (                     Met_Data%LAIE(COL,ROW)  .LT. 0.1     &
           .OR.                Met_Data%FCH (COL,ROW)  .LT. 0.5     &
           .OR. MAX(0.0, 1.0 - Met_Data%FRT (COL,ROW)) .GT. 0.75    &  ! 0.5 for old canopy data
           .OR.                Met_Data%POPU(COL,ROW)  .GT. 10000.0 &
           .OR.      (EXP(-0.5*Met_Data%LAIE(COL,ROW) *             &
                               Met_Data%CLU (COL,ROW)) .GT. 0.45    &
                          .AND.Met_Data%FCH (COL,ROW)  .LT. 18.0) ) THEN

      ! Continuous forest canopy
      ELSE

      END IF ! Forest Canopy Mask

   END DO
   END DO

!   WRITE( LOGDEV, 5003 ) COL, ROW,
!     FRT_mask(COL,ROW),      &
!     Met_Data%LAIE(COL,ROW), Met_Data%FCH (COL,ROW), &
!     Met_Data%FRT (COL,ROW), Met_Data%POPU(COL,ROW)

5003  FORMAT(' get_can_mask: CANOPY LAI FCH FRT POPU = ',1X,2(I5),4(F12.4,1X))

   end subroutine get_can_mask

   end module can_mask
