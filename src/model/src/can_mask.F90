   module can_mask

   implicit none

! Horisontal arrays
   integer(kind=4), public :: ni_can, nj_can, ni_nocan, nj_nocan  ! Number of surface grid pts with canopy shading
   integer(kind=4), allocatable, save, public :: ni_can_col(:), ni_nocan_col(:), &
                                           nj_can_row(:), nj_nocan_row(:)

   real, allocatable, save, public :: FRT_mask(:,:)     ! Continuos Forest Canopy mask

   public :: init_can_mask, get_can_mask

   INTEGER, PRIVATE                :: LOGDEV                 ! unit number for the log file

   CONTAINS

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   subroutine init_can_mask(JDATE, JTIME)


   USE GRID_CONF, ONLY: NROWS, NCOLS, NLAYS, NLAYC, NLAYT, MY_NROWS, MY_NCOLS  ! horizontal & vertical domain specifications
   USE UTILIO_DEFN

   IMPLICIT NONE

!...Arguments:

   INTEGER, INTENT( IN ) :: JDATE         ! current Julian date (YYYYDDD)
   INTEGER, INTENT( IN ) :: JTIME         ! current time (HHMMSS)

!...local variables

   CHARACTER(  32 ) :: PNAME             = 'INIT_CAN_MASK'
   CHARACTER( 120 ) :: XMSG

   INTEGER          :: ALLOCSTAT

   LOGDEV   = INIT3()

!  WRITE( LOGDEV, * ) , 'init_can_mask: NCOLS, NROWS = ', NCOLS, NROWS, MY_NROWS, MY_NCOLS

!...Allocate and initialize new canopy arrays

   allocate (ni_can_col(NCOLS), ni_nocan_col(NCOLS), &
             nj_can_row(NROWS), nj_nocan_row(NROWS) )

   ALLOCATE( FRT_MASK (NCOLS,NROWS), STAT = ALLOCSTAT )
   IF ( ALLOCSTAT .NE. 0 ) THEN
      XMSG = 'Failure allocating FRT_MASK canopy array'
      CALL M3EXIT ( PNAME, JDATE, JTIME, XMSG, XSTAT1 )
   END IF

! Initializations

   ni_can_col  (:) = 0
   ni_nocan_col(:) = 0
   nj_can_row  (:) = 0
   nj_nocan_row(:) = 0

   FRT_mask(:,:)=0.0

!  WRITE( LOGDEV, * ) 'init_can_mask: NLAYC, NLAYT, NLAYS ', NLAYC, NLAYT, NLAYS

   end subroutine init_can_mask

!:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

   subroutine get_can_mask (JDATE, JTIME)

   USE GRID_CONF, ONLY: NROWS, NCOLS, NLAYS, NLAYC, NLAYT, MY_NROWS, MY_NCOLS  ! horizontal & vertical domain specifications
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

   LOGDEV   = INIT3()

   DO ROW = 1, NROWS  !J-index
   DO COL = 1, NCOLS  !I-index

      !NOT a Continuos forest canopy
      IF (                     Met_Data%LAIE(COL,ROW)  .LT. 0.1     &
           .OR.                Met_Data%FCH (COL,ROW)  .LT. 0.5     &
           .OR. MAX(0.0, 1.0 - Met_Data%FRT (COL,ROW)) .GT. 0.75    &  ! IVAI: 0.5 for old canopy data
           .OR.                Met_Data%POPU(COL,ROW)  .GT. 10000.0 &
           .OR.      (EXP(-0.5*Met_Data%LAIE(COL,ROW) *             &
                               Met_Data%CLU (COL,ROW)) .GT. 0.45    &
                          .AND.Met_Data%FCH (COL,ROW)  .LT. 18.0) ) THEN

!   WRITE( LOGDEV, 5003 ) COL, ROW,
!     Met_Data%LAIE(COL,ROW), Met_Data%FCH (COL,ROW), &
!     Met_Data%FRT (COL,ROW), Met_Data%POPU(COL,ROW)

         FRT_mask(COL,ROW) = -1.0
         nj_nocan_row(ROW) = nj_nocan_row(ROW) +1

      ! Continuous forest canopy
      ELSE

         FRT_mask(COL,ROW) = 1.0
         nj_can_row(ROW) = nj_can_row(ROW) + 1

      END IF ! Forest Canopy Mask

   END DO
   END DO

   nj_can = 0
   nj_nocan = 0
   DO ROW = 1, NROWS  !J-index
      nj_can   = nj_can   + nj_can_row  (ROW)
      nj_nocan = nj_nocan + nj_nocan_row(ROW)
   END DO

!  WRITE( LOGDEV, 5002 ) nj_can, nj_nocan


   DO COL = 1, NCOLS  !I-index
   DO ROW = 1, NROWS  !J-index

      !NOT a Continuos forest canopy
      IF (                     Met_Data%LAIE(COL,ROW)  .LT. 0.1     &
           .OR.                Met_Data%FCH (COL,ROW)  .LT. 0.5     &
           .OR. MAX(0.0, 1.0 - Met_Data%FRT (COL,ROW)) .GT. 0.75    &  ! IVAI: 0.5 for old canopy data
           .OR.                Met_Data%POPU(COL,ROW)  .GT. 10000.0 &
           .OR.      (EXP(-0.5*Met_Data%LAIE(COL,ROW) *             &
                               Met_Data%CLU (COL,ROW)) .GT. 0.45    &
                          .AND.Met_Data%FCH (COL,ROW)  .LT. 18.0) ) THEN

         ni_nocan_col(COL) = ni_nocan_col(COL) +1

      ! Continuous forest canopy
      ELSE

         ni_can_col(COL) = ni_can_col(COL) + 1

      END IF ! Forest Canopy Mask

   END DO
   END DO

   ni_can = 0
   ni_nocan = 0
   DO COL = 1, NCOLS  !I-index
      ni_can   = ni_can   + ni_can_col  (COL)
      ni_nocan = ni_nocan + ni_nocan_col(COL)
   END DO

!  WRITE( LOGDEV, 5001 ) ni_can, ni_nocan

!   WRITE( LOGDEV, 5003 ) COL, ROW,
!     FRT_mask(COL,ROW),      &
!     Met_Data%LAIE(COL,ROW), Met_Data%FCH (COL,ROW), &
!     Met_Data%FRT (COL,ROW), Met_Data%POPU(COL,ROW)

5001  FORMAT(' get_can_mask: NI_CAN = ',I6,1X,' NI_NOCAN= ',I6,1X )
5002  FORMAT(' get_can_mask: NJ_CAN = ',I6,1X,' NJ_NOCAN= ',I6,1X )
5003  FORMAT(' get_can_mask: CANOPY LAI FCH FRT POPU = ',1X,2(I5),4(F12.4,1X))

   end subroutine get_can_mask

   end module can_mask
