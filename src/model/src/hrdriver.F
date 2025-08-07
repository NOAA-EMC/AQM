
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

       SUBROUTINE CHEM( CGRID, JDATE, JTIME, TSTEP )
C**********************************************************************
C
C  FUNCTION: Driver subroutine for Euler Backward Iterative solver
C
C  PRECONDITIONS: For the CB6R5_AE7_AQ mechanism
C
C  KEY SUBROUTINES/FUNCTIONS CALLED:  HRINIT, HRCALCKS, HRSOLVER
C                                     FIND_DEGRADED, INIT_DEGRADE, FINAL_DEGRADE,
C                                     CANOPY_TRANSFER
C
C  REVISION HISTORY: Created by EBI solver program, Jun  8, 2022
C                       Based on the algorithm in "Test of Two Numerical
C                       Schemes for Use in Atmospheric Transport-Chemistry
C                       Models", O. Hertel, R. Berkowicz, J. Christensen,
C                       and O. Hov, Atm Env., Vol. 27A, No. 16, 1993.
C                       Original MEBI code developed by Ho-Chun Huang,
C                       SUNY, Albany -- "On the performance of numerical
C                       solvers for a chemistry submodel in three-dimensional
C                       air quality models 1. Box model simulations",
C                       H. Huang and J.S. Chang, JGR, Vol 106, No. D17, 2001.
C                       This version replaces Huang and Chang use of numerical
C                       solutions with analytical solutions derived in
C                       Hertel et al.
C   21 Jun 10 J.Young: convert for Namelist redesign
C   11 May 11 D.Wong: incorporated twoway model implementation
C   27 Sep 11 B.Hutzell: revised method for defining CALL_DEG variable
C   18 Jul 14 B.Hutzell: revised: 1) to use the RXNS_DATA and RXNS_FUNCTION
C                        modules instead of include files, 2) to enable
C                        reactions between species types, 3) to calculate
C                        heterogeneous rate constants via AEROSOL_CHEMISTRY
C                        module, 4) to replace call to HRCALCKS with
C                        subroutine in RXNS_FUNCTION module and 5) to change
C                        how degrade routines are used, if present
C   02 Dec 14 B.Hutzell: 1) added terrestrial data to conduct surface
C                        dependent reactions and 2) modified the call CALC_RCONST
C                        routine
C   01 Feb 19 D.Wong:    Implemented centralized I/O approach, removed all MY_N
C                        clauses
C   01 May 24 I.Ivanova:
C         - Split the EBI solver on 3 sub-layer canopy layers to account for the
C           the canopy shading effect on photolysis attenuation.
C           based on Makar, P. A. et al. (2017) "The effects of forest canopy shading
C           and turbulence on boundary layer ozone." Nat. Commun. 8, 15243
C           doi: 10.1038/ncomms15243 (2017).
C   25 Aug 24 I.Ivanova
C         - Add ozone diagnostics and auxilliary diagnostic output
C**********************************************************************

      USE HGRD_DEFN             ! horizontal domain specifications
      USE VGRD_DEFN             ! vertical layer specifications
      USE CGRID_SPCS            ! CGRID mechanism species
      USE UTILIO_DEFN           ! IOAPI parameters and functions declarations
      USE RXNS_DATA
      USE AEROSOL_CHEMISTRY
      USE RXNS_FUNCTION
!IVAI
      USE GRID_CONF, Only: NLAYC, NLAYT     ! horizontal & vertical domain specifications
!IVAI
#ifdef isam
      USE SA_IRR_DEFN
      USE SA_DEFN
#endif
      USE HRDATA
      USE PHOT_MOD, Only: INIT_PHOT_SHARED, RJ,     ! photolysis rate, in-line module
     &                    LO3O1D !IVAI
      USE PA_DEFN, Only: LIRR                      ! Process Analysis control and data variable
      USE PA_IRR_CLT

!IVAI
      use can_mask
! =====================
!     call init_can_mask
!     call get_can_mask
! Out:
!      FRT_MASK         (get_can_mask)
! =====================
      use can_levs_defn
! =====================
!     call get_can_levs
! In:  CGRID
!
! Out: ZMID_CAN  (:,:,NLAYT)
!      ZMOM_CAN  (:,:,NLAYT+1)
!      TA_CAN    (:,:,NLAYT)
!      QV_CAN
!      PRES_CAN
!      DENS_CAN
!      CONC_MOD        <= CGRID      ! ALL CANOPY & NON-CANOPY COLUMNS !
!      CONC_CAN(1,2,3) <= CGRID(1)   ! ALL CANOPY & NON-CANOPY COLUMNS !
! =====================
      use can_trans_mod
! =====================
!     call canopy_transfer
!  In:
!   CGRID   (:,:, NLAYS, NSPCSD) :  Chemical tracers conc. ppmv on model levels
!   CONC_MOD(:,:, NLAYS, NSPCSD) :  Chemical tracers conc. ppmv on model levels
!
!  Output:
!   CONC_CAN(:,:, NLAYT, NSPCSD) :  Chemical tracers conc. ppmv on combined canopy+resolved layers
!                                    ! CANOPY COLUMNS ONLY !
! ================================
!IVAI

#ifndef isam
      USE DEGRADE_ROUTINES, ONLY : N_REACT, RXTANT_MAP, DEG_LAY, DEG_COL, DEG_ROW,
     &                              FIND_DEGRADED, INIT_DEGRADE, FINAL_DEGRADE
#else
      USE DEGRADE_ROUTINES, ONLY : N_REACT, RXTANT_MAP, SA_DEGRADE_EXTRACT,
     &                              FIND_DEGRADED, INIT_DEGRADE, FINAL_DEGRADE,
     &                              SA_DEGRADE_UPLOAD, DEG_LAY, DEG_COL, DEG_ROW
#endif
      USE CENTRALIZED_IO_MODULE, ONLY : INTERPOLATE_VAR, OCEAN, SZONE
#ifdef sens
      USE DDM3D_CHEM
      Use DDM3D_DEFN, Only: DATENUM, STARTDATE, IPT, IDATE, HIGH, NP, NPMAX, CKTIME
#endif

      IMPLICIT NONE

C..Includes:
      INCLUDE SUBST_FILES_ID  ! CMAQ files
      INCLUDE SUBST_CONST     ! CMAQ constants

      INCLUDE SUBST_EMISPRM   ! Emissions processing control parameters

C..Arguments:
      REAL, POINTER :: CGRID( :,:,:,: )  ! Species concentrations
      INTEGER JDATE           ! Current date (YYYYDDD)
      INTEGER JTIME           ! Current time (HHMMSS)
      INTEGER TSTEP( 3 )      ! Time step vector (HHMMSS)
! IVAI:
! TSTEP(1) = local output step
! TSTEP(2) = sciproc sync. step (chem)
! TSTEP(3) = twoway model time step w.r.t. wrf time
! IVAI

C..Parameters:
      REAL( 8 ), PARAMETER :: DCONMIN = 1.0D-30               ! minimum species concentration allowed
      REAL,      PARAMETER :: CONCMIN = 1.0E-30               ! minimum species concentration allowed
      REAL,      PARAMETER :: MAOMV   = 1.0E6 * MWAIR / MWWAT ! Mol Wt of air over Mol Wt of water times 1.0E6

!IVAI
C.. Canopy option control
      CHARACTER( 20 ), SAVE :: CTM_CANOPY_SHADE = 'CTM_CANOPY_SHADE    '! env var for in-line
      LOGICAL,         SAVE :: CANOPY_SHADE     ! flag in-lining canopy shading
!IVAI

C..External Functions:


C..Saved Local Variables:

      CHARACTER( 16 ), SAVE :: PNAME = 'HRDRIVER'     ! Program name

!IVAI
      CHARACTER( 120 ) :: XMSG
!IVAI

      INTEGER, SAVE :: ISTFL            ! Unit no. of iteration stat output file
      LOGICAL, SAVE :: LFIRST = .TRUE.  ! Flag for first call to this subroutine

      REAL( 8 ), SAVE :: PA2ATM   ! Pascal to atm conv fac

C..Scratch Local Variables:
      CHARACTER( 132 ) :: MSG           ! Message text
      CHARACTER(  16 ) :: VNAME         ! Name of I/O API data variable

      INTEGER C, E, L, R, S   ! Loop indices
      INTEGER ISP             ! array index

      INTEGER AVGEBI          ! Average no. of EBI iterations
      INTEGER DELT_SEC        ! EBI max time step in seconds
      INTEGER ESP             ! Loop index for emissions species
      INTEGER ITMSTEP         ! Chemistry integration interval (sec)
      INTEGER LEV             ! Layer index
      INTEGER MIDDATE         ! Date at time step midpoint
      INTEGER MIDTIME         ! Time at time step midpoint
      INTEGER NPH             ! Index for number of phot. rxns in PHOT
      INTEGER SPC             ! Species loop index
#ifdef hrstats
      INTEGER MNEBI           ! Min no. of EBI iterations
      INTEGER MXEBI           ! Max no. of EBI iterations
#endif

      LOGICAL LSUNLIGHT       ! Flag for sunlight

      REAL       INV_DENS     ! reciprocal of air mass density, m3/Kg
#ifdef hrstats
      REAL       SUMEBI       ! Sum of EBI iterations
#endif

!IVAI
      INTEGER       :: KOUNT
      INTEGER       :: IOSX            ! i/o and allocate memory status
!IVAI

      INTERFACE
        SUBROUTINE HRSOLVER( JDATE, JTIME, C, R, L )
           INTEGER, INTENT( IN ) :: JDATE    ! Current date (YYYYDDD)
           INTEGER, INTENT( IN ) :: JTIME    ! Current time (HHMMSS)
           INTEGER, INTENT( IN ) :: C, R, L  ! Cell col, row, lev
        END SUBROUTINE HRSOLVER
        SUBROUTINE HETCHEM_UPDATE_AERO( CGRID )
           REAL, POINTER :: CGRID( :,:,:,: )  !  species concentration in cell
        END SUBROUTINE HETCHEM_UPDATE_AERO
      END INTERFACE

C**********************************************************************

      IF( NUMB_MECH_SPC .EQ. 0 ) RETURN

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  On first call, call routines to set-up for EBI solver
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF( LFIRST ) THEN

         IF( MECHNAME .NE. 'CB6R5_AE7_AQ' ) THEN
             MSG = 'This version of the EBI solver can only be used with'
     &            // ' the CB6R5_AE7_AQ chemical mechanism'
             CALL M3EXIT( PNAME, 0, 0, MSG, XSTAT1 )
         END IF

#ifdef emis_chem
         EMISVD = .FALSE.
#else
         EMISVD = .TRUE.
#endif

         IF( INDEX( MECHNAME, 'AE' ) .NE. 0 ) THEN
           L_AE_VRSN = .TRUE.
         ELSE
           L_AE_VRSN = .FALSE.
         END IF

         IF( INDEX( MECHNAME, 'AQ' ) .NE. 0 ) THEN
           L_AQ_VRSN = .TRUE.
         ELSE
           L_AQ_VRSN = .FALSE.
         END IF

         IF( LIRR ) THEN
            CALL_IRR = .TRUE.
         ELSE
            CALL_IRR = .FALSE.
         END IF

         MODEL_SPECIES = NSPCSD    ! WTH: find number of model species

! Determine whether DEGRADE rountines are needed.

         CALL FIND_DEGRADED( JDATE, JTIME, CALL_DEG )
         IF( CALL_DEG )THEN
            WRITE(LOGDEV,*)'TX DEGRADE ROUTINES USED'
            WRITE(LOGDEV,*)'Mechanism contains degraded species'
#ifdef verbose_gas
         ELSE
            WRITE(LOGDEV,*)'TX DEGRADE ROUTINES OMITTED'
            WRITE(LOGDEV,*)'MECHANISM does not include degraded species'
#endif
         END IF

         CALL HRINIT

         ITMSTEP = TIME2SEC( TSTEP( 2 ) )
         CHEMSTEP = REAL( ITMSTEP, 8 ) / 60.0D0
         WRITE( LOGDEV, 92000 ) CHEMSTEP, DELTAT

         WRITE( LOGDEV, 92020 )
         DO SPC = 1, NUMB_MECH_SPC
            WRITE( LOGDEV, 92040 ) CHEMISTRY_SPC( SPC ), RTOL( SPC )
         END DO

         PA2ATM =  REAL( 1.0 / STDATMPA, 8)

c..If emissions processing requested stop
         IF( .NOT. EMISVD ) THEN  ! assumes emis processing in gas chem

            MSG = 'ERROR: EBI solver not configured to '//
     &            'process emissions in chemistry'
            CALL M3EXIT( PNAME, JDATE, JTIME, MSG, XSTAT1 )

         END IF   ! End if doing emissions


#ifdef hrstats
         ISTFL = JUNIT()
         OPEN( UNIT=ISTFL, FILE='iterstat.dat' )
         WRITE( ISTFL, 94020 )
#endif
C Define processor offsets

         PECOL_OFFSET = COLSD_PE( 1, MYPE+1 ) - 1
         PEROW_OFFSET = ROWSD_PE( 1, MYPE+1 ) - 1

         ALLOCATE( SEAWATER_ZONE( NCOLS, NROWS ) )
         DO R = 1, NROWS
            DO C = 1, NCOLS
               SEAWATER_ZONE( C,R ) =  OCEAN( C,R ) + SZONE( C,R )
             END DO
         END DO

         ALLOCATE( DENS( NCOLS, NROWS, NLAYS ), PRES( NCOLS, NROWS, NLAYS ),
     &             QV  ( NCOLS, NROWS, NLAYS ), TA  ( NCOLS, NROWS, NLAYS ),
     &             SEAICE( NCOLS, NROWS ) ,
!IVAI
! 2D
     &             Q2    ( NCOLS, NROWS ),       TEMP2   ( NCOLS, NROWS ),
     &           aux2d_01( NCOLS, NROWS ),       aux2d_02( NCOLS, NROWS ),
     &           aux2d_03( NCOLS, NROWS ),
! 3D
     &             o3_old( NCOLS, NROWS, NLAYS ), no2_old( NCOLS, NROWS, NLAYS ),
     &             o3_new( NCOLS, NROWS, NLAYS ), no2_new( NCOLS, NROWS, NLAYS ),
     &             o3_tend(NCOLS, NROWS, NLAYS ), no2_tend(NCOLS, NROWS, NLAYS ))
!IVAI

C..Initialize shared photolysis data
         CALL INIT_PHOT_SHARED()

C..Determine which cells need IRR
         ALLOCATE( LFLAGIRR ( NCOLS, NROWS, NLAYS ) )
         CALL      PA_IRR_CKCELLS ( LFLAGIRR )

#ifdef isam
        NUMB_ISAM_CELLS = 1.0D0 / ( NCOLS * NROWS * NLAYS )
        CALL SA_IRR_INIT
#endif

#ifdef sens
         CALL INIT_DDM3D_CHEM()

C For higher order sensitivities
         IF ( HIGH ) THEN
            DO RXN = 1, NRXNS
               IF( NREACT( RXN ) .EQ. 1 ) THEN
                  ORDER1( RXN ) = .TRUE.
               ELSE
                  ORDER1( RXN ) = .FALSE.
               END IF
            END DO
         END IF
#endif

!IVAI
!> ccccccccccccccccccccc canopy shade option!ccccccccccccccccccccc
         CANOPY_SHADE = ENVYN( 'CTM_CANOPY_SHADE',
     &                   'Flag for in-line canopy shading',
     &                   .FALSE., IOSX )

         IF ( CANOPY_SHADE ) THEN
            XMSG = 'Using in-line canopy shading option'
            CALL M3MSG2( XMSG )
         END IF

         LFIRST = .FALSE.
!IVAI

      END IF      ! First time

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C  Set date and time to center of time step, get necessary physical
C  data
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      MIDDATE = JDATE
      MIDTIME = JTIME
      ITMSTEP = TIME2SEC( TSTEP( 2 ) )
      CHEMSTEP = REAL( ITMSTEP, 8 ) / 60.0D0
      CALL NEXTIME( MIDDATE, MIDTIME, SEC2TIME( ITMSTEP / 2 ) )

C.. Compute number of time step loops and step size for EBI solver
      DELT_SEC = INT( DELTAT * 60.0D0 + 0.1D0 )
      IF( DELT_SEC .GE. ITMSTEP ) THEN
         N_EBI_STEPS = 2
         EBI_TMSTEP  = 0.5D0 * CHEMSTEP
      ELSE
         IF( MOD( ITMSTEP, DELT_SEC ) .EQ. 0 ) THEN
            N_EBI_STEPS = ITMSTEP / DELT_SEC
         ELSE
            N_EBI_STEPS = ITMSTEP / DELT_SEC + 1
         END IF
         EBI_TMSTEP =  CHEMSTEP / REAL( N_EBI_STEPS, 8 )
      END IF

      N_INR_STEPS = 1


C.. Get fractional seaice coverage from the METCRO2D file.

      CALL INTERPOLATE_VAR ('SEAICE', MIDDATE, MIDTIME, SEAICE)

C.. Get ambient temperature in K

      CALL INTERPOLATE_VAR ('TA', MIDDATE, MIDTIME, TA)

C.. Get specific humidity in Kg H2O / Kg air
      CALL INTERPOLATE_VAR ('QV', MIDDATE, MIDTIME, QV)

! Get ambient MASS DENSITY in Kg/m^3
      CALL INTERPOLATE_VAR ('DENS', MIDDATE, MIDTIME, DENS)

C.. Get pressure in Pascals
      CALL INTERPOLATE_VAR ('PRES', MIDDATE, MIDTIME, PRES)

!IVAI
! Get 2-m temperature in K
      CALL INTERPOLATE_VAR ('TEMP2', MIDDATE, MIDTIME, TEMP2)

! Get 2-m Specific Humidity in Kg H2O / Kg air
      CALL INTERPOLATE_VAR ('Q2', MIDDATE, MIDTIME, Q2)
!IVAI

C.. Get Heteorogeneous rates and Update Aerosol Distribution Properties
      CALL HETCHEM_RATES( TA, PRES, QV, CGRID, DENS )

#ifdef sens
      DATENUM = 1 + JDATE - STARTDATE !  Set the date and hour counters used in sensitivity calls

C For reaction rate sensitivities
      DO NP = 1, NPMAX
         IF ( IPT( NP ) .EQ. 5 ) THEN
            CALL CKTIME( JDATE,JTIME,NP,RXNFLAG(NP) ) ! Rxnflag set to true iff ipt=5 and time, date within bounds
            IF ( IDATE( NP, DATENUM ) .NE. 1 ) RXNFLAG( NP ) = .FALSE.
         ELSE
            RXNFLAG( NP ) = .FALSE.
         END IF
      END DO

#endif sens

! IVAI:
! Species before gas-phase tendencies added (NB. Species indices defined in "hrdata_mod", for O3 = 4)
      o3_old (:,:,:) = CGRID(:,:,:,4) ! O3 = 4

! Initialize canopy levels
      If ( CANOPY_SHADE  ) Then

        WRITE( LOGDEV, *) 'HRDRIVER: NLAYC NLAYT = ', NLAYC, NLAYT

! The statement below must reside after call PHOT
      IF (.FALSE.) THEN
      DO R = 1, MY_NROWS
         DO C = 1, MY_NCOLS

            KOUNT = 0
            IF ( FRT_MASK(C,R) > 0. ) THEN

            IF ( KOUNT < 3 )  THEN

!1             print*, 'HRDRIVER: RES2CAN = ',

               print*, 'HRDRIVER: KHETERO 1HY = ',
     &         KHETERO(1:NHETERO,C,R,1 )  ! NHETERO = 11  in total

               print*, 'HRDRIVER: KHETERO 1CY = ',

!0             DO L = 1, 3 !NLAYT

!1   &         CONC_MOD(C,R,1, 4), ! O3 = 4
!1   &         RJ_CAN   (C,R,1, LO3O1D),
!1   &         CONC_CAN(C,R,1, 4), CONC_CAN(C,R,3, 4)  ! O3 = 4
!
!1   &         TA_CAN(C,R, 1), DENS_CAN(C,R, 1), QV_CAN(C,R, 1)
     &         KHETERO_CAN(1:NHETERO,C,R,1 )  ! NHETERO = 11  in total

!0             END DO ! NLAYT

            END IF ! KOUNT

! Print up to KOUNT number of canopy columns
               KOUNT = KOUNT + 1

            END IF ! FRT_MASK

         END DO
      END DO
      END IF ! .FALSE.

! Temporary output
!     aux2d_03(:,:) = RJ (:,:,1,LO3O1D)
!     aux2d_03(:,:) = RJ_CAN (:,:,1,LO3O1D)
!     aux2d_02(:,:) = real(    KHETERO(NHETERO,:,:,1 ),4) ! NHETERO=11 reaction
!     aux2d_03(:,:) = real(KHETERO_CAN(NHETERO,:,:,1 ),4) ! NHETERO=11 reaction

! This is CONC_can from canopy_transfer after r2c (flag = 0)
!     aux2d_02(:,:) = CONC_CAN(:,:,1,4) ! 1st canopy layer O3 = 4

!!!!!!!!!!!!!!!!!
! !!! TESTING !!! Overwrite met3d & cgrid calculated in get_can_levs (phot)
!!!!!!!!!!!!!!!!!
! NB. Comment out to use values from canopy_transfer "resolved_to_canopy"
!
!... First, carry over original model values for the matching layers
!     TA_CAN   (:,:,NLAYC+1:NLAYT)   = TA  (:,:,1:NLAYS)
!     QV_CAN   (:,:,NLAYC+1:NLAYT)   = QV  (:,:,1:NLAYS)
!     PRES_CAN (:,:,NLAYC+1:NLAYT)   = PRES(:,:,1:NLAYS)
!     DENS_CAN (:,:,NLAYC+1:NLAYT)   = DENS(:,:,1:NLAYS)

! Overwrite CONC_can calculated in canopy_transfer "resolved_to_canopy"
!   NB. Comment out to use values from canopy_transfer "resolved_to_canopy"
!     CONC_CAN(:,:,NLAYC+1:NLAYT,:) = CONC(:,:,1:NLAYS,:)

!... Initialize the canopy layers with 1hy layer values
!     TA_CAN(:,:,3) = TA( :,:,1) ! using values from get_can_levs when commented out
!     TA_CAN(:,:,2) = TA( :,:,1)
!     TA_CAN(:,:,1) = TA( :,:,1)

!     QV_CAN(:,:,3) = QV( :,:,1)
!     QV_CAN(:,:,2) = QV( :,:,1)
!     QV_CAN(:,:,1) = QV( :,:,1)

!     PRES_CAN(:,:,3) = PRES( :,:,1)
!     PRES_CAN(:,:,2) = PRES( :,:,1)
!     PRES_CAN(:,:,1) = PRES( :,:,1)

!     DENS_CAN(:,:,3) = DENS( :,:,1)
!     DENS_CAN(:,:,2) = DENS( :,:,1)
!     DENS_CAN(:,:,1) = DENS( :,:,1)

! NB. Must initialize all canopy & non-canopy columns RJ_CAN & KHETERO_CAN

!... First, carry over original model values for the matching layers
! NB. KHETERO_CAN(NHETERO,NCOLS, NROWS, NLAYT) is real(8)
      KHETERO_CAN(:,:,:,NLAYC+1:NLAYT)   = KHETERO(:,:,:,1:NLAYS) ! real(8)

!... Initialize the canopy layers with 1hy layer values
      KHETERO_CAN(:,:,:,3) = KHETERO(:,:,:,1)  ! real(8)
      KHETERO_CAN(:,:,:,2) = KHETERO(:,:,:,1)  ! real(8)
      KHETERO_CAN(:,:,:,1) = KHETERO(:,:,:,1)  ! real(8)

! Overwrite CONC_can calculated in canopy_transfer "resolved_to_canopy"
! NB. Comment out to use values from canopy_transfer "resolved_to_canopy"
!     CONC_CAN(:,:,3,:) = CONC(:,:,1,:)
!     CONC_CAN(:,:,2,:) = CONC(:,:,1,:)
!     CONC_CAN(:,:,1,:) = CONC(:,:,1,:)

!!!!!!!!!!!!!!!!!!!!!
! !!! END TESTING !!! overwrite met3d & cgrid calculated in get_can_levs
!!!!!!!!!!!!!!!!!!!!!

! Species before gas-phase tendencies added (NB. All canopy & non-canopy grid-cells)
      o3_old_can (:,:,1:NLAYT) = CONC_CAN(:,:,1:NLAYT,4) ! O3 = 4

! Temporary output
!     aux2d_03(:,:) = PRES_CAN(:,:,1)
!     aux2d_02(:,:) = DENS_CAN(:,:,1)
!     aux2d_02(:,:) = TA_CAN  (:,:,1)
!     aux2d_03(:,:) = CONC_CAN(:,:,1,4) ! 1st canopy layer O3 = 4

      End If  !CANOPY_SHADE


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Top of loop over cells
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

#ifdef hrstats
      MNEBI = 1000
      MXEBI = 0
      SUMEBI = 0.0
#endif

      NCELL = 1

      DO L = 1, NLAYS
         DO R = 1, NROWS
            DO C = 1, NCOLS

               DEG_LAY = L
               DEG_ROW = R
               DEG_COL = C

c..reset CALL_IRR based on cell's values
               CALL_IRR = LFLAGIRR( C,R,L )

c..Load ICs
               INV_DENS = 1.0 / DENS( C,R,L )
               DO SPC = 1, NUMB_MECH_SPC
                  S = CGRID_INDEX( SPC )
                  IF( CONVERT_CONC( SPC ) )THEN
                      YC( SPC ) = REAL( MAX( CONCMIN,
     &                            INV_DENS*FORWARD_CONV( SPC )*CGRID( C,R,L,S )), 8 )
                  ELSE
                      YC( SPC ) = REAL( MAX( CGRID( C,R,L,S ), CONCMIN), 8 )
                  END IF
               END DO

               IF(CALL_DEG)THEN ! INITIALIZE degradation routines

                  YCCELL = 0.0D0
                  DO S = 1, MODEL_SPECIES
                     YCCELL( S ) = REAL( CGRID(C,R,L,S), 8)
                  END DO
                  CALL INIT_DEGRADE(YCCELL,TA(C,R,L),DENS(C,R,L),PRES(C,R,L),QV(C,R,L),RJIN(NCELL,:),
     &                              JDATE, JTIME)

#ifdef isam
                  CALL SA_DEGRADE_EXTRACT( C,R,L,DENS(C,R,L) )
#endif
               END IF

c..Set physical quantities
               TEMP( NCELL )     = REAL( TA( C,R,L ), 8)
               DENSITY( NCELL )  = REAL( DENS( C,R,L ), 8)
!                PRESCELL( NCELL ) = REAL( PRES( C,R,L ), 8)
               ATMPRES( NCELL )  = PA2ATM * REAL( PRES( C,R,L ), 8)
               H2O( NCELL )      = REAL( MAX ( MAOMV * QV( C,R,L ), 0.0 ), 8)
               SEAWATER (NCELL)      = MAX ( 0.0D0, ( REAL( SEAWATER_ZONE( C,R ) - SEAICE (C,R) ,8) ) )

c..Get rate constants
               LSUNLIGHT = .FALSE.
               FORALL( NPH = 1:NPHOTAB ) RJIN( NCELL,NPH ) = REAL( RJ( C,R,L,NPH ), 8)
               IF( MAXVAL(RJIN) .GT. 0.0D0 ) LSUNLIGHT = .TRUE.

               FORALL ( NPH = 1:NHETERO )HET( NCELL,NPH ) = REAL( KHETERO( NPH,C,R,L ), 8)

               CALL CALC_RCONST( TEMP, ATMPRES, H2O, RJIN, HET, LSUNLIGHT, SEAWATER, RKI_SAV, NCELL )
               FORALL( NPH = 1:NRXNS )RKI( NPH ) = RKI_SAV( NCELL, NPH )

#ifdef isam
#if defined(isam) || defined(verbose_isam)
C...For diagnosing IRR calculations in log files
                IF( CHECK_ISAM )THEN
                   WRITE_CELL = .FALSE.
                   IF( C .EQ. MAX(1, NCOLS/2) .AND. R .EQ. MAX(1,NROWS/2) .AND. L .EQ. 1 )THEN
                        WRITE_CELL = .TRUE.
                        WRITE(LOGDEV,*)'WRITE_CELL = .TRUE.'
                   ELSE
                        WRITE_CELL = .FALSE.
                   END IF
                END IF
#endif
               CALL SA_IRR_EXTRACT( C, R, L, DENS( C,R,L ),YC )
#endif

c..Call EBI solver
               N_EBI_IT = 0

               CALL HRSOLVER( JDATE, JTIME, C, R, L )

#ifdef hrstats
               MXEBI  = MAX( MXEBI, N_EBI_IT )
               MNEBI  = MIN( MNEBI, N_EBI_IT )
               SUMEBI = SUMEBI + REAL( N_EBI_IT )
#endif


c..Update concentration array
               DO SPC = 1, NUMB_MECH_SPC
                  S = CGRID_INDEX( SPC )
                  IF( CONVERT_CONC( SPC ) )THEN
                      CGRID( C,R,L,S ) = REAL( REVERSE_CONV( SPC ) * DENS( C,R,L )
     &                                 * MAX( YC( SPC ), DCONMIN), 4)
                  ELSE
                      CGRID( C,R,L,S ) = REAL( MAX( YC( SPC ), DCONMIN), 4)
                  END IF
               END DO

               IF(CALL_DEG)THEN  ! WTH: update based on degrade routines
                  CALL FINAL_DEGRADE(YCCELL)
                  UPDATE_DEGRADED: DO SPC = 1, N_REACT
                     ISP = RXTANT_MAP( SPC )
                     IF( ISP .LE. 0 )CYCLE UPDATE_DEGRADED
                     DO S = 1, NUMB_MECH_SPC
                        IF(  CGRID_INDEX( S ) .EQ. ISP )CYCLE UPDATE_DEGRADED
                     END DO
                     CGRID(C,R,L,ISP) = REAL( YCCELL(ISP), 4)
                  END DO UPDATE_DEGRADED
#ifdef isam
                  CALL SA_DEGRADE_UPLOAD( C,R,L,DENS(C,R,L) )
#endif
               END IF

#ifdef isam
               CALL SA_IRR_UPLOAD( C, R, L, DENS( C,R,L ), YC )
#endif

c..update irrout arrays if needed
               IF ( LFLAGIRR( C,R,L ) )CALL PA_IRR_CELLENDF( C, R, L )
#ifdef sens
               DO RXN = 1, NRXNS
                  SRK( RXN ) = RKI( RXN )
                  IF ( HIGH ) THEN
                     IF ( ORDER1 (RXN ) ) THEN
                        SRK2( RXN ) = 0.0
                     ELSE
                        SRK2( RXN ) = RKI( RXN )
                     END IF
                  END IF
               END DO


               CALL SOLVE_DDM3D_CHEM( C,R,L,CHEMSTEP )
#endif

            END DO
         END DO
      END DO

!IVAI

! Species after gas-phase: all canopy & non-canopy grid cells
      o3_new  (:,:,:) = CGRID(:,:,:,4)

! Gas-phase tendency  ppmv sec-1
      o3_tend (:,:,:) = ( o3_new(:,:,:) -  o3_old(:,:,:)) /
     &                  REAL (CHEMSTEP, 4) ! ppm  sec-1

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  Top of loop over cells IF CANOPY SHADE
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF ( CANOPY_SHADE ) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      NCELL = 1

! Call on combined canopy plus resolved model layer
!     DO L = 1, NLAYS
      DO L = 1, NLAYT
         DO R = 1, NROWS
            DO C = 1, NCOLS

            IF ( FRT_MASK(C,R) > 0. ) THEN

               DEG_LAY = L
               DEG_ROW = R
               DEG_COL = C

c..reset CALL_IRR based on cell's values
               CALL_IRR = LFLAGIRR( C,R,L )

!IVAI:
!              if( L.EQ.1)  print*, 'HRDRIVER: 1HY = ',
!    &            RJ( C,R,1 ,LO3O1D ), CGRID( C,R,1, 4 )         ! O3 = 4
! NB. CHEMSTEP = 3 min TSTEP = 300 (1hr)
!              if( L.EQ.1 .OR. L.EQ.2 .OR. L.EQ.3 ) print*,
!    &           'HRDRIVER: CANLEV3 =', L,
!    &            RJ_CAN( C,R,L ,LO3O1D ), CONC_CAN( C,R,L, 4 ) ! O3 = 4
!    &            KHETERO_CAN(NHETERO,C,R,L ) ! REAL( 8 )
!    &            TA_CAN(C,R,L), DENS_CAN(C,R,L), QV_CAN(C,R,L),
!IVAI

c..Load ICs
               INV_DENS = 1.0 / DENS_CAN( C,R,L )
               DO SPC = 1, NUMB_MECH_SPC
                  S = CGRID_INDEX( SPC )
                  IF( CONVERT_CONC( SPC ) )THEN
                      YC( SPC ) = REAL( MAX( CONCMIN,
     &                            INV_DENS*FORWARD_CONV( SPC )*CONC_CAN( C,R,L,S )), 8 )
                  ELSE
                      YC( SPC ) = REAL( MAX( CONC_CAN( C,R,L,S ), CONCMIN), 8 )
                  END IF
               END DO

               IF(CALL_DEG)THEN ! INITIALIZE degradation routines

                  YCCELL = 0.0D0
                  DO S = 1, MODEL_SPECIES
                     YCCELL( S ) = REAL( CONC_CAN(C,R,L,S), 8)
                  END DO
                  CALL INIT_DEGRADE(YCCELL,TA_CAN(C,R,L),DENS_CAN(C,R,L),PRES_CAN(C,R,L),QV_CAN(C,R,L),RJIN(NCELL,:),
     &                              JDATE, JTIME)

#ifdef isam
                  CALL SA_DEGRADE_EXTRACT( C,R,L,DENS_CAN(C,R,L) )
#endif
               END IF

c..Set physical quantities
               TEMP( NCELL )     = REAL( TA_CAN( C,R,L ), 8)
               DENSITY( NCELL )  = REAL( DENS_CAN( C,R,L ), 8)
!                PRESCELL( NCELL ) = REAL( PRES_CAN( C,R,L ), 8)
               ATMPRES( NCELL )  = PA2ATM * REAL( PRES_CAN( C,R,L ), 8)
               H2O( NCELL )      = REAL( MAX ( MAOMV * QV_CAN( C,R,L ), 0.0 ), 8)
               SEAWATER (NCELL)      = MAX ( 0.0D0, ( REAL( SEAWATER_ZONE( C,R ) - SEAICE (C,R) ,8) ) )

c..Get rate constants
               LSUNLIGHT = .FALSE.
               FORALL( NPH = 1:NPHOTAB ) RJIN( NCELL,NPH ) = REAL( RJ_CAN( C,R,L,NPH ), 8)
               IF( MAXVAL(RJIN) .GT. 0.0D0 ) LSUNLIGHT = .TRUE.

               FORALL ( NPH = 1:NHETERO )HET( NCELL,NPH ) = REAL( KHETERO_CAN( NPH,C,R,L ), 8)

               CALL CALC_RCONST( TEMP, ATMPRES, H2O, RJIN, HET, LSUNLIGHT, SEAWATER, RKI_SAV, NCELL )
               FORALL( NPH = 1:NRXNS )RKI( NPH ) = RKI_SAV( NCELL, NPH )

#ifdef isam
#if defined(isam) || defined(verbose_isam)
C...For diagnosing IRR calculations in log files
                IF( CHECK_ISAM )THEN
                   WRITE_CELL = .FALSE.
                   IF( C .EQ. MAX(1, NCOLS/2) .AND. R .EQ. MAX(1,NROWS/2) .AND. L .EQ. 1 )THEN
                        WRITE_CELL = .TRUE.
                        WRITE(LOGDEV,*)'WRITE_CELL = .TRUE.'
                   ELSE
                        WRITE_CELL = .FALSE.
                   END IF
                END IF
#endif
               CALL SA_IRR_EXTRACT( C, R, L, DENS_CAN( C,R,L ),YC )
#endif

c..Call EBI solver
               N_EBI_IT = 0

               CALL HRSOLVER( JDATE, JTIME, C, R, L )

#ifdef hrstats
               MXEBI  = MAX( MXEBI, N_EBI_IT )
               MNEBI  = MIN( MNEBI, N_EBI_IT )
               SUMEBI = SUMEBI + REAL( N_EBI_IT )
#endif


c..Update concentration array
               DO SPC = 1, NUMB_MECH_SPC
                  S = CGRID_INDEX( SPC )
                  IF( CONVERT_CONC( SPC ) )THEN
                      CONC_CAN( C,R,L,S ) = REAL( REVERSE_CONV( SPC ) * DENS_CAN( C,R,L )
     &                                 * MAX( YC( SPC ), DCONMIN), 4)
                  ELSE
                      CONC_CAN( C,R,L,S ) = REAL( MAX( YC( SPC ), DCONMIN), 4)
                  END IF
               END DO

               IF(CALL_DEG)THEN  ! WTH: update based on degrade routines
                  CALL FINAL_DEGRADE(YCCELL)
                  UPDATE_DEGRADED_CAN: DO SPC = 1, N_REACT
                     ISP = RXTANT_MAP( SPC )
                     IF( ISP .LE. 0 )CYCLE UPDATE_DEGRADED_CAN
                     DO S = 1, NUMB_MECH_SPC
                        IF(  CGRID_INDEX( S ) .EQ. ISP )CYCLE UPDATE_DEGRADED_CAN
                     END DO
                     CONC_CAN(C,R,L,ISP) = REAL( YCCELL(ISP), 4)
                  END DO UPDATE_DEGRADED_CAN
#ifdef isam
                  CALL SA_DEGRADE_UPLOAD( C,R,L,DENS_CAN(C,R,L) )
#endif
               END IF

#ifdef isam
               CALL SA_IRR_UPLOAD( C, R, L, DENS_CAN( C,R,L ), YC )
#endif

c..update irrout arrays if needed
               IF ( LFLAGIRR( C,R,L ) )CALL PA_IRR_CELLENDF( C, R, L )
#ifdef sens
               DO RXN = 1, NRXNS
                  SRK( RXN ) = RKI( RXN )
                  IF ( HIGH ) THEN
                     IF ( ORDER1 (RXN ) ) THEN
                        SRK2( RXN ) = 0.0
                     ELSE
                        SRK2( RXN ) = RKI( RXN )
                     END IF
                  END IF
               END DO


               CALL SOLVE_DDM3D_CHEM( C,R,L,CHEMSTEP )
#endif

            ELSE    ! FRT_MASK
!
! Update non-canopy grid cells after gas-phase
!
            END IF  ! FRT_MASK

            END DO
         END DO
      END DO ! LOOP L = 1, NLAYT

! Species after gas-phase (NB. only canopy-grid cells are updated)
      o3_new_can (:,:,1:NLAYT) = CONC_CAN(:,:,1:NLAYT,4) ! O3 = 4
!Nov8:o3_new     (:,:,1:NLAYS) = CONC_CAN(:,:,1:NLAYS,4) ! O3 = 4

! Temporary output
!     aux2d_02(:,:) = o3_new_can  (:,:,1)
!     aux2d_02(:,:) = CONC_CAN(:,:,1,4) ! O3 = 4 => o3_new_can

      o3_tend_can (:,:,:) = ( o3_new_can(:,:,:) -  o3_old_can(:,:,:)) /
     &                       REAL (CHEMSTEP, 4) ! ppmv sec-1

! Temporary output

      aux2d_03(:,:) = o3_tend_can (:,:,3)          ! coszens
      aux2d_02(:,:) = o3_tend_can (:,:,1)          ! jno2
!     aux2d_01(:,:) = o3_tend_can (:,:,1)          ! jo3o1d

!     aux2d_03(:,:) = o3_tend     (:,:,3)          ! coszens
!     aux2d_02(:,:) = o3_tend     (:,:,2)          ! jno2
!     aux2d_01(:,:) = o3_tend     (:,:,1)          ! jo3o1d

!===============================================================================
! Gather tracer concentration from canopy layers into model resolved layers (flag = 1)
!===============================================================================

      CALL canopy_transfer(CGRID, JDATE, JTIME, 1 )

! Input:
!   CONC_CAN after gas-phase
!
! Output:
!   CONC_MOD after gas-phase

! Temporary output
!     aux2d_01(:,:) =  CONC_MOD(:,:,NLAYS,4) ! top model layer O3 = 4
!     aux2d_01(:,:) =  CONC_MOD(:,:,2,4) ! 2hy model layer O3 = 4
!     aux2d_01(:,:) =  CONC_MOD(:,:,1,4) ! 1hy model layer O3 = 4

! Update CGRID for advection, deposition etc.
      DO L = 1, NLAYS
         DO R = 1, MY_NROWS
            DO C = 1, MY_NCOLS

            KOUNT = 0
            IF ( FRT_MASK(C,R) > 0. ) THEN

            IF (.FALSE.) THEN
            IF ( KOUNT < 3 .AND. L == 1)  THEN

            DO SPC = 1, NUMB_MECH_SPC

               S = CGRID_INDEX( SPC )
               print*, 'HRDRIVER: AFTER C2R SPC= ', S,
! N_GAS_CHEM_SPC = 127 (NO2 = 1 O3 = 4)
!    &         CONC_CAN(C,R,1, S), CONC_CAN(C,R,2, S), CONC_CAN(C,R,3, S)  ! O3 = 4
     &         CGRID    (C,R,1, S), CONC_MOD(C,R,1, S)
            END DO

            END IF ! KOUNT L=1
            END IF !.FALSE.

! Print up to KOUNT number of canopy columns
            KOUNT = KOUNT + 1

! Update CGRID with values from canopy_transfer
            DO SPC = 1, NUMB_MECH_SPC
               S = CGRID_INDEX( SPC )

! All levels
               CGRID(C,R,L,S) = CONC_MOD(C,R,L,S)
            END DO

            END IF !FRT_MASK

            END DO
         END DO
      END DO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      END IF ! CANOPY_SHADE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!IVAI

!  Update Aerosol Surface Area
      CALL HETCHEM_UPDATE_AERO( CGRID )

#ifdef hrstats
      AVGEBI = SUMEBI / REAL( NCOLS * NROWS * NLAYS )
      WRITE( ISTFL, 94040 ) JDATE, JTIME, MNEBI, AVGEBI, MXEBI
#endif

      RETURN

C*********************** FORMAT STATEMENTS ****************************

92000 FORMAT( / 10X, 'Euler Backward Iterative Parameters -'
     &        / 10X, 'Chemistry Integration Time Interval (min):', F12.4,
     &        / 10X, 'EBI maximum time step (min):              ', F12.4 )

92020 FORMAT( //10X, 'Species convergence tolerances:' )

92040 FORMAT(   10X, A16, 2X, 1PE12.2 )

92060 FORMAT( / 10X, 'Emissions Processing in Chemistry ...'
     &        / 10X, 'Number of Emissions Layers:         ', I3
     &        / 10X, 'out of total Number of Model Layers:', I3 )


94020 FORMAT( 'DATE      TIME ', 'MNEBI AVEBI MXEBI' )

94040 FORMAT( I7, 1X, I6, 1X, 3( I5, 1X ) )
      END
