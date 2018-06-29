
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


C RCS file, release, date & time of last delta, author, state, [and locker]
C $Header: 

C what(1) key, module and SID; SCCS file; date and time of last delta:
C %W% %P% %G% %U%

C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      SUBROUTINE UNLOAD_CGRID ( CGRID )
!     SUBROUTINE UNLOAD_CGRID ( CGRID, scalars_in)
!     SUBROUTINE UNLOAD_CGRID ( CGRID, gc_species_in,
!    &                          ae_species_in, nr_species_in)

C-----------------------------------------------------------------------
C Function:

C Revision history:
C   David Wong - Mar 17 - initial implmentation
C-----------------------------------------------------------------------

      USE CGRID_SPCS            ! CGRID mechanism species
      USE HGRD_DEFN, ONLY : NCOLS, NROWS
      USE VGRD_DEFN, ONLY : NLAYS
      USE coupler_module, ONLY : my_gc_adj, my_ae_adj, my_nr_adj, cmaq_species

      IMPLICIT NONE

C Arguments:

      REAL, POINTER :: CGRID( :,:,:,: )  ! for initial CONC
!     real, intent(out) :: scalars_in(:,:,:)
!     real, intent(out) :: gc_species_in(:,:,:),
!    &                     ae_species_in(:,:,:),
!    &                     nr_species_in(:,:,:)

      LOGICAL, SAVE :: FIRSTIME = .TRUE.
      INTEGER :: C, R, L, V, LOGDEV, SPC

C-----------------------------------------------------------------------

      IF ( FIRSTIME ) THEN
         LOGDEV = 6
         FIRSTIME = .FALSE.
      END IF

      v = size(cmaq_species, 4)

      DO L = 1, NLAYS
         DO C = 1, NCOLS
            cmaq_species( c,1,l,1:v ) = CGRID( C,1,L,1:V )
         END DO
      END DO

      END SUBROUTINE UNLOAD_CGRID
