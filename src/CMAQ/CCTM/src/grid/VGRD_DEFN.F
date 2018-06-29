      MODULE VGRD_DEFN

      IMPLICIT NONE

      INTEGER, SAVE :: NLAYS = 0

      REAL, ALLOCATABLE, SAVE :: VGLVS_GD( : )  ! vert layer surface values
      REAL, ALLOCATABLE, SAVE :: X3FACE_GD( : ) ! mono. incr. vert coord values
                                                ! = 1 - VGLVS_GD
      CHARACTER( 16 ), SAVE :: GDNAME_GD

      INTEGER, SAVE :: VGTYP_GD

      CHARACTER( 16 ), SAVE :: VGTPUN_GD

      REAL, SAVE :: VGTOP_GD

      CHARACTER( 16 ) :: VGLVUN_GD

C-----------------------------------------------------------------------

      END MODULE VGRD_DEFN
