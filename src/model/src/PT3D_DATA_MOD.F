      MODULE PT3D_DATA_MOD

      IMPLICIT NONE

      LOGICAL           :: PT3DEMIS   ! flag in-lining plume rise
      INTEGER           :: NPTGRPS    ! no. pt src input file groups

      REAL, ALLOCATABLE :: VDEMIS_PT( :,:,:,: ) ! 3D pt src non-PM emis
      REAL, ALLOCATABLE :: VDEMIS_PT_FIRE( :,:,:,: ) ! 3D pt src non-PM emis
      REAL, ALLOCATABLE :: PMEMIS_PT( :,:,:,: ) ! 3D pt src PM emis

      CHARACTER( * ), PARAMETER :: CTM_PT3DEMIS = 'CTM_PT3DEMIS' ! env var for in-line 3d pt src emis

      END MODULE PT3D_DATA_MOD
