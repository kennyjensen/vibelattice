      PROGRAM M3INV_REF
      IMPLICIT NONE
      INTEGER N, I
      INTEGER R, C
      REAL A(3,3), AINV(3,3)
C
C     Read number of test cases, then each 3x3 matrix (row-major input).
C
      READ(*,*,END=900) N
      DO I = 1, N
        READ(*,*,END=900) ((A(R,C), C=1,3), R=1,3)
        CALL M3INV(A, AINV)
        WRITE(*,'(9(1X,ES15.7))') ((AINV(R,C), C=1,3), R=1,3)
      END DO
C
  900 CONTINUE
      END
