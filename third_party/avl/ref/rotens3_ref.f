      PROGRAM ROTENS3_REF
      IMPLICIT NONE
      INTEGER N, I
      INTEGER R, C, K
      REAL A(3)
      REAL T(3,3), T_A(3,3,3)
C
C     Read number of test cases, then each 3-vector.
C
      READ(*,*,END=900) N
      DO I = 1, N
        READ(*,*,END=900) (A(K), K=1,3)
        CALL ROTENS3(A, T, T_A)
        WRITE(*,'(9(1X,ES15.7))') ((T(R,C), C=1,3), R=1,3)
        DO K = 1, 3
          WRITE(*,'(9(1X,ES15.7))') ((T_A(R,C,K), C=1,3), R=1,3)
        END DO
      END DO
C
  900 CONTINUE
      END
