      PROGRAM BA_TRANS_REF
      IMPLICIT NONE
      INTEGER N, I, R, C
      REAL ALFA, BETA, BINV
      REAL P(3,3), P_A(3,3), P_B(3,3)
      REAL PS(3,3), PS_A(3,3)
C
C     Read number of test cases. Each case: alfa beta binv
C     Output: ba2wa_mat p, p_a, p_b then ba2sa_mat p, p_a
C
      READ(*,*,END=900) N
      DO I = 1, N
        READ(*,*,END=900) ALFA, BETA, BINV
        CALL BA2WA_MAT(ALFA,BETA,BINV,P,P_A,P_B)
        WRITE(*,'(9(1X,ES15.7))') ((P(R,C), C=1,3), R=1,3)
        WRITE(*,'(9(1X,ES15.7))') ((P_A(R,C), C=1,3), R=1,3)
        WRITE(*,'(9(1X,ES15.7))') ((P_B(R,C), C=1,3), R=1,3)
        CALL BA2SA_MAT(ALFA,PS,PS_A)
        WRITE(*,'(9(1X,ES15.7))') ((PS(R,C), C=1,3), R=1,3)
        WRITE(*,'(9(1X,ES15.7))') ((PS_A(R,C), C=1,3), R=1,3)
      END DO
C
  900 CONTINUE
      END
