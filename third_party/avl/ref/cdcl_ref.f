      PROGRAM CDCL_REF
      IMPLICIT NONE
      INTEGER N, I
      REAL CDCLPOL(6)
      REAL CL, CD, CD_CL
C
C     Read number of test cases. Each case: 6 cdclpol + cl
C
      READ(*,*,END=900) N
      DO I = 1, N
        READ(*,*,END=900) CDCLPOL(1),CDCLPOL(2),CDCLPOL(3),
     &                    CDCLPOL(4),CDCLPOL(5),CDCLPOL(6),CL
        CALL CDCL(CDCLPOL, CL, CD, CD_CL)
        WRITE(*,'(2(1X,ES15.7))') CD, CD_CL
      END DO
C
  900 CONTINUE
      END
