      PROGRAM AIRUTIL_REF
      IMPLICIT NONE
      INTEGER N, NC, I
      REAL X(7), Y(7), XC(20), YC(20), TC(20)
      LOGICAL LNORM
C
      N = 7
      X(1) = 1.0
      X(2) = 0.8
      X(3) = 0.4
      X(4) = 0.0
      X(5) = 0.4
      X(6) = 0.8
      X(7) = 1.0
C
      Y(1) = 0.0
      Y(2) = 0.05
      Y(3) = 0.08
      Y(4) = 0.0
      Y(5) = -0.08
      Y(6) = -0.05
      Y(7) = 0.0
C
      NC = 10
      LNORM = .TRUE.
      CALL GETCAM(X,Y,N,XC,YC,TC,NC,LNORM)
C
      DO I=1, NC
        WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XC(I)
      END DO
      DO I=1, NC
        WRITE(*,'(1X,ES15.7)',ADVANCE='NO') YC(I)
      END DO
      DO I=1, NC
        WRITE(*,'(1X,ES15.7)',ADVANCE='NO') TC(I)
      END DO
      WRITE(*,*)
C
      END
