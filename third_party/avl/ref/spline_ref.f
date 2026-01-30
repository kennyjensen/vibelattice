      PROGRAM SPLINE_REF
      IMPLICIT NONE
      INTEGER N, I
      REAL X(5), S(5), XS(5), XS2(5), XS3(5)
      REAL Y(5), YS(5)
      REAL SS, SE, DE, D2, XX, XXS, XXSS, CV
      REAL SEVAL, DEVAL, D2VAL, CURV
C
      N = 5
      S(1) = 0.0
      S(2) = 0.7
      S(3) = 1.4
      S(4) = 2.2
      S(5) = 3.0
C
      X(1) = 0.0
      X(2) = 1.0
      X(3) = 0.5
      X(4) = 1.5
      X(5) = 1.0
C
      Y(1) = 0.0
      Y(2) = 0.2
      Y(3) = 0.8
      Y(4) = 0.6
      Y(5) = 1.2
C
      CALL SPLINE(X,XS,S,N)
      CALL SPLIND(X,XS2,S,N,999.0,-999.0)
      CALL SPLINA(X,XS3,S,N)
C
      SS = 1.05
      SE = SEVAL(SS,X,XS,S,N)
      DE = DEVAL(SS,X,XS,S,N)
      D2 = D2VAL(SS,X,XS,S,N)
      CALL SEVALL(SS,X,XS,S,N,XX,XXS,XXSS)
C
      CALL SPLINE(Y,YS,S,N)
      CV = CURV(SS,X,XS,Y,YS,S,N)
C
      DO I=1,N
        WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XS(I)
      END DO
      DO I=1,N
        WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XS2(I)
      END DO
      DO I=1,N
        WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XS3(I)
      END DO
      WRITE(*,'(1X,ES15.7)',ADVANCE='NO') SE
      WRITE(*,'(1X,ES15.7)',ADVANCE='NO') DE
      WRITE(*,'(1X,ES15.7)',ADVANCE='NO') D2
      WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XX
      WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XXS
      WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XXSS
      WRITE(*,'(1X,ES15.7)') CV
C
      END
