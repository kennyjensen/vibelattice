      PROGRAM SGUTIL_REF
      IMPLICIT NONE
      INTEGER N, I, NVC
      REAL X(6), Y(6), XN(6), XSP(6)
      REAL XX, YY, SLP, TRP
      REAL TRP1
      REAL PSPACE
      REAL CSPACE, CLAF
      REAL XPT(5), XVR(4), XSR(4), XCP(4)
C
      N = 6
      X(1) = 0.0
      X(2) = 1.0
      X(3) = 2.5
      X(4) = 3.0
      X(5) = 4.5
      X(6) = 6.0
C
      Y(1) = 0.0
      Y(2) = 1.2
      Y(3) = 0.8
      Y(4) = 1.6
      Y(5) = 1.1
      Y(6) = 2.0
C
      XX = 2.7
      CALL AKIMA(X,Y,N,XX,YY,SLP)
C
      TRP = TRP1(N,X,Y,3.4)
C
      DO I=1,N
        XN(I) = X(I)
      END DO
      CALL NRMLIZ(N,XN)
C
      PSPACE = 1.5
      CALL SPACER(N,PSPACE,XSP)
C
      NVC = 4
      CSPACE = 1.7
      CLAF = 0.25
      CALL CSPACER(NVC,CSPACE,CLAF,XPT,XVR,XSR,XCP)
C
      WRITE(*,'(1X,ES15.7)',ADVANCE='NO') YY
      WRITE(*,'(1X,ES15.7)',ADVANCE='NO') SLP
      WRITE(*,'(1X,ES15.7)',ADVANCE='NO') TRP
      DO I=1,N
        WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XN(I)
      END DO
      DO I=1,N
        WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XSP(I)
      END DO
      DO I=1,NVC+1
        WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XPT(I)
      END DO
      DO I=1,NVC
        WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XVR(I)
      END DO
      DO I=1,NVC
        WRITE(*,'(1X,ES15.7)',ADVANCE='NO') XSR(I)
      END DO
      DO I=1,NVC
        WRITE(*,'(1X,ES15.7)') XCP(I)
      END DO
C
      END
