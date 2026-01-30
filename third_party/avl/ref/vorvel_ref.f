      PROGRAM VORVEL_REF
      IMPLICIT NONE
      INTEGER N, I
      INTEGER IBOUND
      LOGICAL LBOUND
      REAL X,Y,Z,X1,Y1,Z1,X2,Y2,Z2,BETA
      REAL U,V,W
C
C     Read number of test cases. Each case:
C     x y z lbound x1 y1 z1 x2 y2 z2 beta
C
      READ(*,*,END=900) N
      DO I = 1, N
        READ(*,*,END=900) X,Y,Z,IBOUND,X1,Y1,Z1,X2,Y2,Z2,BETA
        LBOUND = IBOUND .NE. 0
        CALL VORVEL(X,Y,Z,LBOUND,X1,Y1,Z1,X2,Y2,Z2,BETA,U,V,W)
        WRITE(*,'(3(1X,ES15.7))') U, V, W
      END DO
C
  900 CONTINUE
      END
