      PROGRAM AIC_REF
      IMPLICIT NONE
      INTEGER NV, NC, NCDIM
      INTEGER NBODY, NLDIM, NU
      REAL BETM, YSYM, ZSYM, VRCOREC, VRCOREW, SRCORE
      INTEGER IYSYM, IZSYM
      REAL RV1(3,2), RV2(3,2), CHORDV(2)
      REAL RC(3,2)
      INTEGER NCOMPV(2), NCOMPC(2)
      LOGICAL LVTEST
      REAL WC_GAM(3,2,2)
      REAL X, Y, Z, X1, Y1, Z1, X2, Y2, Z2, RCORE
      REAL U, V, W
      REAL UVWS(3), UVWD(3,3)
      INTEGER LFRST(1), NL(1)
      REAL RL(3,3), RADL(3)
      REAL SRC_U(3,6), DBL_U(3,3,6)
      REAL WC_U(3,2,6)
      REAL XYZREF(3)
      INTEGER I, J, K, IU, L
C
      BETM = 0.9
      IYSYM = 0
      YSYM = 0.2
      IZSYM = 0
      ZSYM = -0.1
      VRCOREC = 0.01
      VRCOREW = 0.02
      SRCORE = 0.1
C
      NV = 2
      NC = 2
      NCDIM = 2
C
      RV1(1,1) = 0.0
      RV1(2,1) = 0.0
      RV1(3,1) = 0.0
      RV2(1,1) = 1.0
      RV2(2,1) = 0.5
      RV2(3,1) = 0.2
C
      RV1(1,2) = 0.2
      RV1(2,2) = -0.3
      RV1(3,2) = 0.1
      RV2(1,2) = 1.2
      RV2(2,2) = 0.2
      RV2(3,2) = -0.1
C
      CHORDV(1) = 1.0
      CHORDV(2) = 0.8
      NCOMPV(1) = 1
      NCOMPV(2) = 2
C
      RC(1,1) = 0.5
      RC(2,1) = 0.1
      RC(3,1) = 0.05
      RC(1,2) = 0.8
      RC(2,2) = -0.1
      RC(3,2) = 0.2
      NCOMPC(1) = 1
      NCOMPC(2) = 2
      LVTEST = .TRUE.
C
      CALL VVOR(BETM,IYSYM,YSYM,IZSYM,ZSYM,
     &     VRCOREC,VRCOREW,
     &     NV,RV1,RV2,NCOMPV,CHORDV,
     &     NC,RC,NCOMPC,LVTEST,
     &     WC_GAM,NCDIM)
C
      X = 0.55
      Y = 0.15
      Z = -0.02
      X1 = RV1(1,1)
      Y1 = RV1(2,1)
      Z1 = RV1(3,1)
      X2 = RV2(1,1)
      Y2 = RV2(2,1)
      Z2 = RV2(3,1)
      RCORE = 0.03
      CALL VORVELC(X,Y,Z,.TRUE.,X1,Y1,Z1,X2,Y2,Z2,BETM,U,V,W,RCORE)
C
      CALL SRDVELC(X,Y,Z,X1,Y1,Z1,X2,Y2,Z2,BETM,0.05,UVWS,UVWD)
C
      NBODY = 1
      NLDIM = 3
      NU = 6
      LFRST(1) = 1
      NL(1) = 3
C
      RL(1,1) = 0.0
      RL(2,1) = 0.0
      RL(3,1) = 0.0
      RL(1,2) = 1.0
      RL(2,2) = 0.1
      RL(3,2) = 0.0
      RL(1,3) = 2.0
      RL(2,3) = 0.1
      RL(3,3) = 0.1
C
      RADL(1) = 0.2
      RADL(2) = 0.25
      RADL(3) = 0.3
      XYZREF(1) = 0.0
      XYZREF(2) = 0.0
      XYZREF(3) = 0.0
C
      CALL SRDSET(BETM,XYZREF,IYSYM,
     &            NBODY,LFRST,NLDIM,
     &            NL,RL,RADL,
     &            SRC_U,DBL_U)
C
      CALL VSRD(BETM,IYSYM,YSYM,IZSYM,ZSYM,SRCORE,
     &          NBODY,LFRST,NLDIM,
     &          NL,RL,RADL,
     &          NU,SRC_U,DBL_U,
     &          NC,RC,
     &          WC_U,NCDIM)
C
C     Output order:
C     1) VORVELC U,V,W
C     2) SRDVELC UVWS(1..3), UVWD(1..3,1..3) (K then J)
C     3) SRDSET SRC_U(L=1..2, IU=1..6) then DBL_U(K=1..3, L=1..2, IU=1..6)
C     4) VSRD WC_U(K=1..3, I=1..NC, IU=1..6) (I then IU then K)
C     5) VVOR WC_GAM(K=1..3, I=1..NC, J=1..NV) (I then J then K)
C
      WRITE(*,'(1X,ES15.7)') U
      WRITE(*,'(1X,ES15.7)') V
      WRITE(*,'(1X,ES15.7)') W
      DO K = 1, 3
        WRITE(*,'(1X,ES15.7)') UVWS(K)
      ENDDO
      DO K = 1, 3
        DO J = 1, 3
          WRITE(*,'(1X,ES15.7)') UVWD(K,J)
        ENDDO
      ENDDO
      DO L = 1, 2
        DO IU = 1, 6
          WRITE(*,'(1X,ES15.7)') SRC_U(L,IU)
        ENDDO
      ENDDO
      DO K = 1, 3
        DO L = 1, 2
          DO IU = 1, 6
            WRITE(*,'(1X,ES15.7)') DBL_U(K,L,IU)
          ENDDO
        ENDDO
      ENDDO
      DO I = 1, NC
        DO IU = 1, 6
          DO K = 1, 3
            WRITE(*,'(1X,ES15.7)') WC_U(K,I,IU)
          ENDDO
        ENDDO
      ENDDO
      DO I = 1, NC
        DO J = 1, NV
          DO K = 1, 3
            WRITE(*,'(1X,ES15.7)') WC_GAM(K,I,J)
          ENDDO
        ENDDO
      ENDDO
C
      END
