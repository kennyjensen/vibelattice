      PROGRAM BDFORC_REF
      INTEGER IU, L
      INCLUDE 'AVL.INC'
C
      PI = 3.14159265
      ALFA = 0.1
      MACH = 0.3
      SREF = 1.5
      CREF = 1.0
      BREF = 2.0
C
      XYZREF(1) = 0.0
      XYZREF(2) = 0.0
      XYZREF(3) = 0.0
C
      VINF(1) = 0.9
      VINF(2) = 0.1
      VINF(3) = 0.05
      WROT(1) = 0.01
      WROT(2) = -0.02
      WROT(3) = 0.03
C
      NBODY = 1
      NLNODE = 2
      NL(1) = 2
      LFRST(1) = 1
C
      RL(1,1) = 0.0
      RL(2,1) = 0.0
      RL(3,1) = 0.0
      RL(1,2) = 1.0
      RL(2,2) = 0.5
      RL(3,2) = 0.2
C
      RADL(1) = 0.10
      RADL(2) = 0.12
C
      SRC(1) = 0.8
      SRC(2) = 0.0
      DO IU = 1, NUMAX
        SRC_U(1,IU) = 0.01*FLOAT(IU)
        SRC_U(2,IU) = 0.0
      ENDDO
C
      CDTOT = 0.0
      CYTOT = 0.0
      CLTOT = 0.0
      DO L = 1, 3
        CFTOT(L) = 0.0
        CMTOT(L) = 0.0
      ENDDO
      DO IU = 1, NUMAX
        CDTOT_U(IU) = 0.0
        CYTOT_U(IU) = 0.0
        CLTOT_U(IU) = 0.0
        DO L = 1, 3
          CFTOT_U(L,IU) = 0.0
          CMTOT_U(L,IU) = 0.0
        ENDDO
      ENDDO
C
      CALL BDFORC
C
C     Output order:
C     CDBDY(1), CYBDY(1), CLBDY(1)
C     CFBDY(1..3,1), CMBDY(1..3,1)
C     DCPB(1..3,1)
C     CDTOT, CYTOT, CLTOT
C     CFTOT(1..3), CMTOT(1..3)
C     CDTOT_U(1..NUMAX), CYTOT_U(1..NUMAX), CLTOT_U(1..NUMAX)
C     CFTOT_U(1..3,1..NUMAX), CMTOT_U(1..3,1..NUMAX)
C
      WRITE(*,'(1X,ES15.7)') CDBDY(1)
      WRITE(*,'(1X,ES15.7)') CYBDY(1)
      WRITE(*,'(1X,ES15.7)') CLBDY(1)
      DO L = 1, 3
        WRITE(*,'(1X,ES15.7)') CFBDY(L,1)
      ENDDO
      DO L = 1, 3
        WRITE(*,'(1X,ES15.7)') CMBDY(L,1)
      ENDDO
      DO L = 1, 3
        WRITE(*,'(1X,ES15.7)') DCPB(L,1)
      ENDDO
      WRITE(*,'(1X,ES15.7)') CDTOT
      WRITE(*,'(1X,ES15.7)') CYTOT
      WRITE(*,'(1X,ES15.7)') CLTOT
      DO L = 1, 3
        WRITE(*,'(1X,ES15.7)') CFTOT(L)
      ENDDO
      DO L = 1, 3
        WRITE(*,'(1X,ES15.7)') CMTOT(L)
      ENDDO
      DO IU = 1, NUMAX
        WRITE(*,'(1X,ES15.7)') CDTOT_U(IU)
      ENDDO
      DO IU = 1, NUMAX
        WRITE(*,'(1X,ES15.7)') CYTOT_U(IU)
      ENDDO
      DO IU = 1, NUMAX
        WRITE(*,'(1X,ES15.7)') CLTOT_U(IU)
      ENDDO
      DO IU = 1, NUMAX
        DO L = 1, 3
          WRITE(*,'(1X,ES15.7)') CFTOT_U(L,IU)
        ENDDO
      ENDDO
      DO IU = 1, NUMAX
        DO L = 1, 3
          WRITE(*,'(1X,ES15.7)') CMTOT_U(L,IU)
        ENDDO
      ENDDO
C
      END
