      PROGRAM ATPFORC_REF
      INTEGER I, N
      INCLUDE 'AVL.INC'
C
      PI = 3.14159265
      AMACH = 0.3
      YSYM = 0.2
      ZSYM = -0.1
      IYSYM = 0
      IZSYM = 0
      VRCOREC = 0.01
      VRCOREW = 0.02
      SREF = 1.5
      BREF = 2.0
C
      NSTRIP = 2
      NCONTROL = 0
      NDESIGN = 0
C
      LSSURF(1) = 1
      LSSURF(2) = 1
      LNCOMP(1) = 1
      LFLOAD(1) = .TRUE.
C
      IJFRST(1) = 1
      NVSTRP(1) = 1
      IJFRST(2) = 2
      NVSTRP(2) = 1
C
      CHORD(1) = 1.0
      CHORD(2) = 0.8
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
      RC(1,1) = 0.5
      RC(2,1) = 0.1
      RC(3,1) = 0.05
      RC(1,2) = 0.8
      RC(2,2) = -0.1
      RC(3,2) = 0.2
C
      GAM(1) = 0.5
      GAM(2) = 0.3
      DO I = 1, 2
        DO N = 1, NUMAX
          GAM_U(I,N) = 0.05*FLOAT(I+N)
        ENDDO
      ENDDO
C
      DO I = 1, 2
        DO N = 1, NDMAX
          GAM_D(I,N) = 0.0
        ENDDO
      ENDDO
      DO I = 1, 2
        DO N = 1, NGMAX
          GAM_G(I,N) = 0.0
        ENDDO
      ENDDO
C
      CALL TPFORC
C
C     Output order: CLFF CYFF CDFF SPANEF
C     DWWAKE(1..NSTRIP)
C     CLFF_U(1..NUMAX), CYFF_U(1..NUMAX), CDFF_U(1..NUMAX), SPANEF_U(1..NUMAX)
C
      WRITE(*,'(1X,ES15.7)') CLFF
      WRITE(*,'(1X,ES15.7)') CYFF
      WRITE(*,'(1X,ES15.7)') CDFF
      WRITE(*,'(1X,ES15.7)') SPANEF
      DO I = 1, NSTRIP
        WRITE(*,'(1X,ES15.7)') DWWAKE(I)
      ENDDO
      DO N = 1, NUMAX
        WRITE(*,'(1X,ES15.7)') CLFF_U(N)
      ENDDO
      DO N = 1, NUMAX
        WRITE(*,'(1X,ES15.7)') CYFF_U(N)
      ENDDO
      DO N = 1, NUMAX
        WRITE(*,'(1X,ES15.7)') CDFF_U(N)
      ENDDO
      DO N = 1, NUMAX
        WRITE(*,'(1X,ES15.7)') SPANEF_U(N)
      ENDDO
C
      END
