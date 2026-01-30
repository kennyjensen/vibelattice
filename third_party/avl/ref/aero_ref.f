      PROGRAM AERO_REF
      INTEGER I, N, L
      INCLUDE 'AVL.INC'
C
      PI = 3.14159265
      ALFA = 0.1
      BETA = 0.05
      MACH = 0.3
      AMACH = MACH
      IYSYM = 0
      IZSYM = 0
      YSYM = 0.0
      ZSYM = 0.0
      VRCOREC = 0.01
      VRCOREW = 0.02
      SREF = 1.5
      CREF = 1.0
      BREF = 2.0
      CDREF = 0.02
      XYZREF(1) = 0.0
      XYZREF(2) = 0.0
      XYZREF(3) = 0.0
C
      NSTRIP = 1
      NVOR = 1
      NSURF = 1
      NBODY = 0
      NCONTROL = 0
      NDESIGN = 0
C
      LTRFORCE = .FALSE.
      LNFLD_WV = .FALSE.
      LVISC = .FALSE.
      LVISCSTRP(1) = .FALSE.
C
      IJFRST(1) = 1
      NVSTRP(1) = 1
      JFRST(1) = 1
      NJ(1) = 1
      LSSURF(1) = 1
      IMAGS(1) = 1
      LFLOAD(1) = .TRUE.
      LNCOMP(1) = 1
C
      CHORD(1) = 1.0
      WSTRIP(1) = 0.5
      CHORD1(1) = 1.0
      CHORD2(1) = 1.0
C
      RLE1(1,1) = 0.0
      RLE1(2,1) = 0.0
      RLE1(3,1) = 0.0
      RLE2(1,1) = 0.0
      RLE2(2,1) = 1.0
      RLE2(3,1) = 0.0
      RLE(1,1) = 0.0
      RLE(2,1) = 0.0
      RLE(3,1) = 0.0
C
      ENSY(1) = 0.0
      ENSZ(1) = 1.0
      ESS(1,1) = 0.0
      ESS(2,1) = 1.0
      ESS(3,1) = 0.0
      AINC(1) = 0.0
      XSREF(1) = 0.25
      YSREF(1) = 0.0
      ZSREF(1) = 0.0
      SSURF(1) = CHORD(1)*WSTRIP(1)
      CAVESURF(1) = CHORD(1)
C
      RV1(1,1) = 0.0
      RV1(2,1) = -0.5
      RV1(3,1) = 0.0
      RV2(1,1) = 1.0
      RV2(2,1) = 0.5
      RV2(3,1) = 0.0
      RV(1,1) = 0.5
      RV(2,1) = 0.0
      RV(3,1) = 0.0
      RC(1,1) = 0.25
      RC(2,1) = 0.0
      RC(3,1) = 0.0
      DXV(1) = 1.0
      ENV(1,1) = 0.0
      ENV(2,1) = 0.0
      ENV(3,1) = 1.0
C
      VV(1,1) = 0.01
      VV(2,1) = 0.02
      VV(3,1) = 0.03
      DO L = 1, 3
        DO N = 1, NUMAX
          VV_U(L,1,N) = 0.0
        ENDDO
      ENDDO
C
      GAM(1) = 0.4
      DO N = 1, NUMAX
        GAM_U(1,N) = 0.01*FLOAT(N)
      ENDDO
C
      WROT(1) = 0.01
      WROT(2) = -0.02
      WROT(3) = 0.03
C
      CALL VINFAB
      CALL AERO
C
C     Output order:
C     CDTOT CYTOT CLTOT
C     CFTOT(1..3) CMTOT(1..3)
C     CDVTOT CLFF CYFF CDFF SPANEF
C     DCP(1) CNC(1)
C     CFSTRP(1..3,1) CMSTRP(1..3,1)
C     CDSTRP(1) CYSTRP(1) CLSTRP(1)
C     CDVSURF(1) CDSURF(1) CYSURF(1) CLSURF(1)
C
      WRITE(*,'(1X,ES15.7)') CDTOT
      WRITE(*,'(1X,ES15.7)') CYTOT
      WRITE(*,'(1X,ES15.7)') CLTOT
      DO L = 1, 3
        WRITE(*,'(1X,ES15.7)') CFTOT(L)
      ENDDO
      DO L = 1, 3
        WRITE(*,'(1X,ES15.7)') CMTOT(L)
      ENDDO
      WRITE(*,'(1X,ES15.7)') CDVTOT
      WRITE(*,'(1X,ES15.7)') CLFF
      WRITE(*,'(1X,ES15.7)') CYFF
      WRITE(*,'(1X,ES15.7)') CDFF
      WRITE(*,'(1X,ES15.7)') SPANEF
      WRITE(*,'(1X,ES15.7)') DCP(1)
      WRITE(*,'(1X,ES15.7)') CNC(1)
      DO L = 1, 3
        WRITE(*,'(1X,ES15.7)') CFSTRP(L,1)
      ENDDO
      DO L = 1, 3
        WRITE(*,'(1X,ES15.7)') CMSTRP(L,1)
      ENDDO
      WRITE(*,'(1X,ES15.7)') CDSTRP(1)
      WRITE(*,'(1X,ES15.7)') CYSTRP(1)
      WRITE(*,'(1X,ES15.7)') CLSTRP(1)
      WRITE(*,'(1X,ES15.7)') CDVSURF(1)
      WRITE(*,'(1X,ES15.7)') CDSURF(1)
      WRITE(*,'(1X,ES15.7)') CYSURF(1)
      WRITE(*,'(1X,ES15.7)') CLSURF(1)
C
      END
