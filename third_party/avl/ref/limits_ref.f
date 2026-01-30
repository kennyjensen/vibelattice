      PROGRAM LIMITS_REF
      INCLUDE 'AVL.INC'
      INCLUDE 'AVLPLT.INC'
      REAL XYZMIN(3), XYZMAX(3)
      REAL XYZMINR(3), XYZMAXR(3)
      REAL TT(3,3), XYZR(3), DXYZ(3)
      REAL ANG(3), POS(3)
      REAL RINV, XIHAT, YIHAT, ZIHAT, XJHAT, YJHAT, ZJHAT
      REAL XKHAT, YKHAT, ZKHAT
      COMMON /VIEWDATA/ RINV,
     &       XIHAT, YIHAT, ZIHAT,
     &       XJHAT, YJHAT, ZJHAT,
     &       XKHAT, YKHAT, ZKHAT
      INTEGER I
C
      NSURF = 1
      NBODY = 1
      NOB = 3
      LPLTSURF(1) = .TRUE.
      LPLTBODY(1) = .TRUE.
      LOBPLT = .TRUE.

      JFRST(1) = 1
      NJ(1) = 3

      RLE1(1,1) = 0.0
      RLE1(2,1) = 0.0
      RLE1(3,1) = 0.0
      RLE1(1,2) = 1.0
      RLE1(2,2) = 0.5
      RLE1(3,2) = 0.2
      RLE1(1,3) = 2.0
      RLE1(2,3) = 1.0
      RLE1(3,3) = 0.4

      RLE2(1,1) = 0.0
      RLE2(2,1) = 0.0
      RLE2(3,1) = 1.0
      RLE2(1,2) = 1.0
      RLE2(2,2) = 0.5
      RLE2(3,2) = 1.2
      RLE2(1,3) = 2.0
      RLE2(2,3) = 1.0
      RLE2(3,3) = 1.4

      CHORD1(1) = 1.0
      CHORD1(2) = 0.8
      CHORD1(3) = 0.6

      CHORD2(1) = 1.1
      CHORD2(2) = 0.9
      CHORD2(3) = 0.7

      LFRST(1) = 2
      NL(1) = 2

      RL(1,1) = 0.0
      RL(2,1) = 0.0
      RL(3,1) = 0.0
      RL(1,2) = -1.0
      RL(2,2) = -0.5
      RL(3,2) = 0.2
      RL(1,3) = 2.5
      RL(2,3) = 0.4
      RL(3,3) = -0.1

      ROB(1,1) = 0.2
      ROB(2,1) = 1.5
      ROB(3,1) = 0.0
      ROB(1,2) = 1.1
      ROB(2,2) = -2.0
      ROB(3,2) = 0.3
      ROB(1,3) = -0.5
      ROB(2,3) = 0.7
      ROB(3,3) = -0.2

      GMIN(1) = -1.0
      GMIN(2) = -2.0
      GMIN(3) = -0.2
      GMAX(1) = 2.6
      GMAX(2) = 1.5
      GMAX(3) = 1.4

      RINV = 0.0
      XIHAT = 1.0
      YIHAT = 0.0
      ZIHAT = 0.0
      XJHAT = 0.0
      YJHAT = 1.0
      ZJHAT = 0.0
      XKHAT = 0.0
      YKHAT = 0.0
      ZKHAT = 1.0

      TT(1,1) = 1.0
      TT(2,1) = 0.0
      TT(3,1) = 0.0
      TT(1,2) = 0.0
      TT(2,2) = 1.0
      TT(3,2) = 0.0
      TT(1,3) = 0.0
      TT(2,3) = 0.0
      TT(3,3) = 1.0

      XYZR(1) = 0.0
      XYZR(2) = 0.0
      XYZR(3) = 0.0
      DXYZ(1) = 0.5
      DXYZ(2) = -0.25
      DXYZ(3) = 1.0

      ANG(1) = 0.0
      ANG(2) = 0.0
      ANG(3) = 0.0
      POS(1) = 1.0
      POS(2) = 2.0
      POS(3) = 3.0

      CALL GLIMS(XYZMIN, XYZMAX, .FALSE.)
      CALL GRLIMS(XYZMINR, XYZMAXR, .FALSE., TT, XYZR, DXYZ)
      CALL AXLIMS

      NTRI = 0
      CALL HIDINIT(.TRUE.)
      WRITE(*,'(6(1X,ES15.7))') (XYZMIN(I), I=1,3), (XYZMAX(I), I=1,3)
      WRITE(*,'(6(1X,ES15.7))') (XYZMINR(I), I=1,3), (XYZMAXR(I), I=1,3)
      WRITE(*,'(12(1X,ES15.7))') (AXMIN(I), I=1,3), (AXMAX(I), I=1,3),
     &                           (AXSPAN(I), I=1,3), (AXDEL(I), I=1,3)
      WRITE(*,'(3(1X,I6))') (NAXANN(I), I=1,3)
      WRITE(*,'(1X,I6)') NTRI
      WRITE(*,'(16(1X,ES15.7))') (TRI(I,1), I=1,16)
      WRITE(*,'(16(1X,ES15.7))') (TRI(I,NTRI), I=1,16)

      NTRI = 0
      CALL HIDINITE(.TRUE., ANG, POS, XYZR)
      WRITE(*,'(1X,I6)') NTRI
      WRITE(*,'(16(1X,ES15.7))') (TRI(I,1), I=1,16)
      WRITE(*,'(16(1X,ES15.7))') (TRI(I,NTRI), I=1,16)

      END
