      PROGRAM AOUTMRF_REF
      INCLUDE 'AVL.INC'
      INTEGER I, J
      INTEGER IR
      CHARACTER*80 FILEID
C
      IR = 1
      IRUN = 1
      FILEID = 'TESTFILE'
C
      PI = 3.1415927
      DTR = PI/180.0
      LNASA_SA = .FALSE.
      IYSYM = 0
      IZSYM = 0
      YSYM = 0.0
      ZSYM = 0.0
C
      NVOR = 1
      NSURF = 1
      NSTRIP = 1
      NBODY = 1
      NCONTROL = 1
      NDESIGN = 1
C
      TITLE = 'AOUTMRF REF'
      STITLE(1) = 'SURF1'
      BTITLE(1) = 'BODY'
      RTITLE(1) = 'RUN1'
      DNAME(1) = 'CTRL1'
      GNAME(1) = 'DES1'
C
      ALFA = 0.1
      BETA = 0.05
      AMACH = 0.2
      SREF = 1.5
      CREF = 0.75
      BREF = 2.0
      XYZREF(1) = 0.1
      XYZREF(2) = -0.2
      XYZREF(3) = 0.3
C
      WROT(1) = 0.01
      WROT(2) = -0.02
      WROT(3) = 0.03
C
      CFTOT(1) = 0.4
      CFTOT(2) = -0.1
      CFTOT(3) = 0.2
      CMTOT(1) = 0.05
      CMTOT(2) = -0.02
      CMTOT(3) = 0.04
      CLTOT = 0.6
      CDTOT = 0.08
      CYTOT = -0.03
      CDVTOT = 0.01
      CLFF = 0.58
      CDFF = 0.07
      CYFF = -0.02
      SPANEF = 0.85
C
      DELCON(1) = 2.0
      DELDES(1) = -1.0
C
      SSURF(1) = 1.1
      CAVESURF(1) = 0.8
      CLSURF(1) = 0.5
      CDSURF(1) = 0.09
      CYSURF(1) = -0.02
      CMSURF(1,1) = 0.01
      CMSURF(2,1) = -0.02
      CMSURF(3,1) = 0.03
      CDVSURF(1) = 0.015
      CL_LSRF(1) = 0.45
      CD_LSRF(1) = 0.05
C
      NJ(1) = 1
      NK(1) = 1
      JFRST(1) = 1
      IMAGS(1) = 1
      IJFRST(1) = 1
      WSTRIP(1) = 0.4
      CHORD(1) = 0.9
      AINC(1) = 0.02
      RLE(1,1) = 0.0
      RLE(2,1) = 0.5
      RLE(3,1) = 0.1
      RLE1(1,1) = 0.0
      RLE1(2,1) = 0.45
      RLE1(3,1) = 0.1
      RLE2(1,1) = 0.1
      RLE2(2,1) = 0.55
      RLE2(3,1) = 0.1
C
      CL_LSTRP(1) = 0.4
      CD_LSTRP(1) = 0.03
      CDV_LSTRP(1) = 0.005
      CMLE_LSTRP(1) = -0.01
      CMC4_LSTRP(1) = -0.02
      CLT_LSTRP(1) = 0.35
      CN_LSTRP(1) = 0.5
      CA_LSTRP(1) = 0.1
      CNC(1) = 0.6
      DWWAKE(1) = 0.02
C
      RV1(1,1) = 0.0
      RV1(2,1) = 0.4
      RV1(3,1) = 0.1
      RV2(1,1) = 0.2
      RV2(2,1) = 0.6
      RV2(3,1) = 0.1
      DXV(1) = 0.15
      SLOPEC(1) = 0.02
      DCP(1) = -0.1
C
      ELBDY(1) = 1.2
      SRFBDY(1) = 0.9
      VOLBDY(1) = 0.3
      CLBDY(1) = 0.05
      CDBDY(1) = 0.01
      CMBDY(1,1) = 0.004
      CMBDY(2,1) = -0.005
      CMBDY(3,1) = 0.006
      CFBDY(1,1) = 0.02
      CFBDY(2,1) = -0.01
      CFBDY(3,1) = 0.015
C
      CHINGE(1) = -0.03
C
      WRITE(*,'(A)') 'BEGIN MRFTOT'
      CALL MRFTOT(6, FILEID)
      WRITE(*,'(A)') 'BEGIN MRFSURF'
      CALL MRFSURF(6)
      WRITE(*,'(A)') 'BEGIN MRFBODY'
      CALL MRFBODY(6)
      WRITE(*,'(A)') 'BEGIN MRFSTRP'
      CALL MRFSTRP(6)
      WRITE(*,'(A)') 'BEGIN MRFELE'
      CALL MRFELE(6)
      WRITE(*,'(A)') 'BEGIN MRFHINGE'
      CALL MRFHINGE(6)
      WRITE(*,'(A)') 'BEGIN MRFCNC'
      CALL MRFCNC(6)
      WRITE(*,'(A)') 'BEGIN MRFVM'
      CALL MRFVM(6)
      END


      SUBROUTINE GETSA(LSA,SATYPE,DIR)
      LOGICAL LSA
      CHARACTER*(*) SATYPE
C
      IF(LSA) THEN
       SATYPE = 'Standard axis orientation,  X fwd, Z down'
       DIR = -1.0
      ELSE
       SATYPE = 'Geometric axis orientation,  X aft, Z up  '
       DIR =  1.0
      ENDIF
      RETURN
      END


      SUBROUTINE STRIP(STRING,NS)
      CHARACTER*(*) STRING
      N = LEN(STRING)
      DO 10 K2=N, 1, -1
        IF(STRING(K2:K2).NE.' ') GO TO 11
   10 CONTINUE
      K2 = 0
   11 CONTINUE
      DO 20 K1=1, K2
        IF(STRING(K1:K1).NE.' ') GO TO 21
   20 CONTINUE
   21 CONTINUE
      NS = K2 - K1 + 1
      IF(NS.EQ.0) RETURN
      STRING(1:NS) = STRING(K1:K2)
      DO 30 K=NS+1, N
        STRING(K:K) = ' '
   30 CONTINUE
      RETURN
      END
