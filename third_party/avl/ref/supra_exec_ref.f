      PROGRAM SUPRA_EXEC_REF
      USE avl_heap_inc
      INCLUDE 'AVL.INC'
      LOGICAL ERROR
      INTEGER IR, J, K, N, IS, IOFF, NITER
      CHARACTER*256 FILAVL
      CHARACTER*32 ARG2
      CHARACTER*32 ARGS(40)
      INTEGER IARG
      INTEGER KALFA, KBETA, KPB2V, KQC2V, KRB2V
      INTEGER KD1, KD2, KD3, KD4
      REAL*8 DALFA, DBETA, DCL, DCD0, DBANK, DVEL, DRHO, DGEE
      REAL*8 DXCG, DYCG, DZCG, DMASS, DIXX, DIYY, DIZZ
      REAL*8 DIXY, DIYZ, DIZX
      REAL*8 DPB2V, DQC2V, DRB2V, DD1, DD2, DD3, DD4
      REAL*8 DIR, CA, SA, RX, RY, RZ, WROT_A(3), CL_AL, CM_AL, XNP

      PI = 4.0*ATAN(1.0)
      DTR = PI/180.0

      LUINP = 4

      CALL DEFINI
      LNASA_SA = .FALSE.
      LSA_RATES = .FALSE.
      LVISC = .FALSE.
      LBFORCE = .FALSE.
      CALL MASINI
      CALL AVLHEAP_INIT

      CALL GETARG0(1,FILAVL)
      IF(FILAVL .EQ. ' ') THEN
        FILAVL = 'supra.avl'
      ENDIF

      NITER = NITMAX
      CALL GETARG0(2,ARG2)
      IF(ARG2 .NE. ' ') THEN
        READ(ARG2,*,ERR=10) NITER
      ENDIF
 10   CONTINUE

      DO IARG = 1, 40
        ARGS(IARG) = ' '
        CALL GETARG0(IARG+2, ARGS(IARG))
      END DO

      DALFA = 0.0D0
      DBETA = 0.0D0
      DCL = 0.0D0
      DCD0 = 0.0D0
      DBANK = 0.0D0
      DVEL = 1.0D0
      DRHO = 1.0D0
      DGEE = 1.0D0
      DXCG = 0.0D0
      DYCG = 0.0D0
      DZCG = 0.0D0
      DMASS = 1.0D0
      DIXX = 0.0D0
      DIYY = 0.0D0
      DIZZ = 0.0D0
      DIXY = 0.0D0
      DIYZ = 0.0D0
      DIZX = 0.0D0
      DPB2V = 0.0D0
      DQC2V = 0.0D0
      DRB2V = 0.0D0
      DD1 = 0.0D0
      DD2 = 0.0D0
      DD3 = 0.0D0
      DD4 = 0.0D0
      KALFA = 1
      KBETA = 2
      KPB2V = 3
      KQC2V = 4
      KRB2V = 5
      KD1 = 101
      KD2 = 8
      KD3 = 9
      KD4 = 10

      IF(ARGS(1) .NE. ' ') READ(ARGS(1),*,ERR=11) DALFA
 11   CONTINUE
      IF(ARGS(2) .NE. ' ') READ(ARGS(2),*,ERR=12) DBETA
 12   CONTINUE
      IF(ARGS(3) .NE. ' ') READ(ARGS(3),*,ERR=13) DCL
 13   CONTINUE
      IF(ARGS(4) .NE. ' ') READ(ARGS(4),*,ERR=14) DCD0
 14   CONTINUE
      IF(ARGS(5) .NE. ' ') READ(ARGS(5),*,ERR=15) DBANK
 15   CONTINUE
      IF(ARGS(6) .NE. ' ') READ(ARGS(6),*,ERR=16) DVEL
 16   CONTINUE
      IF(ARGS(7) .NE. ' ') READ(ARGS(7),*,ERR=17) DRHO
 17   CONTINUE
      IF(ARGS(8) .NE. ' ') READ(ARGS(8),*,ERR=18) DGEE
 18   CONTINUE
      IF(ARGS(9) .NE. ' ') READ(ARGS(9),*,ERR=19) DXCG
 19   CONTINUE
      IF(ARGS(10) .NE. ' ') READ(ARGS(10),*,ERR=20) DYCG
 20   CONTINUE
      IF(ARGS(11) .NE. ' ') READ(ARGS(11),*,ERR=21) DZCG
 21   CONTINUE
      IF(ARGS(12) .NE. ' ') READ(ARGS(12),*,ERR=22) DMASS
 22   CONTINUE
      IF(ARGS(13) .NE. ' ') READ(ARGS(13),*,ERR=23) DIXX
 23   CONTINUE
      IF(ARGS(14) .NE. ' ') READ(ARGS(14),*,ERR=24) DIYY
 24   CONTINUE
      IF(ARGS(15) .NE. ' ') READ(ARGS(15),*,ERR=25) DIZZ
 25   CONTINUE
      IF(ARGS(16) .NE. ' ') READ(ARGS(16),*,ERR=26) DIXY
 26   CONTINUE
      IF(ARGS(17) .NE. ' ') READ(ARGS(17),*,ERR=27) DIYZ
 27   CONTINUE
      IF(ARGS(18) .NE. ' ') READ(ARGS(18),*,ERR=28) DIZX
 28   CONTINUE
      IF(ARGS(19) .NE. ' ') READ(ARGS(19),*,ERR=29) DPB2V
 29   CONTINUE
      IF(ARGS(20) .NE. ' ') READ(ARGS(20),*,ERR=30) DQC2V
 30   CONTINUE
      IF(ARGS(21) .NE. ' ') READ(ARGS(21),*,ERR=31) DRB2V
 31   CONTINUE
      IF(ARGS(22) .NE. ' ') READ(ARGS(22),*,ERR=32) DD1
 32   CONTINUE
      IF(ARGS(23) .NE. ' ') READ(ARGS(23),*,ERR=33) DD2
 33   CONTINUE
      IF(ARGS(24) .NE. ' ') READ(ARGS(24),*,ERR=34) DD3
 34   CONTINUE
      IF(ARGS(25) .NE. ' ') READ(ARGS(25),*,ERR=35) DD4
 35   CONTINUE
      IF(ARGS(26) .NE. ' ') READ(ARGS(26),*,ERR=36) KALFA
 36   CONTINUE
      IF(ARGS(27) .NE. ' ') READ(ARGS(27),*,ERR=37) KBETA
 37   CONTINUE
      IF(ARGS(28) .NE. ' ') READ(ARGS(28),*,ERR=38) KPB2V
 38   CONTINUE
      IF(ARGS(29) .NE. ' ') READ(ARGS(29),*,ERR=39) KQC2V
 39   CONTINUE
      IF(ARGS(30) .NE. ' ') READ(ARGS(30),*,ERR=40) KRB2V
 40   CONTINUE
      IF(ARGS(31) .NE. ' ') READ(ARGS(31),*,ERR=41) KD1
 41   CONTINUE
      IF(ARGS(32) .NE. ' ') READ(ARGS(32),*,ERR=42) KD2
 42   CONTINUE
      IF(ARGS(33) .NE. ' ') READ(ARGS(33),*,ERR=43) KD3
 43   CONTINUE
      IF(ARGS(34) .NE. ' ') READ(ARGS(34),*,ERR=44) KD4
 44   CONTINUE

      CALL INPUT(LUINP,FILAVL,ERROR)
      IF(ERROR) THEN
        WRITE(*,*) 'ERROR: INPUT failed for ', FILAVL
        STOP
      ENDIF

      CALL PARSET
      CALL ENCALC
      CALL VARINI

      IR = 1
      ICON(IVALFA,IR) = KEYTOIC(KALFA, ICTOT)
      ICON(IVBETA,IR) = KEYTOIC(KBETA, ICTOT)
      ICON(IVROTX,IR) = KEYTOIC(KPB2V, ICTOT)
      ICON(IVROTY,IR) = KEYTOIC(KQC2V, ICTOT)
      ICON(IVROTZ,IR) = KEYTOIC(KRB2V, ICTOT)
      ICON(IVTOT+1,IR) = KEYTOIC(KD1, ICTOT)
      ICON(IVTOT+2,IR) = KEYTOIC(KD2, ICTOT)
      ICON(IVTOT+3,IR) = KEYTOIC(KD3, ICTOT)
      ICON(IVTOT+4,IR) = KEYTOIC(KD4, ICTOT)

      PARVAL(IPMACH,IR) = 0.0
      PARVAL(IPVEE,IR) = DVEL
      PARVAL(IPRHO,IR) = DRHO
      PARVAL(IPGEE,IR) = DGEE
      PARVAL(IPCL,IR) = DCL
      PARVAL(IPPHI,IR) = DBANK
      PARVAL(IPXCG,IR) = DXCG
      PARVAL(IPYCG,IR) = DYCG
      PARVAL(IPZCG,IR) = DZCG
      PARVAL(IPCD0,IR) = DCD0
      PARVAL(IPMASS,IR) = DMASS
      PARVAL(IPIXX,IR) = DIXX
      PARVAL(IPIYY,IR) = DIYY
      PARVAL(IPIZZ,IR) = DIZZ
      PARVAL(IPIXY,IR) = -DIXY
      PARVAL(IPIYZ,IR) = -DIYZ
      PARVAL(IPIZX,IR) = -DIZX

      ALFA = DALFA*DTR
      BETA = DBETA*DTR
      MACH = PARVAL(IPMACH,IR)
      AMACH = MACH

      CONVAL(ICALFA,IR) = DALFA
      CONVAL(ICBETA,IR) = DBETA
      CONVAL(ICROTX,IR) = DPB2V
      CONVAL(ICROTY,IR) = DQC2V
      CONVAL(ICROTZ,IR) = DRB2V
      CONVAL(ICCL,IR) = DCL
      CONVAL(ICTOT+1,IR) = DD1
      CONVAL(ICTOT+2,IR) = DD2
      CONVAL(ICTOT+3,IR) = DD3
      CONVAL(ICTOT+4,IR) = DD4
      CONVAL(ICMOMX,IR) = DD2
      CONVAL(ICMOMY,IR) = DD3
      CONVAL(ICMOMZ,IR) = DD4

      CALL EXEC(NITER,0,IR)

      IF(LNASA_SA) THEN
        DIR = -1.0
      ELSE
        DIR =  1.0
      ENDIF
      CALL VINFAB
      CA = COS(ALFA)
      SA = SIN(ALFA)
      RX = (WROT(1)*CA + WROT(3)*SA) * DIR
      RY =  WROT(2)
      RZ = (WROT(3)*CA - WROT(1)*SA) * DIR
      WROT_A(1)  = -RX*SA - RZ*CA
      WROT_A(2)  =  0.0
      WROT_A(3)  = -RZ*SA + RX*CA

      CL_AL = CLTOT_U(1)*VINF_A(1) + CLTOT_U(4)*WROT_A(1)
     &      + CLTOT_U(2)*VINF_A(2) + CLTOT_U(5)*WROT_A(2)
     &      + CLTOT_U(3)*VINF_A(3) + CLTOT_U(6)*WROT_A(3) + CLTOT_A
      CM_AL = CMTOT_U(2,1)*VINF_A(1) + CMTOT_U(2,4)*WROT_A(1)
     &      + CMTOT_U(2,2)*VINF_A(2) + CMTOT_U(2,5)*WROT_A(2)
     &      + CMTOT_U(2,3)*VINF_A(3) + CMTOT_U(2,6)*WROT_A(3)
      IF(CL_AL .NE. 0.0) THEN
        XNP = XYZREF(1) - CREF*CM_AL/CL_AL
      ELSE
        XNP = -1.0E30
      ENDIF

      WRITE(*,'(A,1X,3(ES23.15E3))') 'GAMU0_1', GAM_U_0(1,1),
     &  GAM_U_0(1,2), GAM_U_0(1,3)
      WRITE(*,'(A,1X,3(ES23.15E3))') 'AICN1',
     &  AICN(1,1), AICN(1,2), AICN(1,3)
      WRITE(*,'(A,1X,3(ES23.15E3))') 'ENC1',
     &  ENC(1,1), ENC(2,1), ENC(3,1)

      WRITE(*,'(A,1X,9(ES23.15E3))') 'FORCE',
     &  CLTOT, CDTOT, CYTOT,
     &  CMTOT(1), CMTOT(2), CMTOT(3),
     &  CFTOT(1), CFTOT(2), CFTOT(3)
      WRITE(*,'(A,1X,ES23.15E3)') 'CDVTOT', CDVTOT
      WRITE(*,'(A,1X,I8)') 'NVOR', NVOR
      WRITE(*,'(A,1X,I8)') 'NCONTROL', NCONTROL
      DO N = 1, NCONTROL
        WRITE(*,'(A,1X,I6,1X,ES23.15E3)') 'CHINGE', N, CHINGE(N)
      END DO
      WRITE(*,'(A,1X,I8)') 'NSTRIP', NSTRIP
      DO J = 1, NSTRIP
        IOFF = 0
        IF(LSTRIPOFF(J)) IOFF = 1
        WRITE(*,'(A,1X,I6,1X,6(ES23.15E3),1X,I2)') 'STRIP', J,
     &    RLE(2,J), RLE(3,J), CNC(J), CLA_LSTRP(J),
     &    CLT_LSTRP(J), DWWAKE(J), IOFF
      END DO

C---- Full parity dump for all displayed outputs
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'ALFA', ALFA
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'BETA', BETA
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'MACH', MACH
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'CLTOT', CLTOT
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'CDTOT', CDTOT
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'CYTOT', CYTOT
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'CDVTOT', CDVTOT
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'SPANEF', SPANEF
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'SREF', SREF
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'CREF', CREF
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'BREF', BREF
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'XNP', XNP
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'CLA', CL_AL
      WRITE(*,'(A,1X,A,1X,ES23.15E3)') 'SCALAR', 'CMA', CM_AL

      WRITE(*,'(A,1X,A,1X,3(ES23.15E3,1X))') 'VEC3', 'WROT',
     &  WROT(1), WROT(2), WROT(3)
      WRITE(*,'(A,1X,A,1X,3(ES23.15E3,1X))') 'VEC3', 'CFTOT',
     &  CFTOT(1), CFTOT(2), CFTOT(3)
      WRITE(*,'(A,1X,A,1X,3(ES23.15E3,1X))') 'VEC3', 'CMTOT',
     &  CMTOT(1), CMTOT(2), CMTOT(3)

      WRITE(*,'(A,1X,I8)') 'NSURF', NSURF
      DO IS = 1, NSURF
        WRITE(*,'(A,1X,I6,1X,10(ES23.15E3,1X))') 'SURFROW', IS,
     &    CLSURF(IS), CDSURF(IS), CYSURF(IS), CDVSURF(IS),
     &    CFSURF(1,IS), CFSURF(2,IS), CFSURF(3,IS),
     &    CMSURF(1,IS), CMSURF(2,IS), CMSURF(3,IS)
      END DO

      DO K = 1, NUMAX
        WRITE(*,'(A,1X,I3,1X,9(ES23.15E3,1X))') 'UROW', K,
     &    CLTOT_U(K), CDTOT_U(K), CYTOT_U(K),
     &    CFTOT_U(1,K), CFTOT_U(2,K), CFTOT_U(3,K),
     &    CMTOT_U(1,K), CMTOT_U(2,K), CMTOT_U(3,K)
      END DO

      DO N = 1, NCONTROL
        WRITE(*,'(A,1X,I3,1X,11(ES23.15E3,1X))') 'DROW', N,
     &    CLTOT_D(N), CDTOT_D(N), CYTOT_D(N),
     &    CFTOT_D(1,N), CFTOT_D(2,N), CFTOT_D(3,N),
     &    CMTOT_D(1,N), CMTOT_D(2,N), CMTOT_D(3,N),
     &    CHINGE(N), DELCON(N)
      END DO

      DO J = 1, NSTRIP
        IOFF = 0
        IF(LSTRIPOFF(J)) IOFF = 1
        WRITE(*,'(A,1X,I6,1X,9(ES23.15E3,1X),1X,I2)') 'STRIPROW', J,
     &    RLE(2,J), RLE(3,J), CLSTRP(J), CDSTRP(J), CYSTRP(J),
     &    CNC(J), CLA_LSTRP(J), CLT_LSTRP(J), DWWAKE(J), IOFF
      END DO

      END

      INTEGER FUNCTION KEYTOIC(KEY, ICTOTV)
      INCLUDE 'AVL.INC'
      INTEGER KEY, ICTOTV

      KEYTOIC = 0
      IF(KEY .EQ. 1) THEN
        KEYTOIC = ICALFA
      ELSEIF(KEY .EQ. 2) THEN
        KEYTOIC = ICBETA
      ELSEIF(KEY .EQ. 3) THEN
        KEYTOIC = ICROTX
      ELSEIF(KEY .EQ. 4) THEN
        KEYTOIC = ICROTY
      ELSEIF(KEY .EQ. 5) THEN
        KEYTOIC = ICROTZ
      ELSEIF(KEY .EQ. 6) THEN
        KEYTOIC = ICCL
      ELSEIF(KEY .EQ. 7) THEN
        KEYTOIC = ICCY
      ELSEIF(KEY .EQ. 8) THEN
        KEYTOIC = ICMOMX
      ELSEIF(KEY .EQ. 9) THEN
        KEYTOIC = ICMOMY
      ELSEIF(KEY .EQ. 10) THEN
        KEYTOIC = ICMOMZ
      ELSEIF(KEY .GE. 100) THEN
        KEYTOIC = ICTOTV + (KEY - 100)
      ENDIF
      RETURN
      END

C--------------------------------------------------------------
C  Minimal subset of avl.f subroutines needed for parsing/setup
C--------------------------------------------------------------
      SUBROUTINE DEFINI
      INCLUDE 'AVL.INC'
C
C---- flag for forces in standard NASA stability axes (as in Etkin)
      LNASA_SA  = .TRUE.
C
C---- flag for rotations defined in stability axes or body axes
      LSA_RATES = .TRUE.
C
      LPTOT   = .TRUE.
      LPSURF  = .FALSE.
      LPSTRP  = .FALSE.
      LPELE   = .FALSE.
      LPHINGE = .FALSE.
      LPDERIV = .FALSE.
C
      LGEO  = .FALSE.
      LENC  = .FALSE.
C
      LAIC  = .FALSE.
      LSRD  = .FALSE.
      LVEL  = .FALSE.
      LSOL  = .FALSE.
      LSEN  = .FALSE.
C
      LOBAIC = .FALSE.
      NOB = 0
C
      LVISC    = .TRUE.
      LBFORCE  = .TRUE.
      LNFLD_WV  = .FALSE.
      LTRFORCE = .FALSE.
C
      LMWAIT = .FALSE.
C
      MATSYM = 0
      NITMAX = 20
C
      SAXFR = 0.25
C
      VRCOREC = 0.00
      VRCOREW = 2.0
      SRCORE  = 1.0
C
C---- dafault basic units
      UNITL = 1.
      UNITM = 1.
      UNITT = 1.
      UNCHL = 'Lunit'
      UNCHM = 'Munit'
      UNCHT = 'Tunit'
      NUL = 5
      NUM = 5
      NUT = 5
C
C---- set corresponding derived units
      CALL UNITSET
C
C---- default air density and grav. accel.
      RHO0 = 1.0
      GEE0 = 1.0
C
C---- no eigenvalue reference data yet
      FEVDEF = ' '
      DO IR = 1, NRMAX
        NEIGENDAT(IR) = 0
      ENDDO
C
C---- no run cases defined yet
      NRUN = 0
      IRUN = 1
C
C---- number of valid time levels stored
      NTLEV = 0
C
C---- default time step, and number of time steps to take
      DELTAT = 0.0
      NTSTEPS = 0
C
      RETURN
      END


      SUBROUTINE PARSET
      INCLUDE 'AVL.INC'
C
C---- variable names
      VARNAM(IVALFA) = 'alpha '
      VARNAM(IVBETA) = 'beta  '
      VARNAM(IVROTX) = 'pb/2V '
      VARNAM(IVROTY) = 'qc/2V '
      VARNAM(IVROTZ) = 'rb/2V '
C
C---- variable selection keys
      VARKEY(IVALFA) = 'A lpha'
      VARKEY(IVBETA) = 'B eta'
      VARKEY(IVROTX) = 'R oll  rate'
      VARKEY(IVROTY) = 'P itch rate'
      VARKEY(IVROTZ) = 'Y aw   rate'
C
C---- constraint names
      CONNAM(ICALFA) = 'alpha '
      CONNAM(ICBETA) = 'beta  '
      CONNAM(ICROTX) = 'pb/2V '
      CONNAM(ICROTY) = 'qc/2V '
      CONNAM(ICROTZ) = 'rb/2V '
      CONNAM(ICCL  ) = 'CL    '
      CONNAM(ICCY  ) = 'CY    '
      CONNAM(ICMOMX) = 'Cl roll mom'
      CONNAM(ICMOMY) = 'Cm pitchmom'
      CONNAM(ICMOMZ) = 'Cn yaw  mom'
C
C---- constraint selection keys
      CONKEY(ICALFA) = 'A '
      CONKEY(ICBETA) = 'B '
      CONKEY(ICROTX) = 'R '
      CONKEY(ICROTY) = 'P '
      CONKEY(ICROTZ) = 'Y '
      CONKEY(ICCL  ) = 'C '
      CONKEY(ICCY  ) = 'S '
      CONKEY(ICMOMX) = 'RM'
      CONKEY(ICMOMY) = 'PM'
      CONKEY(ICMOMZ) = 'YM'
C
      IZERO = ICHAR('0')
C
C---- add control variables, direct constraints
      DO N = 1, NCONTROL
        ITEN = N/10
        IONE = N - 10*ITEN
        IV = IVTOT + N
        IC = ICTOT + N
        VARNAM(IV) = DNAME(N)
        CONNAM(IC) = DNAME(N)
        IF(ITEN.EQ.0) THEN
         VARKEY(IV) = 'D' // CHAR(IZERO+IONE) // ' '
     &             // ' ' // DNAME(N)(1:8)
         CONKEY(IC) = 'D' // CHAR(IZERO+IONE)
        ELSE
         VARKEY(IV) = 'D' // CHAR(IZERO+ITEN) // CHAR(IZERO+IONE)
     &             // ' ' // DNAME(N)(1:8)
         CONKEY(IC) = 'D' // CHAR(IZERO+ITEN) // CHAR(IZERO+IONE)
        ENDIF
        LCONDEF(N) = .TRUE.
      ENDDO
C
      DO N = 1, NDESIGN
        LDESDEF(N) = .TRUE.
      ENDDO
C
      NVTOT = IVTOT + NCONTROL
      NCTOT = ICTOT + NCONTROL
C
      PARNAM(IPALFA) = 'alpha    '
      PARNAM(IPBETA) = 'beta     '
      PARNAM(IPROTX) = 'pb/2V    '
      PARNAM(IPROTY) = 'qc/2V    '
      PARNAM(IPROTZ) = 'rb/2V    '
      PARNAM(IPCL )  = 'CL       '
      PARNAM(IPCD0)  = 'CDo      '
      PARNAM(IPPHI)  = 'bank     '
      PARNAM(IPTHE)  = 'elevation'
      PARNAM(IPPSI)  = 'heading  '
      PARNAM(IPMACH) = 'Mach     '
      PARNAM(IPVEE)  = 'velocity '
      PARNAM(IPRHO)  = 'density  '
      PARNAM(IPGEE)  = 'grav.acc.'
      PARNAM(IPRAD)  = 'turn_rad.'
      PARNAM(IPFAC)  = 'load_fac.'
      PARNAM(IPXCG)  = 'X_cg     '
      PARNAM(IPYCG)  = 'Y_cg     '
      PARNAM(IPZCG)  = 'Z_cg     '
      PARNAM(IPMASS) = 'mass     '
      PARNAM(IPIXX)  = 'Ixx      '
      PARNAM(IPIYY)  = 'Iyy      '
      PARNAM(IPIZZ)  = 'Izz      '
      PARNAM(IPIXY)  = 'Ixy      '
      PARNAM(IPIYZ)  = 'Iyz      '
      PARNAM(IPIZX)  = 'Izx      '
      PARNAM(IPCLA)  = 'visc CL_a'
      PARNAM(IPCLU)  = 'visc CL_u'
      PARNAM(IPCMA)  = 'visc CM_a'
      PARNAM(IPCMU)  = 'visc CM_u'
C
      NPTOT = IPTOT
      CALL PARNSET
C
      RETURN
      END


      SUBROUTINE PARNSET
      INCLUDE 'AVL.INC'
C
      DO IP = 1, IPTOT
        PARUNCH(IP) = ' '
      ENDDO
C
      PARUNCH(IPALFA) = 'deg'
      PARUNCH(IPBETA) = 'deg'
      PARUNCH(IPPHI)  = 'deg'
      PARUNCH(IPTHE)  = 'deg'
      PARUNCH(IPPSI)  = 'deg'
      PARUNCH(IPVEE)  = UNCHV
      PARUNCH(IPRHO)  = UNCHD
      PARUNCH(IPGEE)  = UNCHA
      PARUNCH(IPRAD)  = UNCHL
C
      PARUNCH(IPXCG)  = 'Lunit'
      PARUNCH(IPYCG)  = 'Lunit'
      PARUNCH(IPZCG)  = 'Lunit'
C
      PARUNCH(IPMASS) = UNCHM
      PARUNCH(IPIXX)  = UNCHI
      PARUNCH(IPIYY)  = UNCHI
      PARUNCH(IPIZZ)  = UNCHI
      PARUNCH(IPIXY)  = UNCHI
      PARUNCH(IPIYZ)  = UNCHI
      PARUNCH(IPIZX)  = UNCHI
C
      RETURN
      END


      SUBROUTINE VARINI
      INCLUDE 'AVL.INC'
C
      ALFA = 0.
      BETA = 0.
      WROT(1) = 0.
      WROT(2) = 0.
      WROT(3) = 0.
C
      DO N = 1, NCONTROL
        DELCON(N) = 0.0
      ENDDO
C
      DO N = 1, NDESIGN
        DELDES(N) = 0.0
      ENDDO
      LSOL = .FALSE.
C
      RETURN
      END
