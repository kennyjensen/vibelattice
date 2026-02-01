      PROGRAM B737_EXEC_REF
      USE avl_heap_inc
      INCLUDE 'AVL.INC'
      LOGICAL ERROR
      INTEGER IR, J, IOFF, NITER, N
      CHARACTER*256 FILAVL
      CHARACTER*32 ARG2

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
        FILAVL = 'b737.avl'
      ENDIF

      NITER = 8
      CALL GETARG0(2,ARG2)
      IF(ARG2 .NE. ' ') THEN
        READ(ARG2,*,ERR=10) NITER
      ENDIF
 10   CONTINUE

      CALL INPUT(LUINP,FILAVL,ERROR)
      IF(ERROR) THEN
        WRITE(*,*) 'ERROR: INPUT failed for ', FILAVL
        STOP
      ENDIF

      CALL PARSET
      CALL ENCALC
      CALL VARINI

C---- Align trim variables with JS pipeline (include control vars)
      NVTOT = IVTOT + NCONTROL
      NCTOT = ICTOT + NCONTROL

      IR = 1
      ICON(IVALFA,IR) = ICCL
      ICON(IVBETA,IR) = ICBETA
      ICON(IVROTX,IR) = ICROTX
      ICON(IVROTY,IR) = ICROTY
      ICON(IVROTZ,IR) = ICROTZ

      DO N = 1, NCONTROL
        ICON(IVTOT+N,IR) = ICTOT + N
        CONVAL(ICTOT+N,IR) = 0.0
      END DO

C---- Run case parameters (match JS test values)
      PARVAL(IPMACH,IR) = 0.78
      PARVAL(IPVEE,IR) = 16.34
      PARVAL(IPRHO,IR) = 1.225
      PARVAL(IPGEE,IR) = 9.81
      PARVAL(IPCL,IR) = 0.6
      PARVAL(IPPHI,IR) = 0.0
      PARVAL(IPXCG,IR) = 60.0
      PARVAL(IPYCG,IR) = 0.0
      PARVAL(IPZCG,IR) = 0.0
      PARVAL(IPCD0,IR) = 0.0

      ALFA = 2.0*DTR
      BETA = 0.0

      CONVAL(ICCL,IR) = 0.6
      CONVAL(ICBETA,IR) = 0.0
      CONVAL(ICROTX,IR) = 0.0
      CONVAL(ICROTY,IR) = 0.0
      CONVAL(ICROTZ,IR) = 0.0

      CALL EXEC(NITER,0,IR)

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
      WRITE(*,'(A,1X,I8)') 'NSTRIP', NSTRIP
      DO J = 1, NSTRIP
        IOFF = 0
        IF(LSTRIPOFF(J)) IOFF = 1
        WRITE(*,'(A,1X,I6,1X,6(ES23.15E3),1X,I2)') 'STRIP', J,
     &    RLE(2,J), RLE(3,J), CNC(J), CLA_LSTRP(J),
     &    CLT_LSTRP(J), DWWAKE(J), IOFF
      END DO

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
      CONNAM(ICALFA) = 'alpha       '
      CONNAM(ICBETA) = 'beta        '
      CONNAM(ICROTX) = 'pb/2V       '
      CONNAM(ICROTY) = 'qc/2V       '
      CONNAM(ICROTZ) = 'rb/2V       '
      CONNAM(ICCL  ) = 'CL          '
      CONNAM(ICCY  ) = 'CY          '
      CONNAM(ICMOMX) = 'Cl roll mom '
      CONNAM(ICMOMY) = 'Cm pitchmom '
      CONNAM(ICMOMZ) = 'Cn yaw  mom '
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
      DO N = 1, NCONTROL
        IV = IVTOT + N
        IC = ICTOT + N
        ITEN = N/10
        IONE = N - 10*ITEN
        IF(ITEN.EQ.0) THEN
         VARKEY(IV) = 'D' // CHAR(IZERO+IONE)
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
