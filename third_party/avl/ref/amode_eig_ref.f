      PROGRAM AMODE_REF
      INCLUDE 'AVL.INC'
      REAL*8 ASYS(JEMAX,JEMAX),BSYS(JEMAX,NDMAX),RSYS(JEMAX)
      INTEGER IR
      LOGICAL OK
C
      IR = 1
      NRUN = 2
      NCONTROL = 1
      NVTOT = IVTOT
      UNITL = 1.0
      SREF = 1.5
      CREF = 0.75
      BREF = 2.0
      PI = 3.1415927
      DTR = PI/180.0
C
      VINF(1) = 0.8
      VINF(2) = -0.1
      VINF(3) = 0.2
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
C
      DO IU = 1, NUMAX
        CFTOT_U(1,IU) = 0.01*IU
        CFTOT_U(2,IU) = -0.02*IU
        CFTOT_U(3,IU) = 0.03*IU
        CMTOT_U(1,IU) = 0.005*IU
        CMTOT_U(2,IU) = -0.006*IU
        CMTOT_U(3,IU) = 0.007*IU
      END DO
      CFTOT_D(1,1) = 0.11
      CFTOT_D(2,1) = -0.12
      CFTOT_D(3,1) = 0.13
      CMTOT_D(1,1) = 0.021
      CMTOT_D(2,1) = -0.022
      CMTOT_D(3,1) = 0.023
C
      AMASS(1,1) = 0.1
      AMASS(2,2) = 0.2
      AMASS(3,3) = 0.15
      AMASS(1,2) = 0.01
      AMASS(2,1) = 0.01
      AMASS(1,3) = -0.02
      AMASS(3,1) = -0.02
      AMASS(2,3) = 0.03
      AMASS(3,2) = 0.03
C
      AINER(1,1) = 0.02
      AINER(2,2) = 0.03
      AINER(3,3) = 0.025
      AINER(1,2) = 0.004
      AINER(2,1) = 0.004
      AINER(1,3) = -0.003
      AINER(3,1) = -0.003
      AINER(2,3) = 0.002
      AINER(3,2) = 0.002
C
      PARVAL(IPGEE ,IR) = 9.81
      PARVAL(IPRHO ,IR) = 1.225
      PARVAL(IPVEE ,IR) = 30.0
      PARVAL(IPPHI ,IR) = 5.0
      PARVAL(IPTHE ,IR) = -2.0
      PARVAL(IPPSI ,IR) = 1.0
      PARVAL(IPXCG ,IR) = 0.1
      PARVAL(IPYCG ,IR) = -0.2
      PARVAL(IPZCG ,IR) = 0.3
      PARVAL(IPMASS,IR) = 120.0
      PARVAL(IPIXX ,IR) = 12.0
      PARVAL(IPIYY ,IR) = 15.0
      PARVAL(IPIZZ ,IR) = 20.0
      PARVAL(IPIXY ,IR) = 0.5
      PARVAL(IPIYZ ,IR) = -0.4
      PARVAL(IPIZX ,IR) = 0.3
      PARVAL(IPCLU ,IR) = 0.02
      PARVAL(IPCMU ,IR) = -0.01
      PARVAL(IPCLA ,IR) = 0.03
      PARVAL(IPCMA ,IR) = -0.015
C
      DO IV = 1, NVTOT
        ICON(IV,1) = IV
        ICON(IV,2) = 1
      END DO
C
      CALL RUNCHK(1,OK)
      WRITE(*,'(A)') 'BEGIN RUNCHK'
      WRITE(*,*) OK
      CALL RUNCHK(2,OK)
      WRITE(*,*) OK
C
      CALL SYSMAT(IR,ASYS,BSYS,RSYS,NSYS)
      WRITE(*,'(A)') 'BEGIN SYSMAT'
      WRITE(*,*) NSYS
      DO I = 1, NSYS
        WRITE(*,*) (ASYS(I,J), J=1, NSYS), (BSYS(I,N), N=1, NCONTROL), RSYS(I)
      END DO
      WRITE(*,'(A)') 'BEGIN RSYS_SYSMAT'
      DO I = 1, NSYS
        WRITE(*,*) RSYS(I)
      END DO
C
      CALL APPMAT(IR,ASYS,BSYS,RSYS,NSYS)
      WRITE(*,'(A)') 'BEGIN APPMAT'
      WRITE(*,*) NSYS
      DO I = 1, NSYS
        WRITE(*,*) (ASYS(I,J), J=1, NSYS), (BSYS(I,N), N=1, NCONTROL), RSYS(I)
      END DO
      WRITE(*,'(A)') 'BEGIN RSYS_APPMAT'
      DO I = 1, NSYS
        WRITE(*,*) RSYS(I)
      END DO
C
      CALL EIGSOL(IERR,IR,1.0E-5,ASYS,NSYS)
      WRITE(*,'(A)') 'BEGIN EIGSOL'
      WRITE(*,*) IERR, NEIGEN(IR)
      DO I = 1, NEIGEN(IR)
        WRITE(*,*) REAL(EVAL(I,IR)), AIMAG(EVAL(I,IR))
      END DO
C
      END


      SUBROUTINE RUNCHK(JRUN,OK)
      INCLUDE 'AVL.INC'
      LOGICAL OK
      OK = .TRUE.
      DO IV = 1, NVTOT
        DO JV = 1, NVTOT
          IF(IV.NE.JV .AND. ICON(IV,JRUN).EQ.ICON(JV,JRUN)) THEN
           OK = .FALSE.
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END


      SUBROUTINE EIGSOL(INFO,IR,ETOL,ASYS,NSYS)
C------------------------------------------------------------------
C     Computes eigenvalues and eigenvectors for run case IR.
C------------------------------------------------------------------
      INCLUDE 'AVL.INC'
      REAL*8 ASYS(JEMAX,JEMAX)
C
      REAL*8 WR(JEMAX),WI(JEMAX),WVEC(JEMAX,JEMAX)
      REAL*8 WORK(JEMAX)
      INTEGER IWORK(JEMAX)
C
      ICALC = 1
      CALL RG(JEMAX,NSYS,ASYS,WR,WI,ICALC,WVEC,IWORK,WORK,IERR)
C
      VEE = PARVAL(IPVEE,IR)
      BREFD = BREF*UNITL
      ETOLSQ = (ETOL*VEE/BREFD)**2
C
      KEIG = 0
      DO 100 J=1, NSYS
        EMAGSQ = WR(J)**2 + WI(J)**2
        IF(EMAGSQ .LT. ETOLSQ) GO TO 100
        KEIG = KEIG + 1
        EVAL(KEIG,IR) = CMPLX( WR(J), WI(J) )
        IF    (WI(J) .EQ. 0.0) THEN
         DO I = 1, NSYS
           EVR = WVEC(I,J)
           EVI = 0.
           EVEC(I,KEIG,IR) = CMPLX( EVR , EVI )
         ENDDO
        ELSEIF(WI(J) .GT. 0.0) THEN
         JP1 = MIN( J+1 , NSYS )
         DO I = 1, NSYS
           EVR = WVEC(I,J)
           EVI = WVEC(I,JP1)
           EVEC(I,KEIG,IR) = CMPLX( EVR , EVI )
         ENDDO
        ELSEIF(WI(J) .LT. 0.0) THEN
         JM = MAX( J-1 , 1 )
         DO I = 1, NSYS
           EVR =  WVEC(I,JM)
           EVI = -WVEC(I,J)
           EVEC(I,KEIG,IR) = CMPLX( EVR , EVI )
         ENDDO
        ENDIF
 100  CONTINUE
      NEIGEN(IR) = KEIG
      INFO = IERR
      RETURN
      END


      SUBROUTINE SYSMAT(IR,ASYS,BSYS,RSYS,NSYS)
      INCLUDE 'AVL.INC'
      REAL*8 ASYS(JEMAX,JEMAX),BSYS(JEMAX,NDMAX),RSYS(JEMAX)
      LOGICAL LTERR
      REAL RINER(3,3),
     &     MAMAT(3,3),
     &     MAINV(3,3),
     &     RIMAT(3,3),
     &     RIINV(3,3),
     &     P(3), P_U(3,NUMAX),
     &     H(3), H_U(3,NUMAX),
     &     WXP(3), WXP_U(3,NUMAX),
     &     WXH(3), WXH_U(3,NUMAX),
     &     MIF(3), MIF_U(3,NUMAX), MIF_D(3,NDMAX),
     &     RIM(3), RIM_U(3,NUMAX), RIM_D(3,NDMAX),
     &     PRF(3), PRF_U(3,NUMAX),
     &     PRM(3), PRM_U(3,NUMAX)
      REAL ANG(3), TT(3,3), TT_ANG(3,3,3), RT(3,3), RT_ANG(3,3,3)
      INTEGER ICRS(3), JCRS(3)
      DATA ICRS / 2, 3, 1 / , JCRS / 3, 1, 2 /
      GEE = PARVAL(IPGEE,IR)
      RHO = PARVAL(IPRHO,IR)
      VEE = PARVAL(IPVEE,IR)
      PHI = PARVAL(IPPHI,IR)
      THE = PARVAL(IPTHE,IR)
      PSI = PARVAL(IPPSI,IR)
      XCG = PARVAL(IPXCG,IR)
      YCG = PARVAL(IPYCG,IR)
      ZCG = PARVAL(IPZCG,IR)
      RMASS = PARVAL(IPMASS,IR)
      RINER(1,1) = PARVAL(IPIXX,IR)
      RINER(2,2) = PARVAL(IPIYY,IR)
      RINER(3,3) = PARVAL(IPIZZ,IR)
      RINER(1,2) = PARVAL(IPIXY,IR)
      RINER(2,3) = PARVAL(IPIYZ,IR)
      RINER(3,1) = PARVAL(IPIZX,IR)
      RINER(2,1) = PARVAL(IPIXY,IR)
      RINER(3,2) = PARVAL(IPIYZ,IR)
      RINER(1,3) = PARVAL(IPIZX,IR)
      DCL_U = PARVAL(IPCLU,IR)
      DCM_U = PARVAL(IPCMU,IR)
      DCL_A = PARVAL(IPCLA,IR)
      DCM_A = PARVAL(IPCMA,IR)
      LTERR = .FALSE.
      IF(VEE .LE. 0.0) LTERR = .TRUE.
      IF(RMASS .LE. 0.0) LTERR = .TRUE.
      IF(RINER(1,1) .LE. 0.0) LTERR = .TRUE.
      IF(RINER(2,2) .LE. 0.0) LTERR = .TRUE.
      IF(RINER(3,3) .LE. 0.0) LTERR = .TRUE.
      IF(LTERR) RETURN
      SREFD = SREF*UNITL**2
      BREFD = BREF*UNITL
      CREFD = CREF*UNITL
      XYZREF(1) = XCG
      XYZREF(2) = YCG
      XYZREF(3) = ZCG
      QS  = 0.5*RHO*VEE**2 * SREFD
      QSM = QS/RMASS
      ROT = VEE/UNITL
      DO K = 1, 3
        MAMAT(K,1) = AMASS(K,1)*RHO
        MAMAT(K,2) = AMASS(K,2)*RHO
        MAMAT(K,3) = AMASS(K,3)*RHO
        MAMAT(K,K) = MAMAT(K,K) + RMASS
        RIMAT(K,1) = RINER(K,1) + AINER(K,1)*RHO
        RIMAT(K,2) = RINER(K,2) + AINER(K,2)*RHO
        RIMAT(K,3) = RINER(K,3) + AINER(K,3)*RHO
      ENDDO
      CALL M3INV(MAMAT,MAINV)
      CALL M3INV(RIMAT,RIINV)
      DO K = 1, 3
        P(K) = -(  MAMAT(K,1)*VINF(1)
     &           + MAMAT(K,2)*VINF(2)
     &           + MAMAT(K,3)*VINF(3) )*VEE
        P_U(K,1) = -MAMAT(K,1)*VEE
        P_U(K,2) = -MAMAT(K,2)*VEE
        P_U(K,3) = -MAMAT(K,3)*VEE
        P_U(K,4) = 0.
        P_U(K,5) = 0.
        P_U(K,6) = 0.
        H(K) = (  RIMAT(K,1)*WROT(1)
     &          + RIMAT(K,2)*WROT(2)
     &          + RIMAT(K,3)*WROT(3) )*ROT
        H_U(K,1) = 0.
        H_U(K,2) = 0.
        H_U(K,3) = 0.
        H_U(K,4) = RIMAT(K,1)*ROT
        H_U(K,5) = RIMAT(K,2)*ROT
        H_U(K,6) = RIMAT(K,3)*ROT
      ENDDO
      DO K = 1, 3
        I = ICRS(K)
        J = JCRS(K)
        WXP(K) = (WROT(I)*P(J) - WROT(J)*P(I))*ROT
        WXH(K) = (WROT(I)*H(J) - WROT(J)*H(I))*ROT
        WXP_U(K,1) = (WROT(I)*P_U(J,1) - WROT(J)*P_U(I,1))*ROT
        WXP_U(K,2) = (WROT(I)*P_U(J,2) - WROT(J)*P_U(I,2))*ROT
        WXP_U(K,3) = (WROT(I)*P_U(J,3) - WROT(J)*P_U(I,3))*ROT
        WXP_U(K,4) = 0.
        WXP_U(K,5) = 0.
        WXP_U(K,6) = 0.
        WXP_U(K,I+3) = WXP_U(K,I+3) + P(J)*ROT
        WXP_U(K,J+3) = WXP_U(K,J+3) - P(I)*ROT
        WXH_U(K,1) = 0.
        WXH_U(K,2) = 0.
        WXH_U(K,3) = 0.
        WXH_U(K,4) = (WROT(I)*H_U(J,4) - WROT(J)*H_U(I,4))*ROT
        WXH_U(K,5) = (WROT(I)*H_U(J,5) - WROT(J)*H_U(I,5))*ROT
        WXH_U(K,6) = (WROT(I)*H_U(J,6) - WROT(J)*H_U(I,6))*ROT
        WXH_U(K,I+3) = WXH_U(K,I+3) + H(J)*ROT
        WXH_U(K,J+3) = WXH_U(K,J+3) - H(I)*ROT
      ENDDO
      DO K = 1, 3
        MIF(K) = MAINV(K,1)*CFTOT(1)*QS
     &         + MAINV(K,2)*CFTOT(2)*QS
     &         + MAINV(K,3)*CFTOT(3)*QS
        RIM(K) = RIINV(K,1)*CMTOT(1)*QS*BREFD
     &         + RIINV(K,2)*CMTOT(2)*QS*CREFD
     &         + RIINV(K,3)*CMTOT(3)*QS*BREFD
        PRF(K) = MAINV(K,1)*WXP(1)
     &         + MAINV(K,2)*WXP(2)
     &         + MAINV(K,3)*WXP(3)
        PRM(K) = RIINV(K,1)*WXH(1)
     &         + RIINV(K,2)*WXH(2)
     &         + RIINV(K,3)*WXH(3)
        DO IU = 1, 6
          MIF_U(K,IU) = 
     &           MAINV(K,1)*CFTOT_U(1,IU)*QS
     &         + MAINV(K,2)*CFTOT_U(2,IU)*QS
     &         + MAINV(K,3)*CFTOT_U(3,IU)*QS
          RIM_U(K,IU) = 
     &           RIINV(K,1)*CMTOT_U(1,IU)*QS*BREFD
     &         + RIINV(K,2)*CMTOT_U(2,IU)*QS*CREFD
     &         + RIINV(K,3)*CMTOT_U(3,IU)*QS*BREFD
          PRF_U(K,IU) =
     &           MAINV(K,1)*WXP_U(1,IU)
     &         + MAINV(K,2)*WXP_U(2,IU)
     &         + MAINV(K,3)*WXP_U(3,IU)
          PRM_U(K,IU) =
     &           RIINV(K,1)*WXH_U(1,IU)
     &         + RIINV(K,2)*WXH_U(2,IU)
     &         + RIINV(K,3)*WXH_U(3,IU)
        ENDDO
        DO N = 1, NCONTROL
          MIF_D(K,N) = 
     &           MAINV(K,1)*CFTOT_D(1,N)*QS
     &         + MAINV(K,2)*CFTOT_D(2,N)*QS
     &         + MAINV(K,3)*CFTOT_D(3,N)*QS
          RIM_D(K,N) = 
     &           RIINV(K,1)*CMTOT_D(1,N)*QS*BREFD
     &         + RIINV(K,2)*CMTOT_D(2,N)*QS*CREFD
     &         + RIINV(K,3)*CMTOT_D(3,N)*QS*BREFD
        ENDDO
        IU = 1
        MIF_U(K,IU) = MIF_U(K,IU)  -  MAINV(K,3)*DCL_U*QS
        RIM_U(K,IU) = RIM_U(K,IU)  -  RIINV(K,2)*DCM_U*QS*CREFD
        IU = 3
        MIF_U(K,IU) = MIF_U(K,IU)  +  MAINV(K,3)*DCL_A*QS
        RIM_U(K,IU) = RIM_U(K,IU)  +  RIINV(K,2)*DCM_A*QS*CREFD
      ENDDO
      ANG(1) = PHI*DTR
      ANG(2) = THE*DTR
      ANG(3) = PSI*DTR
      CALL ROTENS3(ANG,TT,TT_ANG)
      CALL RATEKI3(ANG,RT,RT_ANG)
      NSYS = 12
      DO IEQ = 1, NSYS
        DO JE = 1, NSYS
          ASYS(IEQ,JE) = 0.
        ENDDO
        DO N = 1, NCONTROL
          BSYS(IEQ,N) = 0.
        ENDDO
      ENDDO
      IEQ = JEU
      K = 1
      RSYS(IEQ)     =   MIF(K)     - PRF(K)      - GEE*TT(3,K)
      ASYS(IEQ,JEU) = -(MIF_U(K,1) - PRF_U(K,1)) / VEE
      ASYS(IEQ,JEV) = -(MIF_U(K,2) - PRF_U(K,2)) / VEE
      ASYS(IEQ,JEW) = -(MIF_U(K,3) - PRF_U(K,3)) / VEE
      ASYS(IEQ,JEP) =  (MIF_U(K,4) - PRF_U(K,4)) / ROT
      ASYS(IEQ,JEQ) =  (MIF_U(K,5) - PRF_U(K,5)) / ROT
      ASYS(IEQ,JER) =  (MIF_U(K,6) - PRF_U(K,6)) / ROT
      ASYS(IEQ,JEPH)=                            - GEE*TT_ANG(3,K,1)
      ASYS(IEQ,JETH)=                            - GEE*TT_ANG(3,K,2)
      ASYS(IEQ,JEPS)=                            - GEE*TT_ANG(3,K,3)
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =   MIF_D(K,N)
      ENDDO
      IEQ = JEV
      K = 2
      RSYS(IEQ)     =   MIF(K)     - PRF(K)      - GEE*TT(3,K)
      ASYS(IEQ,JEU) = -(MIF_U(K,1) - PRF_U(K,1)) / VEE
      ASYS(IEQ,JEV) = -(MIF_U(K,2) - PRF_U(K,2)) / VEE
      ASYS(IEQ,JEW) = -(MIF_U(K,3) - PRF_U(K,3)) / VEE
      ASYS(IEQ,JEP) =  (MIF_U(K,4) - PRF_U(K,4)) / ROT
      ASYS(IEQ,JEQ) =  (MIF_U(K,5) - PRF_U(K,5)) / ROT
      ASYS(IEQ,JER) =  (MIF_U(K,6) - PRF_U(K,6)) / ROT
      ASYS(IEQ,JEPH)=                            - GEE*TT_ANG(3,K,1)
      ASYS(IEQ,JETH)=                            - GEE*TT_ANG(3,K,2)
      ASYS(IEQ,JEPS)=                            - GEE*TT_ANG(3,K,3)
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =   MIF_D(K,N)
      ENDDO
      IEQ = JEW
      K = 3
      RSYS(IEQ)     =   MIF(K)     - PRF(K)      - GEE*TT(3,K)
      ASYS(IEQ,JEU) = -(MIF_U(K,1) - PRF_U(K,1)) / VEE
      ASYS(IEQ,JEV) = -(MIF_U(K,2) - PRF_U(K,2)) / VEE
      ASYS(IEQ,JEW) = -(MIF_U(K,3) - PRF_U(K,3)) / VEE
      ASYS(IEQ,JEP) =  (MIF_U(K,4) - PRF_U(K,4)) / ROT
      ASYS(IEQ,JEQ) =  (MIF_U(K,5) - PRF_U(K,5)) / ROT
      ASYS(IEQ,JER) =  (MIF_U(K,6) - PRF_U(K,6)) / ROT
      ASYS(IEQ,JEPH)=                            - GEE*TT_ANG(3,K,1)
      ASYS(IEQ,JETH)=                            - GEE*TT_ANG(3,K,2)
      ASYS(IEQ,JEPS)=                            - GEE*TT_ANG(3,K,3)
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =   MIF_D(K,N)
      ENDDO
      IEQ = JEP
      K = 1
      RSYS(IEQ)     =   RIM(K)     - PRM(K)
      ASYS(IEQ,JEU) = -(RIM_U(K,1) - PRM_U(K,1)) / VEE
      ASYS(IEQ,JEV) = -(RIM_U(K,2) - PRM_U(K,2)) / VEE
      ASYS(IEQ,JEW) = -(RIM_U(K,3) - PRM_U(K,3)) / VEE
      ASYS(IEQ,JEP) =  (RIM_U(K,4) - PRM_U(K,4)) / ROT
      ASYS(IEQ,JEQ) =  (RIM_U(K,5) - PRM_U(K,5)) / ROT
      ASYS(IEQ,JER) =  (RIM_U(K,6) - PRM_U(K,6)) / ROT
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =   RIM_D(K,N)
      ENDDO
      IEQ = JEQ
      K = 2
      RSYS(IEQ)     =   RIM(K)     - PRM(K)
      ASYS(IEQ,JEU) = -(RIM_U(K,1) - PRM_U(K,1)) / VEE
      ASYS(IEQ,JEV) = -(RIM_U(K,2) - PRM_U(K,2)) / VEE
      ASYS(IEQ,JEW) = -(RIM_U(K,3) - PRM_U(K,3)) / VEE
      ASYS(IEQ,JEP) =  (RIM_U(K,4) - PRM_U(K,4)) / ROT
      ASYS(IEQ,JEQ) =  (RIM_U(K,5) - PRM_U(K,5)) / ROT
      ASYS(IEQ,JER) =  (RIM_U(K,6) - PRM_U(K,6)) / ROT
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =   RIM_D(K,N)
      ENDDO
      IEQ = JER
      K = 3
      RSYS(IEQ)     =   RIM(K)     - PRM(K)
      ASYS(IEQ,JEU) = -(RIM_U(K,1) - PRM_U(K,1)) / VEE
      ASYS(IEQ,JEV) = -(RIM_U(K,2) - PRM_U(K,2)) / VEE
      ASYS(IEQ,JEW) = -(RIM_U(K,3) - PRM_U(K,3)) / VEE
      ASYS(IEQ,JEP) =  (RIM_U(K,4) - PRM_U(K,4)) / ROT
      ASYS(IEQ,JEQ) =  (RIM_U(K,5) - PRM_U(K,5)) / ROT
      ASYS(IEQ,JER) =  (RIM_U(K,6) - PRM_U(K,6)) / ROT
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =   RIM_D(K,N)
      ENDDO
      IEQ = JEPH
      K = 1
      RSYS(IEQ)     = ROT*(  RT(K,1)*WROT(1)
     &                     + RT(K,2)*WROT(2)
     &                     + RT(K,3)*WROT(3) )
      ASYS(IEQ,JEP) =        RT(K,1)
      ASYS(IEQ,JEQ) =        RT(K,2)
      ASYS(IEQ,JER) =        RT(K,3)
      ASYS(IEQ,JEPH)= ROT*(  RT_ANG(K,1,1)*WROT(1)
     &                     + RT_ANG(K,2,1)*WROT(2)
     &                     + RT_ANG(K,3,1)*WROT(3) )
      ASYS(IEQ,JETH)= ROT*(  RT_ANG(K,1,2)*WROT(1)
     &                     + RT_ANG(K,2,2)*WROT(2)
     &                     + RT_ANG(K,3,2)*WROT(3) )
      ASYS(IEQ,JEPS)= ROT*(  RT_ANG(K,1,3)*WROT(1)
     &                     + RT_ANG(K,2,3)*WROT(2)
     &                     + RT_ANG(K,3,3)*WROT(3) )
      IEQ = JETH
      K = 2
      RSYS(IEQ)     = ROT*(  RT(K,1)*WROT(1)
     &                     + RT(K,2)*WROT(2)
     &                     + RT(K,3)*WROT(3) )
      ASYS(IEQ,JEP) =        RT(K,1)
      ASYS(IEQ,JEQ) =        RT(K,2)
      ASYS(IEQ,JER) =        RT(K,3)
      ASYS(IEQ,JEPH)= ROT*(  RT_ANG(K,1,1)*WROT(1)
     &                     + RT_ANG(K,2,1)*WROT(2)
     &                     + RT_ANG(K,3,1)*WROT(3) )
      ASYS(IEQ,JETH)= ROT*(  RT_ANG(K,1,2)*WROT(1)
     &                     + RT_ANG(K,2,2)*WROT(2)
     &                     + RT_ANG(K,3,2)*WROT(3) )
      ASYS(IEQ,JEPS)= ROT*(  RT_ANG(K,1,3)*WROT(1)
     &                     + RT_ANG(K,2,3)*WROT(2)
     &                     + RT_ANG(K,3,3)*WROT(3) )
      IEQ = JEPS
      K = 3
      RSYS(IEQ)     = ROT*(  RT(K,1)*WROT(1)
     &                     + RT(K,2)*WROT(2)
     &                     + RT(K,3)*WROT(3) )
      ASYS(IEQ,JEP) =        RT(K,1)
      ASYS(IEQ,JEQ) =        RT(K,2)
      ASYS(IEQ,JER) =        RT(K,3)
      ASYS(IEQ,JEPH)= ROT*(  RT_ANG(K,1,1)*WROT(1)
     &                     + RT_ANG(K,2,1)*WROT(2)
     &                     + RT_ANG(K,3,1)*WROT(3) )
      ASYS(IEQ,JETH)= ROT*(  RT_ANG(K,1,2)*WROT(1)
     &                     + RT_ANG(K,2,2)*WROT(2)
     &                     + RT_ANG(K,3,2)*WROT(3) )
      ASYS(IEQ,JEPS)= ROT*(  RT_ANG(K,1,3)*WROT(1)
     &                     + RT_ANG(K,2,3)*WROT(2)
     &                     + RT_ANG(K,3,3)*WROT(3) )
      IEQ = JEX
      K = 1
      RSYS(IEQ)     = -(  TT(K,1)*VINF(1)
     &                  + TT(K,2)*VINF(2)
     &                  + TT(K,3)*VINF(3) )*VEE
      ASYS(IEQ,JEU) =     TT(K,1)
      ASYS(IEQ,JEV) =     TT(K,2)
      ASYS(IEQ,JEW) =     TT(K,3)
      ASYS(IEQ,JEPH)= -(  TT_ANG(K,1,1)*VINF(1)
     &                  + TT_ANG(K,2,1)*VINF(2)
     &                  + TT_ANG(K,3,1)*VINF(3) )*VEE
      ASYS(IEQ,JETH)= -(  TT_ANG(K,1,2)*VINF(1)
     &                  + TT_ANG(K,2,2)*VINF(2)
     &                  + TT_ANG(K,3,2)*VINF(3) )*VEE
      ASYS(IEQ,JEPS)= -(  TT_ANG(K,1,3)*VINF(1)
     &                  + TT_ANG(K,2,3)*VINF(2)
     &                  + TT_ANG(K,3,3)*VINF(3) )*VEE
      IEQ = JEY
      K = 2
      RSYS(IEQ)     = -(  TT(K,1)*VINF(1)
     &                  + TT(K,2)*VINF(2)
     &                  + TT(K,3)*VINF(3) )*VEE
      ASYS(IEQ,JEU) =     TT(K,1)
      ASYS(IEQ,JEV) =     TT(K,2)
      ASYS(IEQ,JEW) =     TT(K,3)
      ASYS(IEQ,JEPH)= -(  TT_ANG(K,1,1)*VINF(1)
     &                  + TT_ANG(K,2,1)*VINF(2)
     &                  + TT_ANG(K,3,1)*VINF(3) )*VEE
      ASYS(IEQ,JETH)= -(  TT_ANG(K,1,2)*VINF(1)
     &                  + TT_ANG(K,2,2)*VINF(2)
     &                  + TT_ANG(K,3,2)*VINF(3) )*VEE
      ASYS(IEQ,JEPS)= -(  TT_ANG(K,1,3)*VINF(1)
     &                  + TT_ANG(K,2,3)*VINF(2)
     &                  + TT_ANG(K,3,3)*VINF(3) )*VEE
      IEQ = JEZ
      K = 3
      RSYS(IEQ)     = -(  TT(K,1)*VINF(1)
     &                  + TT(K,2)*VINF(2)
     &                  + TT(K,3)*VINF(3) )*VEE
      ASYS(IEQ,JEU) =     TT(K,1)
      ASYS(IEQ,JEV) =     TT(K,2)
      ASYS(IEQ,JEW) =     TT(K,3)
      ASYS(IEQ,JEPH)= -(  TT_ANG(K,1,1)*VINF(1)
     &                  + TT_ANG(K,2,1)*VINF(2)
     &                  + TT_ANG(K,3,1)*VINF(3) )*VEE
      ASYS(IEQ,JETH)= -(  TT_ANG(K,1,2)*VINF(1)
     &                  + TT_ANG(K,2,2)*VINF(2)
     &                  + TT_ANG(K,3,2)*VINF(3) )*VEE
      ASYS(IEQ,JEPS)= -(  TT_ANG(K,1,3)*VINF(1)
     &                  + TT_ANG(K,2,3)*VINF(2)
     &                  + TT_ANG(K,3,3)*VINF(3) )*VEE
      RETURN
      END


      SUBROUTINE APPMAT(IR,ASYS,BSYS,RSYS,NSYS)
      INCLUDE 'AVL.INC'
      REAL*8 ASYS(JEMAX,JEMAX),BSYS(JEMAX,NDMAX),RSYS(JEMAX)
      LOGICAL LTERR
      REAL ANG(3), TT(3,3), TT_ANG(3,3,3), RT(3,3), RT_ANG(3,3,3)
      GEE = PARVAL(IPGEE,IR)
      RHO = PARVAL(IPRHO,IR)
      VEE = PARVAL(IPVEE,IR)
      PHI = PARVAL(IPPHI,IR)
      THE = PARVAL(IPTHE,IR)
      PSI = PARVAL(IPPSI,IR)
      XCG = PARVAL(IPXCG,IR)
      YCG = PARVAL(IPYCG,IR)
      ZCG = PARVAL(IPZCG,IR)
      RMASS = PARVAL(IPMASS,IR)
      RINXX = PARVAL(IPIXX,IR)
      RINYY = PARVAL(IPIYY,IR)
      RINZZ = PARVAL(IPIZZ,IR)
      LTERR = .FALSE.
      IF(VEE .LE. 0.0) LTERR = .TRUE.
      IF(RMASS .LE. 0.0) LTERR = .TRUE.
      IF(RINXX .LE. 0.0) LTERR = .TRUE.
      IF(RINYY .LE. 0.0) LTERR = .TRUE.
      IF(RINZZ .LE. 0.0) LTERR = .TRUE.
      IF(LTERR) RETURN
      SREFD = SREF*UNITL**2
      BREFD = BREF*UNITL
      CREFD = CREF*UNITL
      XYZREF(1) = XCG
      XYZREF(2) = YCG
      XYZREF(3) = ZCG
      QS  = 0.5*RHO*VEE**2 * SREFD
      QSC = QS*CREFD
      QSB = QS*BREFD
      ROT = VEE/UNITL
      ANG(1) = PHI*DTR
      ANG(2) = THE*DTR
      ANG(3) = PSI*DTR
      CALL ROTENS3(ANG,TT,TT_ANG)
      CALL RATEKI3(ANG,RT,RT_ANG)
      NSYS = 12
      DO IEQ = 1, NSYS
        DO JE = 1, NSYS
          ASYS(IEQ,JE) = 0.
        ENDDO
        DO N = 1, NCONTROL
          BSYS(IEQ,N) = 0.
        ENDDO
      ENDDO
      IEQ = JEU
      ASYS(IEQ,JEU) = -CFTOT_U(1,1)*QS /RMASS / VEE
      ASYS(IEQ,JEW) = -CFTOT_U(1,3)*QS /RMASS / VEE
      ASYS(IEQ,JEQ) =  CFTOT_U(1,5)*QS /RMASS / ROT  +  VINF(3)*VEE
      ASYS(IEQ,JETH)=  GEE
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =  CFTOT_D(1,N)*QS /RMASS
      ENDDO
      IEQ = JEW
      ASYS(IEQ,JEU) = -CFTOT_U(3,1)*QS /RMASS / VEE
      ASYS(IEQ,JEW) = -CFTOT_U(3,3)*QS /RMASS / VEE
      ASYS(IEQ,JEQ) =  CFTOT_U(3,5)*QS /RMASS / ROT  -  VINF(1)*VEE
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =  CFTOT_D(3,N)*QS /RMASS
      ENDDO
      IEQ = JEQ
      ASYS(IEQ,JEU) = -CMTOT_U(2,1)*QSC/RINYY / VEE
      ASYS(IEQ,JEW) = -CMTOT_U(2,3)*QSC/RINYY / VEE
      ASYS(IEQ,JEQ) =  CMTOT_U(2,5)*QSC/RINYY / ROT
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =  CMTOT_D(2,N)*QSC/RINYY
      ENDDO
      IEQ = JETH
      ASYS(IEQ,JEQ) =  1.0
      IEQ = JEV
      ASYS(IEQ,JEV) = -CFTOT_U(2,2)*QS /RMASS / VEE
      ASYS(IEQ,JEP) =  CFTOT_U(2,4)*QS /RMASS / ROT  -  VINF(3)*VEE
      ASYS(IEQ,JER) =  CFTOT_U(2,6)*QS /RMASS / ROT  +  VINF(1)*VEE
      ASYS(IEQ,JEPH)=  GEE
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =  CFTOT_D(2,N)*QS /RMASS
      ENDDO
      IEQ = JEP
      ASYS(IEQ,JEV) = -CMTOT_U(1,2)*QSB/RINXX / VEE
      ASYS(IEQ,JEP) =  CMTOT_U(1,4)*QSB/RINXX / ROT
      ASYS(IEQ,JER) =  CMTOT_U(1,6)*QSB/RINXX / ROT
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =  CMTOT_D(1,N)*QSB/RINXX
      ENDDO
      IEQ = JER
      ASYS(IEQ,JEV) = -CMTOT_U(3,2)*QSB/RINZZ / VEE
      ASYS(IEQ,JEP) =  CMTOT_U(3,4)*QSB/RINZZ / ROT
      ASYS(IEQ,JER) =  CMTOT_U(3,6)*QSB/RINZZ / ROT
      DO N = 1, NCONTROL
        BSYS(IEQ,N) =  CMTOT_D(3,N)*QSB/RINZZ
      ENDDO
      IEQ = JEPH
      ASYS(IEQ,JEP) =  -1.0
      IEQ = JEPS
      K = 3
      RSYS(IEQ)     = ROT*(  RT(K,1)*WROT(1)
     &                     + RT(K,2)*WROT(2)
     &                     + RT(K,3)*WROT(3) )
      ASYS(IEQ,JEP) =        RT(K,1)
      ASYS(IEQ,JEQ) =        RT(K,2)
      ASYS(IEQ,JER) =        RT(K,3)
      ASYS(IEQ,JEPH)= ROT*(  RT_ANG(K,1,1)*WROT(1)
     &                     + RT_ANG(K,2,1)*WROT(2)
     &                     + RT_ANG(K,3,1)*WROT(3) )
      ASYS(IEQ,JETH)= ROT*(  RT_ANG(K,1,2)*WROT(1)
     &                     + RT_ANG(K,2,2)*WROT(2)
     &                     + RT_ANG(K,3,2)*WROT(3) )
      ASYS(IEQ,JEPS)= ROT*(  RT_ANG(K,1,3)*WROT(1)
     &                     + RT_ANG(K,2,3)*WROT(2)
     &                     + RT_ANG(K,3,3)*WROT(3) )
      IEQ = JEX
      K = 1
      RSYS(IEQ)     = -(  TT(K,1)*VINF(1)
     &                  + TT(K,2)*VINF(2)
     &                  + TT(K,3)*VINF(3) )*VEE
      ASYS(IEQ,JEU) =     TT(K,1)
      ASYS(IEQ,JEV) =     TT(K,2)
      ASYS(IEQ,JEW) =     TT(K,3)
      ASYS(IEQ,JEPH)= -(  TT_ANG(K,1,1)*VINF(1)
     &                  + TT_ANG(K,2,1)*VINF(2)
     &                  + TT_ANG(K,3,1)*VINF(3) )*VEE
      ASYS(IEQ,JETH)= -(  TT_ANG(K,1,2)*VINF(1)
     &                  + TT_ANG(K,2,2)*VINF(2)
     &                  + TT_ANG(K,3,2)*VINF(3) )*VEE
      ASYS(IEQ,JEPS)= -(  TT_ANG(K,1,3)*VINF(1)
     &                  + TT_ANG(K,2,3)*VINF(2)
     &                  + TT_ANG(K,3,3)*VINF(3) )*VEE
      IEQ = JEY
      K = 2
      RSYS(IEQ)     = -(  TT(K,1)*VINF(1)
     &                  + TT(K,2)*VINF(2)
     &                  + TT(K,3)*VINF(3) )*VEE
      ASYS(IEQ,JEU) =     TT(K,1)
      ASYS(IEQ,JEV) =     TT(K,2)
      ASYS(IEQ,JEW) =     TT(K,3)
      ASYS(IEQ,JEPH)= -(  TT_ANG(K,1,1)*VINF(1)
     &                  + TT_ANG(K,2,1)*VINF(2)
     &                  + TT_ANG(K,3,1)*VINF(3) )*VEE
      ASYS(IEQ,JETH)= -(  TT_ANG(K,1,2)*VINF(1)
     &                  + TT_ANG(K,2,2)*VINF(2)
     &                  + TT_ANG(K,3,2)*VINF(3) )*VEE
      ASYS(IEQ,JEPS)= -(  TT_ANG(K,1,3)*VINF(1)
     &                  + TT_ANG(K,2,3)*VINF(2)
     &                  + TT_ANG(K,3,3)*VINF(3) )*VEE
      IEQ = JEZ
      K = 3
      RSYS(IEQ)     = -(  TT(K,1)*VINF(1)
     &                  + TT(K,2)*VINF(2)
     &                  + TT(K,3)*VINF(3) )*VEE
      ASYS(IEQ,JEU) =     TT(K,1)
      ASYS(IEQ,JEV) =     TT(K,2)
      ASYS(IEQ,JEW) =     TT(K,3)
      ASYS(IEQ,JEPH)= -(  TT_ANG(K,1,1)*VINF(1)
     &                  + TT_ANG(K,2,1)*VINF(2)
     &                  + TT_ANG(K,3,1)*VINF(3) )*VEE
      ASYS(IEQ,JETH)= -(  TT_ANG(K,1,2)*VINF(1)
     &                  + TT_ANG(K,2,2)*VINF(2)
     &                  + TT_ANG(K,3,2)*VINF(3) )*VEE
      ASYS(IEQ,JEPS)= -(  TT_ANG(K,1,3)*VINF(1)
     &                  + TT_ANG(K,2,3)*VINF(2)
     &                  + TT_ANG(K,3,3)*VINF(3) )*VEE
      RETURN
      END
