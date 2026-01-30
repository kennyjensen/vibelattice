      PROGRAM EXEC_REF
      USE avl_heap_inc
      INCLUDE 'AVL.INC'
      INTEGER I, K, IU, J
      INTEGER IR
C
      IR = 1
      NVOR = 1
      NLNODE = 0
      NCONTROL = 0
      NDESIGN = 0
      NVTOT = IVTOT
      NSTRIP = 0
      NSURF = 0
      NBODY = 0
      IYSYM = 0
      IZSYM = 0
C
      PI = 3.1415927
      DTR = PI/180.0
C
      SREF = 1.0
      CREF = 1.0
      BREF = 1.0
C
      ALLOCATE(AICN(NVMAX,NVMAX))
      ALLOCATE(WC_GAM(3,NVOR,NVOR))
      ALLOCATE(WV_GAM(3,NVOR,NVOR))
C
      DO I = 1, NVMAX
        DO J = 1, NVMAX
          AICN(I,J) = 0.0
        END DO
      END DO
      AICN(1,1) = 1.0
      IAPIV(1) = 1
C
      DO K = 1, 3
        DO I = 1, NVOR
          DO IU = 1, NVOR
            WC_GAM(K,I,IU) = 0.1D0*K
            WV_GAM(K,I,IU) = 0.2D0*K
          END DO
        END DO
      END DO
C
      DO K = 1, 3
        ENC(K,1) = 0.0
      END DO
      ENC(3,1) = 1.0
      LVNC(1) = .TRUE.
      LVALBE(1) = .FALSE.
C
      DO K = 1, 3
        DO IU = 1, NUMAX
          WCSRD_U(K,1,IU) = 0.0
          WVSRD_U(K,1,IU) = 0.0
        END DO
      END DO
C
      DO I = 1, NVMAX
        GAM(I) = 0.0
        DO IU = 1, NUMAX
          GAM_U_0(I,IU) = 0.0
          GAM_U(I,IU) = 0.0
        END DO
      END DO
C
      LNASA_SA = .FALSE.
      LSA_RATES = .FALSE.
      LAIC = .TRUE.
      LSRD = .TRUE.
      LVEL = .TRUE.
C
      AMACH = 0.2
      PARVAL(IPMACH,IR) = 0.2
      PARVAL(IPCD0,IR) = 0.01
      PARVAL(IPXCG,IR) = 0.1
      PARVAL(IPYCG,IR) = 0.2
      PARVAL(IPZCG,IR) = 0.3
C
      ALFA = 0.1
      BETA = 0.05
      WROT(1) = 0.01
      WROT(2) = -0.02
      WROT(3) = 0.03
C
      CALL EXEC(0,0,IR)
C
      WRITE(*,*) VINF(1), VINF(2), VINF(3)
      WRITE(*,*) WROT(1), WROT(2), WROT(3)
      WRITE(*,*) GAM(1)
      WRITE(*,*) WC(1,1), WC(2,1), WC(3,1)
      WRITE(*,*) WV(1,1), WV(2,1), WV(3,1)
      WRITE(*,*) PARVAL(IPALFA,IR), PARVAL(IPBETA,IR), PARVAL(IPCL,IR)
C
      END


      SUBROUTINE EXEC(NITER,INFO,IR)
C---------------------------------------------------
C     Solves for the flow condition specified by
C     the global operating parameters:
C
C       CONVAL(ICALFA)     alpha (deg)
C       CONVAL(ICBETA)     beta  (deg)
C       CONVAL(ICROTX)     roll_rate * Bref / 2V
C       CONVAL(ICROTY)    pitch_rate * Cref / 2V
C       CONVAL(ICROTZ)      yaw_rate * Bref / 2V
C        .
C        .
C
C---------------------------------------------------
      INCLUDE 'AVL.INC'
      REAL*8 VSYS(IVMAX,IVMAX), VRES(IVMAX), DDC(NDMAX), WORK(IVMAX)
      INTEGER IVSYS(IVMAX)
C
C---- convergence epsilon, max angle limit (90 deg in radians)
      DATA EPS, DMAX / 0.00002, 1.5708 /
C
      IF(LNASA_SA) THEN
C----- NASA Std. Stability axes, X fwd, Z down
       DIR = -1.0
      ELSE
C----- Geometric Stability axes, X aft, Z up
       DIR =  1.0
      ENDIF
C===================================================================
C---- start a new solution
      LSOL = .FALSE.
C
      XYZREF(1) = PARVAL(IPXCG,IR)
      XYZREF(2) = PARVAL(IPYCG,IR)
      XYZREF(3) = PARVAL(IPZCG,IR)
C
      CDREF = PARVAL(IPCD0,IR)
C
      MACH = PARVAL(IPMACH,IR)
C
      IF(MACH.NE.AMACH) THEN
C----- new Mach number invalidates close to everything that's stored
       LAIC = .FALSE.
       LSRD = .FALSE.
       LVEL = .FALSE.
       LSOL = .FALSE.
       LSEN = .FALSE.
       LOBAIC = .FALSE.
       LOBVEL = .FALSE.
      ENDIF
C
C---- set, factor AIC matrix and induced-velocity matrix (if they don't exist)
      CALL SETUP
C
      IF(NITER.GT.0) THEN
C----- might as well directly set operating variables if they are known
       IF(ICON(IVALFA,IR).EQ.ICALFA) ALFA    = CONVAL(ICALFA,IR)*DTR
       IF(ICON(IVBETA,IR).EQ.ICBETA) BETA    = CONVAL(ICBETA,IR)*DTR
       IF(ICON(IVROTX,IR).EQ.ICROTX) WROT(1) = CONVAL(ICROTX,IR)*2./BREF
       IF(ICON(IVROTY,IR).EQ.ICROTY) WROT(2) = CONVAL(ICROTY,IR)*2./CREF
       IF(ICON(IVROTZ,IR).EQ.ICROTZ) WROT(3) = CONVAL(ICROTZ,IR)*2./BREF
      ENDIF
C
C----- set GAM_U
      CALL GUCALC
C
C-------------------------------------------------------------
C---- calculate initial operating state
C
C---- set VINF() vector from initial ALFA,BETA
      CALL VINFAB
C
C---- sum AIC matrices to get GAM,SRC,DBL
      CALL GAMSUM
C
C---- sum AIC matrices to get WC,WV
      CALL VELSUM
C
C---- compute forces
      CALL AERO
C
C---- Newton loop for operating variables
      DO 190 ITER = 1, NITER
C
        IF(LSA_RATES) THEN
C-------- rates specified in NASA stability-axes, transform to body axes
          CA = COS(ALFA)
          SA = SIN(ALFA)
          CA_A = -SA
          SA_A =  CA
         ELSE
C-------- rates specified in body-axes, no transformation
          CA = 1.0
          SA = 0.0
          CA_A = 0.
          SA_A = 0.
        ENDIF
C
        DO K=1, IVMAX
          DO L=1, IVMAX
            VSYS(K,L) = 0.
          ENDDO
        ENDDO
C
C------ set up Newton system:  set constraints for all parameters
        DO 100 IV = 1, NVTOT
C
C-------- set index and value of constraint for this parameter
          IC = ICON(IV,IR)
C
C------------------------------------
          IF    (IC.EQ.ICALFA) THEN
           VRES(IV) = ALFA - CONVAL(IC,IR)*DTR
           VSYS(IV,IVALFA) = 1.0
C
C------------------------------------
          ELSEIF(IC.EQ.ICBETA) THEN
           VRES(IV) = BETA - CONVAL(IC,IR)*DTR
           VSYS(IV,IVBETA) = 1.0
C
C------------------------------------
          ELSEIF(IC.EQ.ICROTX) THEN
           VRES(IV) = (WROT(1)*CA + WROT(3)*SA)*DIR
     &              - CONVAL(IC,IR)*2.0/BREF
           VSYS(IV,IVROTX) = CA*DIR
           VSYS(IV,IVROTZ) = SA*DIR
           VSYS(IV,IVALFA) = (WROT(1)*CA_A + WROT(3)*SA_A)*DIR
C
C------------------------------------
          ELSEIF(IC.EQ.ICROTY) THEN
           VRES(IV) = WROT(2)
     &              - CONVAL(IC,IR)*2.0/CREF
           VSYS(IV,IVROTY) = 1.0
C
C------------------------------------
          ELSEIF(IC.EQ.ICROTZ) THEN
           VRES(IV) = (WROT(3)*CA - WROT(1)*SA)*DIR
     &              - CONVAL(IC,IR)*2.0/BREF
           VSYS(IV,IVROTX) = -SA*DIR
           VSYS(IV,IVROTZ) =  CA*DIR
           VSYS(IV,IVALFA) = (WROT(3)*CA_A - WROT(1)*SA_A)*DIR
C
C------------------------------------
          ELSEIF(IC.EQ.ICCL  ) THEN
           VRES(IV) = CLTOT - CONVAL(IC,IR)
           VSYS(IV,IVALFA) = CLTOT_U(1)*VINF_A(1)
     &                     + CLTOT_U(2)*VINF_A(2)
     &                     + CLTOT_U(3)*VINF_A(3) + CLTOT_A
           VSYS(IV,IVBETA) = CLTOT_U(1)*VINF_B(1)
     &                     + CLTOT_U(2)*VINF_B(2)
     &                     + CLTOT_U(3)*VINF_B(3)
           VSYS(IV,IVROTX) = CLTOT_U(4)
           VSYS(IV,IVROTY) = CLTOT_U(5)
           VSYS(IV,IVROTZ) = CLTOT_U(6)
C
           DO N = 1, NCONTROL
             NV = IVTOT + N
             VSYS(IV,NV) = CLTOT_D(N)
           ENDDO
C
C------------------------------------
          ELSEIF(IC.EQ.ICCY  ) THEN
           VRES(IV) = CYTOT - CONVAL(IC,IR)
           VSYS(IV,IVALFA) = CYTOT_U(1)*VINF_A(1)
     &                     + CYTOT_U(2)*VINF_A(2)
     &                     + CYTOT_U(3)*VINF_A(3)
           VSYS(IV,IVBETA) = CYTOT_U(1)*VINF_B(1)
     &                     + CYTOT_U(2)*VINF_B(2)
     &                     + CYTOT_U(3)*VINF_B(3)
           VSYS(IV,IVROTX) = CYTOT_U(4)
           VSYS(IV,IVROTY) = CYTOT_U(5)
           VSYS(IV,IVROTZ) = CYTOT_U(6)
C
           DO N = 1, NCONTROL
             NV = IVTOT + N
             VSYS(IV,NV) = CYTOT_D(N)
           ENDDO
C
C------------------------------------
          ELSEIF(IC.EQ.ICMOMX) THEN
           VRES(IV) = (CMTOT(1)*CA + CMTOT(3)*SA)*DIR - CONVAL(IC,IR)
           VSYS(IV,IVALFA) = ( CMTOT_U(1,1)*VINF_A(1)
     &                        +CMTOT_U(1,2)*VINF_A(2)
     &                        +CMTOT_U(1,3)*VINF_A(3))*CA*DIR
     &                     + ( CMTOT_U(3,1)*VINF_A(1)
     &                        +CMTOT_U(3,2)*VINF_A(2)
     &                        +CMTOT_U(3,3)*VINF_A(3))*SA*DIR
     &                     + (CMTOT(1)*CA_A + CMTOT(3)*SA_A)*DIR
           VSYS(IV,IVBETA) = ( CMTOT_U(1,1)*VINF_B(1)
     &                        +CMTOT_U(1,2)*VINF_B(2)
     &                        +CMTOT_U(1,3)*VINF_B(3))*CA*DIR
     &                     + ( CMTOT_U(3,1)*VINF_B(1)
     &                        +CMTOT_U(3,2)*VINF_B(2)
     &                        +CMTOT_U(3,3)*VINF_B(3))*SA*DIR
           VSYS(IV,IVROTX) = (CMTOT_U(1,4)*CA + CMTOT_U(3,4)*SA)*DIR
           VSYS(IV,IVROTY) = (CMTOT_U(1,5)*CA + CMTOT_U(3,5)*SA)*DIR
           VSYS(IV,IVROTZ) = (CMTOT_U(1,6)*CA + CMTOT_U(3,6)*SA)*DIR
C
           DO N = 1, NCONTROL
             NV = IVTOT + N
             VSYS(IV,NV) = (CMTOT_D(1,N)*CA + CMTOT_D(3,N)*SA)*DIR
           ENDDO
C
C------------------------------------
          ELSEIF(IC.EQ.ICMOMY) THEN
           VRES(IV) = CMTOT(2) - CONVAL(IC,IR)
           VSYS(IV,IVALFA) = CMTOT_U(2,1)*VINF_A(1)
     &                     + CMTOT_U(2,2)*VINF_A(2)
     &                     + CMTOT_U(2,3)*VINF_A(3)
           VSYS(IV,IVBETA) = CMTOT_U(2,1)*VINF_B(1)
     &                     + CMTOT_U(2,2)*VINF_B(2)
     &                     + CMTOT_U(2,3)*VINF_B(3)
           VSYS(IV,IVROTX) = CMTOT_U(2,4)
           VSYS(IV,IVROTY) = CMTOT_U(2,5)
           VSYS(IV,IVROTZ) = CMTOT_U(2,6)
C
           DO N = 1, NCONTROL
             NV = IVTOT + N
             VSYS(IV,NV) = CMTOT_D(2,N)
           ENDDO
C
C------------------------------------
          ELSEIF(IC.EQ.ICMOMZ) THEN
           VRES(IV) = (CMTOT(3)*CA - CMTOT(1)*SA)*DIR - CONVAL(IC,IR)
           VSYS(IV,IVALFA) = ( CMTOT_U(3,1)*VINF_A(1)
     &                        +CMTOT_U(3,2)*VINF_A(2)
     &                        +CMTOT_U(3,3)*VINF_A(3))*CA*DIR
     &                     - ( CMTOT_U(1,1)*VINF_A(1)
     &                        +CMTOT_U(1,2)*VINF_A(2)
     &                        +CMTOT_U(1,3)*VINF_A(3))*SA*DIR
     &                     + (CMTOT(3)*CA_A - CMTOT(1)*SA_A)*DIR
           VSYS(IV,IVBETA) = ( CMTOT_U(3,1)*VINF_B(1)
     &                        +CMTOT_U(3,2)*VINF_B(2)
     &                        +CMTOT_U(3,3)*VINF_B(3))*CA*DIR
     &                     - ( CMTOT_U(1,1)*VINF_B(1)
     &                        +CMTOT_U(1,2)*VINF_B(2)
     &                        +CMTOT_U(1,3)*VINF_B(3))*SA*DIR
           VSYS(IV,IVROTX) = (CMTOT_U(3,4)*CA - CMTOT_U(1,4)*SA)*DIR
           VSYS(IV,IVROTY) = (CMTOT_U(3,5)*CA - CMTOT_U(1,5)*SA)*DIR
           VSYS(IV,IVROTZ) = (CMTOT_U(3,6)*CA - CMTOT_U(1,6)*SA)*DIR
C
           DO N = 1, NCONTROL
             NV = IVTOT + N
             VSYS(IV,NV) = (CMTOT_D(3,N)*CA - CMTOT_D(1,N)*SA)*DIR
           ENDDO
C
C------------------------------------
          ELSE
           DO N = 1, NCONTROL
             ICCON = ICTOT + N
             IVCON = IVTOT + N
             IF(IC.EQ.ICCON) THEN
              VRES(IV) = DELCON(N) - CONVAL(ICCON,IR)
              VSYS(IV,IVCON) = 1.0
              GO TO 100
             ENDIF
           ENDDO
C
           WRITE(*,*) '? Illegal constraint index: ', IC
          ENDIF
C
 100    CONTINUE
C
C------ LU-factor,  and back-substitute RHS
        CALL LUDCMP(IVMAX,NVTOT,VSYS,IVSYS,WORK)
        CALL BAKSUB(IVMAX,NVTOT,VSYS,IVSYS,VRES)
C
C------ set Newton deltas
        DAL = -VRES(IVALFA)
        DBE = -VRES(IVBETA)
        DWX = -VRES(IVROTX)
        DWY = -VRES(IVROTY)
        DWZ = -VRES(IVROTZ)
        DO N = 1, NCONTROL
          IV = IVTOT + N
          DDC(N) = -VRES(IV)
        ENDDO
C
C------ limits on angles and rates
        DMAXA = DMAX
        DMAXR = 5.0*DMAX/BREF
C
C------ if changes are too big, configuration is probably untrimmable
        IF(ABS(ALFA+DAL).GT.DMAXA) THEN
         WRITE(*,*) 'Cannot trim.  Alpha too large.  a =',(ALFA+DAL)/DTR
         RETURN
        ENDIF
C
        IF(ABS(BETA+DBE).GT.DMAXA) THEN
         WRITE(*,*) 'Cannot trim.  Beta too large.  b =',(BETA+DBE)/DTR
         RETURN
        ENDIF
C
        IF(ABS(WROT(1)+DWX).GT.DMAXR) THEN
         WRITE(*,*) 'Cannot trim.  Roll rate too large.  pb/2V =',
     &               (WROT(1)+DWX)*BREF*0.5
         RETURN
        ENDIF
C
        IF(ABS(WROT(2)+DWY).GT.DMAXR) THEN
         WRITE(*,*) 'Cannot trim.  Pitch rate too large.  qc/2V =',
     &               (WROT(2)+DWY)*CREF*0.5
         RETURN
        ENDIF
C
        IF(ABS(WROT(3)+DWZ).GT.DMAXR) THEN
         WRITE(*,*) 'Cannot trim.  Yaw rate too large.  rb/2V =',
     &               (WROT(3)+DWZ)*BREF*0.5
         RETURN
        ENDIF
C
C------ update
        ALFA  = ALFA  + DAL
        BETA  = BETA  + DBE
        WROT(1) = WROT(1) + DWX
        WROT(2) = WROT(2) + DWY
        WROT(3) = WROT(3) + DWZ
        DO K = 1, NCONTROL
          DELCON(K) = DELCON(K) + DDC(K)
        ENDDO
C
C------ set VINF() vector from new ALFA,BETA
        CALL VINFAB
C
        IF(NCONTROL.GT.0) THEN
C------- set new GAM_D
         CALL GDCALC(NCONTROL,LCONDEF,ENC_D,GAM_D)
        ENDIF
C
        IF(NDESIGN.GT.0) THEN
C------- set new GAM_G
         CALL GDCALC(NDESIGN ,LDESDEF,ENC_G,GAM_G)
        ENDIF
C
C------ sum AIC matrices to get GAM,SRC,DBL
        CALL GAMSUM
C
C------ sum AIC matrices to get WC,WV
        CALL VELSUM
C
C------ compute forces
        CALL AERO
C
C------ convergence check
        DELMAX = MAX( ABS(DAL),
     &                ABS(DBE),
     &                ABS(DWX*BREF/2.0),
     &                ABS(DWY*CREF/2.0),
     &                ABS(DWZ*BREF/2.0) )
        DO K = 1, NCONTROL
          DELMAX = MAX( DELMAX , ABS(DDC(K)) )
        ENDDO
C
        IF(DELMAX.LT.EPS) THEN
         LSOL = .TRUE.
         LOBVEL = .FALSE.
C------- mark trim case as being converged
         ITRIM(IR) = IABS(ITRIM(IR))
         GO TO 191
        ENDIF
C
 190  CONTINUE
      IF(NITER.GT.0) THEN
       WRITE(*,*) 'Trim convergence failed'
       LSOL = .FALSE.
       LOBVEL = .FALSE.
       RETURN
      ENDIF
C
 191  CONTINUE
      PARVAL(IPALFA,IR) = ALFA/DTR
      PARVAL(IPBETA,IR) = BETA/DTR
      PARVAL(IPROTX,IR) = WROT(1)*0.5*BREF
      PARVAL(IPROTY,IR) = WROT(2)*0.5*CREF
      PARVAL(IPROTZ,IR) = WROT(3)*0.5*BREF
      PARVAL(IPCL  ,IR) = CLTOT
C
      LSEN = .TRUE.
      RETURN
C
      END ! EXEC


      SUBROUTINE LUDCMP(NSIZ,N,A,INDX,WORK)
C     *******************************************************
C     *   Factors a full NxN matrix into an LU form.        *
C     *   Subr. BAKSUB can back-substitute it with some RHS.*
C     *   Assumes matrix is non-singular...                 *
C     *    ...if it isn't, a divide by zero will result.    *
C     *******************************************************
C
      REAL*8 A(NSIZ,NSIZ), WORK(NSIZ)
      INTEGER INDX(NSIZ)
C
      DO 12 I=1, N
        AAMAX = 0.0D0
        DO 11 J=1, N
          AAMAX = MAX( ABS(A(I,J)) , AAMAX )
 11     CONTINUE
        WORK(I) = 1.0D0/AAMAX
 12   CONTINUE
C
      DO 19 J=1, N
        DO 14 I=1, J-1
          SUM = A(I,J)
          DO 13 K=1, I-1
            SUM = SUM - A(I,K)*A(K,J)
 13       CONTINUE
          A(I,J) = SUM
 14     CONTINUE
C
        AAMAX = 0.0D0
        DO 16 I=J, N
          SUM = A(I,J)
          DO 15 K=1, J-1
            SUM = SUM - A(I,K)*A(K,J)
 15       CONTINUE
          A(I,J) = SUM
          DUM = WORK(I)*ABS(SUM)
          IF(DUM.GE.AAMAX) THEN
            IMAX = I
            AAMAX = DUM
          ENDIF
 16     CONTINUE
C
        IF(J.NE.IMAX) THEN
          DO 17 K=1, N
            DUM = A(IMAX,K)
            A(IMAX,K) = A(J,K)
            A(J,K) = DUM
 17       CONTINUE
          WORK(IMAX) = WORK(J)
        ENDIF
        INDX(J) = IMAX
        IF(J.NE.N) THEN
          DUM = 1.0D0/A(J,J)
          DO 18 I=J+1, N
            A(I,J) = A(I,J)*DUM
 18       CONTINUE
        ENDIF
 19   CONTINUE
C
      RETURN
      END


      SUBROUTINE BAKSUB(NSIZ,N,A,INDX,B)
C     *******************************************************
C     *   Back-substitutes to get x from LUx = b.            *
C     *   A must be the LU factors from LUDCMP.              *
C     *******************************************************
C
      REAL*8 A(NSIZ,NSIZ), B(NSIZ)
      INTEGER INDX(NSIZ)
C
      II = 0
      DO 12 I=1, N
        LL = INDX(I)
        SUM = B(LL)
        B(LL) = B(I)
        IF(II.NE.0) THEN
          DO 11 J=II, I-1
            SUM = SUM - A(I,J)*B(J)
 11       CONTINUE
        ELSEIF(SUM.NE.0.0D0) THEN
          II = I
        ENDIF
        B(I) = SUM
 12   CONTINUE
C
      DO 14 I=N, 1, -1
        SUM = B(I)
        IF(I.LT.N) THEN
          DO 13 J=I+1, N
            SUM = SUM - A(I,J)*B(J)
 13       CONTINUE
        ENDIF
        B(I) = SUM/A(I,I)
 14   CONTINUE
C
      RETURN
      END
