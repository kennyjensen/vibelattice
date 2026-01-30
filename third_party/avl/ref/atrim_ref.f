      PROGRAM ATRIM_REF
      INCLUDE 'AVL.INC'
      LOGICAL LMATCH
      CHARACTER*4 COMAND
      CHARACTER*2 COMARG
      INTEGER IR, IP, IV, IC
C
      PI = 4.0*ATAN(1.0)
      DTR = PI/180.0
      UNITL = 2.0
      CREF = 1.5
      BREF = 2.5
      SREF = 3.0
      RHO0 = 1.2
      GEE0 = 9.81
      RMASS0 = 100.0
C
      NRUN = 2
      IRUN = 1
      NVTOT = IVTOT
C
      DO IR = 1, NRUN
        ITRIM(IR) = 0
        DO IP = 1, IPTOT
          PARVAL(IP,IR) = 0.0
        END DO
        DO IV = 1, IVTOT
          ICON(IV,IR) = 0
        END DO
        DO IC = 1, ICTOT
          CONVAL(IC,IR) = 0.0
        END DO
      END DO
C
      CONVAL(ICCL,1) = 0.7
      ICON(IVALFA,1) = ICCL
      PARVAL(IPPHI,1) = 20.0
      PARVAL(IPVEE,1) = 0.0
      PARVAL(IPCL,1) = 0.0
      PARVAL(IPRHO,1) = 0.0
      PARVAL(IPGEE,1) = 0.0
      PARVAL(IPMASS,1) = 0.0
      PARVAL(IPFAC,1) = 0.0
      PARVAL(IPRAD,1) = 0.0
C
      PARVAL(IPPHI,2) = 0.0
      PARVAL(IPCL,2) = 0.6
      PARVAL(IPVEE,2) = 50.0
      PARVAL(IPRAD,2) = 0.0
      PARVAL(IPFAC,2) = 0.0
      PARVAL(IPRHO,2) = 1.1
      PARVAL(IPGEE,2) = 9.8
      PARVAL(IPMASS,2) = 120.0
C
      COMAND = 'C1'
      COMARG = '1 '
      IR = 1
      CALL TRMSET(COMAND, COMARG, LMATCH, IR)
C
      COMAND = 'C2'
      COMARG = '2 '
      IR = 2
      CALL TRMSET(COMAND, COMARG, LMATCH, IR)
C
      DO IR = 1, NRUN
        WRITE(*,*) PARVAL(IPVEE,IR), PARVAL(IPCL,IR),
     &             PARVAL(IPRAD,IR), PARVAL(IPFAC,IR),
     &             PARVAL(IPTHE,IR)
        WRITE(*,*) CONVAL(ICCL,IR), CONVAL(ICROTX,IR),
     &             CONVAL(ICROTY,IR), CONVAL(ICROTZ,IR)
        WRITE(*,*) ICON(IVALFA,IR), ICON(IVROTX,IR),
     &             ICON(IVROTY,IR), ICON(IVROTZ,IR)
        WRITE(*,*) ITRIM(IR)
      END DO
C
      END
