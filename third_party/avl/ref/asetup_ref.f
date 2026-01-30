      PROGRAM ASETUP_REF
      USE avl_heap_inc
      INCLUDE 'AVL.INC'
      INTEGER I, J, K, L, IU, N
C
      NVOR = 2
      NLNODE = 2
      NCONTROL = 1
      NDESIGN = 1
C
      ALLOCATE(WC_GAM(3,NVOR,NVOR))
      ALLOCATE(WV_GAM(3,NVOR,NVOR))
C
      DO K = 1, 3
        DO I = 1, NVOR
          DO J = 1, NVOR
            WC_GAM(K,I,J) = 0.01D0*K + 0.001D0*I + 0.0001D0*J
            WV_GAM(K,I,J) = 0.02D0*K + 0.002D0*I + 0.0002D0*J
          END DO
        END DO
      END DO
C
      VINF(1) = 10.0
      VINF(2) = -2.0
      VINF(3) = 1.0
      WROT(1) = 0.1
      WROT(2) = -0.2
      WROT(3) = 0.3
C
      DO N = 1, NDMAX
        DELCON(N) = 0.0
      END DO
      DO N = 1, NGMAX
        DELDES(N) = 0.0
      END DO
      DELCON(1) = 0.2
      DELDES(1) = -0.1
C
      DO I = 1, NVMAX
        DO IU = 1, NUMAX
          GAM_U_0(I,IU) = 0.0
          DO N = 1, NDMAX
            GAM_U_D(I,IU,N) = 0.0
          END DO
          DO N = 1, NGMAX
            GAM_U_G(I,IU,N) = 0.0
          END DO
          GAM_U(I,IU) = 0.0
        END DO
        DO N = 1, NDMAX
          GAM_D(I,N) = 0.0
        END DO
        DO N = 1, NGMAX
          GAM_G(I,N) = 0.0
        END DO
        GAM(I) = 0.0
      END DO
C
      DO I = 1, NVOR
        DO IU = 1, NUMAX
          GAM_U_0(I,IU) = 0.1*I + 0.01*IU
          GAM_U_D(I,IU,1) = 0.02*I + 0.001*IU
          GAM_U_G(I,IU,1) = -0.03*I + 0.002*IU
        END DO
      END DO
C
      DO L = 1, NLMAX
        SRC(L) = 0.0
        DO IU = 1, NUMAX
          SRC_U(L,IU) = 0.0
          DO K = 1, 3
            DBL_U(K,L,IU) = 0.0
          END DO
        END DO
        DO K = 1, 3
          DBL(K,L) = 0.0
        END DO
      END DO
C
      DO L = 1, NLNODE
        DO IU = 1, NUMAX
          SRC_U(L,IU) = 0.05*L + 0.002*IU
          DO K = 1, 3
            DBL_U(K,L,IU) = 0.01*K + 0.001*L + 0.0005*IU
          END DO
        END DO
      END DO
C
      DO K = 1, 3
        DO I = 1, NVMAX
          VC(K,I) = 0.0
          VV(K,I) = 0.0
          WC(K,I) = 0.0
          WV(K,I) = 0.0
          WCSRD(K,I) = 0.0
          WVSRD(K,I) = 0.0
          DO N = 1, NUMAX
            VC_U(K,I,N) = 0.0
            VV_U(K,I,N) = 0.0
            WC_U(K,I,N) = 0.0
            WV_U(K,I,N) = 0.0
            WCSRD_U(K,I,N) = 0.0
            WVSRD_U(K,I,N) = 0.0
          END DO
          DO N = 1, NDMAX
            VC_D(K,I,N) = 0.0
            VV_D(K,I,N) = 0.0
            WC_D(K,I,N) = 0.0
            WV_D(K,I,N) = 0.0
          END DO
          DO N = 1, NGMAX
            VC_G(K,I,N) = 0.0
            VV_G(K,I,N) = 0.0
            WC_G(K,I,N) = 0.0
            WV_G(K,I,N) = 0.0
          END DO
        END DO
      END DO
C
      DO K = 1, 3
        DO I = 1, NVOR
          DO IU = 1, NUMAX
            WCSRD_U(K,I,IU) = 0.03*K + 0.001*I + 0.0001*IU
            WVSRD_U(K,I,IU) = 0.04*K + 0.0015*I + 0.0002*IU
          END DO
        END DO
      END DO
C
      CALL GAMSUM
      CALL VELSUM
C
      WRITE(*,*) (GAM(I), I=1,NVOR)
      WRITE(*,*) (GAM_D(I,1), I=1,NVOR)
      WRITE(*,*) (GAM_G(I,1), I=1,NVOR)
      WRITE(*,*) (SRC(L), L=1,NLNODE)
      WRITE(*,*) ((DBL(K,L), K=1,3), L=1,NLNODE)
      WRITE(*,*) ((WC(K,I), K=1,3), I=1,NVOR)
      WRITE(*,*) ((WV(K,I), K=1,3), I=1,NVOR)
      WRITE(*,*) ((WC_U(K,I,1), K=1,3), I=1,NVOR)
      WRITE(*,*) ((WV_U(K,I,1), K=1,3), I=1,NVOR)
      WRITE(*,*) ((VC_D(K,I,1), K=1,3), I=1,NVOR)
      WRITE(*,*) ((VC_G(K,I,1), K=1,3), I=1,NVOR)
C
      END
