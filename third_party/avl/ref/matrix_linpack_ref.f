      PROGRAM MATRIX_LINPACK_REF
      INTEGER NC, I, N, R, C
      INTEGER NMAX
      PARAMETER (NMAX=10)
      REAL A(NMAX,NMAX), B(NMAX), WORK(NMAX)
      INTEGER INDX(NMAX)

      READ(*,*,END=900) NC
      DO 100 I = 1, NC
        READ(*,*,END=900) N
        READ(*,*,END=900) ((A(R,C), C=1,N), R=1,N)
        READ(*,*,END=900) (B(R), R=1,N)
        CALL LUDCMP(NMAX, N, A, INDX, WORK)
        CALL BAKSUB(NMAX, N, A, INDX, B)
        WRITE(*,'(200(1X,ES15.7))') (B(R), R=1,N)
  100 CONTINUE
  900 CONTINUE
      END
