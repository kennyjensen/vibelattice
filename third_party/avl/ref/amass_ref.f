      PROGRAM AMASS_REF
      INCLUDE 'AVL.INC'
      INTEGER I, J
      REAL UNITF, UNITS, UNITV, UNITA, UNITI, UNITD
      REAL RINV
      LOGICAL ERROR
      CHARACTER*256 MFNAME
C
C----- MASINI
      CALL MASINI
      WRITE(*,'(1X,ES15.7)') RMASS0
      WRITE(*,'(3(1X,ES15.7))') (XYZMASS0(I), I=1,3)
      WRITE(*,'(3(1X,ES15.7))') RINER0(1,1), RINER0(2,2), RINER0(3,3)
      IF(LMASS) THEN
        WRITE(*,'(1X,I6)') 1
      ELSE
        WRITE(*,'(1X,I6)') 0
      ENDIF
C
C----- UNITSET with custom units
      UNITL = 2.0
      UNITM = 3.0
      UNITT = 4.0
      UNCHL = 'm'
      UNCHM = 'kg'
      UNCHT = 's'
      CALL UNITSET
      WRITE(*,'(6(1X,ES15.7))') UNITF, UNITS, UNITV, UNITA, UNITI, UNITD
C
C----- APPGET setup
      PI = 4.0*ATAN(1.0)
      NSTRIP = 2
      DO J = 1, NSTRIP
        CHORD(J) = 1.0 + 0.2*J
        WSTRIP(J) = 0.5 + 0.1*J
        ENSY(J) = 0.1*J
        ENSZ(J) = 0.2*J
        RLE1(1,J) = 0.1*J
        RLE1(2,J) = 0.2*J
        RLE1(3,J) = 0.3*J
        RLE2(1,J) = 0.4*J
        RLE2(2,J) = 0.5*J
        RLE2(3,J) = 0.6*J
        CHORD1(J) = 0.8 + 0.1*J
        CHORD2(J) = 1.1 + 0.1*J
        RLE(1,J) = 0.05*J
        RLE(2,J) = 0.06*J
        RLE(3,J) = 0.07*J
      END DO
      CALL APPGET
      WRITE(*,'(9(1X,ES15.7))') ((AMASS(I,J), J=1,3), I=1,3)
      WRITE(*,'(9(1X,ES15.7))') ((AINER(I,J), J=1,3), I=1,3)
C
C----- MASPUT setup
      DO I = 1, IPTOT
        PARVAL(I,1) = 0.
      END DO
C
      RMASS0 = 5.0
      RINER0(1,1) = 1.1
      RINER0(2,2) = 2.2
      RINER0(3,3) = 3.3
      RINER0(1,2) = -0.1
      RINER0(2,3) = -0.2
      RINER0(3,1) = -0.3
      GEE0 = 9.81
      RHO0 = 1.225
      XYZMASS0(1) = 0.4
      XYZMASS0(2) = -0.5
      XYZMASS0(3) = 0.6
      UNITL = 2.0
      CALL MASPUT(1,1)
      WRITE(*,'(12(1X,ES15.7))') (PARVAL(I,1), I=1,12)
C
C----- MASGET setup
      MFNAME = 'amass_test.mass'
      OPEN(77,FILE=MFNAME,STATUS='UNKNOWN')
      WRITE(77,'(A)') '# comment'
      WRITE(77,'(A)') 'Lunit = 2.0 m'
      WRITE(77,'(A)') 'Munit = 3.0 kg'
      WRITE(77,'(A)') 'Tunit = 4.0 s'
      WRITE(77,'(A)') 'g = 9.81 m/s^2'
      WRITE(77,'(A)') 'rho = 1.2 kg/m^3'
      WRITE(77,'(A)') '* 2 3 4 5 6 7 8 9 10 11'
      WRITE(77,'(A)') '+ 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0'
      WRITE(77,'(A)') '1 1 2 3 0.1 0.2 0.3 0.01 0.02 0.03'
      WRITE(77,'(A)') '2 0.5 1.5 -0.5 0.2 0.1 0.05 0.02 -0.01 0.04'
      CLOSE(77)
C
      CALL MASGET(77,MFNAME,ERROR)
      WRITE(*,'(1X,ES15.7)') RMASS0
      WRITE(*,'(3(1X,ES15.7))') (XYZMASS0(I), I=1,3)
      WRITE(*,'(9(1X,ES15.7))') ((RINER0(I,J), J=1,3), I=1,3)
      WRITE(*,'(2(1X,ES15.7))') GEE0, RHO0
      WRITE(*,'(3(1X,ES15.7))') UNITL, UNITM, UNITT
      IF(LMASS) THEN
        WRITE(*,'(1X,I6)') 1
      ELSE
        WRITE(*,'(1X,I6)') 0
      ENDIF
C
      END
