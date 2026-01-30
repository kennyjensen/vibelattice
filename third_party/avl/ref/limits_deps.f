      SUBROUTINE VIEWPROJ(XYZ,N,XYZPROJ)
      REAL XYZ(3,N), XYZPROJ(3,N)
      COMMON /VIEWDATA/ RINV,
     &       XIHAT, YIHAT, ZIHAT,
     &       XJHAT, YJHAT, ZJHAT,
     &       XKHAT, YKHAT, ZKHAT
      DO I=1, N
        RDOTI = XYZ(1,I)*XIHAT + XYZ(2,I)*YIHAT + XYZ(3,I)*ZIHAT
        RDOTJ = XYZ(1,I)*XJHAT + XYZ(2,I)*YJHAT + XYZ(3,I)*ZJHAT
        RDOTK = XYZ(1,I)*XKHAT + XYZ(2,I)*YKHAT + XYZ(3,I)*ZKHAT
        RKX = RDOTK*XKHAT
        RKY = RDOTK*YKHAT
        RKZ = RDOTK*ZKHAT
        VSCAL = 1.0 / SQRT( (XKHAT-RINV*RKX)**2
     &                    + (YKHAT-RINV*RKY)**2
     &                    + (ZKHAT-RINV*RKZ)**2 )
        XYZPROJ(1,I) = VSCAL * RDOTI
        XYZPROJ(2,I) = VSCAL * RDOTJ
        XYZPROJ(3,I) = VSCAL * RDOTK
      END DO
      RETURN
      END

      SUBROUTINE AXISADJ(XMIN,XMAX,XSPAN,DELTAX,NTICS)
      REAL XMIN,XMAX,XSPAN,DELTAX,XINC,XINCTBL(5)
      INTEGER NTICS,I
      DATA XINCTBL / 0.1, 0.2, 0.25, 0.5, 1.0 /
      XSPAN1 = XMAX-XMIN
      IF (XSPAN1.EQ.0.) XSPAN1 = 1.
      XPON = IFIX(LOG10(XSPAN1))
      XSPAN = XSPAN1 / 10.**XPON
      DO I = 1, 5
        XINC = XINCTBL(I)
        NTICS = 1 + IFIX(XSPAN/XINC + 0.1)
        IF (NTICS.LE.6) GO TO 1
      END DO
   1  DELTAX = XINC*10.**XPON
      XMIN = DELTAX*  IFLOOR(XMIN/DELTAX)
      XMAX = DELTAX* ICEILING(XMAX/DELTAX)
      XSPAN = XMAX - XMIN
      NTICS = 1 + IFIX(XSPAN/DELTAX + 0.1)
      RETURN
      END

      INTEGER FUNCTION ICEILING(X)
      REAL X
      I = IFIX(X)
      IF(X-I.GT.0.) I = I+1
      ICEILING = I
      RETURN
      END

      INTEGER FUNCTION IFLOOR(X)
      REAL X
      I = IFIX(X)
      IF(X-I.LT.0.) I = I-1
      IFLOOR = I
      RETURN
      END

      SUBROUTINE TETRAN(R,TT,RREF,DR)
      REAL R(3), TT(3,3), RREF(3), DR(3)
      REAL RB(3)
      RB(1) = R(1) - RREF(1)
      RB(2) = R(2) - RREF(2)
      RB(3) = R(3) - RREF(3)
      DO K = 1, 3
        R(K) = TT(K,1)*RB(1)
     &       + TT(K,2)*RB(2)
     &       + TT(K,3)*RB(3) + RREF(K) + DR(K)
      ENDDO
      RETURN
      END

      SUBROUTINE TRIINIT(ID,NROWS,NCOLS,PTS, NTRI,TRI)
      REAL PTS(3,*), TRI(16,*)
      INTEGER ID,NROWS,NCOLS,NTRI
      INTEGER IT, IDX, J, K, IP1, IP2, IP3, IP4
      IT = NTRI
      IDX = ID - 1
      DO 110 J = 1, NCOLS
        DO 1110 K = 1, NROWS
          IDX  = IDX + 1
          IP1 = (NROWS+1)*(J-1) + K
          IP2 = IP1 + 1
          IP3 = IP1 + NROWS+1
          IP4 = IP3 + 1
          IT = IT+1
          TRI(1,IT) = PTS(1,IP1)
          TRI(2,IT) = PTS(2,IP1)
          TRI(3,IT) = PTS(3,IP1)
          TRI(4,IT) = PTS(1,IP2)
          TRI(5,IT) = PTS(2,IP2)
          TRI(6,IT) = PTS(3,IP2)
          TRI(7,IT) = PTS(1,IP3)
          TRI(8,IT) = PTS(2,IP3)
          TRI(9,IT) = PTS(3,IP3)
          TRI(10,IT) = MIN( TRI(1,IT) , TRI(4,IT) , TRI(7,IT) )
          TRI(11,IT) = MIN( TRI(2,IT) , TRI(5,IT) , TRI(8,IT) )
          TRI(12,IT) = MIN( TRI(3,IT) , TRI(6,IT) , TRI(9,IT) )
          TRI(13,IT) = MAX( TRI(1,IT) , TRI(4,IT) , TRI(7,IT) )
          TRI(14,IT) = MAX( TRI(2,IT) , TRI(5,IT) , TRI(8,IT) )
          TRI(15,IT) = MAX( TRI(3,IT) , TRI(6,IT) , TRI(9,IT) )
          TRI(16,IT) = FLOAT(IDX)
          IT = IT+1
          TRI(1,IT) = PTS(1,IP3)
          TRI(2,IT) = PTS(2,IP3)
          TRI(3,IT) = PTS(3,IP3)
          TRI(4,IT) = PTS(1,IP2)
          TRI(5,IT) = PTS(2,IP2)
          TRI(6,IT) = PTS(3,IP2)
          TRI(7,IT) = PTS(1,IP4)
          TRI(8,IT) = PTS(2,IP4)
          TRI(9,IT) = PTS(3,IP4)
          TRI(10,IT) = MIN( TRI(1,IT) , TRI(4,IT) , TRI(7,IT) )
          TRI(11,IT) = MIN( TRI(2,IT) , TRI(5,IT) , TRI(8,IT) )
          TRI(12,IT) = MIN( TRI(3,IT) , TRI(6,IT) , TRI(9,IT) )
          TRI(13,IT) = MAX( TRI(1,IT) , TRI(4,IT) , TRI(7,IT) )
          TRI(14,IT) = MAX( TRI(2,IT) , TRI(5,IT) , TRI(8,IT) )
          TRI(15,IT) = MAX( TRI(3,IT) , TRI(6,IT) , TRI(9,IT) )
          TRI(16,IT) = FLOAT(IDX)
 1110   CONTINUE
  110 CONTINUE
      NTRI = IT
      ID = IDX
      RETURN
      END
