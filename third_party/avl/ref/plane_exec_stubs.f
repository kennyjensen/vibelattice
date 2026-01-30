      SUBROUTINE GETSA(LSA,SATYPE,DIR)
      LOGICAL LSA
      CHARACTER*(*) SATYPE
      IF(LSA) THEN
        SATYPE = 'Standard axis orientation,  X fwd, Z down'
        DIR = -1.0
      ELSE
        SATYPE = 'Geometric axis orientation,  X aft, Z up  '
        DIR = 1.0
      ENDIF
      RETURN
      END

      SUBROUTINE TRMSET
      RETURN
      END

      SUBROUTINE OUTHINGE
      RETURN
      END

      SUBROUTINE RUNGET
      RETURN
      END

      SUBROUTINE LOWRIT
      RETURN
      END

      SUBROUTINE CLRZOOM
      RETURN
      END

      SUBROUTINE PARMOD
      RETURN
      END

      SUBROUTINE OUTELE
      RETURN
      END

      SUBROUTINE OUTTOT
      RETURN
      END

      SUBROUTINE OUTSURF
      RETURN
      END

      SUBROUTINE OUTSTRP
      RETURN
      END

      SUBROUTINE PLEND
      RETURN
      END

      SUBROUTINE PLOTVL
      RETURN
      END

      SUBROUTINE MRFSTRP
      RETURN
      END

      SUBROUTINE MRFSURF
      RETURN
      END

      SUBROUTINE MRFTOT
      RETURN
      END

      SUBROUTINE PLOTTP
      RETURN
      END

      SUBROUTINE MRFELE
      RETURN
      END

      SUBROUTINE OBOPER
      RETURN
      END

      SUBROUTINE RUNSAV
      RETURN
      END

      SUBROUTINE MRFCNC
      RETURN
      END

      SUBROUTINE MRFVM
      RETURN
      END

      SUBROUTINE MRFHINGE
      RETURN
      END

      SUBROUTINE MRFBODY
      RETURN
      END

      SUBROUTINE OUTVM
      RETURN
      END

      SUBROUTINE OUTCNC
      RETURN
      END

      SUBROUTINE DERMATM
      RETURN
      END

      SUBROUTINE OUTBODY
      RETURN
      END

      SUBROUTINE DERMATB
      RETURN
      END

      SUBROUTINE OUTSTRPB
      RETURN
      END

      SUBROUTINE DERMATS
      RETURN
      END

      SUBROUTINE CPOML
      RETURN
      END
