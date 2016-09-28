C-----------------------------------------------------------------------
C  REPPARMS IS A PREPROCESSING PROGRAM WHICH READS INSTRUCTIONS FROM
C  THE AUTOBATT PROGRAM GIVING REPLACEMENT VARIABLES AND VALUES
C  FOR CHANGING PARAMETERS IN A SKELETON BATCH DECK AND SUBMITTING
C  THE PROCESSED DECK AS THE NEXT STEP IN AN AUTOBATT TASK LIST
C-----------------------------------------------------------------------

      CHARACTER*10 TVARS(100),TVALS(100)
      CHARACTER STRING*72,CARD*72

      INTEGER   NVAR,NVARS(100),NVAL,NVALS(100)
      LOGICAL   VAR,VAL

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

1     FORMAT(A72)

C  READ THE PARMS AND MAKE TABELS OF VARIABLES AND VALUES
C  ------------------------------------------------------

      NVAR = 0
      NVAL = 0

10    READ(8,1,END=40) CARD

      VAR = .TRUE.
      VAL = .FALSE.
      ICOL = 0

      DO 30 I=1,72
      IF(I.LE.ICOL                 ) GOTO 30
      IF(VAR .AND. CARD(I:I).EQ.' ') GOTO 30
      IF(VAL .AND. CARD(I:I).EQ.' ') GOTO 904

      IF(VAR) THEN
         IF(CARD(I:I).EQ.'=') GOTO 900
         NVAR = NVAR + 1
         IF(NVAR.GT.100)    GOTO 906
         NVARS(NVAR) = 0
         DO 15 J=1,11
         ICOL = I+J-1
         IF(CARD(ICOL:ICOL).EQ.'=') GOTO 25
         NVARS(NVAR) = NVARS(NVAR)+1
         TVARS(NVAR)(NVARS(NVAR):NVARS(NVAR)) = CARD(ICOL:ICOL)
15       CONTINUE
         GOTO 901
      ENDIF

      IF(VAL) THEN
         IF(CARD(I:I).EQ.'=') GOTO 902
         NVAL = NVAL + 1
         IF(NVAL.GT.100)    GOTO 906
         NVALS(NVAL) = 0
         DO 20 J=1,11
         ICOL = I+J-1
         IF(CARD(ICOL:ICOL).EQ.' ') GOTO 25
         NVALS(NVAL) = NVALS(NVAL)+1
         TVALS(NVAL)(NVALS(NVAL):NVALS(NVAL)) = CARD(ICOL:ICOL)
20       CONTINUE
         GOTO 903
      ENDIF

25    VAR = .NOT.VAR
      VAL = .NOT.VAL

30    CONTINUE
      GOTO 10

40    IF(NVAR.NE.NVAL) GOTO 905

C  READ THROUGH THE SKELETON DECK AND REPLACE VARS WITH VALS
C  ---------------------------------------------------------

50    READ(9,1,END=100)CARD

      IF(NVAR.EQ.0          ) GOTO 85

      DO 80 I1=1,NVAR
      ICOL=0
      DO 80 I=1,72
      IF(I.LT.ICOL) GOTO 80

      IF(CARD(I:I+NVARS(I1)-1) .NE. TVARS(I1)(1:NVARS(I1))) GOTO 80

      I2 = I+NVARS(I1)
      I3 = I+NVALS(I1)
      I4 = 72-I2+1
      I5 = 72-I3+1
      STRING = ' '
      STRING(1:I4) = CARD(I2:I4+I2-1)
      CARD(I3:72) = STRING(1:I5)

      CARD(I:I+NVALS(I1)-1) = TVALS(I1)(1:NVALS(I1))

      ICOL = I+NVALS(I1)

80    CONTINUE

C  WRITE OUT THE CARD - CHANGED OR OTHERWISE
C  -----------------------------------------

85    WRITE(11,1) CARD
      GOTO 50

100   STOP

C  ERROR EXITS OF VARIOUS SORTS
C  ----------------------------

900   PRINT*,'VAR NAME IS NULL'
      STOP 900
901   PRINT*,'VAR NAME IS TOO LONG'
      STOP 901
902   PRINT*,'VAL HAS EQUAL SIGN IN IT'
      STOP 902
903   PRINT*,'VAL NAME IS TOO LONG'
      STOP 903
904   PRINT*,'VAL NAME IS NULL'
      STOP 904
905   PRINT*,'NVAR NE NVAL - CALL THE FIRE DEPARTMENT'
      STOP 905
906   PRINT*,'TOO MANY PARMS FOR THE PARM TABLES'
      STOP 906
      END
