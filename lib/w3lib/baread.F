#include <define.h>
C-----------------------------------------------------------------------
      SUBROUTINE BAREAD(LU,IB,NB,KA,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAREAD         BYTE-ADDRESSABLE READ
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 94-04-01
C
C ABSTRACT: READ A GIVEN NUMBER OF BYTES FROM AN UNBLOCKED FILE,
C   SKIPPING A GIVEN NUMBER OF BYTES.  THE FILE MUST BE OPENED
C   USING BAOPEN.
C
C PROGRAM HISTORY LOG:
C   94-04-01  IREDELL
C   96-10-01  IREDELL     STANDARD F77 VERSION
C
C USAGE:    CALL BAREAD(LU,IB,NB,KA,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO READ
C     IB           INTEGER NUMBER OF BYTES TO SKIP
C                  (IF IB<0, THEN DO NOT REPOSITION THE FILE)
C     NB           INTEGER NUMBER OF BYTES TO READ
C   OUTPUT ARGUMENTS:
C     KA           INTEGER NUMBER OF BYTES ACTUALLY READ
C     A            CHARACTER*1 (NB) BUFFER READ
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      CHARACTER A(NB)
#ifdef LINUX
      INTEGER*4 LU4,IB4
      INTEGER*4 FSEEK,FGETC
#endif
#ifdef DEC
      INTEGER FSEEK,FGETC
#endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifdef LINUX
       LU4=LU
       IB4=IB
       IF(FSEEK(LU4,IB4,0).EQ.0) THEN
        KA=0
        DOWHILE(KA.LT.NB.AND.FGETC(LU4,A(KA+1)).EQ.0)
          KA=KA+1
        ENDDO
      ENDIF
      LU=LU4
      IB=IB4
#else
#ifdef DEC
      IF(FSEEK(LU,IB,0).EQ.0) THEN
        KA=0
        DOWHILE(KA.LT.NB.AND.FGETC(LU,A(KA+1)).EQ.0)
          KA=KA+1
        ENDDO
      ENDIF
#else
      KA=0
      IF(IB.LT.0) THEN
        INQUIRE(LU,NEXTREC=KR,IOSTAT=IOS)
        IF(IOS.NE.0) RETURN
      ELSE
        KR=IB+1
      ENDIF
      DO KB=1,NB
        READ(LU,REC=KR,IOSTAT=IOS) A(KA+1)
        IF(IOS.NE.0) RETURN
        KA=KA+1
        KR=KR+1
      ENDDO
#endif
#endif
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
