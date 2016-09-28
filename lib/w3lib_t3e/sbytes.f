      SUBROUTINE SBYTES(PACK,INI,NSKIP,NPICK,ISKIP,IPTS)
C 
C pack integer into character
C PACK : charater hold the packed data
C INI  : input integer to pack
C NSKIP: bit skip in PACK for next pack
C NPICK: bit number to be pick from INI to PACK
C
      DIMENSION INI(IPTS)
      CHARACTER*1 PACK(*)
C
      NS=NSKIP
      DO I=1,IPTS,ISKIP+1
        CALL SBYTE(PACK,INI(I),NS,NPICK)
        NS=NS+NPICK
      ENDDO
C
      RETURN
      END   
