   SUBROUTINE SBYTES(PACK,INI,NSKIP,NPICK,ISKIP,IPTS)
! 
! pack integer into character
! PACK : charater hold the packed data
! INI  : input integer to pack
! NSKIP: bit skip in PACK for next pack
! NPICK: bit number to be pick from INI to PACK
!
   DIMENSION         ::  INI(IPTS)
   CHARACTER(len=1)  ::  PACK(*)
!
   NS=NSKIP
   DO I=1,IPTS,ISKIP+1
      CALL SBYTE(PACK,INI(I),NS,NPICK)
      NS=NS+NPICK
   ENDDO
!
   RETURN
   END   
