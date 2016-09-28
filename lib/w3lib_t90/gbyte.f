      SUBROUTINE GBYTE(IN,IOUT,ISKIP,NBYTE)
      character*1 in(*)
      integer iout(*)
      CALL GBYTES(IN,IOUT,ISKIP,NBYTE,0,1)
      RETURN
      END
