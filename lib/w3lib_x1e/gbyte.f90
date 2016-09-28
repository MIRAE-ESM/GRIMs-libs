   SUBROUTINE GBYTE (IN,IOUT,ISKIP,NBYTE)
   character(len=1)  ::  in(*)
   integer           ::  iout(*)
   CALL GBYTES (IN,IOUT,ISKIP,NBYTE,0,1)
   RETURN
   END
