    SUBROUTINE SWAP32(A,N)
   !
   ! REVERSE ORDER OF BYTES IN INTEGER*4 WORD, or REAL*4
   !
   INTEGER*4         ::  A(N)
   CHARACTER(len=1)  ::  JTEMP(4)
   CHARACTER(len=1)  ::  KTEMP

   EQUIVALENCE (JTEMP(1),ITEMP)

   SAVE

   DO I = 1,N
      ITEMP    = A(I)
      KTEMP    = JTEMP(4)
      JTEMP(4) = JTEMP(1)
      JTEMP(1) = KTEMP
      KTEMP    = JTEMP(3)
      JTEMP(3) = JTEMP(2)
      JTEMP(2) = KTEMP
      A(I)     = ITEMP
   END DO
   RETURN
   END
