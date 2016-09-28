   SUBROUTINE GBYTES(IN,IOUT,ISKIP,NBYTE,NSKIP,N)
!
! THIS PROGRAM WRITTEN BY.....
!             DR. ROBERT C. GAMMILL, CONSULTANT
!             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
!             MAY 1972
!
!             CHANGES FOR SiliconGraphics IRIS-4D/25, 35, INDIGO
!             SiliconGraphics 3.5 FORTRAN 77
!             MARCH 1991, RUSSELL E. JONES
!             NATIONAL WEATHER SERVICE
!             CHANGE AUG. 1992 REPLACE ISHFT WITH LSHIFT & RSHIFT
!             THIS MAKES IT FASTER, BUT NOT PORTABLE.
!             SGI VERSION ONLY.
!
! THIS IS THE FORTRAN VERSION OF GBYTES.
!
   INTEGER  ::  IN(*)
   INTEGER  ::  IOUT(*)
   INTEGER  ::  MASKS(32)

   SAVE

!     DATA  NBITSW/32/
!     DATA  MASKS /Z'00000001',Z'00000003',Z'00000007',Z'0000000F',
!    &             Z'0000001F',Z'0000003F',Z'0000007F',Z'000000FF',
!    &             Z'000001FF',Z'000003FF',Z'000007FF',Z'00000FFF',
!    &             Z'00001FFF',Z'00003FFF',Z'00007FFF',Z'0000FFFF',
!    &             Z'0001FFFF',Z'0003FFFF',Z'0007FFFF',Z'000FFFFF',
!    &             Z'001FFFFF',Z'003FFFFF',Z'007FFFFF',Z'00FFFFFF',
!    &             Z'01FFFFFF',Z'03FFFFFF',Z'07FFFFFF',Z'0FFFFFFF',
!    &             Z'1FFFFFFF',Z'3FFFFFFF',Z'7FFFFFFF',Z'FFFFFFFF'/
!
!     MASKS TABLE PUT IN DECIMAL SO IT WILL COMPILE ON ANY 32 BIT 
!     COMPUTER 
!
   DATA  MASKS / 1, 3, 7, 15, 31, 63, 127, 255, 511, 1023, 2047,               &
    4095, 8191, 16383, 32767, 65535, 131071, 262143, 524287,                   &
    1048575, 2097151, 4194303, 8388607, 16777215, 33554431,                    &
    67108863, 134217727, 268435455, 536870911, 1073741823,                     &
    2147483647, -1/
   !
   ! NBYTE MUST BE LESS THAN OR EQUAL TO NBITSW                                    
   !
   ICON   = 32 - NBYTE
   IF (ICON.LT.0) RETURN
   MASK   = MASKS(NBYTE)
   !
   ! INDEX TELLS HOW MANY WORDS INTO THE ARRAY 'IN' THE NEXT BYTE APPEARS.         
   !
   INDEX  = ISKIP / 32
   !
   ! II TELLS HOW MANY BITS THE BYTE IS FROM THE LEFT SIDE OF THE WORD.
   !
   II     = MOD(ISKIP,32)
   !
   ! ISTEP IS THE DISTANCE IN BITS FROM THE START OF ONE BYTE TO THE NEXT.
   !
   ISTEP  = NBYTE + NSKIP      
   !
   ! IWORDS TELLS HOW MANY WORDS TO SKIP FROM ONE BYTE TO THE NEXT.                
   !
   IWORDS = ISTEP / 32    
   !
   ! IBITS TELLS HOW MANY BITS TO SKIP AFTER SKIPPING IWORDS.                      
   !
   IBITS  = MOD(ISTEP,32) 

   DO 10 I = 1,N
   !
   ! MOVER SPECIFIES HOW FAR TO THE RIGHT A BYTE MUST BE MOVED IN ORDER            
   !
   !    TO BE RIGHT ADJUSTED.                                                      
   !
   MOVER = ICON - II
   !                                                                               
   ! THE BYTE IS SPLIT ACROSS A WORD BREAK.                 
   !                       
   IF (MOVER.LT.0) THEN                                                  
     MOVEL   = - MOVER                                                       
     MOVER   = 32 - MOVEL                                                
!       IOUT(I) = IAND(IOR(LSHIFT(IN(INDEX+1),MOVEL),
!    &            RSHIFT(IN(INDEX+2),MOVER)),MASK)
     IOUT(I) = IAND(IOR(ISHFT(IN(INDEX+1),MOVEL),                              &
               ISHFT(IN(INDEX+2),-MOVER)),MASK)
   !
   ! RIGHT ADJUST THE BYTE.
   !
   ELSE IF (MOVER.GT.0) THEN
!       IOUT(I) = IAND(RSHIFT(IN(INDEX+1),MOVER),MASK)
     IOUT(I) = IAND(ISHFT(IN(INDEX+1),-MOVER),MASK)
   !                                             
   ! THE BYTE IS ALREADY RIGHT ADJUSTED.
   !
   ELSE
     IOUT(I) = IAND(IN(INDEX+1),MASK)
   ENDIF
   !                                                                               
   ! INCREMENT II AND INDEX.
   !
     II    = II + IBITS
     INDEX = INDEX + IWORDS
     IF (II.GE.32) THEN
       II    = II - 32
       INDEX = INDEX + 1
     ENDIF

   10 CONTINUE
     RETURN
   END
