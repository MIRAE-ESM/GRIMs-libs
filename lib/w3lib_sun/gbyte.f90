   SUBROUTINE GBYTE(IN,IOUT,ISKIP,NBYTE)
!
! THIS PROGRAM WRITTEN BY.....
!             DR. ROBERT C. GAMMILL, CONSULTANT
!             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
!             MAY 1972
!             CHANGES FOR SUN SPARCSTATION 1+
!             AUGUST 1990, RUSSELL E. JONES
!             NATIONAL WEATHER SERVICE
!
! THIS IS THE FORTRAN VERSION OF GBYTE
!
   INTEGER  ::  IN(*)
   INTEGER  ::  IOUT
   INTEGER  ::  MASKS(32)
!
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
   SAVE
!
   DATA  MASKS / 1, 3, 7, 15, 31, 63, 127, 255, 511, 1023, 2047,               &
    4095, 8191, 16383, 32767, 65535, 131071, 262143, 524287,                   &
    1048575, 2097151, 4194303, 8388607, 16777215, 33554431,                    &
    67108863, 134217727, 268435455, 536870911, 1073741823,                     &
    2147483647, -1/
   !
   ! NBYTE MUST BE LESS THAN OR EQUAL TO 32                                    
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
   ! MOVER SPECIFIES HOW FAR TO THE RIGHT NBYTE MUST BE MOVED IN ORDER            
   !
   !    TO BE RIGHT ADJUSTED.                                                      
   !
   MOVER = ICON - II
!
   IF (MOVER.GT.0) THEN
     IOUT  = IAND(ISHFT(IN(INDEX+1),-MOVER),MASK)
   !                                                                               
   ! THE BYTE IS SPLIT ACROSS A WORD BREAK.                 
   !
   ELSE IF (MOVER.LT.0) THEN                      
     MOVEL = - MOVER                                                       
     MOVER = 32 - MOVEL                                                
     IOUT  = IAND(IOR(LSHIFT(IN(INDEX+1),MOVEL),                               &
             ISHFT(IN(INDEX+2),-MOVER)),MASK)
   !                                                                               
   ! THE BYTE IS ALREADY RIGHT ADJUSTED.
   !  
   ELSE                              
     IOUT  = IAND(IN(INDEX+1),MASK) 
   ENDIF
!   
   RETURN
   END
