    SUBROUTINE SBYTE(IOUT,IN,ISKIP,NBYTE)                            
! THIS PROGRAM WRITTEN BY.....                                                  
!             DR. ROBERT C. GAMMILL, CONSULTANT                                 
!             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
!             JULY 1972 
! THIS IS THE FORTRAN VERSIONS OF SBYTE.
!             SUN SPARCSTATION 1+ VERSION OF SBYTE  SUN FORTRAN 1.3
!             AUGUST 1990  RUSSELL E. JONES
!             NATIONAL WEATHER SERVICE
!             BIT MANIPULATION FUNCTIONS ISHFT, IBSET, IBCLR, BTEST
!             DO NOT COMPILE IN LINE ON THE SUN, THEY CALL A FUNCTION
!             THEY WERE REPLACED BY LSHIFT, LRSHFT, AND'S AND OR'S.
!             THIS WILL CUT THE RUNNING TIME BY A FACTOR OF 2 TO 4.
!
   INTEGER  ::  IN
   INTEGER  ::  IOUT(*)
   INTEGER  ::  MASKS(32)
!
   SAVE
!
    DATA  NBITSW/32/
!      DATA  MASKS /Z'00000001',Z'00000003',Z'00000007',Z'0000000F',
!     &             Z'0000001F',Z'0000003F',Z'0000007F',Z'000000FF',
!     &             Z'000001FF',Z'000003FF',Z'000007FF',Z'00000FFF',
!     &             Z'00001FFF',Z'00003FFF',Z'00007FFF',Z'0000FFFF',
!     &             Z'0001FFFF',Z'0003FFFF',Z'0007FFFF',Z'000FFFFF',
!     &             Z'001FFFFF',Z'003FFFFF',Z'007FFFFF',Z'00FFFFFF',
!     &             Z'01FFFFFF',Z'03FFFFFF',Z'07FFFFFF',Z'0FFFFFFF',
!     &             Z'1FFFFFFF',Z'3FFFFFFF',Z'7FFFFFFF',Z'FFFFFFFF'/
!
!     MASK TABLE PUT IN DECIMAL SO IT WILL COMPILE ON AN 32 BIT 
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
   ICON  = NBITSW - NBYTE
   IF (ICON.LT.0) RETURN   
   MASK  = MASKS(NBYTE)
   !
   ! INDEX TELLS HOW MANY WORDS INTO IOUT THE NEXT BYTE IS TO BE STORED.           
   !
   INDEX = ISKIP / NBITSW   
   !
   ! II TELLS HOW MANY BITS IN FROM THE LEFT SIDE OF THE WORD TO STORE IT.         
   !
   II    = MOD(ISKIP,NBITSW)

   J     = IAND(MASK,IN)    
   MOVEL = ICON - II         
   !                                                                               
   ! BYTE IS TO BE STORED IN MIDDLE OF WORD.  SHIFT LEFT.                          
   !
   IF (MOVEL.GT.0) THEN                                                        
      MSK           = LSHIFT(MASK,MOVEL)                                       
      IOUT(INDEX+1) = IOR(IAND(NOT(MSK),IOUT(INDEX+1)),                        &
      LSHIFT(J,MOVEL))
   !
   ! THE BYTE IS TO BE SPLIT ACROSS A WORD BREAK.                                  
   !
   ELSE IF (MOVEL.LT.0) THEN          
      MSK           = MASKS(NBYTE+MOVEL)                                      
      IOUT(INDEX+1) = IOR(IAND(NOT(MSK),IOUT(INDEX+1)),                       &
      ISHFT(J,MOVEL))  
      ITEMP         = IAND(MASKS(NBITSW+MOVEL),IOUT(INDEX+2))
      IOUT(INDEX+2) = IOR(ITEMP,LSHIFT(J,NBITSW+MOVEL))
   !             
   ! BYTE IS TO BE STORED RIGHT-ADJUSTED.                                          
   !
   ELSE
      IOUT(INDEX+1) = IOR(IAND(NOT(MASK),IOUT(INDEX+1)),J)
   ENDIF
!
   RETURN
   END
