    SUBROUTINE SBYTE(IPACKD,IUNPKD,NOFF,NBITS)                            
! THIS PROGRAM WRITTEN BY.....                                                  
!             DR. ROBERT C. GAMMILL, CONSULTANT                                 
!             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
!             JULY 1972 
!
! THIS IS THE FORTRAN 32 bit VERSION OF SBYTE.
!             Changes for SiliconGraphics IRIS-4D/25
!             SiliconGraphics 3.3 FORTRAN 77
!             MARCH 1991  RUSSELL E. JONES
!             NATIONAL WEATHER SERVICE
!
!***********************************************************************
!
! SUBROUTINE SBYTE (IPACKD,IUNPKD,NOFF,NBITS)
!
! PURPOSE                GIVEN A BYTE, RIGHT-JUSTIFIED IN A WORD, TO
!                        PACK THE BYTE INTO A TARGET WORD OR ARRAY.
!                        BITS SURROUNDING THE BYTE IN THE TARGET
!                        AREA ARE UNCHANGED.
!
! USAGE                  CALL SBYTE (IPACKD,IUNPKD,NOFF,NBITS)
!
! ARGUMENTS
! ON INPUT               IPACKD
!                          THE WORD OR ARRAY WHICH WILL CONTAIN THE
!                          PACKED BYTE.  BYTE MAY CROSS WORD BOUNDARIES.
!
!                        IUNPKD
!                          THE WORD CONTAINING THE RIGHT-JUSTIFIED BYTE
!                          TO BE PACKED.
!
!                        NOFF
!                          THE NUMBER OF BITS TO SKIP, LEFT TO RIGHT,
!                          IN 'IPACKD' IN ORDER TO LOCATE WHERE THE
!                          BYTE IS TO BE PACKED.
!
!                        NBITS
!                          NUMBER OF BITS IN THE BYTE TO BE PACKED.
!                          MAXIMUM OF 64 BITS ON 64 BIT MACHINE, 32
!                          BITS ON 32 BIT MACHINE.
!
! ON OUTPUT              IPACKD
!                          WORD OR CONSECUTIVE WORDS CONTAINING THE
!                          REQUESTED BYTE.
!***********************************************************************

   INTEGER  ::  IUNPKD
   INTEGER  ::  IPACKD(*)
   INTEGER  ::  MASKS(64)
!
   SAVE
!
   DATA IFIRST/1/
   IF(IFIRST.EQ.1) THEN
      CALL W3FI01(LW)
      NBITSW = 8 * LW
      JSHIFT = -1 * NINT(ALOG(FLOAT(NBITSW)) / ALOG(2.0))
      MASKS(1) = 1
      DO I=2,NBITSW-1
         MASKS(I) = 2 * MASKS(I-1) + 1
      ENDDO
      MASKS(NBITSW) = -1
      IFIRST = 0
   ENDIF

   !
   ! NBITS MUST BE LESS THAN OR EQUAL TO NBITSW
   !
   ICON  = NBITSW - NBITS
   IF (ICON.LT.0) RETURN   
   MASK  = MASKS(NBITS)

   !
   ! INDEX TELLS HOW MANY WORDS INTO IOUT THE NEXT BYTE IS TO BE STORED.           
   !
   INDEX = ISHFT(NOFF,JSHIFT) 

   !
   ! II TELLS HOW MANY BITS IN FROM THE LEFT SIDE OF THE WORD TO STORE IT.         
   !
   II    = MOD(NOFF,NBITSW)

   J     = IAND(MASK,IUNPKD)    
   MOVEL = ICON - II         

   !                                                                               
   ! BYTE IS TO BE STORED IN MIDDLE OF WORD.  SHIFT LEFT.                          
   !
   IF (MOVEL.GT.0) THEN          
      MSK           = ISHFT(MASK,MOVEL)  
      IPACKD(INDEX+1) = IOR(IAND(NOT(MSK),IPACKD(INDEX+1)),                    &
                        ISHFT(J,MOVEL))

   !                                                                               
   ! THE BYTE IS TO BE SPLIT ACROSS A WORD BREAK.                                  
   !
   ELSE IF (MOVEL.LT.0) THEN          
      MSK           = MASKS(NBITS+MOVEL)      
      IPACKD(INDEX+1) = IOR(IAND(NOT(MSK),IPACKD(INDEX+1)),                   &
                        ISHFT(J,MOVEL))  
      ITEMP         = IAND(MASKS(NBITSW+MOVEL),IPACKD(INDEX+2))
      IPACKD(INDEX+2) = IOR(ITEMP,ISHFT(J,NBITSW+MOVEL))

   !             
   ! BYTE IS TO BE STORED RIGHT-ADJUSTED.                                          
   !
   ELSE
      IPACKD(INDEX+1) = IOR(IAND(NOT(MASK),IPACKD(INDEX+1)),J)
   ENDIF
!
   RETURN
   END
