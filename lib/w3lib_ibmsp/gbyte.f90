   SUBROUTINE GBYTE(IPACKD,IUNPKD,NOFF,NBITS)
!
! THIS PROGRAM WRITTEN BY.....
!             DR. ROBERT C. GAMMILL, CONSULTANT
!             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
!             MAY 1972
!
!             CHANGES FOR SiliconGraphics IRIS-4D/25
!             SiliconGraphics 3.3 FORTRAN 77
!             March 1991, RUSSELL E. JONES
!             NATIONAL WEATHER SERVICE
!
! THIS IS THE FORTRAN VERSION OF GBYTE
!
!***********************************************************************
!
! SUBROUTINE GBYTE (IPACKD,IUNPKD,NOFF,NBITS)
!
! PURPOSE                TO UNPACK A BYTE INTO A TARGET WORD.  THE
!                        UNPACKED BYTE IS RIGHT-JUSTIFIED IN THE
!                        TARGET WORD, AND THE REMAINDER OF THE
!                        WORD IS ZERO-FILLED.
!
! USAGE                  CALL GBYTE(IPACKD,IUNPKD,NOFF,NBITS)
!
! ARGUMENTS
!
! ON INPUT               IPACKD
!                          THE WORD OR ARRAY CONTAINING THE BYTE TO BE
!                          UNPACKED.
!
!                        IUNPKD
!                          THE WORD WHICH WILL CONTAIN THE UNPACKED
!                          BYTE.
!
!                        NOFF
!                          THE NUMBER OF BITS TO SKIP, LEFT TO RIGHT,
!                          IN 'IPACKD' IN ORDER TO LOCATE THE BYTE
!                          TO BE UNPACKED.
!
!                        NBITS
!                          NUMBER OF BITS IN THE BYTE TO BE UNPACKED.
!                          MAXIMUM OF 64 BITS ON 64 BIT MACHINE, 32
!                          BITS ON 32 BIT MACHINE.
!
! ON OUTPUT              IUNPKD
!                          CONTAINS THE REQUESTED UNPACKED BYTE.
!***********************************************************************

   INTEGER  ::  IPACKD(*)
   INTEGER  ::  IUNPKD
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
   ICON   = NBITSW - NBITS
   IF (ICON.LT.0) RETURN
   MASK   = MASKS(NBITS)
   
   !
   ! INDEX TELLS HOW MANY WORDS INTO THE ARRAY 'IPACKD' THE NEXT BYTE
   ! APPEARS.         
   !
   INDEX  = ISHFT(NOFF,JSHIFT)
   
   !
   ! II TELLS HOW MANY BITS THE BYTE IS FROM THE LEFT SIDE OF THE WORD.
   !
   II     = MOD(NOFF,NBITSW)
   
   !
   ! MOVER SPECIFIES HOW FAR TO THE RIGHT NBITS MUST BE MOVED IN ORDER            
   !
   !    TO BE RIGHT ADJUSTED.                                                 
   !
   MOVER = ICON - II

   IF (MOVER.GT.0) THEN
      IUNPKD  = IAND(ISHFT(IPACKD(INDEX+1),-MOVER),MASK)
      
   !
   ! THE BYTE IS SPLIT ACROSS A WORD BREAK.                 
   !
   ELSE IF (MOVER.LT.0) THEN                      
      MOVEL = - MOVER                                                       
      MOVER = NBITSW - MOVEL                                                
      IUNPKD  = IAND(IOR(ISHFT(IPACKD(INDEX+1),MOVEL),                         &
                ISHFT(IPACKD(INDEX+2),-MOVER)),MASK)
                
   !
   ! THE BYTE IS ALREADY RIGHT ADJUSTED.
   !  
   ELSE                              
      IUNPKD  = IAND(IPACKD(INDEX+1),MASK) 
   ENDIF
!   
   RETURN
   END
