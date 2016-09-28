   SUBROUTINE GBYTES(IPACKD,IUNPKD,NOFF,NBITS,ISKIP,ITER)
!
! THIS PROGRAM WRITTEN BY.....
!             DR. ROBERT C. GAMMILL, CONSULTANT
!             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
!             MAY 1972
!
!             CHANGES FOR SiliconGraphics IRIS-4D/25
!             SiliconGraphics 3.3 FORTRAN 77
!             MARCH 1991, RUSSELL E. JONES
!             NATIONAL WEATHER SERVICE
!
! THIS IS THE FORTRAN VERSION OF GBYTES.
!
!***********************************************************************
!
! SUBROUTINE GBYTES (IPACKD,IUNPKD,NOFF,NBITS,ISKIP,ITER)
!
! PURPOSE                TO UNPACK A SERIES OF BYTES INTO A TARGET
!                        ARRAY.  EACH UNPACKED BYTE IS RIGHT-JUSTIFIED
!                        IN ITS TARGET WORD, AND THE REMAINDER OF THE
!                        WORD IS ZERO-FILLED.
!
! USAGE                  CALL GBYTES (IPACKD,IUNPKD,NOFF,NBITS,NSKIP,
!                                     ITER)
!
! ARGUMENTS
! ON INPUT                IPACKD
!                           THE WORD OR ARRAY CONTAINING THE PACKED
!                           BYTES.
!
!                         IUNPKD
!                           THE ARRAY WHICH WILL CONTAIN THE UNPACKED
!                           BYTES.
!
!                         NOFF
!                           THE INITIAL NUMBER OF BITS TO SKIP, LEFT
!                           TO RIGHT, IN 'IPACKD' IN ORDER TO LOCATE
!                           THE FIRST BYTE TO UNPACK.
!
!                        NBITS
!                          NUMBER OF BITS IN THE BYTE TO BE UNPACKED.
!                          MAXIMUM OF 64 BITS ON 64 BIT MACHINE, 32
!                          BITS ON 32 BIT MACHINE.
!
!                         ISKIP
!                           THE NUMBER OF BITS TO SKIP BETWEEN EACH BYTE
!                           IN 'IPACKD' IN ORDER TO LOCATE THE NEXT BYTE
!                           TO BE UNPACKED.
!
!                         ITER
!                           THE NUMBER OF BYTES TO BE UNPACKED.
!
! ARGUMENTS
! ON OUTPUT               IUNPKD
!                           CONTAINS THE REQUESTED UNPACKED BYTES.
!***********************************************************************

   INTEGER  ::  IPACKD(*)
   INTEGER  ::  IUNPKD(*)
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
   ! ISTEP IS THE DISTANCE IN BITS FROM THE START OF ONE BYTE TO THE NEXT.
   !
   ISTEP  = NBITS + ISKIP      

   !
   ! IWORDS TELLS HOW MANY WORDS TO SKIP FROM ONE BYTE TO THE NEXT.                
   !
   IWORDS = ISTEP / NBITSW    

   !
   ! IBITS TELLS HOW MANY BITS TO SKIP AFTER SKIPPING IWORDS.                      
   !
   IBITS  = MOD(ISTEP,NBITSW) 

   DO I = 1,ITER
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
         MOVER   = NBITSW - MOVEL                                                
         IUNPKD(I) = IAND(IOR(ISHFT(IPACKD(INDEX+1),MOVEL),                    &
                     ISHFT(IPACKD(INDEX+2),-MOVER)),MASK)

      !
      ! RIGHT ADJUST THE BYTE.
      !
      ELSE IF (MOVER.GT.0) THEN
         IUNPKD(I) = IAND(ISHFT(IPACKD(INDEX+1),-MOVER),MASK)

      !                                             
      ! THE BYTE IS ALREADY RIGHT ADJUSTED.
      !
      ELSE
         IUNPKD(I) = IAND(IPACKD(INDEX+1),MASK)
      ENDIF

      !                                                                               
      ! INCREMENT II AND INDEX.
      !
      II    = II + IBITS
      INDEX = INDEX + IWORDS
      IF (II.GE.NBITSW) THEN
         II    = II - NBITSW
         INDEX = INDEX + 1
      ENDIF

   END DO
   RETURN
   END
