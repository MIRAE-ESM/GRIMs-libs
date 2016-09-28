   SUBROUTINE GBYTES (IN,IOUT,ISKIP,NBYTE,NSKIP,N)
!
!          Get bytes - unpack bits:  Extract arbitrary size values from a
!          packed bit string, right justifying each value in the unpacked
!          array.
!
   DIMENSION  ::  IN(*), IOUT(*)
!
!            IN    = packed array input
!            IO    = unpacked array output
!            ISKIP = initial number of bits to skip
!            NBYTE = number of bits to take
!            NSKIP = additional number of bits to skip on each iteration
!            N     = number of iterations
!
!          Machine dependent information required:
!            LMWD   = Number of bits in a word on this machine
!            MASKS  = Set of word masks where the first element has only the
!                     right most bit set to 1, the second has the two, ...
!            LEFTSH = Shift left bits in word M to the by N bits
!            RGHTSH = Shift right
!            OR     = Logical OR (add) on this machine.
!            AND    = Logical AND (multiply) on this machine
!          This is for DEC Alpha
!
   INTEGER, PARAMETER  ::  LMWD=32
   DIMENSION           ::  MASKS(LMWD)
   SAVE      MASKS
   DATA      MASKS /'1'X,'3'X,'7'X,'F'X, '1F'X,'3F'X,'7F'X,'FF'X,              &
   '1FF'X,'3FF'X,'7FF'X,'FFF'X, '1FFF'X,'3FFF'X,'7FFF'X,'FFFF'X,               &
   '1FFFF'X,       '3FFFF'X,       '7FFFF'X,       'FFFFF'X,                   &
   '1FFFFF'X,      '3FFFFF'X,      '7FFFFF'X,      'FFFFFF'X,                  &
   '1FFFFFF'X,     '3FFFFFF'X,     '7FFFFFF'X,     'FFFFFFF'X,                 &
   '1FFFFFFF'X,    '3FFFFFFF'X,    '7FFFFFFF'X,    'FFFFFFFF'X/

   INTEGER   ::  RGHTSH, OR, AND
   LEFTSH(M,N) = ISHFT(M,N)
   RGHTSH(M,N) = ISHFT(M,-N)
!
!          History:  written by Robert C. Gammill, jul 1972.
!                    Corrected for use in grmap my M. Kanamitsu  Dec. 1996
!
!          NBYTE must be less than or equal to LMWD
!
   ICON = LMWD-NBYTE
   IF (ICON.LT.0) RETURN
   MASK = MASKS (NBYTE)
!
!          INDEX  = number of words into IN before the next "byte" appears
!          II     = number of bits the "byte" is from the left side of the word
!          ISTEP  = number of bits from the start of one "byte" to the next
!          IWORDS = number of words to skip from one "byte" to the next
!          IBITS  = number of bits to skip after skipping IWORDS
!          MOVER  = number of bits to the right, a byte must be moved to be
!                   right adjusted
!
   INDEX = ISKIP/LMWD
   II    = MOD (ISKIP,LMWD)
   ISTEP = NBYTE+NSKIP
   IWORDS= ISTEP/LMWD
   IBITS = MOD (ISTEP,LMWD)

   DO 6 I=1,N                                                                
   MOVER = ICON-II
   IF (MOVER) 2,3,4
   !
   ! The "byte" is split across a word break.
   !
    2 MOVEL = -MOVER
   MOVER = LMWD-MOVEL
   IN1=IN(INDEX+1)
   CALL SWAP32(IN1,1)
   IN2=IN(INDEX+2)
   CALL SWAP32(IN2,1)
   NP1 = LEFTSH (IN1,MOVEL)
   NP2 = RGHTSH (IN2,MOVER)
   IOUT(I) = AND (OR (NP1,NP2) , MASK)
   GO TO 5                                                                   
   !
   ! The "byte" is already right adjusted.
   !
    3 CONTINUE
   IN1=IN(INDEX+1)
   CALL SWAP32(IN1,1)
   IOUT(I) = AND (IN1 , MASK)
   GO TO 5                                                                   
   !
   ! Right adjust the "byte".
   !
    4 CONTINUE
   IN1=IN(INDEX+1)
   CALL SWAP32(IN1,1)
   IOUT(I) = AND (RGHTSH (IN1,MOVER) , MASK)

    5 II = II+IBITS
   INDEX = INDEX+IWORDS
   IF (II .LT. LMWD) GO TO 6
   II = II-LMWD
   INDEX = INDEX+1
    6 CONTINUE                                                                  

   RETURN                                                                    
   END                                                                       
