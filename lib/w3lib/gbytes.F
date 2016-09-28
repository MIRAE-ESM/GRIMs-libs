#include <define.h>
#define OTHER
#ifdef LINUX
#define DEC
#endif
#ifdef DEC
#undef OTHER
      SUBROUTINE GBYTES(IN,IOUT,ISKIP,NBYTE,NSKIP,N)
C          Get bytes - unpack bits:  Extract arbitrary size values from a
C          packed bit string, right justifying each value in the unpacked
C          array.
C            IN    = character*1 array input
C            IOUT  = unpacked array output
C            ISKIP = initial number of bits to skip
C            NBYTE = number of bits to take
C            NSKIP = additional number of bits to skip on each iteration
C            N     = number of iterations
C v1.1
C
      character*1 in(*)
      integer iout(*)
      integer ones(8), tbit, bitcnt
      save ones
      data ones/1,3,7,15,31,63,127,255/
c

c     nbit is the start position of the field in bits
      nbit = iskip
      do i = 1, n
         bitcnt = nbyte
         index=nbit/8+1
         ibit=mod(nbit,8)
         nbit = nbit + nbyte + nskip

c        first byte
         tbit = min(bitcnt,8-ibit)
         itmp = and(ichar(in(index)),ones(8-ibit))
         if (tbit.ne.8-ibit) itmp = ishft(itmp,tbit-8+ibit)
         index = index + 1
         bitcnt = bitcnt - tbit

c        now transfer whole bytes
         do while (bitcnt.ge.8)
             itmp = or(ishft(itmp,8),ichar(in(index)))
             bitcnt = bitcnt - 8
             index = index + 1
         enddo

c        get data from last byte
         if (bitcnt.gt.0) then
             itmp = or(ishft(itmp,bitcnt), and(ishft(ichar(in(index)),
     1          -(8-bitcnt)),ones(bitcnt)))
         endif

         iout(i) = itmp
      enddo

      RETURN
      END                                                                  
#endif
#ifdef OTHER
      SUBROUTINE GBYTES(IPACKD,IUNPKD,NOFF,NBITS,ISKIP,ITER)
C
C THIS PROGRAM WRITTEN BY.....
C             DR. ROBERT C. GAMMILL, CONSULTANT
C             NATIONAL CENTER FOR ATMOSPHERIC RESEARCH
C             MAY 1972
C
C             CHANGES FOR SiliconGraphics IRIS-4D/25
C             SiliconGraphics 3.3 FORTRAN 77
C             MARCH 1991, RUSSELL E. JONES
C             NATIONAL WEATHER SERVICE
C
C THIS IS THE FORTRAN VERSION OF GBYTES.
C
C***********************************************************************
C
C SUBROUTINE GBYTES (IPACKD,IUNPKD,NOFF,NBITS,ISKIP,ITER)
C
C PURPOSE                TO UNPACK A SERIES OF BYTES INTO A TARGET
C                        ARRAY.  EACH UNPACKED BYTE IS RIGHT-JUSTIFIED
C                        IN ITS TARGET WORD, AND THE REMAINDER OF THE
C                        WORD IS ZERO-FILLED.
C
C USAGE                  CALL GBYTES (IPACKD,IUNPKD,NOFF,NBITS,NSKIP,
C                                     ITER)
C
C ARGUMENTS
C ON INPUT                IPACKD
C                           THE WORD OR ARRAY CONTAINING THE PACKED
C                           BYTES.
C
C                         IUNPKD
C                           THE ARRAY WHICH WILL CONTAIN THE UNPACKED
C                           BYTES.
C
C                         NOFF
C                           THE INITIAL NUMBER OF BITS TO SKIP, LEFT
C                           TO RIGHT, IN 'IPACKD' IN ORDER TO LOCATE
C                           THE FIRST BYTE TO UNPACK.
C
C                        NBITS
C                          NUMBER OF BITS IN THE BYTE TO BE UNPACKED.
C                          MAXIMUM OF 64 BITS ON 64 BIT MACHINE, 32
C                          BITS ON 32 BIT MACHINE.
C
C                         ISKIP
C                           THE NUMBER OF BITS TO SKIP BETWEEN EACH BYTE
C                           IN 'IPACKD' IN ORDER TO LOCATE THE NEXT BYTE
C                           TO BE UNPACKED.
C
C                         ITER
C                           THE NUMBER OF BYTES TO BE UNPACKED.
C
C ARGUMENTS
C ON OUTPUT               IUNPKD
C                           CONTAINS THE REQUESTED UNPACKED BYTES.
C***********************************************************************

      INTEGER    IPACKD(*)

      INTEGER    IUNPKD(*)
      INTEGER    MASKS(64)
C
      SAVE
C
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
C
C NBITS MUST BE LESS THAN OR EQUAL TO NBITSW                                    
C
      ICON   = NBITSW - NBITS
      IF (ICON.LT.0) RETURN
      MASK   = MASKS(NBITS)
C
C INDEX TELLS HOW MANY WORDS INTO THE ARRAY 'IPACKD' THE NEXT BYTE
C APPEARS.         
C
      INDEX  = ISHFT(NOFF,JSHIFT)
C
C II TELLS HOW MANY BITS THE BYTE IS FROM THE LEFT SIDE OF THE WORD.
C
      II     = MOD(NOFF,NBITSW)
C
C ISTEP IS THE DISTANCE IN BITS FROM THE START OF ONE BYTE TO THE NEXT.
C
      ISTEP  = NBITS + ISKIP      
C
C IWORDS TELLS HOW MANY WORDS TO SKIP FROM ONE BYTE TO THE NEXT.                
C
      IWORDS = ISTEP / NBITSW    
C
C IBITS TELLS HOW MANY BITS TO SKIP AFTER SKIPPING IWORDS.                      
C
      IBITS  = MOD(ISTEP,NBITSW) 
C
      DO 10 I = 1,ITER
C
C MOVER SPECIFIES HOW FAR TO THE RIGHT A BYTE MUST BE MOVED IN ORDER            
C
C    TO BE RIGHT ADJUSTED.                                                      
C
      MOVER = ICON - II
C                                                                               
C THE BYTE IS SPLIT ACROSS A WORD BREAK.                 
C                       
      IF (MOVER.LT.0) THEN                                                  
        MOVEL   = - MOVER                                                       
        MOVER   = NBITSW - MOVEL                                                
        IUNPKD(I) = IAND(IOR(ISHFT(IPACKD(INDEX+1),MOVEL),
     &            ISHFT(IPACKD(INDEX+2),-MOVER)),MASK)
C
C RIGHT ADJUST THE BYTE.
C
      ELSE IF (MOVER.GT.0) THEN
        IUNPKD(I) = IAND(ISHFT(IPACKD(INDEX+1),-MOVER),MASK)
C                                             
C THE BYTE IS ALREADY RIGHT ADJUSTED.
C
      ELSE
        IUNPKD(I) = IAND(IPACKD(INDEX+1),MASK)
      ENDIF
C                                                                               
C INCREMENT II AND INDEX.
C
        II    = II + IBITS
        INDEX = INDEX + IWORDS
        IF (II.GE.NBITSW) THEN
          II    = II - NBITSW
          INDEX = INDEX + 1
        ENDIF
C
   10 CONTINUE
        RETURN
      END
#endif
