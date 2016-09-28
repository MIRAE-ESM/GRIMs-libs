   SUBROUTINE SBYTE(PACK,INI,NSKIP,NPICK)
! 
! pack integer into character
! PACK : charater hold the packed data
! INI  : input integer to pack
! NSKIP: bit skip in PACK for next pack
! NPICK: bit number to be pick from INI to PACK
!
   CHARACTER(len=1)  ::  PACK(*)
!
   IN=INI
   NS=NSKIP
   NP=NPICK
!
 1    NSL = MOD(NS,8)
   NSC = NS/8+1
   NSR = 8-NSL
   INL = ICHAR(PACK(NSC))
!
   IF( NP.GE.NSR ) THEN
     NPP = NP - NSR
     NSS = 0
     NSP = NSR
   ELSE
     NPP = 0
     NSS = NSR - NP
     NSP = NP
   ENDIF
!
   IF(NP.EQ.32) THEN
     INR = IN/2**NPP
   ELSE
     INR = MOD(IN,2**NP)/2**NPP
   ENDIF
   ICH = INT(INL/2**NSR)*2**NSR + INR*2**NSS
   PACK(NSC) = CHAR(ICH)
   NS = NS + NSP
   NP = NPP
!
   IF( NP.EQ.0 ) THEN
     RETURN
   ELSE
     IN = MOD(IN,2**NPP)
     GO TO 1
   ENDIF
!
   END   
