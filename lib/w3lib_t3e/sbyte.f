      SUBROUTINE SBYTE(PACK,INI,NSKIP,NPICK)
C 
C pack integer into character
C PACK : charater hold the packed data
C INI  : input integer to pack
C NSKIP: bit skip in PACK for next pack
C NPICK: bit number to be pick from INI to PACK
C
      CHARACTER*1 PACK(*)
C
      IN=INI
      NS=NSKIP
      NP=NPICK
C
 1    NSL = MOD(NS,8)
      NSC = NS/8+1
      NSR = 8-NSL
      INL = ICHAR(PACK(NSC))
C
      IF( NP.GE.NSR ) THEN
        NPP = NP - NSR
        NSS = 0
        NSP = NSR
      ELSE
        NPP = 0
        NSS = NSR - NP
        NSP = NP
      ENDIF
C
      IF(NP.EQ.32) THEN
        INR = IN/2**NPP
      ELSE
        INR = MOD(IN,2**NP)/2**NPP
      ENDIF
      ICH = INT(INL/2**NSR)*2**NSR + INR*2**NSS
      PACK(NSC) = CHAR(ICH)
      NS = NS + NSP
      NP = NPP
C
      IF( NP.EQ.0 ) THEN
        RETURN
      ELSE
        IN = MOD(IN,2**NPP)
        GO TO 1
      ENDIF
C
      END   
