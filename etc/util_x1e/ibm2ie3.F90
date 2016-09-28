!
   program nibm2ie3
!
!-------------------------------------------------------------------------------
!
!     Converts sigma or surface files written on cray
!     using assign -Fcos -Nibm -Cascii to ieee format.
!
!     Written by M. Kanamitsu 9/19/98
!
!     Decoding part was given by Wesley.  9/18/98
!
!-------------------------------------------------------------------------------
!
   character(LEN = 80)  ::  fname
   integer, parameter   ::  lmwd = 32
   integer, parameter   ::  lwk = 20+2*512*64/lmwd
   integer, parameter   ::  mxlblk = 550000
   integer              ::  nwork(3) 
   integer              ::  iwk(lwk)
   real*4                   array(mxlblk)
   integer*4                nbf(mxlblk)
!
   namelist/namfl/ fname
!
   iunit=11
   read(5,namfl)
   write(6,namfl)
   print *,'iunit=',iunit
!
   nrec=1
   call cbfopn(iunit,fname,6,lmwd,1,iwk,lwk)
   !
   100 continue
   !
   call cbfrd(iunit,nbf,mxlblk,lblk,nub,istat,iwk)
   if(lblk.gt.mxlblk) then
      print *,'Increase mxlblk to ',lblk
      call abort
   endif
   if(istat.eq.1) then
      print *,'nrec=',nrec
      call cbfcls(iunit,iwk)
      stop
   elseif(istat.ne.0) then
      print *,'istat=',istat
      call abort
   endif
!
   if(nrec.eq.1) then
      write(51)(nbf(i),i=1,lblk)
      print *,'nrec=',nrec,' lblk=',lblk
      call swap32(nbf,lblk)
      print *,(nbf(i),i=1,lblk)
   else
      do iiint = 0,lblk-1
         call gbytes (nbf,nwork(1),iiint*32+0, 1,0,1)
         call gbytes (nbf,nwork(2),iiint*32+1, 7,0,1)
         call gbytes (nbf,nwork(3),iiint*32+8,24,0,1)
         base = float(nwork(3)) * 16. ** (nwork(2) - 70)
         if (nwork(1).eq.1) base = -base
         array(iiint+1)=base
      enddo
      print *,'nrec=',nrec,' lblk=',lblk,' array(1)=',array(1)
      if(nrec.eq.2) then
         print *,'fhour=',base,' idate=',(nbf(i),i=2,5)
         call swap32(nbf(2),4)
         write(51) array(1),(nbf(i),i=2,5),(array(i),i=6,lblk)
      else
         write(51)(array(i),i=1,lblk)
      endif
   endif
   nrec = nrec+1
   !
   go to 100
   !
   end
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE CBFOPN (IUN,NAME,MSGUN,LMWD,IRW,IWK,LWK)
!
!          Establish connection between fortran unit IUN and file NAME prior
!          to performing Cray blocked file reading (CBFRD) or writing (CBFWR).
!          Multiple read or write operations are possible as long as each
!          fortran unit is established separately (CBFOPN) and subsequent
!          references consistantly provide unit numbers and work arrays.
!          Additional entries to close (CBFCLS), rewind (CBFREW), and
!          write end-of-file (CBFEOF) are described below:
!
!          CALL CBFOPN (IUN,NAME,MSGUN,LMWD,IRW,IWK,LWK)
!            INPUTS:
!              IUN   = Fortran unit to use for reading (or writing).
!              NAME  = Character string containing the input file name
!              MSGUN = Fortran unit to use for any diagnostic messages
!              LMWD  = Word size in bits; must be 16, 32 or 64.  If 16,
!                      see special warning in HISTORY description below.
!              IRW   = Read or write specifier; Set IRW = 1 for read or
!                      IRW = 2 for write.
!              IWK   = Work array provided by the user.
!              LWK   = Length of IWK array provided by the user.  The
!                      dimension of IWK is a function of the machine
!                      word size:  dimension = 20 + 2 * 512 * 64 / LMWD
!
!--------  CBFRD reads fixed length Cray blocks, removes and interprets the
!          control words, and combines the rest to return a single variable
!          length block.
!
!          CALL CBFRD (IUN,IBLK,MXLBLK,LBLK,NUB,ISTAT,IWK)
!            INPUTS:
!              IUN    = Fortran unit number.
!              MXLBLK = Dimension of array IBLK provided by the user for
!                       holding the binary block to be returned.  The user
!                       will not be warned of an array overflow condition;
!                       the array will be truncated to length MXLBLK, thus,
!                       this condition is evident when LBLK > MXLBLK.
!              IWK    = Work array provided by user; see LWK above.
!            RETURNS:
!              IBLK  = Binary block.
!              LBLK  = Length of the binary block in words (length LMWD)
!              NUB   = Number of unused bits in last assigned word of the
!                      block; IBLK(LBLK) may may contain some unneeded
!                      zero filled low order bits.
!              ISTAT = Read status flag; 0 = ok, 1 = EOF, 2 = parity hit,
!                      and 3 = EOD.  When ISTAT is not 0, IBLK contains
!                      nothing new.
!
!--------  CBFWR performs the opposite function of CBFRD:  records are
!          inserted into fixed length Cray blocks.  Blocks are written
!          when they are filled (or as a result of a call to CBFCLS).
!
!          CALL CBFWR (IUN,IBLK,MXLBLK,LBLK,NUB,ISTAT,IWK)
!            INPUTS:
!              IUN    = Fortran unit number.
!              IBLK   = Binary block to be written.
!              MXLBLK = Dimension of array IBLK.
!              LBLK   = Number of words (length LMWD) assigned in IBLK to
!                       be written this time; if LBLK > MXLBLK, only the
!                       first MXLBLK words are written.
!              NUB    = Number of unused bits in last assigned word of the
!                       block.
!              IWK    = Work array provided by user; see LWK above.
!            RETURNS:
!              ISTAT = Write status flag; 0 = okay, 2 = write failed.
!
!
!--------  CBFCLS breaks the connection between IUN and NAME.  If IUN is
!          opened for writing (IRW=2), before flushing the last block
!          an EOD is added.  If there is no previous EOF, one is inserted
!          before the EOD.  It is necessary to call CBFCLS to properly
!          terminate writing via CBFWR.
!
!          CALL CBFCLS (IUN,IWK)
!            INPUTS:
!              IUN   = Fortran unit number.
!              IWK   = Work array provided by the user.
!
!--------  CBFREW positions a unit open for reading at the beginning (i.e.
!          REWIND).  It is an error to attempt to rewind a unit open for
!          writing (IRW=2).
!
!          CALL CBFREW (IUN,IWK)
!            INPUTS:
!              IUN   = Fortran unit number.
!              IWK   = Work array provided by the user.
!
!--------  CBFEOF writes an end of file (EOF) to IUN.  IUN must be open for
!          writing (IRW=2).  No warning is issued when a second EOF is
!          written without intervening data.
!
!          CALL CBFEOF (IUN,IWK)
!            INPUTS:
!              IUN   = Fortran unit number.
!              IWK   = Work array provided by the user.
!
!          EXTERNALS:
!             RDCRBK   - Should be next subroutine in this file
!             GBYTE(S)/SBYTE(S) - Unpacking and packing routines.
!             SWAP4 - only needed for byte reversal; see Cbyte below
!
!          INSTALLATION:
!          The following changes will accomodate different computers.  The
!          first item is a call argument:
!
!            LMWD  define word size and work array dimension in CBFOPN call.
!
!          The rest require swapping commented source code lines in CBFOPN
!          and RDCRBK.  Search for the string indicated on the left (e.g.,
!          "Cbyte") and swap commented code segments:
!
!            Cbyte  PCs and DECs interpret bytes in each word in the
!                   reverse order of most other computers.  It is probably
!                   best to turn on byte reversal on input and output, so
!                   the order in the files is consistant with other machines.
!                   However, if files are only written and read back locally,
!                   one can leave everything byte reversed (and gain a
!                   slight speedup).  This requires subroutine SWAP4.
!
!            Crecl  The RECL argument units may be bytes or words.
!
!
!          HISTORY:  Dennis Joseph built the first version, called crayio.
!          His intentions were to recover bad blocks rejected by Cray
!          software; hence, his version ran on a Cray.  I modified it to
!          work on a 32-bit Unix machine and made provisions for 16 and 64
!          bit word sizes.  The current word size restriction is a result
!          of choosing efficiency over generality.  When an exact multiple
!          of LMWD equals 64 bits (the length of BCW, Cray block control word,
!          or RCW, record control word), array assignment is done via
!          replacement statements; e.g., loops on label 110 and 1110.  If
!          the word size is not an exact multiple of LMWD, the contents of
!          the block are no longer word aligned due to presence of these
!          64 bit control words.  This means that the assignment statements
!          would have to be replaced by GBYTES and SBYTES calls and pointers
!          and counters converted to units of bits rather than words (of
!          length LMWD).
!          Mar 93:  Added CBFWR and CBFEOF.  16-BIT MACHINES MUST declare
!          array IWK as INTEGER*4 and set LMWD=32, to avoid a truncation
!          problem in CBFWR when calculating the block number (modulus 2**24),
!          forward word index (modulus 2**20), and previous file index
!          (modulus 2**20) for BCW and RCW entries.  Note that these modulus
!          calculations are implicit by SBYTE packing.
!          Oct 97:  Changed termination conditions to handle reading files
!          with truncated last COS blocks missing an EOD RCW.  RDCRBK returned
!          status (IOST) definition differs and IWK(20) is defined.
!
!          Formal parameter declarations for all entries
!
   INTEGER       ::  IBLK(MXLBLK) , IWK(*)
   CHARACTER*(*)     NAME
!
!          The user need not be concerned about the contents of IWK, however,
!          in case anyone cares, the layout of IWK is as follows, the first
!          elements are flags, pointers and counters:
!          IWK(1) = dimension of IWK.
!              2  = IUN = fortran unit to read or write.
!              3  = MSGUN = fortran unit to write diagnostic messages.
!              4  = LMWD = length of machine word (must be 16, 32, or 64 bits)
!              5  = IFLG, in CBFRD: IFLG=1 forces read in RDCRBK,
!                                   IFLG=0 means proceed normally.
!                         in CBFWR: IFLG>0 is the location in the Cray block (NB)
!                         of the previous RCW (record control word); used for
!                         packing the FWI (forward word index).
!              6  = NU, number words (length LMWD) used in NB by CBFRD or CBFWR
!              7  = NA, number words (length LMWD) available in NB (for CBFRD
!                   or CBFWR)
!              8  = NR, number Cray blocks read into NB by CBFRD or written
!                   by CBFWR (starting with 0).
!              9  = NP, number words (length LMWD) to next Cray control word
!                   in NB
!             10  = NU in NBF used by RDCRBK
!             11  = NA in NBF used by RDCRBK
!             12  = NR in NBF used by RDCRBK
!             13  = position in IWK of first element of buffer NB
!             14  = length of buffer NB (in LMWD length words)
!             15  = position in IWK of first element of buffer NBF
!             16  = length of NBF (in LMWD length words)
!             17  = IRW = read/write flag (0 => not set, 1 => open for read,
!                                          2 => open for write)
!             18  = PFI, previous file index; used by CBFWR when making RCW
!                   (number of Cray blocks back to the previous EOF RCW)
!             19  = PRI, previous record index; used by CBFWR when making RCW
!                   (number of Cray blocks back to the previous EOR RCW)
!             20  = No. improperly formatted BCWs and RCWs read
!          IWK contains two arrays next:
!             NB  buffer in RDCRBK, length is 512 Cray words (64 bits)
!             NBF buffer in RDCRBK, length is 512 Cray words.
!
!             It may be more efficient to move more than 4096 Bytes on each
!             read.  To provide this capability, two arrays are employed,
!             where NBF is filled during each read, while NB is returned from
!             RDCRBK.  Currently NBFs length is 512 Cray words.  If NBF is
!             made larger, it must be increased an integral multiple number
!             of Cray blocks and IWK must be made appropriately larger.
!             Such buffering is not done when writing; i.e., only the NB
!             buffer is used by CBFWR (except when doing byte rversal).
!
!          Local declarations
!
   CHARACTER(LEN = 6)  ::  CALLNM
   CHARACTER(LEN = 6)  ::  RWST
   INTEGER, PARAMETER  ::  LCRWD = 64,LCRBK = 512*LCRWD,IZERO = 0 ,            &
                           MBCW = 0,MEOR = 8,MEOF = 14,MEOD = 15
!
!          Establish connection between fortran unit IUN and file NAME
!          Check for reasonable fortran unit numbers
!
   IF (IUN   .LT. 0 .OR. IUN   .GT. 99) GO TO 9100
   IF (MSGUN .LT. 0 .OR. MSGUN .GT. 99) GO TO 9120
!
!          Check LMWD for acceptable values
!
   ICK = 1
   IF (LMWD .EQ. 16) ICK = 0
   IF (LMWD .EQ. 32) ICK = 0
   IF (LMWD .EQ. 64) ICK = 0
   IF (ICK  .NE.  0) GO TO 9130
!
!          Check for adequate array size, then initialize assuming reading
!
   ICK = 20 + 2 * LCRBK / LMWD
   IF (LWK .LT. ICK) GO TO 9140
   IWK( 1) = LWK
   IWK( 2) = IUN
   IWK( 3) = MSGUN
   IWK( 4) = LMWD
   IWK( 5) = 1
   IWK( 6) = 99999
   IWK( 7) = 0
   IWK( 8) = 0
   IWK( 9) = 0
   IWK(10) = 99999
   IWK(11) = 0
   IWK(12) = 0
   IWK(13) = 21
   IWK(14) = LCRBK / LMWD
   IWK(15) = IWK(13) + IWK(14)
   IWK(16) = LCRBK / LMWD
!
!          Check input read/write switch value before assigning
!
   IF (IRW .LT. 1 .OR. IRW .GT.2) GO TO 9150
   IWK (17) = IRW
   DO I = 18,LWK
      IWK(I) = 0
   ENDDO
!
!          Now open the unit
!
!recl      units are bytes (works on Cray, Sun, HP-UX, IBM-AIX and PCs)
!  NBYTS = IWK(16)*LMWD/8
!recl      units are words (works on DEC, SGI)
   NBYTS = IWK(16)
!recl end
!
   RWST  = 'OLD'
   IF (IRW .EQ. 2) THEN
!
!          If writing, reset IFLG, NU, NA, and open status
!
      IWK(5) = 0
      IWK(6) = 0
      IWK(7) = IWK(14)
      RWST = 'NEW'
   ENDIF
   OPEN (IUN,STATUS=RWST,ACCESS='DIRECT',RECL=NBYTS,FILE=NAME)
   RETURN
!
!-------------------------------------------------------------------------------
!
   ENTRY CBFREW (IUN,IWK)
!          Reposition at beginning of file (before issuing next read)
   CALLNM = 'CBFREW'
   IF (IUN .NE. IWK(2)) GO TO 9200
   IF (IWK(17) .NE.  1) GO TO 9220
   IWK( 5) = 1
   IWK( 6) = 99999
   IWK( 7) = 0
   IWK( 8) = 0
   IWK( 9) = 0
   IWK(10) = 99999
   IWK(11) = 0
   IWK(12) = 0
   RETURN
!
!-------------------------------------------------------------------------------
!
   ENTRY CBFRD (IUN,IBLK,MXLBLK,LBLK,NUB,ISTAT,IWK)
!
!          Get the next tape block, IBLK, on fortran unit IUN; but first
!          check that the input read unit and work array match and check
!          that the unit has been opened for reading
!
   !
   100 CALLNM = 'CBFRD'
   !
   IF (IUN     .NE. IWK(2)) GO TO 9200
   IF (IWK(17) .NE.      1) GO TO 9220
!
!          Assign IBLK from the NB buffer, incrementing and testing NU, the
!          number of words used in NB, before assignment
!
   IWD = 0
   !
   109 IPNB   = IWK(6) + IWK(13) - 1
   110 IWK(6) = IWK(6) + 1
   !
   IPNB   = IPNB   + 1
   IF (IWK(6) .GT. IWK(9)) GO TO 200
   IWD = IWD + 1
   IF (IWD .LE. MXLBLK) IBLK(IWD) = IWK(IPNB)
   !
   GO TO 110
   !
!
!          Either the contents of NB have been exhausted or the next thing
!          in NB is a record control word (RCW)
!
   !
   200 IF (IWK(6) .LE. IWK(7)) GO TO 300
   !
!
!          Get another Cray block and put it in buffer NB
!
   CALL RDCRBK (IWK,IOST)
   IF (IOST .NE. 0) THEN
   LBLK  = 0
   NUB   = 0
   ISTAT = 3
   !
   GO TO 320
   !
   ENDIF
   IWK(5) = 0
   IWK(8) = IWK(8)+1
!
!          Parse a Cray block control word (BCW), the first 64 bits of NB
!               M    = control word type
!               NBDF = bad data flag
!               NBN  = cray block number
!               NFWI = forward word indicator (pointer to next control word)
!
   CALL GBYTE (IWK(IWK(13)),M   , 0, 4)
   CALL GBYTE (IWK(IWK(13)),NBDF,11, 1)
   CALL GBYTE (IWK(IWK(13)),NBN ,31,24)
   CALL GBYTE (IWK(IWK(13)),NFWI,55, 9)
!
!          Find no. words used by BCW, no. in buffer NB, and no. to next B/RCW
!
   IWK(6) = LCRWD / IWK(4)
   IWK(7) = LCRBK / IWK(4)
   IWK(9) = IWK(6) + NFWI*LCRWD / IWK(4)
!
   IF (M .EQ. MBCW .AND. NBDF .EQ. 0) GO TO 109
   WRITE (0     ,9250) IWK(8),IOST,M,NBDF,NBN,NFWI
   WRITE (IWK(3),9250) IWK(8),IOST,M,NBDF,NBN,NFWI
   IWK(20) = IWK(20) + 1
   IF (IWK(20) .GT. 20) GO TO 9270
   !
   GO TO 109
   !
!
!          Parse a Cray record control word, the 64 bits following each record
!               NUBC = Unused bit count in last 64 bits of cray block
!
   !
   300 IP = IWK(13) + IWK(6) - 1
   !
   CALL GBYTE (IWK(IP),M   , 0,4)
   CALL GBYTE (IWK(IP),NUBC, 4,6)
   CALL GBYTE (IWK(IP),NBDF,11,1)
   CALL GBYTE (IWK(IP),NFWI,55,9)
   IWK(6) = IWK(6) + ( LCRWD-IWK(4) )/IWK(4)
   IWK(9) = IWK(6) + NFWI*LCRWD / IWK(4)
   NUW  = NUBC / IWK(4)
   LBLK = IWD  - NUW
   NUB  = NUBC - NUW*IWK(4)
!
!          Zero fill the end of the block.  The 110 loop assigns IBLK 1 word
!          (length LMWD) at a time, but always ends at a multiple of a Cray
!          word because Cray control words are aligned on 64 bit increments.
!
   IF (IWD .LE. MXLBLK) THEN
      J = IWD + 1
      DO I = 1,NUW
         J = J - 1
         IBLK(J) = IZERO
      ENDDO
      ISKPB = LCRWD-NUB
      IF (NUB .GT. 0) CALL SBYTE (IBLK(LBLK),IZERO,ISKPB,NUB)
   ENDIF
!
!          Assign status based on RCW and return
!
   ISTAT = 1
   IF (M .EQ. MEOF) GO TO 320
   ISTAT = 3
   IF (M .EQ. MEOD .AND. NFWI .EQ. 0) GO TO 320
   ISTAT = 0
   IF (M .EQ. MEOR .AND. NBDF .EQ. 0) GO TO 320
   WRITE (0     ,9260) IWK(8),IOST,M,NUBC,NBDF,NFWI
   WRITE (IWK(3),9260) IWK(8),IOST,M,NUBC,NBDF,NFWI
   IWK(20) = IWK(20) + 1
   IF (IWK(20) .GT. 20) GO TO 9270
   ISTAT = 2
   !
   310 IWK(5) = 1
   !
   IWK(6) = 99999
   IWK(7) = 0
   IWK(9) = 0
   IIST = 0
   !
   320 CONTINUE
   !
   RETURN
!
!-------------------------------------------------------------------------------
!
      ENTRY CBFWR (IUN,IBLK,MXLBLK,LBLK,NUB,ISTAT,IWK)
!          Put record (IBLK) into Cray block structure, writing Cray blocks
!          only when filled.
!
   CALLNM = 'CBFWR'
   IF (IUN     .NE. IWK(2)) GO TO 9200
   IF (IWK(17) .NE.      2) GO TO 9300
   IF (NUB .LT. 0 .OR. NUB .GT. IWK(4)) GO TO 9320
   ISTAT = 0
   NZ = 0
   M = MEOR
   NWPC = LCRWD/IWK(4)
   NWDM = LBLK
   IF (NWDM .GT. MXLBLK) THEN
      NWDM = MXLBLK
      NUB  = 0
   ENDIF
   IF (NWDM .EQ. 0 .AND. NUB .GT. 0) NWDM = 1
   NWDC = (NWDM*IWK(4) + LCRWD-1) / LCRWD
   NUBC = NUB + NWDC*LCRWD - NWDM*IWK(4)
   NCW  = 512
   IF (IWK(6) .EQ. 0) NCW = 511
   NFWI = MIN0 (NCW-IWK(6)/NWPC,NWDC)
   IF (IWK(5) .GT. 0) THEN
!
!          Pack FWI in the EOR RCW from previous call and reset NP
!
      CALL SBYTE (IWK(IWK(5)),NFWI,55, 9)
      IWK(9) = IWK(6) + NFWI*NWPC
   ENDIF
!
   IWD = 0
   !
   1109 IPNB   = IWK(6) + IWK(13) - 1
   1110 IWK(6) = IWK(6) + 1
   !
   IPNB   = IPNB   + 1
   IF (IWK(6) .GT. IWK(9)) GO TO 1200
   IWD = IWD + 1
   IF (IWD .LE. NWDM) IWK(IPNB) = IBLK(IWD)
   !
   GO TO 1110
   !
!
!          Either initialize a Cray block, write one, or add an RCW:
!
   !
   1200 IF (IWK(6) .EQ. 1) THEN
   !
!
!          Begin a Cray block:  Initialize the BCW fields to zero (implicitly
!          setting M to MBCW), then pack BN (block number) and FWI
!
      DO I = IPNB,IPNB+NWPC-1
         IWK(I) = 0
      ENDDO
      CALL SBYTE (IWK(IPNB),IWK(8),31,24)
      CALL SBYTE (IWK(IPNB),NFWI  ,55, 9)
!
!          Set: NU, NA, and NP
!
      IWK(6) = NWPC
      IWK(7) = LCRBK / IWK(4)
      IWK(9) = IWK(6) + NFWI*NWPC
      IF (CALLNM(4:5) .EQ. 'EO') GO TO 1510
      !
      GO TO 1109
      !
   ENDIF
   IF (IWD .GE. NWDM .AND. NUBC .GT. 0 .AND. NZ .EQ. 0) THEN
!
!          Zero the unused low order bits in the last Cray word (NUBC) now
!          in case the block is full and is about to be written
!
      NZ = 1
      NWD0 = NUBC/IWK(4)
      NBI0 = NUBC - NWD0*IWK(4)
      J = IPNB - 1
      DO I = 1,NWD0
         IWK(J) = IZERO
         J = J - 1
      ENDDO
      IOFF = IWK(4) - NBI0
      IF (NBI0 .GT. 0) CALL SBYTE (IWK(J),IZERO,IOFF,NBI0)
   ENDIF
   IF (IWK(6) .GT. IWK(7)) THEN
!
!          Full Cray block:  Write Cray blk (NB), reset NU, NP, FWI, increment
!          PFI, and increment PRI if currently packing a record
!
      IWK(8) = IWK(8) + 1
!
!byte      non-byte reversed version
!
      WRITE (IUN,REC=IWK(8)) (IWK(I),I=IWK(13),IWK(13)+IWK(14)-1)
!
!byte      byte reversed version (works on PCs and DECs)
!       CALL SWAP4 (IWK(IWK(13)),IWK(IWK(15)),4096)
!       WRITE (IUN,REC=IWK(8)) (IWK(I),I=IWK(15),IWK(15)+IWK(14)-1)
!byte      end
!
      IWK(6) = 0
      IWK(9) = 0
      NFWI = MIN0 (511,NWDC - IWD/NWPC)
      IWK(18) = IWK(18) + 1
      IF (CALLNM .EQ. 'CBFWR') IWK(19) = IWK(19) + 1
      IF (CALLNM(4:5) .EQ. 'EO') GO TO 1510
      !
      GO TO 1109
      !
   ENDIF
!
!          Add an RCW, setting M, UBC, PFI and PRI fields, but only zeroing
!          FWI which is unknown until next write request.
!
   DO I = IPNB,IPNB+NWPC-1
      IWK(I) = 0
   ENDDO
   CALL SBYTE (IWK(IPNB),M      , 0, 4)
   CALL SBYTE (IWK(IPNB),NUBC   , 4, 6)
   CALL SBYTE (IWK(IPNB),IWK(18),20,20)
   CALL SBYTE (IWK(IPNB),IWK(19),40,15)
!
!          Set IFLG to the position where FWI is to be packed and increment NU
!
   IWK(5) = IPNB
   IWK(6) = IWK(6) + NWPC - 1
   IF (CALLNM .EQ. 'CBFEOF' .OR. CALLNM(1:3) .EQ. 'CLS') GO TO 1520
!
!          Reset PRI
!
   IWK(19) = 0
   RETURN
!
!-------------------------------------------------------------------------------
!
   ENTRY CBFEOF (IUN,IWK)
!
!          Write an EOF to the Cray block output on unit IUN.
!
   CALLNM = 'CBFEOF'
   IF (IUN     .NE. IWK(2)) GO TO 9200
   IF (IWK(17) .NE.      2) GO TO 9300
   !
   1500 M = MEOF
   1505 NWPC = LCRWD/IWK(4)
   !
   NWDM = 0
   NWDC = 0
   NUBC = 0
   NFWI = 0
!
!          IFLG may be ignored because the FWI in the last control word
!          (pointing to this RCW) has already been initialized to 0.
!
   IWD = 0
   !
   1510 IPNB   = IWK(6) + IWK(13)
   !
   IWK(6) = IWK(6) + 1
   !
   GO TO 1200
   !
!
!          Reset PFI after packing the EOF RCW
!
   !
   1520 IWK(18)= 0
   !
   IF (CALLNM .EQ. 'CLSEOF') GO TO 1610
   IF (CALLNM .EQ. 'CLSEOD') GO TO 1620
   RETURN
!
!-------------------------------------------------------------------------------
!
   ENTRY CBFCLS (IUN,IWK)
!
!          Close fortran unit number IUN
!
   CALLNM = 'CBFCLS'
   IF (IUN .NE. IWK(2)) GO TO 9200
   IF (IWK(17) .EQ.  1) GO TO 1700
   IF (IWK(17) .NE.  2) GO TO 9400
!
!          IUN is open for write
!
   NWPC = LCRWD/IWK(4)
!
!          Add an EOF RCW if the previous Cray word in NB was not an EOF RCW
!
   IF (IWK(5)  .EQ. 0) GO TO 1600
   IF (IWK(18) .NE. 0) GO TO 1600
   IF (IWK(5)  .NE. IWK(6)+IWK(13)-NWPC) GO TO 1600
   CALL GBYTE (IWK(IWK(5)),LASTM,0,4)
   IF (LASTM .EQ. MEOF) GO TO 1610
   !
   1600 CALLNM = 'CLSEOF'
   GO TO 1500
   !
!
!          Add the EOD RCW, zero the rest of the Cray block and write it
!
   !
   1610 M = MEOD
   !
   CALLNM = 'CLSEOD'
   !
   GO TO 1505
   !
!
!          Zero out the remainder of the Cray block, then write it
!
   !
   1620 IENB = IWK(13)+IWK(14)-1
   !
   DO I = IWK(6)+IWK(13),IENB
      IWK(I) = 0
   ENDDO
   IWK(8) = IWK(8) + 1
!
!byte      non-byte reversed version:
!
      WRITE (IUN,REC=IWK(8)) (IWK(I),I=IWK(13),IENB)
!
!byte      byte reversed version (works on PC's and DEC's)
!     CALL SWAP4 (IWK(IWK(13)),IWK(IWK(15)),4096)
!     WRITE (IUN,REC=IWK(8)) (IWK(I),I=IWK(15),IWK(15)+IWK(14)-1)
!byte      end
!
   !
   1700 IWK(17) = 0
   !
   CLOSE (IUN)
!
   RETURN
!
!-------------------------------------------------------------------------------
!
!          Diagnostic messages:  Some are written to stderr (unit 0) if
!          different than MSGUN, in case the work array (IWK) was clobbered
!
   9100 WRITE(MSGUN,9101) IUN
   9101 FORMAT(' CBFOPN:  Goofy fortran unit number input;',' IUN = ',I6)
   STOP
   9120 WRITE(0,9121) MSGUN
   9121 FORMAT(' CBFOPN:  Goofy fortran unit number for',                      & 
               ' diagnostic prints; MSGUN =',I6)
   STOP
   9130 WRITE(MSGUN,9131) LMWD
   9131 FORMAT(' CBFOPN:  Too dumb to handle word length of',I6)
   STOP
   9140 WRITE(MSGUN,9141) LWK,ICK
   9141 FORMAT(' CBFOPN:  Insufficient work array size',                       &
               ' specified.  LWK input =',I8,/,' but LWK must be at least =',I8)
   STOP
   9150 WRITE(MSGUN,9151) IRW
   9151 FORMAT(' CBFOPN:  IRW input =',I6,' is unacceptable.',                 &
               'IRW may be 1 or 2 (unit is open for read or write)')
   STOP
   9200 WRITE (0,9210) CALLNM, IUN, IWK(2)
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9210) CALLNM, IUN, IWK(2)
   9210 FORMAT(' ',A6,': mismatch between input unit number',                  &
                   I4,' and work array value',I5)
   STOP
   9220 WRITE (0,9230) CALLNM, IUN, IWK(17)
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9230) CALLNM, IUN, IWK(17)
   9230 FORMAT(' ',A6,':  Unit',I8,' not open for read (IRW must be 1); ',     &
                      ' IRW =',I8)
   STOP
   9250 FORMAT(' CBFRD: BCW/IOST goofy - NR,IOST,M,NBDF,NBN,NFWI=',6I6)
   9260 FORMAT(' CBFRD: RCW/IOST goofy - NR,IOST,M,NUBC,NBDF,NFWI=',6I6)
   9270 WRITE (0,9280)
   9280 FORMAT (' CBFRD:  Stopping, too many BCW/RCW errors')
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9280)
   STOP
   9300 WRITE (0,9310) CALLNM, IUN, IWK(17)
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9310) CALLNM, IUN, IWK(17)
   9310 FORMAT(' ',A6,':  Unit not open for write (IRW must be 2); IUN,',      &
                      ' IRW =',2I8)
   STOP
   9320 WRITE (IWK(3),9330) NUB
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9330) NUB
   9330 FORMAT (' CBFWR:  0 < NUB =< LMWD, but NUB =',I8)
   STOP
   9400 WRITE (0,9410) CALLNM, IUN, IWK(17)
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9310) CALLNM, IUN, IWK(17)
   9410 FORMAT(' ',A6,':  Unit',I8,' not open (IRW must be 1 or 2); ',         &
                      ' IRW =',I8)
   STOP
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE RDCRBK (IWK,IOST)
!
!          COS block input buffering routine called by CBFRD:  Each call
!          returns a Cray block (512*64 bits or 4096 Bytes).  The Cray block
!          buffer, a read buffer, pointers, and flags are buried in the work
!          array IWK (described in CBFOPN comments).  IOST, the IOSTAT
!          value from the previous read, is only needed for an unusual
!          condition (when the COS block is truncated and EOD control word
!          is missing).
!
   INTEGER, PARAMETER  ::  LCRWD = 64, LCRBK = 512*LCRWD  
   INTEGER             ::  IWK(*)
   DATA                     LIOST /0/
   SAVE                     LIOST
!
   IOST = 0
!
!          Determine the number of words in a Cray block
!
   NMWD = LCRBK / IWK(4)
!
!          If there is another full Cray block and the force read flag (IFLG)
!          is off then go assign it before doing another read
!
   IF( IWK(10)+NMWD .LE. IWK(11) .AND. IWK(5) .EQ. 0) GO TO 5
!
!          Probably the end of file was encountered on previous read (this
!          can happen when the last block is truncated but still contains
!          an EOD RCW
!
   IF (LIOST .NE. 0) THEN
      IOST = LIOST
      RETURN
   ENDIF
!
!          Increment no. reads (NR) and load read buffer (NBF)
!
   IWK(12) = IWK(12) + 1
   READ (IWK(2),REC = IWK(12),IOSTAT = LIOST)(IWK(J),J = IWK(15),              &
                                                         IWK(15)+IWK(16)-1)
!
!          Set no. words used (NU) and the no. available (NA) to length of NBF
!
   IWK(10) = 0
   IWK(11) = IWK(16)
!
!          Assign a Cray block to NB, the return buffer, from NBF
!byte      byte reversed version (works on PCs and DECs)
!   5 IPNB  = IWK(13)
!     IPNBF = IWK(15) + IWK(10)
!     CALL SWAP4 (IWK(IPNBF),IWK(IPNB),4096)
!byte      non-byte reversed version
!
   !
   5 IPNB  = IWK(13) - 1
   !
   IPNBF = IWK(15) + IWK(10) - 1
   DO I = 1,NMWD
      IPNB  = IPNB  + 1
      IPNBF = IPNBF + 1
      IWK(IPNB) = IWK(IPNBF)
   ENDDO
!
!byte      end
!
!          Update no. words used (NU) in NBF
!
   IWK(10) = IWK(10) + NMWD
   RETURN
!
   END
