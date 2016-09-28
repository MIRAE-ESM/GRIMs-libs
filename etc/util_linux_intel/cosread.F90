!
   program crayREAD
!
! this routine has to be compiled without -qrealsize=8
!
! This program works but we don't have all the information to
! finish it yet.  This program is is to be run on NCAR CRAY systems.
! It has also been tested on Sun Workstations, but you need to comment
! out lines with '!cray' and reactivate lines with '!sun' plus getting
! the following files from our anonymous ftp area on ncardata.ucar.edu:
!   ftp/libraries/io/cbfopn.f
!   ftp/libraries/gbytes/f77.f
!
! This is to read REANALYSIS binary files.
! 
!  integer, parameter    ::  nbit = 64,nword = nbit/8      ! cray
   integer, parameter    ::  nbit = 32,nword = nbit/8      ! sun
   integer, parameter    ::  ncray = 64/8
   integer, parameter    ::  mxx = 50000, mxgaus = 192*94
   INTEGER, PARAMETER    ::  IUN = 1,MSGUN = 6,LMWD = nbit,IRW = 1,LWK = mxx
!
   character             ::  hed*(4*ncray), ahed*(4*ncray), name*72
   character(len=80 )    ::  fname
   character(len=8  )    ::  ftype
   character(len=8  )    ::  label(4)
!
   integer               ::  ione(mxx),ihed(4*ncray/nword)
   integer               ::  iwk(lwk)
!
   logical                   znl,on85,gauss
!
!  real                  ::  a(mxx)
   real*4                    a(mxx)
   real*4                    fhour
!  integer               ::  idate(4)
   integer*4                 idate(4)
!
   equivalence (ihed,hed)
!
   namelist/namfl/ fname,ftype
!
!  call assign("assign -a data.in -b 4 fort.10")           !cray
!
! if the first time, open it and check for recl
!
!  write(*,*)'enter input COS-blocked file? '              !sun
!  read(*,'(a)') name                                      !sun
   read(5,namfl)
   write(6,namfl)
   name = fname
   if (ftype(1:3).eq.'sig') then
      isigsfc = 0
   elseif (ftype(1:3).eq.'sfc') then
      isigsfc = 1
   endif
   call CBFOPN(IUN,NAME,MSGUN,LMWD,IRW,iwk,LWK)            !sun
   nrec=0
   !
   11 continue
   !
!     buffer in (10,0) (ione(1),ione(mxx))                 !cray
!     klen=length(10)                                      !cray
   call cbfrd(iun,ione,mxx,lblk,nub,istat,iwk)             !sun
   klen=lblk                                               !sun
   if(klen.eq.0) then
      write(*,*) 'END_OF_FILE during read, exit'
      !
      go to 999
      !
   endif
   if(istat.ne.0) then                                     !sun
      write(*,*) '****ERROR during cbfrd, exit',istat      !sun
      !
      go to 999                                            !sun
      !
   endif                                                   !sun
   if(nub.ne.0) then                                       !sun
      write(*,*)'****ERROR, unused bits NOT zero',nub      !sun
      !
      go to 999                                            !sun
      !
   endif                                                   !sun
   lenrd=klen*nword
   nrec=nrec+1
!  write(*,*) 'rec: ',nrec,klen
!  if(nrec.eq.4) write(31) (ione(i),i=1,klen)
   amx=-99999.
   amn=99999.
   if(nrec.eq.1) then
      if(lenrd.eq.32) then
         hed=' '
         do i = 1,klen
            ihed(i)=ione(i)
         enddo
      else
         write(*,*) '****ERROR in header ',klen*nword
         !
         go to 999
         !
      endif
!
! header should have 32 bytes, three formats used:
!  if (first 3 bytes = ZNL ) then
!     byte: 1-8  : ASCII
!           9-32 : blank
!  else if (first 3 bytes = blank ) then
!     byte: 1-32 : blank
!  else 
!     byte: 1-6  : ASCII, data set name
!           7-8  : ASCII, set type (B2=binary sequential sets)
!           9    : year of the century, gbtye
!           10   : month, gbyte
!           11   : day of the month, gbyte
!           12   : initial GMT time, gbyte
!           13   : run number as described in ON#84.
!           14   : not used
!           15-16: set initiate time (GMT hour*100), gbyte
!           17-32: EBCDIC
!  endif
!
      znl=.false.
      on85=.false.
      if(hed(1:1).eq.'Z') then
         znl=.true.
         write(6,*) ' ZNL set name:   ',hed(1:32)
      elseif(ichar(hed(1:1)).ne.0 .and. .not.znl) then
         on85=.true.
         write(6,*) ' ON85 set name:   ',hed(1:6)
         write(6,*) '      set type:   ',hed(7:8)
         call gbytes(hed,idate,64,8,0,4)
         write(6,*) '       y,m,d,h:   ',idate
         irun=0
         if(hed(13:13).ne.' ') call gbyte(hed,irun,96,8)
         write(6,*) '          run#:   ',irun
         ini=0
         if(hed(15:16).ne.'  ') call gbyte(hed,ini,112,16)
         write(6,*) '      ini time:   ',ini
         call ebcasc(hed,ahed,32)
!        writ(6,*) '        EBCDIC:   ',ahed(17:30)
      else
         write(*,*) ' BLANK set name: ',hed(1:32)
      endif
      print *,'label wa hed',hed
      label(1) = hed(1:8)
      label(2) = hed(9:16)
      label(3) = hed(17:24)
      label(4) = hed(25:32)
      print *,'label wa hed',label
      !
      go to 11
      !
!
   elseif(nrec.eq.2) then
!
!  if(znl) then
!             byte:  1-4  : valid hour (real*4)
!                    5-8  : ?
!                    9-12 : hour
!                   13-16 : month
!                   17-20 : day
!                   21-24 : year
!                   25-   : unpk360
!  elseif(on85) then
!             byte:  1-4  : valid hour (real*4)
!                    5-8  : hour
!                    9-12 : month
!                   13-16 : day
!                   17-20 : year
!                   21-24 : ?
!                   25-   : unpk360
!  else                                      (24 bytes only)
!             byte:  1-4  : valid hour (real*4)
!                    5-8  : hour
!                    9-12 : month
!                   13-16 : day
!                   17-20 : year
!                   21-24 : ?NOT USED
!  endif
!
      if(znl) then
         call unp360(ione(1),a,2)
         write(*,*) ' ZNL   first 8:   ',a(1),a(2)
         do i = 1,4*ncray/nword
            ihed(i)=ione(i)
         enddo
         call gbytes(hed,idate,64,32,0,4)
         write(*,*) ' ZNL   h,m,d,y:   ',idate
!
!        ihed(1)=ione(7)
!        ihed(2)=ione(8)
!        call ebcasc(hed,ahed,8)
!        write(*,*) ' ::: ',ahed(1:8)
         call gbyte(ione(12011),idum,0,32)
         nofa=(klen*nword-24)/4
         write(*,*) '      12011   :   ',idum,nofa
         call unp360(ione(7),a,nofa)
         do i = 1,nofa
!           if(i.le.40) write(*,*) 'i= ',i,a(i)
!           if((i.ge.12010) .and. (i.le.12030)) write(*,*) 'i= ',i,a(i)
            if(a(i).gt.amx) then
               kmx=i
               amx=a(i)
            endif
            if(a(i).lt.amx) then
               kmn=i
               amn=a(i)
            endif
         enddo
         write(*,*) '        max,min: ',kmx,amx,kmn,amn
      elseif(on85) then
         call unp360(ione(1),a,1)
         write(*,*) ' ON85  first 4:   ',a(1)
         do i = 1,4*ncray/nword
            ihed(i)=ione(i)
         enddo
         call gbytes(hed,idate,32,32,0,4)
         write(*,*) ' ON85  h,m,d,y:   ',idate,nofa
         nofa=(klen*nword-20)/4
! for conversion, write,a(1),idate    
!        label(1:4) = hed(1:4)  
         print *,'label',label
         write(51) label
         fhour = a(1)
         call unp360(ione(6),a,nofa)
!
! a(01)..a(29) are sigma interfaces
! a(30)..a(57) are mid-level sigma values
!  
         write(*,*) ' a(01)..a(29) are sigma interfaces'
         write(*,*) ' a(30)..a(57) are mid-level sigma values'
         do i = 1,nofa
            write(*,*) ' i= ',i,a(i)
         enddo
! for conversion, write,a(1,57)         
!        write(51) fhour,idate,(a(i),i=1,57)
         write(51) fhour,idate,(a(i),i=1,nofa)
      else
         call unp360(ione(1),a,1)
         write(*,*) ' BLANK first 4:   ',a(1)
         do i = 1,3*ncray/nword
            ihed(i)=ione(i)
         enddo
         call gbytes(hed,idate,32,32,0,4)
         write(*,*) ' BLANK  h,m,d,y:   ',idate
         call unp360(ione(6),a,1)
         write(*,*) ' BLANK 21-24  :   ',a(1)
         write(51) label
         write(51) fhour,idate
      endif
!
! unpack the data
!
   else
      nofa=klen*nword/4
      gauss=.false.
      if(mod(nofa,mxgaus).eq.0) gauss=.true.
      call unp360(ione,a,nofa)
      do i = 1,nofa
         if(a(i).gt.amx) then
            kmx=i
            amx=a(i)
         endif
         if(a(i).lt.amn) then
            kmn=i
            amn=a(i)
         endif
      enddo
! for conversion, write,a(1,4032)  
      print *,'nofa',nofa       
      write(51) (a(i),i=1,nofa)
      if(gauss) then
         write(*,1210) nrec,nofa,kmx,amx,kmn,amn
         !
         1210 format(' GAUSS record: ',i5,i8,'  max,min: ',2(i8,f14.6))
         !
      else
         write(*,1213) nrec,nofa,kmx,amx,kmn,amn
         !
         1213 format('       record: ',i5,i8,'  max,min: ',2(i8,f14.6))
         !
      endif
   endif
!  if(nrec.lt.7) go to 11
   !
   go to 11
   !
!
   !
   999 continue
   !
   call cbfcls(iun,iwk)                                    !sun
!
! to clear the SUN IEEE computational flags                !sun
!     idum=ieee_flags("clear","exception","all",outstr)    !sun
!
   stop
!
   end
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE UNP360(IN,IO,N)                                                
!
   SAVE
!  INTEGER, PARAMETER    ::  IWSZ = 64                     !cray
   INTEGER, PARAMETER    ::  IWSZ = 32                     !sun
!
! FOR EXECUTION ON WORD SIZE IWSZ (>=32)
! CONVERTS ARRAYS OF IBM 360/370 WORDS WHICH ARE IN A CONTINUOUS BIT            
!       STREAM INTO HOST WORDS.
! WILL ACCEPT MIXED ARRAYS OF REAL (REAL*4) AND INTEGER (INTEGER*4)             
!       AND RETURNS THE APPROPRIATE CRAY REAL OR FULL WORD INTEGER              
!       VALUES.                                                                 
!       IN = ARRAY CONTAINING 360 WORDS AS A CONTINUOUS BIT STREAM. M           
!       IO = ARRAY FOR CRAY OUTPUT WORDS.                                       
!       N  = NUMBER OF VALUES TO CONVERT.                                       
!
   INTEGER               ::  IN(1),IO(1)                                                     
!
   IOF=0                                                                     
   DO I = 1,N                                                               
      CALL GBYTES(IN,IS,IOF,1,0,1)                                              
      IOF=IOF+1                                                                 
      CALL GBYTES(IN,IEX,IOF,7,0,1)                                             
      IOF=IOF+7                                                                 
      CALL GBYTES(IN,IFR,IOF,24,0,1)                                            
      IF(IEX .EQ. 0 .OR. IEX .EQ. 127) GO TO 10                                 
      XV=FLOAT(IFR)*16.**(IEX-70)                                               
      IF(IS .NE. 0) XV=-XV                                                      
      CALL SBYTES(IO(I),XV,0,IWSZ,0,1)
      !
      GOTO 20                                                                   
      !
      10 CONTINUE                                                                  
      !
      IF(IS .NE. 0) IFR=IFR-16777216                                            
      IO(I)=IFR                                                                 
      !
      20 IOF=IOF+24                                                                
      !
   ENDDO
!
   RETURN                                                                    
!
   END                                                                       
!
!-------------------------------------------------------------------------------
!
   subroutine ebcasc(nc,nd,num)
!
! convert num characters from ebcdic to ascii, nc is ebcdic input string
! and nd is ascii output string, nc and nd may be the same string.
!
! the conversion table corresponds to the NCAR import/export conversion
! and should give the same results.
!
   integer               ::  ntab(256)
   character             ::  nc*(*),nd*(*)
   data   ntab/                                                                &
              000,001,002,003,156,009,134,127,151,141,142,011,012,013,014,015, &
              016,017,018,019,157,133,008,135,024,025,146,143,028,029,030,031, &
              128,129,130,131,132,010,023,027,136,137,138,139,140,005,006,007, &
              144,145,022,147,148,149,150,004,152,153,154,155,020,021,158,026, &
              032,160,161,162,163,164,165,166,167,168,213,046,060,040,043,124, &
              038,169,170,171,172,173,174,175,176,177,033,036,042,041,059,094, &
              045,047,178,179,180,181,182,183,184,185,229,044,037,095,062,063, &
              186,187,188,189,190,191,192,193,194,096,058,035,064,039,061,034, &
              195,097,098,099,100,101,102,103,104,105,196,197,198,199,200,201, &
              202,106,107,108,109,110,111,112,113,114,203,204,205,206,207,208, &
              209,126,115,116,117,118,119,120,121,122,210,211,212,091,214,215, &
              216,217,218,219,220,221,222,223,224,225,226,227,228,093,230,231, &
              123,065,066,067,068,069,070,071,072,073,232,233,234,235,236,237, &
              125,074,075,076,077,078,079,080,081,082,238,239,240,241,242,243, &
              092,159,083,084,085,086,087,088,089,090,244,245,246,247,248,249, &
              048,049,050,051,052,053,054,055,056,057,250,251,252,253,254,255/
   do i = 1,num
      nd(i:i)=char(ntab(ichar(nc(i:i))+1))
   enddo
!
   return
!
   end
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE CBFOPN (IUN,NAME,MSGUN,LMWD,IRW,IWK,LWK)
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
!            Cbyte  PC's and DEC's interpret bytes in each word in the
!                   reverse order of most other computers.  It is probably
!                   best to turn on byte reversal on input and output, so
!                   the order in the files is consistant with other machines.
!                   However, if files are only written and read back locally,
!                   one can leave everything byte reversed (and gain a
!                   slight speedup).  This requires subroutine SWAP4.
!
!            Crecl  The 'RECL' argument units may be bytes or words.
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
!
!          Mar 93:  Added CBFWR and CBFEOF.  16-BIT MACHINES MUST declare
!          array IWK as INTEGER*4 and set LMWD=32, to avoid a truncation
!          problem in CBFWR when calculating the block number (modulus 2**24),
!          forward word index (modulus 2**20), and previous file index
!          (modulus 2**20) for BCW and RCW entries.  Note that these modulus
!          calculations are implicit by SBYTE packing.
!
!          Oct 97:  Changed termination conditions to handle reading files
!          with truncated last COS blocks missing an EOD RCW.  RDCRBK returned
!          status (IOST) definition differs and IWK(20) is defined.
!
!          Aug 99:  Revised truncated last block logic s.t. a non-zero IOSTAT
!          is stored as a flag in IWK(20) rather than local variable in RDCRBK
!          (a shortsighted design - wonder what I was thinking!).  Now IWK(20)
!          serves two purposes: bump up the IOST flag value in RDCRBK when
!          increasing the allowed maximum number of COS blks w/o a good BCW!

!          Formal parameter declarations for all entries
!
   CHARACTER*(*)             NAME
   INTEGER               ::  IBLK(MXLBLK) , IWK(*)
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
!             20  = No. improperly formatted BCW's and RCW's read
!          IWK contains two arrays next:
!             NB  buffer in RDCRBK, length is 512 Cray words (64 bits)
!             NBF buffer in RDCRBK, length is 512 Cray words.
!
!             It may be more efficient to move more than 4096 Bytes on each
!             read.  To provide this capability, two arrays are employed,
!             where NBF is filled during each read, while NB is returned from
!             RDCRBK.  Currently NBF's length is 512 Cray words.  If NBF is
!             made larger, it must be increased an integral multiple number
!             of Cray blocks and IWK must be made appropriately larger.
!             Such buffering is not done when writing; i.e., only the NB
!             buffer is used by CBFWR (except when doing byte rversal).

!          Local declarations
!
   CHARACTER(LEN = 6  )  ::  CALLNM
   CHARACTER(LEN = 3  )  ::  RWST
   INTEGER, PARAMETER    ::  LCRWD = 64 , LCRBK = 512*LCRWD, IZERO = 0,        &
                             MBCW  = 0  , MEOR  = 8 , MEOF = 14, MEOD = 15
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
!recl      units are bytes (works on Cray, Sun, HP-UX, IBM-AIX and PC's)
!
   NBYTS = IWK(16)*LMWD/8
!
!recl      units are words (works on DEC, SGI)
!  NBYTS = IWK(16)
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
!
   OPEN (IUN,STATUS=RWST,ACCESS='DIRECT',RECL=NBYTS,FILE=NAME)
!
   !
   RETURN
   !
!-------------------------------------------------------------------------------
   ENTRY CBFREW (IUN,IWK)
!
!          Reposition at beginning of file (before issuing next read)
!
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
   !
   RETURN
   !
!-------------------------------------------------------------------------------
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
   !
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

   IF (M .EQ. MBCW .AND. NBDF .EQ. 0) GO TO 109
   WRITE (0     ,9250) IWK(8),IOST,M,NBDF,NBN,NFWI
   WRITE (IWK(3),9250) IWK(8),IOST,M,NBDF,NBN,NFWI
   IWK(20) = IWK(20) + 1
   IF (IWK(20) .GT. 20) GO TO 9270    ! see also RDCRBK's use of IWK(20)
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
   IF (IWK(20) .GT. 20) GO TO 9270    ! see also RDCRBK's use of IWK(20)
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
   ENTRY CBFWR (IUN,IBLK,MXLBLK,LBLK,NUB,ISTAT,IWK)
!
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

   IWD = 0
   !
   1109 IPNB   = IWK(6) + IWK(13) - 1
   !
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
!byte      byte reversed version (works on PC's and DEC's)
!     CALL SWAP4 (IWK(IWK(13)),IWK(IWK(15)),4096)
!     WRITE (IUN,REC=IWK(8)) (IWK(I),I=IWK(15),IWK(15)+IWK(14)-1)
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
   IWK(19)= 0
   !
   RETURN
   !
!-------------------------------------------------------------------------------
   ENTRY CBFEOF (IUN,IWK)
!
!          Write an EOF to the Cray block output on unit IUN.
!
   CALLNM = 'CBFEOF'
   IF (IUN     .NE. IWK(2)) GO TO 9200
   IF (IWK(17) .NE.      2) GO TO 9300
   !
   1500 M = MEOF
   !
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
   !
   RETURN
   !
!-------------------------------------------------------------------------------
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
   !
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
!
!          Diagnostic messages:  Some are written to stderr (unit 0) if
!          different than MSGUN, in case the work array (IWK) was clobbered
!
   !
   9100 WRITE(MSGUN,'('' CBFOPN:  Goofy fortran unit number input;'',          &
                    '' IUN = '',I6)') IUN
   STOP
   !
   9120 WRITE(0,'('' CBFOPN:  Goofy fortran unit number for'',                 &
                '' diagnostic prints; MSGUN ='',I6)') MSGUN
   STOP
   !
   9130 WRITE(MSGUN,'('' CBFOPN:  Too dumb to handle word length of'',I6)      &
                    ') LMWD
   STOP
   !
   9140 WRITE (MSGUN,'('' CBFOPN:  Insufficient work array size'',             &
                     '' specified.  LWK input ='',I8,/,                        &
                     ''          but LWK must be at least ='',I8)')LWK,ICK
   STOP
   !
   9150 WRITE(MSGUN,'('' CBFOPN:  IRW input ='',I6,'' is unacceptable. '',     &
                    ''IRW may be 1 or 2 (unit is open '',/,                    &
                    ''          for read or write).'')') IRW
   STOP
   !
   9200 WRITE (0,9210) CALLNM, IUN, IWK(2)
   !
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9210) CALLNM, IUN, IWK(2)
   !
   9210 FORMAT(' ',A6,': mismatch between input unit number',                  &
                   I4,' and work array value',I5)
   STOP
   !
   9220 WRITE (0,9230) CALLNM, IUN, IWK(17)
   !
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9230) CALLNM, IUN, IWK(17)
   !
   9230 FORMAT(' ',A6,':  Unit',I8,' not open for read (IRW must be 1); ',     &
               ' IRW =',I8)
   STOP
   !
   9250 FORMAT(' CBFRD: BCW/IOST goofy - NR,IOST,M,NBDF,NBN, NFWI=',6I6)
   !
   9260 FORMAT(' CBFRD: RCW/IOST goofy - NR,IOST,M,NUBC,NBDF,NFWI=',6I6)
   !
   9270 WRITE (0,9280)
   !
   9280 FORMAT (' CBFRD:  Stopping, too many BCW/RCW errors')
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9280)
   STOP

   !
   9300 WRITE (0,9310) CALLNM, IUN, IWK(17)
   !
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9310) CALLNM, IUN, IWK(17)
   !
   9310 FORMAT(' ',A6,':  Unit not open for write (IRW must be 2); IUN,',      &
               ' IRW =',2I8)
   STOP
   !
   9320 WRITE (IWK(3),9330) NUB
   !
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9330) NUB
   !
   9330 FORMAT (' CBFWR:  0 < NUB =< LMWD, but NUB =',I8)
   !
   STOP

   !
   9400 WRITE (0,9410) CALLNM, IUN, IWK(17)
   !
   IF (IWK(3) .NE. 0) WRITE (IWK(3),9310) CALLNM, IUN, IWK(17)
   !
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
   INTEGER, PARAMETER    ::  LCRWD = 64, LCRBK = 512*LCRWD
   INTEGER               ::  IWK(*)

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
!          can happen when the last COS block is truncated after an EOR RCW;
!          i.e. it's missing the EOD RCW and fill to 4096 bytes
!
   IF (IWK(20) .GT. 999) THEN
      IOST = 1
      !
      RETURN
      !
   ENDIF
!
!          Increment no. reads (NR) and load read buffer (NBF)
!
   IWK(12) = IWK(12) + 1
   READ (IWK(2),REC=IWK(12),IOSTAT=IOS)(IWK(J),J=IWK(15),IWK(15)+IWK(16)-1)
!
!          Non-zero IOST probably means this is a rare improperly terminated
!          last COS block which has no EOD RCW which must be remembered s.t.
!          it can be handled on the next call.  Note that IWK(20) is also used
!          to count no. COS blks w/o a BCW (traps bad beginning)
!
   IF (IOS .NE. 0) IWK(20) = IWK(20) + 1000
!
!          Set no. words used (NU) and the no. available (NA) to length of NBF
!
   IWK(10) = 0
   IWK(11) = IWK(16)
!
!          Assign a Cray block to NB, the return buffer, from NBF
!byte      byte reversed version (works on PC's and DEC's)
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
!
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE GBYTE (IN,IOUT,ISKIP,NBYTE)
!
   CALL GBYTES (IN,IOUT,ISKIP,NBYTE,0,1)
!
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE SBYTE (IOUT,IN,ISKIP,NBYTE)
!
   CALL SBYTES (IOUT,IN,ISKIP,NBYTE,0,1)
!
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE GBYTES (IN,IOUT,ISKIP,NBYTE,NSKIP,N)
!
!          Get bytes - unpack bits:  Extract arbitrary size values from a
!          packed bit string, right justifying each value in the unpacked
!          array.
!
   INTEGER               ::  IN(*), IOUT(*)
!
!            IN    = packed array input
!            IO    = unpacked array output
!            ISKIP = initial number of bits to skip
!            NBYTE = number of bits to take
!            NSKIP = additional number of bits to skip on each iteration
!            N     = number of iterations
!************************************** MACHINE SPECIFIC CHANGES START HERE
!          Machine dependent information required:
!            LMWD   = Number of bits in a word on this machine
!            MASKS  = Set of word masks where the first element has only the
!                     right most bit set to 1, the second has the two, ...
!            LEFTSH = Shift left bits in word M to the by N bits
!            RGHTSH = Shift right
!            OR     = Logical OR (add) on this machine.
!            AND    = Logical AND (multiply) on this machine
!          This is for Sun UNIX Fortran, DEC Alpha, and RS6000
!
   INTEGER, PARAMETER    ::  LMWD = 32
   INTEGER, SAVE         ::  MASKS(LMWD)
   DATA      MASKS /'1'X,'3'X,'7'X,'F'X, '1F'X,'3F'X,'7F'X,'FF'X,              &
   '1FF'X,'3FF'X,'7FF'X,'FFF'X, '1FFF'X,'3FFF'X,'7FFF'X,'FFFF'X,               &
   '1FFFF'X,       '3FFFF'X,       '7FFFF'X,       'FFFFF'X,                   &
   '1FFFFF'X,      '3FFFFF'X,      '7FFFFF'X,      'FFFFFF'X,                  &
   '1FFFFFF'X,     '3FFFFFF'X,     '7FFFFFF'X,     'FFFFFFF'X,                 &
   '1FFFFFFF'X,    '3FFFFFFF'X,    '7FFFFFFF'X,    'FFFFFFFF'X/
!     '1FFFFFFFF'X,   '3FFFFFFFF'X,   '7FFFFFFFF'X,   'FFFFFFFFF'X,            &
!     '1FFFFFFFFF'X,  '3FFFFFFFFF'X,  '7FFFFFFFFF'X,  'FFFFFFFFFF'X,           &
!     '1FFFFFFFFFF'X, '3FFFFFFFFFF'X, '7FFFFFFFFFF'X, 'FFFFFFFFFFF'X,          &
!     '1FFFFFFFFFFF'X,'3FFFFFFFFFFF'X,'7FFFFFFFFFFF'X,'FFFFFFFFFFFF'X,         &
!     '1FFFFFFFFFFFF'X,   '3FFFFFFFFFFFF'X,   '7FFFFFFFFFFFF'X,                &
!                                             'FFFFFFFFFFFFF'X,                &
!     '1FFFFFFFFFFFFF'X,  '3FFFFFFFFFFFFF'X,  '7FFFFFFFFFFFFF'X,               &
!                                             'FFFFFFFFFFFFFF'X,               &
!     '1FFFFFFFFFFFFFF'X, '3FFFFFFFFFFFFFF'X, '7FFFFFFFFFFFFFF'X,              &
!                                             'FFFFFFFFFFFFFFF'X,              &
!     '1FFFFFFFFFFFFFFF'X,'3FFFFFFFFFFFFFFF'X,'7FFFFFFFFFFFFFFF'X,             &
!                                             'FFFFFFFFFFFFFFFF'X/
!
!          IBM PC using Microsoft Fortran uses different syntax:
!
!  DATA MASKS/16#1,16#3,16#7,16#F,16#1F,16#3F,16#7F,16#FF,                     &
!      16#1FF,16#3FF,16#7FF,16#FFF,16#1FFF,16#3FFF,16#7FFF,16#FFFF,            &
!      16#1FFFF,16#3FFFF,16#7FFFF,16#FFFFF,16#1FFFFF,16#3FFFFF,                &
!      16#7FFFFF,16#FFFFFF,16#1FFFFFF,16#3FFFFFF,16#7FFFFFF,16#FFFFFFF,        &
!      16#1FFFFFFF,16#3FFFFFFF,16#7FFFFFFF,16#FFFFFFFF/
!
   INTEGER               ::  RGHTSH, OR, AND
!
   LEFTSH(M,N) = ISHFT(M,N)
   RGHTSH(M,N) = ISHFT(M,-N)
!
!     OR(M,N)  = M.OR.N
!     AND(M,N) = M.AND.N
!     OR(M,N)  = IOR(M,N)
!     AND(M,N) = IAND(M,N)
!************************************** MACHINE SPECIFIC CHANGES END HERE
!          History:  written by Robert C. Gammill, jul 1972.
!
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

   DO 6 I = 1,N                                                                
      MOVER = ICON-II
      IF (MOVER) 2,3,4
!
!          The "byte" is split across a word break.
!
      !
      2 MOVEL = -MOVER
      !
      MOVER = LMWD-MOVEL
      NP1 = LEFTSH (IN(INDEX+1),MOVEL)
      NP2 = RGHTSH (IN(INDEX+2),MOVER)
      IOUT(I) = AND (OR (NP1,NP2) , MASK)
      !
      GO TO 5                                                                   
      !
!
!          The "byte" is already right adjusted.
!
      !
      3 IOUT(I) = AND (IN (INDEX+1) , MASK)
      !
      GO TO 5                                                                   
      !
!
!          Right adjust the "byte".
!
      !
      4 IOUT(I) = AND (RGHTSH (IN (INDEX+1),MOVER) , MASK)
      !
      5 II = II+IBITS
      !
      INDEX = INDEX+IWORDS
      IF (II .LT. LMWD) GO TO 6
      II = II-LMWD
      INDEX = INDEX+1
   !
   6 CONTINUE                                                                  
   !
   RETURN                                                                    
!
   END                                                                       
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE SBYTES (IOUT,IN,ISKIP,NBYTE,NSKIP,N)
!
!          Store bytes - pack bits:  Put arbitrary size values into a
!          packed bit string, taking the low order bits from each value
!          in the unpacked array.
!
   INTEGER               ::  IN(*), IOUT(*)
!
!            IOUT  = packed array output
!            IN    = unpacked array input
!            ISKIP = initial number of bits to skip
!            NBYTE = number of bits to pack
!            NSKIP = additional number of bits to skip on each iteration
!            N     = number of iterations
!************************************** MACHINE SPECIFIC CHANGES START HERE
!          Machine dependent information required:
!            LMWD   = Number of bits in a word on this machine
!            MASKS  = Set of word masks where the first element has only the
!                     right most bit set to 1, the second has the two, ...
!            LEFTSH = Shift left bits in word M to the by N bits
!            RGHTSH = Shift right
!            OR     = Logical OR (add) on this machine
!            AND    = Logical AND (multiply) on this machine
!            NOT    = Logical NOT (negation) on this machine
!          This is for Sun UNIX Fortran
!
   INTEGER, PARAMETER    ::  LMWD = 32
   INTEGER               ::  RGHTSH, OR, AND
   INTEGER, SAVE         ::  MASKS(LMWD)
!
   DATA  MASKS /'1'X,'3'X,'7'X,'F'X, '1F'X,'3F'X,'7F'X,'FF'X,                  &
               '1FF'X,'3FF'X,'7FF'X,'FFF'X, '1FFF'X,'3FFF'X,'7FFF'X,'FFFF'X,   &
               '1FFFF'X,       '3FFFF'X,       '7FFFF'X,       'FFFFF'X,       &
               '1FFFFF'X,      '3FFFFF'X,      '7FFFFF'X,      'FFFFFF'X,      &
               '1FFFFFF'X,     '3FFFFFF'X,     '7FFFFFF'X,     'FFFFFFF'X,     &
               '1FFFFFFF'X,    '3FFFFFFF'X,    '7FFFFFFF'X,    'FFFFFFFF'X/
   LEFTSH(M,N) = ISHFT(M,N)
   RGHTSH(M,N) = ISHFT(M,-N)
!
!     OR(M,N)  = M.OR.N
!     AND(M,N) = M.AND.N
!     OR(M,N)  = IOR(M,N)
!     AND(M,N) = IAND(M,N)
!     NOT(M)   = .NOT.M
!***********************************************************************        
!
!          NBYTE must be less than or equal to LMWD
!
   ICON = LMWD-NBYTE
   IF (ICON .LT. 0) RETURN
   MASK = MASKS(NBYTE)
!
!          INDEX  = number of words into IOUT the next "byte" is to be stored
!          II     = number of bits in from the left side of the word to store it
!          ISTEP  = number of bits from the start of one "byte" to the next
!          IWORDS = number of words to skip from one "byte" to the next
!          IBITS  = number of bits to skip after skipping IWORDS
!          MOVER  = number of bits to the right, a byte must be moved to be
!                   right adjusted
!
   INDEX = ISKIP/LMWD
   II    = MOD(ISKIP,LMWD)
   ISTEP = NBYTE+NSKIP
   IWORDS = ISTEP/LMWD
   IBITS = MOD(ISTEP,LMWD)
!
   DO 6 I = 1,N                                                                
      J = AND (MASK,IN(I))
      MOVEL = ICON-II
      IF (MOVEL) 2,3,4
!
!          The "byte" is to be split across a word break
!
      !
      2 MSK = MASKS (NBYTE+MOVEL)
      !
      IOUT(INDEX+1) = OR (AND(NOT(MSK),IOUT(INDEX+1)),RGHTSH(J,-MOVEL))
      ITEMP = AND (MASKS(LMWD+MOVEL),IOUT(INDEX+2))
      IOUT(INDEX+2) = OR(ITEMP,LEFTSH(J,LMWD+MOVEL))
      !
      GO TO 5                                                                   
      !
!
!          The "byte" is to be stored right-adjusted
!
      !
      3 IOUT(INDEX+1) = OR ( AND (NOT(MASK),IOUT(INDEX+1)) , J)
      !
      GO TO 5                                                                   
      !
!
!          The "byte" is to be stored in middle of word, so shift left.
!
      !
      4 MSK = LEFTSH(MASK,MOVEL)
      !
      IOUT(INDEX+1) = OR(AND(NOT(MSK),IOUT(INDEX+1)),LEFTSH(J,MOVEL))
      !
      5 II = II+IBITS
      !
      INDEX = INDEX+IWORDS
      IF (II .LT. LMWD) GO TO 6
      II = II-LMWD
      INDEX = INDEX+1
   !
   6 CONTINUE
   !
   RETURN                                                                    
!
   END                                                                       
