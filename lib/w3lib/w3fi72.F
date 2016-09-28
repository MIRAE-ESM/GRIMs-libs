#include <define.h>
#ifndef LINUX
      subroutine w3fi72(itype,fld,ifld,ibitl,
     &                  ipflag,id,pds,
     &                  igflag,igrid,igds,icomp,
     &                  ibflag,ibmap,iblen,ibdsfl,
     &                  npts,kbuf,itot,jerr)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:  w3fi72        make a complete grib message
c   prgmmr: farley           org: nmc421      date:94-11-22
c
c abstract: makes a complete grib message from a user supplied
c   array of floating point or integer data.  the user has the
c   option of supplying the pds or an integer array that will be
c   used to create a pds (with w3fi68).  the user must also
c   supply other necessary info; see usage section below.
c
c program history log:
c   91-05-08  r.e.jones
c   92-07-01  m. farley    added gds and bms logic.  placed existing
c                          logic for bds in a routine.
c   92-10-02  r.e.jones    add error exit for w3fi73
c   93-04-30  r.e.jones    replace do loops to move character data
c                          with xmovex, use xstore to zero character
c                          array. make change so flat field will pack.
c   93-08-06  cavanaugh    modified call to w3fi75
c   93-10-26  cavanaugh    added code to restore input field to original
c                          values if d-scale not 0
c   94-01-27  cavanaugh    added igds array in call to w3fi75 to provide
c                          information for boustrophedonic processing
c   94-03-03  cavanaugh    increased size of gds array for thin grids
c   94-05-16  farley       cleaned up documentation
c   94-11-10  farley       increased size of pfld/ifld arrarys from
c                          100k to 260k for .5 degree sst anal fields
c   94-12-04  r.e.jones    change document for ipflag.
c   95-10-31  iredell      removed saves and prints
c   98-05-19  gilbert      increased array dimensions to handle grids
c                          of up to 500,000 grid points.
c   95-10-31  iredell      generalized word size
c   98-12-21  gilbert      replaced function ichar with mova2i.
c   99-02-01  gilbert      changed the method of zeroing out array kbuf.
c                          the old method, using w3fi01 and xstore was
c                          incorrect with 4-byte integers and 8-byte reals.
c
c usage:  call w3fi72(itype,fld,ifld,ibitl,
c        &            ipflag,id,pds,
c        &            igflag,igrid,igds,icomp,
c        &            ibflag,ibmap,iblen,ibdsfl,
c        &            ibdsfl,
c        &            npts,kbuf,itot,jerr)
c
c   input argument list:
c     itype    - 0 = floating point data supplied in array 'fld'
c                1 = integer data supplied in array 'ifld'
c     fld      - real array of data (at proper gridpoints) to be
c                converted to grib format if itype=0.
c                see remarks #1 & 2.
c     ifld     - integer array of data (at proper gridpoints) to be
c                converted to grib format if itype=1.
c                see remarks #1 & 2.
c     ibitl    - 0 = computer computes length for packing data from
c                    power of 2 (number of bits) best fit of data
c                    using 'variable' bit packer w3fi58.
c                8, 12, etc. computer rescales data to fit into that
c                    'fixed' number of bits using w3fi59.
c                see remarks #3.
c
c     ipflag   - 0 = make pds from user supplied array (id)
c                1 = user supplying pds
c                note: if pds is greater than 30, use iplfag=1.
c                the user could call w3fi68 before he calls
c                w3fi72. this would make the first 30 bytes of
c                the pds, user then would make bytes after 30.
c     id       - integer array of  values that w3fi68 will use
c                to make an edition 1 pds if ipflag=0.  (see the
c                docblock for w3fi68 for layout of array)
c     pds      - character array of values (valid pds supplied
c                by user) if ipflag=1. length may exceed 28 bytes
c                (contents of bytes beyond 28 are passed
c                through unchanged).
c
c     igflag   - 0 = make gds based on 'igrid' value.
c                1 = make gds from user supplied info in 'igds'
c                    and 'igrid' value.
c                see remarks #4.
c     igrid    - #   = grid identification (table b)
c                255 = if user defined grid; igds must be supplied
c                      and igflag must =1.
c     igds     - integer array containing user gds info (same
c                format as supplied by w3fi71 - see dockblock for
c                layout) if igflag=1.
c     icomp    - resolution and component flag for bit 5 of gds(17)
c                0 = earth oriented winds
c                1 = grid oriented winds
c
c     ibflag   - 0 = make bit map from user supplied data
c                # = bit map predefined by center
c                see remarks #5.
c     ibmap    - integer array containing bit map
c     iblen    - length of bit map will be used to verify length
c                of field (error if it doesn't match).
c
c     ibdsfl   - integer array containing table 11 flag info
c                bds octet 4:
c                (1) 0 = grid point data
c                    1 = spherical harmonic coefficients
c                (2) 0 = simple packing
c                    1 = second order packing
c                (3) ... same value as 'itype'
c                    0 = original data were floating point values
c                    1 = original data were integer values
c                (4) 0 = no additional flags at octet 14
c                    1 = octet 14 contains flag bits 5-12
c                (5) 0 = reserved - always set to 0
c         byte 6 option 1 not available (as of 5-16-93)
c                (6) 0 = single datum at each grid point
c                    1 = matrix of values at each grid point
c         byte 7 option 0 with second order packing n/a (as of 5-16-93)
c                (7) 0 = no secondary bit maps
c                    1 = secondary bit maps present
c                (8) 0 = second order values have constant width
c                    1 = second order values have different widths
c
c   output argument list:
c     npts     - number of gridpoints in array fld or ifld
c     kbuf     - entire grib message ('grib' to '7777')
c                equivalence to integer array to make sure it
c                is on word bounary.
c     itot     - total length of grib message in bytes
c     jerr     - = 0, completed making grib field without error
c                  1, ipflag not 0 or 1
c                  2, igflag not 0 or 1
c                  3, error converting ieee f.p. number to ibm370 f.p.
c                  4, w3fi71 error/igrid not defined
c                  5, w3fk74 error/grid representation type not valid
c                  6, grid too large for packer dimension arrays
c                     see automation division for revision!
c                  7, length of bit map not equal to size of fld/ifld
c                  8, w3fi73 error, all values in ibmap are zero
c
c   output files:
c     ft06f001 - standard fortran output print file
c
c   subprograms called:
c     library:
c       w3lib    - w3fi58, w3fi59, w3fi68, w3fi71, w3fi73, w3fi74
c                  w3fi75, w3fi76
c       fortran 90 intrinsic - bit_size
c
c remarks:
c   1)  if bit map to be included in message, null data should
c       be included in fld or ifld.  this routine will take care
c       of 'discarding' any null data based on the bit map.
c   2)  units must be those in grib documentation:  nmc o.n. 388
c       or wmo publication 306.
c   3)  in either case, input numbers will be multiplied by
c       '10 to the nth' power found in id(25) or pds(27-28),
c       the d-scaling factor, prior to binary packing.
c   4)  all nmc produced grib fields will have a grid definition
c       section included in the grib message.  id(6) will be
c       set to '1'.
c       - gds will be built based on grid number (igrid), unless
c         igflag=1 (user supplying igds).  user must still supply
c         igrid even if igds provided.
c   5)  if bit map used then id(7) or pds(8) must indicate the
c       presence of a bit map.
c   6)  array kbuf should be equivalenced to an integer value or
c       array to make sure it is on a word boundary.
c   7)  subprogram can be called from a multiprocessing environment.
c
c attributes:
c   language: fortran 90
c
c$$$
c
      real            fld(*)
c
      integer         ibdsfl(*)
      integer         ibmap(*)
      integer         id(*)
      integer         ifld(*)
      integer         igds(*)
      integer,allocatable:: ipfld(:)
      integer         ib(4)
c
      character * 1   bds11(11)
      character * 1   kbuf(*)
      character * 1   pds(*)
      character * 1   gds(200)
      character(1),allocatable:: bms(:)
      character(1),allocatable:: pfld(:)
      character * 1   seven
      character * 1   zero
c
c
c   ascii rep of  /'G', 'R', 'I', 'B'/
c
      data  ib    / 71,  82,  73,  66/
c
      ier    = 0
      iberr  = 0
      jerr   = 0
      igribl = 8
      ipdsl  = 0
      lengds = 0
      lenbms = 0
      lenbds = 0
      itoss  = 0
#ifndef DEC
#define MOVA2I mova2i
#else
#define MOVA2I ichar
#endif
c
c$           1.0   product definition section(pds).
c
c   set id(6) to 1 ...or... modify pds(8) ...
c      regardless of user specification...
c   nmc grib fields will always have a gds
c
      if (ipflag .eq.0) then
        id(6) = 1
        call w3fi68(id,pds)
      else if (ipflag .eq. 1) then
        if (iand(MOVA2I(pds(8)),64) .eq. 64) then
c         both gds and bms
          pds(8) = char(192)
        else if (MOVA2I(pds(8)) .eq. 0) then
c         gds only
          pds(8) = char(128)
        end if
        continue
      else
c       print *,' w3fi72 error, ipflag is not 0 or 1 ipflag = ',ipflag
        jerr = 1
        go to 900
      end if
c
c     get length of pds
c
      ipdsl = MOVA2I(pds(1)) * 65536 + MOVA2I(pds(2)) * 256 +
     &        MOVA2I(pds(3))
c
c$           2.0   grid definition section (gds).
c
c     if igflag=1 then user is supplying the igds information
c
      if (igflag .eq. 0) then
        call w3fi71(igrid,igds,igerr)
        if (igerr .eq. 1) then
c         print *,' w3fi71 error, grid type not defined...',igrid
          jerr = 4
          go to 900
        end if
      end if
      if (igflag .eq. 0  .or.  igflag .eq.1) then
        call w3fi74(igds,icomp,gds,lengds,npts,igerr)
        if (igerr .eq. 1) then
c         print *,' w3fi74 error, grid rep type not valid...',igds(3)
          jerr = 5
          go to 900
        else
        end if
      else
c       print *,' w3fi72 error, igflag is not 0 or 1 igflag = ',igflag
        jerr = 2
        go to 900
      end if
c
c$           3.0   bit map section (bms).
c
c     set itoss=1 if bitmap being used.  w3fi75 will toss data
c     prior to packing.  later coding will be needed when the
c     'predefined' grids are finally 'defined'.
c
      if (MOVA2I(pds(8)) .eq. 64 .or.
     &    MOVA2I(pds(8)) .eq. 192)   then
        itoss = 1
        if (ibflag .eq. 0) then
          if (iblen .ne. npts) then
c           print *,' w3fi72 error, iblen .ne. npts = ',iblen,npts
            jerr = 7
            go to 900
          end if
          allocate(bms(npts/8+6))
          call w3fi73(ibflag,ibmap,iblen,bms,lenbms,ier)
          if (ier .ne. 0) then
c           print *,' w3fi73 error, ibmap values are all zero'
            jerr = 8
            go to 900
          end if
        else
c         print *,'   bit map predefined by center, ibflag = ',ibflag
        end if
      end if
c
c$           4.0   binary data section (bds).
c
c$           4.1   scale the data with d-scale from pds(27-28)
c
      jscale = MOVA2I(pds(27)) * 256 + MOVA2I(pds(28))
      if (iand(jscale,32768).ne.0) then
        jscale = - iand(jscale,32767)
      end if
      scale  = 10.0 ** jscale
      if (itype .eq. 0) then
        do 410 i = 1,npts
          fld(i) = fld(i) * scale
  410   continue
      else
        do 411 i = 1,npts
          ifld(i) = nint(float(ifld(i)) * scale)
  411   continue
      end if
c
c$           4.2   call w3fi75 to pack data and make bds.
c
      allocate(pfld(npts*4))
c
      if(ibdsfl(2).ne.0) then
        allocate(ipfld(npts*32/bit_size(1)+1))
        ipfld=0
      endif
c
      call w3fi75(ibitl,itype,itoss,fld,ifld,ibmap,ibdsfl,
     &         npts,bds11,ipfld,pfld,len,lenbds,iberr,pds,igds)
c
      if(ibdsfl(2).ne.0) then
        call xmovex(pfld,ipfld,npts*4)
        deallocate(ipfld)
      endif
c
        if (iberr .eq. 1) then
          jerr = 3
          go to 900
        end if
c            4.3   if d-scale not 0, rescale input field to
c                   original value
c
      if (jscale.ne.0) then
          dscale = 1.0 / scale
          if (itype.eq.0) then
              do 412 i = 1, npts
                  fld(i)  = fld(i) * dscale
  412         continue
          else
              do 413 i = 1, npts
                  fld(i)  = nint(float(ifld(i)) * dscale)
  413         continue
          end if
      end if
c
c$           5.0   output section.
c
c$           5.1   zero out the output array kbuf.
c
      zero    = char(00)
      itot    = igribl + ipdsl + lengds + lenbms + lenbds + 4
c     print *,'igribl  =',igribl
c     print *,'ipdsl   =',ipdsl
c     print *,'lengds  =',lengds
c     print *,'lenbms  =',lenbms
c     print *,'lenbds  =',lenbds
c     print *,'itot    =',itot
      kbuf(1:itot)=zero
c
c$           5.2   move section 0 - 'is' into kbuf (8 bytes).
c
      istart  = 0
      do 520 i = 1,4
        kbuf(i) = char(ib(i))
  520 continue
c
      kbuf(5) = char(mod(itot / 65536,256))
      kbuf(6) = char(mod(itot /   256,256))
      kbuf(7) = char(mod(itot        ,256))
      kbuf(8) = char(1)
c
c$           5.3   move section 1 - 'pds' into kbuf (28 bytes).
c
      istart  = istart + igribl
      if (ipdsl.gt.0) then
        call xmovex(kbuf(istart+1),pds,ipdsl)
      else
c       print *,'length of pds less or equal 0, ipdsl = ',ipdsl
      end if
c
c$           5.4   move section 2 - 'gds' into kbuf.
c
      istart  = istart + ipdsl
      if (lengds .gt. 0) then
        call xmovex(kbuf(istart+1),gds,lengds)
      end if
c
c$           5.5   move section 3 - 'bms' into kbuf.
c
      istart  = istart + lengds
      if (lenbms .gt. 0) then
        call xmovex(kbuf(istart+1),bms,lenbms)
      end if
c
c$           5.6   move section 4 - 'bds' into kbuf.
c
c$                 move the first 11 octets of the bds into kbuf.
c
      istart  = istart + lenbms
      call xmovex(kbuf(istart+1),bds11,11)
c
c$                 move the packed data into the kbuf
c
      istart  = istart + 11
      if (len.gt.0) then
        call xmovex(kbuf(istart+1),pfld,len)
      end if
c
c$                 add '7777' to end off kbuf
c   note that these 4 octets not included in actual size of bds.
c
      seven  = char(55)
      istart = itot - 4
      do 562 i = 1,4
        kbuf(istart+i) = seven
 562  continue
c
 900  continue
      if(allocated(bms)) deallocate(bms)
      if(allocated(pfld)) deallocate(pfld)
      return
      end
#else
      SUBROUTINE W3FI72(ITYPE,FLD,IFLD,IBITL,IPFLAG,ID,PDS,
     &                  IGFLAG,IGRID,IGDS,ICOMP,
     &                  IBFLAG,IBMAP,IBLEN,
     &                  IBDSFL,
     &                  NPTS,KBUF,ITOT,JERR)
C  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  W3FI72        MAKE A COMPLETE GRIB MESSAGE
C   PRGMMR: CAVANAUGH        ORG: NMC421      DATE:93-09-22
C
C ABSTRACT: MAKES A COMPLETE GRIB MESSAGE FROM A USER SUPPLIED
C   ARRAY OF FLOATING POINT OR INTEGER DATA.  THE USER HAS THE
C   OPTION OF SUPPLYING THE PDS OR AN INTEGER ARRAY THAT WILL BE
C   USED TO CREATE A PDS (WITH W3FI68).  THE USER MUST ALSO
C   SUPPLY OTHER NECESSARY INFO; SEE USAGE SECTION BELOW.
C
C PROGRAM HISTORY LOG:
C   91-05-08  R.E.JONES
C   92-07-01  M. FARLEY    ADDED GDS AND BMS LOGIC.  PLACED EXISTING
C                          LOGIC FOR BDS IN A ROUTINE.
C   92-10-02  R.E.JONES    ADD ERROR EXIT FOR W3FI73
C   93-04-30  R.E.JONES    REPLACE DO LOOPS TO MOVE CHARACTER DATA
C                          WITH XMOVEX, USE XSTORE TO ZERO CHARACTER
C                          ARRAY. MAKE CHANGE SO FLAT FIELD WILL PACK.
C   93-08-06  CAVANAUGH    MODIFIED CALL TO W3FI75
C   93-10-01  R.E.JONES    INCREASE SIZE OF GDS ARAAY FOR GRIB TYPES
C                          37-44
C   93-10-25  R.E.JONES    IF D SCALING NOT EQUAL 0, FLD IS CHANGED
C                          BEFORE CALL TO W3FI75, CHANGE BACK TO
C                          ORIGINAL VALUES AFTER W3FI75.
C
C USAGE:  CALL W3FI72(ITYPE,FLD,IFLD,IBITL,
C        &            IPFLAG,ID,PDS,
C        &            IGFLAG,IGRID,IGDS,ICOMP,
C        &            IBFLAG,IBMAP,IBLEN,IBDSFL,
C        &            NPTS,KBUF,ITOT,JERR)
C
C   INPUT ARGUMENT LIST:
C     ITYPE    - 0 = FLOATING POINT DATA SUPPLIED IN ARRAY 'FLD'
C                1 = INTEGER DATA SUPPLIED IN ARRAY 'IFLD'
C     FLD      - REAL ARRAY OF DATA (AT PROPER GRIDPOINTS) TO BE
C                CONVERTED TO GRIB FORMAT IF ITYPE=0.
C                SEE REMARKS #1 & 2.
C     IFLD     - INTEGER ARRAY OF DATA (AT PROPER GRIDPOINTS) TO BE
C                CONVERTED TO GRIB FORMAT IF ITYPE=1.
C                SEE REMARKS #1 & 2.
C     IBITL    - 0 = COMPUTER COMPUTES LENGTH FOR PACKING DATA FROM
C                    POWER OF 2 (NUMBER OF BITS) BEST FIT OF DATA
C                    USING 'VARIABLE' BIT PACKER W3FI58.
C                8, 12, ETC. COMPUTER RESCALES DATA TO FIT INTO THAT
C                    'FIXED' NUMBER OF BITS USING W3FI59.
C                SEE REMARKS #3.
C
C     IPFLAG   - 0 = MAKE PDS FROM USER SUPPLIED ARRAY (ID)
C                1 = USER SUPPLYING PDS
C     ID       - INTEGER ARRAY OF  VALUES THAT W3FI68 WILL USE
C                TO MAKE AN EDITION 1 PDS IF IPFLAG=0.  (SEE THE
C                DOCBLOCK FOR W3FI68 FOR LAYOUT OF ARRAY)
C     PDS      - CHARACTER ARRAY OF  VALUES (VALID PDS SUPPLIED
C                BY USER) IF IPFLAG=1.
C
C     IGFLAG   - 0 = MAKE GDS BASED ON 'IGRID' VALUE.
C                1 = MAKE GDS FROM USER SUPPLIED INFO IN 'IGDS'
C                    AND 'IGRID' VALUE.
C                SEE REMARKS #4.
C     IGRID    - #   = GRID IDENTIFICATION (TABLE B)
C                255 = IF USER DEFINED GRID; IGDS MUST BE SUPPLIED
C                      AND IGFLAG MUST =1.
C     IGDS     - INTEGER ARRAY CONTAINING USER GDS INFO (SAME
C                FORMAT AS SUPPLIED BY W3FI71 - SEE DOCKBLOCK FOR
C                LAYOUT) IF IGFLAG=1.
C     ICOMP    - RESOLUTION AND COMPONENT FLAG FOR BIT 5 OF GDS(17)
C                0 = EARTH ORIENTED WINDS
C                1 = GRID ORIENTED WINDS
C
C     IBFLAG   - 0 = MAKE BIT MAP FROM USER SUPPLIED DATA
C                # = BIT MAP PREDEFINED BY CENTER
C                SEE REMARKS #5.
C     IBMAP    - INTEGER ARRAY CONTAINING BIT MAP
C     IBLEN    - LENGTH OF BIT MAP WILL BE USED TO VERIFY LENGTH
C                OF FIELD (ERROR IF IT DOESN'T MATCH).
C
C     IBDSFL   - INTEGER ARRAY CONTAINING TABLE 11 FLAG INFO
C                BDS OCTET 4:
C                (1) 0 = GRID POINT DATA
C                    1 = SPHERICAL HARMONIC COEFFICIENTS
C                (2) 0 = SIMPLE PACKING
C                    1 = SECOND ORDER PACKING
C                (3) ... SAME VALUE AS 'ITYPE'
C                    0 = ORIGINAL DATA WERE FLOATING POINT VALUES
C                    1 = ORIGINAL DATA WERE INTEGER VALUES
C                (4) 0 = NO ADDITIONAL FLAGS AT OCTET 14
C                    1 = OCTET 14 CONTAINS FLAG BITS 5-12
C       *** ***  BYTES 5-9 NOT IMPLENTED YET... SET TO '0' FOR NOW
C                (5) 0 = RESERVED - ALWAYS SET TO 0
C                (6) 0 = SINGLE DATUM AT EACH GRID POINT
C                    1 = MATRIX OF VALUES AT EACH GRID POINT
C                (7) 0 = NO SECONDARY BIT MAPS
C                    1 = SECONDARY BIT MAPS PRESENT
C                (8) 0 = SECOND ORDER VALUES HAVE CONSTANT WIDTH
C                    1 = SECOND ORDER VALUES HAVE DIFFERENT WIDTHS
C                (9) 0 = LIST ENCODED DATA
C                    1 = RUN LENGTH ENCODED
C
C   OUTPUT ARGUMENT LIST:
C     NPTS     - NUMBER OF GRIDPOINTS IN ARRAY FLD OR IFLD
C     KBUF     - ENTIRE GRIB MESSAGE ('GRIB' TO '7777')
C                EQUIVALENCE TO INTEGER ARRAY TO MAKE SURE IT
C                IS ON WORD BOUNARY.
C     ITOT     - TOTAL LENGTH OF GRIB MESSAGE IN BYTES
C     JERR     - = 0, COMPLETED MAKING GRIB FIELD WITHOUT ERROR
C                  1, IPFLAG NOT 0 OR 1
C                  2, IGFLAG NOT 0 OR 1
C                  3, ERROR CONVERTING IEEE F.P. NUMBER TO IBM370 F.P.
C                  4, W3FI71 ERROR/IGRID NOT DEFINED
C                  5, W3FK74 ERROR/GRID REPRESENTATION TYPE NOT VALID
C                  6, GRID TOO LARGE FOR PACKER DIMENSION ARRAYS
C                     SEE AUTOMATION DIVISION FOR REVISION!
C                  7, LENGTH OF BIT MAP NOT EQUAL TO SIZE OF FLD/IFLD
C                  8, W3FI73 ERROR, ALL VALUES IN IBMAP ARE ZERO
C
C   OUTPUT FILES:
C     FT06F001 - STANDARD FORTRAN OUTPUT PRINT FILE
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       W3LIB    - W3FI58, W3FI59, W3FI68, W3FI71, W3FI73, W3FI74
C                  W3FI75, W3FI76, W3FI01
C
C REMARKS:
C   1)  IF BIT MAP TO BE INCLUDED IN MESSAGE, NULL DATA SHOULD
C       BE INCLUDED IN FLD OR IFLD.  THIS ROUTINE WILL TAKE CARE
C       OF 'DISCARDING' ANY NULL DATA BASED ON THE BIT MAP.
C   2)  UNITS MUST BE THOSE IN GRIB DOCUMENTATION:  NMC O.N. 388
C       OR WMO PUBLICATION 306.
C   3)  IN EITHER CASE, INPUT NUMBERS WILL BE MULTIPLIED BY
C       '10 TO THE NTH' POWER FOUND IN ID(25) OR PDS(27-28),
C       THE D-SCALING FACTOR, PRIOR TO BINARY PACKING.
C   4)  ALL NMC PRODUCED GRIB FIELDS WILL HAVE A GRID DEFINITION
C       SECTION INCLUDED IN THE GRIB MESSAGE.  ID(6) WILL BE
C       SET TO '1'.
C       - GDS WILL BE BUILT BASED ON GRID NUMBER (IGRID), UNLESS
C         IGFLAG=1 (USER SUPPLYING IGDS).  USER MUST STILL SUPPLY
C         IGRID EVEN IF IGDS PROVIDED.
C   5)  IF BIT MAP USED THEN ID(7) OR PDS(8) MUST INDICATE THE
C       PRESENCE OF A BIT MAP.
C   6)  ARRAY KBUF SHOULD BE EQUIVALENCED TO AN INTEGER VALUE OR
C       ARRRAY TO MAKE SURE IT IS ON A WORD BOUNDARY.
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/864, CRAY Y-MP EL2/256
C
C
C
      PARAMETER       (MXSIZE=400000)
C     ALLOW UP TO 24 BITS PER POINT
      PARAMETER       (MXSIZ3=MXSIZE*3)
      PARAMETER       (MXSIZB=MXSIZE/8+6)
C     FOR 64 BIT CRAY
C     PARAMETER       (MXSIZI=MXSIZ3/8)
C     FOR 32 BIT WORKSTATIONS AND HDS
      PARAMETER       (MXSIZI=MXSIZ3/4)
C
      REAL*8          FLD(*)
C
      INTEGER         IBDSFL(*)
      INTEGER         IBMAP(*)
      INTEGER         ID(*)
      INTEGER         IFLD(*)
      INTEGER         IGDS(*)
      INTEGER         IPFLD(MXSIZI)
      INTEGER         IB(4)
C
      CHARACTER * 1   BDS11(11)
      CHARACTER * 1   KBUF(*)
      CHARACTER * 1   PDS(*)
      CHARACTER * 1   GDS(500)
      CHARACTER * 1   BMS(MXSIZB)
      CHARACTER * 1   PFLD(MXSIZ3)
      CHARACTER * 1   SEVEN
      CHARACTER * 1   ZERO
C
      SAVE
C
      EQUIVALENCE     (IPFLD(1),PFLD(1))
      EQUIVALENCE     (BDS11(1),IDUMMY)
C
C   ASCII REP OF  /'G', 'R', 'I', 'B'/
C
      DATA  IB    / 71,  82,  73,  66/
C
      IER    = 0
      IBERR  = 0
      JERR   = 0
      IGRIBL = 8
      IPDSL  = 0
      LENGDS = 0
      LENBMS = 0
      LENBDS = 0
      ITOSS  = 0
C
C           1.0   PRODUCT DEFINITION SECTION(PDS).
C
C   SET ID(6) TO 1 ...OR... MODIFY PDS(8) ...
C      REGARDLESS OF USER SPECIFICATION...
C   NMC GRIB FIELDS WILL ALWAYS HAVE A GDS
C
C-DEBUG
      IF (IPFLAG .EQ.0) THEN
        ID(6) = 1
C       PRINT *,' W3FI72 - CHECK POINT IPFLAG = 0'
        CALL W3FI68(ID,PDS)
      ELSE IF (IPFLAG .EQ. 1) THEN
        IF (IAND(ICHAR(PDS(8)),64) .EQ. 64) THEN
C         BOTH GDS AND BMS
          PDS(8) = CHAR(192)
        ELSE IF (ICHAR(PDS(8)) .EQ. 0) THEN
C         GDS ONLY
          PDS(8) = CHAR(128)
        END IF
        CONTINUE
      ELSE
        PRINT *,' W3FI72 ERROR, IPFLAG IS NOT 0 OR 1 IPFLAG = ',IPFLAG
        JERR = 1
        GO TO 900
      END IF
C
C     GET LENGTH OF PDS
C
      IPDSL = ICHAR(PDS(1)) * 65536 + ICHAR(PDS(2)) * 256 +
     &        ICHAR(PDS(3))
C
C           2.0   GRID DEFINITION SECTION (GDS).
C
C     IF IGFLAG=1 THEN USER IS SUPPLYING THE IGDS INFORMATION
C
      IF (IGFLAG .EQ. 0) THEN
        CALL W3FI71(IGRID,IGDS,IGERR)
        IF (IGERR .EQ. 1) THEN
          PRINT *,' W3FI71 ERROR, GRID TYPE NOT DEFINED...',IGRID
          JERR = 4
          GO TO 900
        END IF
      END IF
      IF (IGFLAG .EQ. 0  .OR.  IGFLAG .EQ.1) THEN
        CALL W3FI74(IGDS,ICOMP,GDS,LENGDS,NPTS,IGERR)
        IF (IGERR .EQ. 1) THEN
          PRINT *,' W3FI74 ERROR, GRID REP TYPE NOT VALID...',IGDS(3)
          JERR = 5
          GO TO 900
        ELSE
        END IF
        IF (NPTS .GT. MXSIZE) THEN
          PRINT *,' W3FI72 ERROR, GRID TOO LARGE FOR PACKER ARRAY',
     &            ' DIMENSIONS'
          JERR = 6
          GO TO 900
        END IF
      ELSE
        PRINT *,' W3FI72 ERROR, IGFLAG IS NOT 0 OR 1 IGFLAG = ',IGFLAG
        JERR = 2
        GO TO 900
      END IF
C
C           3.0   BIT MAP SECTION (BMS).
C
C     SET ITOSS=1 IF BITMAP BEING USED.  W3FI75 WILL TOSS DATA
C     PRIOR TO PACKING.  LATER CODING WILL BE NEEDED WHEN THE
C     'PREDEFINED' GRIDS ARE FINALLY 'DEFINED'.
C
C     IF (ID(7) .EQ.1 .OR. ICHAR(PDS(8)) .EQ. 64 .OR.
      IF (ICHAR(PDS(8)) .EQ. 64 .OR.
     &    ICHAR(PDS(8)) .EQ. 192)   THEN
        ITOSS = 1
        IF (IBFLAG .EQ. 0) THEN
          IF (IBLEN .NE. NPTS) THEN
            PRINT *,' W3FI72 ERROR, IBLEN .NE. NPTS = ',IBLEN,NPTS
            JERR = 7
            GO TO 900
          END IF
          CALL W3FI73(IBFLAG,IBMAP,IBLEN,BMS,LENBMS,IER)
          IF (IER .NE. 0) THEN
            PRINT *,' W3FI73 ERROR, IBMAP VALUES ARE ALL ZERO'
            JERR = 8
            GO TO 900
          END IF
        ELSE
          PRINT *,'   BIT MAP PREDEFINED BY CENTER, IBFLAG = ',IBFLAG
        END IF
      END IF
C
C           4.0   BINARY DATA SECTION (BDS).
C
C           4.1   SCALE THE DATA WITH D-SCALE FROM PDS(27-28)
C
      JSCALE = ICHAR(PDS(27)) * 256 + ICHAR(PDS(28))
      IF (IAND(JSCALE,32768).NE.0) THEN
        JSCALE = - IAND(JSCALE,32767)
      END IF
      SCALE  = 10.0 ** JSCALE
      IF (ITYPE .EQ. 0) THEN
        DO 410 I = 1,NPTS
          FLD(I) = FLD(I) * SCALE
  410   CONTINUE
      ELSE
        DO 411 I = 1,NPTS
          IFLD(I) = NINT(FLOAT(IFLD(I)) * SCALE)
  411   CONTINUE
      END IF
C
C           4.2   CALL W3FI75 TO PACK DATA AND MAKE BDS.
C
      CALL W3FI75(IBITL,ITYPE,ITOSS,FLD,IFLD,IBMAP,IBDSFL,
     &         NPTS,BDS11,IPFLD,PFLD,LEN,LENBDS,IBERR,PDS)
        IF (IBERR .EQ. 1) THEN
          JERR = 3
          GO TO 900
        END IF
C
C   4.3 IF D-SCALE NOT 0, RESCALE INPUT FIELD TO ORIGINAL VALUE
C
      IF (JSCALE .NE. 0) THEN
        DSCALE = 1.0 / SCALE
        IF (ITYPE .EQ. 0) THEN
          DO 412 I = 1,NPTS
            FLD(I) = FLD(I) * DSCALE
  412     CONTINUE
        ELSE
          DO 413 I = 1,NPTS
            IFLD(I) = NINT(FLOAT(IFLD(I)) * DSCALE)
  413     CONTINUE
        END IF
      END IF
C
C           5.0   OUTPUT SECTION.
C
C           5.1   ZERO OUT THE OUTPUT ARRAY KBUF.
C
      ZERO    = CHAR(00)
      ITOT    = IGRIBL + IPDSL + LENGDS + LENBMS + LENBDS + 4
C
C     PRINT *,'IGRIBL  =',IGRIBL
C     PRINT *,'IPDSL   =',IPDSL
C     PRINT *,'LENGDS  =',LENGDS
C     PRINT *,'LENBMS  =',LENBMS
C     PRINT *,'LENBDS  =',LENBDS
C     PRINT *,'ITOT    =',ITOT
C
C     KBUF MUST BE ON A WORD BOUNDRY, EQUIVALENCE TO AN
C     INTEGER ARRAY IN THE MAIN PROGRAM TO MAKE SURE IT IS.
C     THIS IS BOTH COMPUTER AND COMPILER DEPENDENT, W3FI01
C     IS USED TO FILD OUT IF THE COMPUTER IS A 64 BIT OR
C     32 BIT WORD SIZE COMPUTER. LW IS SET TO 4 FOR 32 BIT
C     COMPUTER, 8 FOR 64 BIT COMPUTER.
C
      CALL W3FI01(LW)
      IWORDS = ITOT / LW
CHMHJ CALL XSTORE(KBUF,0,IWORDS)
      CALL XSTORE(KBUF,ZERO,ITOT)
CHMHJ      IF (MOD(ITOT,LW).NE.0) THEN
CHMHJ        IBYTES = ITOT - IWORDS * LW
CHMHJ        DO 510 I = 1,IBYTES
CHMHJ          KBUF(IWORDS * LW + I) = ZERO
  510   CONTINUE
CHMHJ      END IF
C
C           5.2   MOVE SECTION 0 - 'IS' INTO KBUF (8 BYTES).
C
      ISTART  = 0
      DO 520 I = 1,4
        KBUF(I) = CHAR(IB(I))
  520 CONTINUE
C
      KBUF(5) = CHAR(MOD(ITOT / 65536,256))
      KBUF(6) = CHAR(MOD(ITOT /   256,256))
      KBUF(7) = CHAR(MOD(ITOT        ,256))
      KBUF(8) = CHAR(1)
C
C           5.3   MOVE SECTION 1 - 'PDS' INTO KBUF (28 BYTES).
C
      ISTART  = ISTART + IGRIBL
      IF (IPDSL.GT.0) THEN
        CALL XMOVEX(KBUF(ISTART+1),PDS,IPDSL)
      ELSE
        PRINT *,'LENGTH OF PDS LESS OR EQUAL 0, IPDSL = ',IPDSL
      END IF
C
C           5.4   MOVE SECTION 2 - 'GDS' INTO KBUF.
C
      ISTART  = ISTART + IPDSL
      IF (LENGDS .GT. 0) THEN
        CALL XMOVEX(KBUF(ISTART+1),GDS,LENGDS)
      END IF
C
C           5.5   MOVE SECTION 3 - 'BMS' INTO KBUF.
C
      ISTART  = ISTART + LENGDS
      IF (LENBMS .GT. 0) THEN
        CALL XMOVEX(KBUF(ISTART+1),BMS,LENBMS)
      END IF
C
C           5.6   MOVE SECTION 4 - 'BDS' INTO KBUF.
C
C                 MOVE THE FIRST 11 OCTETS OF THE BDS INTO KBUF.
C
      ISTART  = ISTART + LENBMS
      CALL XMOVEX(KBUF(ISTART+1),BDS11,11)
C
C                 MOVE THE PACKED DATA INTO THE KBUF
C
      ISTART  = ISTART + 11
      IF (LEN.GT.0) THEN
        CALL XMOVEX(KBUF(ISTART+1),PFLD,LEN)
      END IF
C
C                 ADD '7777' TO END OFF KBUF
C   NOTE THAT THESE 4 OCTETS NOT INCLUDED IN ACTUAL SIZE OF BDS.
C
      SEVEN  = CHAR(55)
      ISTART = ITOT - 4
      DO 562 I = 1,4
        KBUF(ISTART+I) = SEVEN
 562  CONTINUE
C
 900  CONTINUE
      RETURN
      END
#endif
