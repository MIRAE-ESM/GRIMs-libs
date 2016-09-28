         PROGRAM MAIN
C
C  GRIB unpacker and grads control file maker.
C
C  MTOTL ... Max number of fields available
C  LKPDS ... Number of PDS words to be recorded.
C  LKGDS ... Number of GDS words to be recorded.
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
C
      COMMON/COMKEY/ KEYPDS(LKPDS,MTOTL),KEYGDS(LKGDS,MTOTL)
C
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
C  MFGRP  ... maximum number of independent grib file groups.
C             group name is given by grads % format.  Group
C             name can be a grib file name.
C  MFILE  ... maximum number of files for each grib file group.
C
      PARAMETER (MFGRP=100)
      PARAMETER (MFILE=1000)
C
C  NAMELIST PARAMETERS:
C
C  IDIML  ... Output unpacked field resolution in x-dir. Default=input res.
C  JDIML  ... Output unpacked field resolution in y-dir.
C
C  IFKEY  ... ='I' for sorting time series of the initial states first.
C             ='F' for sorting time series of forecast first
C
C  LUNPK  ... =.TRUE.  Unpacks and sort selected records in proper order
C  LVDATE ... =.TRUE.  ctl file date is set to verifying date when MIN(FHOR)
C                      in the file is non zero
C
C  FNGRIB ... GRIB file name(s). 
C             Dimension is for number of forecast-times or initial-times.
C
C  FNAMPRE ... Namelist data file name
C
C  FNPRINT ... File name where printouts from premap program goes
C
C  DIRNAME ... TMP directroy name where grmap results are stored
C
C  LSELCT  ... If .TRUE. allow selection of fields
C
C  DXPHI   ... Output lat/lon grid resolution.
C  XLATS,XLATN,XLONW,XLONE ... Output area selection
C
C  IFTYPE  ... Type of input file.  0=GRIB file, 1=ZNL file, 2=Ken file
C              3=BGES
C
C  INCTIME ... Time increment when file name is given in grads % format
C  INCUNIT ... Time unit when file name is given in grads % format
C
C  UNDEF   ... Undefined value used in GRADS
C
C  LHISKEY ... .TRUE. for reading interactive input from a file.
C
C  LEVMDL ... Model levels (needed and used for ZNL and KEN files)
C
C  LDEBUG  ... TRUE for debugging printout. Output goes to FNPRINT
C
C  NAMELIST VARIABLE TYPES
C 
      CHARACTER*1 IFKEY
C
C  Grib file group name
C
      CHARACTER*128 FNGRBG(MFGRP)
C
C  Grib file name for each FNGRBG
C
      CHARACTER*128 FNGRIB(MFILE)
C
C  Input file time increment unit
C
      CHARACTER*2  INCUNIT(MFGRP)
C
C  Output unpacked grib file name
C
      CHARACTER*128 FNGUPO
C
C  input data card file
C
      CHARACTER*128 FNAMPRE
C
C  Print file
C
      CHARACTER*128 FNPRINT
C
C  grib kpds(5) and kpds(6) table file
C
      CHARACTER*128 FNKPDS5,FNKPDS6
C
C  Interactive variable selection recording file
C
      CHARACTER*128 FNSELCT
C
C  Directory name where output are created
C
      CHARACTER*128 DIRNAME
C
      CHARACTER*128 FNSHRT
C
      LOGICAL LUNPK,LVDATE,LHISKEY
C
C  NAMELIST DEFAULTS
C
      DATA IFKEY/'I'/
      DATA LUNPK/.FALSE./
      DATA LVDATE/.FALSE./
      DATA LHISKEY/.FALSE./
C
      DATA FNGRIB /MFILE*'        '/
      DATA FNAMPRE/'./grmap.parm'/
      DATA FNPRINT/'./print.out'/
      DATA FNSELCT/'./vselect.dat'/
      DATA FNKPDS5/'/home/sgi90/wd23ln/etc/grib1.kpds5.vsn21'/
      DATA FNKPDS6/'/home/sgi90/wd23ln/etc/grib1.kpds6.vsn21'/
C
      DATA INCUNIT/MFGRP*' '/
C
C  Unpack output lat/lon grid resolution
C
      DATA DXPHI/0.0/
C
C  Model level required for ken and znl files
C
      DATA LEVMDL/28/
C
      DATA UNDEF/-9.99E33/
C
      DIMENSION IFTYPE(MFGRP)
      DATA IFTYPE/MFGRP*0/
C
      DIMENSION INCTIME(MFGRP)
      DATA INCTIME/MFGRP*0/
C
      LOGICAL LSELCT
      DATA LSELCT/.FALSE./
      DATA XLATS,XLATN,XLONW,XLONE/4*-999./
C
      LOGICAL LDEBUG
      DATA LDEBUG/.TRUE./
C
      LOGICAL LCRAY,LY2000
      DATA LY2000/.TRUE./
C
      CHARACTER*1 DELIM
      DATA DELIM/'%'/
C
      NAMELIST/NAMPRE/ IFTYPE,IFKEY,LUNPK,UNDEF,LCRAY,
     1                 FNAMPRE,FNPRINT,FNKPDS5,FNKPDS6,
     1                 LVDATE,DIRNAME,LHISKEY,INCTIME,INCUNIT,LSELCT,
     2                 DXPHI,XLATS,XLATN,XLONW,XLONE,
     3                 LEVMDL,IBGES,JBGES,LDEBUG,DELIM,LY2000
C
      CHARACTER*80 RUNAME
      DATA RUNAME,LENRUN/'EXP',3/
C
C  LMRUNP Flag for merging unpacked files
C
      LOGICAL LMRUNP
C
      CHARACTER*1 YN
      DATA YN/' '/
C
C   Flag for space interpolation
C
      LOGICAL LINTRP
C
C   Flag for % formatted grib file name
C
      LOGICAL LTMPL
C
C  dset line file name in the grads control file
C
      CHARACTER*128 FNDSET
C
C  Grads control file name
C
      CHARACTER*128 FNCNTL
C
C  Parent directory when grib file directory contains % format
C
      CHARACTER*128 FNHEAD
C
C  Fort Unit numbers used inside the program
C
C  LUPTR  ... Unit unmber for diagnostic/debug print output
C  LUPRE  ... Unit unmber for grib read namelist input
C  LUPGB  ... Unit unmber for input (grib) file
C  LUUPO  ... Unit unmber for unpacked output file
C  LUCTL  ... Unit unmber for GRADS ctl file output file
C
      DATA LUPRE/ 1/
      DATA LUPGB/21/
      DATA LUUPO/61/
      DATA LUCTL/85/
      DATA LUPTR/82/
      DATA LUSEL/87/
C
      INTEGER SELN(MTOTL)
C
      CHARACTER*64 LABEL4
      CHARACTER*8 ITINC,IFTINC,TINC
C
      CHARACTER*80 ASGNSTR
C
C  Count and record of duplicate named unpacked files
C
      CHARACTER*128 FNGUPOS
      COMMON/COMINDY/NFGUPO,FNGUPOS(MTOTL)
C
C  Count and record of duplicate named control files
C
      CHARACTER*128 FNCNTLS
      COMMON/COMCNTL/NFCNTL,FNCNTLS(MFGRP)
C
      CHARACTER*24 DATES
C
      LOGICAL LDBG
      COMMON/COMDBG/ LDBG
C
C  Set duplicate file name counts to zero
C
      NFGUPO=0
      NFCNTL=0
C
C  ?..
C
      IGLOB=1
      JGLOB=1
C
C  Read in NAMELIST data
C
      LCRAY=.TRUE.
      IF(LCRAY) THEN
        CALL ASSIGN('assign -R')
      ENDIF
C
C  Open main program (debug) print file
C
      OPEN(LUPTR,FILE=FNPRINT,ERR=711)
      GO TO 712
  711 CONTINUE
      WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNPRINT
      PRINT *,' ERROR IN OPENING FILE ',FNPRINT
      CALL ABORT
  712 CONTINUE
      WRITE(LUPTR,*) ' FILE ',FNPRINT(1:128),' opened.  Unit=',LUPTR
C
C  Open main program namelist file
C
      OPEN(LUPRE,FILE=FNAMPRE,STATUS='OLD',ERR=721)
      GO TO 722
  721 CONTINUE
      WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNAMPRE
      PRINT *,'ERROR IN OPENING FILE ',FNAMPRE
      CALL ABORT
  722 CONTINUE
      WRITE(LUPTR,*) ' FILE ',FNAMPRE(1:128),' opened.  Unit=',LUPRE
C
C  Read main program namelist
C
      READ (LUPRE,*) NFGRP
      IF(NFGRP.GT.MFGRP) THEN
  	    WRITE(LUPTR,*) ' ERROR!!! NFGRP too large',
     1                 ' NFGRP=',NFGRP,' but should be smaller than',
     2                 ' MFGRP=',MFGRP
  	    PRINT *,' ERROR!!! NFGRP too large',
     1                 ' NFGRP=',NFGRP,' but should be smaller than',
     2                 ' MFGRP=',MFGRP
		    CALL ABORT
      ENDIF
      WRITE(LUPTR,*) ' NFGRP=',NFGRP
      IF(NFGRP.GT.0) THEN
        DO N=1,NFGRP
          DO I=1,128
            FNGRBG(N)(I:I)=' '
          ENDDO
          READ(LUPRE,'(A128)') FNGRBG(N)
          WRITE(LUPTR,'(1X,A128)') FNGRBG(N)
        ENDDO
      ENDIF
C
      DO I=1,128
        DIRNAME(I:I)=' '
      ENDDO
C
      READ (LUPRE,NAMPRE) 
      LCRAY=.TRUE.
      WRITE(LUPTR,NAMPRE) 
      WRITE(LUPTR,*) ' ' 
C
      LDBG=LDEBUG
C
C  Output unpack field dimension
C
      IF(DXPHI.GT.0) THEN
        IDIML=NINT(360./DXPHI)
        JDIML=IDIML/2+1
      ELSE
        IDIML=0
        JDIML=0
      ENDIF
C
      IF(.NOT.LUNPK) THEN
        LMRUNP=.FALSE.
      ELSE
        LMRUNP=.TRUE.
      ENDIF
C
C  Open field selection data file
C
      IF(LSELCT) THEN
        OPEN(LUSEL,FILE=FNSELCT,ERR=741)
        GO TO 742
  741   CONTINUE
        WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNSELCT
        PRINT *,'ERROR IN OPENING FILE ',FNSELCT
        CALL ABORT
  742   CONTINUE
        WRITE(LUPTR,*) ' FILE ',FNSELCT(1:128),' opened.  Unit=',LUSEL
      ENDIF
C
      DO NGRP=1,NFGRP
C
        READ (LUPRE,*,END=908) NFILES
        GO TO 907
  908   CONTINUE
        WRITE(LUPTR,*) ' EOF on data'
        CALL ABORT
  907   CONTINUE
        WRITE(LUPTR,*) ' NFILES=',NFILES,' for NGRP=',NGRP
        IF(NFILES.EQ.0) THEN
          WRITE(LUPTR,*) ' Processing of the file ',
     1                   FNGRBG(NGRP)(1:128),' SKIPPED.'
          WRITE(LUPTR,*) ' No such files exist.'
          PRINT *,' Processing of the file ',FNGRBG(NGRP)(1:128),
     1            ' SKIPPED.'
          PRINT *,' No such files exist.'
          GO TO 166
        ENDIF
        IF(NFILES.GT.MFILE) THEN
          WRITE(LUPTR,*) ' ERROR!!!  Number of files too many.'
          PRINT *, ' ERROR!!!  Number of files too many.'
          CALL ABORT
        ENDIF
        DO N=1,NFILES
          READ (LUPRE,'(A128)') FNGRIB(N)
          WRITE(LUPTR,'(8H FNGRIB=,A128)') FNGRIB(N)
        ENDDO
C
C  Clear arrays
C
        DO J=1,MTOTL
          DO I=1,LKPDS
            KEYPDS(I,J)=0
          ENDDO
        ENDDO
        DO J=1,MTOTL
          DO I=1,LKGDS
            KEYGDS(I,J)=0
          ENDDO
        ENDDO
        DO I=1,MTOTL
          LCHOSEN(I)=.TRUE.
        ENDDO
C
        IXDIM=0
        JXDIM=0
C
        IMAXL=0
        JMAXL=0
        IJMAXL=0
C
        DO III=1,MTOTL
          SELN(III)=0
        ENDDO
C
C  Loop through given types of GRIB INDEX file
C
        CALL SETMPL(FNGRBG(NGRP),NFILES,LTMPL,NDIR,FNHEAD,DELIM,LUPTR)
C
			  IF(LTMPL) THEN
          NFILESX=MIN(NFILES,2)
        ELSE
          NFILESX=NFILES
        ENDIF
C
        MFHMIN=9999
C
        ifpp=0
        DO NFILE=1,NFILES
C
          IF(NFILE.LE.NFILESX) THEN
C
            PRINT *,' Processing ', FNGRIB(NFILE)(1:128)
C
C  Go through GRIB file and find all necessary info.
C
            KENTINC=0
            NFLX=MIN(NFILE,2)
            ITINC ='        '
            IFTINC='        '
            IF(IFTYPE(NGRP).EQ.0) THEN
              CALL GETGRB(LUNPK,FNGRIB(NFILE),NDIR,IMAXL,JMAXL,IJMAXL,
     1                    IFKEY,NTOTL,SELN,LHISKEY,FNKPDS5,FNKPDS6,
     2                    LSELCT,NFILE,LY2000,LUPGI,LUPGB,LUSEL,
     3                    LUPTR)
            ELSEIF(IFTYPE(NGRP).EQ.1) THEN
              LUNPK=.TRUE.
              CALL GETZNL(FNGRIB(NFILE),IMAXL,JMAXL,IJMAXL,LEVMDL,
     1                    NFILE,IFKEY,NTOTL,SELN,LY2000,LUPGB,LUPTR)
            ELSEIF(IFTYPE(NGRP).EQ.2) THEN
              LUNPK=.TRUE.
              CALL GETKEN(FNGRIB(NFILE),IMAXL,JMAXL,IJMAXL,LEVMDL,
     1                    KENTINC,KENPTS,KENLEN,
     2                    NFILE,IFKEY,NTOTL,SELN,LY2000,LUPGB,LUPTR)
            ELSEIF(IFTYPE(NGRP).EQ.3) THEN
              LUNPK=.TRUE.
              CALL GETBGS(FNGRIB(NFILE),IBGES,JBGES,IMAXL,JMAXL,IJMAXL,
     1                    NFILE,IFKEY,NTOTL,SELN,LY2000,LUPGB,LUPTR)
            ELSEIF(IFTYPE(NGRP).EQ.4) THEN
              LUNPK=.TRUE.
              CALL GETBGSN(FNGRIB(NFILE),IBGES,JBGES,IMAXL,JMAXL,IJMAXL,
     1                    NFILE,IFKEY,NTOTL,SELN,LY2000,LUPGB,LUPTR)
            ELSEIF(IFTYPE(NGRP).EQ.5) THEN
              LUNPK=.TRUE.
              CALL GETBGSM(FNGRIB(NFILE),IBGES,JBGES,IMAXL,JMAXL,IJMAXL,
     1                    NFILE,IFKEY,NTOTL,SELN,LY2000,LUPGB,LUPTR)
            ELSE
              WRITE(LUPTR,*) ' Error!!  Illegal value of IFTYPE'
              WRITE(LUPTR,*) ' NGRP=',NGRP,' IFTYPE=',IFTYPE(NGRP)
              PRINT *,'Error!!  Illegal value of IFTYPE'
              PRINT *,'NGRP=',NGRP,' IFTYPE=',IFTYPE(NGRP)
              CALL ABORT
            ENDIF
C
C  Define time increment
C
            IF(KENTINC.EQ.0) THEN
              IF(NFILE.EQ.NFILESX) THEN
                CALL INCTFT(NFILE,NTOTL,SELN,ITINC,IFTINC,
     1                      MFHINC,LUPTR)
              ENDIF
            ELSE
C
C     Ken file has different time increment
C
              MFHINC=NINT(12.*60./KENTINC)
              IF(MFHINC.LT.10) THEN
                WRITE(IFTINC,'(I1,2Hmn)') MFHINC
              ELSEIF(MFHINC.LT.100) THEN
                WRITE(IFTINC,'(I2,2Hmn)') MFHINC
              ELSE
                WRITE(IFTINC,'(I3,2Hmn)') MFHINC
              ENDIF
            ENDIF
            WRITE(LUPTR,*) 'KENTINC=',KENTINC,' MFHINC=',MFHINC
C
C  Find minimum fhour
C
            DO NN=1,NTOTL
              N=SELN(NN)
              IF(LCHOSEN(N)) THEN
                MFHR=NINT(FJHOUR(N,MIN(NFILE,2)))
                IF(MFHR.LT.MFHMIN) MFHMIN=MFHR
              ENDIF
            ENDDO
            WRITE(LUPTR,*) ' MFHMIN=',MFHMIN
          ENDIF
C
C  Create file that contains list of variables
C
        IF(NFILE.EQ.1) THEN
  		  	DO 2 NN=1,NTOTL
            N=SELN(NN)
            IF(N.GT.1) THEN
              WRITE(LUPTR,*) N,')',LABEL(1,N)(1:LENLA(1,N)),
     1                       ' ',LABEL(2,N)(1:LENLA(2,N)),' ',
     2                       LABEL(3,N)(1:LENLA(3,N)),' ',
     3                       LABEL(4,N)(1:LENLA(4,N)),' ',
     4                       LABEL(5,N)(1:LENLA(5,N))
              IF(LAREA) THEN
                WRITE(LUPTR,*) '  (',RSLAT(N),',',WLON(N),')',
     1                           '(',RNLAT(N),',',ELON(N),')'
              ENDIF
            ENDIF
    2     CONTINUE
          ENDIF
C
          WRITE(LUPTR,*) ' Before Loop LCHOSEN=',(LCHOSEN(I),I=1,NTOTL)
C
          DLON1=ABS(DLON(1))
          DLAT1=ABS(DLAT(1))
          DO NN=2,NTOTL
            IF(LUNPK) THEN
              N=SELN(NN)
              IF(N.LE.0.OR..NOT.LCHOSEN(N)) GO TO 223
            ELSE
              N=NN
            ENDIF
            IF(ABS(DLON(N)).NE.DLON1.and.ifpp.eq.0) THEN
              ifpp=1
              WRITE(LUPTR,*) ' Warning !!! Mixed resolution.'
              WRITE(LUPTR,*) ' N=',N,' NN=',NN
              WRITE(LUPTR,*) ' DLON(1)=',DLON(1),' DLON(N)=',DLON(N)
              PRINT *,' Warning!!! Mixed resolution fields.'
              PRINT *,' DLON(1)=',DLON(1),' DLON(N)=',DLON(N),' N=',N
            ENDIF
            IF(ABS(DLAT(N)).NE.DLAT1.and.ifpp.eq.0) THEN
              ifpp=1
              WRITE(LUPTR,*) ' Warning !!! Mixed resolution.'
              WRITE(LUPTR,*) ' N=',N,' NN=',NN
              WRITE(LUPTR,*) ' DLAT(1)=',DLAT(1),' DLAT(N)=',DLAT(N)
              PRINT *,' Warning!!! Mixed resolution fields.'
              PRINT *,' DLAT(1)=',DLAT(1),' DLAT(N)=',DLAT(N),' N=',N
            ENDIF
  223       CONTINUE
          ENDDO
C
C  Unpack grib file and write in the grads order
C
          IF(LUNPK) THEN
            IF(IFTYPE(NGRP).EQ.0) THEN
              FNSHRT=FNGRIB(NFILE)
              IF(.NOT.LMRUNP.OR.NFILE.EQ.1) THEN
                CALL NAMUNPO(FNSHRT,NDIR,FNGUPO,LUPTR)
              ENDIF
              IF(LAREA.AND.LUNPK) THEN
                IF(DLAT(1).NE.-99.) THEN
                  IGLOB=NINT(360./DLON(1))
                  JGLOB=NINT(180./DLON(1)+1.)
                ELSE
                  IGLOB=IMAXL
                  JGLOB=JMAXL
                ENDIF
              ENDIF
C
C  Find dimension for limited area
C
              IF(XLATS.NE.-999.) THEN
                IF(IDIML.GT.0.AND.JDIML.GT.0) THEN
                  IDIMQ=IDIML
                  JDIMQ=JDIML
                ELSE
                  IDIMQ=IDIM(1)
                  JDIMQ=JDIM(1)
                ENDIF
                DLONX=NINT(360./IDIMQ*1000.)/1000.
                DLATX=NINT(180./(JDIMQ-1)*1000.)/1000.
                JXS=NINT((90.-XLATN)/DLATX+1)
                JXF=NINT((90.-XLATS)/DLATX+1)
                XLOW=XLONW
                IF(XLOW.LT.0) XLOW=XLOW+360.
                IXS=NINT(XLOW/DLONX+1)
                IF(IXS.GT.IDIMQ) IXS=IXS-IDIMQ
                XLOE=XLONE
                IF(XLOE.LT.0) XLOE=XLOE+360.
                IXF=NINT(XLOE/DLONX+1)
                IF(IXF.GT.IDIMQ) IXF=IXF-IDIMQ
                IF(IXF.LE.IXS) IXF=IXF+IDIMQ
                IXDIM=IXF-IXS+1
                JXDIM=JXF-JXS+1
                IF(IXDIM.GT.IDIMQ) IXDIM=IDIMQ
              ENDIF
              LINTRP=.FALSE.
              IF(IDIML.GT.0.AND.JDIML.GT.0) THEN
                LINTRP=.TRUE.
                IF(XLATS.EQ.-999.) THEN
                  IYDIM=IDIML
                  JYDIM=JDIML
                  YLONW=WLON(1)
                  YLATN=RNLAT(1)
                ELSE
                  IYDIM=IXDIM
                  JYDIM=JXDIM
                  YLONW=XLONW
                  YLATN=XLATN
                ENDIF
                DXLON=360./FLOAT(IDIML)
                DXLAT=DXLON
              ELSE
                IF(XLATS.NE.-999.) THEN
                  IYDIM=IXDIM
                  JYDIM=JXDIM
                  YLONW=XLONW
                  YLATN=XLATN
        				  DXLON=ABS(DLON(1))
                  DXLAT=ABS(DLAT(1))
                  LINTRP=.TRUE.
                ENDIF
              ENDIF
 
              WRITE(LUPTR,*) 'LINTRP=',LINTRP
              IF(LINTRP) THEN
                WRITE(LUPTR,*) 'IDIML=',IDIML,' JDIML=',JDIML
                WRITE(LUPTR,*) 'IYDIM=',IYDIM,' JYDIM=',JYDIM
                WRITE(LUPTR,*) 'DXLON=',DXLON,' DXLAT=',DXLAT
              ENDIF
C
              CALL UNPGRB(FNGRIB(NFILE),FNGUPO,NFILES,
     1                    NFILE,SELN,NTOTL,IDIML,JDIML,MTOTL,LMRUNP,
     2                    IMAXL,JMAXL,IJMAXL,IGLOB,JGLOB,UNDEF,LCRAY,
     4                    LINTRP,IYDIM,JYDIM,YLONW,YLATN,DXLON,DXLAT,
     3                    LREC,NREC,LUPGI,LUPGB,LUUPO,LUPTR)
            ELSEIF(IFTYPE(NGRP).EQ.1) THEN
              IF(NFILE.EQ.1) THEN
                FNSHRT=FNGRIB(NFILE)
                IF(.NOT.LMRUNP.OR.NFILE.EQ.1) THEN
                  CALL NAMUNPO(FNSHRT,NDIR,FNGUPO,LUPTR)
                ENDIF
              ENDIF
              CALL UNPZNL(FNGRIB(NFILE),FNGUPO,NFILE,LEVMDL,LCRAY,
     1                    LREC,NREC,LUPGB,LUUPO,LUPTR)
            ELSEIF(IFTYPE(NGRP).EQ.2) THEN
              IF(NFILE.EQ.1) THEN
                FNSHRT=FNGRIB(NFILE)
                IF(.NOT.LMRUNP.OR.NFILE.EQ.1) THEN
                  CALL NAMUNPO(FNSHRT,NDIR,FNGUPO,LUPTR)
                ENDIF
              ENDIF
              CALL UNPKEN(FNGRIB(NFILE),FNGUPO,KENPTS,KENLEN,LEVMDL,
     2                    NFILE,UNDEF,LCRAY,LREC,NREC,LUPGB,LUUPO,LUPTR)
            ELSEIF(IFTYPE(NGRP).EQ.3) THEN
              IF(NFILE.EQ.1) THEN
                FNSHRT=FNGRIB(NFILE)
                IF(.NOT.LMRUNP.OR.NFILE.EQ.1) THEN
                  CALL NAMUNPO(FNSHRT,NDIR,FNGUPO,LUPTR)
                ENDIF
              ENDIF
              CALL UNPBGS(FNGRIB(NFILE),FNGUPO,NFILE,LCRAY,IJMAXL,
     1                    IDIML,JDIML,LREC,NREC,LUPGB,LUUPO,LUPTR)
            ELSEIF(IFTYPE(NGRP).EQ.4) THEN
              IF(NFILE.EQ.1) THEN
                FNSHRT=FNGRIB(NFILE)
                IF(.NOT.LMRUNP.OR.NFILE.EQ.1) THEN
                  CALL NAMUNPO(FNSHRT,NDIR,FNGUPO,LUPTR)
                ENDIF
              ENDIF
              CALL UNPBGSN(FNGRIB(NFILE),FNGUPO,NFILE,LCRAY,IJMAXL,
     1                    IDIML,JDIML,LREC,NREC,LUPGB,LUUPO,LUPTR)
            ELSEIF(IFTYPE(NGRP).EQ.5) THEN
              IF(NFILE.EQ.1) THEN
                FNSHRT=FNGRIB(NFILE)
                IF(.NOT.LMRUNP.OR.NFILE.EQ.1) THEN
                  CALL NAMUNPO(FNSHRT,NDIR,FNGUPO,LUPTR)
                ENDIF
              ENDIF
              CALL UNPBGSM(FNGRIB(NFILE),FNGUPO,NFILE,LCRAY,IJMAXL,
     1                    IDIML,JDIML,LREC,NREC,LUPGB,LUUPO,LUPTR)
            ENDIF
          ENDIF
C
          WRITE(LUPTR,*) ' IMAXL=',IMAXL,' JMAXL=',JMAXL
          WRITE(LUPTR,*) ' IGLOB=',IGLOB,' JGLOB=',JGLOB
          WRITE(LUPTR,*) ' NTOTL=',NTOTL
          WRITE(LUPTR,*) ' LREC=',LREC,' NREC=',NREC
C
C  Make ctl file
C
C  Check file contents in terms of time series
C
          IF(LTMPL.AND.NITOTL.GT.1.AND.IFTINC(1:3).NE.'   ') THEN
            WRITE(LUPTR,*) ' Warning!!!'
            WRITE(LUPTR,*) ' Files contain initial time fields.'
            WRITE(LUPTR,*) ' GRADS format may need to be disabled.'
            WRITE(LUPTR,*) ' Multiple control file will be created',
     1                     ' for each file.'
            WRITE(LUPTR,*) ' IFTINC,NITOTL=',IFTINC,' ',NITOTL
            IF(YN.EQ.' ') THEN
              PRINT *,' '
              PRINT *,' Warning!!!'
              PRINT *,' File contains initial time series fields.'
              PRINT *,' Multiple control file should be created',
     1                ' for each file.  Confirm (y/n) ==>'
              READ (*,'(A1)') YN
              WRITE(LUSEL,'(A1)') YN
            ENDIF
            NFILESX=NFILES
            IF(YN.EQ.'y') THEN
              LTMPL=.FALSE.
              LMRUNP=.FALSE.
            ENDIF
          ENDIF
          IF(LTMPL.AND.NFXTTL.GT.1.AND.ITINC (1:3).NE.'   ') THEN
            WRITE(LUPTR,*) ' Warning!!!'
            WRITE(LUPTR,*) ' File contains forecast time series fields.'
            WRITE(LUPTR,*) ' GRADS wild card format disabled.'
            WRITE(LUPTR,*) ' Multiple control file will be created',
     1                     ' for each file.'
            WRITE(LUPTR,*) ' ITINC,NFXTTL=',ITINC,' ',NFXTTL
            IF(YN.EQ.' ') THEN
              PRINT *,' '
              PRINT *,' Warning!!!'
              PRINT *,' File contains forecast time series fields.'
              PRINT *,' Multiple control file should be created',
     1                ' for each file.  Confirm (y/n) ==>'
              READ (*,'(A1)') YN
              WRITE(LUSEL,'(A1)') YN
            ENDIF
            NFILESX=NFILES
            IF(YN.EQ.'y') THEN
              LTMPL=.FALSE.
              LMRUNP=.FALSE.
              NFILESX=NFILES
            ENDIF
          ENDIF
          IF(NFILE.LE.NFILESX) THEN
            IF(IGLOB.EQ.1.AND.JGLOB.EQ.1) THEN
              IMAXX=IMAXL
              JMAXX=JMAXL
              WLONX=WLON(1)
              RSLATX=RSLAT(1)
            ELSEIF(LGLOBL(1)) THEN
              IMAXX=IGLOB
              JMAXX=JGLOB
              WLONX=0.
              RSLATX=-90.
            ELSE
              IMAXX=IMAXL
              JMAXX=JMAXL
              WLONX=WLON(1)
              RSLATX=RSLAT(1)
            ENDIF
            IF(LTMPL.AND.IFTYPE(NGRP).EQ.0) THEN
              WRITE(LUPTR,*) 'GETINC call suppressed'
C             WRITE(LUPTR,*) 'GETINC called'
C             CALL GETINC(ITINC,NFILE,LUPTR)
              IF(NFILE.EQ.1) THEN
                DO I=1,64
                  LABEL4(I:I)=' '
                ENDDO
                LENLA4=LENLA(4,1)
                LABEL4(1:LENLA4)=LABEL(4,1)(1:LENLA(4,1))
              ENDIF
            ELSE
              LENLA4=LENLA(4,1)
              LABEL4(1:LENLA4)=LABEL(4,1)(1:LENLA(4,1))
            ENDIF
          ENDIF
C
C  Find Dset name
C
          IF(.NOT.LTMPL.OR.
     1       .NOT.LMRUNP.AND.(LTMPL.AND.NFILE.EQ.NFILESX).OR.
     2       LMRUNP.AND.NFILE.EQ.1) THEN
C
            CALL SETDSET(LTMPL,LUNPK,LMRUNP,
     1                   NFILE,IFTYPE,NGRP,NDIR,
     2                   DIRNAME,FNGRBG,FNGRIB,
     3                   FNCNTL,FNHEAD,FNGUPO,FNDSET,DELIM,
     4                   LUPTR)
          ENDIF
C
C  Create ctl file
C
          IF(.NOT.LTMPL.OR.(LTMPL.AND.NFILE.EQ.NFILESX)) THEN
C
            DO I=1,24
              DATES(I:I)=' '
            ENDDO
            CALL GETDELT(IFKEY,LITSR,LFTSR,ITINC,IFTINC,
     1                   LABEL4,LENLA4,MFHMIN,LVDATE,
     2                   DATES,TINC,INCTIME(NGRP),INCUNIT(NGRP),LUPTR)
C
            IF(LTMPL.AND..NOT.LUNPK) THEN
              NTDEF=NFILES
            ELSEIF(.NOT.LUNPK.OR.LUNPK.AND..NOT.LTMPL) THEN
              NTDEF=NITOTL
              IF(NTDEF.LT.NFTOTL) NTDEF=NFTOTL
            ELSEIF(LUNPK.AND.LTMPL) THEN
              NTDEF=999
            ENDIF
C
            CALL MKCTL(DIRNAME,FNDSET,FNCNTL,RUNAME,NTOTL,LUNPK,LINTRP,
     1                LVDATE,IFTYPE(NGRP),IDIML,JDIML,IMAXX,JMAXX,NTDEF,
     2                 WLONX,RSLATX,DLON(1),DLAT(1),
     3                 IXDIM,JXDIM,XLATS,XLONW,
     4                 TINC,DATES,UNDEF,LTMPL,LUCTL,LUPTR)
          ENDIF
        ENDDO
C
  166   CONTINUE
      ENDDO
C
      STOP
      END
      SUBROUTINE SETMPL(FNGRBG,NFILES,LTMPL,NDIR,FNHEAD,DELIM,LUPTR)
C
      CHARACTER*128 FNGRBG
      CHARACTER*128 FNHEAD
      CHARACTER*1 DELIM
      LOGICAL LTMPL
C
      CHARACTER*128 FNTEMP
C
C  Find directory name that contains grads wild card %
C
      NDIR=0
      LTMPL=.FALSE.
  	  J=0
			NN=128
 			DO I=1,NN
       IF(.NOT.LTMPL.AND.FNGRBG(I:I).EQ.'/') THEN
          NDIR=NDIR+1
          DO II=I+1,126
            IF(FNGRBG(II:II).EQ.'/') THEN
              J=0
              GO TO 122
            ENDIF
            IF(FNGRBG(II:II).EQ.DELIM) THEN
              IF(FNGRBG(II+1:II+1).EQ.'f'.OR.
     1           FNGRBG(II+1:II+1).EQ.'y'.OR.
     2           FNGRBG(II+1:II+1).EQ.'m'.OR.
     3           FNGRBG(II+1:II+1).EQ.'d'.OR.
     4           FNGRBG(II+1:II+1).EQ.'h') THEN
                 LTMPL=.TRUE.
                 GO TO 123
              ENDIF
            ENDIF
          ENDDO
  123     CONTINUE
          J=0
        ELSEIF(FNGRBG(I:I).EQ.' ') THEN
          GO TO 122
        ELSE
          J=J+1
          FNTEMP(J:J)=FNGRBG(I:I)
        ENDIF
  122   CONTINUE
      ENDDO
      DO I=J+1,128
        FNTEMP(I:I)=' '
      ENDDO
C
      DO I=1,128
        FNHEAD(I:I)=' '
      ENDDO
      DO I=128,1,-1
        IF(FNTEMP(I:I).EQ.'/') THEN
          FNHEAD=FNTEMP(1:I)
          GO TO 111
        ENDIF
      ENDDO
  111 CONTINUE
C
      WRITE(LUPTR,*) ' FNHEAD=',FNHEAD(1:128)
      WRITE(LUPTR,*) ' NDIR=',NDIR,' LTMPL=',LTMPL
C
      RETURN
      END
      SUBROUTINE GETGRB(LUNPK,FNGRIB,NDIR,IMAXL,JMAXL,IJMAXL,
     1                  IFKEY,NTOTL,SELN,LHISKEY,FNKPDS5,FNKPDS6,
     2                  LSELCT,NFILE,LY2000,LUPGI,LUPGB,LUSEL,LUPTR)
C
      LOGICAL LUNPK,LSELCT,LHISKEY,LY2000
      CHARACTER*128 FNGRIB
      CHARACTER*128 FNKPDS5,FNKPDS6
      CHARACTER*1 IFKEY
      CHARACTER*8 ITINC,IFTINC
C
      CHARACTER*128 FXNAME
C
C  IVINDX ... record number that identifies unique variable name
C  ILINDX ... record number that identifies levels for eavh variable
C  ITINDX ... record number that identifies initial time
C  IFINDX ... record number that identifies forecast time
C  IAINDX ... record number that identifies areas
C
C  NLTOTL ... number of multi-level fields
C  NITOTL ... number of time series fields 
C  NFTOTL ... number of forecast time series fields
C  NFXTTL ... number of forecast time series fields
C             (accum and avr counted the same)
C  NATOTL ... number of area separated fields
C  NPTOTL ... number of level types
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
C
      COMMON/COMKEY/ KEYPDS(LKPDS,MTOTL),KEYGDS(LKGDS,MTOTL)
C
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      INTEGER KPDS(25,MTOTL),KGDS(22,MTOTL)
C
      INTEGER SELN(*)
C
      LOGICAL LFP(MTOTL),LPFND,LEXIST
C
      CHARACTER*80 ASGNSTR
      CHARACTER*64 LABTMP
C
      LOGICAL LDEBUG
      COMMON/COMDBG/ LDEBUG
C
      DATA MSK1/32000/,MSK2/4000/
      PARAMETER(MBUF=1024*128*64)
      CHARACTER*1 CBUF(MBUF)
C
      INTEGER*4 LUPGB4,MSK14,MSK24,MNUM4,MBUF4
      INTEGER*4 NLEN4,NNUM4,IRET4
C
      INTEGER*4 KPDS4(25,MTOTL),KGDS4(22,MTOTL)
      INTEGER*4 LSKIP4(MTOTL),LGRIB4(MTOTL)
C
      IF(NFILE.GT.1) GO TO 1
C
      DO N=1,MTOTL
        LABBR(N)='        '
      ENDDO
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' DEBUG print from GETGRB start'
      ENDIF
C
      CALL INIPARM(FNKPDS5,LUPTR)
      CALL INILEVL(FNKPDS6,LUPTR)
C
    1 CONTINUE
C
C  Get grib index buffer
C
      WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUPGB
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUPGB,FILE=FNGRIB,FORM='UNFORMATTED',ERR=978)
      GO TO 977
  978 CONTINUE
      WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNGRIB
      PRINT *,'ERROR IN OPENING FILE ',FNGRIB
      CALL ABORT
  977 CONTINUE
      WRITE(LUPTR,*) ' FILE ',FNGRIB(1:128),' opened. Unit=',LUPGB
C
      MNUM=0
C
      LUPGB4=LUPGB
      MSK14=MSK1
      MSK24=MSK2
      MNUM4=MNUM
      MBUF4=MBUF
      CALL GETGIR(LUPGB4,MSK14,MSK24,MNUM4,MBUF4,CBUF,NLEN4,NNUM4,IRET4)
      NLEN=NLEN4
      NNUM=NNUM4
      IRET=IRET4
C
      IF(IRET.NE.0) THEN
        WRITE(LUPTR,*) 'ERROR.  CBUF length too short in GETGIR'
        PRINT *,'ERROR.  CBUF length too short in GETGIR'
        CALL ABORT
      ENDIF
      IF(NNUM.EQ.0) THEN
        WRITE(LUPTR,*) 'ERROR. Not a grib file. Detected in GETGIR'
        PRINT *,'ERROR.  Not a grib file. Detected in GETGIR'
        CALL ABORT
      ENDIF
      IF(NLEN.EQ.0) THEN
        WRITE(LUPTR,*) 'ERROR. NLEN=0. Detected in GETGIR'
        PRINT *,'ERROR.  NLEN=0.  Detected in GETGIR'
        CALL ABORT
      ENDIF
      IF(NNUM.GT.MTOTL) THEN
        WRITE(LUPTR,*) 'ERROR!!! Number of parameters in the index',
     1          ' buffer exceeded limit of ',MTOTL
        PRINT *,'ERROR!!! Number of parameters in the index',
     1          ' buffer exceeded limit of ',MTOTL
        CALL ABORT
      ENDIF
C
      NLEN4=NLEN
      NNUM4=NNUM
      CALL UNPINDX(CBUF,NLEN4,NNUM4,KPDS4,KGDS4,LSKIP4,LGRIB4,IRET4)
      DO N=1,NNUM
        DO I=1,25
          KPDS(I,N)=KPDS4(I,N)
        ENDDO
        DO I=1,22
          KGDS(I,N)=KGDS4(I,N)
        ENDDO
        LSKIP(N)=LSKIP4(N)
        LGRIB(N)=LGRIB4(N)
      ENDDO
      IRET=IRET4
C
      NFLX=MIN(NFILE,2)
      NSEL=0
      NFLDS=0
C
      NTOTL=NNUM
      WRITE(LUPTR,*) ' NTOTL=',NTOTL
C
      WRITE(LUPTR,*) ' KPDS=',(KPDS(I,1),I=1,25)
      WRITE(LUPTR,*) ' KGDS=',(KGDS(I,1),I=1,22)
C
      IF(NFILE.EQ.1) THEN
        CALL GETPARM(KPDS,NTOTL,LABEL,LENLA,LABBR,LUPTR)
        CALL GETLEVL(KPDS,NTOTL,LABEL,LENLA,IRLEV,LVABBR,LUPTR)
      ENDIF
C
      CALL GETDATE(KPDS,NTOTL,LABEL,LENLA,RJLDAY(1,MIN(NFILE,2)),
     1             LY2000,LUPTR)
      CALL GETFTIM(KPDS,NTOTL,LABEL,LENLA,FJHOUR(1,MIN(NFILE,2)),LUPTR)
C
      IF(NFILE.EQ.1) THEN
        CALL GETAREA(KGDS,NTOTL,DLAT,DLON,LGLOBL,RSLAT,RNLAT,
     2               WLON,ELON,IJORDR,PARMAP,NPARMP,LUPTR)
        CALL GETMXDM(KGDS,NTOTL,IDIM,JDIM,IMAXL,JMAXL,IJMAXL,LUPTR)
      ENDIF
      IF(.NOT.LUNPK.OR.NFILE.EQ.1) THEN
        DO N=1,NNUM
          DO K=1,LKPDS
            KEYPDS(K,N)=KPDS(K,N)
          ENDDO
          DO K=1,LKGDS
            KEYGDS(K,N)=KGDS(K,N)
          ENDDO
        ENDDO
      ENDIF
C
      WRITE(LUPTR,*) ' FJHOUR=',FJHOUR(1,MIN(NFILE,2))
C
      IF(NFILE.GT.1) RETURN
C
C  Get rid of unnecessary spaces from LABEL
C
      DO N=1,NTOTL
        CALL RMBLNK(LABEL(1,N),LENLA(1,N),5)
      ENDDO
C
      IF(LSELCT) THEN
        CALL INTRCT(NTOTL,NCHOSEN,LUSEL,LHISKEY,LUPTR)
      ELSEIF(.NOT.LSELCT) THEN
        NCHOSEN=NTOTL
      ENDIF
C
      WRITE(LUPTR,*) 'After INTRCT LCHOSEN=',(LCHOSEN(I),I=1,NTOTL)
C
      CALL FLSRCH(NTOTL,LUPTR)
C
      WRITE(LUPTR,*) 'After FLSRCH LCHOSEN=',(LCHOSEN(I),I=1,NTOTL)
C
      IF(LUNPK) THEN
C       CALL GRSORT(SELN,NCHOSEN,NTOTL,IFKEY,LUPTR)
C       WRITE(LUPTR,*) 'After GRSORT LCHOSEN=',(LCHOSEN(I),I=1,NTOTL)
        DO N=1,NTOTL
          SELN(N)=N
        ENDDO
      ELSE
        DO N=1,NTOTL
          SELN(N)=N
        ENDDO
        DO N=1,NTOTL
          WRITE(LUPTR,*) N,')',LABEL(1,N)(1:LENLA(1,N)),
     1                       ' (',LABBR(N),' ) ',
     2                   ' at ',LABEL(2,N)(1:LENLA(2,N)),' ',
     3                   LABEL(3,N)(1:LENLA(3,N)),' ',
     4                   LABEL(4,N)(1:LENLA(4,N)),' ',
     5                   LABEL(5,N)(1:LENLA(5,N))
          WRITE(LUPTR,*) '   over the area (',
     1              RSLAT(N),',',WLON(N),')(',RNLAT(N),',',ELON(N),')'
        ENDDO
      ENDIF
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NTOTL=',NTOTL
        WRITE(LUPTR,*) ' IMAXL,JMAXL,IJMAXL=',IMAXL,JMAXL,IJMAXL
      ENDIF
      WRITE(LUPTR,*) ' '
      PRINT *,' '
C
C  Get key KPDS and KGDS values for selected fields
C
      IF(LSELCT) THEN
        IF(SELN(I).EQ.N) THEN
          DO K=1,LKPDS
C           KEYPDS(K,I)=KPDS(K,NN)
C           KEYPDS(K,N)=KPDS(K,NN)
            KEYPDS(K,I)=KPDS(K,N)
            KEYPDS(K,N)=KPDS(K,N)
          ENDDO
          DO K=1,LKGDS
C           KEYGDS(K,I)=KGDS(K,NN)
C           KEYGDS(K,N)=KGDS(K,NN)
            KEYGDS(K,I)=KGDS(K,N)
            KEYGDS(K,N)=KGDS(K,N)
          ENDDO
          GO TO 765
        ELSEIF(SELN(I).EQ.-99) THEN
          DO K=1,LKPDS
            KEYPDS(K,I)=0
            KEYPDS(K,N)=0
          ENDDO
          DO K=1,LKGDS
            KEYGDS(K,I)=0
            KEYGDS(K,N)=0
          ENDDO
          GO TO 765
        ENDIF
  765   CONTINUE
      ENDIF
C
  333 CONTINUE
C
      IF(NTOTL.GT.MTOTL) THEN
        WRITE(LUPTR,*) 'ERROR!!! ',
     1      ' Too many fields.  NTOTL=',NTOTL
        WRITE(LUPTR,*) 'Max allowed MTOTL=',MTOTL
        PRINT *,'ERROR!!! ',
     1      ' Too many fields.  NTOTL=',NTOTL
        PRINT *,'Max allowed MTOTL=',MTOTL
        CALL ABORT
      ENDIF
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NTOTL=',NTOTL
        WRITE(LUPTR,*) ' DEBUG print from GETGRB end'
        WRITE(LUPTR,*) ' '
      ENDIF
C
      CLOSE(UNIT=LUPGB)
      RETURN
      END
      SUBROUTINE INIPARM(FNKPDS5,LUPTR)
C
C     Initialize KPDS(5) table
C
      PARAMETER(LUKP5=90)
C
      CHARACTER*128 FNKPDS5

      PARAMETER (NKPDS5=255)
      CHARACTER*64 PINFO(0:NKPDS5)
      CHARACTER*64 PNAME(0:NKPDS5)
      CHARACTER*64 PUNIT(0:NKPDS5)
      DIMENSION LENPI(0:NKPDS5),LENPN(0:NKPDS5),LENPU(0:NKPDS5)
C
      COMMON/COMPRM/ PINFO,PUNIT,PNAME,LENPI,LENPU,LENPN
C
      CHARACTER*80 CPDS5
      CHARACTER*80 STRING
C
C  Open Parameter table (PDS5) file
C
      OPEN(LUKP5,FILE=FNKPDS5,STATUS='OLD',ERR=1)
      GO TO 2
    1 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNKPDS5
      PRINT *,'ERROR IN OPENING FILE ',FNKPDS5
      CALL ABORT
    2 CONTINUE
      WRITE(LUPTR,*) 'FILE ',FNKPDS5(1:128),' opened.  Unit=',LUKP5
C
      N=1
  300 CONTINUE
      READ(LUKP5,100,END=200) STRING
  100 FORMAT(A80)
      IS=1
			NDELIM=0
      DO 500 I=1,80
			IF(STRING(I:I).EQ.':') THEN
        NDELIM=NDELIM+1
        NCHAR=I-1-IS+1
        IF(NCHAR.GT.0) THEN
          IF(NDELIM.EQ.1) THEN
            CPDS5(1:NCHAR)=STRING(IS:I-1)
						READ(CPDS5,110) M
  110       FORMAT(I3)
          ELSEIF(NDELIM.EQ.2) THEN
						JJ=0
            DO 505 J=IS+1,I-1
            IF(STRING(J-1:J-1).NE.' '.OR.STRING(J:J).NE.' ') THEN
	  					JJ=JJ+1
              PINFO(M)(JJ:JJ)=STRING(J-1:J-1)
						ENDIF
  505       CONTINUE
            IF(STRING(I-1:I-1).NE.' ') THEN
	  					JJ=JJ+1
              PINFO(M)(JJ:JJ)=STRING(I-1:I-1)
						ENDIF
            LENPI(M)=JJ
          ELSEIF(NDELIM.EQ.3) THEN
						JJ=0
            DO 510 J=IS,I-1
            IF(STRING(J:J).NE.' ') THEN
						  JJ=JJ+1
              PUNIT(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  510       CONTINUE
            LENPU(M)=JJ
						JJ=0
            DO 520 J=I+1,80
            IF(STRING(J:J).NE.' ') THEN
						  JJ=JJ+1
              PNAME(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  520       CONTINUE
            LENPN(M)=JJ
						GO TO 500
          ENDIF
  				IS=I+1
				ENDIF
			ENDIF
  500 CONTINUE
C
			IF(NDELIM.EQ.1) THEN
				PINFO(M)(1:1)='?'
				PUNIT(M)(1:1)='?'
				PNAME(M)(1:1)='?'
        LENPI(M)=1
        LENPU(M)=1
        LENPN(M)=1
			ELSEIF(NDELIM.EQ.2) THEN
				PUNIT(M)(1:1)='?'
				PNAME(M)(1:1)='?'
        LENPU(M)=1
        LENPN(M)=1
			ENDIF
C
      N=N+1
      GO TO 300
  200 CONTINUE
C
      CLOSE(UNIT=LUKP5)
      RETURN
      END
      SUBROUTINE INILEVL(FNKPDS6,LUPTR)
C
      PARAMETER (LUKP6=90)
      PARAMETER (NKPDS6=255)
C
      CHARACTER*128 FNKPDS6
      CHARACTER*80  PDS6 (0:NKPDS6)
C
      CHARACTER*64 LINFO(0:NKPDS6)
      CHARACTER*64 LUNIT(0:NKPDS6)
      CHARACTER*64 LFACT(0:NKPDS6)
      CHARACTER*64 LBASE(0:NKPDS6)
      CHARACTER*64 LTYPE(0:NKPDS6)
      CHARACTER*64 LEVLR(0:NKPDS6)
      CHARACTER*3  LEVAB(0:NKPDS6)
C
      DIMENSION LENLI(0:NKPDS6),LENLU(0:NKPDS6),LENLF(0:NKPDS6),
     1          LENLB(0:NKPDS6),LENLT(0:NKPDS6),LENLE(0:NKPDS6)
C
      COMMON/COMLVL/ LINFO,LUNIT,LFACT,LBASE,LTYPE,LEVLR,LEVAB,
     1               LENLI,LENLU,LENLF,LENLB,LENLT,LENLE
C
      CHARACTER*160 STRING
C
C  Open Level table (PDS6) file
C
      OPEN(LUKP6,FILE=FNKPDS6,STATUS='OLD',ERR=761)
      GO TO 762
  761 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNKPDS6
      PRINT *,'ERROR IN OPENING FILE ',FNKPDS6
      CALL ABORT
  762 CONTINUE
      WRITE(LUPTR,*) 'FILE ',FNKPDS6(1:128),' opened.  Unit=',LUKP6
C
      N=1
  300 CONTINUE
      READ(LUKP6,100,END=200) STRING
  100 FORMAT(A80)
      IS=1
			NDELIM=0
      DO 500 I=1,80
			IF(STRING(I:I).EQ.':') THEN
        NDELIM=NDELIM+1
        NCHAR=I-1-IS+1
        IF(NCHAR.GT.0) THEN
          IF(NDELIM.EQ.1) THEN
            PDS6(N-1)(1:NCHAR)=STRING(IS:I-1)
						READ(PDS6(N-1),110) M
  110       FORMAT(I3)
          ELSEIF(NDELIM.EQ.2) THEN
						JJ=0
            DO 505 J=IS+1,I-1
            IF(STRING(J-1:J-1).NE.' '.OR.STRING(J:J).NE.' ') THEN
	  					JJ=JJ+1
              LINFO(M)(JJ:JJ)=STRING(J-1:J-1)
						ENDIF
  505       CONTINUE
            IF(STRING(I-1:I-1).NE.' ') THEN
	  					JJ=JJ+1
              LINFO(M)(JJ:JJ)=STRING(I-1:I-1)
						ENDIF
						LENLI(M)=JJ
          ELSEIF(NDELIM.EQ.3) THEN
						JJ=0
            DO 506 J=IS,I-1
            IF(STRING(J-1:J-1).NE.' ') THEN
	  					JJ=JJ+1
              LUNIT(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  506       CONTINUE
						LENLU(M)=JJ
            IF(LUNIT(M)(1:1).EQ.'-') THEN
              LUNIT(M)(1:1)=' '
              LENLU(M)=1
            ENDIF
          ELSEIF(NDELIM.EQ.4) THEN
						JJ=0
            DO 507 J=IS,I-1
            IF(STRING(J-1:J-1).NE.' ') THEN
	  					JJ=JJ+1
              LFACT(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  507       CONTINUE
						LENLF(M)=JJ
          ELSEIF(NDELIM.EQ.5) THEN
						JJ=0
            DO 508 J=IS,I-1
            IF(STRING(J-1:J-1).NE.' ') THEN
	  					JJ=JJ+1
              LBASE(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  508       CONTINUE
						LENLB(M)=JJ
          ELSEIF(NDELIM.EQ.6) THEN
						JJ=0
            DO 510 J=IS,I-1
            IF(STRING(J:J).NE.' ') THEN
						  JJ=JJ+1
              LTYPE(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  510       CONTINUE
						LENLT(M)=JJ
          ELSEIF(NDELIM.EQ.7) THEN
						JJ=0
            DO 520 J=IS,I-1
            IF(STRING(J:J).NE.' ') THEN
						  JJ=JJ+1
              LEVLR(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  520       CONTINUE
						LENLE(M)=JJ
						JJ=0
C           DO 530 J=I+1,80
            DO 530 J=I+1,I+3
            IF(STRING(J:J).NE.' ') THEN
						  JJ=JJ+1
              LEVAB(M)(JJ:JJ)=STRING(J:J)
						ENDIF
  530       CONTINUE
						GO TO 500
          ENDIF
  				IS=I+1
				ENDIF
			ENDIF
  500 CONTINUE
C
			IF(NDELIM.EQ.1) THEN
				LINFO(M)(1:1)='?'
				LUNIT(M)(1:1)='?'
        LFACT(M)(1:1)='?'
        LBASE(M)(1:1)='?'
				LTYPE(M)(1:1)='?'
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLI(M)=1
        LENLU(M)=1
        LENLF(M)=1
        LENLB(M)=1
        LENLT(M)=1
        LENLE(M)=1
			ELSEIF(NDELIM.EQ.2) THEN
				LUNIT(M)(1:1)='?'
        LFACT(M)(1:1)='?'
        LBASE(M)(1:1)='?'
				LTYPE(M)(1:1)='?'
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLU(M)=1
        LENLF(M)=1
        LENLB(M)=1
        LENLT(M)=1
        LENLE(M)=1
			ELSEIF(NDELIM.EQ.3) THEN
        LFACT(M)(1:1)='?'
        LBASE(M)(1:1)='?'
				LTYPE(M)(1:1)='?'
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLF(M)=1
        LENLB(M)=1
        LENLT(M)=1
        LENLE(M)=1
			ELSEIF(NDELIM.EQ.4) THEN
        LBASE(M)(1:1)='?'
				LTYPE(M)(1:1)='?'
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLB(M)=1
        LENLT(M)=1
        LENLE(M)=1
			ELSEIF(NDELIM.EQ.5) THEN
				LTYPE(M)(1:1)='?'
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLT(M)=1
        LENLE(M)=1
			ELSEIF(NDELIM.EQ.6) THEN
				LEVLR(M)(1:1)='?'
				LEVAB(M)(1:3)='?..'
        LENLE(M)=1
			ENDIF
C
      N=N+1
      GO TO 300
  200 CONTINUE
C
      CLOSE(UNIT=LUKP6)
C
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE GETPARM(KPDS,NTOTL,LABEL,LENLA,LABBR,LUPTR)
C
      DIMENSION KPDS(25,*)
C
C     Transrates KPDS(5,*) parameter to variable names
C
      CHARACTER*64 LABEL(5,*)
      DIMENSION LENLA(5,*)
      CHARACTER*8 LABBR(*)
C
      PARAMETER (NKPDS5=255)
      CHARACTER*64 PINFO(0:NKPDS5)
      CHARACTER*64 PNAME(0:NKPDS5)
      CHARACTER*64 PUNIT(0:NKPDS5)
      DIMENSION LENPI(0:NKPDS5),LENPN(0:NKPDS5),LENPU(0:NKPDS5)
C
      SAVE NUNDEF
      DATA NUNDEF/0/
C
      COMMON/COMPRM/ PINFO,PUNIT,PNAME,LENPI,LENPU,LENPN
C
      LOGICAL LDEBUG
      COMMON/COMDBG/ LDEBUG
C
      DO N=1,NTOTL
				K=KPDS(5,N)
C
				IF(K.GT.NKPDS5) THEN
           WRITE(LUPTR,*) ' FATAL ERROR in INDEX file! ',
     1          ' KPDS(5,N)=',KPDS(5,N),' FOR N=',N
           WRITE(LUPTR,*) ' KPDS(5,N) CANNOT BE GREATER THAN ',NKPDS5
           PRINT *,' FATAL ERROR in INDEX file! ',
     1          ' KPDS(5,N)=',KPDS(5,N),' FOR N=',N
           PRINT *,' KPDS(5,N) CANNOT BE GREATER THAN ',NKPDS5
           CALL ABORT
        ENDIF
        IF(PNAME(K)(1:1).EQ.'?'.AND.PINFO(K)(1:1).EQ.'?'
     1     .AND.PUNIT(K)(1:1).EQ.'?') THEN
           WRITE(LUPTR,*) ' SERIOUS PROBLEM in INDEX file',
     1           ' KPDS(5,N)=',KPDS(5,N),' FOR N=',N
           WRITE(LUPTR,*) ' THE PARAMETER IS NOT DEFINED ',
     1           'IN THE KPDS(5) TABLE FILE '
           PRINT *,' Serious problem in INDEX file',
     1           ' KPDS(5,N)=',KPDS(5,N),' FOR N=',N
           PRINT *,' THE PARAMETER IS NOT DEFINED ',
     1           'IN THE KPDS(5) TABLE FILE '
           NUNDEF=NUNDEF+1
           IF(NUNDEF.GE.100) THEN
             WRITE(LUPTR,*) 'Too many undefined variables'
             PRINT *,'Too many undefined variables'
             CALL ABORT
           ENDIF
           IF(NUNDEF.LT.10) THEN
             WRITE(PINFO(K),'(6HUNDEF0,I1)') NUNDEF
           ELSEIF(NUNDEF.LT.100) THEN
             WRITE(PINFO(K),'(5HUNDEF,I2)') NUNDEF
           ENDIF
           LENPI(K)=7
           PNAME(K)=PINFO(K)
           LENPN(K)=7
           PUNIT(K)='?'
           LENPU(K)=1
           WRITE(LUPTR,*) 'NUNDEF=',NUNDEF,' PINFO(K)=',PINFO(K),
     1                    ' PNAME(K)=',PNAME(K),' PUNIT(K)=',PUNIT(K)
		    ENDIF
C
        LABEL(1,N)=PINFO(K)(1:LENPI(K))//' ('//PUNIT(K)(1:LENPU(K))//')'
        LENLA(1,N)=LENPI(K)+2+LENPU(K)+1
C
C       WRITE(LUPTR,*) 'N=',N,' K=',K,' ',LABEL(1,N)(1:LENLA(1,N))
        LABBR(N)='        '
        LABBR(N)=PNAME(K)(1:LENPN(K))
      ENDDO
C
      RETURN
			END
      SUBROUTINE GETLEVL(KPDS,NTOTL,LABEL,LENLA,IRLEV,LVABBR,LUPTR)
C
C     Transrates KPDS(6,*) parameter to level names
C
      CHARACTER*64 LABEL(5,*)
      DIMENSION LENLA(5,*)
      DIMENSION IRLEV(*)
      CHARACTER*3 LVABBR(*)
C
      DIMENSION KPDS(25,*)
C
      CHARACTER*160 FMT
C
      PARAMETER (NKPDS6=255)
      CHARACTER*64 LINFO(0:NKPDS6)
      CHARACTER*64 LUNIT(0:NKPDS6)
      CHARACTER*64 LFACT(0:NKPDS6)
      CHARACTER*64 LBASE(0:NKPDS6)
      CHARACTER*64 LTYPE(0:NKPDS6)
      CHARACTER*64 LEVLR(0:NKPDS6)
      CHARACTER*3  LEVAB(0:NKPDS6)
C
      DIMENSION LENLI(0:NKPDS6),LENLU(0:NKPDS6),LENLF(0:NKPDS6),
     1          LENLB(0:NKPDS6),LENLT(0:NKPDS6),LENLE(0:NKPDS6)
C
      COMMON/COMLVL/ LINFO,LUNIT,LFACT,LBASE,LTYPE,LEVLR,LEVAB,
     1               LENLI,LENLU,LENLF,LENLB,LENLT,LENLE
C
      CHARACTER*64 LTOPL,LBTML
C
      SAVE NUNDEF
      DATA NUNDEF/0/
C
      DO 100 N=1,NTOTL
				K=KPDS(6,N)
				IF(K.GT.NKPDS6.OR.K.LT.0) THEN
           WRITE(LUPTR,*) ' FATAL ERROR in INDEX FILE! ',
     1          'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           WRITE(LUPTR,*) ' KPDS(6,N) CANNOT BE GREATER THAN ',NKPDS6
           PRINT *,' FATAL ERROR in INDEX FILE! ',
     1          'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           PRINT *,' KPDS(6,N) CANNOT BE GREATER THAN ',NKPDS6
           CALL ABORT
        ENDIF
        IF(LINFO(K)(1:1).EQ.'?'.AND.LUNIT(K)(1:1).EQ.'?'
     1     .AND.LEVLR(K)(1:1).EQ.'?') THEN
           WRITE(LUPTR,*) ' Serious problem in index buffer! ',
     1        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           WRITE(LUPTR,*) ' THE LEVEL IS NOT DEFINED ',
     1        'IN THE FILE KPDS(6) TABLE FILE.'
           PRINT *,' Serious problem in index buffer! ',
     1        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           PRINT *,' THE LEVEL IS NOT DEFINED ',
     1        'IN THE FILE KPDS(6) TABLE FILE.'
           NUNDEF=NUNDEF+1
           IF(NUNDEF.GE.100) THEN
             WRITE(LUPTR,*) 'Too many undefined levels'
             PRINT *,'Too many undefined levels'
             CALL ABORT
           ENDIF
           IF(NUNDEF.LT.10) THEN
             WRITE(LINFO(K),'(6HUNDEF0,I1)') NUNDEF
           ELSEIF(NUNDEF.LT.100) THEN
             WRITE(LINFO(K),'(5HUNDEF,I2)') NUNDEF
           ENDIF
           LENLI(K)=7
           LUNIT(K)='?'
           LENLU(K)=1
           LEVLR(K)='level'
           LTYPE(K)='-'
		    ENDIF
				IF(K.GE.140.AND.K.LE.159) THEN
           WRITE(LUPTR,*) ' ERROR in utilizing index buffer ! ',
     1        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           WRITE(LUPTR,*) ' Mixed precision level not programed',
     1        'to work in this program.'
           PRINT *,' ERROR in utilizing index buffer ! ',
     1        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           PRINT *,' Mixed precision level not programed',
     1        'to work in this program.'
  		     CALL ABORT
        ENDIF
C
        IF(LTYPE(K)(1:1).EQ.'?') THEN
          WRITE(LUPTR,*) ' FATAL ERROR! KPDS(6,N)=',
     1           KPDS(6,N),' FOR N=',N
          WRITE(LUPTR,*) ' LVTYP NOT SPECIFIED IN THE LEVEL subroutine'
          PRINT *,' FATAL ERROR! KPDS(6,N)=',
     1           KPDS(6,N),' FOR N=',N
          PRINT *,' LTYPE NOT SPECIFIED IN THE LEVEL subroutine'
          CALL ABORT
        ENDIF
C
        DO I=1,LENLF(K)
          IF(LFACT(K)(I:I).EQ.'.') THEN
            NDEC=LENLF(K)-I
          ENDIF
        ENDDO
        IF(LENLF(K).LT.10) THEN
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I1,1H.,I1,1H))') LENLF(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I1,1H.,I2,1H))') LENLF(K),NDEC
          ENDIF
        ELSEIF(LENLF(K).LT.100) THEN
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I2,1H.,I1,1H))') LENLF(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I2,1H.,I2,1H))') LENLF(K),NDEC
          ENDIF
        ELSE
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I3,1H.,I1,1H))') LENLF(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I3,1H.,I2,1H))') LENLF(K),NDEC
          ENDIF
        ENDIF
C
        READ(LFACT(K),FMT) RFACT
        DO I=1,LENLB(K)
          IF(LBASE(K)(I:I).EQ.'.') THEN
            NDEC=LENLB(K)-I
          ENDIF
        ENDDO
        IF(LENLB(K).LT.10) THEN
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I1,1H.,I1,1H))') LENLB(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I1,1H.,I2,1H))') LENLB(K),NDEC
          ENDIF
        ELSEIF(LENLB(K).LT.100) THEN
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I2,1H.,I1,1H))') LENLB(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I2,1H.,I2,1H))') LENLB(K),NDEC
          ENDIF
        ELSE
          IF(NDEC.LT.10) THEN
            WRITE(FMT,'(2H(F,I3,1H.,I1,1H))') LENLB(K),NDEC
          ELSE
            WRITE(FMT,'(2H(F,I3,1H.,I2,1H))') LENLB(K),NDEC
          ENDIF
        ENDIF
C
        READ(LBASE(K),FMT) RBASE
C
        IF(LEVLR(K)(1:5).EQ.'layer') THEN
          NN=KPDS(7,N)/2**8
          TOPL=NN*RFACT+RBASE
          BTML=(KPDS(7,N)-NN*2**8)*RFACT+RBASE
        ELSEIF(LEVLR(K)(1:5).EQ.'level') THEN
          TOPL=KPDS(7,N)*RFACT+RBASE
          BTML=KPDS(7,N)*RFACT+RBASE
        ELSEIF(LEVLR(K)(1:6).EQ.'slevel') THEN
  		    IF(LTYPE(K)(1:7).EQ.'surface') THEN
            TOPL=0.
            BTML=0.
          ELSE
            TOPL=KPDS(7,N)*RFACT+RBASE
            BTML=KPDS(7,N)*RFACT+RBASE
          ENDIF
        ELSE
           WRITE(LUPTR,*) ' FATAL ERROR! KPDS(6,N)=',
     1             KPDS(6,N),' FOR N=',N
           WRITE(LUPTR,*) ' ILLEGAL LEVEL TYPE ENCOUNTERED ',
     1             'IN THE FILE KPDS(6) TABLE FILE'
           WRITE(LUPTR,*) ' LEVLR(K)=',LEVLR(K)
           PRINT *,' FATAL ERROR! KPDS(6,N)=',
     1             KPDS(6,N),' FOR N=',N
           PRINT *,' ILLEGAL LEVEL TYPE ENCOUNTERED ',
     1             'IN THE FILE KPDS(6) TABLE FILE'
           PRINT *,' LEVLR(K)=',LEVLR(K)
  		     CALL ABORT
		    ENDIF
C
C       RLEV(N)=(TOPL+BTML)*0.5
C
        IRLEV(N)=NINT(BTML/RFACT)
C
        IF(FLOAT(INT(TOPL)).EQ.TOPL) THEN
          WRITE(LTOPL,'(I5)') INT(TOPL)
          LENT=5
        ELSE
          IF(TOPL.GT.999.99) THEN
            WRITE(LTOPL,'(F8.2)') TOPL
          ELSEIF(TOPL.GT.99.999) THEN
            WRITE(LTOPL,'(F8.3)') TOPL
          ELSEIF(TOPL.GT.9.9999) THEN
            WRITE(LTOPL,'(F8.4)') TOPL
          ELSE
            WRITE(LTOPL,'(F8.5)') TOPL
          ENDIF
          LENT=8
        ENDIF
        IF(FLOAT(INT(BTML)).EQ.BTML) THEN
          WRITE(LBTML,'(I5)') INT(BTML)
          LENB=5
        ELSE
          IF(BTML.GT.999.99) THEN
            WRITE(LBTML,'(F8.2)') BTML
          ELSEIF(BTML.GT.99.999) THEN
            WRITE(LBTML,'(F8.3)') BTML
          ELSEIF(BTML.GT.9.9999) THEN
            WRITE(LBTML,'(F8.4)') BTML
          ELSE
            WRITE(LBTML,'(F8.5)') BTML
          ENDIF
          LENB=8
        ENDIF
C
        LABEL(2,N)=LINFO(K)(1:LENLI(K))
        LENLA(2,N)=LENLI(K)
C
C 		  IF(LEVLR(K)(1:5).EQ.'level'.OR.LEVLR(K)(1:6).EQ.'slevel') THEN
C 		    IF(LTYPE(K)(1:7).EQ.'surface'.OR.LTYPE(K)(1:1).EQ.'-') THEN
C           LABEL(3,N)=' '
C           LENLA(3,N)=1
C  	    ELSE
C           LABEL(3,N)=LTOPL(1:LENT)//LUNIT(K)(1:LENLU(K))
C           LENLA(3,N)=LENT+LENLU(K)
C	  		ENDIF
        IF(LEVLR(K)(1:6).EQ.'slevel') THEN
          LABEL(3,N)=' '
          LENLA(3,N)=1
  		  ELSEIF(LEVLR(K)(1:5).EQ.'level') THEN
          LABEL(3,N)=LTOPL(1:LENT)//LUNIT(K)(1:LENLU(K))
          LENLA(3,N)=LENT+LENLU(K)
		 	  ELSEIF(LEVLR(K)(1:5).EQ.'layer') THEN
          LABEL(3,N)=LBTML(1:LENB)//LUNIT(K)(1:LENLU(K))//' and '//
     1               LTOPL(1:LENT)//LUNIT(K)(1:LENLU(K))
          LENLA(3,N)=LENB+LENLU(K)+5+LENT+LENLU(K)
        ENDIF
C
        LVABBR(N)=LEVAB(K)
C
  100 CONTINUE
C
      RETURN
			END
      SUBROUTINE GETFTIM(KPDS,NTOTL,LABEL,LENLA,FJHOUR,LUPTR)
C
      DIMENSION KPDS(25,*)
C
      CHARACTER*64 LABEL(5,*)
      DIMENSION LENLA(5,*)
      DIMENSION FJHOUR(*)
C
      CHARACTER*16 FHUNIT
C
      CHARACTER*160 FHTEXT
      CHARACTER*160 YLABEL(3)
      DIMENSION LENLBY(3)
C
      SAVE MFH0
C
      LOGICAL LDEBUG
      COMMON/COMDBG/ LDEBUG
C
C  Forecast time range
C
      DO N=1,NTOTL
C
C  Forecast hour units
C
        IF(KPDS(13,N).EQ.0) THEN
          FHUNIT='min'
          LENFHU=3
        ELSEIF(KPDS(13,N).EQ.1) THEN
          FHUNIT='hr'
          LENFHU=2
        ELSEIF(KPDS(13,N).EQ.2) THEN
          FHUNIT='day'
          LENFHU=3
        ELSEIF(KPDS(13,N).EQ.3) THEN
          FHUNIT='month'
          LENFHU=5
        ELSEIF(KPDS(13,N).EQ.4) THEN
          FHUNIT='year'
          LENFHU=4
        ELSEIF(KPDS(13,N).EQ.5) THEN
          FHUNIT='decade'
          LENFHU=6
        ELSEIF(KPDS(13,N).EQ.6) THEN
          FHUNIT='normal'
          LENFHU=6
        ELSEIF(KPDS(13,N).EQ.7) THEN
          FHUNIT='century'
          LENFHU=7
        ELSEIF(KPDS(13,N).EQ.254) THEN
          FHUNIT='sec'
          LENFHU=3
        ELSE
          WRITE(LUPTR,*) 'Error!!!  KPDS(13)=',KPDS(13,N)
          WRITE(LUPTR,*) 'Reserved status in the table 4.'
          PRINT *,'Error!!!  KPDS(13)=',KPDS(13,N)
          PRINT *,'Reserved status in the table 4.'
          CALL ABORT
        ENDIF
C
        IF(KPDS(16,N).EQ.0) THEN
          IF(KPDS(14,N).EQ.0) THEN
            FHTEXT='UNINITIALIZED ANALYSIS'
            LENFHT=22
            FJHOUR(N)=0.
          ELSE
            WRITE(FHTEXT,'(I6,A8)') KPDS(14,N),FHUNIT
            LENFHT=14
            FJHOUR(N)=KPDS(14,N)
          ENDIF
        ELSEIF(KPDS(16,N).EQ.1) THEN
          IF(KPDS(14,N).EQ.0) THEN
            FHTEXT='INITIALIZED ANALYSIS'
            LENFHT=20
            FJHOUR(N)=0
          ELSE
            WRITE(LUPTR,*) 'Error!!!' 
            WRITE(LUPTR,*) 'KPDS(16)=1 but KPDS(14)=',KPDS(14,N),'.NE.0'
            WRITE(LUPTR,*) 'Inconsistent according to the table 5.'
            PRINT *,'Error!!! ' 
            PRINT *,'KPDS(16)=1 but KPDS(14)=',KPDS(14,N),'.NE.0'
            PRINT *,'Inconsistent according to the table 5.'
            CALL ABORT
          ENDIF
        ELSEIF(KPDS(16,N).EQ.2) THEN
          WRITE(FHTEXT,'(6HVALID ,I6,2H -,I6,A8)')
     1          KPDS(14,N),KPDS(15,N),FHUNIT
          LENFHT=28
          FJHOUR(N)=(KPDS(14,N)+KPDS(15,N))/2
        ELSEIF(KPDS(16,N).EQ.3) THEN
          WRITE(FHTEXT,'(8HAVERAGE ,I6,2H -,I6,A8)')
     1          KPDS(14,N),KPDS(15,N),FHUNIT
          LENFHT=30
          FJHOUR(N)=KPDS(15,N)
        ELSEIF(KPDS(16,N).EQ.4) THEN
          WRITE(FHTEXT,'(7HACCUM. ,I6,2H -,I6,A8)')
     1          KPDS(14,N),KPDS(15,N),FHUNIT
          LENFHT=29
          FJHOUR(N)=KPDS(15,N)
        ELSEIF(KPDS(16,N).EQ.5) THEN
          WRITE(FHTEXT,'(6HDIFF. ,I6,2H -,I6,A8)')
     1          KPDS(15,N),KPDS(14,N),FHUNIT
          LENFHT=28
          FJHOUR(N)=KPDS(15,N)
        ELSEIF(KPDS(16,N).EQ.10) THEN
          WRITE(FHTEXT,'(I6,A8)') KPDS(14,N),FHUNIT
          LENFHT=14
          FJHOUR(N)=KPDS(14,N)
        ELSEIF(KPDS(16,N).EQ.113) THEN
          WRITE(FHTEXT,'(8HAVERAGE ,I4,4H FT=,I6,10HINI EVERY ,I6,A8)')
     1          KPDS(17,N),KPDS(14,N),KPDS(15,N),FHUNIT
          LENFHT=46
          FJHOUR(N)=KPDS(14,N)
        ELSEIF(KPDS(16,N).EQ.114) THEN
          WRITE(FHTEXT,'(7HACCUM. ,I4,4H FT=,I6,10HINI EVERY ,I6,A8)')
     1          KPDS(17,N),KPDS(14,N),KPDS(15,N),FHUNIT
          LENFHT=45
          FJHOUR(N)=KPDS(14,N)
        ELSEIF(KPDS(16,N).EQ.115) THEN
          WRITE(FHTEXT,'(8HAVERAGE ,I4,4H FT=,I6,1H/,I6,A8)')
     1          KPDS(17,N),KPDS(14,N),KPDS(15,N),FHUNIT
          LENFHT=37
          FJHOUR(N)=KPDS(14,N)
        ELSEIF(KPDS(16,N).EQ.116) THEN
          WRITE(FHTEXT,'(7HACCUM. ,I4,4H FT=,I6,1H/,I6,A8)')
     1          KPDS(17,N),KPDS(14,N),KPDS(15,N),FHUNIT
          LENFHT=36
          FJHOUR(N)=KPDS(14,N)
        ELSEIF(KPDS(16,N).EQ.117) THEN
          WRITE(FHTEXT,'(8HAVERAGE ,I4,4H FT=,I6,2H -,I6,3H*N,A8)')
     1          KPDS(17,N),KPDS(14,N),KPDS(15,N),FHUNIT
          LENFHT=41
          FJHOUR(N)=KPDS(14,N)
        ELSEIF(KPDS(16,N).EQ.118) THEN
          WRITE(FHTEXT,'(9HVARIANCE ,I4,4H FT=,I6,2H -,I6,3H*N,A8)')
     1          KPDS(17,N),KPDS(14,N),KPDS(15,N),FHUNIT
          LENFHT=42
          FJHOUR(N)=KPDS(14,N)
        ELSEIF(KPDS(16,N).EQ.123) THEN
          WRITE(FHTEXT,'(8HAVERAGE ,I4,4H FT=,I6,2H /,I6,3H*N,A8)')
     1          KPDS(17,N),KPDS(14,N),KPDS(15,N),FHUNIT
          LENFHT=41
          FJHOUR(N)=KPDS(14,N)
        ELSEIF(KPDS(16,N).EQ.124) THEN
          WRITE(FHTEXT,'(7HACCUM. ,I4,4H FT=,I6,1H/,I6,A8)')
     1          KPDS(17,N),KPDS(14,N),KPDS(15,N),FHUNIT
          LENFHT=36
          FJHOUR(N)=KPDS(14,N)
        ELSEIF(KPDS(16,N).EQ.51) THEN
          WRITE(FHTEXT,'(7HCLIMO. ,I6,A8)')
     1          KPDS(15,N),FHUNIT
          LENFHT=36
          FJHOUR(N)=KPDS(14,N)
        ELSE
          WRITE(LUPTR,*) 'Error!!!' 
          WRITE(LUPTR,*) 'KPDS(16)=',KPDS(16,N),
     1                   ' Not defined in table 5.'
          PRINT *,'Error!!!' 
          PRINT *,'KPDS(16)=',KPDS(16,N),' Not defined in table 5.'
          CALL ABORT
        ENDIF
C
        LABEL(5,N)(1:LENFHT+4)='FT='//FHTEXT(1:LENFHT)
        LENLA(5,N)=LENFHT+4
C
        IF(KPDS(13,N).EQ.0) THEN
          FJHOUR(N)=FJHOUR(N)/60.
        ELSEIF(KPDS(13,N).EQ.1) THEN
          FJHOUR(N)=FJHOUR(N)
        ELSEIF(KPDS(13,N).EQ.2) THEN
          FJHOUR(N)=FJHOUR(N)*24.
        ELSEIF(KPDS(13,N).EQ.3) THEN
          FJHOUR(N)=FJHOUR(N)*24.*30.
        ELSEIF(KPDS(13,N).EQ.4) THEN
          FJHOUR(N)=FJHOUR(N)*24.*365.
        ELSEIF(KPDS(13,N).EQ.5) THEN
          FJHOUR(N)=FJHOUR(N)*24.*3650.
        ELSEIF(KPDS(13,N).EQ.6) THEN
          FJHOUR(N)=-999.
        ELSEIF(KPDS(13,N).EQ.7) THEN
          FJHOUR(N)=-9999.
        ELSEIF(KPDS(13,N).EQ.254) THEN
          FJHOUR(N)=FJHOUR(N)/3600.
        ENDIF
C
      ENDDO
C
      RETURN
      END
      SUBROUTINE GETAREA(KGDS,NTOTL,DLAT,DLON,
     1                   LGLOBL,RSLAT,RNLAT,WLON,ELON,IJORDR,
     2                   PARMAP,NPARMP,LUPTR)
C
C  Get area of the grib record
C
      DIMENSION KGDS(22,*)
C
      DIMENSION RSLAT(*),RNLAT(*),WLON(*),ELON(*),DLON(*),DLAT(*)
      LOGICAL LGLOBL(*),IJORDR(*)
C
      DIMENSION PARMAP(10),NPARMP(10)
C
      LOGICAL LDEBUG
      COMMON/COMDBG/ LDEBUG
C
C  Lat/Lon grid
C
C     WRITE(LUPTR,*) 'KGDS=',(KGDS(1,N),N=1,NTOTL)
C
      IF(KGDS(1,1).EQ.0) THEN
        WRITE(LUPTR,*) 'LAT/LON GRID'
        DO N=1,NTOTL
          DLAT(N)=FLOAT(KGDS(10,N))/1000.0
          DLON(N)=FLOAT(KGDS( 9,N))/1000.0
          F0LON=FLOAT(KGDS(5,N))/1000.0
          F0LAT=FLOAT(KGDS(4,N))/1000.0
          KGDS11=KGDS(11,N)
C
C  INCREASE ACCURACY OF DLON IF POSSIBLE (12-18-00)
C
          DLONX=360./FLOAT(KGDS(2,N))
          IF(ABS(DLONX-DLON(N)).LT.0.001) THEN
            PRINT *,'DLON INCREASED ITS ACCURACY USING X-DIMENSION'
            PRINT *,'GRIB ENCODED DLON=',DLON(N)
            PRINT *,'CALCULATED DLON=',DLONX
            IF(ABS(DLON(N)).EQ.ABS(DLAT(N))) THEN
              PRINT *,'DLAT INCREASED ITS ACCURACY USING CORRECTED DLON'
              PRINT *,'GRIB ENCODED DLAT=',DLAT(N)
              DLAT(N)=DLAT(N)/ABS(DLAT(N))*DLONX
              PRINT *,'CORRECTED DLAT=',DLAT(N)
            ENDIF
            DLON(N)=DLON(N)/ABS(DLON(N))*DLONX
          ENDIF
          IF(KGDS11.GE.128) THEN
            WLON(N)=F0LON-DLON(N)*(KGDS(2,N)-1)
            ELON(N)=F0LON
            IF(DLON(N)*KGDS(2,N).GT.359.99) THEN
              WLON(N)=F0LON-DLON(N)*KGDS(2,N)
            ENDIF
            DLON(N)=-DLON(N)
            KGDS11=KGDS11-128
          ELSE
            WLON(N)=F0LON
            ELON(N)=F0LON+DLON(N)*(KGDS(2,N)-1)
            IF(DLON(N)*KGDS(2,N).GT.359.99) THEN 
              ELON(N)=F0LON+DLON(N)*KGDS(2,N)
            ENDIF
          ENDIF
          IF(KGDS11.GE.64) THEN
            RNLAT(N)=F0LAT+DLAT(N)*(KGDS(3,N)-1)
            RSLAT(N)=F0LAT
            KGDS11=KGDS11-64
          ELSE
            RNLAT(N)=F0LAT
            RSLAT(N)=F0LAT-DLAT(N)*(KGDS(3,N)-1)
            DLAT(N)=-DLAT(N)
          ENDIF
          IF(KGDS11.GE.32) THEN
            IJORDR(N)=.FALSE.
          ELSE
            IJORDR(N)=.TRUE.
          ENDIF
C
          LGLOBL(N)=.TRUE.
          FZLAT=ABS(DLAT(N))*(KGDS(3,N)-1)
          IF(FZLAT.LT.89.99) THEN
            LGLOBL(N)=.FALSE.
          ENDIF
C
          IF(LGLOBL(N)) THEN
            FZLON=ABS(DLON(N))*KGDS(2,N)
            IF(FZLON.LT.359.99) THEN
              LGLOBL(N)=.FALSE.
            ENDIF
          ENDIF
C
          IF(WLON(N).GT.180.) WLON(N)=WLON(N)-360.
          IF(ELON(N).GT.180.) ELON(N)=ELON(N)-360.
          WLON(N)=NINT(WLON(N)*1000.)/1000.
          ELON(N)=NINT(ELON(N)*1000.)/1000.
          RSLAT(N)=NINT(RSLAT(N)*1000.)/1000.
          RNLAT(N)=NINT(RNLAT(N)*1000.)/1000.
        ENDDO
        RETURN
C
C  Mercator projection
C         PARMAP(1)=KGDS(9,N) ... LATIN
C         NPARMP(1)=KGDS(6,N) ... Resolution flag
C
      ELSEIF(KGDS(1,1).EQ.1) THEN
        WRITE(LUPTR,*) 'Mercator GRID'
        PI=4.*ATAN(1.)
        RAD=PI/180.
        RADI=180./PI
        ER=6371.2E03
        DO N=1,NTOTL
          DLAT(N)=KGDS(13,N)
          F0LAT=FLOAT(KGDS(4,N))/1000.0
          F0LON=FLOAT(KGDS(5,N))/1000.0
          FLLAT=FLOAT(KGDS(7,N))/1000.0
          FLLON=FLOAT(KGDS(8,N))/1000.0
          PARMAP(1)=FLOAT(KGDS(9,N))/1000.0
c
c  Wesley's fix
c
          f00=f0lon
          if(fllon.lt.f0lon) f00=f0lon-360.
          dlon(n)=(fllon-f00)/float(kgds(2,n)-1)
c
          NPARMP(1)=KGDS(6,N)
          KGDS11=KGDS(11,N)
C
          IF(KGDS11.GE.128) THEN
            WLON(N)=FLLON
            ELON(N)=F0LON
            DLON(N)=-DLON(N)
            KGDS11=KGDS11-128
          ELSE
            WLON(N)=F0LON
            ELON(N)=FLLON
          ENDIF
          IF(KGDS11.GE.64) THEN
            RNLAT(N)=FLLAT
            RSLAT(N)=F0LAT
            KGDS11=KGDS11-64
          ELSE
            RNLAT(N)=F0LAT
            RSLAT(N)=FLLAT
            DLAT(N)=-DLAT(N)
          ENDIF
          IF(KGDS11.GE.32) THEN
            IJORDR(N)=.FALSE.
          ELSE
            IJORDR(N)=.TRUE.
          ENDIF
C
          IF(N.EQ.1) THEN
            WRITE(LUPTR,*) 'KGDS(11,N)=',KGDS(11,1)
            WRITE(LUPTR,*) 'F0LON=',F0LON,' FLLON=',FLLON
            WRITE(LUPTR,*) 'F0LAT=',F0LAT,' FLLAT=',FLLAT
            WRITE(LUPTR,*) 'DLAT=',DLAT(1),' DLON=',DLON(1)
            WRITE(LUPTR,*) 'Parmap=',PARMAP(1),' NPARMP=',NPARMP(1)
          ENDIF
C
          LGLOBL(N)=.TRUE.
          FZLAT=ABS(DLAT(N))*(KGDS(3,N)-1)
          IF(FZLAT.LT.80.) THEN
            LGLOBL(N)=.FALSE.
          ENDIF
C
          IF(LGLOBL(N)) THEN
            FZLON=ABS(DLON(N))*KGDS(2,N)
            IF(FZLON.LT.359.99) THEN
              LGLOBL(N)=.FALSE.
            ENDIF
          ENDIF
C
          IF(WLON(N).GT.180.) WLON(N)=WLON(N)-360.
          IF(ELON(N).GT.180.) ELON(N)=ELON(N)-360.
          WLON(N)=NINT(WLON(N)*1000.)/1000.
          ELON(N)=NINT(ELON(N)*1000.)/1000.
          RSLAT(N)=NINT(RSLAT(N)*1000.)/1000.
          RNLAT(N)=NINT(RNLAT(N)*1000.)/1000.
        ENDDO
        RETURN
C
C  Gnomonic projection
C
      ELSEIF(KGDS(1,1).EQ.2) THEN
        WRITE(LUPTR,*) 'Gnomonic GRID'
        WRITE(LUPTR,*) 'ERROR!! Gnomonic projection not coded'
        WRITE(LUPTR,*) 'KGDS(1,1)=',KGDS(1,1)
        PRINT *,'ERROR!! Gnomonic projection not coded'
        PRINT *,'KGDS(1,1)=',KGDS(1,1)
        CALL ABORT
C
C  Lambert conformal
C
C   (7)  - LOV GRID ORIENTATION   ...  Stored into NPARMP(1)
C  (10)  - PROJECTION CENTER FLAG ...  Stored into NPARMP(2)
C  (12)  - Latin1                 ...  Stored into NPARMP(3)
C  (13)  - Latin2                 ...  Stored into NPARMP(4)
C
      ELSEIF(KGDS(1,1).EQ.3) THEN
        WRITE(LUPTR,*) 'Lambert conformal'
        DO N=1,NTOTL
          DLAT(N)=FLOAT(KGDS( 9,N))
          DLON(N)=FLOAT(KGDS( 8,N))
          F0LAT=FLOAT(KGDS(4,N))/1000.0
          F0LON=FLOAT(KGDS(5,N))/1000.0
          NPARMP(1)=KGDS(7,N)
          NPARMP(2)=KGDS(10,N)
          NPARMP(3)=KGDS(12,N)
          NPARMP(4)=KGDS(13,N)
          IF(KGDS(11,N).GE.32) THEN
            IJORDR(N)=.FALSE.
          ELSE
            IJORDR(N)=.TRUE.
          ENDIF
          LGLOBL(N)=.FALSE.
        ENDDO
        RETURN
      ELSEIF(KGDS(1,1).EQ.4) THEN
C
C  Gaussian grid
C
        WRITE(LUPTR,*) 'Gaussian GRID'
        DO N=1,NTOTL
          DLAT(N)=99.
          DLON(N)=FLOAT(KGDS( 9,N))/1000.0
          DLONX=FLOAT(NINT(360./FLOAT(KGDS(2,N))*1000.))/1000.0
          IF(DLON(N).NE.DLONX) THEN
             WRITE(LUPTR,*) 'WARNING..DLON IN KGDS(9) NOT CONSISTENT'
             DLON(N)=DLONX
          ENDIF
          F0LON=FLOAT(KGDS(5,N))/1000.0
          F0LAT=99.
          KGDS11=KGDS(11,N)
          IF(KGDS11.GE.128) THEN
            WLON(N)=F0LON
            ELON(N)=F0LON
            IF(DLON(N)*KGDS(2,N).GT.359.99) THEN
              WLON(N)=F0LON-DLON(N)*KGDS(2,N)
            ENDIF
            DLON(N)=-DLON(N)
            KGDS11=KGDS11-128
          ELSE
            WLON(N)=F0LON
            ELON(N)=F0LON+DLON(N)*(KGDS(2,N)-1)
            IF(DLON(N)*KGDS(2,N).GT.359.99) THEN 
              ELON(N)=F0LON+DLON(N)*KGDS(2,N)
            ENDIF
          ENDIF
          IF(KGDS11.GE.64) THEN
            RNLAT(N)=99.
            RSLAT(N)=99.
          ELSE
            RNLAT(N)=99.
            RSLAT(N)=99.
            DLAT(N)=-99.
          ENDIF
          IF(KGDS11.GE.32) THEN
            IJORDR(N)=.FALSE.
          ELSE
            IJORDR(N)=.TRUE.
          ENDIF
C
          LGLOBL(N)=.TRUE.
        ENDDO
        RETURN
C
C  Polar Strereographic
C   (7)  - LOV GRID ORIENTATION   ...  Stored into PARMAP(1)
C  (10)  - PROJECTION CENTER FLAG ...  Stored into NPARMP(1)
C
      ELSEIF(KGDS(1,1).EQ.5) THEN
        WRITE(LUPTR,*) 'Polar Stereographic GRID'
        DO N=1,NTOTL
          DLAT(N)=FLOAT(KGDS( 9,N))
          DLON(N)=FLOAT(KGDS( 8,N))
          F0LON=FLOAT(KGDS(5,N))/1000.0
          F0LAT=FLOAT(KGDS(4,N))/1000.0
          WLON(N)=F0LON
          RSLAT(N)=F0LAT
          NPARMP(1)=KGDS(10,N)
          PARMAP(1)=KGDS(7,N)/1000.
          KGDS11=KGDS(11,N)
          IF(KGDS11.GE.128) THEN
            DLON(N)=-DLON(N)
            KGDS11=KGDS11-128
          ENDIF
          IF(KGDS11.GE.64) THEN
            KGDS11=KGDS11-64
          ELSE
            DLAT(N)=-DLAT(N)
          ENDIF
          IF(KGDS11.GE.32) THEN
            IJORDR(N)=.FALSE.
          ELSE
            IJORDR(N)=.TRUE.
          ENDIF
          LGLOBL(N)=.FALSE.
C
C  Dirty fix for bad stereographic file (RSM reanalysis 1986 June run)
C
C         IF(KGDS(11,N).EQ.2) THEN
C           DLAT(N)=-DLAT(N)
C         ENDIF
        ENDDO
        RETURN
C
C  Oblique Lambert conformal
C
      ELSEIF(KGDS(1,1).EQ.13) THEN
        WRITE(LUPTR,*) 'Oblique Lambert conformal GRID'
        WRITE(LUPTR,*) 'ERROR!! Oblique Lambert projection not coded'
        WRITE(LUPTR,*) 'KGDS(1,1)=',KGDS(1,1)
        PRINT *,'ERROR!! Oblique Lambert projection not coded'
        PRINT *,'KGDS(1,1)=',KGDS(1,1)
        CALL ABORT
C
C  Spherical Coefficient
C
      ELSEIF(KGDS(1,1).EQ.50) THEN
        DO N=1,NTOTL
        WRITE(LUPTR,*) 'Spherical Coefficient'
          DLAT(N)=FLOAT(KGDS( 9,N))
          DLON(N)=FLOAT(KGDS( 8,N))
          F0LON=FLOAT(KGDS(5,N))/1000.0
          F0LAT=FLOAT(KGDS(4,N))/1000.0
          WLON(N)=F0LON
          ELON(N)=KGDS(7,N)
          RSLAT(N)=F0LAT
          RNLAT(N)=KGDS(10,N)
          IF(KGDS(11,N).GE.32) THEN
            IJORDR(N)=.FALSE.
          ELSE
            IJORDR(N)=.TRUE.
          ENDIF
          LGLOBL(N)=.TRUE.
        ENDDO
        WRITE(LUPTR,*) 'ERROR!! Spectral coeff not coded'
        WRITE(LUPTR,*) 'KGDS(1,1)=',KGDS(1,1)
        PRINT *,'ERROR!! Spectral coeff not coded'
        PRINT *,'KGDS(1,1)=',KGDS(1,1)
        CALL ABORT
        RETURN
C
C  Space view perspective (orthographic grid)
C
      ELSEIF(KGDS(1,1).EQ.90) THEN
        WRITE(LUPTR,*) 'Space view perspective GRID'
        WRITE(LUPTR,*) 'ERROR!! Space view projection not coded'
        WRITE(LUPTR,*) 'KGDS(1,1)=',KGDS(1,1)
        PRINT *,'ERROR!! Space view projection not coded'
        PRINT *,'KGDS(1,1)=',KGDS(1,1)
        CALL ABORT
        RETURN
C
C  Unknown projection.  Abort.
C
      ELSE
        WRITE(LUPTR,*) 'ERROR!! Unknown map projection'
        WRITE(LUPTR,*) 'KGDS(1,1)=',KGDS(1,1)
        PRINT *,'ERROR!! Unknown map projection'
        PRINT *,'KGDS(1,1)=',KGDS(1,1)
        CALL ABORT
      ENDIF
C
      RETURN
      END
      SUBROUTINE GETMXDM(KGDS,NTOTL,IDIM,JDIM,IMAXL,JMAXL,IJMAXL,LUPTR)
C
      DIMENSION KGDS(22,*)
C
      DIMENSION IDIM(*),JDIM(*)
C
      LOGICAL LDEBUG
      COMMON/COMDBG/ LDEBUG
C
      DO N=1,NTOTL
        IDIM(N)=KGDS(2,N)
        JDIM(N)=KGDS(3,N)
        IJDIM=IDIM(N)*JDIM(N)
        IF(IJDIM.GT.IJMAXL) IJMAXL=IJDIM
        IF(IDIM(N).GT.IMAXL) IMAXL=IDIM(N)
        IF(JDIM(N).GT.JMAXL) JMAXL=JDIM(N)
      ENDDO
C
      RETURN
      END
      SUBROUTINE RMBLNK(LABEL,LENLA,N)
C
C  Get rid of two or more consecutive blanks here
C
      CHARACTER*64 LABEL(N)
      DIMENSION LENLA(N)
      CHARACTER*160 LABEX
C
      DO II=1,N
        JJ=0
        DO J=2,LENLA(II)
          IF(LABEL(II)(J-1:J-1).NE.' '.OR.LABEL(II)(J:J).NE.' ') THEN
	          JJ=JJ+1
            LABEX(JJ:JJ)=LABEL(II)(J-1:J-1)
          ENDIF
          IF(JJ.GT.1) THEN
            IF(LABEX(JJ-1:JJ-1).EQ.'='.AND.LABEX(JJ:JJ).EQ.' ') THEN
              JJ=JJ-1
            ENDIF
          ENDIF
        ENDDO
C
        IF(LABEL(II)(LENLA(II):LENLA(II)).NE.' ') THEN
  	    	JJ=JJ+1
          LABEX(JJ:JJ)=LABEL(II)(LENLA(II):LENLA(II))
        ENDIF
C
        IF(JJ.LE.0) THEN
          LENLA(II)=1
          LABEL(II)(1:1)=' '
        ELSE
          LENLA(II)=JJ
          LABEL(II)(1:LENLA(II))=LABEX(1:LENLA(II))
        ENDIF
C
      ENDDO
C
      RETURN
      END
      SUBROUTINE FLSRCH(NTOTL,LUPTR)
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      LOGICAL LFP(MTOTL),LPFND
      CHARACTER*64 LABTMP
C
      CHARACTER*1 ALFA(26)
      DATA ALFA/'a','b','c','d','e','f','g','h','i','j','k','l','m','n',
     1          'o','p','q','r','s','t','u','v','w','x','y','z'/
C
      LOGICAL LDEBUG
      COMMON/COMDBG/ LDEBUG
C
C  Modify variable name if same variable name but with differenct level types
C
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      DO N=1,NTOTL
        IF(.NOT.LFP(N).AND.LCHOSEN(N)) THEN
          LPFND=.FALSE.
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M).AND.LCHOSEN(M)) THEN
              IF(LABBR(N).EQ.LABBR(M).AND.
     1           LABEL(2,N)(1:LENLA(2,N)).NE.
     2           LABEL(2,M)(1:LENLA(2,M))) THEN
                LPFND=.TRUE.
                J=0
                DO I=1,5
                  IF(LABBR(M)(I:I).NE.' ') THEN
                    J=J+1
                    LABTMP(J:J)=LABBR(M)(I:I)
                  ENDIF
                ENDDO
                LABBR(M)(1:J+3)=LABTMP(1:J)//LVABBR(M)(1:3)
                DO I=J+4,8
                  LABBR(M)(I:I)=' '
                ENDDO
                WRITE(LUPTR,*) 'Variable "',LABTMP(1:J),'" on ',
     1                        LABEL(2,M)(1:LENLA(2,M)),' modified to "',
     2                        LABBR(M)(1:J+3),'"'
                LFP(M)=.TRUE.
              ENDIF
            ENDIF
          ENDDO
          IF(LPFND) THEN
            DO M=N+1,NTOTL
              IF(LABBR(N).EQ.LABBR(M)) THEN
                J=0
                DO I=1,5
                  IF(LABBR(M)(I:I).NE.' ') THEN
                    J=J+1
                    LABTMP(J:J)=LABBR(M)(I:I)
                  ENDIF
                ENDDO
                LABBR(M)(1:J+3)=LABTMP(1:J)//LVABBR(M)(1:3)
                DO I=J+4,8
                  LABBR(M)(I:I)=' '
                ENDDO
                WRITE(LUPTR,*) 'Variable "',LABTMP(1:J),'" on ',
     1                          LABEL(2,M)(1:LENLA(2,M)),
     2                         ' modified to "',LABBR(M)(1:J+3),'"'
              ENDIF
            ENDDO
            J=0
            DO I=1,5
              IF(LABBR(N)(I:I).NE.' ') THEN
                J=J+1
                LABTMP(J:J)=LABBR(N)(I:I)
              ENDIF
            ENDDO
            LABBR(N)(1:J+3)=LABTMP(1:J)//LVABBR(N)(1:3)
            DO I=J+4,8
              LABBR(N)(I:I)=' '
            ENDDO
            WRITE(LUPTR,*) 'Variable "',LABTMP(1:J),'" on ',
     1                      LABEL(2,N)(1:LENLA(2,N)),
     2                     ' modified to "',LABBR(N)(1:J+3),'"'
          ENDIF
        ENDIF
      ENDDO
C
C  Modify variable name if duplicate name is found
C
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      DO N=1,NTOTL
        IF(.NOT.LFP(N).AND.LCHOSEN(N)) THEN
          NDUP=0
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M).AND.LCHOSEN(M)) THEN
              IF(LABBR(N).EQ.LABBR(M).AND.
     1           LABEL(2,N)(1:LENLA(2,N)).EQ.
     2           LABEL(2,M)(1:LENLA(2,M)).AND.
     3           LABEL(3,N)(1:LENLA(3,N)).EQ.
     4           LABEL(3,M)(1:LENLA(3,M)).AND.
     5           LABEL(4,N)(1:LENLA(4,N)).EQ.
     6           LABEL(4,M)(1:LENLA(4,M)).AND.
     7           LABEL(5,N)(1:LENLA(5,N)).EQ.
     8           LABEL(5,M)(1:LENLA(5,M)).AND.
     9           RSLAT(N).EQ.RSLAT(M).AND.RNLAT(N).EQ.RNLAT(M).AND.
     A           WLON (N).EQ.WLON (M).AND.ELON (N).EQ.ELON (M)) THEN
                NDUP=NDUP+1
                J=0
                DO I=1,5
                  IF(LABBR(M)(I:I).NE.' ') THEN
                    J=J+1
                    LABTMP(J:J)=LABBR(M)(I:I)
                  ENDIF
                ENDDO
                LABBR(M)(1:J+3)=LABTMP(1:J)
     1                          //ALFA(NDUP)//ALFA(NDUP)//ALFA(NDUP)
                DO I=J+4,8
                  LABBR(M)(I:I)=' '
                ENDDO
                WRITE(LUPTR,*) 'Duplicate Variable name "',LABTMP(1:J),
     1                        LABEL(2,M)(1:LENLA(2,M)),' modified to "',
     2                        LABBR(M)(1:J+3),'"'
                PRINT *,'!! Duplicate Variable name "',LABTMP(1:J),
     1                      LABEL(2,M)(1:LENLA(2,M)),' modified to "',
     2                      LABBR(M)(1:J+3),'"'
                LFP(M)=.TRUE.
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C  Delete character '-'.
C
      DO N=1,NTOTL
        DO I=1,8
          LABTMP(I:I)=LABBR(N)(I:I)
        ENDDO
        LABBR(N)='        '
        J=0
        DO I=1,8
          IF(LABTMP(I:I).NE.'-') THEN
            J=J+1
            LABBR(N)(J:J)=LABTMP(I:I)
          ENDIF
        ENDDO
      ENDDO
C
C  Find number of variables 
C
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      NVTOTL=0
      NN=1
      NV=1
      DO N=1,NTOTL
        IF(.NOT.LFP(N).AND.LCHOSEN(N)) THEN
          NVTOTL=NVTOTL+1
          IVINDX(NVTOTL)=N
          LFP(N)=.TRUE.
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M).AND.LCHOSEN(M).AND.
     1          LABBR(N).EQ.LABBR(M)) THEN
              LFP(M)=.TRUE.
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NVTOTL=',NVTOTL
        DO K=1,NVTOTL
          WRITE(LUPTR,*) LABBR(IVINDX(K))
        ENDDO
      ENDIF
C
C  Find number of levels for each variable
C
      LLEVL=.FALSE.
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      NNN=0
      DO NV=1,NVTOTL
        NVL=IVINDX(NV)
        NLTOTL(NV)=0
        DO N=1,NTOTL
          IF(.NOT.LFP(N).AND.LCHOSEN(N).AND.
     1                       LABEL(1,N)(1:LENLA(1,N)).EQ.
     2                       LABEL(1,NVL)(1:LENLA(1,NVL)).AND.
     3                       LABBR(N).EQ.LABBR(NVL)) THEN
            NNN=NNN+1
            NLTOTL(NV)=NLTOTL(NV)+1
            ILINDX(NNN)=N
            LFP(N)=.TRUE.
            DO M=N+1,NTOTL
              IF(.NOT.LFP(M).AND.LCHOSEN(M).AND.
     1                       LABEL(1,M)(1:LENLA(1,M)).EQ.
     1                       LABEL(1,NVL)(1:LENLA(1,NVL)).AND.
     2                       LABBR(M).EQ.LABBR(NVL).AND.
     3                       LABEL(3,M)(1:LENLA(3,M)).EQ.
     4                       LABEL(3,N)(1:LENLA(3,N))) THEN
                  LFP(M)=.TRUE.
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
C  Insert 12/14/95
C
C  Merge single multi-level field into surface fields,
C  by modifying LABEL(3,*) and LENLA(3,*) variables
C 
      DO NV=1,NVTOTL
        IF(NLTOTL(NV).EQ.1) THEN
          NVL=IVINDX(NV)
          IF(LENLA(3,NVL).EQ.1) THEN
            LABEL(3,NVL)='-XXX'
          ELSE
            LABEL(3,NVL)='-'//LABEL(3,NVL)(1:LENLA(3,NVL))
          ENDIF
          LENLA(3,NVL)=1
        ENDIF
      ENDDO
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NLTOTL(NV)=',(NLTOTL(NV),NV=1,NVTOTL)
        NNN=0
        DO NV=1,NVTOTL
          WRITE(LUPTR,*) (LABEL(3,ILINDX(K+NNN))
     1                   (1:10),',',K=1,NLTOTL(NV))
C    1                   (1:LENLA(3,ILINDX(K+NNN))),',',K=1,NLTOTL(NV))
          NNN=NNN+NLTOTL(NV)
        ENDDO
      ENDIF
      LLEVL=.FALSE.
      DO NV=1,NVTOTL
        IF(NLTOTL(NV).GT.1) THEN
          LLEVL=.TRUE.
          GO TO 101
        ENDIF
      ENDDO
  101 CONTINUE
C
C  Find number of multi-level level types
C
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      NPTOTL=0
      NN=0
      DO N=1,NTOTL
        IF(.NOT.LFP(N).AND.LCHOSEN(N).AND.LENLA(3,N).GT.1) THEN
          NPTOTL=NPTOTL+1
          LFP(N)=.TRUE.
          IPINDX(NPTOTL)=N
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M).AND.LCHOSEN(M).AND.
     1                         LABEL(2,N)(1:LENLA(2,N)).EQ.
     2                         LABEL(2,M)(1:LENLA(2,M))) THEN
              LFP(M)=.TRUE.
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      IF(NPTOTL.GT.1) THEN
        LLTYP=.TRUE.
      ELSE
        LLTYP=.FALSE.
      ENDIF
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NPTOTL=',NPTOTL
        DO K=1,NPTOTL
          WRITE(LUPTR,*) LABEL(2,IPINDX(K))(1:LENLA(2,IPINDX(K)))
        ENDDO
      ENDIF
C
C  Find number of separated areas
C
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      NNN=0
      DO NV=1,NVTOTL
        NVL=IVINDX(NV)
        NATOTL(NV)=0
        DO N=1,NTOTL
          IF(.NOT.LFP(N).AND.LCHOSEN(N).AND.
     1                       LABEL(1,N)(1:LENLA(1,N)).EQ.
     2                       LABEL(1,NVL)(1:LENLA(1,NVL)).AND.
     3                       LABBR(N).EQ.LABBR(NVL)) THEN
            NNN=NNN+1
            NATOTL(NV)=NATOTL(NV)+1
            IAINDX(NNN)=N
            LFP(N)=.TRUE.
            DO M=N+1,NTOTL
              IF(.NOT.LFP(M).AND.LCHOSEN(M).AND.
     1                       LABEL(1,M)(1:LENLA(1,M)).EQ.
     2                       LABEL(1,NVL)(1:LENLA(1,NVL)).AND.
     3                       LABBR(M).EQ.LABBR(NVL).AND.
     4             RSLAT(N).EQ.RSLAT(M).AND.RNLAT(N).EQ.RNLAT(M).AND.
     5             WLON (N).EQ.WLON (M).AND.ELON (N).EQ.ELON (M)) THEN
                  LFP(M)=.TRUE.
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDDO
      LAREA=.FALSE.
      DO NV=1,NVTOTL
        IF(NATOTL(NV).GT.1) THEN
          LAREA=.TRUE.
          GO TO 102
        ENDIF
      ENDDO
  102 CONTINUE
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NATOTL=',(NATOTL(NV),NV=1,NVTOTL)
        NNN=0
        DO NV=1,NVTOTL
          N=IAINDX(K+NNN)
          WRITE(LUPTR,*) ('(',RSLAT(IAINDX(K+NNN)),
     1                    ',',WLON(IAINDX(K+NNN)),')',
     2                    '(',RNLAT(IAINDX(K+NNN)),
     3                   ',',ELON(IAINDX(K+NNN)),')',',',K=1,NATOTL(NV))
          NNN=NNN+NATOTL(NV)
        ENDDO
      ENDIF
C
C  Find number of initial time series fields
C
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      NITOTL=0
      NN=0
      DO N=1,NTOTL
        IF(.NOT.LFP(N).AND.LCHOSEN(N)) THEN
          NITOTL=NITOTL+1
          LFP(N)=.TRUE.
          ITINDX(NITOTL)=N
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M).AND.LCHOSEN(M).AND.
     1                         LABEL(4,N)(1:LENLA(4,N)).EQ.
     2                         LABEL(4,M)(1:LENLA(4,M))) THEN
              LFP(M)=.TRUE.
C           ELSE
C            WRITE(LUPTR,*) 'LABEL(N)=',LABEL(4,N),' LEN=',LENLA(4,N)
C            WRITE(LUPTR,*) 'LABEL(M)=',LABEL(4,M),' LEN=',LENLA(4,M)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      IF(NITOTL.GT.1) THEN
        LITSR=.TRUE.
      ELSE
        LITSR=.FALSE.
      ENDIF
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NITOTL=',NITOTL
        DO K=1,NITOTL
          WRITE(LUPTR,*) LABEL(4,ITINDX(K))(1:LENLA(4,ITINDX(K)))
        ENDDO
      ENDIF
C
C  Find number of forecast time series fields
C
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      NFTOTL=1
      NN=0
      DO N=1,NTOTL
        IF(.NOT.LFP(N).AND.LCHOSEN(N)) THEN
          NFTOTL=NFTOTL+1
          LFP(N)=.TRUE.
          IFINDX(NFTOTL)=N
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M).AND.LCHOSEN(M).AND.
     1                         LABEL(5,N)(1:LENLA(5,N)).EQ.
     2                         LABEL(5,M)(1:LENLA(5,M))) THEN
              LFP(M)=.TRUE.
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
C  Find number of forecast time series fields.  Consider end time of 
C  accumulation and average as a forecast time 
C
      DO N=1,NTOTL
        LFP(N)=.FALSE.
      ENDDO
      NFXTTL=0
      NN=0
      DO N=1,NTOTL
        IF(.NOT.LFP(N).AND.LCHOSEN(N)) THEN
          NFXTTL=NFXTTL+1
          LFP(N)=.TRUE.
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M).AND.LCHOSEN(M).AND.
     1         FJHOUR(N,1).EQ.FJHOUR(M,1)) THEN
              LFP(M)=.TRUE.
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      IF(NFXTTL.GT.1) THEN
        LFTSR=.TRUE.
      ELSE
        LFTSR=.FALSE.
      ENDIF
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NFTOTL=',NFTOTL,' NFXTTL=',NFXTTL
        DO K=1,NFTOTL
          WRITE(LUPTR,*) LABEL(5,IFINDX(K))(1:LENLA(5,IFINDX(K)))
        ENDDO
      ENDIF
C
      WRITE(LUPTR,*) ' '
      IF(LLEVL) THEN
        WRITE(LUPTR,*) ' Multi-level fields found in a given file'
        WRITE(LUPTR,*) ' Number of multi-level fields='
        WRITE(LUPTR,*) (NLTOTL(NV),NV=1,NVTOTL)
      ENDIF
      IF(LITSR) THEN
        WRITE(LUPTR,*) ' Time-series fields found in a given file'
        WRITE(LUPTR,*) ' Number of time series fields=',NITOTL
      ENDIF
      IF(LFTSR) THEN
        WRITE(LUPTR,*) ' Fcst-time-series fields found in a given file'
        WRITE(LUPTR,*) ' Number of fcst-time series fields=',NFXTTL
      ENDIF
      IF(LAREA) THEN
        WRITE(LUPTR,*) ' Same field devided into different areas found',
     1                 ' in a given file'
        WRITE(LUPTR,*) ' Number of such fields='
        WRITE(LUPTR,*) ' NATOTL=',(NATOTL(NV),NV=1,NVTOTL)
      ENDIF
      IF(LLTYP) THEN
        WRITE(LUPTR,*) ' Same variable with different level type found',
     1                 ' in a given file'
        WRITE(LUPTR,*) ' Number of such fields=',NPTOTL
      ENDIF
C
      RETURN
      END
      SUBROUTINE INTRCT(NTOTL,NCHOSEN,LUSEL,LHISKEY,LUPTR)
C
C  INPUT ... NTOTL, LUPTR
C  OUTPT ... NCHOSEN, LCHOSEN
C
C  LINES ... Number of display lines for fields
C
      PARAMETER (LINES=8)
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      DIMENSION LSTIO(80)
      CHARACTER*1 YN
      CHARACTER*1 MARK
C
      LOGICAL LHISKEY
C
      DO III=1,MTOTL
      LCHOSEN(III)=.FALSE.
      ENDDO
      NCHOSEN=0
C
      PRINT *,' '
      PRINT *,' ',NTOTL,' Parameters are found.'
      PRINT *,' '
C
    5 CONTINUE
C
      NLN=NTOTL/LINES
      MODN=MOD(NTOTL,LINES)
      IF(MODN.NE.0) NLN=NLN+1
      N=0
      DO 10 NN=1,NLN
       NLF=LINES
       IF(NN.EQ.NLN.AND.MODN.NE.0) NLF=MODN
   15  CONTINUE
			 DO 20 NL=1,NLF
        N=N+1
        LEN5=MIN(8,LENLA(5,N))
        IF(.NOT.LHISKEY) THEN
          MARK=' '
          IF(LCHOSEN(N)) MARK='*'
          PRINT *, MARK,N,')',LABEL(1,N)(1:LENLA(1,N)),' ',
     1                   LABEL(2,N)(1:LENLA(2,N)),' ',
     1                   LABEL(3,N)(1:LENLA(3,N)),' ',
     2                   LABEL(4,N)(1:LENLA(4,N)),' ',
     3                   LABEL(5,N)(1:LEN5)
          IF(LAREA) THEN
             PRINT *,' (',RSLAT(N),',',WLON(N),
     1             ')  (',RNLAT(N),',',ELON(N),')'
          ENDIF
        ENDIF
   20   CONTINUE
        IF(.NOT.LHISKEY) THEN
         PRINT *,' '
         PRINT *,'Type the field number(s) you want to select.'
         PRINT *,'Separate them with comma and end the line with ',
     1         '/ and enter.'
         PRINT *,'Type -1 .. exit from the list.'
         PRINT *,'Type -9 .. to select all the currently listed fields.'
         IF(NN.EQ.1) THEN
           PRINT *,'Type -99 .. to select all the fields in the file.'
         ENDIF
        ENDIF
C
       DO 25 I=1,80
        LSTIO(I)=0
   25  CONTINUE
C
       READ(*,*,ERR=33) LSTIO
       WRITE(LUSEL,*) LSTIO,'/'
       GO TO 34
   33  CONTINUE
       IF(.NOT.LHISKEY) THEN
         PRINT *,'Illegal input.  Try again.'
       ELSE
         WRITE(LUPTR,*) ' ERROR.  BAD HISTKEY FILE'
         PRINT *,' ERROR.  BAD HISTKEY FILE'
         CALL ABORT
       ENDIF
       N=MAX(N-NLF,1)
       GO TO 15
   34 CONTINUE
C
      DO 30 I=1,80
      IF(LSTIO(I).NE.0) THEN
        IF(LSTIO(I).GT.N) THEN
C			  IF(.NOT.LHISKEY) THEN
C           PRINT *,'Input too large. Try again.'
C         ELSE
C           WRITE(LUPTR,*) ' ERROR.  BAD HISTKEY FILE'
C           PRINT *,' ERROR.  BAD HISTKEY FILE'
C           CALL ABORT
C         ENDIF
C         N=MAX(N-NLF,1)
C         GO TO 15
          NCHOSEN=NCHOSEN+1
          LCHOSEN(LSTIO(I))=.TRUE.
        ELSEIF(LSTIO(I).EQ.-9) THEN
          PRINT *,'Input -9.  Select all the currently displayed',
     1            ' parameters.'
          JJ=0
          DO J=N-NLF+1,N
            JJ=JJ+1
            LSTIO(JJ)=J
          ENDDO
        ELSEIF(NN.EQ.1.AND.LSTIO(I).EQ.-99) THEN
          PRINT *,'Input -99.  Select all the parameters.'
          PRINT *,'Are you sure? Type y/n.'
          PRINT *,' '
          READ (*,'(A1)') YN
          WRITE(LUSEL,'(A1)') YN
          IF(YN.EQ.'y') THEN
             NCHOSEN=NTOTL
             DO J=1,NCHOSEN
               LCHOSEN(J)=.TRUE.
             ENDDO
             GO TO 11
          ELSEIF(YN.NE.'n') THEN
            PRINT *,'Illegal input.  Try again.'
            N=MAX(N-NLF,1)
            GO TO 15
          ENDIF
        ELSEIF(LSTIO(I).EQ.-1) THEN
          IF(.NOT.LHISKEY) THEN
            PRINT *,'Input is -1.  Skip parameter list.'
          ENDIF
          GO TO 11
        ENDIF
        IF(LSTIO(I).GT.0) THEN
          NCHOSEN=NCHOSEN+1
          LCHOSEN(LSTIO(I))=.TRUE.
        ENDIF
      ENDIF
   30 CONTINUE
   10 CONTINUE
   11 CONTINUE
C
      IF(NCHOSEN.LE.0) THEN
        PRINT *,' '
        PRINT *,' You have not selected any fields.'
        PRINT *,' Press <enter> to go back to start.' 
        READ(*,'(A1)',ERR=39) YN
   39   CONTINUE
        GO TO 5
      ELSE
          PRINT *,' '
          PRINT *,' You have selected ',NCHOSEN,' fields from this file'
          PRINT *,' '
          DO N=1,NTOTL
            IF(LCHOSEN(N)) THEN
              PRINT *, N,')',LABEL(1,N)(1:LENLA(1,N)),' ',
     1                       LABEL(3,N)(1:LENLA(3,N)),' ',
     2                       LABEL(4,N)(1:LENLA(4,N)),' ',
     3                       LABEL(5,N)(1:LEN5),' ',
     4                      '(',RSLAT(N),',',WLON(N),')',
     5                      '(',RNLAT(N),',',ELON(N),')'
            ENDIF
          ENDDO
          PRINT *,' '
        IF(.NOT.LHISKEY) THEN
          PRINT *,' Press <enter> to continue.' 
          PRINT *,' Press "b" and <enter> to go back to the start for',
     1            ' more selection.'
        ENDIF
        READ(*,'(A1)',ERR=40) YN
        IF(YN.EQ.'b') GO TO 5
   40   CONTINUE
        WRITE(LUSEL,*) YN
      ENDIF
C
      WRITE(LUPTR,*) 'NCHOSEN=',NCHOSEN
      WRITE(LUPTR,*) 'LCHOSEN=',(LCHOSEN(I),I=1,NTOTL)
C
      RETURN
      END
      SUBROUTINE GRSORT(SELN,NCHOSEN,NTOTL,IFKEY,LUPTR)
C
      INTEGER SELN(*)
      CHARACTER*1 IFKEY
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      LOGICAL LPFND
C
      LOGICAL LDEBUG
      COMMON/COMDBG/ LDEBUG
C
C  Sort in grads order
C
      IF(IFKEY.EQ.'I') THEN
        NT1E=NFXTTL
        NT2E=NITOTL
      ELSE
        NT1E=NITOTL
        NT2E=NFXTTL
      ENDIF
      NN=0
      DO NT1=1,NT1E
        IF(IFKEY.EQ.'I') THEN
          NFT=NT1
        ELSE
          NIT=NT1
        ENDIF
        DO NT2=1,NT2E
          IF(IFKEY.EQ.'I') THEN
            NIT=NT2
          ELSE
            NFT=NT2
          ENDIF
          IT=ITINDX(NIT)
          IF=IFINDX(NFT)
          NNL=0
          NNA=0
          DO NVR=1,NVTOTL
            IV=IVINDX(NVR)
            DO NLV=1,NLTOTL(NVR)
              IL=ILINDX(NNL+NLV)
              DO NAR=1,NATOTL(NVR)
                IA=IAINDX(NNA+NAR)
                LPFND=.FALSE.
                DO N=1,NTOTL
                  IF(LCHOSEN(N).AND.
     1               LABEL(1,N)(1:LENLA(1,N)).EQ.
     2               LABEL(1,IV)(1:LENLA(1,IV)).AND.
     3               LABBR(N).EQ.LABBR(IV).AND.
     4               LABEL(2,N)(1:LENLA(2,N)).EQ.
     5               LABEL(2,IL)(1:LENLA(2,IL)).AND.
     6               LABEL(3,N)(1:LENLA(3,N)).EQ.
     7               LABEL(3,IL)(1:LENLA(3,IL)).AND.
     8               RSLAT(N).EQ.RSLAT(IA).AND.
     8               RNLAT(N).EQ.RNLAT(IA).AND.
     9               WLON(N).EQ.WLON(IA).AND.ELON(N).EQ.ELON(IA)) THEN
                     LPFND=.TRUE.
                    IF(LABEL(4,N)(1:LENLA(4,N)).EQ.
     1                 LABEL(4,IT)(1:LENLA(4,IT)).AND.
     2                 FJHOUR(N,1).EQ.FJHOUR(IF,1)) THEN
                      NN=NN+1
                      SELN(NN)=N
                      GO TO 100
                    ENDIF
                  ENDIF
                ENDDO
                IF(LPFND.AND.NAR.EQ.NATOTL(NVR)) THEN
                  GO TO 844
                ENDIF
  100           CONTINUE
              ENDDO
            ENDDO
            NNL=NNL+NLTOTL(NVR)
            NNA=NNA+NATOTL(NVR)
          ENDDO
        ENDDO
      ENDDO
C
      NWNTTL=NN
C
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) 'NTOTL=',NTOTL,' NCHOSEN=',NCHOSEN,
     1                 ' NWNTTL=',NWNTTL
        WRITE(LUPTR,*) 'Sorted rec. number'
        WRITE(LUPTR,*) (SELN(N),N=1,NWNTTL)
      ENDIF
C
C  Check logic
C
  844 CONTINUE
      IF(NWNTTL.NE.NCHOSEN) THEN
        WRITE(LUPTR,*) 'Cannot sort. Sorting disabled.'
        WRITE(LUPTR,*) 'NWNTTL.NE.NCHOSEN'
        WRITE(LUPTR,*) 'NWNTTL=',NWNTTL,' NCHOSEN=',NCHOSEN
        PRINT *,'Cannot sort.  Sorting disabled.'
        DO N=1,NTOTL
          SELN(N)=N
        ENDDO
      ENDIF
C
      RETURN
      END
      SUBROUTINE GETDATE(KPDS,NTOTL,LABEL,LENLA,RJLDAY,LY2000,LUPTR)
C
      SAVE
C
      LOGICAL LY2000
C
      DIMENSION KPDS(25,*)
C
      DIMENSION RJLDAY(*)
C
      CHARACTER*64 LABEL(5,*)
      DIMENSION LENLA(5,*)
C
      CHARACTER*64 DATES
C
      CHARACTER*5 MONTH(12)
C
      DATA MONTH/' JAN ',' FEB ',' MAR ',' APR ',' MAY ',' JUN ',
     1           ' JUL ',' AUG ',' SEP ',' OCT ',' NOV ',' DEC '/
C
C  Initial dates
C
      DO N=1,NTOTL
C
        IY=KPDS(8,N)
        IM=KPDS(9,N)
        ID=KPDS(10,N)
        IH=KPDS(11,N)
        IMIN=KPDS(12,N)
        ICENTURY=KPDS(21,N)
        IY=IY+(ICENTURY-1)*100
C
C       WRITE(LUPTR,*) 'IY,IM,ID,IH,IMIN=',IY,IM,ID,IH,IMIN
C
        CALL COMPJD(IY,IM,ID,IH,IMIN,JD,FJD)
C
        RJLDAY(N)=JD+FJD
C
C       WRITE(LUPTR,*) 'JD,FJD=',JD,FJD
C
        IF(IM.LT.0.OR.IM.GT.12) THEN
          WRITE(LUPTR,*) 'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
          PRINT *,'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
          CALL ABORT
        ENDIF
        IF(ID.LT.0.OR.ID.GT.31) THEN
          WRITE(LUPTR,*) 'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
          PRINT *,'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
          CALL ABORT
        ENDIF
        IF(IH.LT.0.OR.IH.GT.24) THEN
          WRITE(LUPTR,*) 'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
          PRINT *,'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
          CALL ABORT
        ENDIF
        IF(IMIN.LT.0.OR.IMIN.GT.60) THEN
          WRITE(LUPTR,*) 'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
          PRINT *,'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
          CALL ABORT
        ENDIF
C
        IF(IM.EQ.0) IM=1
C
        LY2000=.TRUE.
        IF(IMIN.GT.0.AND.IMIN.LT.10) THEN
          IF(IH.GE.10) THEN
            IF(.NOT.LY2000) THEN
              WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1                  IH,IMIN,ID,MONTH(IM),IY
              LENDA=17
            ELSE
              WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1                  IH,IMIN,ID,MONTH(IM),IY
              LENDA=19
            ENDIF
          ELSE
            IF(.NOT.LY2000) THEN
              WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1                 IH,IMIN,ID,MONTH(IM),IY
              LENDA=17
            ELSE
              WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1                 IH,IMIN,ID,MONTH(IM),IY
              LENDA=19
            ENDIF
          ENDIF
        ELSEIF(IMIN.GE.10) THEN
          IF(IH.GE.10) THEN
            IF(.NOT.LY2000) THEN
              WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I2)')
     1                 IH,IMIN,ID,MONTH(IM),IY
              LENDA=17
            ELSE
              WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I4)')
     1                 IH,IMIN,ID,MONTH(IM),IY
              LENDA=19
            ENDIF
          ELSE
            IF(.NOT.LY2000) THEN
              WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I2)')
     1                 IH,IMIN,ID,MONTH(IM),IY
              LENDA=17
            ELSE
              WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I4)')
     1                 IH,IMIN,ID,MONTH(IM),IY
              LENDA=19
            ENDIF
          ENDIF
        ELSEIF(IH.LT.10) THEN
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I2)')
     1               IH,ID,MONTH(IM),IY
            LENDA=14
          ELSE
            WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I4)')
     1               IH,ID,MONTH(IM),IY
            LENDA=16
          ENDIF
        ELSE
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(I2,1HZ,I3,A5,1X,I2)')
     1               IH,ID,MONTH(IM),IY
            LENDA=14
          ELSE
            WRITE(DATES,'(I2,1HZ,I3,A5,1X,I4)')
     1               IH,ID,MONTH(IM),IY
            LENDA=16
          ENDIF
        ENDIF
C
        LABEL(4,N)(1:LENDA)=DATES(1:LENDA)
        LENLA(4,N)=LENDA
      ENDDO
C
      RETURN
      END
      SUBROUTINE COMPJD(JYR,JMNTH,JDAY,JHR,JMN,JD,FJD)
CFPP$ NOCONCUR R
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    COMPJD      COMPUTES JULIAN DAY AND FRACTION
C   PRGMMR: KENNETH CAMPANA  ORG: W/NMC23    DATE: 89-07-07
C
C ABSTRACT: THIS CODE WRITTEN AT GFDL ....
C   COMPUTES JULIAN DAY AND FRACTION
C   FROM YEAR, MONTH, DAY AND TIME UT...ACCURATE ONLY BETWEEN
C   MARCH 1, 1900 AND FEBRUARY 28, 2100.. BASED ON JULIAN CALENDAR
C   CORRECTED TO CORRESPOND TO GREGORIAN CALENDAR DURING THIS PERIOD.
C
C PROGRAM HISTORY LOG:
C   77-05-06  RAY ORZOL,GFDL
C
C USAGE:    CALL COMPJD(JYR,JMNTH,JDAY,JHR,JMN,JD,FJD)
C   INPUT ARGUMENT LIST:
C     JYR      - YEAR (4 DIGITS)-INTIAL FCST TIME.
C     JMNTH    - MONTH-INITIAL FCST TIME.
C     JDAY     - DAY-INITIAL FCST TIME.
C     JHR      - Z-TIME OF INITIAL FCST TIME.
C     JMN      - MINUTES (ZERO PASSED FROM CALLING PROGRAM).
C   OUTPUT ARGUMENT LIST:
C     JD       - JULIAN DAY.
C     FJD      - FRACTION OF THE JULIAN DAY.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 200.
C   MACHINE:  CYBER 205.
C
C$$$
C    *******************************************************************
C    *                           C O M P J D                           *
C    *    STATEMENT BLOCKED BY RAY ORZOL                               *
C    *******************************************************************
C
                               D I M E N S I O N
     1   NDM(12)
C
                                   D A T A
     1   JDOR/2415019/,
     2   JYR19/1900/
C
                                   D A T A
     1   NDM/0,31,59,90,120,151,181,212,243,273,304,334/
C    *******************************************************************
C     COMPUTES JULIAN DAY AND FRACTION FROM YEAR, MONTH, DAY AND TIME UT
C     ACCURATE ONLY BETWEEN MARCH 1, 1900 AND FEBRUARY 28, 2100
C     BASED ON JULIAN CALENDAR CORRECTED TO CORRESPOND TO GREGORIAN
C        CALENDAR DURING THIS PERIOD
C    JDOR=JD OF DECEMBER 30, 1899 AT 12 HOURS UT
C    *******************************************************************
      JD=JDOR
      JYRM9=JYR-JYR19
      LP=JYRM9/4
      IF(LP.LE.0) GO TO 4
      JD=JD+1461*LP
  4   NY=JYRM9-4*LP
      IC=0
      IF(NY.GT.0) GO TO 5
      IF(JMNTH.GT.2) IC=1
      GO TO 6
  5   JD=JD+365*NY+1
  6   JD=JD+NDM(JMNTH)+JDAY+IC
      IF(JHR.GE.12) GO TO 7
      FJD=.041666667 E 0*FLOAT(JHR)+.00069444444 E 0*FLOAT(JMN)
      RETURN
  7   FJD=0.5E0+.041666667E0*FLOAT(JHR-12)+.00069444444E0*FLOAT(JMN)
C
C     IF(JHR.GE.12) GO TO 7
C     JD=JD-1
C     FJD=.5 E 0+.041666667 E 0*FLOAT(JHR)+.00069444444 E 0*FLOAT(JMN)
C     RETURN
C 7   FJD=.041666667 E 0*FLOAT(JHR-12)+.00069444444 E 0*FLOAT(JMN)
C
      RETURN
      END
      SUBROUTINE UNPGRB(FNGRIB,FNGUPO,NFILES,
     1                  NFILE,SELN,NTOTL,IDIML,JDIML,MGI,LMRUNP,
     2                  IMAXL,JMAXL,IJMAXL,IGLOB,JGLOB,UNDEF,LCRAY,
     3                  LINTRP,IYDIM,JYDIM,YLONW,YLATN,DXLON,DXLAT,
     4                  LREC,NREC,LUPGI,LUPGB,LUUPO,LUPTR)
C
      PARAMETER(TRUTH=60.)
C
      INTEGER SELN(NTOTL)
C
      CHARACTER*128 FNGRIB,FNGUPO
      LOGICAL LCRAY
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      COMMON/COMKEY/ KEYPDS(LKPDS,MTOTL),KEYGDS(LKGDS,MTOTL)
C
      DATA MSK1/32000/,MSK2/4000/
      PARAMETER(MBUF=1024*128*64)
      CHARACTER*1 CBUF(MBUF)
C
      INTEGER KPDS(25,MTOTL),KGDS(22,MTOTL)
C
      PARAMETER(LIJMAXL=360*181)
      PARAMETER(LIJGLOB=LIJMAXL,LIJYDIM=LIJMAXL)
C     LOGICAL LBMS(IJMAXL)
      LOGICAL*1 LBMS(LIJMAXL)
C
C     DIMENSION DATA(IJMAXL),GDATA(IYDIM*JYDIM)
      DIMENSION DATA(LIJMAXL),GDATA(LIJYDIM)
C     DIMENSION GLDATA(IGLOB*JGLOB)
      DIMENSION GLDATA(LIJGLOB)
C
C     DIMENSION WORK(IJMAXL)
      DIMENSION WORK(LIJMAXL)
C
      CHARACTER*80 ASGNSTR,SHLLSTR
C
      LOGICAL LINTRP,LGFILL,LMRUNP,LCHECK
C
      INTEGER*4 LUPGB4,LUPTR4,MSK14,MSK24,MNUM4,MBUF4
      INTEGER*4 NLEN4,NNUM4,IRET4
      INTEGER*4 NDATA4
      REAL*4 DATA4(LIJMAXL)
      INTEGER*4 KPDS4(25,MTOTL),KGDS4(22,MTOTL)
      INTEGER*4 LSKIP4(MTOTL),LGRIB4(MTOTL)
C
      PRINT *,' Unpacking GRIB file ',FNGRIB(1:128)
C
      IF(IJMAXL.GT.LIJMAXL) THEN
        PRINT *,'INCREASE LIJMAXL TO ',IJMAXL
        CALL ABORT
      ENDIF
C
      WRITE(LUPTR,*) 'IJMAXL=',IJMAXL,' IMAXL=',IMAXL,' JMAXL=',JMAXL
      WRITE(LUPTR,*) 'IDIML=',IDIML,' JDIML=',JDIML
      WRITE(LUPTR,*) 'LGLOBL=',(LGLOBL(N),N=1,10)
      WRITE(LUPTR,*) 'IGLOB=',IGLOB,' JGLOB=',JGLOB
C
C  Create subdirectory if unpacked file name contains directory
C
C     IF(.NOT.LMRUNP.OR.(LMRUNP.AND.NFILE.EQ.1)) THEN
C       DO I=1,80
C         IF(FNGUPO(I:I).EQ.'/') THEN
C           DO J=80,1,-1
C             IF(FNGUPO(J:J).EQ.'/') THEN
C               JJ=J
C               GO TO 1
C             ENDIF
C           ENDDO
C   1       CONTINUE
C           SHLLSTR='mkdir -p '//FNGUPO(1:JJ)
C           WRITE(LUPTR,*) 'SHLLSTR=',SHLLSTR
C           NERR=ISHELL(SHLLSTR)
C           IF(NERR.NE.0) THEN
C             WRITE(LUPTR,*) 'Return code for ISHELL mkdir non-zero.'
C           ENDIF
C           GO TO 12
C         ENDIF
C       ENDDO
C  12   CONTINUE
C     ENDIF
C
C  Assign unpacked file
C
      IF(.NOT.LMRUNP.OR.NFILE.EQ.1) THEN
        IF(LCRAY) THEN
          WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUUPO
          CALL ASSIGN(ASGNSTR)
        ENDIF
        OPEN(UNIT=LUUPO,FILE=FNGUPO,FORM='UNFORMATTED',ERR=988)
        GO TO 987
  988   CONTINUE
        WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNGUPO
        PRINT *,'ERROR IN OPENING FILE ',FNGUPO
        CALL ABORT
  987   CONTINUE
        IF(LCRAY) THEN
          WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
        ENDIF
        WRITE(LUPTR,*) 'FILE ',FNGUPO(1:128),' opened. Unit=',LUUPO
      ENDIF
C
C  Assign and Read GRIB file
C
      IF(LCRAY) THEN
        WRITE(ASGNSTR,'(13Hassign -su u:,I2)') LUPGB
        CALL ASSIGN(ASGNSTR)
      ENDIF
      OPEN(UNIT=LUPGB,FILE=FNGRIB,
     1     FORM='UNFORMATTED',STATUS='OLD',ERR=788)
      GO TO 787
  788 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNGRIB
      PRINT *,'ERROR IN OPENING FILE ',FNGRIB
      CALL ABORT
  787 CONTINUE
      IF(LCRAY) THEN
        WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
      ENDIF
      WRITE(LUPTR,*) 'FILE ',FNGRIB(1:128),' opened. Unit=',LUPGB
C
      REWIND LUPGB
      MNUM=0
      LUPGB4=LUPGB
      MSK14=MSK1
      MSK24=MSK2
      MNUM4=MNUM
      MBUF4=MBUF
      CALL GETGIR(LUPGB4,MSK14,MSK24,MNUM4,MBUF4,
     1            CBUF,NLEN4,NNUM4,IRET4)
      NLEN=NLEN4
      NNUM=NNUM4
      IRET=IRET4
C
      IF(IRET.NE.0) THEN
        WRITE(LUPTR,*) 'ERROR.  CBUF length too short in GETGIR'
        PRINT *,'ERROR.  CBUF length too short in GETGIR'
        CALL ABORT
      ENDIF
      IF(NNUM.EQ.0) THEN
        WRITE(LUPTR,*) 'ERROR. Not a grib file. Detected in GETGIR'
        PRINT *,'ERROR.  Not a grib file. Detected in GETGIR'
        CALL ABORT
      ENDIF
      IF(NLEN.EQ.0) THEN
        WRITE(LUPTR,*) 'ERROR. NLEN=0. Detected in GETGIR'
        PRINT *,'ERROR.  NLEN=0.  Detected in GETGIR'
        CALL ABORT
      ENDIF
      IF(NNUM.GT.MTOTL) THEN
        WRITE(LUPTR,*) 'ERROR!!! Number of parameters in the index',
     1           ' buffer exceeded limit of ',MTOTL
        PRINT *,'ERROR!!! Number of parameters in the index',
     1          ' buffer exceeded limit of ',MTOTL
        CALL ABORT
      ENDIF
C
      NLEN4=NLEN
      NNUM4=NNUM
      CALL UNPINDX(CBUF,NLEN4,NNUM4,KPDS4,KGDS4,LSKIP4,LGRIB4,IRET4)
      DO N=1,NNUM
        DO I=1,25
          KPDS(I,N)=KPDS4(I,N)
        ENDDO
        DO I=1,22
          KGDS(I,N)=KGDS4(I,N)
        ENDDO
        LSKIP(N)=LSKIP4(N)
        LGRIB(N)=LGRIB4(N)
      ENDDO
      IRET=IRET4
C
      NTOTL=NNUM
C
      NREC=0
      NUNPRC=0
      REWIND LUPGB
      NAREA=0
      DO NN=1,NTOTL
        N=SELN(NN)
C
C  Skips if N=-99 or <= 0 or LCHOSEN(N)=.FALSE.
C
        IF(N.EQ.-99.OR.N.LE.0.OR..NOT.LCHOSEN(N)) GO TO 555 
C
C  Find the same KPDS as KEYPDS
C
        IF(NFILES.GT.1.AND.NFXTTL.EQ.1.AND.NITOTL.EQ.1) THEN
          LCHECK=.FALSE.
          DO NF=1,NTOTL
            DO II=5,7
              LCHECK=KPDS(II,NF).EQ.KEYPDS(II,N)
              IF(.NOT.LCHECK) GO TO 122
            ENDDO
            GO TO 123
  122       CONTINUE
          ENDDO
          WRITE(LUPTR,*) 'ERROR IN KPDS SEARCH'
          PRINT *,'ERROR IN KPDS SEARCH'
          CALL ABORT
  123     CONTINUE
        ELSE
          NF=N
        ENDIF
C        
        DO I=1,LIJMAXL
    		  LBMS(I)=.TRUE.
        ENDDO
        LUPGB4=LUPGB
        LUPTR4=LUPTR
        CALL RDGB(LUPGB4,LGRIB4(NF),LSKIP4(NF),
     1            KPDS4(1,NF),KGDS4(1,NF),NDATA4,LBMS,DATA4,LUPTR4)
        DO I=1,25
          KPDS(I,NF)=KPDS4(I,NF)
        ENDDO
        DO I=1,22
          KGDS(I,NF)=KGDS4(I,NF)
        ENDDO
        NDATA=NDATA4
        DO I=1,NDATA
          DATA(I)=DATA4(I)
        ENDDO
        DO I=1,NDATA
    		  IF(.NOT.LBMS(I)) DATA(I)=UNDEF
        ENDDO
C 
        WRITE(LUPTR,*) 'N=',N,' NF=',NF
        IF(N.EQ.1) THEN
          WRITE(LUPTR,*) 'IJORDR(1)=',IJORDR(1),' DLAT(1)=',DLAT(1),
     1                   ' DLON(1)=',DLON(1)
          WRITE(LUPTR,*) 'IGLOB*JGLOB=',IGLOB*JGLOB
          WRITE(LUPTR,*) 'NDATA=',NDATA
        ENDIF
C
        IF(NDATA.GT.LIJMAXL) THEN
           WRITE(LUPTR,*) 'NDATA.GT.LIJMAXL'
           WRITE(LUPTR,*) 'ARRAY TOO BIG'
           WRITE(LUPTR,*) 'RECOMPILE grmap'
           CALL ABORT
        ENDIF
C
C  Fill output array with unpacked data
C
        IF(.NOT.IJORDR(NF).OR.
     1    (IJORDR(NF).AND.(DLAT(NF).GT.0..OR.DLON(NF).LT.0.))) THEN
          IF(.NOT.IJORDR(NF)) THEN
            IJ=0
            DO J=1,JDIM(NF)
              DO I=1,IDIM(NF)
                IJ=(J-1)*IDIM(NF)+I
                JI=(I-1)*JDIM(NF)+J
                WORK(IJ)=DATA(JI)
              ENDDO
            ENDDO
          ELSE
            DO J=1,JDIM(NF)
              DO I=1,IDIM(NF)
                IJ=(J-1)*IDIM(NF)+I
                WORK(IJ)=DATA(IJ)
              ENDDO
            ENDDO
          ENDIF
          DO J=1,JDIM(NF)
            DO I=1,IDIM(NF)
              IF(DLAT(NF).GT.0..AND.DLON(NF).GT.0.) THEN
                IJ=IDIM(NF)*JDIM(NF)-IDIM(NF)*J+I
              ELSEIF(DLAT(NF).GT.0..AND.DLON(NF).LT.0.) THEN
                IJ=IDIM(NF)*JDIM(NF)-(J-1)*IDIM(NF)-IDIM(NF)+I-1
              ELSEIF(DLAT(NF).LT.0..AND.DLON(NF).LT.0.) THEN
                IJ=IDIM(NF)*(J-1)+IDIM(NF)-I+1
              ENDIF
              IJO=(J-1)*IDIM(NF)+I
              DATA(IJ)=WORK(IJO)
            ENDDO
          ENDDO
        ENDIF
C
C  Merging areas
C
        LGFILL=.FALSE.
        IF(LAREA) THEN
          IF(NAREA.EQ.0) THEN
            DO NX=1,NVTOTL
              IF(LABBR(NF).EQ.LABBR(IVINDX(NX))) THEN
                NFINL=NATOTL(NX)
                DO IJ=1,IGLOB*JGLOB
                  GLDATA(IJ)=UNDEF
                ENDDO
                GO TO 777
              ENDIF
            ENDDO
            WRITE(LUPTR,*) 'Warning. Rec not found in multi-area list'
            WRITE(LUPTR,*) 'N=',N,' NF=',NF
            GO TO 333
   20       CONTINUE
          ENDIF
C
  777     CONTINUE
          WLONX=WLON(NF)
          IF(WLONX.LT.0.) WLONX=WLONX+360.
          DO J=1,JDIM(NF)
            JJ=NINT((90.-RNLAT(NF))/ABS(DLAT(NF)))+J
            DO I=1,IDIM(NF)
              II=NINT(WLONX/ABS(DLON(NF)))+I
              IF(II.GT.IGLOB) II=II-IGLOB
              GLDATA((JJ-1)*IGLOB+II)=DATA((J-1)*IDIM(NF)+I)
            ENDDO
          ENDDO
          NAREA=NAREA+1
          LGFILL=.TRUE.
          IF(LABBR(NF).NE.LABBR(NF+1)) GO TO 333
          IF(NAREA.LT.NFINL) GO TO 555
        ENDIF
C
C  End merging areas
C
  333   CONTINUE
        NAREA=0
C
C  Write output
C
        IF(LINTRP) THEN
C
C  Interpolation to user specified lat/lon grid
C
          IF(KGDS(1,NF).EQ.1) THEN
            WRITE(LUPTR,*) ' MER2LL'
            IF(LGFILL) THEN
							WRITE(LUPTR,*) 'Error.  Multi-area Mercator grid',
     1                       ' not allowed'
            ELSE
              CALL MER2LL(DATA,IDIM(NF),JDIM(NF),
     1                    ABS(DLON(NF)),ABS(DLAT(NF)),
     2                    PARMAP(1),WLON(NF),RSLAT(1),LGLOBL(NF),
     3                    YLONW,YLATN,DXLON,DXLAT,GDATA,IYDIM,JYDIM,
     4                    UNDEF,LUPTR)
            ENDIF
          ELSEIF(KGDS(1,NF).EQ.4) THEN
            WRITE(LUPTR,*) ' GAU2LL'
            IF(LGFILL) THEN
							WRITE(LUPTR,*) 'Error.  Multi-area Gaussian grid',
     1                       ' not allowed'
							PRINT *,'Error.  Multi-area Gaussian grid',
     1                ' not allowed'
              CALL ABORT
            ELSE
              CALL GAU2LL(DATA,IDIM(NF),JDIM(NF),
     1                    YLONW,YLATN,DXLON,DXLAT,GDATA,IYDIM,JYDIM,
     2                    UNDEF,LUPTR)
            ENDIF
          ELSEIF(KGDS(1,NF).EQ.5) THEN
            WRITE(LUPTR,*) ' STR2LL'
            IF(LGFILL) THEN
							WRITE(LUPTR,*) 'Error.  Multi-area Polar stereo grid',
     1                       ' not allowed'
            ELSE
              CALL GETIJNP(TRUTH,PARMAP(1),ABS(DLON(NF)),ABS(DLAT(NF)),
     1                     RSLAT(1),WLON(1),RLFTGRD,BOTMGRD)
              DXLON=360./FLOAT(IDIML)
              DXLAT=DXLON
              CALL STR2LL(DATA,IDIM(NF),JDIM(NF),
     1                    TRUTH,PARMAP(1),ABS(DLON(NF)),ABS(DLAT(NF)),
     2                    BOTMGRD,RLFTGRD,
     3                    YLONW,YLATN,DXLON,DXLAT,GDATA,IYDIM,JYDIM,
     4                    UNDEF,LUPTR)
            ENDIF
          ELSE
            IF(LGFILL) THEN
              WRITE(LUPTR,*) ' LL2LL of GLDATA'
              CALL LL2LL(GLDATA,IGLOB,JGLOB,ABS(DLON(NF)),ABS(DLAT(NF)),
     1                   WLON(NF),RNLAT(NF),LGLOBL(NF),
     2                   YLONW,YLATN,DXLON,DXLAT,
     3                   GDATA,IYDIM,JYDIM,UNDEF,LUPTR)
            ELSE
              WRITE(LUPTR,*) ' LL2LL of DATA'
              CALL LL2LL(DATA,IDIM(NF),JDIM(NF),ABS(DLON(NF)),
     1                   ABS(DLAT(NF)),WLON(NF),RNLAT(NF),LGLOBL(NF),
     2                   YLONW,YLATN,DXLON,DXLAT,
     3                   GDATA,IYDIM,JYDIM,UNDEF,LUPTR)
            ENDIF
          ENDIF
C
C  End interploation
C
          NREC=NREC+1
          LREC=IYDIM*JYDIM
          NUNPRC=NUNPRC+1
          WRITE(LUPTR,*) ' a)Unpacked GDATA written.'
          WRITE(LUPTR,*) NUNPRC,')',
     1                     LABEL(1,NF)(1:LENLA(1,NF)),
     2                     LABEL(2,NF)(1:LENLA(2,NF)),' ',
     3                     LABEL(3,NF)(1:LENLA(3,NF)),' ',
     4                     LABEL(4,NF)(1:LENLA(4,NF)),' ',
     5                     LABEL(5,NF)(1:LENLA(5,NF))
          WRITE(LUPTR,*) ' GDATA info. LGRIB=',LGRIB(NF),
     1                   ' LSKIP=',LSKIP(NF),' NDATA=',NDATA
          WRITE(LUPTR,*) ' Len=IYDIM*JYDIM=',IYDIM*JYDIM
          DO IJ=1,IYDIM*JYDIM
            DATA4(IJ)=GDATA(IJ)
          ENDDO
          WRITE(LUUPO) (DATA4(IJ),IJ=1,IYDIM*JYDIM)
          CALL MAXMIN(DATA,IYDIM*JYDIM,LUPTR)
        ELSE
C
C  Non interpolating case
C
C  Case of Merged areas
C
          IF(LGFILL) THEN
            DO IJ=1,IGLOB*JGLOB
              DATA4(IJ)=GLDATA(IJ)
            ENDDO
            WRITE(LUUPO) (DATA4(IJ),IJ=1,IGLOB*JGLOB)
            NUNPRC=NUNPRC+1
            NREC=NREC+1
            LREC=IGLOB*JGLOB
            WRITE(LUPTR,*) ' b) Unpacked GLDATA written'
            WRITE(LUPTR,*) NUNPRC,')',
     1                     LABEL(1,NF)(1:LENLA(1,NF)),
     2                     LABEL(2,NF)(1:LENLA(2,NF)),' ',
     3                     LABEL(3,NF)(1:LENLA(3,NF)),' ',
     4                     LABEL(4,NF)(1:LENLA(4,NF)),' ',
     5                     LABEL(5,NF)(1:LENLA(5,NF))
            WRITE(LUPTR,*) ' Entire GLDATA info. LGRIB=',LGRIB(NF),
     1                     ' LSKIP=',LSKIP(NF),' NDATA=',NDATA
            WRITE(LUPTR,*) ' Len=IGLOB*JGLOB=',IGLOB*JGLOB
            CALL MAXMIN(GLDATA,IGLOB*JGLOB,LUPTR)
          ELSE
C
C Normal global case
C
            DO IJ=1,NDATA
              DATA4(IJ)=DATA(IJ)
            ENDDO
            WRITE(LUUPO) (DATA4(IJ),IJ=1,NDATA)
            NUNPRC=NUNPRC+1
            NREC=NREC+1
            LREC=NDATA
            WRITE(LUPTR,*) ' c) Unpacked DATA written'
            WRITE(LUPTR,*) NUNPRC,')',
     1                     LABEL(1,NF)(1:LENLA(1,NF)),
     2                     LABEL(2,NF)(1:LENLA(2,NF)),' ',
     3                     LABEL(3,NF)(1:LENLA(3,NF)),' ',
     4                     LABEL(4,NF)(1:LENLA(4,NF)),' ',
     5                     LABEL(5,NF)(1:LENLA(5,NF))
            WRITE(LUPTR,*) ' Entire DATA info. LGRIB=',LGRIB(NF),
     1                     ' LSKIP=',LSKIP(NF),' NDATA=',NDATA
            CALL MAXMIN(DATA,NDATA,LUPTR)
          ENDIF
        ENDIF
  555   CONTINUE
      ENDDO
C
      WRITE(LUPTR,*) 'Number of unpacked records=',NUNPRC
      WRITE(LUPTR,*) 'Number of records written=',NREC
      WRITE(LUPTR,*) 'REC LEN=',LREC
C
      WRITE(LUPTR,*) ' Unpacked GRIB FILE CREATED'
C
      CLOSE (UNIT=LUPGB)
      RETURN
      END
      SUBROUTINE GETZNL(FNZNLI,IMAXL,JMAXL,IJMAXL,LEVMDL,
     1                  NFILE,IFKEY,NTOTL,SELN,LY2000,LUZNLI,LUPTR)
C
      REAL*8 FHOUR,FSTEP
C
      CHARACTER*128 FNZNLI
      CHARACTER*80 ASGNSTR
C
      CHARACTER*1 IFKEY
C
      LOGICAL LY2000
C
      INTEGER SELN(*)
C
      PARAMETER(NLB=6,NST=6,NLV=28+1,NVRM=23,NVRS=32)
C
      DIMENSION IDATE(4)
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      COMMON/COMKEY/ KEYPDS(LKPDS,MTOTL),KEYGDS(LKGDS,MTOTL)
C
      CHARACTER*64 DATES,FTIME
C
      CHARACTER*5 MONTH(12)
C
      DATA MONTH/' JAN ',' FEB ',' MAR ',' APR ',' MAY ',' JUN ',
     1           ' JUL ',' AUG ',' SEP ',' OCT ',' NOV ',' DEC '/
C
      CHARACTER*46 VARSM(NVRM)
      DIMENSION LENVM(NVRM)
      DATA (VARSM(I),LENVM(I),I=1,NVRM)/
     &           'u component of wind (m/s)',25,
     &           'v component of wind (m/s)',25,
     &           'virtual temperature (K)',23,
     &           'specific humidity (gm/gm)',25,
     &           'squared vorticity (1/s**2)',26,
     &           'squared divergence (1/s**2)',27,
     &           'pressure vertical velocity (Pa/s)',33,
     &           'temperature (K)',15,
     &           'relative humidity (%)',21,
     &           'kinetic enerty (m**2/s**2)',26,
     &           'convective heating (K/s)',24,
     &           'large scale heating (K/s)',25,
     &           'shallow convection heating (K/s)',32,
     &           'vertical diffusion heating (K/s)',32,
     &           'convective moistning (gm/gm/s)',30,
     &           'shallow convection moistning (gm/gm/s)',38,
     &           'vertical diffusion moistning (gm/gm/s)',38,
     &           'zonal accel by vertical diffusion (m/s**2)',42,
     &           'meridional accel by vertical diff (m/s**2)',42,
     &           'short wave radiation heating (K/s)',34,
     &           'long wave radiation heating (K/s)',33,
     &           'cloud cover (fraction)',22,
     &           'convective cloud cover (fraction)',33/
C
      CHARACTER*46 VARSS(NVRS)
      DIMENSION LENVS(NVRS)
      DATA (VARSS(I),LENVS(I),I=1,NVRS)/
     &           'total precipitation (Kg/m**2)',29,
     &           'convective precipitation (Kg/m**2)',34,
     &           'sensible heat flux (w/m**2)',27,
     &           'latent heat flux (w/m**2)',25,
     &           'zonal stress (dyn/m**2)',23,
     &           'meridional stress (dyn/m**2)',28,
     &           'rain area coverage (%)',22,
     &           'convective rain area coverage (%)',33,
     &           'surface pressure (hPa)',22,
     &           'surface skin temperature (K)',28,
     &           'soil wetness (cm)',17,
     &           'snow depth (m)',14,
     &           '10cm deep soil temperature (K)',30,
     &           '50cm deep soil temperature (K)',30,
     &           '500cm deep soil temperature (K)',31,
     &           'surface net short wave flux (W/m**2)',36,
     &           'surface net long wave flux (W/m**2)',35,
     &           'relative humidty at the lowest model level (%)',46,
     &           'virtual temp at the lowest model level (K)',42,
     &           'temperature at the lowest model level (K)',41,
     &           'specific humidty at the lowest model level (K)',46,
     &           'surface roughness (m)',21,
     &           'land sea seaice mask (int)',26,
     &           'zonal accel by gravity wave drag (m/s**2)',41,
     &           'meridional accel by gravity wave drag (m/s**2)',46,
     &           'surface torque (gm/m**2/s**2)',29,
     &           'gravity wave drag torque (gm/m**2/s**2)',39,
     &           'mountain torque (gm/m**2/s**2)',30,
     &           'total angular momentum (m**2/s)',31,
     &           'planetary angular momentum (m**2/s)',35,
     &           'potential evaporation',21,
     &           'cloud work function',19/
C
      CHARACTER*8 ABBRM(NVRM)
      DATA ABBRM/ 'u',
     &            'v',
     &            'tv',
     &            'q',
     &            'sqvot',
     &            'sqdiv',
     &            'omg',
     &            't',
     &            'rh',
     &            'ke',
     &            'hcnv',
     &            'hlrg',
     &            'hshl',
     &            'hvdf',
     &            'mcnv',
     &            'mshl',
     &            'mvdf',
     &            'uvdf',
     &            'vvdf',
     &            'hsw',
     &            'hlw',
     &            'cld',
     &            'ccld'/
C
      CHARACTER*8 ABBRS(NVRS)
      DATA ABBRS/ 'prcp',
     &            'prcpc',
     &            'hflx',
     &            'qflx',
     &            'ustrs',
     &            'vstrs',
     &            'rac',
     &            'crac',
     &            'ps',
     &            'tskin',
     &            'sw',
     &            'snow',
     &            'tg1',
     &            'tg2',
     &            'tg3',
     &            'sfxs',
     &            'lfxs',
     &            'rhs',
     &            'tvs',
     &            'ts',
     &            'qs',
     &            'z0',
     &            'msk',
     &            'ugwd',
     &            'vgwd',
     &            'strq',
     &            'gtrq',
     &            'mtrq',
     &            'tam',
     &            'pam',
     &            'pev',
     &            'cwk'/
C
      CHARACTER*10 SFCCHR(NST)
      CHARACTER*1  ABSFCH(NST)
      DATA SFCCHR/'globe     ','bare_land ','snow_land ','bare_ice  ',
     1            'snow_ice  ','open_sea  '/
      DATA ABSFCH/'g','l','s','i','c','o'/
C
      DO N=1,MTOTL
        LABBR(N)='        '
      ENDDO
C
      NFLX=MIN(NFILE,2)
      IMAXL=1
      JMAXL=NLB
      IJMAXL=NLB
C
      WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2)') LUZNLI
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUZNLI,FILE=FNZNLI,FORM='UNFORMATTED',ERR=988)
      GO TO 987
  988 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNZNLI
      PRINT *,'ERROR IN OPENING FILE ',FNZNLI
      CALL ABORT
  987 CONTINUE
      WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
      WRITE(LUPTR,*) 'FILE ',FNZNLI(1:128),' opened. Unit=',LUZNLI
C
      READ(LUZNLI)
      READ(LUZNLI) FHOUR,FSTEP,IDATE
      WRITE(LUPTR,*) 'FHOUR,FSTEP=',FHOUR,FSTEP,' IDATE=',IDATE
C
      IY=IDATE(4)
      IM=IDATE(2)
      ID=IDATE(3)
      IH=IDATE(1)
      IMIN=0
      KEYPDS(8,1)=IY
      KEYPDS(9,1)=IM
      KEYPDS(10,1)=ID
      KEYPDS(11,1)=IH
      KEYPDS(12,1)=IMIN
C
      WRITE(LUPTR,*) 'IY,IM,ID,IH,IMIN=',IY,IM,ID,IH,IMIN
C
      CALL COMPJD(IY,IM,ID,IH,IMIN,JD,FJD)
C
      WRITE(LUPTR,*) 'JD,FJD=',JD,FJD
C
      IF(IM.LT.0.OR.IM.GT.12) THEN
        WRITE(LUPTR,*) 'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
        PRINT *,'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
        CALL ABORT
      ENDIF
      IF(ID.LT.0.OR.ID.GT.31) THEN
        WRITE(LUPTR,*) 'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
        PRINT *,'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
        CALL ABORT
      ENDIF
      IF(IH.LT.0.OR.IH.GT.24) THEN
        WRITE(LUPTR,*) 'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
        PRINT *,'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
        CALL ABORT
      ENDIF
      IF(IMIN.LT.0.OR.IMIN.GT.60) THEN
        WRITE(LUPTR,*) 'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
        PRINT *,'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
        CALL ABORT
      ENDIF
C
        LY2000=.TRUE.
      IF(IMIN.GT.0.AND.IMIN.LT.10) THEN
        IF(IH.GE.10) THEN
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ELSE
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ENDIF
      ELSEIF(IMIN.GE.10) THEN
        IF(IH.GE.10) THEN
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ELSE
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I2)')
     1              IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I4)')
     1              IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ENDIF
      ELSEIF(IH.LT.10) THEN
        IF(.NOT.LY2000) THEN
          WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I2)')
     1             IH,ID,MONTH(IM),IY
          LENDA=14
        ELSE
          WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I4)')
     1             IH,ID,MONTH(IM),IY
          LENDA=16
        ENDIF
      ELSE
        IF(.NOT.LY2000) THEN
          WRITE(DATES,'(I2,1HZ,I3,A5,1X,I2)')
     1             IH,ID,MONTH(IM),IY
          LENDA=14
        ELSE
          WRITE(DATES,'(I2,1HZ,I3,A5,1X,I4)')
     1             IH,ID,MONTH(IM),IY
          LENDA=16
        ENDIF
      ENDIF
C
      WRITE(FTIME,'(3HFT=,F5.1)') FHOUR
      LENFT=8
C
      N=0
      NVTOTL=0
C
      DO MVR=1,NVRM
        NVTOTL=NVTOTL+1
        DO K=1,LEVMDL+1
          N=N+1
          LABEL(1,N)=VARSM(MVR)
          LENLA(1,N)=LENVM(MVR)
          LABEL(2,N)='sigma level'
          LENLA(2,N)=11
          WRITE(LABEL(3,N),'(I2)') K
          LENLA(3,N)=5
          LABBR(N)=ABBRM(MVR)
          IRLEV(N)=K
          LVABBR(N)='sig'
        ENDDO
      ENDDO
C
      DO MVR=1,NVRS
        DO K=1,NST
          NVTOTL=NVTOTL+1
          N=N+1
          LABEL(1,N)=VARSS(MVR)(1:LENVS(MVR))//' over '//SFCCHR(K)
          LENLA(1,N)=LENVS(MVR)+16
          LABEL(2,N)='surface'
          LENLA(2,N)=7
          LABEL(3,N)=' '
          LENLA(3,N)=1
          J=0
          DO I=1,8
            IF(ABBRS(MVR)(I:I).NE.' ') THEN
              J=J+1
              LABBR(N)(J:J)=ABBRS(MVR)(I:I)
            ENDIF
            LABBR(N)(J+1:8)=ABSFCH(K)
          ENDDO
          LVABBR(N)='sfc'
        ENDDO
      ENDDO
C
      NTOTL=N
      NVTOTL=NVTOTL
      NPTOTL=0
      IPINDX(1)=1
C
      DO N=1,NTOTL
        LABEL(4,N)(1:LENDA)=DATES(1:LENDA)
        LENLA(4,N)=LENDA
        LABEL(5,N)(1:LENDA)=FTIME(1:LENFT)
        LENLA(5,N)=LENFT
      ENDDO
C
      DO N=1,NTOTL
        RSLAT(N)=-90.
        RNLAT(N)=90.
        WLON(N)=0.
        ELON(N)=360.
        IDIM(N)=1
        JDIM(N)=NLB
        DLON(N)=360.
        DLAT(N)=30.
        LGLOBL(N)=.TRUE.
        IJORDR(N)=.TRUE.
        FJHOUR(N,NFLX)=FHOUR
        RJLDAY(N,NFLX)=JD+FJD
      ENDDO
C
      CALL FLSRCH(NTOTL,LUPTR)
C
      DO N=1,NTOTL
        SELN(N)=N
      ENDDO
C
      RETURN
      END
      SUBROUTINE GETKEN(FNKENI,IMAXL,JMAXL,IJMAXL,LEVMDL,
     1                  ITNUM,KENPTS,KENLEN,
     2                  NFILE,IFKEY,NTOTL,SELN,LY2000,LUKENI,LUPTR)
C
      REAL*8 FHOUR
C
      CHARACTER*128 FNKENI
      CHARACTER*80 ASGNSTR
C
      CHARACTER*1 IFKEY
C
      LOGICAL LY2000
C
      INTEGER SELN(*)
C
      DIMENSION IDATE(4)
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      COMMON/COMKEY/ KEYPDS(LKPDS,MTOTL),KEYGDS(LKGDS,MTOTL)
C
      CHARACTER*64 DATES,FTIME
C
      CHARACTER*5 MONTH(12)
C
      DATA MONTH/' JAN ',' FEB ',' MAR ',' APR ',' MAY ',' JUN ',
     1           ' JUL ',' AUG ',' SEP ',' OCT ',' NOV ',' DEC '/
C
      PARAMETER(NVRM=8,NVRS=80)
C
      CHARACTER*46 VARSM(NVRM)
      DIMENSION LENVM(NVRM)
      DATA (VARSM(I),LENVM(I),I=1,NVRM)/
     &           'u component of wind (m/s)',25,
     &           'v component of wind (m/s)',25,
     &           'virtual temperature (K)',23,
     &           'specific humidity (gm/gm)',25,
     &           'short wave radiation heating (K/s)',34,
     &           'long wave radiation heating (K/s)',33,
     &           'vertical velocity (mb/s)',24,
     &           'clouds (fraction)',17/
C
      CHARACTER*46 VARSS(NVRS)
      DIMENSION LENVS(NVRS)
      DATA (VARSS(I),LENVS(I),I=1,29)/
     &           'longitude (deg)',15,
     &           'latitude (deg)',14,
     &           'land sea mask (int)',19,
     &           'surface pressure (hPa)',22,
     &           'surface skin temperature (K)',28,
     &           '10cm deep soil temperature (K)',30,
     &           '50cm deep soil temperature (K)',30,
     &           '500cm deep soil temperature (K)',31,
     &           'soil wetness (cm)',17,
     &           'snow depth (m)',14,
     &           'convective precipitation (m)',28,
     &           'large scale precipitation (m)',29,
     &           'radsl',5,
     &           'downward shortwave flux at surface (W/m**2)',43,
     &           'surface saturation specific humidity (gm/gm)',44,
     &           'plant resistance',16,
     &           'latent heat flux (W/m**2)',25,
     &           'sensible heat flux (W/m**2)',27,
     &           'Ground heat flux (W/m**2)',25,
     &           'zonal stress (dyn/m**2)',23,
     &           'meridional stress (dyn/m**2)',19,
     &           'surface roughness (m)',21,
     &           'drag coeff of momentum',22,
     &           'drag coeff of heat mois',23,
     &           'high cloud amount (%)',21,
     &           'middle cloud amount (%)',23,
     &           'low cloud amount (%)',20,
     &           'zonal stress by gravity wave drag',29,
     &           'meridional stress by gravity wave drag (dyn/m**2)',40/
      DATA (VARSS(I),I=30,40)/11*'undefined'/
      DATA (LENVS(I),I=30,40)/11*9/
      DATA (VARSS(I),LENVS(I),I=41,58)/
     &           'ird',3,
     &           'radlat',6,
     &           'land sea mask (int)',19,
     &           'surface temperature (K)',23,
     &           'surface downward shortwave flux (W/m**2)',40,
     &           'surface upward shortwave flux (W/m**2)',38,
     &           'surface downward longwave flux (W/m**2)',39,
     &           'top downward shortwave flux (W/m**2)',36,
     &           'top upward shortwave flux (W/m**2)',34,
     &           'top upward longwave flux (W/m**2)',33,
     &           'high cloud top (hPa)',20,
     &           'middle cloud top (hPa)',22,
     &           'low cloud top (hPa)',19,
     &           'high cloud bottom (hPa)',23,
     &           'middle cloud bottom (hPa)',25,
     &           'low cloud bottom (hPa)',22,
     &           'cosine of sun zenith angle (deg)',32,
     &           'sun declination angle (deg)',27/
      DATA (VARSS(I),I=59,NVRS)/22*'undefined'/
      DATA (LENVS(I),I=59,NVRS)/22*9/
C
      CHARACTER*8 ABBRM(NVRM)
      DATA ABBRM/ 'u',
     &            'v',
     &            'tv',
     &            'hsw',
     &            'hlw',
     &            'q',
     &            'omg',
     &            'cld'/
C
      CHARACTER*8 ABBRS(NVRS)
      DATA ABBRS/ 'ipnt',
     &            'jpnt',
     &            'msk',
     &            'ps',
     &            'ts',
     &            'tg1',
     &            'tg2',
     &            'tg3',
     &            'soil',
     &            'snow',
     &            'cprc',
     &            'lprc',
     &            'rdsl',
     &            'dsfs',
     &            'qss',
     &            'plr',
     &            'lhfx',
     &            'shfx',
     &            'sevp',
     &            'ustr',
     &            'vstr',
     &            'z0',
     &            'cd',
     &            'cdq',
     &            'clh',
     &            'clm',
     &            'cll',
     &            'ugrw',
     &            'vgrw',
     &            'dummy1',
     &            'dummy2',
     &            'dummy3',
     &            'dummy4',
     &            'dummy5',
     &            'dummy6',
     &            'dummy7',
     &            'dummy8',
     &            'dummy9',
     &            'dummy10',
     &            'dummy11',
     &            'radird',
     &            'radlat',
     &            'lsmsk',
     &            'tsfc',
     &            'ssdn',
     &            'ssup',
     &            'sldn',
     &            'tsdn',
     &            'tsup',
     &            'tlup',
     &            'clth',
     &            'cltm',
     &            'cltl',
     &            'clbh',
     &            'clbm',
     &            'clbl',
     &            'cosz',
     &            'decl',
     &            'dummy12',
     &            'dummy13',
     &            'dummy14',
     &            'dummy15',
     &            'dummy16',
     &            'dummy17',
     &            'dummy18',
     &            'dummy19',
     &            'dummy20',
     &            'dummy21',
     &            'dummy22',
     &            'dummy23',
     &            'dummy24',
     &            'dummy25',
     &            'dummy26',
     &            'dummy27',
     &            'dummy28',
     &            'dummy29',
     &            'dummy30',
     &            'dummy31',
     &            'dummy32',
     &            'dummy33'/
C
      WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2)') LUKENI
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUKENI,FILE=FNKENI,FORM='UNFORMATTED',ERR=988)
      GO TO 987
  988 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNKENI
      PRINT *,'ERROR IN OPENING FILE ',FNKENI
      CALL ABORT
  987 CONTINUE
      WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
      WRITE(LUPTR,*) 'FILE ',FNKENI(1:128),' opened. Unit=',LUKENI
C
      READ(LUKENI)
      READ(LUKENI) FHOUR, IDATE
      READ(LUKENI) NVRKEN,NPTKEN,NSTKEN,NPOINTS,ITNUM
      WRITE(LUPTR,*) 'FHOUR,FSTEP=',' IDATE=',IDATE
      WRITE(LUPTR,*) 'NVRKEN=',NVRKEN,' NPTKEN=',NPTKEN,
     1       ' NSTKEN=',NSTKEN,' NPOINTS=',NPOINTS,' ITNUM=',ITNUM
      KENLEN=ITNUM*NVRKEN
      KENPTS=NPOINTS
      WRITE(LUPTR,*) 'KENPTS=',KENPTS,' KENLEN=',KENLEN
C
      DO N=1,MTOTL
        LABBR(N)='        '
      ENDDO
C
      NFLX=MIN(NFILE,2)
      IMAXL=KENPTS
      JMAXL=1
      IJMAXL=KENPTS
C
C  Initial hour
C
      IY=IDATE(4)
      IM=IDATE(2)
      ID=IDATE(3)
      IH=IDATE(1)
      IMIN=0
      KEYPDS(8,1)=IY
      KEYPDS(9,1)=IM
      KEYPDS(10,1)=ID
      KEYPDS(11,1)=IH
      KEYPDS(12,1)=IMIN
C
      WRITE(LUPTR,*) 'IY,IM,ID,IH,IMIN=',IY,IM,ID,IH,IMIN
C
      CALL COMPJD(IY,IM,ID,IH,IMIN,JD,FJD)
C
      WRITE(LUPTR,*) 'JD,FJD=',JD,FJD
C
      IF(IM.LT.0.OR.IM.GT.12) THEN
        WRITE(LUPTR,*) 'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
        PRINT *,'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
        CALL ABORT
      ENDIF
      IF(ID.LT.0.OR.ID.GT.31) THEN
        WRITE(LUPTR,*) 'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
        PRINT *,'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
        CALL ABORT
      ENDIF
      IF(IH.LT.0.OR.IH.GT.24) THEN
        WRITE(LUPTR,*) 'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
        PRINT *,'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
        CALL ABORT
      ENDIF
      IF(IMIN.LT.0.OR.IMIN.GT.60) THEN
        WRITE(LUPTR,*) 'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
        PRINT *,'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
        CALL ABORT
      ENDIF
C
        LY2000=.TRUE.
      IF(IMIN.GT.0.AND.IMIN.LT.10) THEN
        IF(IH.GE.10) THEN
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ELSE
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ENDIF
      ELSEIF(IMIN.GE.10) THEN
        IF(IH.GE.10) THEN
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ELSE
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I2)')
     1              IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I4)')
     1              IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ENDIF
      ELSEIF(IH.LT.10) THEN
        IF(.NOT.LY2000) THEN
          WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I2)')
     1             IH,ID,MONTH(IM),IY
          LENDA=14
        ELSE
          WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I4)')
     1             IH,ID,MONTH(IM),IY
          LENDA=16
        ENDIF
      ELSE
        IF(.NOT.LY2000) THEN
          WRITE(DATES,'(I2,1HZ,I3,A5,1X,I2)')
     1             IH,ID,MONTH(IM),IY
          LENDA=14
        ELSE
          WRITE(DATES,'(I2,1HZ,I3,A5,1X,I4)')
     1             IH,ID,MONTH(IM),IY
          LENDA=16
        ENDIF
      ENDIF
C
C  Forecast hour
C
      WRITE(FTIME,'(3HFT=,F5.1)') FHOUR
      LENFT=8
C
      N=0
      NVTOTL=0
C
      DO MVR=1,NVRM
        NVTOTL=NVTOTL+1
        DO K=1,LEVMDL
          N=N+1
          LABEL(1,N)=VARSM(MVR)
          LENLA(1,N)=LENVM(MVR)
          LABEL(2,N)='sigma level'
          LENLA(2,N)=11
          WRITE(LABEL(3,N),'(I2)') K
          LENLA(3,N)=5
          LABBR(N)=ABBRM(MVR)
          IRLEV(N)=K
          LVABBR(N)='sig'
        ENDDO
      ENDDO
C
      DO MVR=1,NVRS
        NVTOTL=NVTOTL+1
        N=N+1
        LABEL(1,N)=VARSS(MVR)(1:LENVS(MVR))
        LENLA(1,N)=LENVS(MVR)
        LABEL(2,N)='surface'
        LENLA(2,N)=7
        LABEL(3,N)=' '
        LENLA(3,N)=1
        J=0
        DO I=1,8
          IF(ABBRS(MVR)(I:I).NE.' ') THEN
            J=J+1
            LABBR(N)(J:J)=ABBRS(MVR)(I:I)
          ENDIF
        ENDDO
        LVABBR(N)='sfc'
      ENDDO
C
      NTOTL=N
      NVTOTL=NVTOTL
      NPTOTL=0
      IPINDX(1)=1
C
      DO N=1,NTOTL
        LABEL(4,N)(1:LENDA)=DATES(1:LENDA)
        LENLA(4,N)=LENDA
        LABEL(5,N)(1:LENDA)=FTIME(1:LENFT)
        LENLA(5,N)=LENFT
      ENDDO
C
      DO N=1,NTOTL
        RSLAT(N)=1.
        RNLAT(N)=90.
        WLON(N)=0.
        ELON(N)=360.
        IDIM(N)=KENPTS
        JDIM(N)=1
        DLON(N)=1
        DLAT(N)=1.
        LGLOBL(N)=.TRUE.
        IJORDR(N)=.TRUE.
        FJHOUR(N,NFLX)=FHOUR
        RJLDAY(N,NFLX)=JD+FJD
      ENDDO
C
      CALL FLSRCH(NTOTL,LUPTR)
C
      DO N=1,NTOTL
        SELN(N)=N
      ENDDO
C
      RETURN
      END
      SUBROUTINE GETBGS(FNBGSI,IBGES,JBGES,IMAXL,JMAXL,IJMAXL,
     1                  NFILE,IFKEY,NTOTL,SELN,LY2000,LUBGSI,LUPTR)
C
      REAL*8 FHOUR
C
      CHARACTER*128 FNBGSI
      CHARACTER*80 ASGNSTR
C
      CHARACTER*1 IFKEY
C
      LOGICAL LY2000
C
      INTEGER SELN(*)
C
      DIMENSION IDATE(4)
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      COMMON/COMKEY/ KEYPDS(LKPDS,MTOTL),KEYGDS(LKGDS,MTOTL)
C
      CHARACTER*64 DATES,FTIME
C
      CHARACTER*5 MONTH(12)
C
      DATA MONTH/' JAN ',' FEB ',' MAR ',' APR ',' MAY ',' JUN ',
     1           ' JUL ',' AUG ',' SEP ',' OCT ',' NOV ',' DEC '/
C
      PARAMETER(NVRM=17)
      CHARACTER*46 VARSM(NVRM)
      DIMENSION LENVM(NVRM)
      DATA (VARSM(I),LENVM(I),I=1,NVRM)/
     &           'earth surface temperature (K)',29,
     &           'soil moisture level 1 (?)',24,
     &           'soil moisture level 2 (?)',24,
     &           'snow depth (m)',14,
     &           'soil temperature level 1 (K)',28,
     &           'soil temperature level 2 (K)',28,
     &           'soil temperature level 3 (K)',28,
     &           'surface roughness length (m)',28,
     &           'convective cloud cover (percent)',32,
     &           'convective cloud bottom height (sigma)',38,
     &           'convective cloud top height (sigma)',35,
     &           'albedo (fraction)',17,
     &           'snow/ice/land mask',18,
     &           'minimum stomatal resistance (?)',31,
     &           'canopy water content (?)',24,
     &           'ratio of 10m and k=1 winds (fraction)',37,
     &           'vegetation type',15/
C
      CHARACTER*8 ABBRM(NVRM)
      DATA ABBRM/ 'skint',
     &            'wet1',
     &            'wet2',
     &            'snow',
     &            'tg1',
     &            'tg2',
     &            'tg3',
     &            'zor',
     &            'cv',
     &            'cvb',
     &            'cvt',
     &            'alb',
     &            'mask',
     &            'plantr',
     &            'cwc',
     &            'f10m',
     &            'veg'/
C
      DO N=1,MTOTL
        LABBR(N)='        '
      ENDDO
C
      NFLX=MIN(NFILE,2)
      IMAXL=IBGES
      JMAXL=JBGES
      IJMAXL=IMAXL*JMAXL
C
      WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2)') LUBGSI
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUBGSI,FILE=FNBGSI,FORM='UNFORMATTED',ERR=988)
      GO TO 987
  988 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNBGSI
      PRINT *,'ERROR IN OPENING FILE ',FNBGSI
      CALL ABORT
  987 CONTINUE
      WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
      WRITE(LUPTR,*) 'FILE ',FNBGSI(1:128),' opened. Unit=',LUBGSI
C
      READ(LUBGSI)
      READ(LUBGSI) FHOUR,IDATE
      WRITE(LUPTR,*) 'FHOUR=',FHOUR,' IDATE=',IDATE
C
      IY=IDATE(4)
      IM=IDATE(2)
      ID=IDATE(3)
      IH=IDATE(1)
      IMIN=0
      KEYPDS(8,1)=IY
      KEYPDS(9,1)=IM
      KEYPDS(10,1)=ID
      KEYPDS(11,1)=IH
      KEYPDS(12,1)=IMIN
      KEYGDS(1,1)=4
C
      WRITE(LUPTR,*) 'IY,IM,ID,IH,IMIN=',IY,IM,ID,IH,IMIN
C
      CALL COMPJD(IY,IM,ID,IH,IMIN,JD,FJD)
C
      WRITE(LUPTR,*) 'JD,FJD=',JD,FJD
C
      IF(IM.LT.0.OR.IM.GT.12) THEN
        WRITE(LUPTR,*) 'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
        PRINT *,'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
        CALL ABORT
      ENDIF
      IF(ID.LT.0.OR.ID.GT.31) THEN
        WRITE(LUPTR,*) 'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
        PRINT *,'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
        CALL ABORT
      ENDIF
      IF(IH.LT.0.OR.IH.GT.24) THEN
        WRITE(LUPTR,*) 'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
        PRINT *,'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
        CALL ABORT
      ENDIF
      IF(IMIN.LT.0.OR.IMIN.GT.60) THEN
        WRITE(LUPTR,*) 'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
        PRINT *,'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
        CALL ABORT
      ENDIF
C
        LY2000=.TRUE.
      IF(IMIN.GT.0.AND.IMIN.LT.10) THEN
        IF(IH.GE.10) THEN
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ELSE
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ENDIF
      ELSEIF(IMIN.GE.10) THEN
        IF(IH.GE.10) THEN
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ELSE
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I2)')
     1              IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I4)')
     1              IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ENDIF
      ELSEIF(IH.LT.10) THEN
        IF(.NOT.LY2000) THEN
          WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I2)')
     1             IH,ID,MONTH(IM),IY
          LENDA=14
        ELSE
          WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I4)')
     1             IH,ID,MONTH(IM),IY
          LENDA=16
        ENDIF
      ELSE
        IF(.NOT.LY2000) THEN
          WRITE(DATES,'(I2,1HZ,I3,A5,1X,I2)')
     1             IH,ID,MONTH(IM),IY
          LENDA=14
        ELSE
          WRITE(DATES,'(I2,1HZ,I3,A5,1X,I4)')
     1             IH,ID,MONTH(IM),IY
          LENDA=16
        ENDIF
      ENDIF
C
      WRITE(FTIME,'(3HFT=,F5.1)') FHOUR
      LENFT=8
C
      N=0
      NVTOTL=0
C
      DO MVR=1,NVRM
        NVTOTL=NVTOTL+1
        N=N+1
        LABEL(1,N)=VARSM(MVR)
        LENLA(1,N)=LENVM(MVR)
        LABEL(2,N)='surface'
        LENLA(2,N)=7
        LABEL(3,N)=' '
        LENLA(3,N)=1
        LABBR(  N)=ABBRM(MVR)
        IRLEV(  N)=1
        LVABBR( N)='sfc'
      ENDDO
C
      NTOTL=N
      NVTOTL=NVTOTL
      NPTOTL=0
      IPINDX(1)=1
C
      DO N=1,NTOTL
        LABEL(4,N)(1:LENDA)=DATES(1:LENDA)
        LENLA(4,N)=LENDA
        LABEL(5,N)(1:LENDA)=FTIME(1:LENFT)
        LENLA(5,N)=LENFT
      ENDDO
C
      DO N=1,NTOTL
        RSLAT(N)=99.
        RNLAT(N)=99.
        WLON(N)=0.
        ELON(N)=360.
        IDIM(N)=IMAXL
        JDIM(N)=JMAXL
        DLON(N)=360./FLOAT(IMAXL)
        DLAT(N)=99.
        LGLOBL(N)=.TRUE.
        IJORDR(N)=.TRUE.
        FJHOUR(N,NFLX)=FHOUR
        RJLDAY(N,NFLX)=JD+FJD
      ENDDO
C
      CALL FLSRCH(NTOTL,LUPTR)
C
      DO N=1,NTOTL
        SELN(N)=N
      ENDDO
C
      RETURN
      END
      SUBROUTINE INCTFT(NFILE,NTOTL,SELN,ITINC,IFTINC,MFHINC,
     1                  LUPTR)
C
      INTEGER SELN(*)
      CHARACTER*8 ITINC,IFTINC
C
C  FIND TINC and FTINC
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      SAVE IMNINC
C
      NFLX=MIN(NFILE,2)
C
      WRITE(LUPTR,*) 'RJLDAY1=',(RJLDAY(NN,1),NN=1,NTOTL)
      WRITE(LUPTR,*) 'RJLDAY2=',(RJLDAY(NN,2),NN=1,NTOTL)
      IMNINC=9999999
      DO NN=1,NTOTL
        N=SELN(NN)
        IF(N.GT.0) THEN
          IF(LCHOSEN(N)) THEN
            RJLDAY1=RJLDAY(N,1)
            GO TO 1
          ENDIF
        ENDIF
      ENDDO
    1 CONTINUE
C
      WRITE(LUPTR,*) 'RJLDAY1=',RJLDAY1
C  
      DO NNN=1,NFLX
        NNS=2
        IF(NNN.EQ.2) NNS=1
        DO NN=NNS,NTOTL
          N=SELN(NN)
          IF(N.GT.0) THEN
            IF(LCHOSEN(N)) THEN
              JMNINC=NINT(ABS(RJLDAY1-RJLDAY(N,NNN))*24.*60.)
C             WRITE(LUPTR,*) 'N,NNN,RJLDAY,JMNINC,IMNINC=',
C    1                        N,NNN,RJLDAY(N,NNN),JMNINC,IMNINC
              IF(JMNINC.GT.0.AND.JMNINC.LT.IMNINC) THEN
                IMNINC=JMNINC
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      IF(IMNINC.EQ.9999999) IMNINC=0
C
      WRITE(LUPTR,*) 'IMNINC=',IMNINC
C
      IYRINC=IMNINC/(60*24*30*12)
      IMOINC=IMNINC/(60*24*30)
      IDYINC=IMNINC/(60*24)
      IHRINC=IMNINC/60
C
      IF(IYRINC.GT.0) THEN
        WRITE(ITINC,'(I2,2Hyr)') IYRINC
      ELSEIF(IMOINC.GT.0) THEN
        WRITE(ITINC,'(I2,2Hmo)') IMOINC
      ELSEIF(IDYINC.GT.4) THEN
        WRITE(ITINC,'(I2,2Hdy)') IDYINC
      ELSEIF(IHRINC.GT.0) THEN
        WRITE(ITINC,'(I2,2Hhr)') IHRINC
      ELSEIF(IMNINC.GT.0) THEN
        WRITE(ITINC,'(I2,2Hmn)') IMNINC
      ENDIF
C
      LMNINC=9999999
      WRITE(LUPTR,*) 'FJHOUR1=',(FJHOUR(NN,1),NN=1,NTOTL)
      WRITE(LUPTR,*) 'FJHOUR2=',(FJHOUR(NN,2),NN=1,NTOTL)
      DO NN=1,NTOTL
        N=SELN(NN)
        IF(N.GT.0) THEN
          IF(LCHOSEN(N)) THEN
            FJHOUR1=FJHOUR(N,1)
            GO TO 2
          ENDIF
        ENDIF
      ENDDO
    2 CONTINUE
C
      WRITE(LUPTR,*) 'FJHOUR1=',FJHOUR1
C  
      DO NNN=1,NFLX
        NNS=2
        IF(NNN.EQ.2) NNS=1
        DO NN=NNS,NTOTL
          N=SELN(NN)
          IF(N.GT.0) THEN
            IF(LCHOSEN(N)) THEN
              KMNINC=NINT(ABS(FJHOUR1-FJHOUR(N,NNN))*60.)
              IF(KMNINC.GT.0.AND.KMNINC.LT.LMNINC) THEN
                LMNINC=KMNINC
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      IF(LMNINC.EQ.9999999) LMNINC=0
C
      IF(IMNINC.EQ.0.AND.LMNINC.EQ.0.AND.FJHOUR1.GT.0.) THEN
        LMNINC=FJHOUR1*60.
      ENDIF
C
      LDYINC=LMNINC/(60*24)
      LHRINC=LMNINC/60
      IF(LDYINC.GT.4) THEN
        IF(LDYINC.LT.10) THEN
          WRITE(IFTINC,'(I1,2Hdy)') LDYINC
        ELSE
          WRITE(IFTINC,'(I2,2Hdy)') LDYINC
        ENDIF
      ELSEIF(LHRINC.GT.0) THEN
        IF(LHRINC.LT.10) THEN
          WRITE(IFTINC,'(I1,2Hhr)') LHRINC
        ELSE
          WRITE(IFTINC,'(I2,2Hhr)') LHRINC
        ENDIF
      ELSE
        IF(LMNINC.LT.10) THEN
          WRITE(IFTINC,'(I1,2Hmn)') LMNINC
        ELSE
          WRITE(IFTINC,'(I2,2Hmn)') LMNINC
        ENDIF
      ENDIF
C
      WRITE(LUPTR,*) 'ITINC=',ITINC,' IFTINC=',IFTINC
C
      MFHINC=LHRINC
C
      RETURN
      END
      SUBROUTINE NAMUNPO(FNGRIB,NDIR,FNGUPO,LUPTR)
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11,MLTYP=6)
C
      CHARACTER*128 FNGRIB,FNGUPO
C
      LOGICAL LFIND
C
      CHARACTER*1 LOW(26)
      DATA LOW/'a','b','c','d','e','f','g','h','i','j','k','l','m',
     1         'n','o','p','q','r','s','t','u','v','w','x','y','z'/
C
      CHARACTER*128 FNGUPOS
      COMMON/COMINDY/NFGUPO,FNGUPOS(MTOTL)
C
      WRITE(LUPTR,*) 'NDIR=',NDIR
      LFIND=.FALSE.
      NH=0
      J=0
      DO I=1,128
        IF(.NOT.LFIND.AND.FNGRIB(I:I).EQ.'/') THEN
          NH=NH+1
          IF(NH.EQ.NDIR) LFIND=.TRUE.
          J=0
        ELSEIF(FNGRIB(I:I).EQ.' ') THEN
          GO TO 122
        ELSE
          J=J+1
          FNGUPO(J:J)=FNGRIB(I:I)
        ENDIF
  122   CONTINUE
      ENDDO
      FNGUPO=FNGUPO(1:J)//'.unp'
      NN=0
  333 CONTINUE
      NN=NN+1
      DO NX=1,NFGUPO
        IF(FNGUPO.EQ.FNGUPOS(NX)) THEN
          FNGUPO=FNGUPO(1:J)//LOW(NN)//'.unp'
          GO TO 333
        ENDIF   
      ENDDO
      NFGUPO=NFGUPO+1
      IF(NFGUPO.GT.MTOTL) THEN
        WRITE(LUPTR,*) 'ERROR.  Too many unpacked grib files'
        PRINT *,'ERROR.  Number of unpacked grid file exceeded limit'
        CALL ABORT
      ENDIF
      FNGUPOS(NFGUPO)=FNGUPO
      WRITE(LUPTR,*) 'FNGRIB=',FNGRIB(1:128)
      WRITE(LUPTR,*) 'FNGUPO=',FNGUPO(1:128)
      RETURN
      END
      SUBROUTINE UNPZNL(FNZNLI,FNZNLO,NFILE,LEVMDL,LCRAY,
     1                  LREC,NREC,LUZNLI,LUZNLO,LUPTR)
C
      CHARACTER*128 FNZNLI,FNZNLO
      CHARACTER*80 ASGNSTR,SHLLSTR
      LOGICAL LCRAY
C
      DIMENSION IDATE(4)
C
      PARAMETER(NLB=6,NST=6,NVRM=23,NVRS=32)
C
      PARAMETER(LLEVMDL=28+1)
      REAL*8 FHOUR,FSTEP
      REAL*8 ZNLM(NLB,LLEVMDL,NVRM),ZNLS(NLB,NST,NVRS)
      REAL*8 WRKM(NLB,LLEVMDL),WRKS(NLB,NST)
      REAL*4 RNLM(NLB,LLEVMDL,NVRM),RNLS(NLB,NST,NVRS)
C
      PRINT *,' Reformatting ZONAL file ',FNZNLI(1:128)
C
      WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2)') LUZNLI
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUZNLI,FILE=FNZNLI,FORM='UNFORMATTED',ERR=988)
      GO TO 987
  988 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNZNLI
      PRINT *,'ERROR IN OPENING FILE ',FNZNLI
      CALL ABORT
  987 CONTINUE
      WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
      WRITE(LUPTR,*) 'FILE ',FNZNLI(1:128),' opened. Unit=',LUZNLI
C
      IF(NFILE.EQ.1) THEN
C       DO I=1,80
C         IF(FNZNLO(I:I).EQ.'/') THEN
C           DO J=80,1,-1
C             IF(FNZNLO(J:J).EQ.'/') THEN
C               JJ=J
C               GO TO 1
C             ENDIF
C           ENDDO
C   1       CONTINUE
C           SHLLSTR='mkdir -p '//FNZNLO(1:JJ)
C           WRITE(LUPTR,*) 'SHLLSTR=',SHLLSTR
C           NERR=ISHELL(SHLLSTR)
C           IF(NERR.NE.0) THEN
C             WRITE(LUPTR,*) ' ISHELL mkdir return code non-zero.'
C           ENDIF
C           GO TO 12
C         ENDIF
C       ENDDO
C  12   CONTINUE
        IF(.NOT.LCRAY) THEN
          WRITE(ASGNSTR,'(23Hassign -Nieee -Fnull u:,I2)') LUZNLO
        ELSE
          WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUZNLO
        ENDIF
        CALL ASSIGN(ASGNSTR)
        OPEN(UNIT=LUZNLO,FILE=FNZNLO,FORM='UNFORMATTED',ERR=888)
        GO TO 887
  888   CONTINUE
        WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNZNLO
        PRINT *,'ERROR IN OPENING FILE ',FNZNLO
        CALL ABORT
  887   CONTINUE
        WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
        WRITE(LUPTR,*) 'FILE ',FNZNLO(1:128),' opened. Unit=',LUZNLO
      ENDIF
C
      NREC=0
      NLV=LEVMDL+1
      IF(NLV.GT.LLEVMDL) THEN
        PRINT *,'NEED TO INCREASE LLEVMDL in UNPZNL to ',NLV
        CALL ABORT
      ENDIF
      REWIND LUZNLI
      READ(LUZNLI)
      READ(LUZNLI) FHOUR,FSTEP,IDATE,
     1             (((ZNLM(J,K,N),J=1,NLB),K=1,NLV),N= 1,10),
     2             (((ZNLM(J,K,N),J=1,NLB),K=1,NLV),N=20,21),
     3             (((WRKM(J,K  ),J=1,NLB),K=1,NLV),N=13,30),
     4             (((ZNLS(J,K,N),J=1,NLB),K=1,NST),N= 9,15),
     5             (  WRKS                         ,N= 8,11),
     6             (((ZNLS(J,K,N),J=1,NLB),K=1,NST),N=16,22),
     7             (  WRKS                         ,N=19,29),
     8             (((ZNLS(J,K,N),J=1,NLB),K=1,NST),N=23,23),
     9             (((ZNLM(J,K,N),J=1,NLB),K=1,NLV),N=11,19),
     3             (((WRKM(J,K  ),J=1,NLB),K=1,NLV),N=10,30),
     B             (((ZNLS(J,K,N),J=1,NLB),K=1,NST),N= 1, 8),
     C             (((ZNLS(J,K,N),J=1,NLB),K=1,NST),N=24,30),
     D             (  WRKS                         ,N=16,30)
      WRITE(LUPTR,*) 'FHOUR,FSTEP=',FHOUR,FSTEP,' IDATE=',IDATE
C
      DO K=1,NVRM
        DO J=1,LEVMDL+1
          DO I=1,NLB
            RNLM(I,J,K)=ZNLM(I,J,K)
          ENDDO
        ENDDO
      ENDDO
      DO K=1,NVRS
        DO J=1,NST
          DO I=1,NLB
            RNLS(I,J,K)=ZNLS(I,J,K)
          ENDDO
        ENDDO
      ENDDO
      DO K=1,NVRM
        DO J=1,LEVMDL+1
          WRITE(LUZNLO) (RNLM(I,J,K),I=1,NLB)
        ENDDO
      ENDDO
      DO K=1,NVRS
        DO J=1,NST
          WRITE(LUZNLO) (RNLS(I,J,K),I=1,NLB)
        ENDDO
      ENDDO
      NREC=NREC+1
      LREC=NLB*(LEVMDL+1)*NVRM+NLB*NST*NVRS
C
      RETURN
      END
      SUBROUTINE UNPKEN(FNKENI,FNKENO,KENPTS,KENLEN,LEVMDL,
     1                  NFILE,UNDEF,LCRAY,LREC,NREC,LUKENI,LUKENO,LUPTR)
C
      PARAMETER(NVRM=8,NVRS=80)
C
      CHARACTER*80 ASGNSTR,SHLLSTR
      CHARACTER*128 FNKENI,FNKENO
      LOGICAL LCRAY
C
      DIMENSION IDATE(4)
C
      PARAMETER(LSVDATA=1000)
      REAL*8 SVDATA(LSVDATA),FHOUR
      REAL*4 RVDATA(LSVDATA)
C
      PRINT *,' Reformatting KEN file ',FNKENI(1:128)
C
      WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2)') LUKENI
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUKENI,FILE=FNKENI,FORM='UNFORMATTED',ERR=988)
      GO TO 987
  988 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNKENI
      PRINT *,'ERROR IN OPENING FILE ',FNKENI
      CALL ABORT
  987 CONTINUE
      WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
      WRITE(LUPTR,*) 'FILE ',FNKENI(1:128),' opened. Unit=',LUKENI
C
      IF(NFILE.EQ.1) THEN
        IF(LCRAY) THEN
          WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUKENO
          CALL ASSIGN(ASGNSTR)
        ENDIF
        OPEN(UNIT=LUKENO,FILE=FNKENO,FORM='UNFORMATTED',ERR=888)
        GO TO 887
  888   CONTINUE
        WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNKENO
        PRINT *,'ERROR IN OPENING FILE ',FNKENO
        CALL ABORT
  887   CONTINUE
        IF(LCRAY) THEN
          WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
        ENDIF
        WRITE(LUPTR,*) 'FILE ',FNKENO(1:128),' opened. Unit=',LUKENO
      ENDIF
C
      NREC=0
      REWIND LUKENI
      READ(LUKENI)
      READ(LUKENI) FHOUR, IDATE
      READ(LUKENI) NVRKEN,NPTKEN,NSTKEN,NPOINTS,ITNUM
      NLEN=ITNUM*NVRKEN
      IF(NLEN.NE.KENLEN.OR.NPOINTS.NE.KENPTS) THEN
        WRITE(LUPTR,*) 'ERROR! Inconsistent KEN file encountered.'
        WRITE(LUPTR,*) 'KENLEN=',KENLEN,' KENPTS=',KENPTS
        WRITE(LUPTR,*) 'NVRKEN=',NVRKEN,' NPTKEN=',NPTKEN,
     1         ' NSTKEN=',NSTKEN,' NPOINTS=',NPOINTS,' ITNUM=',ITNUM,
     2         ' NLEN=',NLEN
        PRINT *,'ERROR! Inconsistent KEN file encountered.'
        CALL ABORT
      ENDIF
C
      REWIND LUKENI
      READ(LUKENI)
      READ(LUKENI)
      READ(LUKENI)
      IF(KENPTS*KENLEN.GT.LSVDATA) THEN
        PRINT *,'ERROR.  INCREASE DIM OF SVDATA TO ',KENPTS*KENLEN
        CALL ABORT
      ENDIF
      DO M = 1, KENPTS
        READ(LUKENI) (SVDATA((M-1)*KENLEN+IJ),IJ=1,KENLEN)
        DO IJ=1,KENLEN
          RVDATA((M-1)*KENLEN+IJ)=SVDATA((M-1)*KENLEN+IJ)
        ENDDO
        IPTT=SVDATA((M-1)*KENLEN+1)
        JPTT=SVDATA((M-1)*KENLEN+ITNUM+1)
        WRITE(LUPTR,*) ' POINT#=',M,' I=',IPTT,' J=',JPTT
      ENDDO
C
      NSLEN=ITNUM*NVRS
      NKLEN=ITNUM*LEVMDL
      DO IT=1,ITNUM
        DO L=1,NVRM
         DO K=1,LEVMDL
          WRITE(LUKENO) (RVDATA((M-1)*KENLEN+NSLEN+(L-1)*NKLEN+
     1    (K-1)*ITNUM+IT),M=1,KENPTS)
         ENDDO
        ENDDO
        NREC=NREC+LEVMDL*NVRM
        DO L=1,NVRS
         WRITE(LUKENO) (RVDATA((M-1)*KENLEN+(L-1)*ITNUM+IT),M=1,KENPTS)
        ENDDO
        NREC=NREC+NVRS
        LREC=KENPTS
      ENDDO
C
      RETURN
      END
      SUBROUTINE UNPBGS(FNBGSI,FNBGSO,NFILE,LCRAY,IJMAX,
     1                  IDIML,JDIML,LREC,NREC,LUBGSI,LUBGSO,LUPTR)
C
      CHARACTER*128 FNBGSI,FNBGSO
      CHARACTER*80 ASGNSTR,SHLLSTR
      LOGICAL LCRAY
C
      DIMENSION IDATE(4)
C
      PARAMETER(LIJMAX=180*91)
      REAL*8 BGES(LIJMAX),CGES(LIJMAX),FHOUR
      REAL*4 RGES(LIJMAX),SGES(LIJMAX)
C
      PRINT *,' Reformatting BGES file ',FNBGSI(1:128)
C
      WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2)') LUBGSI
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUBGSI,FILE=FNBGSI,FORM='UNFORMATTED',ERR=988)
      GO TO 987
  988 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNBGSI
      PRINT *,'ERROR IN OPENING FILE ',FNBGSI
      CALL ABORT
  987 CONTINUE
      WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
      WRITE(LUPTR,*) 'FILE ',FNBGSI(1:128),' opened. Unit=',LUBGSI
C
      IF(NFILE.EQ.1) THEN
        IF(LCRAY) THEN
          WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUBGSO
          CALL ASSIGN(ASGNSTR)
        ENDIF
        OPEN(UNIT=LUBGSO,FILE=FNBGSO,FORM='UNFORMATTED',ERR=888)
        GO TO 887
  888   CONTINUE
        WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNBGSO
        PRINT *,'ERROR IN OPENING FILE ',FNBGSO
        CALL ABORT
  887   CONTINUE
        IF(LCRAY) THEN
          WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
        ENDIF
        WRITE(LUPTR,*) 'FILE ',FNBGSO(1:128),' opened. Unit=',LUBGSO
      ENDIF
C
      REWIND LUBGSI
      READ(LUBGSI)
      READ(LUBGSI) FHOUR,IDATE
      WRITE(LUPTR,*) 'FHOUR',FHOUR,' IDATE=',IDATE
C
      NREC=0
      DO I=1,1000
        IF(I.EQ.2.OR.I.EQ.4) THEN
          READ (LUBGSI,END=100)(BGES(IJ),IJ=1,IJMAX),
     1                         (CGES(IJ),IJ=1,IJMAX)
          NREC=NREC+2
          DO IJ=1,IJMAX
            RGES(IJ)=BGES(IJ)
            SGES(IJ)=CGES(IJ)
          ENDDO
          WRITE(LUBGSO) (RGES(IJ),IJ=1,IJMAX)
          WRITE(LUBGSO) (SGES(IJ),IJ=1,IJMAX)
        ELSE
          READ (LUBGSI,END=100,ERR=100) (BGES(IJ),IJ=1,IJMAX)
          NREC=NREC+1
          DO IJ=1,IJMAX
            RGES(IJ)=BGES(IJ)
          ENDDO
          WRITE(LUBGSO) (RGES(IJ),IJ=1,IJMAX)
        ENDIF
      ENDDO
      LREC=IJMAX
  100 CONTINUE
      IF(NFILE.EQ.1) THEN
        WRITE(LUPTR,*) ' Number of Rec. written=',NREC
      ENDIF
C
      RETURN
      END
      SUBROUTINE GETINC(ITINC,NFILE,LUPTR)
C
      SAVE JD,FJD,KFH
C
      CHARACTER*8 ITINC
      DIMENSION JD(2),FJD(2),KFH(2)
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11,MLTYP=6)
      COMMON/COMKEY/ KEYPDS(LKPDS,MTOTL),KEYGDS(LKGDS,MTOTL)
C
      IY=KEYPDS(8,1)
      IM=KEYPDS(9,1)
      ID=KEYPDS(10,1)
      IH=KEYPDS(11,1)
      IMIN=KEYPDS(12,1)
      CALL COMPJD(IY,IM,ID,IH,IMIN,JD(NFILE),FJD(NFILE))
      WRITE(LUPTR,*) ' NFILE=',NFILE,' JD(NFILE)=',JD(NFILE),
     1               ' FJD=',FJD(NFILE)
C
      IF(KEYPDS(16,1).EQ.0.OR.KEYPDS(16,1).EQ.10) THEN
        IF(KEYPDS(14,1).EQ.0) THEN
          KFH(NFILE)=0
        ELSE
          KFH(NFILE)=KEYPDS(14,1)
        ENDIF
      ELSEIF(KEYPDS(16,1).EQ.1) THEN
        IF(KEYPDS(14,1).EQ.0) THEN
          KFH(NFILE)=0
        ELSE
          WRITE(LUPTR,*) ' Error!!!' 
          WRITE(LUPTR,*) ' KEYPDS(14,1) incorrect !!!' 
        ENDIF
      ENDIF
      IF(NFILE.EQ.2) THEN
        NDYINC=ABS(JD(2)-JD(1))
        WRITE(LUPTR,*) ' NDYINC=',NDYINC
        IF(FJD(2).NE.FJD(1)) THEN
          IF(JD(2).GT.JD(1)) THEN
            FHRINC=MOD(NINT(ABS((FJD(2)+NDYINC-FJD(1))*24.)),24)
            NDYINC=0
          ELSEIF(JD(2).LT.JD(1)) THEN
            FHRINC=MOD(NINT(ABS((FJD(1)+NDYINC-FJD(2))*24.)),24)
            NDYINC=0
          ELSE
            FHRINC=MOD(NINT(ABS(FJD(2)-FJD(1))*24.),24)
          ENDIF
        ELSE
          IF(JD(2).NE.JD(1)) THEN
            FHRINC=NDYINC*24.
            NDYINC=0
          ELSE
            FHRINC=0.
          ENDIF
        ENDIF
        NHRINC=NINT(FHRINC)
        WRITE(LUPTR,*) 'NHRINC=',NHRINC
        NMNINC=NINT((FHRINC-NHRINC)*60.)
        IF(NHRINC.GT.0.AND.NMNINC.GT.0) THEN
          NMNINC=NMNINC+NHRINC*60
          NHRINC=0
        ENDIF
        IF(NMNINC.GT.0) THEN
          NYRINC=NMNINC/(60*24*30*12)
          NMOINC=NMNINC/(60*24*30)
        ENDIF
        IF(NYRINC.GT.0) THEN
          WRITE(ITINC,'(I2,2Hyr)') NYRINC
        ELSEIF(NMOINC.GT.0) THEN
          WRITE(ITINC,'(I2,2Hmo)') NMOINC
        ELSEIF(NDYINC.GT.0) THEN
          WRITE(ITINC,'(I2,2Hdy)') NDYINC
        ELSEIF(NHRINC.GT.0) THEN
          WRITE(ITINC,'(I2,2Hhr)') NHRINC
        ELSEIF(NMNINC.GT.0) THEN
          WRITE(ITINC,'(I2,2Hmn)') NMNINC
        ENDIF
        IF(ITINC(1:8).EQ.'        ') THEN
          KFHINC=KFH(2)-KFH(1)
          WRITE(ITINC,'(I2,2Hhr)') KFHINC
          WRITE(LUPTR,*) 'KFHINC=',KFHINC
        ENDIF
      ENDIF
      WRITE(LUPTR,*) ' ITINC=',ITINC
      RETURN
      END
      SUBROUTINE GETDELT(IFKEY,LITSR,LFTSR,ITINC,IFTINC,
     1                   LABEL4,LENLA4,MFHMIN,LVDATE,
     2                   DATES,TINC,INCTIME,INCUNIT,LUPTR)
C
      CHARACTER*2 INCUNIT
C
      CHARACTER*1 IFKEY
      CHARACTER*8 TINC,ITINC,IFTINC
      CHARACTER*64 LABEL4
      LOGICAL LITSR,LFTSR,LVDATE
      CHARACTER*24 DATES
C
      IF(INCUNIT(1:1).NE.' '.AND.INCTIME.NE.0) THEN
        J=0
        DO I=1,LENLA4
          IF(LABEL4(I:I).NE.' ') THEN
            J=J+1
            DATES(J:J)=LABEL4(I:I)
          ENDIF
        ENDDO
        WRITE(TINC,'(I5,A2)') INCTIME,INCUNIT(1:2)
        WRITE(LUPTR,*) '0) TINC=',TINC
        RETURN
      ENDIF
C
      TINC(1:8)='        '
      IF(LITSR.AND.IFKEY.EQ.'I') THEN
        TINC=ITINC
        WRITE(LUPTR,*) 'a) TINC=',TINC
      ELSEIF(LFTSR.AND.IFKEY.EQ.'F') THEN
          TINC=IFTINC
          WRITE(LUPTR,*) 'b) TINC=',TINC
        ELSEIF(LITSR.AND..NOT.LFTSR) THEN
          TINC=ITINC
          WRITE(LUPTR,*) 'c) TINC=',TINC
        ELSEIF(LFTSR.AND..NOT.LITSR) THEN
          TINC=IFTINC
          WRITE(LUPTR,*) 'd) TINC=',TINC
        ELSEIF(TINC(1:8).EQ.'        '.AND.
     1        ITINC(1:8).NE.'        ') THEN
          TINC=ITINC
          WRITE(LUPTR,*) 'e) TINC=',TINC
        ELSEIF(TINC(1:8).EQ.'        '.AND.
     1         IFTINC(1:8).NE.'        ') THEN
          TINC=IFTINC
          WRITE(LUPTR,*) 'f) TINC=',TINC
        ENDIF
C
      IF(TINC(1:3).EQ.'   '.OR.TINC(1:2).EQ.'**'.OR.
     1   TINC(1:3).EQ.'0mn'.OR.TINC(1:3).EQ.'0hr'.OR.
     2   TINC(1:4).EQ.' 0mn'.OR.TINC(1:4).EQ.' 0hr') THEN
        TINC='12hr'
        WRITE(LUPTR,*) 'g) TINC=',TINC
      ENDIF
C
      J=0
      DO I=1,LENLA4
        IF(LABEL4(I:I).NE.' ') THEN
          J=J+1
          DATES(J:J)=LABEL4(I:I)
        ENDIF
      ENDDO
      IF(LVDATE.AND.(MFHMIN.GT.0.AND.MFHMIN.NE.9999)) THEN
        CALL INCDATE(DATES,J,MFHMIN,LUPTR)
      ENDIF
C
      RETURN
      END
      SUBROUTINE MKCTL(DIRNAME,FNDSET,FNCNTL,RUNAME,NTOTL,LUNPK,LINTRP,
     1                 LVDATE,IFTYPE,IDIML,JDIML,IMAXL,JMAXL,NTDEF,
     2                 WLONL,SLATL,DLONL,DLATL,
     3                 IXDIM,JXDIM,XLATS,XLONW,
     4                 TINC,DATES,UNDEF,LTMPL,LUCTL,LUPTR)
C
      CHARACTER*128 DIRNAME
      CHARACTER*128 FNDSET
      CHARACTER*128 FNCNTL
      CHARACTER*80 RUNAME
      CHARACTER*8 TINC,TINCX
      CHARACTER*24 DATES,DATESX
      CHARACTER*8 NVAR(500)
      CHARACTER*8 NVARN
      CHARACTER*1 YN
      LOGICAL LTMPL,LUNPK,LINTRP,LVDATE
C
      PARAMETER(LWIDTH=54)
C
      CHARACTER*128 FNGSMAP,FNGSCTL
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      COMMON/COMKEY/ KEYPDS(LKPDS,MTOTL),KEYGDS(LKGDS,MTOTL)
C
      DIMENSION RLAT(1000)
C
      DIMENSION IRLEVS(500)
C
      CHARACTER*3 NUMB
C
      PARAMETER(KZERO=0)
C
      CHARACTER*128 FXNAME
      CHARACTER*80 FMT
C
      WRITE(LUPTR,*) ' IDIML=',IDIML,' JDIML=',JDIML
      WRITE(LUPTR,*) ' IMAXL=',IMAXL,' JMAXL=',JMAXL
      WRITE(LUPTR,*) ' IXDIM=',IXDIM,' JXDIM=',JXDIM
      WRITE(LUPTR,*) ' WLONL=',WLONL,' SLATL=',SLATL
      WRITE(LUPTR,*) ' DLONL=',DLONL,' DLATL=',DLATL
      WRITE(LUPTR,*) ' LFTSR=',LFTSR,' LITSR=',LITSR
      WRITE(LUPTR,*) ' NPTOTL=',NPTOTL,' FNCNTL=',FNCNTL
C
C     NLVTYS=MAX(NPTOTL,1)
C     IF(LUNPK) NLVTYS=1
      NLVTYS=1
C
      KGDS1=KEYGDS(1,1)
C
      MLV=0
      DO NLV=1,NLVTYS
C
C dset name
C
        N=1
        DO NNN=1,NVTOTL
          IF(LABEL(2,IVINDX(NNN))(1:LENLA(2,IVINDX(NNN))).EQ.
     1       LABEL(2,IPINDX(NLV))(1:LENLA(2,IPINDX(NLV)))) THEN
            N=IVINDX(NNN)
            GO TO 10
          ENDIF
        ENDDO
   10   CONTINUE
C
C  ctl file name
C
        DO I=1,128
          FNGSCTL(I:I)=' '
          FNGSMAP(I:I)=' '
        ENDDO
        JJ=0
        DO I=1,128
          IF(DIRNAME(I:I).NE.' ') THEN
            JJ=JJ+1
            FNGSMAP(JJ:JJ)=DIRNAME(I:I)
          ENDIF
        ENDDO
        IF(NLVTYS.EQ.1) THEN
          J=0
          DO I=1,128
            IF(FNCNTL(I:I).NE.' ') THEN
              J=J+1
              FNGSCTL(J:J)=FNCNTL(I:I)
            ENDIF
          ENDDO
          FNGSCTL=FNGSCTL(1:J)//'.ctl'
          IF(.NOT.LUNPK) THEN
            FNGSMAP(JJ+1:JJ+J+4)=FNGSCTL(1:J)//'.idx'
          ENDIF
        ELSE
          FNGSCTL=FNCNTL(1:3)//LVABBR(N)//'.ctl'
          FNGSMAP=FNGSMAP(1:JJ)//FNCNTL(1:3)//LVABBR(N)//'.idx'
        ENDIF
C
        DO I=1,128
          IF(FNGSCTL(I:I).EQ.' ') GO TO 111
        ENDDO
  111   CONTINUE
        N1=I-1
        IF(N1.LT.1) THEN
           WRITE(LUPTR,*) 'FNGSCTL IS BLANK'
           CALL ABORT
        ENDIF
        WRITE(LUPTR,*) ' FNGSCTL=',FNGSCTL(1:N1)
C
        IF(.NOT.LUNPK) THEN
					DO I=1,128
						IF(FNGSMAP(I:I).EQ.' ') GO TO 112
					ENDDO
  112     CONTINUE
					N2=I-1
					IF(N2.LT.1) THEN
						 WRITE(LUPTR,*) 'FNGSMAP IS BLANK'
						 CALL ABORT
					ENDIF
          WRITE(LUPTR,*) ' FNGSMAP=',FNGSMAP(1:N2)
        ENDIF
C
C  Open GRADS ctl file
C
        CALL ASSIGN('assign -R')
C
        OPEN(LUCTL,FILE=FNGSCTL,ERR=711)
C
        GO TO 712
  711   CONTINUE
        WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNGSCTL,' UNIT=',LUCTL
        PRINT *,' ERROR IN OPENING FILE ',FNGSCTL,' UNIT=',LUCTL
        CALL ABORT
  712   CONTINUE
        WRITE(LUPTR,*) ' FILE ',FNGSCTL(1:128),' opened.  Unit=',LUCTL
C
C  dset name
C
        WRITE(LUCTL,'(5Hdset ,A128)') FNDSET(1:128)
        IF(.NOT.LUNPK) THEN
          WRITE(LUCTL,'(10Hdtype grib)')
        ENDIF
C
C  For workstation sequential write
C
        IF(LUNPK) THEN
          WRITE(LUCTL,'(18Hoptions sequential)')
        ENDIF
C
        IF(LTMPL) THEN
          IF(LUNPK) THEN
            WRITE(LUCTL,'(21Hoptions template yrev)')
          ELSE
            IF(DLATL.LT.0.) THEN
              WRITE(LUCTL,'(21Hoptions template yrev)')
            ELSE
              WRITE(LUCTL,'(16Hoptions template)')
            ENDIF
          ENDIF
        ELSE
          IF(LUNPK) THEN
            WRITE(LUCTL,'(11Hformat yrev)')
          ELSE
            IF(DLATL.LT.0.) THEN
              WRITE(LUCTL,'(11Hformat yrev)')
            ENDIF
          ENDIF
        ENDIF
        IF(.NOT.LUNPK) THEN
          WRITE(LUCTL,'(6Hindex ,A122)') FNGSMAP(1:122)
        ENDIF
        WRITE(LUCTL,'(6Hundef ,1PE9.2)') UNDEF
C
C  Title
C
        WRITE(LUCTL,'(6Htitle ,A68)') RUNAME(1:68)
C
C  Special for Stereographic
C
        IF(KGDS1.EQ.5.AND.IDIML.EQ.0.AND.JDIML.EQ.0) THEN
          TRUTH=60.
          ORIENT=PARMAP(1)
          DELX=DLON(1)
          DELY=DLAT(1)
          IF(DELX.NE.DELY) THEN
            WRITE(LUPTR,*) 'ERROR.  DELX.NE.DELY for polar proj'
            CALL ABORT
          ENDIF
          FLAT=RSLAT(1)
          FLON=WLON(1)
          CALL GETIJNP(TRUTH,ORIENT,DELX,DELY,FLAT,FLON,XPT,YPT)
          IF(NPARMP(1).EQ.0) THEN
             WRITE(LUCTL,'(5Hpdef ,2I5,4H nps,4F9.3)') 
     1                    IDIM(1),JDIM(1),XPT,YPT,ORIENT,DELX/1000.
          ELSEIF(NPARMP(1).EQ.128) THEN
             WRITE(LUCTL,'(5Hpdef ,2I5,4H sps,4F9.3)') 
     1                    IDIM(1),JDIM(1),XPT,YPT,ORIENT,DELX/1000.
          ELSE
            WRITE(LUPTR,*) 'ERROR.  Bad KGDS(10)=',NPARMP(1)
            WRITE(LUPTR,*) 'Assume KGDS(10)=0, i.e. nps and continues'
            WRITE(LUCTL,'(5Hpdef ,2I5,4H nps,4F9.3)') 
     1                   IDIM(1),JDIM(1),XPT,YPT,ORIENT,DELX/1000.
          ENDIF
          SCALE=10.
 2233     CONTINUE
          DXLL=NINT(DELX/1000./111.*SCALE)/SCALE
          IF(DXLL.LE.0.) THEN
            SCALE=SCALE*10.
            GO TO 2233
          ENDIF
          IMLL=360./DXLL
          JMLL=(90.-FLAT)/DXLL+1
          WRITE(LUCTL,'(5Hxdef ,I5,8H linear ,F8.3,1H ,F6.3)') 
     1                 IMLL,0.,DXLL
          WRITE(LUCTL,'(5Hydef ,I5,8H linear ,F8.3,1H ,F6.3)') 
     1                 JMLL,FLOAT(INT(FLAT)),DXLL
          GO TO 2244
        ENDIF
C
C  X-coord
C
        IF(IXDIM.NE.0) THEN
           IDIMQ=IDIML
           IF(IDIMQ.EQ.0) IDIMQ=IDIM(1)
           WRITE(LUCTL,'(5Hxdef ,I5,8H linear ,F8.3,1H ,F6.3)') 
     1                   IXDIM,XLONW,360./IDIMQ
        ELSE
          IF(IDIML.EQ.0.AND.JDIML.EQ.0) THEN
             IF(IMAXL.GT.1) THEN
               WRITE(LUCTL,'(5Hxdef ,I5,8H linear ,F8.3,1H ,F6.3)') 
     1                      IMAXL,WLONL,ABS(DLONL)
             ELSE
               WRITE(LUCTL,'(5Hxdef ,I5,8H linear ,F8.3,1H ,F6.3)') 
     1                      IMAXL,0.,1.
             ENDIF
          ELSE
            IF(IDIML.GT.1) THEN
              WRITE(LUCTL,'(5Hxdef ,I5,8H linear ,F8.3,1H ,F6.3)') 
     1                      IDIML,0.,360./IDIML
            ELSE
              WRITE(LUCTL,'(5Hxdef ,I5,8H linear ,F8.3,1H ,F6.3)') 
     1                      1,0.,1.
            ENDIF
          ENDIF
        ENDIF
C
C  Y-coord
C
        IF(JXDIM.NE.0) THEN
          JDIMQ=JDIML
          IF(JDIMQ.EQ.0) JDIMQ=JDIM(1)
          WRITE(LUCTL,'(5Hydef ,I5,8H linear ,F8.3,1H ,F6.3)') 
     1                JXDIM,XLATS,180./(JDIMQ-1)
        ELSE
          IF(IDIML.EQ.0.AND.JDIML.EQ.0) THEN
            IF(KGDS1.EQ.1) THEN
              CALL MERCTR(JMAXL,PARMAP(1),DLAT(1),RSLAT(1),
     1                    RNLAT(1),RLAT,LUPTR)
              WRITE(LUCTL,'(5Hydef ,I5,7H levels)') JMAXL
              IF(MOD(JMAXL,10).NE.1) THEN
                WRITE(LUCTL,'(10F8.3)') (RLAT(J),J=JMAXL,1,-1)
              ELSE
                WRITE(LUCTL,'(10F8.3)') (RLAT(J),J=JMAXL,1,-1),0.
              ENDIF
            ELSEIF(KGDS1.EQ.4) THEN
              CALL GAULAT(RLAT,JMAXL)                                         
              WRITE(LUCTL,'(5Hydef ,I5,7H levels)') JMAXL
              IF(MOD(JMAXL,10).NE.1) THEN
                WRITE(LUCTL,'(10F8.3)') (RLAT(J),J=JMAXL,1,-1)
              ELSE
                WRITE(LUCTL,'(10F8.3)') (RLAT(J),J=JMAXL,1,-1),0.
              ENDIF
            ELSE
              WRITE(LUCTL,'(5Hydef ,I5,8H linear ,F8.3,1H ,F6.3)') 
     1                    JMAXL,SLATL,ABS(DLATL)
            ENDIF
          ELSE
            WRITE(LUCTL,'(5Hydef ,I5,8H linear ,F8.3,1H ,F6.3)') 
     1            JDIML,-90.,180./(JDIML-1)
          ENDIF
        ENDIF
 2244   CONTINUE
C
C  Levels
C
C    Find max number of levels
C
        LEVS=0
        DO NV=1,NVTOTL
          IF(LABEL(2,IVINDX(NV))(1:LENLA(2,IVINDX(NV))).EQ.
     1       LABEL(2,IPINDX(NLV))(1:LENLA(2,IPINDX(NLV)))) THEN
            IF(LEVS.LT.NLTOTL(NV)) THEN
              LEVS=NLTOTL(NV)
              NVK=NV
            ENDIF
          ENDIF
        ENDDO
        NN=0
        IF(NVK.GT.1) THEN
          DO NV=1,NVK-1
            NN=NN+NLTOTL(NV)
          ENDDO
        ENDIF
        IF(NVK.GE.1) THEN
          DO KL=1,NLTOTL(NVK)
            IRLEVS(KL)=IRLEV(ILINDX(NN+KL))
          ENDDO
        ENDIF
        IF(LEVS.GT.1) THEN
          DO KL=1,LEVS-1
            DO KM=KL+1,LEVS
              IF(IRLEVS(KL).LT.IRLEVS(KM)) THEN
                ITEMP=IRLEVS(KL)
                IRLEVS(KL)=IRLEVS(KM)
                IRLEVS(KM)=ITEMP
              ENDIF
            ENDDO
          ENDDO
        ENDIF
C
        IF(LEVS.GT.1) THEN
          WRITE(LUCTL,'(5Hzdef ,I5,8H levels )') LEVS
          IF(MOD(LEVS,10).NE.1) THEN
            WRITE(LUCTL,'(10I8)') (IRLEVS(KL),KL=1,LEVS)
          ELSE
            WRITE(LUCTL,'(10I8)') (IRLEVS(KL),KL=1,LEVS),0
          ENDIF
        ELSEIF(LEVS.EQ.1) THEN
          WRITE(LUCTL,'(5Hzdef ,I5,8H levels )') 2
          WRITE(LUCTL,'(10I8)') IRLEVS(LEVS),IRLEVS(LEVS)+1
        ELSE
          WRITE(LUCTL,'(5Hzdef ,I1,8H linear ,2I2)') 1,1,1
        ENDIF
C
C  Dates
C
        TINCX='        '
        DATESX='                        '
        J=0
        DO I=1,24
          IF(DATES(I:I).NE.' ') THEN
            J=J+1
            DATESX(J:J)=DATES(I:I)
          ENDIF
        ENDDO
        K=0
        DO I=1,8
          IF(TINC(I:I).NE.' ') THEN
            K=K+1
            TINCX(K:K)=TINC(I:I)
          ENDIF
        ENDDO
        WRITE(LUCTL,'(5Htdef ,I4,8H linear ,A24,1H ,A8)')
     1        NTDEF,DATESX(1:J),TINCX(1:K)
C
C  Variables
C
C       IF(LUNPK) THEN
C         NVTTL=NVTOTL
C       ELSE
C         NVTTL=0
C         IF(NLV.EQ.1) THEN
C           DO NN=1,NVTOTL
C             IF(LENLA(3,IVINDX(NN)).EQ.1.OR.
C    1           LABEL(2,IVINDX(NN))(1:LENLA(2,IVINDX(NN))).EQ.
C    2           LABEL(2,IPINDX(NLV))(1:LENLA(2,IPINDX(NLV)))) THEN
C               NVTTL=NVTTL+1
C             ENDIF
C           ENDDO
C         ELSE
C           DO NN=1,NVTOTL
C             IF(LABEL(2,IVINDX(NN))(1:LENLA(2,IVINDX(NN))).EQ.
C    2           LABEL(2,IPINDX(NLV))(1:LENLA(2,IPINDX(NLV)))) THEN
C               NVTTL=NVTTL+1
C             ENDIF
C           ENDDO
C         ENDIF
C       ENDIF
        NVTTL=NVTOTL
C
        WRITE(LUCTL,'(5Hvars ,I5)') NVTTL
C
        N=0
        DO NV=1,NVTOTL
C         IF(.NOT.LUNPK) THEN
C           IF(NLV.EQ.1) THEN
C            IF(LENLA(3,IVINDX(NV)).GT.1.AND.
C    1          LABEL(2,IVINDX(NV))(1:LENLA(2,IVINDX(NV))).NE.
C    2          LABEL(2,IPINDX(NLV))(1:LENLA(2,IPINDX(NLV)))) THEN
C              GO TO 811
C            ENDIF
C           ELSE
C            IF(LENLA(3,IVINDX(NV)).LE.1.OR.
C    1          LABEL(2,IVINDX(NV))(1:LENLA(2,IVINDX(NV))).NE.
C    2          LABEL(2,IPINDX(NLV))(1:LENLA(2,IPINDX(NLV)))) THEN
C              GO TO 811
C            ENDIF
C           ENDIF
C         ENDIF
C 
          NN=IVINDX(NV)
          NUMLEV=NLTOTL(NV)
C         IF(LUNPK.AND.NUMLEV.EQ.1) NUMLEV=0
          IF(.NOT.LUNPK) THEN
C
C  Following changed MK 3/14/96
C
            IF(LENLA(3,IVINDX(NV)).EQ.1) THEN
              IF(LABEL(3,IVINDX(NV))(1:4).EQ.'-XXX') THEN
                NUMLEV=0
C
C  PATCH FOR GRIB SINGLE LEVEL HANDLING IN GRADS
C
              ELSEIF(KEYPDS(6,NN).EQ.200) THEN
                NUMLEV=0
              ELSEIF(KEYPDS(6,NN).EQ.234) THEN
                NUMLEV=0
              ELSEIF(KEYPDS(6,NN).EQ.224) THEN
                NUMLEV=0
              ELSEIF(KEYPDS(6,NN).EQ.214) THEN
                NUMLEV=0
              ELSEIF(KEYPDS(6,NN).EQ.111) THEN
                NUMLEV=0
              ELSEIF(KEYPDS(6,NN).EQ.105) THEN
                NUMLEV=0
              ELSE
                NUMLEV=1
              ENDIF
            ENDIF
C
            IPDS5=1
            IF(KEYPDS(5,NN).GE.10) IPDS5=2
            IF(KEYPDS(5,NN).GE.100) IPDS5=3
            IPDS6=1
            IF(KEYPDS(6,NN).GE.10) IPDS6=2
            IF(KEYPDS(6,NN).GE.100) IPDS6=3
            K7=KZERO
            IF(NUMLEV.EQ.0) THEN
              K7=KEYPDS(7,NN)
            ENDIF
            IPDS7=1
            IF(K7.GE.10) IPDS7=2
            IF(K7.GE.100) IPDS7=3
            IF(K7.GE.1000) IPDS7=4
            IF(K7.GE.10000) IPDS7=5
            LENC=MIN(80-8-1-3-1-IPDS5-1-IPDS6-1-1-1,64)
            WRITE(FMT,10001) IPDS5,IPDS6,IPDS7,LENC
10001       FORMAT(16H(A8,1H ,I3,1H ,I,I1,6H,1H,,I,I1,6H,1H,,I,I1,
     1              6H,1H ,A,I2,1H))
            WRITE(LUCTL,FMT) LABBR(NN)(1:8),NUMLEV,
     1            KEYPDS(5,NN),KEYPDS(6,NN),K7,LABEL(1,NN)(1:LENC)
          ELSE
            K7=99
            IF(NUMLEV.EQ.0) THEN
              K7=KEYPDS(7,NN)
            ENDIF
            WRITE(LUCTL,'(A8,1H ,2I5,1H ,A64)') 
     1            LABBR(NN)(1:8),NUMLEV,K7,LABEL(1,NN)(1:64)
          ENDIF
  811     CONTINUE
        ENDDO
C
        WRITE(LUCTL,'(7Hendvars)')
C
        INQUIRE(LUCTL,NAME=FXNAME)
        WRITE(LUPTR,*) ' ctl file ',FXNAME(1:10),' created'
        PRINT *,' ctl file ',FXNAME(1:10),' created'
C
      ENDDO
C
      RETURN
      END
      SUBROUTINE MER2LL(RMRIN,IMXIN,JMXIN,DLON,DLAT,RLATIN,WLON,RSLAT,
     1                  LGLOBL,
     2                  XLONW,XLATN,DXLON,DXLAT,
     3                  REGOUT,IMXOUT,JMXOUT,UNDEF,LUPTR)
C
      LOGICAL LGLOBL
C
      SAVE
C                                                                       
C  INTERPOLATION FROM Mercator GRID TO LAT/LON GRID                
C                                                                       
      DIMENSION RMRIN(IMXIN,JMXIN)                                   
C                                                                       
      DIMENSION REGOUT(IMXOUT,JMXOUT)                                     
C
      DIMENSION RMRL(500),REGL(500)                                     
      DIMENSION RINLON(1000)
C
      DIMENSION IINDX1(1000)                                            
      DIMENSION IINDX2(1000)                                            
      DIMENSION JINDX1(500)                                             
      DIMENSION JINDX2(500)                                             
      DIMENSION DDX(1000)                                               
      DIMENSION DDY(500)                                                
C                                                                       
      DATA IFP/0/                                                       
C                                                                       
      IF(IFP.NE.0) GO TO 111                                            
      IFP=1                                                             
C
      WRITE(LUPTR,*) 'IMXIN=',IMXIN,' JMXIN=',JMXIN
      WRITE(LUPTR,*) 'DLON=',DLON,' DLAT=',DLAT,
     1               'WLON=',WLON,' RLATIN=',RLATIN,' RSLAT=',RSLAT,
     2               ' LGLOBL=',LGLOBL
      WRITE(LUPTR,*) 'XLATN=',XLATN,' XLONW=',XLONW,
     1               ' DXLAT=',DXLAT,' DXLON=',DXLON
      WRITE(LUPTR,*) 'IMXOUT=',IMXOUT,' JMXOUT=',JMXOUT
C
      DO I=1,IMXIN
        RINLON(I)=WLON+FLOAT(I-1)*DLON
      ENDDO
C                                                                       
      CALL MERCTR(JMXIN,RLATIN,DLAT,RSLAT,RNLAT,RMRL,LUPTR)
C                                                                       
      DO J=1,JMXOUT                                                  
        REGL(J)=XLATN-FLOAT(J-1)*DXLAT
      ENDDO                                                          
C                                                                       
      DXIN =360./FLOAT(IMXIN)                                          
C
C  FIND I-INDEX FOR INTERPOLATION
C
      DO I=1,IMXOUT
        ALAMD=XLONW+FLOAT(I-1)*DXLON
        IF(WLON.LT.0.0) THEN
          IF(ALAMD.GT.180.0) ALAMD=ALAMD-360.0
        ELSE
          IF(ALAMD.LT.0.) ALAMD=360.+ALAMD
        ENDIF
        IF(.NOT.LGLOBL.AND.ALAMD.GT.RINLON(IMXIN)) THEN
          I1=-999
          I2=-999
          DDX(I)=0.
        ELSEIF(.NOT.LGLOBL.AND.ALAMD.LT.RINLON(1)) THEN
          I1=-999
          I2=-999
          DDX(I)=0.
        ELSE
          DO II=1,IMXIN
            IF(ALAMD.GT.RINLON(II)) GO TO 35
            IX=II
            GO TO 32
   35       CONTINUE
          ENDDO
          IF(.NOT.LGLOBL) THEN
            I1=IMXIN
            I2=IMXIN-1
          ELSE
            I1=360.0/DLOIN+0.50
            IF(I1.GT.IMXIN) I1=IMXIN
            I2=1
          ENDIF
          GO TO 34
   32     CONTINUE
          IF(IX.GE.2) GO TO 33
          IF(.NOT.LGLOBL) THEN
            I1=2
            I2=1
          ELSE
            I1=360.0/DLOIN+0.50
            IF(I1.GT.IMXIN) I1=IMXIN
            I2=1
          ENDIF
          GO TO 34
   33     CONTINUE
          I2=IX
          I1=I2-1
   34     CONTINUE
          DENOM=RINLON(I2)-RINLON(I1)
          IF(DENOM.LT.0.0) DENOM=DENOM+360.0
          RNUME=ALAMD-RINLON(I1)
          IF(RNUME.LT.0.0) RNUME=RNUME+360.0
          DDX(I)=RNUME/DENOM
        ENDIF
        IINDX1(I)=I1
        IINDX2(I)=I2
      ENDDO
C
C  FIND J-INDEX FOR INTERPLATION
C
      JQ=1
      DO J=1,JMXOUT
        APHI=REGL(J)
        IF(.NOT.LGLOBL.AND.APHI.GT.RMRL(1)) THEN
          J1=-999
          J2=-999
          DDY(J)=0.
        ELSEIF(.NOT.LGLOBL.AND.APHI.LT.RMRL(JMXIN)) THEN
          J1=-999
          J2=-999
          DDY(J)=0.
        ELSE
          DO JJ=1,JMXIN
            IF(APHI.LT.RMRL(JJ)) GO TO 50
            JQ=JJ
            GO TO 42
   50       CONTINUE
          ENDDO
          J1=JMXIN
          J2=JMXIN
          GO TO 44
   42     CONTINUE
          IF(JQ.GE.2) GO TO 43
          J1=1
          J2=1
          GO TO 44
   43     CONTINUE
          J2=JQ
          J1=JQ-1
   44     CONTINUE
          IF(J2.NE.J1) THEN
             DDY(J)=(APHI-RMRL(J1))/(RMRL(J2)-RMRL(J1))
          ELSE
            IF(J1.EQ.1) THEN
              IF(ABS(90.0-RMRL(J1)).GT.0.0010) THEN
                DDY(J)=(APHI-RMRL(J1))/(90.0-RMRL(J1))
              ELSE
                DDY(J)=0.0
              ENDIF
            ENDIF
            IF(J1.EQ.JMXIN) THEN
              IF(ABS(-90.0-RMRL(J1)).GT.0.001) THEN
                DDY(J)=(APHI-RMRL(J1))/(-90.0-RMRL(J1))
              ELSE
                DDY(J)=0.0
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        JINDX1(J)=J1
        JINDX2(J)=J2
      ENDDO
C
  111 CONTINUE                                                          
C                                                                       
C     WRITE(LUPTR,*) 'IINDX1'                                                  
C     WRITE(LUPTR,*) (IINDX1(N),N=1,IMXOUT)                                    
C     WRITE(LUPTR,*) 'IINDX2'                                                  
C     WRITE(LUPTR,*) (IINDX2(N),N=1,IMXOUT)                                    
C     WRITE(LUPTR,*) 'JINDX1'                                                  
C     WRITE(LUPTR,*) (JINDX1(N),N=1,JMXOUT)                                    
C     WRITE(LUPTR,*) 'JINDX2'                                                  
C     WRITE(LUPTR,*) (JINDX2(N),N=1,JMXOUT)                                    
C     WRITE(LUPTR,*) 'DDY'                                                     
C     WRITE(LUPTR,*) (DDY(N),N=1,JMXOUT)                                       
C     WRITE(LUPTR,*) 'DDX'                                                     
C     WRITE(LUPTR,*) (DDX(N),N=1,JMXOUT)                                       
C                                                                       
      IF(LGLOBL) THEN
        SUM1=0.0
        SUM2=0.0
        DO 80 I=1,IMXIN
          SUM1=SUM1+RMRIN(I,1)
          SUM2=SUM2+RMRIN(I,JMXIN)
   80   CONTINUE
        SUMN=SUM1/FLOAT(IMXIN)
        SUMS=SUM2/FLOAT(IMXIN)
        DO I=1,IMXIN
          IF(RMRIN(I,1).EQ.UNDEF) SUMN=UNDEF
          IF(RMRIN(I,JMXIN).EQ.UNDEF) SUMS=UNDEF
        ENDDO
      ENDIF
C
C  QUASI-BILINEAR INTERPOLATION
C
      DO 70 J=1,JMXOUT
        Y=DDY(J)
        J1=JINDX1(J)
        J2=JINDX2(J)
        DO 70 I=1,IMXOUT
          X=DDX(I)
          I1=IINDX1(I)
          I2=IINDX2(I)
          IF(I1.EQ.-999.OR.I2.EQ.-999.OR.J1.EQ.-999.OR.J2.EQ.-999) THEN
            REGOUT(I,J)=UNDEF
          ELSEIF(RMRIN(I1,J1).EQ.UNDEF.OR.RMRIN(I2,J1).EQ.UNDEF.OR.
     1           RMRIN(I1,J2).EQ.UNDEF.OR.RMRIN(I2,J2).EQ.UNDEF) THEN
           	 REGOUT(I,J)=UNDEF
          ELSE
            WI1J1=(1.0-X)*(1.0-Y)
            WI2J1=     X *(1.0-Y)
            WI1J2=(1.0-X)*      Y
            WI2J2=     X *      Y
            WSUM  =WI1J1+WI2J1+WI1J2+WI2J2
            WSUMIV=1./WSUM
            IF(J1.NE.J2) THEN
              REGOUT(I,J)=(WI1J1*RMRIN(I1,J1)+WI2J1*RMRIN(I2,J1)+
     1                     WI1J2*RMRIN(I1,J2)+WI2J2*RMRIN(I2,J2))*WSUMIV
            ELSE
              IF(J1.EQ.1) THEN
                REGOUT(I,J)=(WI1J1*SUMN        +WI2J1*SUMN        +
     1                     WI1J2*RMRIN(I1,J2)+WI2J2*RMRIN(I2,J2))*WSUMIV
              ENDIF
              IF(J1.EQ.JMXIN) THEN
                REGOUT(I,J)=(WI1J1*RMRIN(I1,J1)+WI2J1*RMRIN(I2,J1)+
     1                       WI1J2*SUMS        +WI2J2*SUMS        )*WSUMIV
              ENDIF
            ENDIF
          ENDIF
   70 CONTINUE
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE GAU2LL(GAUIN,IMXIN,JMXIN,
     1                  XLONW,XLATN,DXLON,DXLAT,REGOUT,IMXOUT,JMXOUT,
     2                  UNDEF,LUPTR)
C
      SAVE
C                                                                       
C  INTERPOLATION FROM LAT/LON GRID TO OTHER LAT/LON GRID                
C                                                                       
      DIMENSION GAUIN (IMXIN,JMXIN)                                   
C                                                                       
      DIMENSION REGOUT(IMXOUT,JMXOUT)                                     
      DIMENSION GAUL(500),REGL(500)                                     
      DIMENSION IINDX1(1000)                                            
      DIMENSION IINDX2(1000)                                            
      DIMENSION JINDX1(500)                                             
      DIMENSION JINDX2(500)                                             
      DIMENSION DDX(1000)                                               
      DIMENSION DDY(500)                                                
C                                                                       
      DATA IFP/0/                                                       
C                                                                       
      IF(IFP.NE.0) GO TO 111                                            
      IFP=1                                                             
C
      WRITE(LUPTR,*) 'IMXIN=',IMXIN,' JMXIN=',JMXIN
      WRITE(LUPTR,*) 'XLATN=',XLATN,' XLONW=',XLONW,
     1               ' DXLAT=',DXLAT,' DXLON=',DXLON
      WRITE(LUPTR,*) 'IMXOUT=',IMXOUT,' JMXOUT=',JMXOUT
C                                                                       
      CALL GAULAT(GAUL,JMXIN)                                           
C                                                                       
      DO 20 J=1,JMXOUT                                                  
      REGL(J)=XLATN-FLOAT(J-1)*DXLAT                                       
   20 CONTINUE                                                          
C                                                                       
      DXIN =360./FLOAT(IMXIN)                                          
C                                                                       
      DO 30 I=1,IMXOUT                                                  
      ALAMD=XLONW+FLOAT(I-1)*DXLON
      IF(ALAMD.LT.0.) ALAMD=360.+ALAMD
      I1=ALAMD/DXIN+1.001                                               
      IF(I1.GT.IMXIN) I1=1                                              
      IINDX1(I)=I1                                                      
      I2=I1+1                                                           
      IF(I2.GT.IMXIN) I2=1                                              
      IINDX2(I)=I2                                                      
      DDX(I)=(ALAMD-FLOAT(I1-1)*DXIN)/DXIN                              
   30 CONTINUE                                                          
C                                                                       
      DO 40 J=1,JMXOUT                                                  
      APHI=REGL(J)                                                      
      DO 50 JJ=1,JMXIN                                                  
      IF(APHI.LT.GAUL(JJ)) GO TO 50                                     
      J2=JJ                                                             
      GO TO 42                                                          
   50 CONTINUE                                                          
      J2=JMXIN                                                              
   42 CONTINUE                                                          
      IF(J2.GT.2) GO TO 43                                              
      J1=1                                                              
      J2=2                                                              
      GO TO 44                                                          
   43 CONTINUE                                                          
      IF(J2.LE.JMXIN) GO TO 45                                          
      J1=JMXIN-1                                                        
      J2=JMXIN                                                          
      GO TO 44                                                          
   45 CONTINUE                                                          
      J1=J2-1                                                           
   44 CONTINUE                                                          
      JINDX1(J)=J1                                                      
      JINDX2(J)=J2                                                      
      DDY(J)=(APHI-GAUL(J1))/(GAUL(J2)-GAUL(J1))                        
   40 CONTINUE                                                          
C                                                                       
  111 CONTINUE                                                          
C                                                                       
C     WRITE(LUPTR,*) 'IINDX1'                                                  
C     WRITE(LUPTR,*) (IINDX1(N),N=1,IMXOUT)                                    
C     WRITE(LUPTR,*) 'IINDX2'                                                  
C     WRITE(LUPTR,*) (IINDX2(N),N=1,IMXOUT)                                    
C     WRITE(LUPTR,*) 'JINDX1'                                                  
C     WRITE(LUPTR,*) (JINDX1(N),N=1,JMXOUT)                                    
C     WRITE(LUPTR,*) 'JINDX2'                                                  
C     WRITE(LUPTR,*) (JINDX2(N),N=1,JMXOUT)                                    
C     WRITE(LUPTR,*) 'DDY'                                                     
C     WRITE(LUPTR,*) (DDY(N),N=1,JMXOUT)                                       
C     WRITE(LUPTR,*) 'DDX'                                                     
C     WRITE(LUPTR,*) (DDX(N),N=1,JMXOUT)                                       
C                                                                       
      DO 60 J=1,JMXOUT                                                  
      Y=DDY(J)                                                          
      J1=JINDX1(J)                                                      
      J2=JINDX2(J)                                                      
      DO 60 I=1,IMXOUT                                                  
      X=DDX(I)                                                          
      I1=IINDX1(I)                                                      
      I2=IINDX2(I)                                                      
      IF(GAUIN(I1,J1).EQ.UNDEF.OR.GAUIN(I2,J1).EQ.UNDEF.OR.
     1   GAUIN(I1,J2).EQ.UNDEF.OR.GAUIN(I2,J2).EQ.UNDEF) THEN
        REGOUT(I,J)=UNDEF
      ELSE
        REGOUT(I,J)=(1.-X)*(1.-Y)*GAUIN(I1,J1)+(1.-Y)*X*GAUIN(I2,J1)+     
     1           (1.-X)*Y*GAUIN(I1,J2)+X*Y*GAUIN(I2,J2)                 
      ENDIF
   60 CONTINUE                                                          
C                                                                       
      SUM1=0.                                                           
      SUM2=0.                                                           
      DO 70 I=1,IMXIN                                                   
      SUM1=SUM1+GAUIN(I,1)                                              
      SUM2=SUM2+GAUIN(I,JMXIN)                                          
   70 CONTINUE                                                          
      SUM1=SUM1/FLOAT(IMXIN)                                            
      SUM2=SUM2/FLOAT(IMXIN)                                            
      DO I=1,IMXIN                                                   
        IF(GAUIN(I,1).EQ.UNDEF) SUM1=UNDEF
        IF(GAUIN(I,JMXIN).EQ.UNDEF) SUM2=UNDEF
      ENDDO
C                                                                       
      DO 80 I=1,IMXOUT                                                  
      IF(ABS(REGL(1)).EQ.90.) THEN
        REGOUT(I,     1)=SUM1                                             
      ENDIF
      IF(ABS(REGL(JMXOUT)).EQ.90.) THEN
        REGOUT(I,JMXOUT)=SUM2                                             
      ENDIF
   80 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE GETIJNP(TRUTH,ORIENT,DELX,DELY,FLAT,FLON,XPT,YPT)
C
C   TRUTH    60.0    /* TRUE LATITUDE FOR POLAR PROJECTION */
C   ORIENT -105.     /* ROTATE DEGREE FOR XY PLANE TO LONGITUDE */
C
      PARAMETER(CENLAT=90.,CENLON=0.)
      PARAMETER(RERTH=6.3712E+6)
C
      DIMENSION X(2),Y(2)
C
      HFPI = DASIN(1.0D0)
      PI = 2.0 * HFPI
      TWOPI = 2.0 * PI
      RAD = PI / 180.
C
C --------- SETUP REGIONAL LAT/LON AND MAP FACTOR -----
C
      TRUETH  = TRUTH * RAD
      DLAMDA0 = ORIENT * RAD + HFPI
      A2 =  RERTH * ( 1.0 + SIN(TRUETH) )
C
C =========
C
      DO II=1,2
        IF(II.EQ.1) THEN
          RLAT=FLAT*RAD
          RLON = FLON * RAD - DLAMDA0
        ELSE
          RLAT=90.*RAD
          RLON=-DLAMDA0
        ENDIF
C
        RSOA2 = TAN( (HFPI-RLAT)*0.5 )
        X2PY2 = ( RSOA2 * A2 ) ** 2.0
C
        RLON = AMOD(RLON,TWOPI)
        IF( RLON.LT.0. ) RLON=TWOPI+RLON
        YOX = TAN(RLON)
C
        X(II) = SQRT( X2PY2/(1.+YOX*YOX) )
        Y(II) = SQRT( X2PY2 - X(II)*X(II) )
        IF( RLON.GT.HFPI .AND. RLON.LT. PI+HFPI ) X(II) = -X(II)
        IF( RLON.GT.PI .AND. RLON.LT. TWOPI ) Y(II) = -Y(II)
      ENDDO

      XPT = (X(2) - X(1))/DELX + 1.
      YPT = (Y(2) - Y(1))/DELY + 1.
C
      RETURN
      END
      SUBROUTINE STR2LL(STRIN,IM,JM,
     1                  TRUTH,ORIENT,DELX,DELY,BOTMGRD,RLFTGRD,
     2                  XLONW,XLATN,DXLON,DXLAT,OUTLL,IOUT,JOUT,
     3                  UNDEF,LUPTR)
C
C  Interpolates from polar stereographic to lat/lon grids
C
      DIMENSION STRIN(IM,JM)
C
      DIMENSION OUTLL(IOUT,JOUT)
C
      WRITE(LUPTR,*) 'IM=',IM,' JM=',JM,' STRIN(1,1)=',STRIN(1,1)
      WRITE(LUPTR,*) 'TRUTH=',TRUTH,' ORIENT=',ORIENT,
     1               'DELX=',DELX,' DELY=',DELY,
     2               'BOTMGRD=',BOTMGRD,' RLFTGRD=',RLFTGRD
      WRITE(LUPTR,*) 'XLATN=',XLATN,' XLONW=',XLONW,
     1               ' DXLAT=',DXLAT,' DXLON=',DXLON
C
      DO J=1,JM/2
        JJ=JM+1-J
        DO I=1,IM
          TMP=STRIN(I,JJ)
          STRIN(I,JJ)=STRIN(I,J)
          STRIN(I,J)=TMP
        ENDDO
      ENDDO
C
      DO J=1,JOUT
        DO I=1,IOUT
          FLAT=XLATN-(J-1)*DXLAT
          FLON=XLONW+(I-1)*DXLON
          CALL LL2PT(TRUTH,ORIENT,DELX,DELY,BOTMGRD,RLFTGRD,
     1               FLAT,FLON,XPT,YPT)
          IF(XPT.GE.1..AND.XPT.LE.FLOAT(IM).AND.
     1       YPT.GE.1..AND.YPT.LE.FLOAT(JM)) THEN
            X=XPT-INT(XPT)
            Y=YPT-INT(YPT)
            II=INT(XPT)
            JJ=INT(YPT)
            I1=II
            I2=II+1
            J1=JJ
            J2=JJ+1
            IF(STRIN(I1,J1).EQ.UNDEF.OR.STRIN(I2,J1).EQ.UNDEF.OR.
     1         STRIN(I1,J2).EQ.UNDEF.OR.STRIN(I2,J2).EQ.UNDEF) THEN
              OUTLL(I,J)=UNDEF
            ELSE
              OUTLL(I,J)=(1.-X)*(1.-Y)*STRIN(II  ,JJ  )                     
     1                  +    X *(1.-Y)*STRIN(II+1,JJ  )                     
     2                  +(1.-X)*    Y *STRIN(II  ,JJ+1)                     
     3                  +    X *    Y *STRIN(II+1,JJ+1)                     
            ENDIF
          ELSE
            OUTLL(I,J)=UNDEF 
          ENDIF
        ENDDO
      ENDDO
C
C     DO J=1,JOUT/2
C       JJ=JOUT+1-J
C       DO I=1,IOUT
C         TMP=OUTLL(I,JJ)
C         OUTLL(I,JJ)=OUTLL(I,J)
C         OUTLL(I,J)=TMP
C       ENDDO
C     ENDDO
C
      WRITE(LUPTR,*) ' IOUT=',IOUT,' JOUT=',JOUT,
     1               ' OUTLL(1,1)=',OUTLL(1,1)
C
      RETURN
      END
      SUBROUTINE LL2LL(REGIN,IMXIN,JMXIN,DLOIN,DLAIN,WLON,RNLAT,LGLOBL,
     1                 XLONW,XLATN,DXLON,DXLAT,REGOUT,IMXOUT,JMXOUT,
     2                 UNDEF,LUPTR)
C
      SAVE
C
C  INTERPOLATION FROM LAT/LON OR GAUSSIAN GRID TO OTHER LAT/LON GRID
C
      DIMENSION REGIN (IMXIN ,JMXIN )
      DIMENSION REGOUT(IMXOUT,JMXOUT)
C
      LOGICAL LGLOBL
C
      DIMENSION RINLAT(500),OUTLAT(500)
      DIMENSION RINLON(1000)
C
      DIMENSION IINDX1(1000)
      DIMENSION IINDX2(1000)
      DIMENSION JINDX1(500)
      DIMENSION JINDX2(500)
C
      DIMENSION DDX(1000)
      DIMENSION DDY(500)
C
      DATA IFPI,JFPI,IFPO,JFPO,RFP1,RFP2/4*0,2*0./
C
      WRITE(LUPTR,*) 'IMXIN=',IMXIN,' JMXIN=',JMXIN
      WRITE(LUPTR,*) 'DLOIN=',DLOIN,' DLAIN=',DLAIN,
     1               'WLON=',WLON,' RNLAT=',RNLAT,' LGLOBL=',LGLOBL
      WRITE(LUPTR,*) 'XLATN=',XLATN,' XLONW=',XLONW,
     1               ' DXLAT=',DXLAT,' DXLON=',DXLON
      WRITE(LUPTR,*) 'IMXOUT=',IMXOUT,' JMXOUT=',JMXOUT
C
      IF(IMXIN.EQ.1.OR.JMXIN.EQ.1) THEN
      DO 1 J=1,JMXOUT
      DO 1 I=1,IMXOUT
        REGOUT(I,J)=0.0
    1 CONTINUE
      RETURN
      ENDIF
C
      IF(DLOIN.EQ.0.0.OR.DLAIN.EQ.0.0) THEN
        PRINT *,'DLOIN OR DLAIN IS ZERO'
        CALL ABORT
      ENDIF
C
      IF(IFPI.EQ.IMXIN .AND.JFPI.EQ.JMXIN .AND.
     1   IFPO.EQ.IMXOUT.AND.JFPO.EQ.JMXOUT.AND.
     2   RFP1.EQ.WLON.AND.RFP2.EQ.RNLAT) GO TO 111
C
      IFPI=IMXIN
      JFPI=JMXIN
      IFPO=IMXOUT
      JFPO=JMXOUT
      RFP1=WLON
      RFP2=RNLAT
C
C     WRITE(LUPTR,*) 'DLOIN=',DLOIN
C     WRITE(LUPTR,*) 'DLAIN=',DLAIN
C     WRITE(LUPTR,*) 'WLON=',WLON
C     WRITE(LUPTR,*) 'RNLAT=',RNLAT
C
      DO 5 J=1,JMXIN
        RINLAT(J)=RNLAT-FLOAT(J-1)*DLAIN
    5 CONTINUE
C
C     WRITE(LUPTR,*) 'RINLAT='
C     WRITE(LUPTR,*) (RINLAT(J),J=1,JMXIN)
C
      DO 7 J=1,JMXOUT
        OUTLAT(J)=XLATN-FLOAT(J-1)*DXLAT
    7 CONTINUE
C
C     WRITE(LUPTR,*) 'OUTLAT='
C     WRITE(LUPTR,*) (OUTLAT(J),J=1,JMXOUT)
C
      DO 15 I=1,IMXIN
        RINLON(I)=WLON+FLOAT(I-1)*DLOIN
   15 CONTINUE
C
C     WRITE(LUPTR,*) 'RINLON='
C     WRITE(LUPTR,*) (RINLON(I),I=1,IMXIN)
C
C  FIND I-INDEX FOR INTERPLATION
C
      DO 30 I=1,IMXOUT
        ALAMD=XLONW+FLOAT(I-1)*DXLON
        IF(WLON.LT.0.0) THEN
          IF(ALAMD.GT.180.0) ALAMD=ALAMD-360.0
        ELSE
          IF(ALAMD.LT.0.) ALAMD=360.+ALAMD
        ENDIF
        IF(.NOT.LGLOBL.AND.ALAMD.GT.RINLON(IMXIN)) THEN
          I1=-999
          I2=-999
          DDX(I)=0.
        ELSEIF(.NOT.LGLOBL.AND.ALAMD.LT.RINLON(1)) THEN
          I1=-999
          I2=-999
          DDX(I)=0.
        ELSE
          DO 35 II=1,IMXIN
            IF(ALAMD.GT.RINLON(II)) GO TO 35
            IX=II
            GO TO 32
   35     CONTINUE
          IF(.NOT.LGLOBL) THEN
            I1=IMXIN
            I2=IMXIN-1
          ELSE
            I1=360.0/DLOIN+0.50
            IF(I1.GT.IMXIN) I1=IMXIN
            I2=1
          ENDIF
          GO TO 34
   32     CONTINUE
          IF(IX.GE.2) GO TO 33
          IF(.NOT.LGLOBL) THEN
            I1=2
            I2=1
          ELSE
            I1=360.0/DLOIN+0.50
            IF(I1.GT.IMXIN) I1=IMXIN
            I2=1
          ENDIF
          GO TO 34
   33     CONTINUE
          I2=IX
          I1=I2-1
   34     CONTINUE
          DENOM=RINLON(I2)-RINLON(I1)
          IF(DENOM.LT.0.0) DENOM=DENOM+360.0
          RNUME=ALAMD-RINLON(I1)
          IF(RNUME.LT.0.0) RNUME=RNUME+360.0
          DDX(I)=RNUME/DENOM
        ENDIF
        IINDX1(I)=I1
        IINDX2(I)=I2
   30 CONTINUE
C
C  FIND J-INDEX FOR INTERPLATION
C
      JQ=1
      DO 40 J=1,JMXOUT
        APHI=OUTLAT(J)
        IF(.NOT.LGLOBL.AND.APHI.GT.RINLAT(1)) THEN
          J1=-999
          J2=-999
          DDY(J)=0.
        ELSEIF(.NOT.LGLOBL.AND.APHI.LT.RINLAT(JMXIN)) THEN
          J1=-999
          J2=-999
          DDY(J)=0.
        ELSE
          DO 50 JJ=1,JMXIN
            IF(APHI.LT.RINLAT(JJ)) GO TO 50
            JQ=JJ
            GO TO 42
   50     CONTINUE
          J1=JMXIN
          J2=JMXIN
          GO TO 44
   42     CONTINUE
          IF(JQ.GE.2) GO TO 43
          J1=1
          J2=1
          GO TO 44
   43     CONTINUE
          J2=JQ
          J1=JQ-1
   44     CONTINUE
          IF(J2.NE.J1) THEN
             DDY(J)=(APHI-RINLAT(J1))/(RINLAT(J2)-RINLAT(J1))
          ELSE
            IF(J1.EQ.1) THEN
              IF(ABS(90.0-RINLAT(J1)).GT.0.0010) THEN
                DDY(J)=(APHI-RINLAT(J1))/(90.0-RINLAT(J1))
              ELSE
                DDY(J)=0.0
              ENDIF
            ENDIF
            IF(J1.EQ.JMXIN) THEN
              IF(ABS(-90.0-RINLAT(J1)).GT.0.001) THEN
                DDY(J)=(APHI-RINLAT(J1))/(-90.0-RINLAT(J1))
              ELSE
                DDY(J)=0.0
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        JINDX1(J)=J1
        JINDX2(J)=J2
   40 CONTINUE
C
C     WRITE(LUPTR,*) 'IINDX1'
C     WRITE(LUPTR,*) (IINDX1(N),N=1,IMXOUT)
C     WRITE(LUPTR,*) 'IINDX2'
C     WRITE(LUPTR,*) (IINDX2(N),N=1,IMXOUT)
C     WRITE(LUPTR,*) 'JINDX1'
C     WRITE(LUPTR,*) (JINDX1(N),N=1,JMXOUT)
C     WRITE(LUPTR,*) 'JINDX2'
C     WRITE(LUPTR,*) (JINDX2(N),N=1,JMXOUT)
C     WRITE(LUPTR,*) 'DDY'
C     WRITE(LUPTR,*) (DDY(N),N=1,JMXOUT)
C     WRITE(LUPTR,*) 'DDX'
C     WRITE(LUPTR,*) (DDX(N),N=1,JMXOUT)
C
  111 CONTINUE
C
      IF(LGLOBL) THEN
        SUM1=0.0
        SUM2=0.0
        DO 80 I=1,IMXIN
          SUM1=SUM1+REGIN(I,1)
          SUM2=SUM2+REGIN(I,JMXIN)
   80   CONTINUE
        SUMN=SUM1/FLOAT(IMXIN)
        SUMS=SUM2/FLOAT(IMXIN)
      ENDIF
C
C  QUASI-BILINEAR INTERPOLATION
C
      DO 70 J=1,JMXOUT
        Y=DDY(J)
        J1=JINDX1(J)
        J2=JINDX2(J)
        DO 70 I=1,IMXOUT
          X=DDX(I)
          I1=IINDX1(I)
          I2=IINDX2(I)
          IF(I1.EQ.-999.OR.I2.EQ.-999.OR.J1.EQ.-999.OR.J2.EQ.-999) THEN
           	 REGOUT(I,J)=UNDEF
          ELSEIF(REGIN(I1,J1).EQ.UNDEF.OR.REGIN(I2,J1).EQ.UNDEF.OR.
     1           REGIN(I1,J2).EQ.UNDEF.OR.REGIN(I2,J2).EQ.UNDEF) THEN
           	 REGOUT(I,J)=UNDEF
          ELSE
            WI1J1=(1.0-X)*(1.0-Y)
            WI2J1=     X *(1.0-Y)
            WI1J2=(1.0-X)*      Y
            WI2J2=     X *      Y
            WSUM  =WI1J1+WI2J1+WI1J2+WI2J2
            WSUMIV=1./WSUM
            IF(J1.NE.J2) THEN
              REGOUT(I,J)=(WI1J1*REGIN(I1,J1)+WI2J1*REGIN(I2,J1)+
     1                     WI1J2*REGIN(I1,J2)+WI2J2*REGIN(I2,J2))*WSUMIV
            ELSE
              IF(J1.EQ.1) THEN
                REGOUT(I,J)=(WI1J1*SUMN        +WI2J1*SUMN        +
     1                     WI1J2*REGIN(I1,J2)+WI2J2*REGIN(I2,J2))*WSUMIV
              ENDIF
              IF(J1.EQ.JMXIN) THEN
                REGOUT(I,J)=(WI1J1*REGIN(I1,J1)+WI2J1*REGIN(I2,J1)+
     1                       WI1J2*SUMS        +WI2J2*SUMS        )*WSUMIV
              ENDIF
            ENDIF
          ENDIF
   70 CONTINUE
C
      RETURN
      END
      SUBROUTINE MAXMIN(DATA,NDATA,LUPTR)
C
      DIMENSION DATA(NDATA)
C
      XXMAX=-1.E20
      XXMIN= 1.E20
      DO IJ=1,NDATA
        IF(DATA(IJ).GT.XXMAX) THEN
          XXMAX=DATA(IJ)
          MAXIJ=IJ
        ENDIF
        IF(DATA(IJ).LE.XXMIN) THEN
          XXMIN=DATA(IJ)
          MINIJ=IJ
        ENDIF
      ENDDO
C
      WRITE(LUPTR,*) ' MAX=',XXMAX,' at ',MAXIJ,
     1               ' MIN=',XXMIN,' at ',MINIJ
C
      RETURN
      END
      SUBROUTINE INCDATE(DATES,LEND,MFHMIN,LUPTR)
C
      CHARACTER*24 DATES
      CHARACTER*3 MIN
C
      CHARACTER*3 UMON(12),LMON(12)
      DIMENSION NDY(12)
C
      DATA UMON/'JAN','FEB','MAR','APR','MAY','JUN','JUL','AUG',
     1          'SEP','OCT','NOV','DEC'/
      DATA LMON/'jan','feb','mar','apr','may','jun','jul','aug',
     1          'sep','oct','nov','dec'/
      DATA NDY/31,28,31,30,31,30,31,31,30,31,30,31/
C
C   00:12Z6JUL93/00:12Z06JUL93/12Z6JUL93/12Z06JUL93
C
      WRITE(LUPTR,*) 'DATES=',DATES,' LEND=',LEND
C
      IF(DATES(3:3).EQ.':') THEN
        MIN=DATES(3:3)
        READ(DATES(4:5),'(I2)') IHR
        DO NN=1,12
          IF(DATES(8:10).EQ.UMON(NN).OR.
     1       DATES(8:10).EQ.LMON(NN)) THEN
            IMON=NN
            READ(DATES(7:7),'(I1)') IDY
            IF(LEND.EQ.14) THEN
              READ(DATES(11:14),'(I4)') IYR
            ELSE
              READ(DATES(11:12),'(I2)') IYR
            ENDIF
            GO TO 1
          ENDIF
        ENDDO
        DO NN=1,12
          IF(DATES(9:11).EQ.UMON(NN).OR.
     1       DATES(9:11).EQ.LMON(NN)) THEN
            IMON=NN
            READ(DATES(7:8),'(I2)') IDY
            IF(LEND.EQ.15) THEN
              READ(DATES(11:14),'(I4)') IYR
            ELSE
              READ(DATES(12:13),'(I2)') IYR
            ENDIF
            GO TO 1
          ENDIF
        ENDDO
      ELSE
        MIN='   '
        READ(DATES(1:2),'(I2)') IHR
        WRITE(LUPTR,*) 'DATES(1:2)=',DATES(1:2)
        DO NN=1,12
          IF(DATES(5:7).EQ.UMON(NN).OR.
     1       DATES(5:7).EQ.LMON(NN)) THEN
            IMON=NN
            READ(DATES(4:4),'(I1)') IDY
        WRITE(LUPTR,*) 'DATES(4:4)=',DATES(4:4)
            IF(LEND.EQ.11) THEN
              READ(DATES(8:11),'(I4)') IYR
        WRITE(LUPTR,*) 'DATES(8:11)=',DATES(8:11)
            ELSE
              READ(DATES(8:9),'(I2)') IYR
            ENDIF
            GO TO 1
          ENDIF
        ENDDO
        DO NN=1,12
          IF(DATES(6:8).EQ.UMON(NN).OR.
     1       DATES(6:8).EQ.LMON(NN)) THEN
            IMON=NN
            READ(DATES(4:5),'(I2)') IDY
        WRITE(LUPTR,*) 'DATES(4:5)=',DATES(4:5)
            IF(LEND.EQ.12) THEN
              READ(DATES(9:12),'(I4)') IYR
        WRITE(LUPTR,*) 'DATES(9:12)=',DATES(9:12)
            ELSE
              READ(DATES(9:10),'(I2)') IYR
        WRITE(LUPTR,*) 'DATES(9:10)=',DATES(9:10)
            ENDIF
            GO TO 1
          ENDIF
        ENDDO
      ENDIF
    1 CONTINUE
C
      WRITE(LUPTR,*) 'IYR,IMON,IDY,IHR,MFHMIN=',IYR,IMON,IDY,IHR,MFHMIN
C
      IF(MFHMIN.EQ.9999) MFHMIN=0
C
      IHR=IHR+MFHMIN
      IF(IHR.GE.24) THEN
        IHR=IHR-24
        IDY=IDY+1
        NDYX=NDY(NN)
        IF(NN.EQ.2.AND.MOD(IYR,4).EQ.0.AND.MOD(IYR,100).NE.0) THEN
          NDYX=29
        ENDIF
        IF(IDY.GT.NDYX) THEN
          IMON=IMON+1
          IF(IMON.GT.12) THEN
            IMON=IMON-12
            IYR=IYR+1
          ENDIF
        ENDIF
      ENDIF
C
      WRITE(LUPTR,*) 'Incremented IYR,IMON,IDY,IHR=',IYR,IMON,IDY,IHR
C
      DO I=1,24
        DATES(I:I)=' '
      ENDDO
      IF(MIN(1:3).NE.'   ') THEN
        IF(IHR.LT.10.AND.IDY.LT.10) THEN
          WRITE(DATES,'(A3,1H0,I1,1HZ,I1,A3,I2)') MIN,IHR,IDY,
     1                                           UMON(IMON),IYR
        ELSEIF(IHR.GE.10.AND.IDY.LT.10) THEN
          WRITE(DATES,'(A3,I2,1HZ,I1,A3,I2)') MIN,IHR,IDY,
     1                                       UMON(IMON),IYR
        ELSEIF(IHR.LT.10.AND.IDY.GE.10) THEN
          WRITE(DATES,'(A3,1H0,I1,1HZ,I2,A3,I2)') MIN,IHR,IDY,
     1                                           UMON(IMON),IYR
        ELSEIF(IHR.GE.10.AND.IDY.GE.10) THEN
          WRITE(DATES,'(A3,I2,1HZ,I2,A3,I2)') MIN,IHR,IDY,
     1                                       UMON(IMON),IYR
        ENDIF
      ELSE
        IF(IHR.LT.10.AND.IDY.LT.10) THEN
          WRITE(DATES,'(1H0,I1,1HZ,I1,A3,I2)') IHR,IDY,
     1                                        UMON(IMON),IYR
        ELSEIF(IHR.GE.10.AND.IDY.LT.10) THEN
          WRITE(DATES,'(I2,1HZ,I1,A3,I2)') IHR,IDY,
     1                                    UMON(IMON),IYR
        ELSEIF(IHR.LT.10.AND.IDY.GE.10) THEN
          WRITE(DATES,'(1H0,I1,1HZ,I2,A3,I2)') IHR,IDY,
     1                                        UMON(IMON),IYR
        ELSEIF(IHR.GE.10.AND.IDY.GE.10) THEN
          WRITE(DATES,'(I2,1HZ,I2,A3,I2)') IHR,IDY,
     1                                    UMON(IMON),IYR
        ENDIF
      ENDIF
C
      WRITE(LUPTR,*) 'Incremented DATES=',DATES
C
      RETURN
      END
      SUBROUTINE MERCTR(JMAX,PHI0,DPHI,RSLAT,RNLAT,PHI,LUPTR)
C
      DIMENSION PHI(JMAX)
C
      WRITE(LUPTR,*) 'PHI0,DPHI,RSLAT=',PHI0,DPHI,RSLAT
      PI=4.*ATAN(1.)
      RAD=PI/180.
      RADI=180./PI
      ER=6371.2E03
      RCSPHI=ER*COS(PHI0*RAD)
C
      S0=RCSPHI*ALOG(TAN(PI/4.+RSLAT/2.*RAD))
C
      DO J=1,JMAX
        S=S0+FLOAT(J-1)*ABS(DPHI)
        PHI(JMAX-J+1)=2.*RADI*ATAN(EXP(S/RCSPHI))-90.
      ENDDO
C
      RETURN
      END
      SUBROUTINE GAULAT(GAUL,K)                                         
C                                                                       
      DIMENSION A(500)                                                  
      DIMENSION GAUL(1)                                                 
C                                                                       
      ESP=1.E-6                                                       
      C=(1.E0-(2.E0/3.14159265358979E0)**2)*0.25E0                      
      FK=K                                                              
      KK=K/2                                                            
      CALL BSSLZ1(A,KK)                                                 
      DO 30 IS=1,KK                                                     
      XZ=COS(A(IS)/SQRT((FK+0.5E0)**2+C))                               
      ITER=0                                                            
   10 PKM2=1.E0                                                         
      PKM1=XZ                                                           
      ITER=ITER+1                                                       
      IF(ITER.GT.10) GO TO 70                                           
      DO 20 N=2,K                                                       
      FN=N                                                              
      PK=((2.E0*FN-1.E0)*XZ*PKM1-(FN-1.E0)*PKM2)/FN                     
      PKM2=PKM1                                                         
   20 PKM1=PK                                                           
      PKM1=PKM2                                                         
      PKMRK=(FK*(PKM1-XZ*PK))/(1.E0-XZ**2)                              
      SP=PK/PKMRK                                                       
      XZ=XZ-SP                                                          
      AVSP=ABS(SP)                                                      
      IF(AVSP.GT.ESP) GO TO 10                                          
      A(IS)=XZ                                                          
   30 CONTINUE                                                          
      IF(K.EQ.KK*2) GO TO 50                                            
      A(KK+1)=0.E0                                                      
      PK=2.E0/FK**2                                                     
      DO 40 N=2,K,2                                                     
      FN=N                                                              
   40 PK=PK*FN**2/(FN-1.E0)**2                                          
   50 CONTINUE                                                          
      DO 60 N=1,KK                                                      
      L=K+1-N                                                           
      A(L)=-A(N)                                                        
   60 CONTINUE                                                          
C                                                                       
      RADI=180./(4.*ATAN(1.))                                           
      DO 211 N=1,K                                                      
      GAUL(N)=90.-ACOS(A(N))*RADI                                           
  211 CONTINUE                                                          
C
C     PRINT *,'GAUSSIAN LAT (DEG) FOR JMAX=',K 
C     PRINT *,(GAUL(N),N=1,K)                                           
C                                                                       
      RETURN                                                            
   70 WRITE(6,6000)                                                     
 6000 FORMAT(//5X,14HERROR IN GAUAW//)                                  
      STOP                                                              
      END                                                               
      SUBROUTINE LL2PT(TRUTH,ORIENT,DELX,DELY,BOTMGRD,RLFTGRD,
     1                 FLAT,FLON,XPT,YPT)
C
      PARAMETER(CENLAT=90.,CENLON=0.)
      PARAMETER(RERTH=6.3712E+6)
C
      HFPI = DASIN(1.0D0)
      PI = 2.0 * HFPI
      TWOPI = 2.0 * PI
      RAD = PI / 180.
C
C --------- SETUP REGIONAL LAT/LON AND MAP FACTOR -----
C
      TRUETH  = TRUTH * RAD
      DLAMDA0 = ORIENT * RAD + HFPI
      A2 =  RERTH * ( 1.0 + SIN(TRUETH) )
      RADLAT = CENLAT * RAD
      RADLON = CENLON * RAD - DLAMDA0
      YYY = A2 * COS( RADLAT )/(1. + SIN( RADLAT ) )
      IF( CENLAT .EQ. 90. ) YYY=0.0
      Y00 = YYY * SIN( RADLON ) - ( BOTMGRD -1.) * DELY
      X00 = YYY * COS( RADLON ) - ( RLFTGRD -1.) * DELX
C
C =========
C
      RLAT = FLAT * RAD
      RSOA2 = TAN( (HFPI-RLAT)*0.5 )
      X2PY2 = ( RSOA2 * A2 ) ** 2.0

      RLON = FLON * RAD - DLAMDA0
      RLON = AMOD(RLON,TWOPI)
      IF( RLON.LT.0. ) RLON=TWOPI+RLON
      YOX = TAN(RLON)

      X = SQRT( X2PY2/(1.+YOX*YOX) )
      Y = SQRT( X2PY2 - X*X )
      IF( RLON.GT.HFPI .AND. RLON.LT. PI+HFPI ) X = -X
      IF( RLON.GT.PI .AND. RLON.LT. TWOPI ) Y = -Y

      XPT = (X-X00)/DELX + 1.
      YPT = (Y-Y00)/DELY + 1.
C
      RETURN
      END
      SUBROUTINE BSSLZ1(BES,N)                                          
C                                                                       
      DIMENSION BES(N)                                                  
      DIMENSION BZ(50)                                                  
C                                                                       
      DATA PI/3.14159265358979E0/                                       
      DATA BZ         / 2.4048255577E0, 5.5200781103E0,                 
     $  8.6537279129E0,11.7915344391E0,14.9309177086E0,18.0710639679E0, 
     $ 21.2116366299E0,24.3524715308E0,27.4934791320E0,30.6346064684E0, 
     $ 33.7758202136E0,36.9170983537E0,40.0584257646E0,43.1997917132E0, 
     $ 46.3411883717E0,49.4826098974E0,52.6240518411E0,55.7655107550E0, 
     $ 58.9069839261E0,62.0484691902E0,65.1899648002E0,68.3314693299E0, 
     $ 71.4729816036E0,74.6145006437E0,77.7560256304E0,80.8975558711E0, 
     $ 84.0390907769E0,87.1806298436E0,90.3221726372E0,93.4637187819E0, 
     $ 96.6052679510E0,99.7468198587E0,102.888374254E0,106.029930916E0, 
     $ 109.171489649E0,112.313050280E0,115.454612653E0,118.596176630E0, 
     $ 121.737742088E0,124.879308913E0,128.020877005E0,131.162446275E0, 
     $ 134.304016638E0,137.445588020E0,140.587160352E0,143.728733573E0, 
     $ 146.870307625E0,150.011882457E0,153.153458019E0,156.295034268E0/ 
      NN=N                                                              
      IF(N.LE.50) GO TO 12                                              
      BES(50)=BZ(50)                                                    
      DO 5 J=51,N                                                       
    5 BES(J)=BES(J-1)+PI                                                
      NN=49                                                             
   12 DO 15 J=1,NN                                                      
   15 BES(J)=BZ(J)                                                      
      RETURN                                                            
      END                                                               
      SUBROUTINE SETDSET(LTMPL,LUNPK,LMRUNP,
     1                   NFILE,IFTYPE,NTYP,NDIR,
     2                   DIRNAME,FNGRBG,FNGRIB,
     3                   FNCNTL,FNHEAD,FNGUPO,FNDSET,DELIM,
     4                   LUPTR)
C
      CHARACTER*128 FNGRIB(*),FNGRBG(*),DIRNAME
C
      CHARACTER*128 FNDSET,FNHEAD,FNGUPO,FNCNTL
      LOGICAL LUNPK,LTMPL,LMRUNP
      DIMENSION IFTYPE(*)
C
      CHARACTER*1 DELIM
C
      CHARACTER*1 ALPHA(26)
      DATA ALPHA/'a','b','c','d','e','f','g','h','i','j','k','l','m',
     1           'n','o','p','q','r','s','t','u','v','w','x','y','z'/
C
      PARAMETER(MFTYP=100)
      CHARACTER*128 FNCNTLS
      COMMON/COMCNTL/NFCNTL,FNCNTLS(MFTYP)
C
      FNCNTL=' '
C
C  Get unique control file name for each type
C
      DO I=128,1,-1
        IF(FNGRBG(NTYP)(I:I).EQ.'/') THEN
          II=I
          GO TO 22
        ENDIF
      ENDDO
   22 CONTINUE
      FNCNTL(1:3)=FNGRBG(NTYP)(II+1:II+3)
      DO I=1,3
        IF(FNCNTL(I:I).EQ.'.'.OR.FNCNTL(I:I).EQ.' ') THEN
          FNCNTL(I:I)='x'
        ENDIF
      ENDDO
C
      NN=0
  333 CONTINUE
      NN=NN+1
      DO NX=1,NFCNTL
        IF(FNCNTL.EQ.FNCNTLS(NX)) THEN
          FNCNTL=FNCNTL(1:2)//ALPHA(NN)
          GO TO 333
        ENDIF   
      ENDDO
      NFCNTL=NFCNTL+1
      IF(NFCNTL.GT.MFTYP) THEN
        WRITE(LUPTR,*) 'ERROR.  Number of ctl file too large'
        PRINT *,'ERROR.  Number of ctl file exceeded limit'
        CALL ABORT
      ENDIF
      FNCNTLS(NFCNTL)=FNCNTL
C
      DO I=1,128
        FNDSET(I:I)=' '
      ENDDO
C
C  Modify directory name
C
      IF(.NOT.LTMPL.OR.LMRUNP) THEN
        IF(LUNPK) THEN
          JJ=0
          DO I=1,128
            IF(DIRNAME(I:I).NE.' ') THEN
              JJ=JJ+1
              FNDSET(JJ:JJ)=DIRNAME(I:I)
            ENDIF
          ENDDO
          DO I=1,128
            IF(FNHEAD(I:I).NE.' ') THEN
              JJ=JJ+1
              FNDSET(JJ:JJ)=FNHEAD(I:I)
            ENDIF
          ENDDO
C         IF(LMRUNP) THEN
C           FNDSET(JJ+1:JJ+3)=FNCNTL(1:3)
C           JJ=JJ+3
C         ELSE
            J=JJ
            DO I=1,128
              IF(FNGUPO(I:I).NE.' ') THEN
                J=J+1
                FNDSET(J:J)=FNGUPO(I:I)
              ENDIF
            ENDDO
C         ENDIF
        ELSE
          FNDSET=FNGRIB(NFILE)
        ENDIF
        WRITE(LUPTR,*) 'FNDSET0=',FNDSET
      ELSE
        IF(LUNPK) THEN
          IF(IFTYPE(NTYP).EQ.0) THEN
            JJ=0
            DO I=1,128
              IF(DIRNAME(I:I).NE.' ') THEN
                JJ=JJ+1
                FNDSET(JJ:JJ)=DIRNAME(I:I)
              ENDIF
            ENDDO
            DO I=1,128
              IF(FNHEAD(I:I).NE.' ') THEN
                JJ=JJ+1
                FNDSET(JJ:JJ)=FNHEAD(I:I)
              ENDIF
            ENDDO
C           IF(LMRUNP) THEN
C             FNDSET(JJ+1:JJ+3)=FNCNTL(1:3)
C             JJ=JJ+3
C             J=JJ
C           ELSE
              J=JJ
              DO I=1,128
                IF(FNGRBG(NTYP)(I:I).EQ.'/') THEN
                  J=JJ
                ELSE
                  IF(FNGRBG(NTYP)(I:I).NE.' ') THEN
                    J=J+1
                    FNDSET(J:J)=FNGRBG(NTYP)(I:I)
                  ENDIF
                ENDIF
              ENDDO
C           ENDIF
            WRITE(LUPTR,*) 'FNDSET1=',FNDSET
          ELSE
            JJ=0
            DO I=1,128
              IF(DIRNAME(I:I).NE.' ') THEN
                JJ=JJ+1
                FNDSET(JJ:JJ)=DIRNAME(I:I)
              ENDIF
            ENDDO
            J=JJ
            DO I=1,128
              IF(FNGRIB(1)(I:I).EQ.'/') THEN
                J=JJ
              ELSE
                IF(FNGRIB(1)(I:I).NE.' ') THEN
                  J=J+1
                  FNDSET(J:J)=FNGRIB(1)(I:I)
                ENDIF
              ENDIF
            ENDDO
          ENDIF
          FNDSET=FNDSET(1:J)//'.unp'
          WRITE(LUPTR,*) 'FNDSET2=',FNDSET
        ELSE
          FNDSET=FNGRBG(NTYP)
          WRITE(LUPTR,*) 'FNDSET3=',FNDSET
        ENDIF
      ENDIF
C
C  Fix /tmp/ 
C
      NN=0
      DO I=1,128
        IF(FNDSET(I:I).EQ.'/') THEN
          NN=NN+1
          IF(NN.GT.2) GO TO 155
          IF(NN.EQ.2.AND.FNDSET(1:I).EQ.'/tmp/') THEN
            MM=0
            DO J=1,128
              IF(DIRNAME(J:J).EQ.'/') THEN
                MM=MM+1
                IF(MM.EQ.2) THEN
                  FNDSET=DIRNAME(1:J)//FNDSET(I+1:128)
                  GO TO 155
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDIF
      ENDDO
  155 CONTINUE
C
C  Fix '%'
C
      DO I=1,128
        IF(FNDSET(I:I).EQ.DELIM) FNDSET(I:I)='%'
      ENDDO
      WRITE(LUPTR,*) 'FNDSET=',FNDSET(1:128)
      RETURN
C
      END
      SUBROUTINE GETBGSN(FNBGSI,IBGES,JBGES,IMAXL,JMAXL,IJMAXL,
     1                  NFILE,IFKEY,NTOTL,SELN,LY2000,LUBGSI,LUPTR)
C
      REAL*8 FHOUR
C
      CHARACTER*128 FNBGSI
      CHARACTER*80 ASGNSTR
C
      CHARACTER*1 IFKEY
C
      LOGICAL LY2000
C
      INTEGER SELN(*)
C
      DIMENSION IDATE(4)
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      COMMON/COMKEY/ KEYPDS(LKPDS,MTOTL),KEYGDS(LKGDS,MTOTL)
C
      CHARACTER*64 DATES,FTIME
C
      CHARACTER*5 MONTH(12)
C
      DATA MONTH/' JAN ',' FEB ',' MAR ',' APR ',' MAY ',' JUN ',
     1           ' JUL ',' AUG ',' SEP ',' OCT ',' NOV ',' DEC '/
C
      PARAMETER(NVRM=26)
      CHARACTER*46 VARSM(NVRM)
      DIMENSION LENVM(NVRM)
      DATA (VARSM(I),LENVM(I),I=1,NVRM)/
     &           'earth surface temperature (K)',29,
     &           'soil moisture level 1 (?)',24,
     &           'soil moisture level 2 (?)',24,
     &           'snow depth (m)',14,
     &           'soil temperature level 1 (K)',28,
     &           'soil temperature level 2 (K)',28,
     &           'soil temperature level 3 (K)',28,
     &           'surface roughness length (m)',28,
     &           'convective cloud cover (percent)',32,
     &           'convective cloud bottom height (sigma)',38,
     &           'convective cloud top height (sigma)',35,
     &           'albedo 1(fraction)',18,
     &           'albedo 2(fraction)',18,
     &           'albedo 3(fraction)',18,
     &           'albedo 4(fraction)',18,
     &           'snow/ice/land mask',18,
     &           'vegetation fraction',19,
     &           'canopy water content (?)',24,
     &           'ratio of 10m and k=1 winds (fraction)',37,
     &           'vegetation type',15,
     &           'soil type',9,
     &           'albedo fraction 1',17,
     &           'albedo fraction 2',17,
     &           'ustar',5,
     &           'fm',2,
     &           'fh',2/
C
      CHARACTER*8 ABBRM(NVRM)
      DATA ABBRM/ 'skint',
     &            'wet1',
     &            'wet2',
     &            'snow',
     &            'tg1',
     &            'tg2',
     &            'tg3',
     &            'zor',
     &            'cv',
     &            'cvb',
     &            'cvt',
     &            'alb1',
     &            'alb2',
     &            'alb3',
     &            'alb4',
     &            'mask',
     &            'vfrac',
     &            'cwc',
     &            'f10m',
     &            'veg',
     &            'stype',
     &            'faaf1',
     &            'faaf2',
     &            'ustar',
     &            'fm',
     &            'fh'/
C
      DO N=1,MTOTL
        LABBR(N)='        '
      ENDDO
C
      NFLX=MIN(NFILE,2)
      IMAXL=IBGES
      JMAXL=JBGES
      IJMAXL=IMAXL*JMAXL
C
      WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2)') LUBGSI
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUBGSI,FILE=FNBGSI,FORM='UNFORMATTED',ERR=988)
      GO TO 987
  988 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNBGSI
      PRINT *,'ERROR IN OPENING FILE ',FNBGSI
      CALL ABORT
  987 CONTINUE
      WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
      WRITE(LUPTR,*) 'FILE ',FNBGSI(1:128),' opened. Unit=',LUBGSI
C
      READ(LUBGSI)
      READ(LUBGSI) FHOUR,IDATE
      WRITE(LUPTR,*) 'FHOUR=',FHOUR,' IDATE=',IDATE
C
      IY=IDATE(4)
      IM=IDATE(2)
      ID=IDATE(3)
      IH=IDATE(1)
      IMIN=0
      KEYPDS(8,1)=IY
      KEYPDS(9,1)=IM
      KEYPDS(10,1)=ID
      KEYPDS(11,1)=IH
      KEYPDS(12,1)=IMIN
      KEYGDS(1,1)=4
C
      WRITE(LUPTR,*) 'IY,IM,ID,IH,IMIN=',IY,IM,ID,IH,IMIN
C
      CALL COMPJD(IY,IM,ID,IH,IMIN,JD,FJD)
C
      WRITE(LUPTR,*) 'JD,FJD=',JD,FJD
C
      IF(IM.LT.0.OR.IM.GT.12) THEN
        WRITE(LUPTR,*) 'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
        PRINT *,'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
        CALL ABORT
      ENDIF
      IF(ID.LT.0.OR.ID.GT.31) THEN
        WRITE(LUPTR,*) 'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
        PRINT *,'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
        CALL ABORT
      ENDIF
      IF(IH.LT.0.OR.IH.GT.24) THEN
        WRITE(LUPTR,*) 'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
        PRINT *,'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
        CALL ABORT
      ENDIF
      IF(IMIN.LT.0.OR.IMIN.GT.60) THEN
        WRITE(LUPTR,*) 'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
        PRINT *,'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
        CALL ABORT
      ENDIF
C
        LY2000=.TRUE.
      IF(IMIN.GT.0.AND.IMIN.LT.10) THEN
        IF(IH.GE.10) THEN
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ELSE
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ENDIF
      ELSEIF(IMIN.GE.10) THEN
        IF(IH.GE.10) THEN
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ELSE
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I2)')
     1              IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I4)')
     1              IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ENDIF
      ELSEIF(IH.LT.10) THEN
        IF(.NOT.LY2000) THEN
          WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I2)')
     1             IH,ID,MONTH(IM),IY
          LENDA=14
        ELSE
          WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I4)')
     1             IH,ID,MONTH(IM),IY
          LENDA=16
        ENDIF
      ELSE
        IF(.NOT.LY2000) THEN
          WRITE(DATES,'(I2,1HZ,I3,A5,1X,I2)')
     1             IH,ID,MONTH(IM),IY
          LENDA=14
        ELSE
          WRITE(DATES,'(I2,1HZ,I3,A5,1X,I4)')
     1             IH,ID,MONTH(IM),IY
          LENDA=16
        ENDIF
      ENDIF
C
      WRITE(FTIME,'(3HFT=,F5.1)') FHOUR
      LENFT=8
C
      N=0
      NVTOTL=0
C
      DO MVR=1,NVRM
        NVTOTL=NVTOTL+1
        N=N+1
        LABEL(1,N)=VARSM(MVR)
        LENLA(1,N)=LENVM(MVR)
        LABEL(2,N)='surface'
        LENLA(2,N)=7
        LABEL(3,N)=' '
        LENLA(3,N)=1
        LABBR(  N)=ABBRM(MVR)
        IRLEV(  N)=1
        LVABBR( N)='sfc'
      ENDDO
C
      NTOTL=N
      NVTOTL=NVTOTL
      NPTOTL=0
      IPINDX(1)=1
C
      DO N=1,NTOTL
        LABEL(4,N)(1:LENDA)=DATES(1:LENDA)
        LENLA(4,N)=LENDA
        LABEL(5,N)(1:LENDA)=FTIME(1:LENFT)
        LENLA(5,N)=LENFT
      ENDDO
C
      DO N=1,NTOTL
        RSLAT(N)=99.
        RNLAT(N)=99.
        WLON(N)=0.
        ELON(N)=360.
        IDIM(N)=IMAXL
        JDIM(N)=JMAXL
        DLON(N)=360./FLOAT(IMAXL)
        DLAT(N)=99.
        LGLOBL(N)=.TRUE.
        IJORDR(N)=.TRUE.
        FJHOUR(N,NFLX)=FHOUR
        RJLDAY(N,NFLX)=JD+FJD
      ENDDO
C
      CALL FLSRCH(NTOTL,LUPTR)
C
      DO N=1,NTOTL
        SELN(N)=N
      ENDDO
C
      RETURN
      END
      SUBROUTINE UNPBGSN(FNBGSI,FNBGSO,NFILE,LCRAY,IJMAX,
     1                  IDIML,JDIML,LREC,NREC,LUBGSI,LUBGSO,LUPTR)
C
      CHARACTER*128 FNBGSI,FNBGSO
      CHARACTER*80 ASGNSTR,SHLLSTR
      LOGICAL LCRAY
C
      DIMENSION IDATE(4)
C
      PARAMETER(LIJMAX=180*91)
      REAL*8 BGES(LIJMAX),CGES(LIJMAX),FHOUR
      REAL*8 DGES(LIJMAX),EGES(LIJMAX)
      REAL*4 RGES(LIJMAX),SGES(LIJMAX)
      REAL*4 TGES(LIJMAX),UGES(LIJMAX)
C
      PRINT *,' Reformatting BGES file ',FNBGSI(1:128)
C
      WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2)') LUBGSI
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUBGSI,FILE=FNBGSI,FORM='UNFORMATTED',ERR=988)
      GO TO 987
  988 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNBGSI
      PRINT *,'ERROR IN OPENING FILE ',FNBGSI
      CALL ABORT
  987 CONTINUE
      WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
      WRITE(LUPTR,*) 'FILE ',FNBGSI(1:128),' opened. Unit=',LUBGSI
C
      IF(NFILE.EQ.1) THEN
        IF(LCRAY) THEN
          WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUBGSO
          CALL ASSIGN(ASGNSTR)
        ENDIF
        OPEN(UNIT=LUBGSO,FILE=FNBGSO,FORM='UNFORMATTED',ERR=888)
        GO TO 887
  888   CONTINUE
        WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNBGSO
        PRINT *,'ERROR IN OPENING FILE ',FNBGSO
        CALL ABORT
  887   CONTINUE
        IF(LCRAY) THEN
          WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
        ENDIF
        WRITE(LUPTR,*) 'FILE ',FNBGSO(1:128),' opened. Unit=',LUBGSO
      ENDIF
C
      REWIND LUBGSI
      READ(LUBGSI)
      READ(LUBGSI) FHOUR,IDATE
      WRITE(LUPTR,*) 'FHOUR',FHOUR,' IDATE=',IDATE
C
      NREC=0
      DO I=1,1000
        IF(I.EQ.2.OR.I.EQ.4.OR.I.EQ.17) THEN
          READ (LUBGSI,END=100)(BGES(IJ),IJ=1,IJMAX),
     1                         (CGES(IJ),IJ=1,IJMAX)
          NREC=NREC+2
          DO IJ=1,IJMAX
            RGES(IJ)=BGES(IJ)
            SGES(IJ)=CGES(IJ)
          ENDDO
          WRITE(LUBGSO) (RGES(IJ),IJ=1,IJMAX)
          WRITE(LUBGSO) (SGES(IJ),IJ=1,IJMAX)
        ELSEIF(I.EQ.10) THEN
          READ (LUBGSI,END=100)(BGES(IJ),IJ=1,IJMAX),
     1                         (CGES(IJ),IJ=1,IJMAX),
     2                         (DGES(IJ),IJ=1,IJMAX),
     3                         (EGES(IJ),IJ=1,IJMAX)
          NREC=NREC+4
          DO IJ=1,IJMAX
            RGES(IJ)=BGES(IJ)
            SGES(IJ)=CGES(IJ)
            TGES(IJ)=DGES(IJ)
            UGES(IJ)=EGES(IJ)
          ENDDO
          WRITE(LUBGSO) (RGES(IJ),IJ=1,IJMAX)
          WRITE(LUBGSO) (SGES(IJ),IJ=1,IJMAX)
          WRITE(LUBGSO) (TGES(IJ),IJ=1,IJMAX)
          WRITE(LUBGSO) (UGES(IJ),IJ=1,IJMAX)
        ELSE
          READ (LUBGSI,END=100,ERR=100) (BGES(IJ),IJ=1,IJMAX)
          NREC=NREC+1
          DO IJ=1,IJMAX
            RGES(IJ)=BGES(IJ)
          ENDDO
          WRITE(LUBGSO) (RGES(IJ),IJ=1,IJMAX)
        ENDIF
      ENDDO
      LREC=IJMAX
  100 CONTINUE
      IF(NFILE.EQ.1) THEN
        WRITE(LUPTR,*) ' Number of Rec. written=',NREC
      ENDIF
C
      RETURN
      END
      SUBROUTINE UNPBGSM(FNBGSI,FNBGSO,NFILE,LCRAY,IJMAX,
     1                  IDIML,JDIML,LREC,NREC,LUBGSI,LUBGSO,LUPTR)
C
      CHARACTER*128 FNBGSI,FNBGSO
      CHARACTER*80 ASGNSTR,SHLLSTR
      LOGICAL LCRAY
C
      DIMENSION IDATE(4)
C
      PARAMETER(LIJMAX=180*91)
      REAL*8 BGES(LIJMAX),CGES(LIJMAX),FHOUR
      REAL*8 DGES(LIJMAX),EGES(LIJMAX)
      REAL*4 RGES(LIJMAX),SGES(LIJMAX)
      REAL*4 TGES(LIJMAX),UGES(LIJMAX)
C
      PRINT *,' Reformatting BGES file ',FNBGSI(1:128)
C
      WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2)') LUBGSI
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUBGSI,FILE=FNBGSI,FORM='UNFORMATTED',ERR=988)
      GO TO 987
  988 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNBGSI
      PRINT *,'ERROR IN OPENING FILE ',FNBGSI
      CALL ABORT
  987 CONTINUE
      WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
      WRITE(LUPTR,*) 'FILE ',FNBGSI(1:128),' opened. Unit=',LUBGSI
C
      IF(NFILE.EQ.1) THEN
        IF(LCRAY) THEN
          WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUBGSO
          CALL ASSIGN(ASGNSTR)
        ENDIF
        OPEN(UNIT=LUBGSO,FILE=FNBGSO,FORM='UNFORMATTED',ERR=888)
        GO TO 887
  888   CONTINUE
        WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNBGSO
        PRINT *,'ERROR IN OPENING FILE ',FNBGSO
        CALL ABORT
  887   CONTINUE
        IF(LCRAY) THEN
          WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
        ENDIF
        WRITE(LUPTR,*) 'FILE ',FNBGSO(1:128),' opened. Unit=',LUBGSO
      ENDIF
C
      REWIND LUBGSI
      READ(LUBGSI) IJDIM,LSOIL,IDATE(4),IDATE(1),IDATE(2),IDATE(3),
     1             FHOUR
      WRITE(LUPTR,*) 'FHOUR',FHOUR,' IDATE=',IDATE
C
      NREC=0
      DO I=1,1000
        IF(I.EQ.4.OR.I.EQ.15.OR.I.EQ.16.OR.I.EQ.21) THEN
          READ (LUBGSI,END=100)(BGES(IJ),IJ=1,IJMAX),
     1                         (CGES(IJ),IJ=1,IJMAX)
          NREC=NREC+2
          DO IJ=1,IJMAX
            RGES(IJ)=BGES(IJ)
            SGES(IJ)=CGES(IJ)
          ENDDO
          WRITE(LUBGSO) (RGES(IJ),IJ=1,IJMAX)
          WRITE(LUBGSO) (SGES(IJ),IJ=1,IJMAX)
        ELSEIF(I.EQ.8) THEN
          READ (LUBGSI,END=100)(BGES(IJ),IJ=1,IJMAX),
     1                         (CGES(IJ),IJ=1,IJMAX),
     2                         (DGES(IJ),IJ=1,IJMAX),
     3                         (EGES(IJ),IJ=1,IJMAX)
          NREC=NREC+4
          DO IJ=1,IJMAX
            RGES(IJ)=BGES(IJ)
            SGES(IJ)=CGES(IJ)
            TGES(IJ)=DGES(IJ)
            UGES(IJ)=EGES(IJ)
          ENDDO
          WRITE(LUBGSO) (RGES(IJ),IJ=1,IJMAX)
          WRITE(LUBGSO) (SGES(IJ),IJ=1,IJMAX)
          WRITE(LUBGSO) (TGES(IJ),IJ=1,IJMAX)
          WRITE(LUBGSO) (UGES(IJ),IJ=1,IJMAX)
        ELSE
          READ (LUBGSI,END=100,ERR=100) (BGES(IJ),IJ=1,IJMAX)
          NREC=NREC+1
          DO IJ=1,IJMAX
            RGES(IJ)=BGES(IJ)
          ENDDO
          WRITE(LUBGSO) (RGES(IJ),IJ=1,IJMAX)
        ENDIF
      ENDDO
      LREC=IJMAX
  100 CONTINUE
      IF(NFILE.EQ.1) THEN
        WRITE(LUPTR,*) ' Number of Rec. written=',NREC
      ENDIF
C
      RETURN
      END
      SUBROUTINE GETBGSM(FNBGSI,IBGES,JBGES,IMAXL,JMAXL,IJMAXL,
     1                  NFILE,IFKEY,NTOTL,SELN,LY2000,LUBGSI,LUPTR)
C
      REAL*8 FHOUR
C
      CHARACTER*128 FNBGSI
      CHARACTER*80 ASGNSTR
C
      CHARACTER*1 IFKEY
C
      LOGICAL LY2000
C
      INTEGER SELN(*)
C
      DIMENSION IDATE(4)
C
      PARAMETER(MTOTL=1200,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LCHOSEN,LLEVL,LITSR,LFTSR,LAREA,LLTYP,LGLOBL,IJORDR
      COMMON/COMCTL/ LABEL(5,MTOTL),LENLA(5,MTOTL),LABBR(MTOTL),
     1               LVABBR(MTOTL),RJLDAY(MTOTL,2),FJHOUR(MTOTL,2),
     2               RSLAT(MTOTL),RNLAT(MTOTL),WLON(MTOTL),ELON(MTOTL),
     3               DLAT(MTOTL),DLON(MTOTL),IDIM(MTOTL),JDIM(MTOTL),
     4               LGLOBL(MTOTL),IJORDR(MTOTL),
     5               IRLEV(MTOTL),LGRIB(MTOTL),LSKIP(MTOTL),
     6               ILINDX(MTOTL),ITINDX(MTOTL),IFINDX(MTOTL),
     7               IAINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     8               NLTOTL(MTOTL),NATOTL(MTOTL),PARMAP(10),NPARMP(10),
     9               NVTOTL,NITOTL,NFTOTL,NPTOTL,NFXTTL,
     A               LLEVL,LITSR,LFTSR,LAREA,LLTYP,LCHOSEN(MTOTL)
C
      COMMON/COMKEY/ KEYPDS(LKPDS,MTOTL),KEYGDS(LKGDS,MTOTL)
C
      CHARACTER*64 DATES,FTIME
C
      CHARACTER*5 MONTH(12)
C
      DATA MONTH/' JAN ',' FEB ',' MAR ',' APR ',' MAY ',' JUN ',
     1           ' JUL ',' AUG ',' SEP ',' OCT ',' NOV ',' DEC '/
C
      PARAMETER(NVRM=28)
      CHARACTER*46 VARSM(NVRM)
      DIMENSION LENVM(NVRM)
      DATA (VARSM(I),LENVM(I),I=1,NVRM)/
     &           'snow/ice/land mask clm',22,
     &           'orography',9,
     &           'plrclm',6,
     &           'smcclm1',7,
     &           'smcclm2',7,
     &           'earth surface temperature (K)',29,
     &           'snow depth (m)',14,
     &           'surface roughness length (m)',28,
     &           'albedo 1(fraction)',18,
     &           'albedo 2(fraction)',18,
     &           'albedo 3(fraction)',18,
     &           'albedo 4(fraction)',18,
     &           'soil temperature level 3 (K)',28,
     &           'plranl',6,
     &           'convective cloud cover (percent)',32,
     &           'convective cloud bottom height (sigma)',38,
     &           'convective cloud top height (sigma)',35,
     &           'canopy water content (?)',24,
     &           'soil moisture level 1 (?)',24,
     &           'soil moisture level 2 (?)',24,
     &           'soil temperature level 1 (K)',28,
     &           'soil temperature level 2 (K)',28,
     &           'snow/ice/land mask anl',22,
     &           'vegetation fraction',19,
     &           'vegetation type',15,
     &           'soil type',9,
     &           'albedo fraction 1',17,
     &           'albedo fraction 2',17/
C
      CHARACTER*8 ABBRM(NVRM)
      DATA ABBRM/ 'mskc',
     &            'orog',
     &            'plrc',
     &            'smc1c',
     &            'smc2c',
     &            'tsfc',
     &            'snow',
     &            'zor',
     &            'alb1',
     &            'alb2',
     &            'alb3',
     &            'alb4',
     &            'tg3',
     &            'plranl',
     &            'cv',
     &            'cvb',
     &            'cvt',
     &            'cwc',
     &            'smc1',
     &            'smc2',
     &            'stc1',
     &            'stc2',
     &            'mska',
     &            'vegf',
     &            'vet',
     &            'styp',
     &            'alf1',
     &            'alf2'/
C
      DO N=1,MTOTL
        LABBR(N)='        '
      ENDDO
C
      NFLX=MIN(NFILE,2)
      IMAXL=IBGES
      JMAXL=JBGES
      IJMAXL=IMAXL*JMAXL
C
      WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2)') LUBGSI
      CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUBGSI,FILE=FNBGSI,FORM='UNFORMATTED',ERR=988)
      GO TO 987
  988 CONTINUE
      WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNBGSI
      PRINT *,'ERROR IN OPENING FILE ',FNBGSI
      CALL ABORT
  987 CONTINUE
      WRITE(LUPTR,*) 'ASGNSTR=',ASGNSTR
      WRITE(LUPTR,*) 'FILE ',FNBGSI(1:128),' opened. Unit=',LUBGSI
C
      READ(LUBGSI)
      READ(LUBGSI) FHOUR,IDATE
      WRITE(LUPTR,*) 'FHOUR=',FHOUR,' IDATE=',IDATE
C
      IY=IDATE(4)
      IM=IDATE(2)
      ID=IDATE(3)
      IH=IDATE(1)
      IMIN=0
      KEYPDS(8,1)=IY
      KEYPDS(9,1)=IM
      KEYPDS(10,1)=ID
      KEYPDS(11,1)=IH
      KEYPDS(12,1)=IMIN
      KEYGDS(1,1)=4
C
      WRITE(LUPTR,*) 'IY,IM,ID,IH,IMIN=',IY,IM,ID,IH,IMIN
C
      CALL COMPJD(IY,IM,ID,IH,IMIN,JD,FJD)
C
      WRITE(LUPTR,*) 'JD,FJD=',JD,FJD
C
      IF(IM.LT.0.OR.IM.GT.12) THEN
        WRITE(LUPTR,*) 'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
        PRINT *,'ERROR!!!  MONTH LT 0 or GT 12.  MONTH=',IM
        CALL ABORT
      ENDIF
      IF(ID.LT.0.OR.ID.GT.31) THEN
        WRITE(LUPTR,*) 'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
        PRINT *,'ERROR!!!  DAY LT 0 or GT 31.  DAY=',ID
        CALL ABORT
      ENDIF
      IF(IH.LT.0.OR.IH.GT.24) THEN
        WRITE(LUPTR,*) 'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
        PRINT *,'ERROR!!!  HOUR LT 0 or GT 24.  HOUR=',IH
        CALL ABORT
      ENDIF
      IF(IMIN.LT.0.OR.IMIN.GT.60) THEN
        WRITE(LUPTR,*) 'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
        PRINT *,'ERROR!!!  MINUTE LT 0 or GT 60.  MINUTE=',IMIN
        CALL ABORT
      ENDIF
C
        LY2000=.TRUE.
      IF(IMIN.GT.0.AND.IMIN.LT.10) THEN
        IF(IH.GE.10) THEN
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(I2,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ELSE
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(1H0,I1,1H:,1H0,I1,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ENDIF
      ELSEIF(IMIN.GE.10) THEN
        IF(IH.GE.10) THEN
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I2)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(I2,1H:,I2,1HZ,I3,A5,1X,I4)')
     1               IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ELSE
          IF(.NOT.LY2000) THEN
            WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I2)')
     1              IH,IMIN,ID,MONTH(IM),IY
            LENDA=17
          ELSE
            WRITE(DATES,'(1H0,I1,1H:,I2,1HZ,I3,A5,1X,I4)')
     1              IH,IMIN,ID,MONTH(IM),IY
            LENDA=19
          ENDIF
        ENDIF
      ELSEIF(IH.LT.10) THEN
        IF(.NOT.LY2000) THEN
          WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I2)')
     1             IH,ID,MONTH(IM),IY
          LENDA=14
        ELSE
          WRITE(DATES,'(1H0,I1,1HZ,I3,A5,1X,I4)')
     1             IH,ID,MONTH(IM),IY
          LENDA=16
        ENDIF
      ELSE
        IF(.NOT.LY2000) THEN
          WRITE(DATES,'(I2,1HZ,I3,A5,1X,I2)')
     1             IH,ID,MONTH(IM),IY
          LENDA=14
        ELSE
          WRITE(DATES,'(I2,1HZ,I3,A5,1X,I4)')
     1             IH,ID,MONTH(IM),IY
          LENDA=16
        ENDIF
      ENDIF
C
      WRITE(FTIME,'(3HFT=,F5.1)') FHOUR
      LENFT=8
C
      N=0
      NVTOTL=0
C
      DO MVR=1,NVRM
        NVTOTL=NVTOTL+1
        N=N+1
        LABEL(1,N)=VARSM(MVR)
        LENLA(1,N)=LENVM(MVR)
        LABEL(2,N)='surface'
        LENLA(2,N)=7
        LABEL(3,N)=' '
        LENLA(3,N)=1
        LABBR(  N)=ABBRM(MVR)
        IRLEV(  N)=1
        LVABBR( N)='sfc'
      ENDDO
C
      NTOTL=N
      NVTOTL=NVTOTL
      NPTOTL=0
      IPINDX(1)=1
C
      DO N=1,NTOTL
        LABEL(4,N)(1:LENDA)=DATES(1:LENDA)
        LENLA(4,N)=LENDA
        LABEL(5,N)(1:LENDA)=FTIME(1:LENFT)
        LENLA(5,N)=LENFT
      ENDDO
C
      DO N=1,NTOTL
        RSLAT(N)=99.
        RNLAT(N)=99.
        WLON(N)=0.
        ELON(N)=360.
        IDIM(N)=IMAXL
        JDIM(N)=JMAXL
        DLON(N)=360./FLOAT(IMAXL)
        DLAT(N)=99.
        LGLOBL(N)=.TRUE.
        IJORDR(N)=.TRUE.
        FJHOUR(N,NFLX)=FHOUR
        RJLDAY(N,NFLX)=JD+FJD
      ENDDO
C
      CALL FLSRCH(NTOTL,LUPTR)
C
      DO N=1,NTOTL
        SELN(N)=N
      ENDDO
C
      RETURN
      END
