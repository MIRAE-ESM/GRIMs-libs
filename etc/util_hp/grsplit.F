      PROGRAM MAIN
C
C  GRIB file splitter
C 
C  MTOTL ... Max number of fields available for given index file(s) 
C  LKPDS ... Number of PDS words to be recorded 
C 
      PARAMETER(MTOTL=800,LKPDS=17) 
      CHARACTER*64 LABEL 
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LLEVL,LLTYP
      COMMON/COMCTL/LABEL(3,MTOTL),LENLA(3,MTOTL),LABBR(MTOTL),
     1              LVABBR(MTOTL),RLEV(MTOTL),
     2              ILINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     3              NLTOTL(MTOTL),NVTOTL,NPTOTL,LLEVL,LLTYP
C
      INTEGER KPDS(25,MTOTL),KGDS(22,MTOTL)
      INTEGER LSKIP(MTOTL),LGRIB(MTOTL)
C
      DIMENSION KEYPDS(LKPDS,MTOTL)
C
      LOGICAL LSEL(MTOTL)
C
C  NAMELIST PARAMETERS:
C
C  FNGRIB ... GRIB file name(s). 
C             Dimension for history sequence (in forecast time or initial)
C  FNAMPRE ... Namelist data file name for premap subroutine
C  FNPRINT ... File name where printouts from premap program goes
C  LDEBUG  ... TRUE for debugging printout. Output goes to FNPRINT
C
C  NAMELIST VARIABLE TYPES
C 
      PARAMETER(MFILE=300)
      CHARACTER*64 FNGRIB(MFILE)
      CHARACTER*64 FNGRBO(MTOTL)
      DIMENSION NCHGRBO(MTOTL)
      CHARACTER*64 FNGRBT
      CHARACTER*64 FNAMPRE
      CHARACTER*64 FNPRINT
      CHARACTER*64 FNOLIST
      CHARACTER*64 FNILIST
C
      CHARACTER*64 FNTEMP,FNINBI(MTOTL)
      DIMENSION NCHINBI(MTOTL)
C
      DATA MSK1/32000/,MSK2/4000/
C
      CHARACTER*80 FNKPDS5,FNKPDS6
C
C  NAMELIST DEFAULTS
C
      DATA FNGRIB /MFILE*'        '/
      DATA FNAMPRE/'./namlpre.data'/
      DATA FNPRINT/'./grsplit.out'/
      DATA FNOLIST/'./grsplit.list'/
      DATA FNILIST/'          '/
      DATA FNKPDS5/'/home/sgi90/wd23ln/etc/grib1.kpds5.vsn21'/
      DATA FNKPDS6/'/home/sgi90/wd23ln/etc/grib1.kpds6.vsn21'/
C
      LOGICAL LDEBUG
      DATA LDEBUG/.FALSE./
C
      LOGICAL LISTONLY
      DATA LISTONLY/.FALSE./
C
      NAMELIST/NAMPRE/ FNAMPRE,FNPRINT,FNOLIST,FNILIST,
     1                 LISTONLY,FNKPDS5,FNKPDS6,LDEBUG
C
C  Fort Unit numbers used inside the program
C
C  LUPTR  ... Unit unmber for diagnostic/debug print output other than uNIPLO
C  LUPRE  ... Unit unmber for grib read namelist input
C  LUPGI  ... Unit unmber for grib index file
C  LUPGB  ... Unit unmber for input (grib) file
C  LUPGO  ... Unit unmber for grib output file
C  LULST  ... Unit unmber for output file name list
C  LULSI  ... Unit unmber for input list file name
C             input data
C
      DATA LUPRE/ 1/
      DATA LUPGI/11/
      DATA LUPGB/21/
      DATA LUPGO/51/
      DATA LUPTR/50/
      DATA LULST/52/
      DATA LULSI/53/
C
      PARAMETER(MBUF=1024*128*64)
      CHARACTER*1 CBUF(MBUF),DBUF(MBUF)
C
      CHARACTER*80 ASGNSTR
      CHARACTER*8 CLEV
      CHARACTER*2 CYEAR,CMONTH,CDAY,CHOUR
      CHARACTER*3 CFHOUR1,CFHOUR2
C
      INTEGER JPDS(25),JGDS(22),JENS(5)
      INTEGER MPDS(25),MGDS(22),MENS(5)
C
      LOGICAL LDBG
      COMMON/COMDBG/ LDBG
C
C  Open main program (debug) print file
C
      OPEN(LUPTR,FILE=FNPRINT,ERR=711)
      GO TO 712
  711 CONTINUE
      PRINT *,' ERROR IN OPENING FILE ',FNPRINT
      CALL ABORT
  712 CONTINUE
      WRITE(LUPTR,*) ' FILE ',FNPRINT(1:50),' opened.  Unit=',LUPTR
C
C  Open main program namelist file
C
      OPEN(LUPRE,FILE=FNAMPRE,STATUS='OLD',ERR=721)
      GO TO 722
  721 CONTINUE
      PRINT *,'ERROR IN OPENING FILE ',FNAMPRE
      CALL ABORT
  722 CONTINUE
      WRITE(LUPTR,*) ' FILE ',FNAMPRE(1:50),' opened.  Unit=',LUPRE
C
C  Open output file list file
C
      OPEN(LULST,FILE=FNOLIST,ERR=511)
      GO TO 512
  511 CONTINUE
      PRINT *,' ERROR IN OPENING FILE ',FNOLIST
      CALL ABORT
  512 CONTINUE
      PRINT *,' FILE ',FNOLIST(1:50),' opened.  Unit=',LULST
C
C  Read main program namelist
C
      READ (LUPRE,NAMPRE) 
      WRITE(LUPTR,NAMPRE) 
      WRITE(LUPTR,*) ' ' 
C
      LDBG=LDEBUG
C
      READ (LUPRE,*,END=908) NFILES
      GO TO 907
  908 CONTINUE
      WRITE(LUPTR,*) ' EOF on data'
      CALL ABORT
  907 CONTINUE
      WRITE(LUPTR,*) ' NFILES=',NFILES
      IF(NFILES.EQ.0) THEN
        CALL ABORT
      ENDIF 
      DO N=1,NFILES
        READ (LUPRE,'(A64)') FNGRIB(N)
        WRITE(LUPTR,'(8H FNGRIB=,A50)') FNGRIB(N)
      ENDDO
C
      DO I=1,LKPDS*MTOTL
        KEYPDS(I,1)=-1
      ENDDO
C
      IF(NFILES.GT.MFILE) THEN
        WRITE(LUPTR,*) ' ERROR!!!  Number of files exceeded limit.'
        PRINT *, ' ERROR!!!  Number of files exceeded limit.'
        CALL ABORT
      ENDIF
C
      DO N=1,MTOTL
        LABBR(N)='        '
      ENDDO
C
      CALL INIPARM(FNKPDS5,LUPTR)
      CALL INILEVL(FNKPDS6,LUPTR)
C
C  Get grib index buffer
C
C-CRA WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUPGB
C-CRA CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUPGB,FILE=FNGRIB(1),FORM='UNFORMATTED',ERR=978)
      GO TO 977
  978 CONTINUE
      WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNGRIB(1)
      PRINT *,'ERROR IN OPENING FILE ',FNGRIB(1)
      CALL ABORT
  977 CONTINUE
      WRITE(LUPTR,*) ' FILE ',FNGRIB(1)(1:50),' opened. Unit=',LUPGB
C
      MNUM=0
C
C  BUILD VARIABLE NAME TABLE FROM THE FIRST GRIB FILE
C
      CALL GETGIR(LUPGB,MSK1,MSK2,MNUM,MBUF,CBUF,NLEN,NNUM,IRET)
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
      CALL UNPINDX(CBUF,NLEN,NNUM,KPDS,KGDS,LSKIP,LGRIB,IRET)
C
      NTOTL=NNUM
      WRITE(LUPTR,*) ' NTOTL=',NTOTL
C
      WRITE(LUPTR,*) ' KPDS=',(KPDS(I,1),I=1,25)
      WRITE(LUPTR,*) ' KGDS=',(KGDS(I,1),I=1,22)
C
      DO N=1,NTOTL
        CALL GETPARM(KPDS(1,N),1,LABEL(1,N),LENLA(1,N),LABBR(N),
     1               LUPTR)
        CALL GETLEVL(KPDS(1,N),1,LABEL(1,N),LENLA(1,N),RLEV(N),
     1               LVABBR(N),LUPTR)
        DO I=1,LKPDS
          KEYPDS(I,N)=KPDS(I,N)
        ENDDO
      ENDDO
C
C  Get rid of unnecessary spaces from LABEL
C
      DO N=1,NTOTL
        CALL RMBLNK(LABEL(1,N),LENLA(1,N),3)
      ENDDO
C
      CALL FLSRCH(NTOTL,LUPTR)
C
      DO N=1,NTOTL
        WRITE(LUPTR,*) N,')',LABEL(1,N)(1:LENLA(1,N)),
     1                            ' (',LABBR(N),' ) ',
     2                ' at ',LABEL(2,N)(1:LENLA(2,N)),' ',
     3                       LABEL(3,N)(1:LENLA(3,N))
      ENDDO
C
C  CREATE FILE NAME BASED ON THE VARIABLE NAME, LEVEL AND DATES
C
      DO N=1,NTOTL
        DO I=1,64
          FNGRBT(I:I)=' '
          FNGRBO(N)(I:I)=' '
        ENDDO
        FNGRBT=LABBR(N)
        IF(RLEV(N).GT.0.) THEN
          IF(FLOAT(INT(RLEV(N))).EQ.RLEV(N)) THEN
            MRLEV=INT(RLEV(N))
          ELSE
            MM=ALOG10(RLEV(N))
            MRLEV=RLEV(N)*10**(-MM)*1000
          ENDIF
          WRITE(CLEV,'(I5)') MRLEV
          LENT=5
          FNGRBT=FNGRBT(1:8)//'.'//CLEV(1:LENT)
        ENDIF
C
        IY=KPDS(8,N)
        IM=KPDS(9,N)
        ID=KPDS(10,N)
        IH=KPDS(11,N)
        IF(IY.GE.10) THEN
          WRITE(CYEAR,'(I2)') IY
        ELSE
          WRITE(CYEAR,'(1H0,I1)') IY
        ENDIF
        IF(IM.GE.10) THEN
          WRITE(CMONTH,'(I2)') IM
        ELSE
          WRITE(CMONTH,'(1H0,I1)') IM
        ENDIF
        IF(ID.GE.10) THEN
          WRITE(CDAY,'(I2)') ID
        ELSE
          WRITE(CDAY,'(1H0,I1)') ID
        ENDIF
        IF(IH.GE.10) THEN
          WRITE(CHOUR,'(I2)') IH
        ELSE
          WRITE(CHOUR,'(1H0,I1)') IH
        ENDIF
        IFH1=KPDS(14,N)
        IFH2=KPDS(15,N)
        IF(IFH1.GE.100) THEN
          WRITE(CFHOUR1,'(I3)') IFH1
        ELSEIF(IFH1.GE.10) THEN
          WRITE(CFHOUR1,'(1H0,I2)') IFH1
        ELSE
          WRITE(CFHOUR1,'(2H00,I1)') IFH1
        ENDIF
        IF(IFH2.GE.100) THEN
          WRITE(CFHOUR2,'(I3)') IFH2
        ELSEIF(IFH2.GE.10) THEN
          WRITE(CFHOUR2,'(1H0,I2)') IFH2
        ELSE
          WRITE(CFHOUR2,'(2H00,I1)') IFH2
        ENDIF
C
        IF(KPDS(16,N).LE.1.OR.KPDS(16,N).EQ.10) THEN
          FNGRBT=FNGRBT(1:8+LENT+1)//'.'//CYEAR//CMONTH//
     1           CDAY//CHOUR//'.ft'//CFHOUR1
        ELSE
          FNGRBT=FNGRBT(1:8+LENT+1)//'.'//CYEAR//CMONTH//
     1           CDAY//CHOUR//'.ft'//CFHOUR1//'-'//CFHOUR2
        ENDIF
        J=0
        DO I=1,64
          IF(FNGRBT(I:I).NE.' ') THEN
            J=J+1
            FNGRBO(N)(J:J)=FNGRBT(I:I)
          ENDIF
        ENDDO
        WRITE(LULST,*) FNGRBO(N)(1:J)
        NCHGRBO(N)=J
      ENDDO
C
      IF(LISTONLY) STOP
C
C  SELECTION OF VARIABLES BY INPUT LIST FILE
C
      IF(FNILIST(1:8).NE.'        ') THEN
        OPEN(LULSI,FILE=FNILIST,ERR=411)
        GO TO 412
  411   CONTINUE
        PRINT *,' ERROR IN OPENING FILE ',FNILIST
        CALL ABORT
  412   CONTINUE
        PRINT *,' FILE ',FNILIST(1:50),' opened.  Unit=',LULSI
        DO N=1,1000
          READ(LULSI,'(A64)',END=401) FNTEMP
          PRINT *,FNTEMP(1:10)
          I=0
					DO J=1,64
						IF(FNTEMP(J:J).NE.' ') THEN
              I=I+1
              FNINBI(N)(I:I)=FNTEMP(J:J)
            ENDIF
					ENDDO
          NCHINBI(N)=I
          IF(I.LE.0) THEN
            PRINT *,'INPUT LIST FILE ERROR'
            CALL ABORT
          ENDIF
        ENDDO
  401   CONTINUE
        NTOTLI=N-1
        IF(NTOTLI.LE.0) THEN
          PRINT *,'INPUT LIST FILE ERROR'
          CALL ABORT
        ENDIF
C
C  CROSS CHECK INPUT LIST FILE CONTENTS
C
        DO N=1,NTOTLI
          DO NN=1,NTOTL
            IF(FNINBI(N)(1:NCHINBI(N)).EQ.
     1         FNGRBO(NN)(1:NCHGRBO(NN))) THEN
              LSEL(NN)=.TRUE.
              GO TO 200
            ENDIF
          ENDDO
          PRINT *,'VARIABLE NAME ',FNINBI(N)(1:NCHINBI(N)),' NOT FOUND'
          PRINT *,'N=',N
          CALL ABORT
  200     CONTINUE
        ENDDO
      ELSE
        DO N=1,NTOTL
          LSEL(N)=.TRUE.
        ENDDO
      ENDIF
C
C  Loop through GRIB file
C
      DO NFILE=1,NFILES
C
        PRINT *,' Processing ', FNGRIB(NFILE)(1:50)
C
C  Get grib index buffer for this particular grib file
C
C-CRA   WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUPGB
C-CRA   CALL ASSIGN(ASGNSTR)
C-CRA   WRITE(ASGNSTR,'(23Hassign -s unblocked  u:,I2)') LUPGO
C-CRA   CALL ASSIGN(ASGNSTR)
        OPEN(UNIT=LUPGB,FILE=FNGRIB(NFILE),FORM='UNFORMATTED',ERR=878)
        GO TO 877
  878   CONTINUE
        WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNGRIB(NFILE)
        PRINT *,'ERROR IN OPENING FILE ',FNGRIB(NFILE)
        CALL ABORT
  877   CONTINUE
        WRITE(LUPTR,*) FNGRIB(NFILE)(1:50),' opened. Unit=',LUPGB
        MNUM=0
C
        CALL GETGIR(LUPGB,MSK1,MSK2,MNUM,MBUF,CBUF,NLEN,NNUM,IRET)
C
        WRITE(LUPTR,*)'NLEN=',NLEN,' NNUM=',NNUM
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
     1            ' buffer exceeded limit of ',MTOTL
          PRINT *,'ERROR!!! Number of parameters in the index',
     1            ' buffer exceeded limit of ',MTOTL
          CALL ABORT
        ENDIF
C
        CALL UNPINDX(CBUF,NLEN,NNUM,KPDS,KGDS,LSKIP,LGRIB,IRET)
C
        DO N=1,NTOTL
          IF(.NOT.LSEL(N)) GO TO 5000
          CLOSE (LUPGO)
          IF(NFILE.GT.1) THEN
            OPEN(UNIT=LUPGO,FILE=FNGRBO(N),FORM='UNFORMATTED',
     1           ERR=968,ACCESS='APPEND')
C-CRA1           ERR=968,POSITION='APPEND')
          ELSE
            OPEN(UNIT=LUPGO,FILE=FNGRBO(N),FORM='UNFORMATTED',
     1           ERR=968)
          ENDIF
          GO TO 967
  968     CONTINUE
          WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNGRBO(N)
          WRITE(LUPTR,*) ' N=',N
          PRINT *,'ERROR IN OPENING FILE ',FNGRBO(N)
          PRINT *,'N=',N
          CALL ABORT
  967     CONTINUE
          WRITE(LUPTR,*) ' FILE ',FNGRBO(N)(1:50),' opened. Unit=',LUPGO
C
          DO I=1,25
            JPDS(I)=-1
          ENDDO
          DO I=1,LKPDS
            JPDS(I)=KEYPDS(I,N)
          ENDDO
          DO I=1,22
            JGDS(I)=-1
          ENDDO
          DO I=1,5
            JENS(I)=-1
          ENDDO
C
          NN=0
          CALL GETGBSS(CBUF,NLEN,NNUM,NN,JPDS,JGDS,JENS,
     &                 K,MPDS,MGDS,MENS,MSKIP,MGRIB,IRET)
          IF(MGRIB.EQ.0) THEN
            WRITE(6,*) ' Error in GETGBSS.  Field not found.'
            CALL ABORT
          ENDIF
C
          CALL BAREAD(LUPGB,MSKIP,MGRIB,LREAD,DBUF)
          IF(LREAD.LT.MGRIB) THEN
            WRITE(LUPTR,*) ' ERROR IN BAREAD.  LREAD.LT.MGRIB'
            WRITE(LUPTR,*) ' MGRIB=',MGRIB,' LREAD=',LREAD
            WRITE(LUPTR,*) ' NFILE=',NFILE
            WRITE(LUPTR,*) ' FNGRIB=',FNGRIB(NFILE)
            PRINT *,' ERROR IN BAREAD.   LREAD.LT.MGRIB'
            CALL ABORT
          ENDIF
          CALL WRYTE (LUPGO,LREAD,DBUF)
					CALL FLUSH(LUPTR)
 5000     CONTINUE
        ENDDO
      ENDDO
C
      STOP
      END
      SUBROUTINE INIPARM(FNKPDS5,LUPTR)
C
C     Initialize KPDS(5) table
C
      PARAMETER(LUKP5=90)
C
      CHARACTER*80 FNKPDS5
C
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
      WRITE(LUPTR,*) 'FILE ',FNKPDS5(1:50),' opened.  Unit=',LUKP5
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
      CHARACTER*80 FNKPDS6
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
      WRITE(LUPTR,*) 'FILE ',FNKPDS6(1:50),' opened.  Unit=',LUKP6
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
      SUBROUTINE GETPARM(KPDS,NGI,LABEL,LENLA,LABBR,LUPTR)
C
      DIMENSION KPDS(22,*)
C
C     Transrates KPDS(5,*) parameter to variable names
C
      CHARACTER*64 LABEL(3,*)
      DIMENSION LENLA(3,*)
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
      DO N=1,NGI
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
      SUBROUTINE GETLEVL(KPDS,NGI,LABEL,LENLA,RLEV,LVABBR,LUPTR)
C
C     Transrates KPDS(6,*) parameter to level names
C
      CHARACTER*64 LABEL(3,*)
      DIMENSION LENLA(3,*)
      DIMENSION RLEV(*)
      CHARACTER*3 LVABBR(*)
C
      DIMENSION KPDS(22,*)
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
      DO 100 N=1,NGI
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
           WRITE(LUPTR,*) ' Serious problem in index FILE! ',
     1        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           WRITE(LUPTR,*) ' THE LEVEL IS NOT DEFINED ',
     1        'IN THE FILE KPDS(6) TABLE FILE.'
           PRINT *,' Serious problem in index FILE! ',
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
           WRITE(LUPTR,*) ' ERROR in utilizing index FILE ! ',
     1        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
           WRITE(LUPTR,*) ' Mixed precision level not programed',
     1        'to work in this program.'
           PRINT *,' ERROR in utilizing index FILE ! ',
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
        RLEV(N)=BTML
C
        IF(FLOAT(INT(TOPL)).EQ.TOPL) THEN
          WRITE(LTOPL,'(I5)') INT(TOPL)
          LENT=5
        ELSE
          WRITE(LTOPL,'(F8.2)') TOPL
          LENT=8
        ENDIF
        IF(FLOAT(INT(BTML)).EQ.BTML) THEN
          WRITE(LBTML,'(I5)') INT(BTML)
          LENB=5
        ELSE
          WRITE(LBTML,'(F8.2)') BTML
          LENB=8
        ENDIF
C
        LABEL(2,N)=LINFO(K)(1:LENLI(K))
        LENLA(2,N)=LENLI(K)
C
  		  IF(LEVLR(K)(1:5).EQ.'level'.OR.LEVLR(K)(1:6).EQ.'slevel') THEN
  		    IF(LTYPE(K)(1:7).EQ.'surface'.OR.LTYPE(K)(1:1).EQ.'-') THEN
            LABEL(3,N)=' '
            LENLA(3,N)=1
	  	    ELSE
            LABEL(3,N)=LTOPL(1:LENT)//LUNIT(K)(1:LENLU(K))
            LENLA(3,N)=LENT+LENLU(K)
		  		ENDIF
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
      PARAMETER(MTOTL=800,LKPDS=16,LKGDS=11)
      CHARACTER*64 LABEL
      CHARACTER*8  LABBR
      CHARACTER*3  LVABBR
      LOGICAL LLEVL,LLTYP
      COMMON/COMCTL/LABEL(3,MTOTL),LENLA(3,MTOTL),LABBR(MTOTL),
     1              LVABBR(MTOTL),RLEV(MTOTL),
     2              ILINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),
     4              NLTOTL(MTOTL),NVTOTL,NPTOTL,LLEVL,LLTYP
      CHARACTER*64 LABTMP
C
      LOGICAL LFP(MTOTL),LPFND
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
        IF(.NOT.LFP(N)) THEN
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M)) THEN
              IF(LABBR(N).EQ.LABBR(M).AND.
     1           LABEL(2,N)(1:LENLA(2,N)).NE.
     2           LABEL(2,M)(1:LENLA(2,M))) THEN
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
        ENDIF
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
        IF(.NOT.LFP(N)) THEN
          NVTOTL=NVTOTL+1
          IVINDX(NVTOTL)=N
          LFP(N)=.TRUE.
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M).AND.
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
          IF(.NOT.LFP(N).AND.
     1                       LABEL(1,N)(1:LENLA(1,N)).EQ.
     2                       LABEL(1,NVL)(1:LENLA(1,NVL)).AND.
     3                       LABBR(N).EQ.LABBR(NVL)) THEN
            NNN=NNN+1
            NLTOTL(NV)=NLTOTL(NV)+1
            ILINDX(NNN)=N
            LFP(N)=.TRUE.
            DO M=N+1,NTOTL
              IF(.NOT.LFP(M).AND.
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
      IF(LDEBUG) THEN
        WRITE(LUPTR,*) ' '
        WRITE(LUPTR,*) ' NLTOTL(NV)=',(NLTOTL(NV),NV=1,NVTOTL)
        NNN=0
        DO NV=1,NVTOTL
          WRITE(LUPTR,*) (LABEL(3,ILINDX(K+NNN))
     1                   (1:LENLA(3,ILINDX(K+NNN))),',',K=1,NLTOTL(NV))
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
        IF(.NOT.LFP(N).AND.LENLA(3,N).GT.1) THEN
          NPTOTL=NPTOTL+1
          LFP(N)=.TRUE.
          IPINDX(NPTOTL)=N
          DO M=N+1,NTOTL
            IF(.NOT.LFP(M).AND.
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
      WRITE(LUPTR,*) ' '
      IF(LLEVL) THEN
        WRITE(LUPTR,*) ' Multi-level fields found in a given file'
        WRITE(LUPTR,*) ' Number of multi-level fields=',NLTOTL
      ENDIF
      IF(LLTYP) THEN
        WRITE(LUPTR,*) ' Same variable with different level type found',
     1                 ' in a given file'
        WRITE(LUPTR,*) ' Number of such fields=',NPTOTL
      ENDIF
C
      RETURN
      END
      SUBROUTINE WRYTE(LU,LC,C)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    WRYTE       WRITE DATA OUT BY BYTES
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: EFFICIENTLY WRITE UNFORMATTED A CHARACETER ARRAY.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL WRYTE(LU,LC,C)
C
C   INPUT ARGUMENT LIST:
C     LU       - INTEGER UNIT TO WHICH TO WRITE
C     LC       - INTEGER NUMBER OF CHARACTERS OR BYTES TO WRITE
C     C        - CHARACETER (LC) DATA TO WRITE
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      CHARACTER C(LC)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      WRITE(LU) C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
