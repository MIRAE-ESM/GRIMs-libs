!
      PROGRAM MAIN
!
!-------------------------------------------------------------------------------
!
!  Variance GRIB file generator 
!
!   computes uu,vv,uv,tt,ut,vt,uzeta,vzeta,wt
! 
!  MTOTL ... Max number of fields in the index file(s) 
!  LKPDS ... Number of PDS words to be recorded 
! 
!-------------------------------------------------------------------------------
!
   CHARACTER(LEN = 64)   ::  LABEL
   CHARACTER(LEN = 8 )   ::  LABBR
   CHARACTER(LEN = 8 )   ::  LVABBR
!mskoo   INTEGER, PARAMETER    ::  MTOTL = 600
!mskoo   INTEGER, PARAMETER    ::  MDATA = 1200000
   INTEGER, PARAMETER    ::  MTOTL = 160
   INTEGER, PARAMETER    ::  MDATA = 20000
   INTEGER, PARAMETER    ::  NLEV = 17
   INTEGER, PARAMETER    ::  NVARS = 9
   INTEGER               ::  KPDS(25,MTOTL),KGDS(22,MTOTL)
   INTEGER               ::  LSKIP(MTOTL),LGRIB(MTOTL)
   INTEGER               ::  KKPDS(25,NVARS*NLEV),KKGDS(22,NVARS*NLEV)
   LOGICAL                   LLEVL,LLTYP
   COMMON                    /COMCTL/RLEV(MTOTL),LABEL(3,MTOTL),LENLA(3,MTOTL), &
                             LABBR(MTOTL),LVABBR(MTOTL),                        &
                             ILINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL),         &
                             NLTOTL(MTOTL),NVTOTL,NPTOTL,LLEVL,LLTYP
!
!  NAMELIST PARAMETERS:
!
!  FNGRIB ... GRIB file name(s). 
!             Dimension for history sequence (in forecast time or initial)
!  FNPRINT ... File name where printouts from premap program goes
!  LDEBUG  ... TRUE for debugging printout. Output goes to FNPRINT
!
!  NAMELIST VARIABLE TYPES
! 
   INTEGER, PARAMETER    ::  MFILE = 250
   CHARACTER(LEN = 128)  ::  FNGRIB(MFILE)
   CHARACTER(LEN = 128)  ::  FNGRBO
   CHARACTER(LEN = 128)  ::  FNPRINT
   CHARACTER(LEN = 80 )  ::  FNKPDS5,FNKPDS6
   DATA                      MSK1/32000/,MSK2/4000/
!
!  NAMELIST DEFAULTS
!
   DATA                      FNGRIB /MFILE*'        '/
   DATA                      FNPRINT/'./grvar.out'/
   DATA                      FNKPDS5/'/home/sgi90/wd23ln/etc/grib1.kpds5.vsn21'/
   DATA                      FNKPDS6/'/home/sgi90/wd23ln/etc/grib1.kpds6.vsn21'/
!
   LOGICAL                   LAINC
   LOGICAL                   LDEBUG
   DATA                      LAINC/.TRUE./
   DATA                      LDEBUG/.TRUE./
!
!  Fort Unit numbers used inside the program
!
!  LUPTR  ... Unit unmber for diagnostic/debug print output other than uNIPLO
!  LUPGB  ... Unit unmber for input (grib) file
!  LUPGO  ... Unit unmber for grib output file
!
   DATA                      LUPGB/21/
   DATA                      LUPGO/51/
   DATA                      LUPTR/50/
!
   INTEGER, PARAMETER    ::  MBUF = 1024*128*64
   CHARACTER(LEN = 1 )   ::  CBUF(MBUF),DBUF(MBUF)
!
   CHARACTER(LEN = 80)   ::  ASGNSTR
   CHARACTER(LEN = 8 )   ::  CLEV
   CHARACTER(LEN = 2 )   ::  CYEAR,CMONTH,CDAY,CHOUR
   CHARACTER(LEN = 3 )   ::  CFHOUR1,CFHOUR2
!
!  arrays for calculating transients
!
   REAL                  ::  AVGRID(MDATA,NLEV*NVARS)
!
   REAL                  ::  UGRID(MDATA,NLEV),VGRID(MDATA,NLEV)
   REAL                  ::  TGRID(MDATA,NLEV)
   REAL                  ::  VVELGRID(MDATA,NLEV),ZGRID(MDATA,NLEV)
!
   INTEGER               ::  ULEV(NLEV),VLEV(NLEV)
   INTEGER               ::  TLEV(NLEV)
   INTEGER               ::  VVLEV(NLEV),ZLEV(NLEV)
!
   INTEGER               ::  UUPDS(25,NLEV),UVPDS(25,NLEV),VVPDS(25,NLEV)
   INTEGER               ::  UTPDS(25,NLEV),VTPDS(25,NLEV)
   INTEGER               ::  UZPDS(25,NLEV),VZPDS(25,NLEV)
   INTEGER               ::  TTPDS(25,NLEV)
   INTEGER               ::  TVVPDS(25,NLEV)
!
   INTEGER               ::  UUGDS(25,NLEV),UVGDS(25,NLEV),VVGDS(25,NLEV)
   INTEGER               ::  UTGDS(25,NLEV),VTGDS(25,NLEV)
   INTEGER               ::  UZGDS(25,NLEV),VZGDS(25,NLEV)
   INTEGER               ::  TTGDS(25,NLEV)
   INTEGER               ::  TVVGDS(25,NLEV)
!
   REAL                  ::  GRID(MDATA)
   LOGICAL*1                 LBMS(MDATA)
!
   LOGICAL                   LDBG
   COMMON                    /COMDBG/ LDBG
!
   PARAMETER(LENPDS=28,LENGDS=32,MXBIT=16)
   REAL                  ::  FR(MDATA)
   INTEGER               ::  IBM(MDATA)
!
!     CHARACTER GRIB(30+LENPDS+LENGDS+MDATA*(MXBIT+1)/8)
!
   CHARACTER             ::  GRIB(30+LENPDS+LENGDS+MDATA*(MXBIT+1)/8)
!
   REAL*4                    GRID4(MDATA)
   INTEGER*4                 JPDS4(25),JGDS4(22),JENS4(5)
   INTEGER*4                 MPDS4(25),MGDS4(22),MENS4(5)
   INTEGER*4                 LUPGB4,LUPTR4,MSK14,MSK24,MNUM4,MBUF4
   INTEGER*4                 NLEN4,NNUM4,IRET4,NDATA4
   INTEGER*4                 KPDS4(25,MTOTL),KGDS4(22,MTOTL)
   INTEGER*4                 LSKIP4(MTOTL),LGRIB4(MTOTL)
!
!  Open main program (debug) print file
!
   OPEN(LUPTR,FILE=FNPRINT,ERR=711)
   !
   GO TO 712
   !
   711 CONTINUE
   !
   PRINT *,' ERROR IN OPENING FILE ',FNPRINT
   CALL ABORT
   !
   712 CONTINUE
   !
   WRITE(LUPTR,*) ' FILE ',FNPRINT(1:50),' opened.  Unit=',LUPTR
!
!  Read main program namelist
!
   READ (5,'(A80)') FNKPDS5
   READ (5,'(A80)') FNKPDS6
   WRITE(6,'(A80)') FNKPDS5
   WRITE(6,'(A80)') FNKPDS6
!
   LDBG=LDEBUG
!
   READ (5,'(I10)') INCHR
   WRITE(6,'(I10)') INCHR
   READ (5,'(A128)') FNGRBO
   WRITE(6,'(A128)') FNGRBO
!
   READ (5,*,END=908) NFILES
   !
   GO TO 907
   !
   908 CONTINUE
   !
   WRITE(6,*) ' EOF on data'
   CALL ABORT
   !
   907 CONTINUE
   !
   WRITE(6,*) ' NFILES=',NFILES
   IF(NFILES.EQ.0) THEN
      CALL ABORT
   ENDIF 
!
   IF(NFILES.GT.MFILE) THEN
      WRITE(LUPTR,*) ' ERROR!!!  Number of files exceeded limit.'
      PRINT *, ' ERROR!!!  Number of files exceeded limit.'
      CALL ABORT
   ENDIF
!
   DO N = 1,NFILES
      READ (5,'(A128)') FNGRIB(N)
      WRITE(6,'(8H FNGRIB=,A80)') FNGRIB(N)
   ENDDO
!
   READ(5,*) IFTS
   READ(5,*) IFTE
   WRITE(6,*) 'IFTS,IFTE=',IFTS,IFTE
!
   DO N = 1,MTOTL
      LABBR(N)='        '
   ENDDO
!
   CALL INIPARM(FNKPDS5,LUPTR)
   CALL INILEVL(FNKPDS6,LUPTR)
!
!  Get grib index buffer
!
!  OPEN(UNIT=LUPGB,FILE=FNGRIB(1),FORM='UNFORMATTED',ERR=978)
!-sgi9-f90
!
   LUPGB4=LUPGB
   CALL BAOPEN(LUPGB4,FNGRIB(1),IRET4)
   IRET=IRET4
   IF(IRET.NE.0) GO TO 978
!-sgi9-f90
   !
   GO TO 977
   !
   978 CONTINUE
   !
   WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNGRIB(1)
   PRINT *,'ERROR IN OPENING FILE ',FNGRIB(1)
   CALL ABORT
   !
   977 CONTINUE
   !
   WRITE(LUPTR,*) ' FILE ',FNGRIB(1)(1:80),' opened. Unit=',LUPGB
!
   MNUM=0
!
!  BUILD VARIABLE NAME TABLE FROM THE FIRST GRIB FILE
!
   LUPGB4=LUPGB
   MSK14=MSK1
   MSK24=MSK2
   MNUM4=MNUM
   MBUF4=MBUF
   CALL GETGIR(LUPGB4,MSK14,MSK24,MNUM4,MBUF4,CBUF,NLEN4,NNUM4,IRET4)
   NLEN=NLEN4
   NNUM=NNUM4
   IRET=IRET4
!
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
      WRITE(LUPTR,*) 'ERROR!!! Number of parameters in the index',             &
                     ' buffer exceeded limit of ',MTOTL,' Increase to:',NNUM
      PRINT *,'ERROR!!! Number of parameters in the index',                    &
              ' buffer exceeded limit of ',MTOTL,' Increase to:',NNUM
      CALL ABORT
   ENDIF
!
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
!
   IMAX=KGDS(2,1)
   JMAX=KGDS(3,1)
   IJMAX=IMAX*JMAX
   IF(IJMAX.GT.MDATA) THEN
      WRITE(6,*) 'INCREASE MDATA to ',IJMAX
      CALL ABORT
   ENDIF
!
   NTOTL=NNUM
!
   WRITE(LUPTR,*) ' NTOTL=',NTOTL
!
   WRITE(LUPTR,*) ' KPDS=',(KPDS(I,1),I=1,25)
   WRITE(LUPTR,*) ' KGDS=',(KGDS(I,1),I=1,22)
!
   DO N = 1,NTOTL
      CALL GETPARM(KPDS(1,N),1,LABEL(1,N),LENLA(1,N),LABBR(N),LUPTR)
      CALL GETLEVL(KPDS(1,N),1,LABEL(1,N),LENLA(1,N),RLEV(N),LVABBR(N),LUPTR)
   ENDDO
!
!  Get rid of unnecessary spaces from LABEL
!
   DO N = 1,NTOTL
      CALL RMBLNK(LABEL(1,N),LENLA(1,N),3)
   ENDDO
!
   CALL FLSRCH(NTOTL,LUPTR)
!
   DO N = 1,NTOTL
      WRITE(LUPTR,*) N,')',LABEL(1,N)(1:LENLA(1,N)),' (',LABBR(N),' ) ',       &
                    ' at ',LABEL(2,N)(1:LENLA(2,N)),' ',                       &
                           LABEL(3,N)(1:LENLA(3,N))
   ENDDO
!
   DO N = 1,NLEV*NVARS
      DO IJ=1,IJMAX
         AVGRID(IJ,N)=0.
      ENDDO
   ENDDO
!
   NU=0
   NV=0
   NT=0
   NVV=0
   NZ=0
   DO K=1,NVTOTL
      IF(LABBR(IVINDX(K)).EQ.'UGRD') NU=K
      IF(LABBR(IVINDX(K)).EQ.'VGRD') NV=K
      IF(LABBR(IVINDX(K)).EQ.'TMP')  NT=K
      IF(LABBR(IVINDX(K)).EQ.'VVEL') NVV=K
      IF(LABBR(IVINDX(K)).EQ.'ABSV') NZ=K
   ENDDO
   IF(LDEBUG) THEN
      WRITE(LUPTR,*) ' '
      WRITE(LUPTR,*) 'NU=',NU,' NV=',NV,' NT=',NT,' NVV=',NVV,' NZ=',NZ
   ENDIF
!
!  have levels of each var and see if they are the same
!  (calculate trs only when these var on the same levels) 
!  define the PDS5 for transients
!  (P. PENG July 1999)
!
   KU=0
   KV=0
   KT=0
   KVV=0
   KZ=0
   DO N = 1,NTOTL
      IF(KPDS(5,N).EQ.33) THEN
         KU=KU+1
         ULEV(KU)=KPDS(7,N)
         DO K = 1,25
            UUPDS(K,KU)=KPDS(K,N)
            UVPDS(K,KU)=KPDS(K,N)
         ENDDO
         UUPDS(5,KU)=186
         UVPDS(5,KU)=196
         UUPDS(22,KU)=4
         UVPDS(22,KU)=4
         DO K = 1,22
            UUGDS(K,KU)=KGDS(K,N)
            UVGDS(K,KU)=KGDS(K,N)
         ENDDO
      ENDIF
      IF(KPDS(5,N).EQ.34) THEN
         KV=KV+1
         VLEV(KV)=KPDS(7,N)
         DO K = 1,25
            VVPDS(K,KV)=KPDS(K,N)
         ENDDO
         VVPDS(5,KV)=187
         VVPDS(22,KV)=4
         DO K = 1,22
            VVGDS(K,KV)=KGDS(K,N)
         ENDDO
      ENDIF
      IF(KPDS(5,N).EQ.11) THEN
         KT=KT+1
         TLEV(KT)=KPDS(7,N)
         DO K = 1,25
            UTPDS(K,KT)=KPDS(K,N)
            VTPDS(K,KT)=KPDS(K,N)
            TTPDS(K,KT)=KPDS(K,N)
         ENDDO
         UTPDS(5,KT)=197
         VTPDS(5,KT)=198
         TTPDS(5,KT)=189
         UTPDS(22,KT)=4
         VTPDS(22,KT)=4
         TTPDS(22,KT)=4
         DO K = 1,22
            UTGDS(K,KT)=KGDS(K,N)
            VTGDS(K,KT)=KGDS(K,N)
            TTGDS(K,KT)=KGDS(K,N)
         ENDDO
      ENDIF
      IF(KPDS(5,N).EQ.41) THEN
         KZ=KZ+1
         ZLEV(KZ)=KPDS(7,N)
         DO K = 1,25
            UZPDS(K,KZ)=KPDS(K,N)
            VZPDS(K,KZ)=KPDS(K,N)
         ENDDO
         UZPDS(5,KZ)=202
         VZPDS(5,KZ)=203
         UZPDS(22,KZ)=8
         VZPDS(22,KZ)=8
         DO K = 1,22
            UZGDS(K,KZ)=KGDS(K,N)
            VZGDS(K,KZ)=KGDS(K,N)
         ENDDO
      ENDIF 
      IF(KPDS(5,N).EQ.39) THEN
         KVV=KVV+1
         VVLEV(KVV)=KPDS(7,N)
         DO K = 1,25
            TVVPDS(K,KVV)=KPDS(K,N)
         ENDDO
         TVVPDS(5,KVV)=194
         TVVPDS(22,KVV)=4
         DO K = 1,22
            TVVGDS(K,KVV)=KGDS(K,N)
         ENDDO
      ENDIF 
   ENDDO
!
   DO N=1,NLEV
      IF(ULEV(N).NE.VLEV(N).OR.ULEV(N).NE.TLEV(N)) THEN
         PRINT *,'NUMBER OF LEVELS FOR U, V AND T DOES NOT MATCH'
         CALL ABORT
      ENDIF
      IF(ULEV(N).NE.ZLEV(N).OR.ULEV(N).NE.VVLEV(N)) THEN
         PRINT *,'NUMBER OF LEVELS FOR U, ZETA AND OMGA DOES NOT MATCH'
         CALL ABORT
      ENDIF
   ENDDO
!
   NTRANSL=NVARS*NLTOTL(NU)    
!
!  Loop through GRIB file
!
   DO NFILE = 1,NFILES
!
      PRINT *,' Processing ', FNGRIB(NFILE)(1:80)
!
!  Get grib index buffer for this particular grib file
!
!     OPEN(UNIT=LUPGB,FILE=FNGRIB(NFILE),FORM='UNFORMATTED',ERR=878)
!
      LUPGB4=LUPGB
      CALL BAOPEN(LUPGB4,FNGRIB(NFILE),IRET4)
      IRET=IRET4
      IF(IRET.NE.0) GO TO 878
      !
      GO TO 877
      !
      878 CONTINUE
      !
      WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNGRIB(NFILE)
      PRINT *,'ERROR IN OPENING FILE ',FNGRIB(NFILE)
      CALL ABORT
      !
      877 CONTINUE
      !
      WRITE(LUPTR,*) FNGRIB(NFILE)(1:80),' opened. Unit=',LUPGB
      MNUM=0
!
      DO IJ = 1,IJMAX
         LBMS(IJ)=.TRUE.
      ENDDO
!
      MNUM4=0
      LUPGB4=LUPGB
      MSK14=MSK1
      MSK24=MSK2
      MBUF4=MBUF
      CALL GETGIR(LUPGB4,MSK14,MSK24,MNUM4,MBUF4,CBUF,NLEN4,NNUM4,IRET4)
      NLEN=NLEN4
      NNUM=NNUM4
      IRET=IRET4
!
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
         WRITE(LUPTR,*) 'ERROR!!! Number of parameters in the index',          &
                  ' buffer exceeded limit of ',MTOTL,' Increase to:',NNUM
         PRINT *,'ERROR!!! Number of parameters in the index',                 &
                  ' buffer exceeded limit of ',MTOTL,' Increase to:',NNUM
         CALL ABORT
      ENDIF
!
!  Assuming that the record comes in the same order so that
!  file is not rewound.
!
      KU=0
      KV=0
      KT=0
      KZ=0
      KVV=0
      NN4=0
      DO N = 1,NTOTL
         DO I = 1,25
            JPDS4(I)=-1
         ENDDO
         DO I = 5,7
            JPDS4(I)=KPDS4(I,N)
         ENDDO
         DO I = 1,22
            JGDS4(I)=-1
         ENDDO
         DO I = 1,5
            JENS4(I)=-1
         ENDDO
         NLEN4=NLEN
         NNUM4=NNUM
!
         CALL GETGBSS(CBUF,NLEN4,NNUM4,NN4,JPDS4,JGDS4,JENS4,                  &
                      K4,MPDS4,MGDS4,MENS4,MSKIP4,MGRIB4,IRET4)
         IF(MGRIB4.EQ.0) THEN
            WRITE(LUPTR,*) ' Error in GETGBSS.  Field not found.'
            CALL ABORT
         ENDIF
!
!  TIME CHECK. INCHR.LT.0 IS USED FOR CHECKING FORECAST HOURS
!
         IF(N.EQ.1) THEN
            PRINT *,'YMDHF=',MPDS4(8),MPDS4(9),MPDS4(10),MPDS4(11),MPDS4(14)
            WRITE(LUPTR,*) 'YMDHF=',MPDS4(8),MPDS4(9),MPDS4(10),MPDS4(11),     &
                                    MPDS4(14)
         ENDIF
!
         IF(IFTS.GE.0) THEN
            IF(NFILE.EQ.1.AND.N.EQ.1) THEN
               INIFH=KPDS(14,N)
               IFH=KPDS(15,N)
            ENDIF
         ELSE
            IF(NFILE.EQ.1) INIFH=MPDS4(14)
            IF(NFILE.EQ.NFILES) IFH=MPDS4(14)
         ENDIF
!
         IF(INCHR.NE.0) THEN
            IF(N.EQ.1) THEN
               IF(INCHR.GT.0) THEN
                  IF(NFILE.GT.1) THEN
                     CALL INCDTE(IYPR,IMPR,IDPR,IHPR,JY,JM,JD,JH,INCHR)
                  ENDIF
                  IYPR=MPDS4(8)
                  IMPR=MPDS4(9)
                  IDPR=MPDS4(10)
                  IHPR=MPDS4(11)
               ELSE
                  IF(NFILE.GT.1) THEN
                     JFH=IFPR-INCHR
                  ENDIF
                  IFPR=MPDS4(14)
               ENDIF
            ENDIF
!
            IF(NFILE.GT.1) THEN
               IF(INCHR.GT.0) THEN
                  IF(JY.NE.MPDS4( 8).OR.JM.NE.MPDS4( 9).OR.                       &
                     JD.NE.MPDS4(10).OR.JH.NE.MPDS4(11)) THEN
                     PRINT *,'SHOULD BE:',JY,JM,JD,JH
                     WRITE(LUPTR,*) 'SHOULD BE:',JY,JM,JD,JH
                     PRINT *,'BUT ARE  :',MPDS4(8),MPDS4(9),MPDS4(10),MPDS4(11)
                     WRITE(LUPTR,*) 'BUT ARE  :',MPDS4(8),MPDS4(9),MPDS4(10),     &
                                                 MPDS4(11)
                     PRINT *,'FILE NOT REGULAR INTERVAL IN ANALYSIS TIME'
                     WRITE(LUPTR,*) 'FILE NOT REGULAR INT IN ANALY TIME'
                     CALL ABORT
                  ENDIF
               ELSE
                  IF(JFH.NE.MPDS4(14)) THEN
                     PRINT *,'SHOULD BE:',JFH
                     PRINT *,'BUT ARE  :',MPDS4(14)
                     PRINT *,'FILE NOT REGULAR INTERVAL IN FORECAST TIME'
                     WRITE(LUPTR,*) 'SHOULD BE:',JFH
                     WRITE(LUPTR,*) 'BUT ARE  :',MPDS4(14)
                     WRITE(LUPTR,*) 'FILE NOT REGULAR INTVL IN FCST TIME'
                     CALL ABORT
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
!
         IF(IFTS.GE.0) THEN
            IF(MPDS4(14).LT.IFTS) THEN
               PRINT *,'File skipped'
               WRITE(LUPTR,*) 'File skipped'
               GO TO 8200
            ENDIF
         ENDIF
         IF(IFTE.GE.0) THEN
            IF(MPDS4(14).GT.IFTE) GO TO 8300
         ENDIF
!
         DO IJ = 1,IJMAX
            LBMS(IJ)=.TRUE.
         ENDDO
!
         LUPGB4=LUPGB
         LUPTR4=LUPTR
         CALL RDGB(LUPGB4,MGRIB4,MSKIP4,MPDS4,MGDS4,NDATA4,LBMS,GRID4,LUPTR4)
!
         IF(NDATA4.NE.IJMAX) THEN
            WRITE(LUPTR,*) ' ERROR IN RDGB. NDATA.NE.IJMAX'
            WRITE(LUPTR,*) ' NDATA=',NDATA4,' IJMAX=',IJMAX
            WRITE(LUPTR,*) ' NFILE=',NFILE
            WRITE(LUPTR,*) ' FNGRIB=',FNGRIB(NFILE)
            PRINT *,' ERROR IN RDGB.   NDATA.NE.IJMAX'
            CALL ABORT
         ENDIF
!
!  FILL the arrays UGRID, VGRID, TGRID ... for transients 
!  (P. PENG Jul 1999)
!
         DO IJ = 1,IJMAX
            IF(.NOT.LBMS(IJ)) THEN
               PRINT *,'LBMS CONTAINS .FALSE. VALUES'
               CALL ABORT
            ENDIF
         ENDDO
!
         IF(KPDS(5,N).EQ.33) THEN
            KU=KU+1
            DO IJ = 1,IJMAX
               UGRID(IJ,KU)=GRID4(IJ)
            ENDDO
         ENDIF
         IF(KPDS(5,N).EQ.34) THEN
            KV=KV+1
            DO IJ = 1,IJMAX
               VGRID(IJ,KV)=GRID4(IJ)
            ENDDO
         ENDIF
         IF(KPDS(5,N).EQ.11) THEN
            KT=KT+1
            DO IJ = 1,IJMAX
               TGRID(IJ,KT)=GRID4(IJ)
            ENDDO
         ENDIF
         IF(KPDS(5,N).EQ.41) THEN
            KZ=KZ+1
            DO IJ = 1,IJMAX
               ZGRID(IJ,KZ)=GRID4(IJ)
            ENDDO
         ENDIF 
         IF(KPDS(5,N).EQ.39) THEN
            KVV=KVV+1
            DO IJ = 1,IJMAX
               VVELGRID(IJ,KVV)=GRID4(IJ)
            ENDDO
         ENDIF 
      ENDDO       
!
      WRITE(LUPTR,*) ' KU=',KU,' KZ=',KZ
      WRITE(LUPTR,*) ' ULEV=',ULEV
      WRITE(LUPTR,*) ' VVLEV=',VVLEV
      WRITE(LUPTR,*) ' KPDS(7,N)=',(KPDS(7,N),N=1,NTOTL)
!
! transients hh uu vv uv ut vt tt tvel uvort vvort (P. PENG Jul 1999)
!
      DO NL = 1,NLTOTL(NU)
         DO IJ = 1,IJMAX
            LV1=NL
            AVGRID(IJ,LV1)=AVGRID(IJ,LV1)+UGRID(IJ,NL)*UGRID(IJ,NL)
         ENDDO
      ENDDO
!
      DO NL = 1,NLTOTL(NU)
         DO IJ = 1,IJMAX
            LV2=NL+NLTOTL(NU)
            AVGRID(IJ,LV2)=AVGRID(IJ,LV2)+UGRID(IJ,NL)*VGRID(IJ,NL)
         ENDDO
      ENDDO
!
      DO NL = 1,NLTOTL(NV)
         DO IJ = 1,IJMAX
            LV3=NL+2*NLTOTL(NU)
            AVGRID(IJ,LV3)=AVGRID(IJ,LV3)+VGRID(IJ,NL)*VGRID(IJ,NL)
         ENDDO
      ENDDO
!
      DO NL = 1,NLTOTL(NT)
         DO IJ = 1,IJMAX
            LV4=NL+3*NLTOTL(NU)
            AVGRID(IJ,LV4)=AVGRID(IJ,LV4)+UGRID(IJ,NL)*TGRID(IJ,NL)
         ENDDO
      ENDDO
!
      DO NL = 1,NLTOTL(NT)
         DO IJ = 1,IJMAX
            LV5=NL+4*NLTOTL(NU)
            AVGRID(IJ,LV5)=AVGRID(IJ,LV5)+VGRID(IJ,NL)*TGRID(IJ,NL)
         ENDDO
      ENDDO
!
      DO NL = 1,NLTOTL(NT)
         DO IJ = 1,IJMAX
            LV6=NL+5*NLTOTL(NU)
            AVGRID(IJ,LV6)=AVGRID(IJ,LV6)+TGRID(IJ,NL)*TGRID(IJ,NL)
         ENDDO
      ENDDO
!
      DO NL = 1,NLTOTL(NZ)
         DO IJ = 1,IJMAX
            LV7=NL+6*NLTOTL(NU)
            AVGRID(IJ,LV7)=AVGRID(IJ,LV7)+UGRID(IJ,NL)*ZGRID(IJ,NL)
         ENDDO
      ENDDO
!
      DO NL = 1,NLTOTL(NZ)
         DO IJ = 1,IJMAX
            LV8=NL+7*NLTOTL(NU)
            AVGRID(IJ,LV8)=AVGRID(IJ,LV8)+VGRID(IJ,NL)*ZGRID(IJ,NL)
         ENDDO
      ENDDO
!
      DO NL = 1,NLTOTL(NT)
         DO IJ = 1,IJMAX
            LV9=NL+8*NLTOTL(NU)
            AVGRID(IJ,LV9)=AVGRID(IJ,LV9)+VVELGRID(IJ,NL)*TGRID(IJ,NL)
         ENDDO
      ENDDO
      !
      8200 CONTINUE
      !
   ENDDO
   !
   8300 CONTINUE
   !
!
! Define output PDS & GDS for vars (transients) (PENG, July 1999)
!
   DO NL = 1,NLTOTL(NU)
      DO i = 1,25
         NUU=NL
         KKPDS(i,NUU)=UUPDS(i,NL)
         NUV=NUU+NLTOTL(NU)
         KKPDS(i,NUV)=UVPDS(i,NL)
         NVV=NUU+2*NLTOTL(NU)
         KKPDS(i,NVV)=VVPDS(i,NL)
         NUT=NUU+3*NLTOTL(NU)
         KKPDS(i,NUT)=UTPDS(i,NL)
         NVT=NUU+4*NLTOTL(NU)
         KKPDS(i,NVT)=VTPDS(i,NL)
         NTT=NUU+5*NLTOTL(NU)
         KKPDS(i,NTT)=TTPDS(i,NL)
         NUZ=NUU+6*NLTOTL(NU)
         KKPDS(i,NUZ)=UZPDS(i,NL)
         NVZ=NUU+7*NLTOTL(NU)
         KKPDS(i,NVZ)=VZPDS(i,NL)
         NTVEL=NUU+8*NLTOTL(NU)
         KKPDS(i,NTVEL)=TVVPDS(i,NL)
      ENDDO
   ENDDO
!
   DO NL = 1,NLTOTL(NU)
      DO i = 1,22
         NUU=NL
         KKGDS(i,NUU)=UUGDS(i,NL)
         NUV=NUU+NLTOTL(NU)
         KKGDS(i,NUV)=UVGDS(i,NL)
         NVV=NUU+2*NLTOTL(NU)
         KKGDS(i,NVV)=VVGDS(i,NL)
         NUT=NUU+3*NLTOTL(NU)
         KKGDS(i,NUT)=UTGDS(i,NL)
         NVT=NUU+4*NLTOTL(NU)
         KKGDS(i,NVT)=VTGDS(i,NL)
         NTT=NUU+5*NLTOTL(NU)
         KKGDS(i,NTT)=TTGDS(i,NL)
         NUZ=NUU+6*NLTOTL(NU)
         KKGDS(i,NUZ)=UZGDS(i,NL)
         NVZ=NUU+7*NLTOTL(NU)
         KKGDS(i,NVZ)=VZGDS(i,NL)
         NTVEL=NUU+8*NLTOTL(NU)
         KKGDS(i,NTVEL)=TVVGDS(i,NL)
      ENDDO
   ENDDO
!
!  Open output grib file
!
   OPEN(UNIT=LUPGO,FILE=FNGRBO,FORM='UNFORMATTED',ERR=968)
   !
   GO TO 967
   !
   968 CONTINUE
   !
   WRITE(LUPTR,*) ' ERROR IN OPENING FILE ',FNGRBO
   PRINT *,'ERROR IN OPENING FILE ',FNGRBO
   CALL ABORT
   !
   967 CONTINUE
   !
   WRITE(LUPTR,*) ' FILE ',FNGRBO(1:128),' opened. Unit=',LUPGO
   PRINT *,' FILE ',FNGRBO(1:128),' opened. Unit=',LUPGO
!
   DO N = 1,NTRANSL      
      DO IJ = 1,IJMAX
         LBMS(IJ)=.TRUE.
         AVGRID(IJ,N)=AVGRID(IJ,N)/FLOAT(NFILES)
      ENDDO
!
!  MODIFY KPDS FOR AVERAGE
!
!  Number in the avarage
!
      KKPDS(17,N) = NFILES
!
      IF(INIFH.LT.IFH) LAINC=.FALSE.
!
      IF(LAINC) THEN
!
!  This is many initial conditions
!
         IF((KKPDS(16,N).EQ.10).AND.(KKPDS(14,N).EQ.0)) THEN
!
!  THIS IS ANALYSIS AVERAGED 
!
            WRITE(LUPTR,*) ' THE INPUT GRIB MESSAGE SHOWS:'
            WRITE(LUPTR,*) ' INSTANT PROD VALID AT REFERENCE TIME + P1'
            KKPDS(16,N) = 123
            KKPDS(15,N) = INCHR
         ELSEIF ((KKPDS(16,N).EQ.10).AND.(KKPDS(14,N).NE.0)) THEN
!
!  THIS IS FORECASTS AT HOUR KKPDS(14,1) AVERAGED
!
            WRITE(LUPTR,*) ' THE INPUT GRIB MESSAGE SHOWS:'
            WRITE(LUPTR,*) '  FORECAST PRODUCT VALID AT REF TIME + P1 '
            KKPDS(16,N) = 113
            KKPDS(15,N) = INCHR
         ELSEIF (KKPDS(16,N).EQ.3 ) THEN
!
!  THIS IS AVERAGE OF MANY AVERAGES BETWEEN THE FORECAST HOUR 
!  KKPDS(14,N) and KKPDS(15,N). EXAMPLE MANY FLUX FILES 
!
            WRITE(LUPTR,*) ' THE INPUT GRIB MESSAGE SHOWS:'
            WRITE(LUPTR,*) ' AVERAGE ( REF TIME + P1 TO REF T + P2 )'
            IF(IFTS.EQ.0) THEN
               WRITE(LUPTR,*) ' THE INPUT GRIB MESSAGE SHOWS:'
               WRITE(LUPTR,*) ' AVERAGE ( REF TIME + P1 TO REF T + P2 )'
               KKPDS(14,N) = KKPDS(14,N)/2     ! Octet no. 19
               KKPDS(16,N) = 113               ! Octet no. 21
               KKPDS(15,N) = INCHR
            ELSEIF(IFTS.EQ.-1) THEN
               KKPDS(14,N) = INIFH/24
               KKPDS(15,N) = IFH/24
               KKPDS(13,N) = 2
               KKPDS(16,N) = 3
            ELSEIF(IFTS.EQ.-2) THEN
               KKPDS(14,N) = (INIFH/24+1)/30
               KKPDS(15,N) = (IFH/24+1)/30
               KKPDS(13,N) = 3
               KKPDS(16,N) = 3
               PRINT *,'KKPDS(14,N)=',KKPDS(14,N),'KKPDS(15,N)=',KKPDS(15,N)
            ENDIF
         ELSE
            WRITE(LUPTR,*) ' THE INPUT GRIB MESSAGE SHOWS: ',KKPDS(16,N)
            WRITE(LUPTR,*) ' OUTPUT GRIB MESSAGE MAY NOT BE CORRECT '
         ENDIF
      ELSE
!
!  THIS IS THE AVERAGE FOR THE FORECAST HOUR
!
         WRITE(LUPTR,*) ' THE INPUT GRIB MESSAGE SHOWS:'
         WRITE(LUPTR,*) ' SIMPLY A FORECAST HISTORY FILE'
         IF(IFTS.GE.0) THEN
            KKPDS(14,N) = INIFH
            KKPDS(15,N) = IFH
         ELSEIF(IFTS.EQ.-1) THEN
            KKPDS(14,N) = INIFH/24
            KKPDS(15,N) = IFH/24
            KKPDS(13,N) = 2
         ELSEIF(IFTS.EQ.-2) THEN
            KKPDS(14,N) = (INIFH/24+1)/30
            KKPDS(15,N) = (IFH/24+1)/30
            KKPDS(13,N) = 3
         ENDIF
         KKPDS(16,N) = 3
      ENDIF
!
      WRITE(LUPTR,*) 'MAXMIN of AVGRID N=',N
      CALL MAXMIN(AVGRID(1,N),IJMAX,1,IJMAX,1,1,LUPTR)
!
      WRITE(LUPTR,*) 'IJMAX=',IJMAX
      WRITE(LUPTR,*) 'KKPDS=',(KKPDS(I,N),I=1,25)
      WRITE(LUPTR,*) 'KKGDS=',(KKGDS(I,N),I=1,22)
      CALL PUTGB(LUPGO,IJMAX,KKPDS(1,N),KKGDS(1,N),LBMS,AVGRID(1,N),IBM,FR,    &
                 GRIB,IGRIB,LUPTR,IRET)
      IF(IRET.NE.0) THEN
         WRITE(LUPTR,*) 'ERROR IN PUTGB'
         CALL ABORT
      ELSE
         WRITE(LUPTR,*) 'GRIB-END=',(GRIB(I),I=IGRIB-3,IGRIB)
         WRITE(LUPTR,*) N,') GRIB RECORD WRITTEN. LGRIB=',IGRIB
      ENDIF
   ENDDO
!
   CLOSE(LUPGO)
   CLOSE(LUPTR)
!
   STOP
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE INIPARM(FNKPDS5,LUPTR)
!
!     Initialize KPDS(5) table
!
   INTEGER, PARAMETER   ::  LUKP5 = 90
   INTEGER, PARAMETER   ::  NKPDS5 = 255
   CHARACTER(LEN = 80)  ::  FNKPDS5
   CHARACTER(LEN = 64)  ::  PINFO(0:NKPDS5)
   CHARACTER(LEN = 64)  ::  PNAME(0:NKPDS5)
   CHARACTER(LEN = 64)  ::  PUNIT(0:NKPDS5)
   CHARACTER(LEN = 80)  ::  CPDS5
   CHARACTER(LEN = 80)  ::  STRING
   INTEGER              ::  LENPI(0:NKPDS5),LENPN(0:NKPDS5),LENPU(0:NKPDS5)
!
   COMMON                   /COMPRM/ PINFO,PUNIT,PNAME,LENPI,LENPU,LENPN
!
!  Open Parameter table (PDS5) file
!
   OPEN(LUKP5,FILE=FNKPDS5,STATUS='OLD',ERR=1)
   !
   GO TO 2
   !
   1 CONTINUE
   !
   WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNKPDS5
   PRINT *,'ERROR IN OPENING FILE ',FNKPDS5
   CALL ABORT
   !
   2 CONTINUE
   !
   WRITE(LUPTR,*) 'FILE ',FNKPDS5(1:50),' opened.  Unit=',LUKP5
!
   N=1
   !
   300 CONTINUE
   !
   READ(LUKP5,100,END=200) STRING
   !
   100 FORMAT(A80)
   !
   IS=1
   NDELIM=0
   DO 500 I = 1,80
      IF(STRING(I:I).EQ.':') THEN
         NDELIM=NDELIM+1
         NCHAR=I-1-IS+1
         IF(NCHAR.GT.0) THEN
            IF(NDELIM.EQ.1) THEN
               CPDS5(1:NCHAR)=STRING(IS:I-1)
               READ(CPDS5,110) M
               !
               110 FORMAT(I3)
               !
            ELSEIF(NDELIM.EQ.2) THEN
               JJ=0
               DO J = IS+1,I-1
                  IF(STRING(J-1:J-1).NE.' '.OR.STRING(J:J).NE.' ') THEN
                     JJ=JJ+1
                     PINFO(M)(JJ:JJ)=STRING(J-1:J-1)
                  ENDIF
               ENDDO            
               IF(STRING(I-1:I-1).NE.' ') THEN
                  JJ=JJ+1
                  PINFO(M)(JJ:JJ)=STRING(I-1:I-1)
               ENDIF
               LENPI(M)=JJ
            ELSEIF(NDELIM.EQ.3) THEN
               JJ=0
               DO J = IS,I-1
                  IF(STRING(J:J).NE.' ') THEN
                     JJ=JJ+1
                     PUNIT(M)(JJ:JJ)=STRING(J:J)
                  ENDIF
               ENDDO 
               LENPU(M)=JJ
               JJ=0
               DO J = I+1,80
                  IF(STRING(J:J).NE.' ') THEN
                     JJ=JJ+1
                     PNAME(M)(JJ:JJ)=STRING(J:J)
                  ENDIF
               ENDDO 
               LENPN(M)=JJ
               !
               GO TO 500
               !
            ENDIF
            IS=I+1
         ENDIF
      ENDIF
   !
   500 CONTINUE
   !
!
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
!
    N=N+1
    !
    GO TO 300
    !
    200 CONTINUE
    !
!
    CLOSE(UNIT=LUKP5)
    RETURN
!     
    END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE INILEVL(FNKPDS6,LUPTR)
!
   INTEGER, PARAMETER   ::  LUKP6 = 90
   INTEGER, PARAMETER   ::  NKPDS6 = 255
   CHARACTER(LEN = 80)  ::  PDS6 (0:NKPDS6)
   CHARACTER(LEN = 64)  ::  LINFO(0:NKPDS6)
   CHARACTER(LEN = 64)  ::  LUNIT(0:NKPDS6)
   CHARACTER(LEN = 64)  ::  LFACT(0:NKPDS6)
   CHARACTER(LEN = 64)  ::  LBASE(0:NKPDS6)
   CHARACTER(LEN = 64)  ::  LTYPE(0:NKPDS6)
   CHARACTER(LEN = 64)  ::  LEVLR(0:NKPDS6)
   CHARACTER(LEN = 3 )  ::  LEVAB(0:NKPDS6)
   CHARACTER(LEN = 80)  ::  FNKPDS6
   CHARACTER(LEN =160)  ::  STRING
!
   INTEGER              ::  LENLI(0:NKPDS6),LENLU(0:NKPDS6),LENLF(0:NKPDS6),   &
                            LENLB(0:NKPDS6),LENLT(0:NKPDS6),LENLE(0:NKPDS6)
!
   COMMON                   /COMLVL/ LINFO,LUNIT,LFACT,LBASE,LTYPE,LEVLR,LEVAB,&
                            LENLI,LENLU,LENLF,LENLB,LENLT,LENLE
!
!  Open Level table (PDS6) file
!
   OPEN(LUKP6,FILE=FNKPDS6,STATUS='OLD',ERR=761)
   !
   GO TO 762
   !
   761 CONTINUE
   !
   WRITE(LUPTR,*) 'ERROR IN OPENING FILE ',FNKPDS6
   PRINT *,'ERROR IN OPENING FILE ',FNKPDS6
   CALL ABORT
   !
   762 CONTINUE
   !
   WRITE(LUPTR,*) 'FILE ',FNKPDS6(1:50),' opened.  Unit=',LUKP6
!
   N=1
   !
   300 CONTINUE
   !
   READ(LUKP6,100,END=200) STRING
   !
   100 FORMAT(A80)
   !
   IS=1
   NDELIM=0
   DO 500 I = 1,80
      IF(STRING(I:I).EQ.':') THEN
         NDELIM=NDELIM+1
         NCHAR=I-1-IS+1
         IF(NCHAR.GT.0) THEN
            IF(NDELIM.EQ.1) THEN
               PDS6(N-1)(1:NCHAR)=STRING(IS:I-1)
               READ(PDS6(N-1),110) M
               !
               110 FORMAT(I3)
               !
            ELSEIF(NDELIM.EQ.2) THEN
               JJ=0
               DO J = IS+1,I-1
                  IF(STRING(J-1:J-1).NE.' '.OR.STRING(J:J).NE.' ') THEN
                     JJ=JJ+1
                     LINFO(M)(JJ:JJ)=STRING(J-1:J-1)
                  ENDIF
               ENDDO            
               IF(STRING(I-1:I-1).NE.' ') THEN
                  JJ=JJ+1
                  LINFO(M)(JJ:JJ)=STRING(I-1:I-1)
               ENDIF
               LENLI(M)=JJ
            ELSEIF(NDELIM.EQ.3) THEN
               JJ=0
               DO J = IS,I-1
                  IF(STRING(J-1:J-1).NE.' ') THEN
                     JJ=JJ+1
                     LUNIT(M)(JJ:JJ)=STRING(J:J)
                  ENDIF
               ENDDO
               LENLU(M)=JJ
               IF(LUNIT(M)(1:1).EQ.'-') THEN
                  LUNIT(M)(1:1)=' '
                  LENLU(M)=1
               ENDIF
            ELSEIF(NDELIM.EQ.4) THEN
               JJ=0
               DO J = IS,I-1
                  IF(STRING(J-1:J-1).NE.' ') THEN
                     JJ=JJ+1
                     LFACT(M)(JJ:JJ)=STRING(J:J)
                  ENDIF
               ENDDO
               LENLF(M)=JJ
            ELSEIF(NDELIM.EQ.5) THEN
               JJ=0
               DO J = IS,I-1
                  IF(STRING(J-1:J-1).NE.' ') THEN
                     JJ=JJ+1
                     LBASE(M)(JJ:JJ)=STRING(J:J)
                  ENDIF
               ENDDO
               LENLB(M)=JJ
            ELSEIF(NDELIM.EQ.6) THEN
               JJ=0
               DO J = IS,I-1
                  IF(STRING(J:J).NE.' ') THEN
                     JJ=JJ+1
                     LTYPE(M)(JJ:JJ)=STRING(J:J)
                  ENDIF
               ENDDO
               LENLT(M)=JJ
            ELSEIF(NDELIM.EQ.7) THEN
               JJ=0
               DO J = IS,I-1
                  IF(STRING(J:J).NE.' ') THEN
                     JJ=JJ+1
                     LEVLR(M)(JJ:JJ)=STRING(J:J)
                  ENDIF
               ENDDO
               LENLE(M)=JJ
               JJ=0
!              DO J = I+1,80
               DO J = I+1,I+3
                  IF(STRING(J:J).NE.' ') THEN
                     JJ=JJ+1
                     LEVAB(M)(JJ:JJ)=STRING(J:J)
                  ENDIF
               ENDDO
               !
               GO TO 500
               !
            ENDIF
            IS=I+1
         ENDIF
      ENDIF
!
   500 CONTINUE
!
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
!
   N=N+1
   !
   GO TO 300
   !
   200 CONTINUE
   !
!
   CLOSE(UNIT=LUKP6)
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE GETPARM(KPDS,NGI,LABEL,LENLA,LABBR,LUPTR)
!
   INTEGER  ::  KPDS(22,*)
!
!     Transrates KPDS(5,*) parameter to variable names
!
   INTEGER, PARAMETER   ::  NKPDS5=255
   CHARACTER(LEN = 64)  ::  LABEL(3,*)
   CHARACTER(LEN = 8 )  ::  LABBR(*)
   CHARACTER(LEN = 64)  ::  PINFO(0:NKPDS5)
   CHARACTER(LEN = 64)  ::  PNAME(0:NKPDS5)
   CHARACTER(LEN = 64)  ::  PUNIT(0:NKPDS5)
   INTEGER              ::  LENLA(3,*)
   INTEGER              ::  LENPI(0:NKPDS5),LENPN(0:NKPDS5),LENPU(0:NKPDS5)
   INTEGER, SAVE        ::  NUNDEF
!
   DATA                     NUNDEF/0/
!
   LOGICAL LDEBUG
   COMMON                   /COMPRM/ PINFO,PUNIT,PNAME,LENPI,LENPU,LENPN
   COMMON                   /COMDBG/ LDEBUG
!
   DO N = 1,NGI
      K=KPDS(5,N)
!
      IF(K.GT.NKPDS5) THEN
        WRITE(LUPTR,*) ' FATAL ERROR in INDEX file! ',' KPDS(5,N)=',           &
                       KPDS(5,N), ' FOR N=',N
        WRITE(LUPTR,*) ' KPDS(5,N) CANNOT BE GREATER THAN ',NKPDS5
        PRINT *,' FATAL ERROR in INDEX file! ',' KPDS(5,N)=',                  &
                KPDS(5,N),' FOR N=',N
        PRINT *,' KPDS(5,N) CANNOT BE GREATER THAN ',NKPDS5
        CALL ABORT
      ENDIF
      IF(PNAME(K)(1:1).EQ.'?'.AND.PINFO(K)(1:1).EQ.'?'                         &
                             .AND. PUNIT(K)(1:1).EQ.'?') THEN
         WRITE(LUPTR,*) ' SERIOUS PROBLEM in INDEX file',' KPDS(5,N)=',        &
                        KPDS(5,N),' FOR N=',N
         WRITE(LUPTR,*) ' THE PARAMETER IS NOT DEFINED ',                      &
                        'IN THE KPDS(5) TABLE FILE '
         PRINT *,' Serious problem in INDEX file',                             &
                 ' KPDS(5,N)=',KPDS(5,N),' FOR N=',N
         PRINT *,' THE PARAMETER IS NOT DEFINED ',                             &
                 'IN THE KPDS(5) TABLE FILE '
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
         WRITE(LUPTR,*) 'NUNDEF=',NUNDEF,' PINFO(K)=',PINFO(K),                &
                        ' PNAME(K)=',PNAME(K),' PUNIT(K)=',PUNIT(K)
      ENDIF
!
      LABEL(1,N)=PINFO(K)(1:LENPI(K))//' ('//PUNIT(K)(1:LENPU(K))//')'
      LENLA(1,N)=LENPI(K)+2+LENPU(K)+1
!
!     WRITE(LUPTR,*) 'N=',N,' K=',K,' ',LABEL(1,N)(1:LENLA(1,N))
      LABBR(N)='        '
      LABBR(N)=PNAME(K)(1:LENPN(K))
   ENDDO
!
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE GETLEVL(KPDS,NGI,LABEL,LENLA,RLEV,LVABBR,LUPTR)
!
!     Transrates KPDS(6,*) parameter to level names
!
   INTEGER, PARAMETER     ::  NKPDS6 = 255
   CHARACTER(LEN = 64  )  ::  LABEL(3,*)
   CHARACTER(LEN = 8   )  ::  LVABBR(*)
   CHARACTER(LEN = 160 )  ::  FMT
   CHARACTER(LEN = 64  )  ::  LINFO(0:NKPDS6)
   CHARACTER(LEN = 64  )  ::  LUNIT(0:NKPDS6)
   CHARACTER(LEN = 64  )  ::  LFACT(0:NKPDS6)
   CHARACTER(LEN = 64  )  ::  LBASE(0:NKPDS6)
   CHARACTER(LEN = 64  )  ::  LTYPE(0:NKPDS6)
   CHARACTER(LEN = 64  )  ::  LEVLR(0:NKPDS6)
   CHARACTER(LEN = 3   )  ::  LEVAB(0:NKPDS6)
   CHARACTER(LEN = 64  )  ::  LTOPL,LBTML
   REAL                   ::  RLEV(*)
   INTEGER                ::  LENLA(3,*)
   INTEGER                ::  KPDS(22,*)
   INTEGER                ::  LENLI(0:NKPDS6),LENLU(0:NKPDS6),LENLF(0:NKPDS6), &
                              LENLB(0:NKPDS6),LENLT(0:NKPDS6),LENLE(0:NKPDS6)
   INTEGER, SAVE          ::  NUNDEF
!
   COMMON                     /COMLVL/ LINFO,LUNIT,LFACT,LBASE,LTYPE,LEVLR,    &
                              LEVAB,LENLI,LENLU,LENLF,LENLB,LENLT,LENLE
!
   DATA                       NUNDEF/0/
!
   DO 100 N = 1,NGI
      K=KPDS(6,N)
      IF(K.GT.NKPDS6.OR.K.LT.0) THEN
         WRITE(LUPTR,*) ' FATAL ERROR in INDEX FILE! ','KPDS(6,N)=',KPDS(6,N), &
                        ' FOR N=',N
         WRITE(LUPTR,*) ' KPDS(6,N) CANNOT BE GREATER THAN ',NKPDS6
         PRINT *,' FATAL ERROR in INDEX FILE! ','KPDS(6,N)=',KPDS(6,N),        &
                 ' FOR N=',N
         PRINT *,' KPDS(6,N) CANNOT BE GREATER THAN ',NKPDS6
         CALL ABORT
      ENDIF
      IF(LINFO(K)(1:1).EQ.'?'.AND.LUNIT(K)(1:1).EQ.'?'                         & 
                             .AND.LEVLR(K)(1:1).EQ.'?') THEN
         WRITE(LUPTR,*) ' Serious problem in index FILE! ',                    &
                        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
         WRITE(LUPTR,*) ' THE LEVEL IS NOT DEFINED ',                          &
                        'IN THE FILE KPDS(6) TABLE FILE.'
         PRINT *,' Serious problem in index FILE! ',                           &
                 'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
         PRINT *,' THE LEVEL IS NOT DEFINED ',                                 &
                 'IN THE FILE KPDS(6) TABLE FILE.'
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
         WRITE(LUPTR,*) ' ERROR in utilizing index FILE ! ',                   &
                        'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
         WRITE(LUPTR,*) ' Mixed precision level not programed',                &
                        'to work in this program.'
         PRINT *,' ERROR in utilizing index FILE ! ',                          &
                 'KPDS(6,N)=',KPDS(6,N),' FOR N=',N
         PRINT *,' Mixed precision level not programed',                       &
                 'to work in this program.'
         CALL ABORT
      ENDIF
!
      IF(LTYPE(K)(1:1).EQ.'?') THEN
         WRITE(LUPTR,*) ' FATAL ERROR! KPDS(6,N)=',KPDS(6,N),' FOR N=',N
         WRITE(LUPTR,*) ' LVTYP NOT SPECIFIED IN THE LEVEL subroutine'
         PRINT *,' FATAL ERROR! KPDS(6,N)=',KPDS(6,N),' FOR N=',N
         PRINT *,' LTYPE NOT SPECIFIED IN THE LEVEL subroutine'
         CALL ABORT
      ENDIF
!
      DO I = 1,LENLF(K)
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
!
      READ(LFACT(K),FMT) RFACT
      DO I = 1,LENLB(K)
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
!
      READ(LBASE(K),FMT) RBASE
!
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
         WRITE(LUPTR,*) ' FATAL ERROR! KPDS(6,N)=',KPDS(6,N),' FOR N=',N
         WRITE(LUPTR,*) ' ILLEGAL LEVEL TYPE ENCOUNTERED ',                    &
                        'IN THE FILE KPDS(6) TABLE FILE'
         WRITE(LUPTR,*) ' LEVLR(K)=',LEVLR(K)
         PRINT *,' FATAL ERROR! KPDS(6,N)=',KPDS(6,N),' FOR N=',N
         PRINT *,' ILLEGAL LEVEL TYPE ENCOUNTERED ',                           &
                 'IN THE FILE KPDS(6) TABLE FILE'
         PRINT *,' LEVLR(K)=',LEVLR(K)
         CALL ABORT
      ENDIF
!
      RLEV(N)=BTML
!
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
!
      LABEL(2,N)=LINFO(K)(1:LENLI(K))
      LENLA(2,N)=LENLI(K)
!
      IF(LEVLR(K)(1:5).EQ.'level'.OR.LEVLR(K)(1:6).EQ.'slevel') THEN
         IF(LTYPE(K)(1:7).EQ.'surface'.OR.LTYPE(K)(1:1).EQ.'-') THEN
            LABEL(3,N)=' '
            LENLA(3,N)=1
         ELSE
            LABEL(3,N)=LTOPL(1:LENT)//LUNIT(K)(1:LENLU(K))
            LENLA(3,N)=LENT+LENLU(K)
         ENDIF
      ELSEIF(LEVLR(K)(1:5).EQ.'layer') THEN
         LABEL(3,N)=LBTML(1:LENB)//LUNIT(K)(1:LENLU(K))//' and '//             &
                    LTOPL(1:LENT)//LUNIT(K)(1:LENLU(K))
         LENLA(3,N)=LENB+LENLU(K)+5+LENT+LENLU(K)
      ENDIF
!
      LVABBR(N)=LEVAB(K)
!
   100 CONTINUE
!
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE RMBLNK(LABEL,LENLA,N)
!
!  Get rid of two or more consecutive blanks here
!
   CHARACTER(LEN = 64 )  ::  LABEL(N)
   CHARACTER(LEN = 160)  ::  LABEX
   INTEGER               ::  LENLA(N)
!
   DO II = 1,N
      JJ=0
      DO J = 2,LENLA(II)
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
!
      IF(LABEL(II)(LENLA(II):LENLA(II)).NE.' ') THEN
         JJ=JJ+1
         LABEX(JJ:JJ)=LABEL(II)(LENLA(II):LENLA(II))
      ENDIF
!
      IF(JJ.LE.0) THEN
         LENLA(II)=1
         LABEL(II)(1:1)=' '
      ELSE
         LENLA(II)=JJ
         LABEL(II)(1:LENLA(II))=LABEX(1:LENLA(II))
      ENDIF
!
   ENDDO
!
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE FLSRCH(NTOTL,LUPTR)
!
   INTEGER, PARAMETER   ::  MTOTL = 160
   CHARACTER(LEN = 64)  ::  LABEL
   CHARACTER(LEN = 8 )  ::  LABBR
   CHARACTER(LEN = 8 )  ::  LVABBR
   CHARACTER(LEN = 64)  ::  LABTMP
   LOGICAL                  LLEVL,LLTYP
   LOGICAL                  LFP(MTOTL),LPFND
   LOGICAL                  LDEBUG
   COMMON                   /COMCTL/RLEV(MTOTL),LABEL(3,MTOTL),LENLA(3,MTOTL), &
                                    LABBR(MTOTL),LVABBR(MTOTL),                &
                                    ILINDX(MTOTL),IVINDX(MTOTL),IPINDX(MTOTL), &
                                    NLTOTL(MTOTL),NVTOTL,NPTOTL,LLEVL,LLTYP
   COMMON                   /COMDBG/ LDEBUG
!
!  Modify variable name if same variable name but with differenct level types
!
   DO N = 1,NTOTL
      LFP(N)=.FALSE.
   ENDDO
   DO N = 1,NTOTL
      IF(.NOT.LFP(N)) THEN
         DO M = N+1,NTOTL
            IF(.NOT.LFP(M)) THEN
               IF(LABBR(N).EQ.LABBR(M).AND.LABEL(2,N)(1:LENLA(2,N)).NE.        &
                                           LABEL(2,M)(1:LENLA(2,M))) THEN
                  J=0
                  DO I = 1,5
                     IF(LABBR(M)(I:I).NE.' ') THEN
                        J=J+1
                        LABTMP(J:J)=LABBR(M)(I:I)
                     ENDIF
                  ENDDO
                  LABBR(M)(1:J+3)=LABTMP(1:J)//LVABBR(M)(1:3)
                  DO I = J+4,8
                     LABBR(M)(I:I)=' '
                  ENDDO
                  WRITE(LUPTR,*) 'Variable "',LABTMP(1:J),'" on ',             &
                                  LABEL(2,M)(1:LENLA(2,M)),' modified to "',   &
                                  LABBR(M)(1:J+3),'"'
                  LFP(M)=.TRUE.
               ENDIF
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!
!  Find number of variables 
!
   DO N = 1,NTOTL
      LFP(N)=.FALSE.
   ENDDO
   NVTOTL=0
   NN=1
   NV=1
   DO N = 1,NTOTL
      IF(.NOT.LFP(N)) THEN
         NVTOTL=NVTOTL+1
         IVINDX(NVTOTL)=N
         LFP(N)=.TRUE.
         DO M = N+1,NTOTL
            IF(.NOT.LFP(M).AND.LABBR(N).EQ.LABBR(M)) THEN
               LFP(M)=.TRUE.
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!
   IF(LDEBUG) THEN
      WRITE(LUPTR,*) ' '
      WRITE(LUPTR,*) ' NVTOTL=',NVTOTL
      DO K = 1,NVTOTL
         WRITE(LUPTR,*) LABBR(IVINDX(K))
      ENDDO
      WRITE(LUPTR,*)' IVINDX=',IVINDX
   ENDIF
!
!  Find number of levels for each variable
!
   LLEVL=.FALSE.
   DO N = 1,NTOTL
      LFP(N)=.FALSE.
   ENDDO
   NNN=0
   DO NV = 1,NVTOTL
      NVL=IVINDX(NV)
      NLTOTL(NV)=0
      DO N = 1,NTOTL
         IF(.NOT.LFP(N).AND.LABEL(1,N)(1:LENLA(1,N)).EQ.                       &
                        LABEL(1,NVL)(1:LENLA(1,NVL)).AND.                      &
                        LABBR(N).EQ.LABBR(NVL)) THEN
            NNN=NNN+1
            NLTOTL(NV)=NLTOTL(NV)+1
            ILINDX(NNN)=N
            LFP(N)=.TRUE.
            DO M = N+1,NTOTL
               IF(.NOT.LFP(M).AND.LABEL(1,M)(1:LENLA(1,M)).EQ.                 &
                              LABEL(1,NVL)(1:LENLA(1,NVL)).AND.                & 
                                    LABBR(M).EQ.LABBR(NVL).AND.                &
                                  LABEL(3,M)(1:LENLA(3,M)).EQ.                 &
                                 LABEL(3,N)(1:LENLA(3,N))) THEN
                  LFP(M)=.TRUE.
               ENDIF
            ENDDO
         ENDIF
      ENDDO
   ENDDO
!
   IF(LDEBUG) THEN
      WRITE(LUPTR,*) ' '
      WRITE(LUPTR,*) ' NLTOTL(NV)=',(NLTOTL(NV),NV=1,NVTOTL)
      NNN=0
      DO NV = 1,NVTOTL
         WRITE(LUPTR,*) (LABEL(3,ILINDX(K+NNN))                                &
                      (1:LENLA(3,ILINDX(K+NNN))),',',K=1,NLTOTL(NV))
         NNN=NNN+NLTOTL(NV)
      ENDDO
   ENDIF
   LLEVL=.FALSE.
   DO NV = 1,NVTOTL
      IF(NLTOTL(NV).GT.1) THEN
         LLEVL=.TRUE.
         !
         GO TO 101
         !
      ENDIF
   ENDDO
   !
   101 CONTINUE
   !
!
!  Find number of multi-level level types
!
   DO N = 1,NTOTL
      LFP(N)=.FALSE.
   ENDDO
   NPTOTL=0
   NN=0 
   DO N = 1,NTOTL
      IF(.NOT.LFP(N).AND.LENLA(3,N).GT.1) THEN
         NPTOTL=NPTOTL+1
         LFP(N)=.TRUE.
         IPINDX(NPTOTL)=N
         DO M = N+1,NTOTL
            IF(.NOT.LFP(M).AND.LABEL(2,N)(1:LENLA(2,N)).EQ.                    &
                               LABEL(2,M)(1:LENLA(2,M))) THEN
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
!
   IF(LDEBUG) THEN
      WRITE(LUPTR,*) ' '
      WRITE(LUPTR,*) ' NPTOTL=',NPTOTL
      DO K = 1,NPTOTL
         WRITE(LUPTR,*) LABEL(2,IPINDX(K))(1:LENLA(2,IPINDX(K)))
      ENDDO
   ENDIF
!
   WRITE(LUPTR,*) ' '
   IF(LLEVL) THEN
      WRITE(LUPTR,*) ' Multi-level fields found in a given file'
      WRITE(LUPTR,*) ' Number of multi-level fields=',NLTOTL
   ENDIF
   IF(LLTYP) THEN
      WRITE(LUPTR,*) ' Same variable with different level type found',         &
                     ' in a given file'
      WRITE(LUPTR,*) ' Number of such fields=',NPTOTL
   ENDIF
!
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE WRYTE(LU,LC,C)
!
!   $$$  SUBPROGRAM DOCUMENTATION BLOCK
!
!    SUBPROGRAM:    WRYTE       WRITE DATA OUT BY BYTES
!      PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
!    ABSTRACT: EFFICIENTLY WRITE UNFORMATTED A CHARACETER ARRAY.
!
!    PROGRAM HISTORY LOG:
!      91-10-31  MARK IREDELL
!
!    USAGE:    CALL WRYTE(LU,LC,C)
!
!      INPUT ARGUMENT LIST:
!        LU       - INTEGER UNIT TO WHICH TO WRITE
!        LC       - INTEGER NUMBER OF CHARACTERS OR BYTES TO WRITE
!        C        - CHARACETER (LC) DATA TO WRITE
!
!    ATTRIBUTES:
!      LANGUAGE: CRAY FORTRAN
!
!   $$$
   CHARACTER  ::  C(LC)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   WRITE(LU) C
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   RETURN
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE PUTGB(LUGB,KF,KPDS,KGDS,LB,F,IBM,FR,GRIB,LGRIB,LUPTR,IRET)
!
!   $$$  SUBPROGRAM DOCUMENTATION BLOCK

!    SUBPROGRAM: PUTGB          PACKS AND WRITES A GRIB MESSAGE
!      PRGMMR: IREDELL          ORG: W/NMC23     DATE: 94-04-01
!
!    ABSTRACT: PACK AND WRITE A GRIB MESSAGE.
!      THIS SUBPROGRAM IS NEARLY THE INVERSE OF GETGB.
!
!    PROGRAM HISTORY LOG:
!      94-04-01  IREDELL
!
!    USAGE:    CALL PUTGB(LUGB,KF,KPDS,KGDS,LB,F,IBM,FR,GRIB,IRET)
!      INPUT ARGUMENTS:
!        LUGB         INTEGER UNIT OF THE UNBLOCKED GRIB DATA FILE
!        KF           INTEGER NUMBER OF DATA POINTS
!        KPDS         INTEGER (25) PDS PARAMETERS
!        KGDS         INTEGER (22) GDS PARAMETERS
!        LB           LOGICAL (KF) BITMAP IF PRESENT
!        F            REAL (KF) DATA
!      OUTPUT ARGUMENTS:
!        IRET         INTEGER RETURN CODE
!                       0      ALL OK
!                       OTHER  W3FI72 GRIB PACKER RETURN CODE
!
!    SUBPROGRAMS CALLED:
!      R63W72         MAP W3FI63 PARAMETERS ONTO W3FI72 PARAMETERS
!      GTBITS         GET NUMBER OF BITS AND ROUND DATA
!      W3FI72         PACK GRIB
!      WRYTE          WRITE DATA
!
!    ATTRIBUTES:
!      LANGUAGE: F77 FORTRAN
!
!   $$$
   CHARACTER           ::  PDS(28)
   REAL                ::  F(KF)
   INTEGER             ::  KPDS(25),KGDS(22)
   INTEGER             ::  IBM(KF),IPDS(25),IGDS(18),IBDS(9)
   LOGICAL*1               LB(KF)
!
!  CHARACTER GRIB(200+17*KF/8)
   CHARACTER(LEN = 1)  ::  GRIB(*)
!
   REAL                ::  FR(KF),FMAX,FMIN
!-------------------------------------------------------------------------------
!  GET W3FI72 PARAMETERS
!
   CALL R63W72(KPDS,KGDS,IPDS,IGDS)
   DO I = 1,9
      IBDS(I)=0
   ENDDO
!-------------------------------------------------------------------------------
!  COUNT VALID DATA
!
   KBM=KF
!  WRITE(LUPTR,*) 'IPDS(7)=',IPDS(7)
   IF(IPDS(7).NE.0) THEN
      KBM=0
      DO I = 1,KF
         IF(LB(I)) THEN
            IBM(I)=1
            KBM=KBM+1
         ELSE
            IBM(I)=0
         ENDIF
      ENDDO
      IF(KBM.EQ.KF) IPDS(7)=0
   ENDIF
!  WRITE(LUPTR,*) 'MODIFIED IPDS(7)=',IPDS(7)
!
!-------------------------------------------------------------------------------
!  GET NUMBER OF BITS AND ROUND DATA
!
   IF(KBM.EQ.0) THEN
      DO I = 1,KF
         FR(I)=0.
      ENDDO
      NBIT=0
    ELSE
      CALL GTBITS(IPDS(7),IPDS(25),KF,IBM,F,FR,FMIN,FMAX,NBIT)
      NBIT=MIN(NBIT,16)
    ENDIF
!-------------------------------------------------------------------------------
!  PACK AND WRITE GRIB DATA
!
!  FR IS REAL*8
!
   CALL W3FI68(IPDS,PDS)
!
   WRITE(LUPTR,*) 'IPDS(6)=',IPDS(6),' IPDS(7)=',IPDS(7)
   WRITE(LUPTR,*) 'NBIT=',NBIT,' KBM=',KBM
!
!  CALL W3FI72(0,FR,0,NBIT,0,IPDS,PDS,
   CALL W3FI72(0,FR,0,NBIT,1,IPDS,PDS,1,255,IGDS,0,0,IBM,KF,IBDS,              &
               KFO,GRIB,LGRIB,IRET)
   IF(IRET.EQ.0) CALL WRYTE(LUGB,LGRIB,GRIB)
!-------------------------------------------------------------------------------
   RETURN
   END
!
!-------------------------------------------------------------------------------
!FPP$ NOCONCUR R
   SUBROUTINE GTBITS(IBM,IDS,LEN,MG,G,GROUND,GMIN,GMAX,NBIT)
!
!   $$$  SUBPROGRAM DOCUMENTATION BLOCK
!
!    SUBPROGRAM:    GTBITS      COMPUTE NUMBER OF BITS AND ROUND FIELD.
!   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31
!
!    ABSTRACT: THE NUMBER OF BITS REQUIRED TO PACK A GIVEN FIELD
!      AT A PARTICULAR DECIMAL SCALING IS COMPUTED USING THE FIELD RANGE.
!      THE FIELD IS ROUNDED OFF TO THE DECIMAL SCALING FOR PACKING.
!      THE MINIMUM AND MAXIMUM ROUNDED FIELD VALUES ARE ALSO RETURNED.
!      GRIB BITMAP MASKING FOR VALID DATA IS OPTIONALLY USED.
!
!    PROGRAM HISTORY LOG:
!      92-10-31  IREDELL
!
!    USAGE:    CALL GTBITS(IBM,IDS,LEN,MG,G,GMIN,GMAX,NBIT)
!      INPUT ARGUMENT LIST:
!        IBM      - INTEGER BITMAP FLAG (=0 FOR NO BITMAP)
!        IDS      - INTEGER DECIMAL SCALING
!                   (E.G. IDS=3 TO ROUND FIELD TO NEAREST MILLI-VALUE)
!        LEN      - INTEGER LENGTH OF THE FIELD AND BITMAP
!        MG       - INTEGER (LEN) BITMAP IF IBM=1 (0 TO SKIP, 1 TO KEEP)
!        G        - REAL (LEN) FIELD
!
!      OUTPUT ARGUMENT LIST:
!        GROUND   - REAL*8 (LEN) FIELD ROUNDED TO DECIMAL SCALING
!        GMIN     - REAL*8 MINIMUM VALID ROUNDED FIELD VALUE
!        GMAX     - REAL*8 MAXIMUM VALID ROUNDED FIELD VALUE
!        NBIT     - INTEGER NUMBER OF BITS TO PACK
!
!    SUBPROGRAMS CALLED:
!      ISRCHNE  - FIND FIRST VALUE IN AN ARRAY NOT EQUAL TO TARGET VALUE
!
!    ATTRIBUTES:
!      LANGUAGE: CRAY FORTRAN
!
!   $$$
   REAL     ::  GROUND(LEN),GMAX,GMIN
   INTEGER  ::  MG(LEN),G(LEN)
!-------------------------------------------------------------------------------
!  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
!
   DS=10.**IDS
   IF(IBM.EQ.0) THEN
      GROUND(1)=NINT(G(1)*DS)/DS
      GMAX=GROUND(1)
      GMIN=GROUND(1)
      DO I = 2,LEN
         GROUND(I)=NINT(G(I)*DS)/DS
         GMAX=MAX(GMAX,GROUND(I))
         GMIN=MIN(GMIN,GROUND(I))
      ENDDO
   ELSE
      I1=ISRCHNE(LEN,MG,1,0)
      IF(I1.GT.0.AND.I1.LE.LEN) THEN
         GROUND(I1)=NINT(G(I1)*DS)/DS
         GMAX=GROUND(I1)
         GMIN=GROUND(I1)
         DO I = 1,I1-1
            GROUND(I)=0.
         ENDDO
         DO I = I1+1,LEN
            IF(MG(I).NE.0) THEN
               GROUND(I)=NINT(G(I)*DS)/DS
               GMAX=MAX(GMAX,GROUND(I))
               GMIN=MIN(GMIN,GROUND(I))
            ELSE
               GROUND(I)=0.
            ENDIF
         ENDDO
      ELSE
         GMAX=0.
         GMIN=0.
      ENDIF
   ENDIF
!-------------------------------------------------------------------------------
!  COMPUTE NUMBER OF BITS
   NBIT=LOG((GMAX-GMIN)*DS+0.9)/LOG(2.)+1.
!-------------------------------------------------------------------------------
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE INCDTE(IY,IM,ID,IH,JY,JM,JD,JH,INCHR)
!
!  Compute JY,JM,JD,JH of forecast
!
   JY=IY
   JM=IM
   JD=ID
   INCDY=INCHR/24
   JH=IH+MOD(INCHR,24)
   INCDY=INCDY+JH/24
   JH=MOD(JH,24)
   DO INCD = 1,INCDY
      JD=JD+1
      IF(JM.EQ.4.OR.JM.EQ.6.OR.JM.EQ.9.OR.JM.EQ.11) THEN
         MONDY=30
      ELSEIF(JM.EQ.2) THEN
         IF(MOD(JY,4).EQ.0) THEN
            MODNY=29
         ELSE
            MONDY=28
         ENDIF
      ELSE
         MONDY=31
      ENDIF
      IF(JD.GT.MONDY) THEN
         JM=JM+1
         JD=1
         IF(JM.GT.12) THEN
            JY=JY+1
            JM=1
         ENDIF
      ENDIF
   ENDDO
!
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE MAXMIN(F,IDIM,JDIM,IMAX,JMAX,KMAX,LUPTR)
!
   REAL  ::  F(IDIM,JDIM,KMAX)
!
   DO 10 K = 1,KMAX
!
      FMAX=F(1,1,K)
      FMIN=F(1,1,K)
!
      DO 20 J = 1,JMAX
         DO 20 I = 1,IMAX
            IF(FMAX.LE.F(I,J,K)) THEN
               FMAX=F(I,J,K)
               IIMAX=I
               JJMAX=J
            ENDIF
            IF(FMIN.GE.F(I,J,K)) THEN
               FMIN=F(I,J,K)
               IIMIN=I
               JJMIN=J
            ENDIF
      20 CONTINUE
!
      WRITE(LUPTR,100) K,FMAX,IIMAX,JJMAX,FMIN,IIMIN,JJMIN
      !
      100 FORMAT(2X,'LEVEL=',I2,' MAX=',E10.4,' AT I=',I5,' J=',I5,            &
                                ' MIN=',E10.4,' AT I=',I5,' J=',I5)
      !
!
   10 CONTINUE
!
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   FUNCTION ISRCHNE(N,X,INCX,TARGET)
!
   INTEGER  ::  X(*), TARGET
   J=1
   ISRCHNE=0
   IF(N.LE.0) RETURN
   IF(INCX.LT.0) J=1-(N-1)*INCX
   DO I = 1,N
      IF(X(J).NE.TARGET) THEN
         ISRCHNE=I
         RETURN
      ENDIF
   J=J+INCX
   ENDDO
!
   RETURN
!
   END
!
!-------------------------------------------------------------------------------
!
   SUBROUTINE R63W72(KPDS,KGDS,IPDS,IGDS)
!
!   $$$  SUBPROGRAM DOCUMENTATION BLOCK
!
!    SUBPROGRAM:    R63W72      CONVERT W3FI63 PARMS TO W3FI72 PARMS
!      PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
!    ABSTRACT: DETERMINES THE INTEGER PDS AND GDS PARAMETERS
!              FOR THE GRIB1 PACKING ROUTINE W3FI72 GIVEN THE PARAMETERS
!              RETURNED FROM THE GRIB1 UNPACKING ROUTINE W3FI63.
!
!    PROGRAM HISTORY LOG:
!      91-10-31  MARK IREDELL
!      96-05-03  MARK IREDELL  CORRECTED SOME LEVEL TYPES AND
!                              SOME DATA REPRESENTATION TYPES
!      97-02-14  MARK IREDELL  ONLY ALTERED IPDS(26:27) FOR EXTENDED PDS
!      98-06-01  CHRIS CARUSO  Y2K FIX FOR YEAR OF CENTURY
!
!    USAGE:    CALL R63W72(KPDS,KGDS,IPDS,IGDS)
!
!      INPUT ARGUMENT LIST:
!        KPDS     - INTEGER (200) PDS PARAMETERS FROM W3FI63
!        KGDS     - INTEGER (200) GDS PARAMETERS FROM W3FI63
!
!      OUTPUT ARGUMENT LIST:
!        IPDS     - INTEGER (200) PDS PARAMETERS FOR W3FI72
!        IGDS     - INTEGER (200) GDS PARAMETERS FOR W3FI72
!
!    REMARKS: KGDS AND IGDS EXTEND BEYOND THEIR DIMENSIONS HERE
!             IF PL PARAMETERS ARE PRESENT.
!
!    ATTRIBUTES:
!      LANGUAGE: CRAY FORTRAN
!
!   $$$
   INTEGER  ::  KPDS(200),KGDS(200),IPDS(200),IGDS(200)
!-------------------------------------------------------------------------------
!  DETERMINE PRODUCT DEFINITION SECTION (PDS) PARAMETERS
!
   IF(KPDS(23).NE.2) THEN
      IPDS(1)=28                            ! LENGTH OF PDS
   ELSE
      IPDS(1)=45                            ! LENGTH OF PDS
   ENDIF
   IPDS(2)=KPDS(19)                         ! PARAMETER TABLE VERSION
   IPDS(3)=KPDS(1)                          ! ORIGINATING CENTER
   IPDS(4)=KPDS(2)                          ! GENERATING MODEL
   IPDS(5)=KPDS(3)                          ! GRID DEFINITION
   IPDS(6)=MOD(KPDS(4)/128,2)               ! GDS FLAG
   IPDS(7)=MOD(KPDS(4)/64,2)                ! BMS FLAG
   IPDS(8)=KPDS(5)                          ! PARAMETER INDICATOR
   IPDS(9)=KPDS(6)                          ! LEVEL TYPE
   IF(KPDS(6).EQ.101.OR.KPDS(6).EQ.104.OR.KPDS(6).EQ.106.OR.                   &
      KPDS(6).EQ.108.OR.KPDS(6).EQ.110.OR.KPDS(6).EQ.112.OR.                   &    
      KPDS(6).EQ.114.OR.KPDS(6).EQ.116.OR.KPDS(6).EQ.121.OR.                   &
      KPDS(6).EQ.128.OR.KPDS(6).EQ.141)  THEN
!
      IPDS(10)=MOD(KPDS(7)/256,256)         ! LEVEL VALUE 1
      IPDS(11)=MOD(KPDS(7),256)             ! LEVEL VALUE 2
   ELSE
      IPDS(10)=0                            ! LEVEL VALUE 1
      IPDS(11)=KPDS(7)                      ! LEVEL VALUE 2
   ENDIF
   IPDS(12)=KPDS(8)                         ! YEAR OF CENTURY
   IPDS(13)=KPDS(9)                         ! MONTH
   IPDS(14)=KPDS(10)                        ! DAY
   IPDS(15)=KPDS(11)                        ! HOUR
   IPDS(16)=KPDS(12)                        ! MINUTE
   IPDS(17)=KPDS(13)                        ! FORECAST TIME UNIT
   IPDS(18)=KPDS(14)                        ! TIME RANGE 1
   IPDS(19)=KPDS(15)                        ! TIME RANGE 2
   IPDS(20)=KPDS(16)                        ! TIME RANGE INDICATOR
   IPDS(21)=KPDS(17)                        ! NUMBER IN AVERAGE
   IPDS(22)=KPDS(20)                        ! NUMBER MISSING IN AVERAGE
   IPDS(23)=KPDS(21)                        ! CENTURY
   IPDS(24)=KPDS(23)                        ! SUBCENTER
   IPDS(25)=KPDS(22)                        ! DECIMAL SCALING
   IF(IPDS(1).GT.28) THEN
      IPDS(26)=0                            ! PDS BYTE 29
      IPDS(27)=0                            ! PDS BYTE 30
   ENDIF
!-------------------------------------------------------------------------------
!  DETERMINE GRID DEFINITION SECTION (GDS) PARAMETERS
!
   IGDS(1)=KGDS(19)                         ! NUMBER OF VERTICAL COORDINATES
   IGDS(2)=KGDS(20)                         ! VERTICAL COORDINATES
   IGDS(3)=KGDS(1)                          ! DATA REPRESENTATION
   IGDS(4)=KGDS(2)                          ! (UNIQUE TO REPRESENTATION)
   IGDS(5)=KGDS(3)                          ! (UNIQUE TO REPRESENTATION)
   IGDS(6)=KGDS(4)                          ! (UNIQUE TO REPRESENTATION)
   IGDS(7)=KGDS(5)                          ! (UNIQUE TO REPRESENTATION)
   IGDS(8)=KGDS(6)                          ! (UNIQUE TO REPRESENTATION)
   IGDS(9)=KGDS(7)                          ! (UNIQUE TO REPRESENTATION)
   IGDS(10)=KGDS(8)                         ! (UNIQUE TO REPRESENTATION)
   IGDS(11)=KGDS(9)                         ! (UNIQUE TO REPRESENTATION)
   IGDS(12)=KGDS(10)                        ! (UNIQUE TO REPRESENTATION)
   IGDS(13)=KGDS(11)                        ! (UNIQUE TO REPRESENTATION)
   IGDS(14)=KGDS(12)                        ! (UNIQUE TO REPRESENTATION)
   IGDS(15)=KGDS(13)                        ! (UNIQUE TO REPRESENTATION)
   IGDS(16)=KGDS(14)                        ! (UNIQUE TO REPRESENTATION)
   IGDS(17)=KGDS(15)                        ! (UNIQUE TO REPRESENTATION)
   IGDS(18)=KGDS(16)                        ! (UNIQUE TO REPRESENTATION)
!
!  EXCEPTIONS FOR LATLON OR GAUSSIAN
!
   IF(KGDS(1).EQ.0.OR.KGDS(1).EQ.4) THEN
      IGDS(11)=KGDS(10)
      IGDS(12)=KGDS(9)
!
!  EXCEPTIONS FOR MERCATOR
!
   ELSEIF(KGDS(1).EQ.1) THEN
      IGDS(11)=KGDS(13)
      IGDS(12)=KGDS(12)
      IGDS(13)=KGDS(9)
      IGDS(14)=KGDS(11)
!
!  EXCEPTIONS FOR LAMBERT CONFORMAL
!
   ELSEIF(KGDS(1).EQ.3) THEN
      IGDS(15)=KGDS(12)
      IGDS(16)=KGDS(13)
      IGDS(17)=KGDS(14)
      IGDS(18)=KGDS(15)
   ENDIF
!
!  EXTENSION FOR PL PARAMETERS
!
   IF(KGDS(1).EQ.0.AND.KGDS(19).EQ.0.AND.KGDS(20).NE.255) THEN
      DO J = 1,KGDS(3)
         IGDS(18+J)=KGDS(21+J)
      ENDDO
   ENDIF
!-------------------------------------------------------------------------------
   RETURN
!
   END
