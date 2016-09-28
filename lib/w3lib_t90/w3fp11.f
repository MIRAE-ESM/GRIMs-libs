      SUBROUTINE W3FP11 (IPDS0, IPDS, TITL, IERR)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  W3FP11        ONE-LINE GRIB TITLER FROM PDS SECTION
C   PRGMMR: MCCLEES          ORG: NMC421      DATE:88-02-02
C
C ABSTRACT: CONVERTS GRIB FORMATTED PRODUCT DEFINITION SECTION VERSION 
C   1 TO A ONE LINE READABLE TITLE.  GRIB SECTION 0 IS ALSO TESTED TO
C   VERIFY THAT GRIB DATA IS BEING DECIPHERED.
C
C PROGRAM HISTORY LOG:
C   91-06-19  R.E.JONES
C   92-05-29  R.E.JONES   ADD WATER TEMP TO TABLES  
C   93-01-19  R.E.JONES   ADD MONTGOMARY STREAM FUNCTION TO TABLES 
C                         ADD CODE FOR SURFACE VALUE 113.
C                         ADD CONDENSATION PRESSURE TO TABLES
C   93-02-19  R.E.JONES   ADD CAPE AND TKE (157 & 158) TO TABLES
C   93-02-24  R.E.JONES   ADD GRIB TYPE PMSLE (130) TO TABLES
C   93-03-26  R.E.JONES   ADD GRIB TYPE SGLYR (175) TO TABLES
C   93-03-27  R.E.JONES   CHANGES FOR REVISED O.N.388 MAR. 3,1993
C   93-03-29  R.E.JONES   ADD SAVE STATEMENT
C   93-04-16  R.E.JONES   ADD GRIB TYPE LAT, LON (176,177) TO TABLES
C   93-04-25  R.E.JONES   ADD GRIB TYPE 204, 205, 211, 212, 218
C   93-05-18  R.E.JONES   ADD TEST FOR MODEL 70
C   93-06-26  R.E.JONES   ADD GRIB TYPE 128, 129, TAKE OUT TEST FOR
C                         MODEL 86.
C   93-08-07  R.E.JONES   ADD GRIB TYPE 156 (CIN), 150 (CBMZW),
C                         151 (CBTZW), 152 (CBTMW) TO TABLES.
C   93-10-14  R.E.JONES   CHANGE FOR O.N. 388 REV. OCT. 8,1993
C   93-10-29  R.E.JONES   CHANGE FOR 'L CDC' 'M CDC' 'H CDC'
C   93-10-14  R.E.JONES   CHANGE FOR O.N. 388 REV. NOV. 19,1993
C   94-02-05  R.E.JONES   CHANGE FOR O.N. 388 REV. DEC. 14,1993
C                         ADD MODEL NUMBER 86 AND 87.
C   94-03-24  R.E.JONES   ADD GRIB TYPE 24 (TOTO3), 206 (UVPI)
C   94-06-04  R.E.JONES   CHANGE UVPI TO UVI
C   94-06-16  R.E.JONES   ADD GRIB TYPE 144,145,146,147,148,149
C                         SOILW,PEVPR,CWORK,U-GWD,V-GWD,PV TO TABLES.
C   94-06-22  R.E.JONES   ADD NCAR (60) TO CENTERS
C   94-07-25  R.E.JONES   CORRECTION FOR 71, 72, 213 (T CDC), (CDCON),
C                         (CDLYR)
C   94-10-27  R.E.JONES   ADD GRIB TYPE 191 (PROB), 192 (PROBN), ADD
C                         TEST FOR MODEL 90, 91, 92, 93, ADD SUB
C                         CENTER 2.
C   95-02-09  R.E.JONES   CORRECTION FOR CENTURY FOR FNOC
C   95-04-11  R.E.JONES   CORRECTION FOR LMH AND LMV
C   95-06-20  R.E.JONES   ADD GRIB TYPE 189 (VSTM), 190 (HLCY), 193
C                         (POP), 194 (CPOFP), 195 (CPOZP), 196
C                         (USTM), 197 (VSTM) TO TABLES.
C
C USAGE:    CALL W3FP11  (IPDS0,  IPDS,  TITL, IERR )
C   INPUT ARGUMENT LIST:
C     IPDS0    - GRIB SECTION 0 READ AS CHARACTER*8
C     IPDS     - GRIB PDS SECTION READ AS CHARACTER*28
C
C   OUTPUT ARGUMENT LIST:   
C     TITL     - CHARACTER*86 OUTPUT PRINT LINE
C     IERR   0 - COMPLETED SATISFACTORILY
C            1 - GRIB SECTION 0, CAN NOT FIND 'GRIB'
C            2 - GRIB IS NOT VERSION 1
C            3 - LENGTH OF PDS SECTION IS LESS THAN 28
C            4 - COULD NOT MATCH TYPE INDICATOR
C            5 - COULD NOT MATCH TYPE LEVEL
C            6 - COULD NOT INTERPRET ORIGINATOR OF CODE
C            7 - COULD NOT INTERPRET SUB CENTER 7 ORIGINATOR OF CODE
C            8 - COULD NOT INTERPRET SUB CENTER 9 ORIGINATOR OF CODE
C            9 - PARAMETER TABLE VERSION NOT 1 OR 2
C
C ATTRIBUTES:
C   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
C   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO, Indy
C   LANGUAGE: IBM VS FORTRAN, CRAY CFT77 FORTRAN
C   MACHINE:  HDS, CRAY C916-128, Y-MP8/864, Y-MP EL92/256
C
C$$$
C
        INTEGER         CENTER(15)
        INTEGER         SCNTR1(2)
        INTEGER         SCNTR2(14)
        INTEGER         FCSTIM
        INTEGER         HH(224)
        INTEGER         HH1(105)
        INTEGER         HH2(105)
        INTEGER         HH3(14)
        INTEGER         HHH(42)
        INTEGER         IERR
        INTEGER         P1
        INTEGER         P2
        INTEGER         TIMERG
C
        CHARACTER * 6   HHNAM(224)
        CHARACTER * 6   HHNAM1(105)
        CHARACTER * 6   HHNAM2(105)
        CHARACTER * 6   HHNAM3(14)
        CHARACTER * 4   HHHNAM(42)
        CHARACTER * (*) IPDS 
        CHARACTER * 8   IPDS0
        CHARACTER * 28  IDPDS
        CHARACTER * 4   GRIB
        CHARACTER * 28  KNAM1(15)
        CHARACTER * 28  KNAM2(2)
        CHARACTER * 28  KNAM3(14)
        CHARACTER * 3   MONTH(12)
        CHARACTER * 4   TIMUN(4)
        CHARACTER * 2   TIMUN1(4)
        CHARACTER * 86  TITL
C
        EQUIVALENCE     (HH(1),HH1(1))
        EQUIVALENCE     (HH(106),HH2(1))
        EQUIVALENCE     (HH(211),HH3(1))
        EQUIVALENCE     (HHNAM(1),HHNAM1(1))
        EQUIVALENCE     (HHNAM(106),HHNAM2(1))
        EQUIVALENCE     (HHNAM(211),HHNAM3(1))
C
        SAVE
C
        DATA  CENTER/  7,   8,   9,  34,  52,  54,  57, 
     &                58,  59,  60,  74,  85,  97,  98, 
     &                99/
        DATA  HH1  /
     &                 1,   2,   3,   6,   7,   8,   9,  
     &                11,  12,  13,  14,  15,  16,  17,  
     &                18,  19,  20,  21,  22,  23,  24,
     &                25,  26,  27,  28,  29,  30,  31,
     &                32,  33,  34,  35,  36,  37,  38, 
     &                39,  40,  41,  42,  43,  44,  45,
     &                46,  47,  48,  49,  50,  51,  52,
     &                53,  54,  55,  56,  57,  58,  59, 
     &                60,  61,  62,  63,  64,  65,  66,
     &                67,  68,  69,  70,  71,  72,  73,
     &                74,  75,  76,  78,  79,  80,  81,
     &                82,  83,  84,  85,  86,  87,  88,
     &                89,  90,  91,  92,  93,  94,  95,
     &                96,  97,  98,  99, 100, 101, 102, 
     &               103, 104, 105, 106, 107, 108, 109/
        DATA  HH2  /
     &               110, 111, 112, 113, 114, 115, 116, 
     &               117, 121, 122, 123, 124, 125, 126,  
     &               127, 128, 129, 130, 131, 132, 133, 
     &               134, 135, 136, 137, 138, 139, 140,
     &               141, 142, 143, 144, 145, 146, 147, 
     &               148, 149, 150, 151, 152, 155, 156, 
     &               157, 158, 159, 160, 161, 162, 163,
     &               164, 165, 166, 167, 168, 169, 172,
     &               173, 174, 175, 176, 177, 181, 182,
     &               183, 184, 189, 190, 191, 192, 193, 
     &               194, 195, 196, 197, 201, 204, 205,
     &               206, 207, 208, 209, 211, 212, 213,
     &               214, 215, 216, 217, 218, 219, 220,  
     &               222, 223, 226, 227, 228, 229, 231,
     &               232, 233, 234, 235, 238, 239, 241/ 
        DATA  HH3  /
     &               242, 243, 244, 245, 246, 247, 248,
     &               249, 250, 251, 252, 253, 254, 255/
        DATA  HHH  /   1,   2,   3,   4,   5,   6,   7,
     &                 8,   9, 100, 101, 102, 103, 104,
     &               105, 106, 107, 108, 109, 110, 111,
     &               112, 113, 114, 115, 116, 121, 125,
     &               128, 141, 160, 200, 201, 212, 213,
     &               214, 222, 223, 224, 232, 233, 234/
       DATA  HHHNAM/'SFC ','CBL ','CTL ','0DEG','ADCL','MWSL','TRO ',
     &              'NTAT','SEAB','hPa ','kPa ','MSL ','GPM ','GPHM',
     &              'm   ','hm  ','SIG ','HSIG','HYB ','HHYB','cm  ',
     &              'cm  ','THEK','THEK','SPD ','SPD ','hPa ','cm  ',
     &              'mSig','mSig','m   ','EATM','EOCN','LCBL','LCTL',
     &              'LCY ','MCBL','MCTL','MCY ','HCBL','HCTL','HCY '/
        DATA  HHNAM1/ 
     &' PRES ',' PRMSL',' PTEND',' GP   ',' HGT  ',' DIST ',' HSTDV',
     &' TMP  ',' VTMP ',' POT  ',' EPOT ',' T MAX',' T MIN',' DPT  ',
     &' DEPR ',' LAPR ',' VIS  ',' RDSP1',' RDSP2',' RDSP3',' TOTO3',
     &' TMP A',' PRESA',' GP A ',' WVSP1',' WVSP2',' WVSP3',' WDIR ',
     &' WIND ',' U GRD',' V GRD',' STRM ',' V POT',' MNTSF',' SGCVV',
     &' V VEL',' DZDT ',' ABS V',' ABS D',' REL V',' REL D',' VUCSH',
     &' VVCSH',' DIR C',' SP C ',' UOGRD',' VOGRD',' SPF H',' R H  ',
     &' MIXR ',' P WAT',' VAPP ',' SAT D',' EVP  ',' C ICE',' PRATE',
     &' TSTM ',' A PCP',' NCPCP',' ACPCP',' SRWEQ',' WEASD',' SNO D',
     &' MIXHT',' TTHDP',' MTHD ',' MTH A',' T CDC',' CDCON',' L CDC',
     &' M CDC',' H CDC',' C WAT',' SNO C',' SNO L',' WTMP ',' LAND ',
     &' DSL M',' SFC R',' ALBDO',' TSOIL',' SOILM',' VEG  ',' SALTY',
     &' DEN  ',' WAT R',' ICE C',' ICETK',' DICED',' SICED',' U ICE',
     &' V ICE',' ICE G',' ICE D',' SNO M',' HTSGW',' WVDIR',' WVHGT',
     &' WVPER',' SWDIR',' SWELL',' SWPER',' DIRPW',' PERPW',' DIRSW'/
        DATA  HHNAM2/     
     &' PERSW',' NSWRS',' NLWRS',' NSWRT',' NLWRT',' LWAVR',' SWAVR',
     &' G RAD',' LHTFL',' SHTFL',' BLYDP',' U FLX',' V FLX',' WMIXE',
     &' IMG D',' MSLSA',' MSLMA',' MSLET',' LFT X',' 4LFTX',' K X  ',
     &' S X  ',' MCONV',' VW SH',' TSLSA',' BVF2 ',' PVMW ',' CRAIN',
     &' CRFZR',' CICEP',' CSNOW',' SOILW',' PEVPR',' CWORK',' U-GWD',
     &' V-GWD',' PV   ',' COVMZ',' COVTZ',' COVTM',' GFLUX',' CIN  ',
     &' CAPE ',' TKE  ',' CONDP',' SCUSF',' CSDSF',' CSULF',' CSDFL',
     &' CFNSF',' CFNLF',' VBDSF',' VDDSF',' NBDSF',' NDDSF',' M FLX',
     &' LMH  ',' LMV  ',' MLYNO',' NLAT ',' ELON ',' LPS X',' LPS Y',
     &' HGT X',' HGT Y',' VPTMP',' HLCY ',' PROB ',' PROBN',' POP  ',
     &' CPOFP',' CPOZP',' USTM ',' VSTM ',' ICWAT',' DSWRF',' DLWRF',
     &' UVI  ',' MSTAV',' SFEXC',' MIXLY',' USWRF',' ULWRF',' CDLYR',
     &' CPRAT',' TTDIA',' TTRAD',' TTPHY',' PREIX',' TSD1D',' NLGSP',
     &' 5WAVH',' C WAT',' BMIXL',' AMIXL',' PEVAP',' SNOHF',' MFLUX',
     &' DTRF ',' UTRF ',' BGRUN',' SSRUN',' SNO C',' SNO T',' LRGHR'/
        DATA  HHNAM3/     
     &' CNVHR',' CNVMR',' SHAHR',' SHAMR',' VDFHR',' VDFUA',' VDFVA',
     &' VDFMR',' SWHR ',' LWHR ',' CD   ',' FRICV',' RI   ',' MISS '/
       DATA  GRIB  /'GRIB'/
       DATA  KNAM1 /
     & '   WMC/NMC WASHINGTON.  ','   NWS TELECOMMS GATEWAY',
     & '   US FIELD STATIONS    ','   JAPANESE MA TOKYO    ',
     & '   NAT. HURR. C. MIAMI  ','   CANADIAN MC MONTREAL ',
     & '   U.S.A.F. GWC         ','   FNOC MONTEREY, CA.   ',
     & '   NOAA FCST SYS LAB    ','   NCAR, BOULDER CO.    ',
     & '   U.K MET BRACKNELL    ','   FRENCH WS  TOULOUSE  ',
     & '   EUROPEAN SPACE AGENCY','   EUROPEAN CENTER MRF. ',
     & '   DEBILT NETHERLANDS   '/
       DATA  KNAM2 /
     & '   NMC RE-ANALYSIS PROJ.','   NMC ENSEMBLE PRODUCTS'/
       DATA  KNAM3 /
     & '   ABRFC  TULSA, OK     ','   AKRFC  ANCHORAGE, AK ',
     & '   CBRFC  SALT LAKE, UT ','   CNRFC  SACRAMENTO, CA',
     & '   LMRFC  SLIDEL, LA.   ','   MARFC  STATE CO., PA ',
     & '   MBRFC  KANSAS CITY MO','   NCRFC  MINNEAPOLIS MN',
     & '   NERFC  HARTFORD, CT. ','   NWRFC  PORTLAND, OR  ',
     & '   OHRFC  CINCINNATI, OH','   SERFC  ATLANTA, GA   ',
     & '   WGRFC  FORT WORTH, TX','   OUN  NORMAN OK WFO   '/
       DATA  MONTH /'JAN','FEB','MAR','APR','MAY','JUN',
     &              'JUL','AUG','SEP','OCT','NOV','DEC'/
       DATA  SCNTR1/   1,   2/
       DATA  SCNTR2/ 150, 151, 152, 153, 154, 155, 156,
     &               157, 158, 159, 160, 161, 162, 170/
       DATA  TIMUN /'HRS.','DAYS','MOS.','YRS.'/
       DATA  TIMUN1/'HR','DY','MO','YR'/
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C$          1.0 INITIALIZATION - NO. OF ENTRIES IN INDCATOR PARM.
C$                             - NO. OF ENTRIES IN TYPE LEVEL
C$                             - NO. OF ENTRIES IN CNTR PROD. DTA.
C$                             - NO. OF ENTRIES IN SUB CNTR1 PROD. DTA.
C$                             - NO. OF ENTRIES IN SUB CNTR2 PROD. DTA.
C
        IQ   = 224
        IS   =  42 
        IC   =  15
        ICS1 =   2
        ICS2 =  14
        IERR =   0
C
        TITL(1:30)  = '                              '
        TITL(31:60) = '                              '
        TITL(61:86) = '                          '
C
C ---------------------------------------------------------------------
C$           2.0 TEST SECTION 0 FOR ASCII 'GRIB'
C
         IF (GRIB(1:4) .NE. IPDS0(1:4)) THEN
           IERR = 1
           RETURN
         ENDIF
C      
C       TEST SECTION 0 FOR GRIB VERSION 1
C
        IF (ICHAR(IPDS0(8:8)).NE.1) THEN
          IERR = 2
          RETURN
        END IF
C
C       TEST THE LENGTH OF THE PDS (SECTION 1)  
C
        LENPDS  = ICHAR(IPDS(1:1)) * 65536 + ICHAR(IPDS(2:2)) * 256 +
     &            ICHAR(IPDS(3:3))   
        IF (LENPDS.GE.28) THEN
          IDPDS(1:28) = IPDS(1:28)
        ELSE
          IERR = 3
          RETURN
        ENDIF
C
C       TEST PDS FOR PARAMETER TABLE VERSION NUMBER 1 OR 2
C
        IF (ICHAR(IDPDS(4:4)).EQ.1) THEN
          IVER = 1
        ELSE IF (ICHAR(IDPDS(4:4)).EQ.2) THEN
          IVER = 2
        ELSE
          IERR = 9
          RETURN
        END IF    
C
C           4.0  FIND THE INDICATOR AND TYPE LEVELS
C
       IQQ = ICHAR (IDPDS(9:9))
       DO II = 1,IQ
         IF (IQQ .EQ. HH(II)) GO TO 100
       END DO
         IF (IQQ.EQ.77.AND.IVER.EQ.1) GO TO 100
         IERR = 4
         RETURN
C
 100   CONTINUE
         IF (IQQ .NE. 77) THEN
           TITL(21:27) = HHNAM(II)
         ELSE
           TITL(21:27) = ' CONDP '
         END IF
         IF (IQQ.EQ.137.AND.IVER.EQ.1) TITL(21:27) = ' VISIB '
         ISS = ICHAR (IDPDS(10:10))
C
C        CORRECTION FOR 'NLAT' 'ELON' 'L CDC' 'M CDC', 'H CDC',
C                       'T CDC'
C
         IF (ISS.EQ.0.AND.(IQQ.EQ.176.OR.IQQ.EQ.177.
     &     OR.IQQ.EQ.71.OR.IQQ.EQ.73.OR.IQQ.EQ.74.
     &     OR.IQQ.EQ.72.OR.IQQ.EQ.75.OR.IQQ.EQ.213.
     &     OR.IQQ.EQ.173.OR.IQQ.EQ.174)) THEN
           GO TO 300
         END IF
       DO JJ = 1,IS
         IF (ISS .EQ. HHH(JJ)) GO TO 200
       END DO
         IERR = 5
         RETURN
C
 200   CONTINUE
         IF (ISS.EQ.4.OR.ISS.EQ.5.OR.ISS.EQ.100.OR.ISS.EQ.103.OR.
     &     ISS.EQ.105.OR.ISS.EQ.107.OR.ISS.EQ.109.OR.ISS.EQ.111.OR.
     &     ISS.EQ.113.OR.ISS.EQ.115.OR.ISS.EQ.125.OR.
     &     ISS.EQ.160) THEN
           TITL(16:20) = HHHNAM(JJ)
           LEVEL = ICHAR(IDPDS(11:11)) * 256 + ICHAR(IDPDS(12:12))
           IF (ISS.EQ.107) THEN
             ALEVEL = FLOAT(LEVEL) / 10000.0
             WRITE (TITL(9:15),FMT='(F6.4)') ALEVEL
           ELSE IF (ISS.EQ.5) THEN
C             DO NOTHING
           ELSE 
             WRITE (TITL(11:15),FMT='(I4)') LEVEL
           END IF
         ELSE IF (ISS.EQ.1.OR.ISS.EQ.6.OR.ISS.EQ.7.OR.ISS.EQ.8.OR.
     &     ISS.EQ.9  .OR.ISS.EQ.102.OR.ISS.EQ.200.OR.ISS.EQ.201.OR.
     &     ISS.EQ.212.OR.ISS.EQ.213.OR.ISS.EQ.214.OR.ISS.EQ.222.OR.
     &     ISS.EQ.223.OR.ISS.EQ.224.OR.ISS.EQ.232.OR.ISS.EQ.233.OR.
     &     ISS.EQ.234) THEN
           TITL(16:20) = HHHNAM(JJ)
           TITL(1:4)   = '    '
           TITL(11:15) = '    '
         ELSE IF (ISS.EQ.101.OR.ISS.EQ.104.OR.ISS.EQ.106.OR.ISS.EQ.108.
     &     OR.ISS.EQ.110.OR.ISS.EQ.112.OR.ISS.EQ.114.OR.ISS.EQ.116.OR.
     &     ISS.EQ.121.OR.ISS.EQ.128.OR.ISS.EQ.141) THEN
           TITL(6:11)  = HHHNAM(JJ)
           TITL(16:20) = HHHNAM(JJ)
           ITEMP = ICHAR(IDPDS(11:11))
           WRITE (UNIT=TITL(1:4),FMT='(I4)')   ITEMP
           JTEMP = ICHAR(IDPDS(12:12))
           WRITE (UNIT=TITL(11:15),FMT='(I4)') JTEMP
         END IF
C
C               5.0 INSERT THE YEAR,DAY,MONTH AND TIME
C
 300   CONTINUE
       IHR   = ICHAR (IDPDS(16:16))
       IDAY  = ICHAR (IDPDS(15:15))
       IMON  = ICHAR (IDPDS(14:14))
       IYR   = ICHAR (IDPDS(13:13))
       ICEN  = ICHAR (IDPDS(25:25))
C  
C      SUBTRACT 1 FROM CENTURY TO MAKE 4 DIGIT YEAR, CORRECT
C      FOR YEAR 2000. (NOTE: 21 CENTURY STARTS ON JAN 1,2001).
C
       IF (ICEN.NE.0) THEN
         ICEN = ICEN - 1
       END IF
C
       IYR  = ICEN * 100 + IYR
       WRITE (UNIT=TITL(58:61),FMT='(I4)') IYR
       WRITE (UNIT=TITL(51:52),FMT='(I2)') IDAY
       WRITE (UNIT=TITL(37:48),FMT='(A6,I2.2,A2)') 'AFTER ',IHR,' Z'
       TITL(54:56) = MONTH(IMON)
       FCSTIM      = ICHAR (IDPDS(18:18))
       TITL(33:35) = TIMUN(FCSTIM)
       P1          = ICHAR(IDPDS(19:19))
       P2          = ICHAR(IDPDS(20:20))
       TIMERG      = ICHAR(IDPDS(21:21))
       IF (TIMERG.EQ.10) THEN
         P1 = P1 * 256 + P2
         P2 = 0
       END IF
C
C      ADD CORRECTION IF BYTE 21 (TIME RANGE) IS 2
C
       IF (TIMERG.EQ.2) THEN
         TITL(4:20)  = TITL(11:27)
         TITL(21:21) = ' '
         WRITE (UNIT=TITL(22:24),FMT='(I3)') P1
         TITL(25:28) = ' TO '
         WRITE (UNIT=TITL(29:31),FMT='(I3)') P2
C
C      PRECIP AMOUNTS
C
       ELSE IF (TIMERG.EQ.4) THEN
         WRITE (UNIT=TITL(29:31),FMT='(I3)') P2
         MTEMP      = P2 - P1
         WRITE (UNIT=TITL(2:4),FMT='(I3)') MTEMP
         TITL(6:7)  = TIMUN1(FCSTIM)
         TITL(8:12) = ' ACUM'
C
C      AVERAGE 
C
       ELSE IF (TIMERG.EQ.3) THEN
         WRITE (UNIT=TITL(29:31),FMT='(I3)') P2
         MTEMP      = P2 - P1
         WRITE (UNIT=TITL(2:4),FMT='(I3)') MTEMP
         TITL(6:7)  = TIMUN1(FCSTIM)
         TITL(8:12) = ' AVG'
       ELSE
         WRITE (UNIT=TITL(29:31),FMT='(I3)') P1
       ENDIF
C
C      TEST FOR ANALYSIS (MAKE CORRECTION IF MODEL IS ANALYSIS)
C
       IF (TIMERG.EQ.0.AND.P1.EQ.0) THEN
          TITL(29:41) = ' ANALYSIS VT '
          MODEL       = ICHAR(IDPDS(6:6))
          IF (MODEL.EQ.10.OR.MODEL.EQ.19.OR.MODEL.EQ.39.OR.
     &        MODEL.EQ.53.OR.MODEL.EQ.68.OR.MODEL.EQ.69.OR.
     &        MODEL.EQ.70.OR.MODEL.EQ.77.OR.MODEL.EQ.78.OR.
     &        MODEL.EQ.79.OR.MODEL.EQ.80.OR.MODEL.EQ.83.OR.
     &        MODEL.EQ.84.OR.MODEL.EQ.85.OR.MODEL.EQ.86.OR.
     &        MODEL.EQ.87.OR.MODEL.EQ.90.OR.MODEL.EQ.91.OR.
     &        MODEL.EQ.92.OR.MODEL.EQ.93) THEN
              TITL(29:41) = ' 00-HR FCST  '
          ENDIF    
       ENDIF
C
C      TEST FOR 00-HR FCST (INITIALIZED ANALYSIS)
C
       IF (TIMERG.EQ.1.AND.P1.EQ.0) THEN
          TITL(29:41) = ' 00-HR FCST  '
       ENDIF  
C
C$            3.0 FIND WHO GENERATED THE CODE
C$                CHECK FOR SUB-CENTERS
C
       IGENC = ICHAR (IDPDS(5:5))
       ISUBC = ICHAR (IDPDS(26:26))
C
C      TEST FOR SUB-CENTERS WHEN CENTER IS 7
C
       IF (ISUBC.NE.0.AND.IGENC.EQ.7) THEN
         DO J = 1,ICS1
           IF (ISUBC .EQ. SCNTR1(J)) THEN
             TITL(62:86) = KNAM2(J)
             RETURN
           END IF
         END DO
         IERR = 7
       END IF
C
C      TEST FOR SUB-CENTERS WHEN CENTER IS 9
C
       IF (ISUBC.NE.0.AND.IGENC.EQ.9) THEN
         DO J = 1,ICS2
           IF (ISUBC .EQ. SCNTR2(J)) THEN
             TITL(62:86) = KNAM3(J)
             RETURN
           END IF
         END DO
         IERR = 8
       END IF
C 
C      TEST TO SEE IF CENTER IN TABLES
C       
       DO I = 1,IC
         IF (IGENC .EQ. CENTER(I)) THEN
           TITL(62:86) = KNAM1(I)
           RETURN
         END IF
       END DO
C
       IERR = 6
       RETURN
       END
