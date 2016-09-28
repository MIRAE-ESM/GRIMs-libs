   SUBROUTINE W3FP11 (IPDS0, IPDS, TITL, IERR)
!$$$  SUBPROGRAM DOCUMENTATION  BLOCK
!                .      .    .                                       .
! SUBPROGRAM:  W3FP11        ONE-LINE GRIB TITLER FROM PDS SECTION
!   PRGMMR: MCCLEES          ORG: NMC421      DATE:88-02-02
!
! ABSTRACT: CONVERTS GRIB FORMATTED PRODUCT DEFINITION SECTION VERSION 
!   1 TO A ONE LINE READABLE TITLE.  GRIB SECTION 0 IS ALSO TESTED TO
!   VERIFY THAT GRIB DATA IS BEING DECIPHERED.
!
! PROGRAM HISTORY LOG:
!   91-06-19  R.E.JONES
!   92-05-29  R.E.JONES   ADD WATER TEMP TO TABLES  
!   93-01-19  R.E.JONES   ADD MONTGOMARY STREAM FUNCTION TO TABLES 
!                         ADD CODE FOR SURFACE VALUE 113.
!                         ADD CONDENSATION PRESSURE TO TABLES
!   93-02-19  R.E.JONES   ADD CAPE AND TKE (157 & 158) TO TABLES
!   93-02-24  R.E.JONES   ADD GRIB TYPE PMSLE (130) TO TABLES
!   93-03-26  R.E.JONES   ADD GRIB TYPE SGLYR (175) TO TABLES
!   93-03-27  R.E.JONES   CHANGES FOR REVISED O.N.388 MAR. 3,1993
!   93-03-29  R.E.JONES   ADD SAVE STATEMENT
!   93-04-16  R.E.JONES   ADD GRIB TYPE LAT, LON (176,177) TO TABLES
!   93-04-25  R.E.JONES   ADD GRIB TYPE 204, 205, 211, 212, 218
!   93-05-18  R.E.JONES   ADD TEST FOR MODEL 70
!   93-06-26  R.E.JONES   ADD GRIB TYPE 128, 129, TAKE OUT TEST FOR
!                         MODEL 86.
!   93-08-07  R.E.JONES   ADD GRIB TYPE 156 (CIN), 150 (CBMZW),
!                         151 (CBTZW), 152 (CBTMW) TO TABLES.
!   93-10-14  R.R.JONES   CHANGE FOR O.N. 388 REV. OCT. 8,1993
!   93-10-29  R.E.JONES   CHANGE FOR 'L CDC' 'M CDC' 'H CDC'
!   93-10-14  R.R.JONES   CHANGE FOR O.N. 388 REV. NOV. 19,1993
!   94-02-05  R.R.JONES   CHANGE FOR O.N. 388 REV. DEC. 14,1993
!                         ADD MODEL NUMBER 86 AND 87.
!
! USAGE:    CALL W3FP11  (IPDS0,  IPDS,  TITL, IERR )
!   INPUT ARGUMENT LIST:
!     IPDS0    - GRIB SECTION 0 READ AS CHARACTER*8
!     IPDS     - GRIB PDS SECTION READ AS CHARACTER*28
!
!   OUTPUT ARGUMENT LIST:   
!     TITL     - CHARACTER*86 OUTPUT PRINT LINE
!     IERR   0 - COMPLETED SATISFACTORILY
!            1 - GRIB SECTION 0, CAN NOT FIND 'GRIB'
!            2 - GRIB IS NOT VERSION 1
!            3 - LENGTH OF PDS SECTION IS LESS THAN 28
!            4 - COULD NOT MATCH TYPE INDICATOR
!            5 - COULD NOT MATCH TYPE LEVEL
!            6 - COULD NOT INTERPRET ORIGINATOR OF CODE
!            7 - COULD NOT INTERPRET SUB CENTER 7 ORIGINATOR OF CODE
!            8 - COULD NOT INTERPRET SUB CENTER 9 ORIGINATOR OF CODE
!            9 - PARAMETER TABLE VERSION NOT 1 OR 2
!
! ATTRIBUTES:
!   LANGUAGE: HP FORTRAN 77
!   MACHINE:  Hewlett-Packard 9000/705, 715, 735, 750, 755, 712
!
!$$$

     INTEGER      ::  CENTER(14)
     INTEGER      ::  SCNTR1(1)
     INTEGER      ::  SCNTR2(14)
     INTEGER      ::  FCSTIM
     INTEGER      ::  HH(207)
     INTEGER      ::  HH1(105)
     INTEGER      ::  HH2(102)
     INTEGER      ::  HHH(31)
     INTEGER      ::  IERR
     INTEGER      ::  P1
     INTEGER      ::  P2
     INTEGER      ::  TIMERG

     CHARACTER(len=6)   ::  HHNAM(207)
     CHARACTER(len=6)   ::  HHNAM1(105)
     CHARACTER(len=6)   ::  HHNAM2(102)
     CHARACTER(len=4)   ::  HHHNAM(31)
     CHARACTER * (*)    ::  IPDS 
     CHARACTER(len=8)   ::  IPDS0
     CHARACTER(len=28)  ::  IDPDS
     CHARACTER(len=4)   ::  GRIB
     CHARACTER(len=28)  ::  KNAM1(14)
     CHARACTER(len=28)  ::  KNAM2(1)
     CHARACTER(len=28)  ::  KNAM3(14)
     CHARACTER(len=3)   ::  MONTH(12)
     CHARACTER(len=4)   ::  TIMUN(4)
     CHARACTER(len=2)   ::  TIMUN1(4)
     CHARACTER(len=86)  ::  TITL

     EQUIVALENCE     (HH(1),HH1(1))
     EQUIVALENCE     (HH(106),HH2(1))
     EQUIVALENCE     (HHNAM(1),HHNAM1(1))
     EQUIVALENCE     (HHNAM(106),HHNAM2(1))

     SAVE

     DATA  CENTER/  7,   8,   9,  34,  52,  54,  57,                           & 
                   58,  59,  74,  85,  97,  98,  99/
     DATA  HH1  /                                                              &
                    1,   2,   3,   6,   7,   8,   9,                           &
                   11,  12,  13,  14,  15,  16,  17,                           &
                   18,  19,  20,  21,  22,  23,  25,                           &
                   26,  27,  28,  29,  30,  31,  32,                           &
                   33,  34,  35,  36,  37,  38,  39,                           &
                   40,  41,  42,  43,  44,  45,  46,                           &
                   47,  48,  49,  50,  51,  52,  53,                           &
                   54,  55,  56,  57,  58,  59,  60,                           &
                   61,  62,  63,  64,  65,  66,  67,                           &
                   68,  69,  70,  71,  72,  73,  74,                           &
                   75,  76,  78,  79,  80,  81,  82,                           &
                   83,  84,  85,  86,  87,  88,  89,                           &
                   90,  91,  92,  93,  94,  95,  96,                           &
                   97,  98,  99, 100, 101, 102, 103,                           &
                  104, 105, 106, 107, 108, 109, 110/
     DATA  HH2  /                                                              &
                  111, 112, 113, 114, 115, 116, 117,                           & 
                  121, 122, 123, 124, 125, 126, 127,                           &
                  128, 129, 130, 131, 132, 133, 134,                           &
                  135, 136, 137, 138, 139, 140, 141,                           &
                  142, 143, 150, 151, 152, 155, 156,                           &
                  157, 158, 159, 160, 161, 162, 163,                           &
                  164, 165, 166, 167, 168, 169, 172,                           &
                  173, 174, 175, 176, 177, 181, 182,                           &
                  183, 184, 201, 204, 205, 207, 208,                           &
                  209, 211, 212, 213, 214, 215, 216,                           &
                  217, 218, 219, 220, 222, 223, 226,                           &
                  227, 228, 229, 231, 232, 233, 234,                           &
                  235, 238, 239, 241, 242, 243, 244,                           &
                  245, 246, 247, 248, 249, 250, 251,                           &
                  252, 253, 254, 255/
     DATA  HHH  /   1,   2,   3,   4,   5,   6,   7,                           &
                    8,   9, 100, 101, 102, 103, 104,                           &
                  105, 106, 107, 108, 109, 110, 111,                           &
                  112, 113, 114, 121, 125, 128, 141,                           &
                  160, 200, 201/
    DATA  HHHNAM/'SFC ','CBL ','CTL ','0DEG','ADCL','MWSL','TRO ',             &
                 'NTAT','SEAB','hPa ','kPa ','MSL ','GPM ','GPHM',             &
                 'm   ','hm  ','SIG ','HSIG','HYB ','HHYB','cm  ',             &
                 'cm  ','THEK','THEK','hPa ','cm  ','mSig','mSig',             &
                 'm   ','EATM','EOCN'/
     DATA  HHNAM1/                                                             &
   ' PRES ',' PRMSL',' PTEND',' GP   ',' HGT  ',' DIST ',' HSTDV',             &
   ' TMP  ',' VTMP ',' POT  ',' EPOT ',' T MAX',' T MIN',' DPT  ',             &
   ' DEPR ',' LAPR ',' VIS  ',' RDSP1',' RDSP2',' RDSP3',' TMP A',             &
   ' PRESA',' GP A ',' WVSP1',' WVSP2',' WVSP3',' WDIR ',' WIND ',             &
   ' U GRD',' V GRD',' STRM ',' V POT',' MNTSF',' SGCVV',' V VEL',             &
   ' DZDT ',' ABS V',' ABS D',' REL V',' REL D',' VUCSH',' VVCSH',             &
   ' DIR C',' SP C ',' UOGRD',' VOGRD',' SPF H',' R H  ',' MIXR ',             &
   ' P WAT',' VAPP ',' SAT D',' EVP  ',' C ICE',' PRATE',' TSTM ',             &
   ' A PCP',' NCPCP',' ACPCP',' SRWEQ',' WEASD',' SNO D',' MIXHT',             &
   ' TTHDP',' MTHD ',' MTH A',' T CDC',' CDCON',' L CDC',' M CDC',             &
   ' H CDC',' C WAT',' SNO C',' SNO L',' WTMP ',' LAND ',' DSL M',             &
   ' SFC R',' ALBDO',' TSOIL',' SOILM',' VEG  ',' SALTY',' DEN  ',             &
   ' WAT R',' ICE C',' ICETK',' DICED',' SICED',' U ICE',' V ICE',             &
   ' ICE G',' ICE D',' SNO M',' HTSGW',' WVDIR',' WVHGT',' WVPER',             &
   ' SWDIR',' SWELL',' SWPER',' DIRPW',' PERPW',' DIRSW',' PERSW'/
     DATA  HHNAM2/                                                             & 
   ' NSWRS',' NLWRS',' NSWRT',' NLWRT',' LWAVR',' SWAVR',' G RAD',             &
   ' LHTFL',' SHTFL',' BLYDP',' U FLX',' V FLX',' WMIXE',' IMG D',             &
   ' MSLSA',' MSLMA',' MSLET',' LFT X',' 4LFTX',' K X  ',' S X  ',             &
   ' MCONV',' VW SH',' TSLSA',' BVF2 ',' PVMW ',' CRAIN',' CRFZR',             &
   ' CICEP',' CSNOW',' COVMZ',' COVTZ',' COVTM',' GFLUX',' CIN  ',             &
   ' CAPE ',' TKE  ',' CONDP',' SCUSF',' CSDSF',' CSULF',' CSDLF',             &
   ' CFNSF',' CFNLF',' VBDSF',' VDDSF',' NBDSF',' NDDSF',' M FLX',             &
   ' LMH  ',' LMV  ',' MLYNO',' NLAT ',' ELON ',' LPS X',' HGT X',             &
   ' HGT X',' HGT Y',' ICWAT',' DSWRF',' DLWRF',' MSTAV',' SFEXC',             &
   ' MIXLY',' USWRF',' ULWRF',' CDLYR',' CPRAT',' TTDIA',' TTRAD',             &
   ' TTPHY',' PREIX',' TSD1D',' NLGSP',' 5WAVH',' C WAT',' BMIXL',             &
   ' AMIXL',' PEVAP',' SNOHF',' MFLUX',' DTRF ',' UTRF ',' BGRUN',             &
   ' SSRUN',' SNO C',' SNO T',' LRGHR',' CNVHR',' CNVMR',' SHAHR',             &
   ' SHAMR',' VDFHR',' VDFUA',' VDFVA',' VDFMR',' SWHR ',' LWHR ',             &
   ' CD   ',' FRICV',' RI   ',' MISS '/
    DATA  GRIB  /'GRIB'/
    DATA  KNAM1 /                                                              &
    '   WMC/NMC WASHINGTON.  ','   NWS TELECOMMS GATEWAY',                     &
    '   US FIELD STATIONS    ','   JAPANESE MA TOKYO    ',                     &
    '   NAT. HURR. C. MIAMI  ','   CANADIAN MC MONTREAL ',                     &
    '   U.S.A.F. GWC         ','   FNOC MONTEREY, CA.   ',                     &
    '   NOAA FCST SYS LAB    ','   U.K MET BRACKNELL    ',                     &
    '   FRENCH WS  TOULOUSE  ','   EUROPEAN SPACE AGENCY',                     &
    '   EUROPEAN CENTER MRF. ','   DEBILT NETHERLANDS   '/
    DATA  KNAM2 /                                                              &
    '   NMC RE-ANALYSIS PROJ.'/
    DATA  KNAM3 /                                                              &
    '   ABRFC  TULSA, OK     ','   AKRFC  ANCHORAGE, AK ',                     &
    '   CBRFC  SALT LAKE, UT ','   CNRFC  SACRAMENTO, CA',                     &
    '   LMRFC  SLIDEL, LA.   ','   MARFC  STATE CO., PA ',                     &
    '   MBRFC  KANSAS CITY MO','   NCRFC  MINNEAPOLIS MN',                     &
    '   NERFC  HARTFORD, CT. ','   NWRFC  PORTLAND, OR  ',                     &
    '   OHRFC  CINCINNATI, OH','   SERFC  ATLANTA, GA   ',                     &
    '   WGRFC  FORT WORTH, TX','   OUN  NORMAN OK WFO   '/
    DATA  MONTH /'JAN','FEB','MAR','APR','MAY','JUN',                          &
                 'JUL','AUG','SEP','OCT','NOV','DEC'/
    DATA  SCNTR1/   1/
    DATA  SCNTR2/ 150, 151, 152, 153, 154, 155, 156,                           &
                  157, 158, 159, 160, 161, 162, 170/
    DATA  TIMUN /'HRS.','DAYS','MOS.','YRS.'/
    DATA  TIMUN1/'HR','DY','MO','YR'/
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!$          1.0 INITIALIZATION - NO. OF ENTRIES IN INDCATOR PARM.
!$                             - NO. OF ENTRIES IN TYPE LEVEL
!$                             - NO. OF ENTRIES IN CNTR PROD. DTA.
!$                             - NO. OF ENTRIES IN SUB CNTR1 PROD. DTA.
!$                             - NO. OF ENTRIES IN SUB CNTR2 PROD. DTA.

     IQ   = 207
     IS   =  31 
     IC   =  14
     ICS1 =   1
     ICS2 =  14
     IERR =   0

     TITL(1:30)  = '                              '
     TITL(31:60) = '                              '
     TITL(61:86) = '                          '
!
! ---------------------------------------------------------------------
!$           2.0 TEST SECTION 0 FOR ASCII 'GRIB'
!
   IF (GRIB(1:4) .NE. IPDS0(1:4)) THEN
      IERR = 1
      RETURN
   ENDIF
   !      
   !       TEST SECTION 0 FOR GRIB VERSION 1
   !
   IF (ICHAR(IPDS0(8:8)).NE.1) THEN
      IERR = 2
      RETURN
   END IF
   !
   !       TEST THE LENGTH OF THE PDS (SECTION 1)  
   !
    LENPDS  = ICHAR(IPDS(1:1)) * 65536 + ICHAR(IPDS(2:2)) * 256 +              &
               ICHAR(IPDS(3:3))   
    IF (LENPDS.GE.28) THEN
       IDPDS(1:28) = IPDS(1:28)
    ELSE
       IERR = 3
       RETURN
    ENDIF
    !
    !       TEST PDS FOR PARAMETER TABLE VERSION NUMBER 1 OR 2
    !
    IF (ICHAR(IDPDS(4:4)).EQ.1) THEN
      IVER = 1
    ELSE IF (ICHAR(IDPDS(4:4)).EQ.2) THEN
      IVER = 2
    ELSE
      IERR = 9
      RETURN
    END IF    
   !
   !           4.0  FIND THE INDICATOR AND TYPE LEVELS
   !
    IQQ = ICHAR (IDPDS(9:9))
    DO II = 1,IQ
      IF (IQQ .EQ. HH(II)) GO TO 100
    END DO
      IF (IQQ.EQ.77.AND.IVER.EQ.1) GO TO 100
      IERR = 4
      RETURN

 100   CONTINUE
      IF (IQQ .NE. 77) THEN
        TITL(21:27) = HHNAM(II)
      ELSE
        TITL(21:27) = ' CONDP '
      END IF
      IF (IQQ.EQ.137.AND.IVER.EQ.1) TITL(21:27) = ' VISIB '
      ISS = ICHAR (IDPDS(10:10))
   !
   !        CORRECTION FOR 'NLAT' 'ELON' 'L CDC' 'M CCD', 'H CDC'
   !
      IF (ISS.EQ.0.AND.(IQQ.EQ.176.OR.IQQ.EQ.177                               &
        .OR.IQQ.EQ.73.OR.IQQ.EQ.74.OR.IQQ.EQ.75)) THEN
        GO TO 300
      END IF
    DO JJ = 1,IS
      IF (ISS .EQ. HHH(JJ)) GO TO 200
    END DO
      IERR = 5
      RETURN
!
 200   CONTINUE
      IF (ISS.EQ.4.OR.ISS.EQ.5.OR.ISS.EQ.100.OR.ISS.EQ.103.OR.                 &
        ISS.EQ.105.OR.ISS.EQ.107.OR.ISS.EQ.109.OR.ISS.EQ.111.OR.               &
        ISS.EQ.113.OR.ISS.EQ.125.OR.ISS.EQ.160.OR.ISS.EQ.200.OR.               &
        ISS.EQ.201) THEN
        TITL(16:20) = HHHNAM(JJ)
        LEVEL = ICHAR(IDPDS(11:11)) * 256 + ICHAR(IDPDS(12:12))
        IF (ISS.EQ.107) THEN
          ALEVEL = FLOAT(LEVEL) / 10000.0
          WRITE (TITL(9:15),FMT='(F6.4)') ALEVEL
        ELSE IF (ISS.EQ.5) THEN
!             DO NOTHING
        ELSE 
          WRITE (TITL(11:15),FMT='(I4)') LEVEL
        END IF
      ELSE IF (ISS.EQ.102.OR.ISS.EQ.1.OR.ISS.EQ.6.OR.ISS.EQ.7) THEN
        TITL(16:20) = HHHNAM(JJ)
        TITL(1:4)   = '    '
        TITL(11:15) = '    '
      ELSE IF (ISS.EQ.101.OR.ISS.EQ.104.OR.ISS.EQ.106.OR.ISS.EQ.108            &
        .OR.ISS.EQ.110.OR.ISS.EQ.112.OR.ISS.EQ.114.OR.ISS.EQ.121.OR.           &
        ISS.EQ.128.OR.ISS.EQ.141) THEN
        TITL(6:11)  = HHHNAM(JJ)
        TITL(16:20) = HHHNAM(JJ)
        ITEMP = ICHAR(IDPDS(11:11))
        WRITE (UNIT=TITL(1:4),FMT='(I4)')   ITEMP
        JTEMP = ICHAR(IDPDS(12:12))
        WRITE (UNIT=TITL(11:15),FMT='(I4)') JTEMP
      END IF
!
!               5.0 INSERT THE YEAR,DAY,MONTH AND TIME
!
 300   CONTINUE
    IHR   = ICHAR (IDPDS(16:16))
    IDAY  = ICHAR (IDPDS(15:15))
    IMON  = ICHAR (IDPDS(14:14))
    IYR   = ICHAR (IDPDS(13:13))
    ICEN  = ICHAR (IDPDS(25:25))
!  
!      SUBTRACT 1 FROM CENTURY TO MAKE 4 DIGIT YEAR, CORRECT
!      FOR YEAR 2000. (NOTE: 21 CENTURY STARTS ON JAN 1,2001).
!
    IF (ICEN.NE.0) THEN
      ICEN = ICEN - 1
    END IF
!
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
!
!      ADD CORRECTION IF BYTE 21 (TIME RANGE) IS 2
!
    IF (TIMERG.EQ.2) THEN
      TITL(4:20)  = TITL(11:27)
      TITL(21:21) = ' '
      WRITE (UNIT=TITL(22:24),FMT='(I3)') P1
      TITL(25:28) = ' TO '
      WRITE (UNIT=TITL(29:31),FMT='(I3)') P2
!
!      PRECIP AMOUNTS
!
    ELSE IF (TIMERG.EQ.4) THEN
      WRITE (UNIT=TITL(29:31),FMT='(I3)') P2
      MTEMP      = P2 - P1
      WRITE (UNIT=TITL(2:4),FMT='(I3)') MTEMP
      TITL(6:7)  = TIMUN1(FCSTIM)
      TITL(8:12) = ' ACUM'
    ELSE
      WRITE (UNIT=TITL(29:31),FMT='(I3)') P1
    ENDIF
!
!      TEST FOR ANALYSIS (MAKE CORRECTION IF MODEL IS ANALYSIS)
!
    IF (TIMERG.EQ.0.AND.P1.EQ.0) THEN
       TITL(29:41) = ' ANALYSIS VT '
       MODEL       = ICHAR(IDPDS(6:6))
       IF (MODEL.EQ.10.OR.MODEL.EQ.19.OR.MODEL.EQ.39.OR.                       &
           MODEL.EQ.53.OR.MODEL.EQ.68.OR.MODEL.EQ.69.OR.                       &
           MODEL.EQ.70.OR.MODEL.EQ.77.OR.MODEL.EQ.78.OR.                       &
           MODEL.EQ.79.OR.MODEL.EQ.80.OR.MODEL.EQ.83.OR.                       &
           MODEL.EQ.84.OR.MODEL.EQ.85.OR.MODEL.EQ.86.OR.                       &
           MODEL.EQ.87) THEN
           TITL(29:41) = ' 00-HR FCST  '
       ENDIF    
    ENDIF
!
!      TEST FOR 00-HR FCST (INITIALIZED ANALYSIS)
!
    IF (TIMERG.EQ.1.AND.P1.EQ.0) THEN
       TITL(29:41) = ' 00-HR FCST  '
    ENDIF  
!
!$            3.0 FIND WHO GENERATED THE CODE
!$                CHECK FOR SUB-CENTERS
!
    IGENC = ICHAR (IDPDS(5:5))
    ISUBC = ICHAR (IDPDS(26:26))
!
!      TEST FOR SUB-CENTERS WHEN CENTER IS 7
!
    IF (ISUBC.NE.0.AND.IGENC.EQ.7) THEN
      DO J = 1,ICS1
        IF (ISUBC .EQ. SCNTR1(J)) THEN
          TITL(62:86) = KNAM2(J)
          RETURN
        END IF
      END DO
      IERR = 7
    END IF
!
!      TEST FOR SUB-CENTERS WHEN CENTER IS 9
!
    IF (ISUBC.NE.0.AND.IGENC.EQ.9) THEN
      DO J = 1,ICS2
        IF (ISUBC .EQ. SCNTR2(J)) THEN
          TITL(62:86) = KNAM3(J)
          RETURN
        END IF
      END DO
      IERR = 8
    END IF
! 
!      TEST TO SEE IF CENTER IN TABLES
!       
    DO I = 1,IC
      IF (IGENC .EQ. CENTER(I)) THEN
        TITL(62:86) = KNAM1(I)
        RETURN
      END IF
    END DO
!
    IERR = 6
    RETURN
    END
