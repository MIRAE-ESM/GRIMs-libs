   PROGRAM PRMAP
!$$$  main program documentation block
!
! main program:    prmap
!    prgmmr: hann-ming henry juang    org:w/np51   date:01-02-01
!
! abstract: this program is to creat map data for rsm to
!           display the selected domain by grads
!
! program history log:
!    01-02-01  henry juang    finish test 
!
! input file lists:
!   unit 5   - standard input and namelist
!
! output file list:
!   unit 6   - standar output
!       unit 51 - prmap.data for lat lon data over the rsm domain
!       unit 52 - prmap.parm for script to read 
!       unit 61 - prmap.ctl for grads control file
! 
! subprograms called:
!   getgrd   - to compute all latitude and longitude
!   ctlhead   - to create control file header etc
!
! attributes:
!    language: fortran
!$$$
   integer,parameter    ::  LNMAX=4000000
   COMMON /COMRLOC/ IGRD1,JGRD1                                                &
      ,RPROJ,RTRUTH,RORIENT,RDELX,RDELY,RCENLAT,RCENLON,RLFTGRD,RBTMGRD        &
      ,CPROJ,CTRUTH,CORIENT,CDELX,CDELY,CCENLAT,CCENLON,CLFTGRD,CBTMGRD
!
   integer              ::  IZ(1)
   real                 ::  FLAT(LNMAX),FLON(LNMAX),FLATM(LNMAX)
   real                 ::  FI(LNMAX),FJ(LNMAX)
!
   real(4)              ::  R4FLAT(LNMAX),R4FLON(LNMAX),R4FI(LNMAX),R4FJ(LNMAX)
!
   NAMELIST /NAMLOC/ IGRD1,JGRD1                                               &
      ,RPROJ,RTRUTH,RORIENT,RDELX,RDELY,RCENLAT,RCENLON,RLFTGRD,RBTMGRD        &
      ,CPROJ,CTRUTH,CORIENT,CDELX,CDELY,CCENLAT,CCENLON,CLFTGRD,CBTMGRD
!
   READ(5,NAMLOC)
   WRITE(6,NAMLOC)
   LNGRD=IGRD1*JGRD1
   CALL GETGRD(FLAT,FLON,LNGRD)
   print *,' flat ',(flat(i),i=1,10)
   print *,' flon ',(flon(i),i=1,10)
!     
   NOUT=51
   OPEN(NOUT,FILE='prmap.data',STATUS='UNKNOWN',FORM='UNFORMATTED')
   MOUT=52
   OPEN(MOUT,FILE='prmap.parm',STATUS='UNKNOWN',FORM='FORMATTED')
   N=0
   DO J=1,JGRD1
     DO I=1,IGRD1
       N=N+1
       FI(N)=I
       FJ(N)=J
     ENDDO
   ENDDO
   N=0
   DO J=1,JGRD1
     DO I=1,IGRD1
       N=N+1
       R4FI(N)=FI(N)
       R4FJ(N)=FJ(N)
       R4FLAT(N)=FLAT(N)
       R4FLON(N)=FLON(N)
     ENDDO
   ENDDO
   WRITE(NOUT) (R4FI(I),I=1,LNGRD)
   WRITE(NOUT) (R4FJ(I),I=1,LNGRD)
   WRITE(NOUT) (R4FLAT(I),I=1,LNGRD)
   WRITE(NOUT) (R4FLON(I),I=1,LNGRD)
   CLOSE(NOUT)
!
   NCTL=61
   OPEN(NCTL,FILE='prmap.ctl',STATUS='UNKNOWN',FORM='FORMATTED')
!
   IFH=0
   IHR=0
   IDAY=1
   IMON=1
   IYR =99
   STRLON=FLON(1)*180./ACOS(-1.)
   ENDLON=FLON(IGRD1)*180./ACOS(-1.)
   IF(ENDLON.LT.STRLON) ENDLON=ENDLON+360.
   DLON=(ENDLON-STRLON)/FLOAT(IGRD1-1)
   print *,' strlon endlon dlon ',strlon,endlon,dlon
   DO J=1,JGRD1
     I=(J-1)*IGRD1+1
     FLATM(J)=FLAT(I)*180./ACOS(-1.)
   ENDDO
   STRLAT=FLATM(1)
   DLAT=FLATM(2)-FLATM(1)
   IZ(1)=0
   LEVR=1
   DX=RDELX/1000.
   CALL CTLHEAD(NCTL,IGRD1,JGRD1,LEVR,                                         &
                RPROJ,RLFTGRD,RBTMGRD,RORIENT,DX,                              &
                IHR,IDAY,IMON,IYR,IFH,STRLON,DLON,STRLAT,DLAT,                 &
                FLATM,IZ)
!
   WRITE(NCTL,100)
 100   FORMAT('vars 4')
   WRITE(NCTL,101)
   WRITE(NCTL,102)
   WRITE(NCTL,103)
   WRITE(NCTL,104)
 101  FORMAT('FI  0 99 I grid')
 102  FORMAT('FJ  0 99 J grid')
 103  FORMAT('FLAT  0 99 latitudes')
 104  FORMAT('FLON  0 99 longitudes')
   WRITE(NCTL,300)
 300  FORMAT('endvars')
!
   IPROJ=RPROJ
   IF(IPROJ.EQ.0) THEN
     FLON1=STRLON
     FLON2=ENDLON
   ELSE
     FLON1=FLON(1)
     FLON2=FLON(1)
     FLAT1=FLAT(1)
     FLAT2=FLAT(1)
     DO N=2,LNGRD
       FLON1=MIN(FLON1,FLON(N))
       FLON2=MAX(FLON2,FLON(N))
       FLAT1=MIN(FLAT1,FLAT(N))
       FLAT2=MAX(FLAT2,FLAT(N))
     ENDDO
     FLON1=FLON1*180./ACOS(-1.)
     FLON2=FLON2*180./ACOS(-1.)
     FLAT1=FLAT1*180./ACOS(-1.)
     FLAT2=FLAT2*180./ACOS(-1.)
     RORIENT=MOD(RORIENT,360.)
     FLON1=MOD(FLON1,360.)
     FLON2=MOD(FLON2,360.)
     IF( RORIENT.LT.0.0 ) RORIENT=RORIENT+360.
     IF( FLON1.LT.0.0 ) FLON1=FLON1+360.
     IF( FLON2.LT.0.0 ) FLON2=FLON2+360.
     D1=ABS(FLON1-RORIENT)
     D2=ABS(FLON2-RORIENT)
     DD=MAX(D1,D2)
     DD=MIN(180.,DD)
     FLON1=RORIENT-DD
     FLON2=RORIENT+DD
   ENDIF
   WRITE(MOUT,123) IGRD1,JGRD1,IPROJ,FLON1,FLON2,FLAT1,FLAT2
 123  FORMAT(3I5,4F8.2)
!
   STOP
   END

   SUBROUTINE CTLHEAD(NN,IM,JM,KM,PROJ,PI,PJ,OR,DX,                            &
                     IHR,IDAY,IMON,IYR,IFH,                                    &
                     STRLON,DLON,STRLAT,DLAT,                                  &
                     RLAT,IZ)
!$$$
   real                 ::  RLAT(JM)
   integer              ::  IZ(KM)
   CHARACTER(len=2)     ::  HOUR,DAY
   CHARACTER(len=3)     ::  MON(12)
   DATA MON/'JAN','FEB','MAR','APR','MAY','JUN',                               &
            'JUL','AUG','SEP','OCT','NOV','DEC'/
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   PRINT *,' WRITE CONTROL HEADER TO CTLHEAD '
   WRITE(NN,101)
   WRITE(NN,102)
   WRITE(NN,103)
   WRITE(NN,105)
   WRITE(NN,106)
 101  FORMAT('dset prmap.data')
 102  FORMAT('options sequential')
 103  FORMAT('options byteswapped')
 105  FORMAT('undef -9.99E+33')
 106  FORMAT('title EXP1')
!
   IF( PROJ.EQ.0.0 ) THEN
     WRITE(NN,108) IM,STRLON,DLON
     WRITE(NN,110) JM
     WRITE(NN,111) (RLAT(J),J=1,JM)
   ELSE 
     WRITE(NN,107) IM,JM,PI,PJ,OR,DX
     IMP=360.*111/DX
     STRLONP=0.0
     DLONP=360./IMP
     JMP=IMP/4
     STRLATP=0.0
     DLATP=DLONP
     WRITE(NN,108) IMP,STRLONP,DLONP
     WRITE(NN,109) JMP,STRLATP,DLATP
   ENDIF
 107  FORMAT('pdef',2I5,' nps',4F11.5)
 108  FORMAT('xdef',I5,' linear',2F9.3)
 109  FORMAT('ydef',I5,' linear',2F9.3)
 110  FORMAT('ydef',I5,' levels')
 111  FORMAT(10F8.3)
!
   WRITE(NN,112) KM
 112  FORMAT('zdef',I5,' levels 0 ')
   IF( IHR.LT.10 ) THEN
     WRITE(HOUR,90) IHR
   ELSE
     WRITE(HOUR,91) IHR
   ENDIF
   IF( IDAY.LT.10 ) THEN
     WRITE(DAY,90) IDAY
   ELSE
     WRITE(DAY,91) IDAY
   ENDIF
  90  FORMAT('0',I1)
  91  FORMAT(I2)
   IF( IFH.EQ.0 ) IFH=1
   WRITE(NN,114) HOUR,DAY,MON(IMON),IYR,IFH
 114  FORMAT('tdef 1 linear ',A2,'Z',A2,A3,I2,I10,'hr')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   RETURN
   END


   SUBROUTINE GETGRD(FLAT,FLON,LNGRD)
   real                 ::  FLAT(LNGRD),FLON(LNGRD)
!
   COMMON /COMRLOC/ IGRD1,JGRD1                                                &
      ,RPROJ,RTRUTH,RORIENT,RDELX,RDELY,RCENLAT,RCENLON,RLFTGRD,RBTMGRD        &
      ,CPROJ,CTRUTH,CORIENT,CDELX,CDELY,CCENLAT,CCENLON,CLFTGRD,CBTMGRD
!
   PRINT 1234
 1234 FORMAT(' ==== IN ROUTINE SETGRD === ')
!
   HFPI = DASIN(1.0D0)
   QTPI = HFPI * 0.5
   PI = 2.0 * HFPI
   TWOPI = 2.0 * PI
   RAD = PI / 180.
   DELX = RDELX
   DELY = RDELY
!
! --------- SETUP REGIONAL LAT/LON AND MAP FACTOR -----
!
! IF PROJ=0  DO MERCATER PROJECTION
! IF PROJ=1  DO NORTH POLAR PROJECTION
! IF PROJ=-1 DO SOUTH POLAR PROJECTION
!
   NPROJ = RPROJ
!
   IF( NPROJ.EQ.1 .OR. NPROJ.EQ.-1 ) THEN
! ++++++++++++++++++++++++++++++++++++++
! POLAR PROJECTION
! ++++++++++++++++++++++++++++++++++++++
   TRUTH  = RTRUTH * RAD
   TRUTH  = NPROJ * TRUTH
   ORIENT  = RORIENT * RAD
   DLAMDA0 = ORIENT + HFPI
   A2 =  6371200. * ( 1.0 + SIN(TRUTH) )
   RADLAT = RCENLAT * RAD
   RADLON = RCENLON * RAD - DLAMDA0
   RADLAT = NPROJ * RADLAT
   RADLON = NPROJ * RADLON
   YYY = A2 * COS( RADLAT )/(1. + SIN( RADLAT ) )
   CENLAT = RCENLAT
   IF( ABS(CENLAT) .EQ. 90. ) YYY = 0.0
   Y00 = YYY * SIN( RADLON ) - ( RBTMGRD -1.) * DELY
   X00 = YYY * COS( RADLON ) - ( RLFTGRD -1.) * DELX
   PRINT *,' DELX X00 Y00 ',DELX,X00,Y00
!
! =========
!           LAT LOOP
   DO 100 J = 1,JGRD1
   LATS = J
   IJLATS = (LATS-1)*IGRD1
   YS = Y00 + (LATS-1)*DELY
!
   DO 100 I=1,IGRD1
   X = X00 + (I-1)*DELX
   IF( X .GT. 0.0 ) THEN
      FLONS = ATAN(YS/X)
   ELSE IF ( X .LT. 0.0 ) THEN
      FLONS = PI + ATAN(YS/X)
   ELSE
      FLONS = HFPI
      IF( YS .LT. 0.0 ) FLONS = FLONS * 3.0
   ENDIF
   FLONS = NPROJ * FLONS + DLAMDA0
   FLONS = MOD(FLONS,TWOPI)
   IF(FLONS.LT.0.0) FLONS = TWOPI + FLONS
!
   RSOA2 = SQRT( X*X + YS*YS )/A2
   FLATS = HFPI - 2.0  * ATAN(RSOA2)
   FLAT(IJLATS+I) = NPROJ * FLATS
   FLON(IJLATS+I) = FLONS
!
 100  CONTINUE
!
   ELSE IF ( NPROJ.EQ.0 ) THEN
!
! ++++++++++++++++++++++++++++
! DO MERCATER
! ++++++++++++++++++++++++++++
   TRUTH  = RTRUTH * RAD
   CENLAT = RCENLAT * RAD
   CENLON = RCENLON * RAD 
   A2 =  6371200. * COS( TRUTH ) 
   X0 = 0.0
   Y0 = A2 * LOG( ABS( TAN( QTPI + 0.5 * CENLAT ) ) )
   X00 = - ( RLFTGRD - 1.0 ) * DELX
   Y00 = - ( RBTMGRD - 1.0 ) * DELY
   DLAMDA0 = 0.0
!
   DO J = 1,JGRD1
      LATS = J
      IJLATS = (LATS-1)*IGRD1
      YS = Y00 + (LATS-1)*DELY + Y0
!
      DO I=1,IGRD1
         X = X00 + (I-1)*DELX + X0
         FLONS = X / A2 + CENLON
!
         FLATS = 2.0 *( ATAN( EXP( YS/A2 ) ) - QTPI )
         FLAT(IJLATS+I) = FLATS
         FLON(IJLATS+I) = FLONS
      enddo
   enddo
   !
   print *, 'flat ',(flat(i),i=1,10)
   print *, 'flon ',(flon(i),i=1,10)
!
   ENDIF
!
!
   RETURN
   END
