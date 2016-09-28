#define REAL4_W3LIB
#undef DEBUG
   program maininterp
!
! abstract: read grib file  and do interpolation to other grid
!   it works for global ( gaussian grid input ) as well as regional.
!
   namelist /namgrbin/ idim,jdim,iout,jout
   read(5,namgrbin)
   write(6,namgrbin)
   ijdim=idim*jdim
   ijout=iout*jout
   jf=max(ijdim,ijout)
   call interp(jf,iout,jout)
   stop
   end
!
!   unit   11    grib input file
!
! output files:
!   unit   51    grib output file
!
   subroutine interp(jf,io,jo)
   integer              ::  jpds(25),jgds(22),kpds(25),kgds(22),ids(255)
   integer              ::  kens(5),jens(5),iens(5)
   integer              ::  jpdso(25),jgdso(22),kpdso(25),kgdso(22)
   character(len=132)   ::  fngrib
   integer,parameter    ::  mbuf=1024*128*64
   character(len=1)     ::  cbuf(mbuf)
   logical              ::  lbms(jf),lflag(jf),lo(jf)
   character            ::  go(200+jf*(16+1)/8)
   real                 ::  fn(jf)
   real(8)              ::  fo(jf)
   real(4)              ::  fn4(jf)
   real                 ::  bn(jf),bo(jf),glat(jf)
   integer              ::  n00(jf),n10(jf),n11(jf),n01(jf)
   real                 ::  d00(jf),d10(jf),d11(jf),d01(jf)
   data msk1/32000/,msk2/4000/
!
#ifdef REAL4_W3LIB
   integer(4)           ::  lugb4,msk14,msk24,mnum4,mbuf4
   integer(4)           ::  nlen4,nnum4,iret4
   integer(4)           ::  ndata4
   integer(4)           ::  lskip4,lgrib4,lret4
   integer(4)           ::  n4,jpds4(25),jgds4(22),jens4(5)
   integer(4)           ::  k4,kpds4(25),kgds4(22),kens4(5)
#endif
!     
   namelist /namgrbout/fngrib,i2o,                                             &
                    projo,trutho,cotruo,oriento,igrido,                        &
                    delxo,delyo,rlat1o,rlon1o,rlat2o,rlon2o,                   &
                    ichgpd,kpds8,kpds9,kpds10,kpds11,kpds14,kpds15
   data i2o/0/
   data lugb11/11/,lugb51/51/
   data ichgpd/0/,iflux/0/
   data projo/-999./,trutho/-999./,cotruo/-999./,oriento/-999./
   data delxo/-999./,delyo/-999./,rlat10/-999./,rlon1o/-999./
   data rlat20/-999./,rlon2o/-999./,igrido/255/
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  read entire gaussian grid flux file
   print *,' dimension lugb11  ',jf,lugb11
   read(5,namgrbout)
   write(6,namgrbout)
   if(ichgpd.eq.1) then
      print*,' date changes to ',kpds8,kpds9,kpds10,kpds11
      print*,' period from ',kpds14,' to ',kpds15
   endif
   close(lugb11)
#ifdef ASSIGN
   if(lugb11.lt.10) then
     write(asgnstr,'(22hassign -s unblocked u:,i1)') lugb11
   else
     write(asgnstr,'(22hassign -s unblocked u:,i2)') lugb11
   endif
   call assign('assign -R')
   call assign(asgnstr)
   open(unit=lugb11,file=fngrib,                                               &
        status='old',form='unformatted',err=910)
#else
   call baopen(lugb11,fngrib,iret)
   if(iret.ne.0) go to 910
#endif
   go to 911
  910 continue
   write(6,*) ' error in opening file ',fngrib(1:80)
   print *,'error in opening file ',fngrib(1:80)
   call abort
!
  911 continue
   write(6,*) ' file ',fngrib(1:80),' opened. unit=',lugb11
!
   ids=0
   jpds=-1
!     jgds=-1
   jens=-1
!     jgds(1)=4
   n=0
   mnum=0
   do i=1,jf
     lbms(i)=.true.
   enddo
!
!     call getgb(11,31,jf,n,jpds,jgds,
!    &                 kf,n,kpds,kgds,lbms,fn,iret)
   mnum=0
#ifdef REAL4_W3LIB
   lugb4=lugb11
   msk14=msk1
   msk24=msk2
   mnum4=mnum
   mbuf4=mbuf
   call getgir(lugb4,msk14,msk24,mnum4,mbuf4,cbuf,nlen4,nnum4,iret4)
   nlen=nlen4
   nnum=nnum4
   iret=iret4
#else
   call getgir(lugb11,msk1,msk2,mnum,mbuf,cbuf,nlen,nnum,iret)
#endif
   print *,' iret=',iret
   print *,'nlen=',nlen,' nnum=',nnum
   if(iret.ne.0) then
     write(6,*) 'error.  cbuf length too short in getgir'
     print *,'error.  cbuf length too short in getgir'
     call abort
   endif
   if(nnum.eq.0) then
     write(6,*) 'error. not a grib file. detected in getgir'
     print *,'error.  not a grib file. detected in getgir'
     call abort
   endif
   if(nlen.eq.0) then
     write(6,*) 'error. nlen=0. detected in getgir'
     print *,'error.  nlen=0.  detected in getgir'
     call abort
   endif
   if(nlen.gt.jf) then
     write(6,*) 'error. nlen.gt.jf detected in getgir'
     print *,'error.  nlen .gt. jf  detected in getgir'
     call abort
   endif
!
   n = 0
   jpds = -1
   jgds = -1
   jens = -1
#ifdef REAL4_W3LIB
   nlen4=nlen
   nnum4=nnum
   n4=n
   do i=1,25
     jpds4(i)=jpds(i)
   enddo
   do i=1,22
     jgds4(i)=jgds(i)
   enddo
   do i=1,5
     jens4(i)=jens(i)
   enddo
   call getgbss(cbuf,nlen4,nnum4,n4,jpds4,jgds4,jens4,                         &
                k4,kpds4,kgds4,kens4,lskip4,lgrib4,iret4)
   k=k4
   do i=1,25
     kpds(i)=kpds4(i)
   enddo
   do i=1,22
     kgds(i)=kgds4(i)
   enddo
   do i=1,5
     kens(i)=kens4(i)
   enddo
   lskip=lskip4
   lgrib=lgrib4
   iret=iret4
#else
   call getgbss(cbuf,nlen,nnum,n,jpds,jgds,jens,                               &
                k,kpds,kgds,kens,lskip,lgrib,iret)
#endif
!
   write(6,*) ' Searching the following fields with kpds=.'
   write(6,*) ' jpds( 1-10)=',(jpds(j),j= 1,10)
   write(6,*) ' jpds(11-20)=',(jpds(j),j=11,20)
   write(6,*) ' jpds(21-  )=',(jpds(j),j=21,25)
   write(6,*) ' first grib record in the  grib file ',fngrib
!
#ifdef REAL4_W3LIB
   lugb4=lugb11
   call rdgb(lugb4,lgrib4,lskip4,kpds4,kgds4,ndata4,lbms,fn4,6)
   do i=1,25
      kpds(i)=kpds4(i)
   enddo
   do i=1,22
      kgds(i)=kgds4(i)
   enddo
   ndata=ndata4
   do i=1,ndata
      fn(i)=fn4(i)
   enddo
#else
   call rdgb(lugb11,lgrib,lskip,kpds,kgds,ndata,lbms,fn,6)
#endif
!
   write(6,*) ' kpds( 1-10)=',(kpds(j),j= 1,10)
   write(6,*) ' kpds(11-20)=',(kpds(j),j=11,20)
   write(6,*) ' kpds(21-  )=',(kpds(j),j=21,25)
   print*,' fn rdgb ',(fn(i),i=1,10)
!     
   do while(iret.eq.0.and.n.lt.nnum)
     ibms=mod(kpds(4)/64,2)
     im=kgds(2)
     jm=kgds(3)
     ko=16
     kpdso = kpds
!
     if(i2o.eq.1.or.i2o.eq.0) then
       if(i2o.eq.1) i2o=2
       if(i2o.eq.0) i2o=3
! -------------------------------------------------------------------
! check and setup interpolation
       idrt=kgds(1)
       rlat1=kgds(4)*1.e-3
       rlon1=kgds(5)*1.e-3
       if( idrt.eq.0 ) then      ! lat/lon projection
         proj=3
         rlat1=kgds(04)*1.e-3  ! latitude of first point
         rlat2=kgds(07)*1.e-3  ! latitude of last point
         rlon2=kgds(08)*1.e-3  ! longitude of last point
         delx=kgds(09)  ! dx (milledegree) on truth latitude
         dely=kgds(10)  ! dy (milledegree) on truth latitude
!           orient=kgds(11)  ! scanning mode ( 0 : n->s, 64 : s->n)
!           if(kgds(11).eq.0.) proj = -3.
         if(rlat1.gt.rlat2) proj = -3.
       endif
       if( idrt.eq.4 ) then      ! gaussian projection
         proj=4
         rlat2=kgds(07)*1.e-3  ! latitude of last point
         rlon2=kgds(08)*1.e-3  ! longitude of last point
         delx=kgds(09)  ! dx (milledegree) on truth latitude
         dely=kgds(10)  ! dy (milledegree) on truth latitude
!           orient=kgds(11)  ! scanning mode ( 0 : n->s, 64 : s->n)
         if(kgds(11).eq.0.) proj = -4.
       endif
       if( idrt.eq.1 ) then      ! mercater projection
         proj=0
         rlat2=kgds(07)*1.e-3  ! latitude of last point
         rlon2=kgds(08)*1.e-3  ! longitude of last point
         delx=kgds(12)  ! dx (meter) on truth latitude
         dely=kgds(13)  ! dy (meter) on truth latitude
         orient=rlon1
         truth=kgds(09)*1.e-3 ! truth latitude
         cotru=truth          ! co-truth latitude
       endif
       if( idrt.eq.5 ) then      ! polar projection
         truth=60.0              ! truth latitude
         cotru=60.0              ! co-truth latitude
         orient=kgds(07)*1.e-3   ! orientation 
         delx=kgds(08)  ! dx (meter) on 60 deg
         dely=kgds(09)  ! dy (meter) on 60 deg
         iproj=kgds(10) ! polar projection (first bit 0:north;1=south)
         if( iproj.eq.0 ) then
           proj=1.0
         else
           proj=-1.0
         endif
       endif
       if( idrt.eq.3 ) then      ! lambert projection
         orient=kgds(07)*1.e-3   ! orientation 
         delx=kgds(08)  ! dx (meter) on 60 deg
         dely=kgds(09)  ! dy (meter) on 60 deg
         iproj=kgds(10) !  projection (first bit 0:north;1=south)
         if( iproj.eq.0 ) then
           proj=2.0
         else
           proj=-2.0
         endif
         truth=kgds(12)*1.e-3 ! the 1st lat from pole to cut
         cotru=kgds(13)*1.e-3 ! the 2nd lat from pole to cut
       endif
!
!  check to do interpolation or not
       if(i2o.eq.3) then
          trutho = truth
          oriento = orient
          projo = proj
          cotruo = cotru
          delxo = delx
          delyo = dely
          rlat1o = rlat1
          rlon1o = rlon1
          rlat2o = rlat2
          rlon2o = rlon2
          io = im
          jo = jm
       else
          call i2oini(proj ,orient ,truth ,cotru ,                             &
                 delx ,dely ,rlat1 ,rlon1 ,rlat2,rlon2,im,jm,                  &
                 projo,oriento,trutho,cotruo,                                  &
                 delxo,delyo,rlat1o,rlon1o,rlat2o,rlon2o,                      &
                 n00,n10,n11,n01,d00,d10,d11,d01,lflag,io,jo)
          print*,' n00,n10,n11,n01,d00,d10,d11,d01 ',                          &
             n00(1),n10(1),n11(1),n01(1),d00(1),d10(1),d11(1),d01(1)
       endif
       print *, ' prepare grid to grid interpolation.'
       print *, ' input grid : '
       print *, ' proj=',proj,' orient=',orient,                               &
             ' truth=',truth,' cotru=',cotru,                                  &
             ' delx=',delx,' dely=',dely,                                      &
             ' rlat1=',rlat1,' rlon1=',rlon1
       print *, ' output grid : '
       print *, ' projo=',projo,' oriento=',oriento,                           &
             ' trutho=',trutho,' cotruo=',cotruo,                              &
             ' delxo=',delxo,' delyo=',delyo,                                  &
             ' rlat1o=',rlat1o,' rlon1o=',rlon1o
! .................................................................
       igrid=igrido
       xlat1o=nint(1.e3*rlat1o)
       xlon1o=nint(1.e3*rlon1o)
       xlat2o=nint(1.e3*rlat2o)
       xlon2o=nint(1.e3*rlon2o)
       xtrutho=nint(trutho*1.e3)
       xcotruo=nint(cotruo*1.e3)
       xoriento=nint(oriento*1.e3)
       print *, ' read lat lon (first point) is ',xlat1o,xlon1o
       print *, ' read lat lon (last  point) is ',xlat2o,xlon2o
       nproj=projo
       if( nproj.eq. 0 ) then
         idrt=1                  ! mercater
         iproj=0
         print *, ' mercater projection.'
       elseif( nproj.eq.1 ) then
         idrt=5                  ! polar projection
         iproj=0
         print *, ' north polar projectio.'
       elseif( nproj.eq.-1 ) then
         idrt=5                  ! polar projection
         iproj=128
         print *, ' south polar projectio.'
       elseif( nproj.eq.2 ) then
         idrt=3                  ! lambert projection
         iproj=0
         print *, ' north lambert projectio.'
       elseif( nproj.eq.-2 ) then
         idrt=3                  ! lambert projection
         iproj=128
         print *, ' south lambert projectio.'
       elseif( nproj.eq.3 ) then
         idrt=0                  ! lat-lon 
         iproj=0
         print *, ' equal lat lon projection.'
       elseif( nproj.eq.-3 ) then
         idrt=0                  ! lat-lon 
         iproj=128
         print *, ' equal lat lon projection.'
       elseif( nproj.eq.4 ) then
         idrt=4                  ! gaussian
         iproj=0
         print *, ' gaussian projection.'
       elseif( nproj.eq.-4 ) then
         idrt=4                  ! gaussian
         iproj=128
         print *, ' gaussian projection.'
       else
         idrt=0
         print *, ' undefine map projection.'
       endif
     endif        
!
!--- change the initial time
!
     if(ichgpd.eq.1) then
        kpdso(8) = kpds8
        kpdso(9) = kpds9
        kpdso(10) = kpds10
        kpdso(11) = kpds11
        kpdso(14) = kpds14
        kpdso(15) = kpds15
     endif
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  interpolate and pack and output field
     if(kpds(5).gt.0) then
       if(i2o.eq.2) then
         call subinterp(fn,im*jm,fo,io*jo,                                     &
                      n00,n10,n11,n01,d00,d10,d11,d01,lflag)
         print*,' fn ',(fn(i),i=1,10)
         print*,' fo ',(fo(i),i=1,10)
         do i=1,im*jm
           bn(i)=0.0
           if(lbms(i)) bn(i)=1.0
         enddo
         call subinterp(bn,im*jm,bo,io*jo,                                     &
                      n00,n10,n11,n01,d00,d10,d11,d01,lflag)
         do i=1,io*jo
           lo(i)=.false.
           if(bo(i).ge.1.0) lo(i)=.true.
         enddo
         print *,' write putgb for  io jo ',io,jo
!           call putgb(lugb51,io*jo,kpdso,kgdso,lo,fo,iret)
!           print *,' return code of putgb ',iret
!
       endif
       if(i2o.eq.3) then
         fo = fn
       endif
       if(idrt.eq.4) then
         call gaulat(glat,jo)     !! s-->n
         cl1 = glat(1)*atan(1.)*4/180.   !! colatitude at 1st grid : radian
       else
         cl1 = 0.
       endif
!
       ibms = 0
       ipuk = kpdso(5) 
       itlk = kpdso(6) 
       il1k = kpdso(7) 
       il2k = kpdso(7) 
       iy   = kpdso(8)
       im   = kpdso(9)
       id   = kpdso(10)
       ih   = kpdso(11)
       iftu = kpdso(13)
       ip1  = kpdso(14)
       ip2  = kpdso(15)
       inst = kpdso(16)
       ina  = kpdso(17)
       inm  = kpdso(20)
       icen2= kpdso(23)
       idsk = kpdso(22)

       ienst=0
       iensi=0
       iens(1)=1
       iens(2)=ienst
       iens(3)=iensi
       iens(4)=1
       iens(5)=255
!
!         call gribit(b,lbm,idrt,ig,jg,mxbit,cl1,lnpds,iptv,icen,igen,
!    &            ibms,ipuk,itlk,il1k,il2k,iy,im,id,ih,
!    &            iftu,ip1,ip2,inst,ina,inm,icen2,idsk,jens,
!    &            xlat1,xlon1,xlat2,xlon2,delx,dely,ortru,proj,
!    &            g,lg,ierr)
!
       if( nproj.eq. 0 ) xoriento = xtrutho
        print*,' gribit ',fo(1),lbms(1),idrt,io,jo,16,cl1,28,2,7,80,           &
            ibms,ipuk,itlk,il1k,il2k,iy,im,id,ih,                              &
            iftu,ip1,ip2,inst,ina,inm,icen2,idsk,iens,                         &
            xlat1o,xlon1o,xlat2o,xlon2o,delxo,delyo,xoriento,projo,            &
            lo(1)
!      if(1.eq.1) stop
       call gribit(fo,lbms,idrt,io,jo,16,cl1,28,2,7,80,                        &
           ibms,ipuk,itlk,il1k,il2k,iy,im,id,ih,                               &
           iftu,ip1,ip2,inst,ina,inm,icen2,idsk,iens,                          &
           xlat1o,xlon1o,xlat2o,xlon2o,delxo,delyo,xoriento,projo,             &
           go,lout,ierr)
         print *,' return code of gribit ',ierr
         if(ierr.eq.0) call wryte(lugb51,lout,go)
     endif
!       call getgb(11,31,jf,n,jpds,jgds,
!    &                   kf,n,kpds,kgds,lbms,fn,iret)
!       print *,' return code of getgb iret=',iret
     n = n + 1
   jpds = -1
   jgds = -1
   jens = -1
!
#ifdef REAL4_W3LIB
   nlen4=nlen
   nnum4=nnum
   n4=n
   do i=1,25
     jpds4(i)=jpds(i)
   enddo
   do i=1,22
     jgds4(i)=jgds(i)
   enddo
   do i=1,5
     jens4(i)=jens(i)
   enddo
   call getgbss(cbuf,nlen4,nnum4,n4,jpds4,jgds4,jens4,                         &
                      k4,kpds4,kgds4,kens4,lskip4,lgrib4,iret4)
   k=k4
   do i=1,25
     kpds(i)=kpds4(i)
   enddo
   do i=1,22
     kgds(i)=kgds4(i)
   enddo
   do i=1,5
     kens(i)=kens4(i)
   enddo
   lskip=lskip4
   lgrib=lgrib4
   iret=iret4
#else
   call getgbss(cbuf,nlen,nnum,n,jpds,jgds,jens,                               &
                      k,kpds,kgds,kens,lskip,lgrib,iret)
#endif
!
#ifdef REAL4_W3LIB
   lugb4=lugb11
   if(n.lt.nnum) call rdgb(lugb4,lgrib4,lskip4,                                &
                   kpds4,kgds4,ndata4,lbms,fn4,6)
   do i=1,25
     kpds(i)=kpds4(i)
   enddo
   do i=1,22
     kgds(i)=kgds4(i)
   enddo
   ndata=ndata4
   do i=1,ndata
     fn(i)=fn4(i)
   enddo
#else
   if(n.lt.nnum) call rdgb(lugb11,lgrib,lskip,kpds,kgds,
  1                    ndata,lbms,fn,6)
#endif
   enddo
!
   return
   end
   !
   subroutine gribit(f,lbm,idrt,im,jm,mxbit,colat1,                            &
                     ilpds,iptv,icen,igen,ibms,ipu,itl,il1,il2,                &
                     iyr,imo,idy,ihr,iftu,ip1,ip2,itr,                         &
                     ina,inm,icen2,ids,iens,                                   &
                     xlat1,xlon1,xlat2,xlon2,delx,dely,oritru,proj,            &
                     grib,lgrib,ierr)
!fpp$ noconcur r
!$$$  subprogram documentation block
!
! subprogram:    gribit      create grib message
!   prgmmr: iredell          org: w/nmc23    date: 92-10-31
!
! abstract: create a grib message from a full field.
!   at present, only global latlon grids and gaussian grids
!   and regional polar projections are allowed.
!
! program history log:
!   92-10-31  iredell
!   94-05-04  juang (for gsm and rsm use)
!   98-01-28  juang chnage y2k and add lambert
!   99-01-28  hong chnage multi grids
!
! usage:    call gribit(f,lbm,idrt,im,jm,mxbit,colat1,
!    &                  ilpds,iptv,icen,igen,ibms,ipu,itl,il1,il2,
!    &                  iyr,imo,idy,ihr,iftu,ip1,ip2,itr,
!    &                  ina,inm,icen2,ids,iens,
!    &                  xlat1,xlon1,delx,dely,oritru,proj,
!    &                  grib,lgrib,ierr)
!   input argument list:
!     f        - real (im*jm) field data to pack into grib message
!     lbm      - logical (im*jm) bitmap to use if ibms=1
!     idrt     - integer data representation type
!                (0 for latlon or 4 for gaussian or 5 for polar)
!     im       - integer longitudinal dimension
!     jm       - integer latitudinal dimension
!     mxbit    - integer maximum number of bits to use (0 for no limit)
!     colat1   - real first colatitude of grid if idrt=4 (radians)
!     ilpds    - integer length of the pds (usually 28)
!     iptv     - integer parameter table version (usually 1)
!     icen     - integer forecast center (usually 7)
!     igen     - integer model generating code
!     ibms     - integer bitmap flag (0 for no bitmap)
!     ipu      - integer parameter and unit indicator
!     itl      - integer type of level indicator
!     il1      - integer first level value (0 for single level)
!     il2      - integer second level value
!     iyr      - integer year
!     imo      - integer month
!     idy      - integer day
!     ihr      - integer hour
!     iftu     - integer forecast time unit (1 for hour)
!     ip1      - integer first time period
!     ip2      - integer second time period (0 for single period)
!     itr      - integer time range indicator (10 for single period)
!     ina      - integer number included in average
!     inm      - integer number missing from average
!     icen2    - integer forecast subcenter
!                (usually 0 but 1 for reanal or 2 for ensemble)
!     ids      - integer decimal scaling
!     iens     - integer (5) ensemble extended pds values
!                (application,type,identification,product,smoothing)
!                (used only if icen2=2 and ilpds>=45)
!     xlat1    - real first point of regional latitude (milledegree)
!     xlon1    - real first point of regional longitude (milledegree)
!     xlat2    - real last  point of regional latitude (milledegree)
!     xlon2    - real last  point of regional longitude (milledegree)
!     delx     - real dx on 60n for regional (m)
!     dely     - real dy on 60n for regional (m)
!     proj     - real polar projection flag 1 for north -1 for south
!                     mercater projection 0
!     oritru   - real orientation of regional polar projection or
!                     truth for regional mercater projection (milledegree)
!
!   output argument list:
!     grib     - character (lgrib) grib message
!     lgrib    - integer length of grib message
!                (no more than 100+ilpds+im*jm*(mxbit+1)/8)
!     ierr     - integer error code (0 for success)
!
! subprograms called:
!   gtbits     - compute number of bits and round data appropriately
!   w3fi72     - engrib data into a grib1 message
!
! attributes:
!   language: cray fortran
!
!$$$
#define REAL4_W3LIB
   real                 ::  f(im*jm)
   logical              ::  lbm(im*jm)
   character            ::  grib(*)
   integer,parameter    ::  imax=1000
   integer              ::  ibm(imax*imax),ipds(100),igds(100),ibds(100)
   real                 ::  fr(imax*imax)
   character            ::  pds(1000)
!
   integer              ::  iens(5),kclust(16),kmembr(80),kprob(2)
   real                 ::  xprob(2)
#ifdef REAL4_W3LIB
   integer(4)           ::  iens4(5),kprob4(2),kclust4(16),kmembr4(80)
   integer(4)           ::  ibm4(im*jm)
   integer(4)           ::  ipds4(100),igds4(100),ibds4(100)
   integer(4)           ::  nbit4,nf4,nfo4,lgrib4,ierr4
#endif
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  determine grid parameters
   pi=acos(-1.)
   kprob=0
   nf=im*jm
   if(nf.gt.imax*imax) then
      print *,'imax too small in gribit'
      call abort
   endif
   if(idrt.eq.0) then
     if(im.eq.144.and.jm.eq.73) then
       igrid=2
     elseif(im.eq.360.and.jm.eq.181) then
       igrid=3
     else
       igrid=255
     endif
     iresfl=128
     iscan=0
     lat1=nint(90.e3)
     lon1=0
     lati=nint(180.e3/(jm-1))
     loni=nint(360.e3/im)
     igds09=-lat1
     igds10=-loni
     igds11=lati
     igds12=loni
!yh        iscan=64
     igds13=iscan
     igds14=0
     igds15=0
     igds16=0
     igds17=0
     igds18=0
     if(igrid.eq.255) then
       igrid=255
       lat1= xlat1
       lon1= xlon1
       iresfl=128
       igds09= xlat2
       igds10= xlon2
       igds11= delx
       igds12= dely
       iscan=64     ! s--> n
       if(xlat2.lt.xlat1) iscan = 0
       igds13=iscan
     endif
   elseif(idrt.eq.4) then
     if(im.eq.192.and.jm.eq.94) then
       igrid=98
     elseif(im.eq.384.and.jm.eq.190) then
       igrid=126
     else
       igrid=255
     endif
     iresfl=128
     iscan=0
     lat1=nint(90.e3-180.e3/pi*colat1)
     lon1=0
     lati=jm/2
     loni=nint(360.e3/im)
     igds09=-lat1
     igds10=-loni
     igds11=lati
     igds12=loni
     igds13=iscan
     igds14=0
     igds15=0
     igds16=0
     igds17=0
     igds18=0
   elseif(idrt.eq.5) then    ! polar projection
     igrid=255
     lat1= xlat1
     lon1= xlon1
     iresfl=0
     igds09=oritru
     igds10=delx
     igds11=dely
     if( nint(proj).eq.1  ) igds12=0         ! north polar proj
     if( nint(proj).eq.-1 ) igds12=128       ! south polat proj
     iscan=64
     igds13=iscan
     igds14=0
     igds15=0
     igds16=0
     igds17=0
     igds18=0
   elseif(idrt.eq.3) then    ! lambert projection
     igrid=255
     lat1= xlat1
     lon1= xlon1
     iresfl=8
     igds09=oritru
     igds10=delx
     igds11=dely
     if( nint(proj).eq.2  ) igds12=0         ! north proj
     if( nint(proj).eq.-2 ) igds12=128       ! south proj
     iscan=64
     igds13=iscan
     igds14=0
     igds15=oritru
     igds16=igds15
     igds17=-90000
     igds18=0
   elseif(idrt.eq.1) then    ! mercater projection
     igrid=255
     lat1= xlat1
     lon1= xlon1
     iresfl=0
     igds09= xlat2
     igds10= xlon2
     igds11=delx
     igds12=dely
     igds13=oritru
     iscan=64
     igds14=iscan
     igds15=0
     igds16=0
     igds17=0
     igds18=0
   else
     ierr=40
     return
   endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  reset time range parameter in case of overflow
   if(itr.ge.2.and.itr.le.5.and.ip2.ge.256) then
     jp1=ip2
     jp2=0
     jtr=10
   else
     jp1=ip1
     jp2=ip2
     jtr=itr
   endif
! for y2k
   iyr4=iyr
   if(iyr.le.100) iyr4=2050-mod(2050-iyr,100)
   iy =mod(iyr4-1,100)+1
   ic =(iyr4-1)/100+1
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  fill pds parameters
   ipds(01)=ilpds    ! length of pds
   ipds(02)=iptv     ! parameter table version id
   ipds(03)=icen     ! center id
   ipds(04)=igen     ! generating model id
   ipds(05)=igrid    ! grid id
   ipds(06)=1        ! gds flag
   ipds(07)=ibms     ! bms flag
   ipds(08)=ipu      ! parameter unit id
   ipds(09)=itl      ! type of level id
   ipds(10)=il1      ! level 1 or 0
   ipds(11)=il2      ! level 2
   ipds(12)=iy       ! year
   ipds(13)=imo      ! month
   ipds(14)=idy      ! day
   ipds(15)=ihr      ! hour
   ipds(16)=0        ! minute
   ipds(17)=iftu     ! forecast time unit id
   ipds(18)=jp1      ! time period 1
   ipds(19)=jp2      ! time period 2 or 0
   ipds(20)=jtr      ! time range indicator
   ipds(21)=ina      ! number in average
   ipds(22)=inm      ! number missing
   ipds(23)=ic       ! century
   ipds(24)=icen2    ! forecast subcenter
   ipds(25)=ids      ! decimal scaling
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  fill gds and bds parameters
   igds(01)=0        ! number of vertical coords
   igds(02)=255      ! vertical coord flag
   igds(03)=idrt     ! data representation type
   igds(04)=im       ! east-west points
   igds(05)=jm       ! north-south points
   igds(06)=lat1     ! latitude of origin
   igds(07)=lon1     ! longitude of origin
   igds(08)=iresfl   ! resolution flag
   igds(09)=igds09   ! latitude of end or orientation
   igds(10)=igds10   ! longitude of end or dx in meter on 60n
   igds(11)=igds11   ! lat increment or gaussian lats or dy in meter
   igds(12)=igds12   ! longitude increment or projection
   igds(13)=igds13   ! scanning mode or lat of intercut on earth for mercater
   igds(14)=igds14   ! not used or scanning mode for mercater
   igds(15)=igds15   ! not used or cut latitude near pole for lambert
   igds(16)=igds16   ! not used or cut latitude near equator for lambert
   igds(17)=igds17   ! not used or lat of south pole for lambert
   igds(18)=igds18   ! not used or lon of south pole for lambert
   ibds(1)=0       ! bds flags
   ibds(2)=0       ! bds flags
   ibds(3)=0       ! bds flags
   ibds(4)=0       ! bds flags
   ibds(5)=0       ! bds flags
   ibds(6)=0       ! bds flags
   ibds(7)=0       ! bds flags
   ibds(8)=0       ! bds flags
   ibds(9)=0       ! bds flags
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  fill bitmap and count valid data.  reset bitmap flag if all valid.
   nbm=nf
   if(ibms.ne.0) then
     nbm=0
     do i=1,nf
       if(lbm(i)) then
         ibm(i)=1
         nbm=nbm+1
       else
         ibm(i)=0
       endif
     enddo
     if(nbm.eq.nf) ipds(7)=0
   endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  round data and determine number of bits
   if(nbm.eq.0) then
     do i=1,nf
       fr(i)=0.
     enddo
     nbit=0
   else
     call gtbits(ipds(7),ids,nf,ibm,f,fr,fmin,fmax,nbit)
!       write(0,'("gtbits:",4i4,4x,2i4,4x,2g16.6)')
!    &   ipu,itl,il1,il2,ids,nbit,fmin,fmax
     if(mxbit.gt.0) nbit=min(nbit,mxbit)
   endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  create product definition section
#ifdef REAL4_W3LIB
   do i=1,100
     ipds4(i)=ipds(i)
   enddo
   call w3fi68(ipds4,pds)
#else
   call w3fi68(ipds,pds)
#endif
   if(icen2.eq.2.and.ilpds.ge.45) then
     ilast=45
#ifdef REAL4_W3LIB
     do i=1,5
       iens4(i)=iens(i)
     enddo
     do i=1,2
       kprob4(i)=kprob(i)
     enddo
     do i=1,16
       kclust4(i)=kclust(i)
     enddo
     do i=1,80
       kmembr4(i)=kmembr(i)
     enddo
     ilast4=ilast
     call pdsens(iens4,kprob4,xprob,kclust4,kmembr4,ilast4,pds)
#else
     call pdsens(iens,kprob,xprob,kclust,kmembr,ilast,pds)
#endif
   endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  create grib message
#ifdef REAL4_W3LIB
   nbit4=nbit
   do i=1,100
     ipds4(i)=ipds(i)
   enddo
   do i=1,100
     igds4(i)=igds(i)
   enddo
   do i=1,nf
     ibm4(i)=ibm(i)
   enddo
   nf4=nf
   do i=1,100
     ibds4(i)=ibds(i)
   enddo
   call w3fi72(0,fr,0,nbit4,1,ipds4,pds,                                       &
               1,255,igds4,0,0,ibm4,nf4,ibds4,                                 &
               nfo4,grib,lgrib4,ierr4)
   nfo=nfo4
   lgrib=lgrib4
   ierr=ierr4
#else
   call w3fi72(0,fr,0,nbit,1,ipds,pds,                                         &
               1,255,igds,0,0,ibm,nf,ibds,                                     &
               nfo,grib,lgrib,ierr)
#endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   return
   end
   !
   subroutine subinterp(a,ij,b,ijo,                                            &
                      n00,n10,n11,n01,d00,d10,d11,d01,lflag)
!
   real                 ::  a(ij),b(ijo)
!
   logical              ::  lflag(ijo)
   integer              ::  n00(ijo),n10(ijo)
   integer              ::  n11(ijo),n01(ijo)
   real                 ::  d00(ijo),d10(ijo)
   real                 ::  d11(ijo),d01(ijo)
   real                 ::  a00(ijo),a10(ijo)
   real                 ::  a11(ijo),a01(ijo)
!
!     if there is a point out of data domain put the averaged value.
!     
   ifdomain = 1
   do n = 1,ijo
     if(.not.lflag(n)) ifdomain = 0
   enddo
   if(ifdomain.ne.1) call averag(a,ij,avg)
!
   do n=1,ijo
      if( lflag(n) ) then
         a00(n) = a(n00(n))
         a10(n) = a(n10(n))
         a11(n) = a(n11(n))
         a01(n) = a(n01(n))
      else
!        a00(n)=-9999.9999
         a00(n)=avg
         a10(n)=0.0
         a11(n)=0.0
         a01(n)=0.0
      endif
   enddo
   do n=1,ijo
      b(n)   =    a00(n) * d00(n) + a10(n) * d10(n)                            &
                + a11(n) * d11(n) + a01(n) * d01(n)
   enddo
!
   return
   end
   !
   subroutine averag(a,ij,avg)
   real                 ::  a(ij)
   !
   sum = 0.
   do n = 1,ij
      sum = a(n) + sum
   enddo
   avg = sum / ij
   return
   end
   !
   subroutine i2oini(proj ,orient ,truth ,cotru ,                              &
                     delx ,dely ,rlat1 ,rlon1 ,rlat2,rlon2,ii, jj,             &
                     projo,oriento,trutho,cotruo,                              &
                     delxo,delyo,rlat1o,rlon1o,rlat2o,rlon2o,                  &
                     n00,n10,n11,n01,d00,d10,d11,d01,lflag,io,jo)
!
   logical              ::  lflag(io*jo)
   integer              ::  n00(io*jo),n10(io*jo)
   integer              ::  n11(io*jo),n01(io*jo)
   real                 ::  d00(io*jo),d10(io*jo)
   real                 ::  d11(io*jo),d01(io*jo)
   real                 ::  glat(jj),glon(ii),gaul(jj)
!
   if(abs(proj).eq.4.) then  ! gaussian grid lat/lon data
      call gaulat(gaul,jj)   ! n-->s ( 0 --> 180)
      do j = 1,jj
         if(proj.eq.-4.) then      ! n--> s
            glat(j) = 90.-gaul(j)
         elseif(proj.eq.4) then    ! s--> n
            glat(jj-j+1) = 90.-gaul(j)
         endif
      enddo
   endif
   if(abs(proj).eq.3.) then  ! lat/lon data
      do j = 1,jj
         if(proj.eq.3.) then
            glat(j) = rlat1 + dely/1000. * float(j-1)
         elseif(proj.eq.-3.) then
            glat(j) = rlat1 - dely/1000. * float(j-1)
         endif
      enddo
   endif
!
   if(abs(proj).eq.3..or.abs(proj).eq.4.) then
      do i = 1,ii
         glon(i) = rlon1 + delx/1000. * float(i-1)
         if(glon(i).lt.0.) glon(i) = glon(i) + 360.
      enddo
   else
      call ll2xy(proj,orient,truth,cotru,rlat1,rlon1,x00,y00)
      print *, ' input grid rlat1 rlon1 x00 y00 ',                             &
                        rlat1,rlon1,x00,y00
   endif
!
   if(abs(projo).ne.3.) then
      call ll2xy(projo,oriento,trutho,cotruo,rlat1o,rlon1o,x00o,y00o)
      print *, ' output grid rlat1  rlon1  x00  y00 ',                         &
                         rlat1o,rlon1o,x00o,y00o
   endif
!
#ifdef DEBUG
   print*,' i j glat glon ',glat,glon
#endif
   n = 0
   do j=1,jo
      if(abs(projo).eq.3.) then
         if(projo.eq.3.) then
           rlato = rlat1o + delyo*float(j-1)/1000.
         elseif(projo.eq.-3.) then
           rlato = rlat1o - delyo*float(j-1)/1000.
         endif
      else
         yo=y00o+(j-1)*delyo
      endif
      do i=1,io
         if(abs(projo).eq.3.) then
            rlono = rlon1o + delxo*float(i-1)/1000. 
            if(rlono.lt.0.) rlono = rlono + 360.
            if(abs(proj).eq.3..or.abs(proj).eq.4.) then
               call ll2gg(proj,glat,glon,ii,jj,delx,dely,rlat1,rlon1,          &
                           rlato,rlono,x,y)
            else
               call ll2xy(proj,orient,truth,cotru,rlato,rlono,x,y)
            endif
         else
            xo=x00o+(i-1)*delxo
            call xy2ll(projo,oriento,trutho,cotruo,xo,yo,rlato,rlono)
            if(abs(proj).eq.3..or.abs(proj).eq.4.) then
               call ll2gg(proj,glat,glon,ii,jj,delx,dely,rlat1,rlon1,          &
                     rlato,rlono,x,y)
            else
               call ll2xy(proj,orient,truth,cotru,rlato,rlono,x,y)
            endif
         endif 
         if(abs(proj).eq.3..or.abs(proj).eq.4.) then
            xlon=x
            xlat=y
         else
            xlon=(x-x00)/delx+1
            xlat=(y-y00)/dely+1
         endif
#ifdef DEBUG
         print*,' i j rlato rlono x y '
         print*,i,j,rlato,rlono,x,y
#endif
         lon=xlon
         lat=xlat
!
         n = n + 1
         lflag(n)=.true.
         dlon1=xlon-lon
         dlat1=xlat-lat
         dlon0=1-dlon1
         dlat0=1-dlat1
         if( lat.lt.1 ) then
            print *, y,' out side of y dimension at i j ',i,j
            lflag(n)=.true.
            lon=1
            lat=1
            dlon0=1
            dlat0=1
            dlon1=0
            dlat1=0
         endif
         if( lat.ge.jj.or. (lat.eq.(jj-1).and.lon.eq.ii) ) then
            print *, y,' out side of y dimension at i j ',i,j
            lflag(n)=.true.
            lon=1
            lat=jj-1
            dlon0=0
            dlat0=0
            dlon1=1
            dlat1=1
         endif
!
         d00(n) = dlon0 * dlat0
         d10(n) = dlon1 * dlat0
         d11(n) = dlon1 * dlat1
         d01(n) = dlon0 * dlat1
         n00(n) = lon   + (lat   -1)*ii
         n10(n) = lon+1 + (lat   -1)*ii
         n11(n) = lon+1 + (lat+1 -1)*ii
         n01(n) = lon   + (lat+1 -1)*ii
      enddo
   enddo
!
   return
   end
   subroutine ll2gg(proj,glat,glon,ii,jj,delx,dely,rlat1,rlon1,                &
                    rlat,rlon,xlon,ylat)
   real                 ::  glat(jj),glon(ii)
   logical              ::  flag
!
   flag=.true.
   do i = 1,ii
      if(flag.and.rlon.lt.glon(i)) then
         xlon = i + (rlon-glon(i))/(glon(i+1)-glon(i))
         lon = i
         flag = .false.
      endif
   enddo
   if(flag) then
      xlon = 1
      lon = 1
   endif
!
   flag=.true.
   if(proj.gt.0.) then
   do j = jj,2,-1
      if(flag.and.glat(j).le.rlat) then
         ylat = j + (rlat-glat(j))/(glat(j+1)-glat(j))
         lat = j
         flag = .false.
      endif
      if(rlat.le.glat(1)) then
         ylat = 1
         lat = 1
         flag = .false.
      endif
   enddo
   else
   do j = 2,jj
      if(flag.and.glat(j).le.rlat) then
         ylat = j-1 + (glat(j-1)-rlat)/(glat(j-1)-glat(j))
         lat = j-1
         flag = .false.
      endif
      if(rlat.ge.glat(1)) then
         ylat = 1
         lat = 1
         flag = .false.
      endif
   enddo
   endif
   return
   end
!
   subroutine ll2xy(cproj,corient,ctruth,ccotru,clat,clon,cx,cy)
!
   real,parameter       ::  pi=3.14159,twopi=2.0*pi,hfpi=0.5*pi,qtpi=0.5*hfpi
   real,parameter       ::  rad=pi/180.,rerth=6371220.
!
! input are all degree and output to global x y, not domain x y
!
! if proj=0  do mercater projection
! if proj=1  do north polar projection
! if proj=-1 do south polar projection
! if proj=2  do north lambert projection
! if proj=-2 do south lambert projection
!
   nproj = cproj
   blat = clat * rad
   blon = clon * rad
!
   if( nproj.eq.1 .or. nproj.eq.-1 ) then
! ++++++++++++++++++++++++++++++++++++++
! polar projection
! ++++++++++++++++++++++++++++++++++++++
     truth  = ctruth * rad
     truth  = nproj * truth
     orient  = corient * rad
     cenlon = mod(orient,twopi)
     if(cenlon.lt.0.e0) cenlon = twopi + cenlon
     dlamda0 = cenlon + hfpi
     a2 =  rerth * ( 1.0 + sin(truth) )
     radlat = 90. * rad
     radlon = 0.0 * rad - dlamda0
     radlat = nproj * radlat
     radlon = nproj * radlon
!
! =========
     rsoa2 = tan( (hfpi-blat*nproj)*0.5 )
     x2py2 = ( rsoa2 * a2 ) ** 2.0
     blon = mod(blon,twopi)
     if(blon.lt.0.e0) blon = twopi + blon
     rlon = nproj * (blon - dlamda0)
     rlon = amod(rlon,twopi)
     if( rlon.lt.0. ) rlon=twopi+rlon
     yox = tan(rlon)
     x = sqrt( x2py2/(1.+yox*yox) )
     y = sqrt( x2py2 - x*x )
     if( rlon.gt.hfpi .and. rlon.lt. pi+hfpi ) x = -x
     if( rlon.gt.pi .and. rlon.lt. twopi ) y = -y
!
   else if ( nproj.eq.0 ) then
!
! ++++++++++++++++++++++++++++
! do mercater
! ++++++++++++++++++++++++++++
     truth  = ctruth * rad
     cenlon = corient * rad
     cenlon = mod(cenlon,twopi)
     if(cenlon.lt.0.e0) cenlon = twopi + cenlon
     a2 =  rerth * cos( truth )
     dlamda0 = 0.0
!
     blon = mod(blon,twopi)
     if(blon.lt.0.e0) blon = twopi + blon
     x=a2*(blon-cenlon)
     y=a2*log(tan(blat/2.0+qtpi))
!
   else if( nproj.eq.2 .or. nproj.eq.-2 ) then
!
! ++++++++++++++++++++++++++++
! do lambert
! ++++++++++++++++++++++++++++
     is=1
     if( nproj.lt.0 ) is=-1
     truth  = ctruth * rad
     cotru  = ccotru * rad
     cenlon = corient * rad
     cenlon = mod(cenlon,twopi)
     if(cenlon.lt.0.e0) cenlon = twopi + cenlon
     if( ctruth.eq.ccotru ) then
       cone= cos (hfpi-is*truth)
     else
       cone=(log(cos(truth))-log(cos(cotru)))/                                 &
            (log(tan(qtpi-is*truth/2))-log(tan(qtpi-is*cotru/2)))
     endif
     dlamda0 = 0.0
     r00=rerth/cone*cos(truth)/(tan(qtpi-is*truth/2))**cone
!
     blon = mod(blon,twopi)
     if(blon.lt.0.e0) blon = twopi + blon
     r=r00*(tan(qtpi-is*blat/2))**cone
     x=    r*sin(cone*(blon-cenlon))
     y=-is*r*cos(cone*(blon-cenlon))
!
   endif
!
   cx = x
   cy = y
!
   return
   end
   !
   subroutine xy2ll(cproj,corient,ctruth,ccotru,cx,cy,clat,clon)
!
   real,parameter       ::  pi=3.14159,twopi=2.0*pi,hfpi=0.5*pi,qtpi=0.5*hfpi
   real,parameter       ::  rad=pi/180.,rerth=6371220.
!
! input are all degree and output to global x y, not domain x y
!
! if proj=0  do mercater projection
! if proj=1  do north polar projection
! if proj=-1 do south polar projection
! if proj=2  do north lambert projection
! if proj=-2 do south lambert projection
!
   x = cx
   y = cy
   nproj = cproj
!
   if( nproj.eq.1 .or. nproj.eq.-1 ) then
! ++++++++++++++++++++++++++++++++++++++
! polar projection
! ++++++++++++++++++++++++++++++++++++++
     truth  = ctruth * rad
     truth  = nproj * truth
     orient  = corient * rad
     cenlon = mod(orient,twopi)
     if(cenlon.lt.0.e0) cenlon = twopi + cenlon
     dlamda0 = cenlon + hfpi
     a2 =  rerth * ( 1.0 + sin(truth) )
     radlat = 90. * rad
     radlon = 0.0 * rad - dlamda0
     radlat = nproj * radlat
     radlon = nproj * radlon
!
! =========
     if( x.gt.0.0 ) then
       blon = atan(y/x)
     else if( x.lt.0.0 ) then
       blon = pi + atan(y/x)
     else
       blon = hfpi
       if( y.lt.0.0 ) blon = blon * 3.0
     endif
     blon = blon + dlamda0
     blon = mod(blon,twopi)
     blon = nproj * blon
     rsoa2 = sqrt( x*x + y*y )/a2
     blat = hfpi - 2. * atan(rsoa2)
!
   else if ( nproj.eq.0 ) then
!
! ++++++++++++++++++++++++++++
! do mercater
! ++++++++++++++++++++++++++++
     truth  = ctruth * rad
     cenlon = corient * rad
     cenlon = mod(cenlon,twopi)
     if(cenlon.lt.0.e0) cenlon = twopi + cenlon
     a2 =  rerth * cos( truth )
     dlamda0 = 0.0
!
     blon = x / a2 + cenlon
     blon = mod(blon,twopi)
     if(blon.lt.0.e0) blon = twopi + blon

     blat = 2.*(atan(exp(y/a2))-qtpi)
!
   else if( nproj.eq.2 .or. nproj.eq.-2 ) then
!
! ++++++++++++++++++++++++++++
! do lambert
! ++++++++++++++++++++++++++++
     is=1
     if( nproj.lt.0 ) is=-1
     truth  = ctruth * rad
     cotru  = ccotru * rad
     cenlon = corient * rad
     cenlon = mod(cenlon,twopi)
     if(cenlon.lt.0.e0) cenlon = twopi + cenlon
     if( ctruth.eq.ccotru ) then
       cone= cos (hfpi-is*truth)
     else
       cone=(log(cos(truth))-log(cos(cotru)))/                                 &
            (log(tan(qtpi-is*truth/2))-log(tan(qtpi-is*cotru/2)))
     endif
     dlamda0 = 0.0
     r00=rerth/cone*cos(truth)/(tan(qtpi-is*truth/2))**cone
!
     r = sqrt( x*x + y*y )
     blon = cenlon + asin(x/r) / cone
     blon = mod(blon,twopi)
     if(blon.lt.0.e0) blon = twopi + blon
     blat = hfpi - 2 * is * atan ( (r/r00)**(1./cone) )
!
   endif
!
! output in degree
   clat = blat / rad
   clon = blon / rad
!
   return
   end
   !
   !
   subroutine gaulat(gaulb,k)
!
   implicit double precision (a-h,o-z)
   real                 ::  a(k)
   real                 ::  gaulb(k)
!
   esp=1.e-14
   c=(1.e0-(2.e0/3.14159265358979e0)**2)*0.25e0
   fk=k
   kk=k/2
   call bsslz1(a,kk)
   do 30 is=1,kk
   xz=cos(a(is)/sqrt((fk+0.5e0)**2+c))
   iter=0
   10 pkm2=1.e0
   pkm1=xz
   iter=iter+1
   if(iter.gt.10) go to 70
   do 20 n=2,k
   fn=n
   pk=((2.e0*fn-1.e0)*xz*pkm1-(fn-1.e0)*pkm2)/fn
   pkm2=pkm1
   20 pkm1=pk
   pkm1=pkm2
   pkmrk=(fk*(pkm1-xz*pk))/(1.e0-xz**2)
   sp=pk/pkmrk
   xz=xz-sp
   avsp=abs(sp)
   if(avsp.gt.esp) go to 10
   a(is)=xz
   30 continue
   if(k.eq.kk*2) go to 50
   a(kk+1)=0.e0
   pk=2.e0/fk**2
   do 40 n=2,k,2
   fn=n
   40 pk=pk*fn**2/(fn-1.e0)**2
   50 continue
   do 60 n=1,kk
   l=k+1-n
   a(l)=-a(n)
   60 continue
!
   radi=180./(4.*atan(1.))
   do 211 n=1,k
   gaulb(n)=acos(a(n))*radi
  211 continue
!
!     print *,'gaussian lat (deg) for jmaxb=',k
!     print *,(gaulb(n),n=1,k)
!
   return
   70 write(6,6000)
 6000 format(//5x,14herror in gauaw//)
   end
   !

   subroutine bsslz1(bes,n)
   implicit double precision (a-h,o-z)
   save
!
   real                 ::  bes(n)
   real                 ::  bz(50)
!
   data pi/3.14159265358979e0/
   data bz         / 2.4048255577e0, 5.5200781103e0,                           &
     8.6537279129e0,11.7915344391e0,14.9309177086e0,18.0710639679e0,           &
    21.2116366299e0,24.3524715308e0,27.4934791320e0,30.6346064684e0,           &
    33.7758202136e0,36.9170983537e0,40.0584257646e0,43.1997917132e0,           &
    46.3411883717e0,49.4826098974e0,52.6240518411e0,55.7655107550e0,           &
    58.9069839261e0,62.0484691902e0,65.1899648002e0,68.3314693299e0,           &
    71.4729816036e0,74.6145006437e0,77.7560256304e0,80.8975558711e0,           &
    84.0390907769e0,87.1806298436e0,90.3221726372e0,93.4637187819e0,           &
    96.6052679510e0,99.7468198587e0,102.888374254e0,106.029930916e0,           &
    109.171489649e0,112.313050280e0,115.454612653e0,118.596176630e0,           &
    121.737742088e0,124.879308913e0,128.020877005e0,131.162446275e0,           &
    134.304016638e0,137.445588020e0,140.587160352e0,143.728733573e0,           &
    146.870307625e0,150.011882457e0,153.153458019e0,156.295034268e0/
   !
   nn=n
   if(n.le.50) go to 12
   bes(50)=bz(50)
   do 5 j=51,n
    5 bes(j)=bes(j-1)+pi
   nn=49
   12 do 15 j=1,nn
   15 bes(j)=bz(j)
   return
   end
