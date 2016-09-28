#undef STREAM
#undef DBG
#undef STATION
    program sigphys
#include <machine.h>
#ifndef MAC
!
!  program to calculate u, v, Tv, height, q and ps at a given lat/lon
!  from sigma file
!
   real,parameter       ::  g_=9.8000e+0,rd_=2.8705e+2,rv_=4.6150e+2
   real,parameter       ::  hvap_=2.5000e+6,cp_=1.0046e+3
   real,parameter       ::  fv_=rv_/rd_-1.
   real, allocatable    ::  ppnm(:),hhnm(:)
   real, allocatable    ::  si(:),sl(:)
   real, allocatable    ::  gz(:)
   real, allocatable    ::  ps(:)
   real, allocatable    ::  tv(:,:),tvpt(:), dtvdxpt(:), dtvdypt(:)
   real, allocatable    ::  di(:,:),dipt(:), ddidxpt(:), ddidypt(:)
   real, allocatable    ::  ze(:,:),zept(:), dzedxpt(:), dzedypt(:)
   real, allocatable    ::  rq(:,:),rqpt(:), drqdxpt(:), drqdypt(:)
   real, allocatable    ::  zz(:,:),zzpt(:), dzzdxpt(:), dzzdypt(:)
   real, allocatable    ::  tpt(:),  dtdxpt(:),  dtdypt(:)
   real, allocatable    ::  ppt(:)
   real, allocatable    ::  upt(:),  dudxpt(:),  dudypt(:)
   real, allocatable    ::  vpt(:),  dvdxpt(:),  dvdypt(:)
   real, allocatable    ::  dpsidxpt(:),dpsidypt(:)
   real, allocatable    ::  dchidxpt(:),dchidypt(:)
   real, allocatable    ::  ompt(:)
   real, allocatable    ::  zpt(:)
   real, allocatable    ::  zipt(:)
   real, allocatable    ::  zeflxdiv(:)
   real, allocatable    ::  rqflxdiv(:)
   real, allocatable    ::  tflxdiv(:)
   real, allocatable    ::  zzflxdiv(:)
   real*4, allocatable  ::  ppt4(:),zpt4(:),upt4(:),vpt4(:),tvpt4(:)
   real*4, allocatable  ::  tpt4(:),rqpt4(:),dipt4(:),zept4(:),ompt4(:)
   real*4, allocatable  ::  zeflxdiv4(:),tflxdiv4(:)
   real*4, allocatable  ::  rqflxdiv4(:),zzflxdiv4(:)
!
   real*4, allocatable  ::  buf2(:)
   real*4               ::  r4rdlat,r4rdlon,r4tim
   integer*4            ::  nlev,nflag,idev,nbytes,nwrit
!
   real*4               ::  gzpt4,pspt4,totzeflx4,tottflx4,totrqflx4,totzzflx4
!
   real*4               ::  buf1(7)
   character(len=8)     ::  stid
   equivalence(stid,buf1(1))
   equivalence(r4rdlat,buf1(3))
   equivalence(r4rdlon,buf1(4))
   equivalence(r4tim,buf1(5))
   equivalence(nlev,buf1(6))
   equivalence(nflag,buf1(7))
!
   character(len=120)   ::  filename
   integer              ::  idate(4)
   character(len=8)     ::  filefmt
!
   integer,parameter    ::  n1=1,n2=52,n3=51,n4=53
   data inchr/24/
   data stid/'dummy'/
   data filefmt/' '/
!
   namelist /namdat/ jcap,levs,stid,rdlat,rdlon,inchr,filefmt
!
   pi=4.*atan(1.)
!
   read(5,namdat)
   write(*,'("jcap=",i3," levs=",i2," lat=",f6.2," lon=",f7.2)')               &
              jcap,levs,rdlat,rdlon
   write(n2,'("jcap=",i3," levs=",i2," lat=",f6.2," lon=",f7.2)')              &
              jcap,levs,rdlat,rdlon
   if(jcap.le.0.or.levs.le.0) then
     print *,'negative resolution'
     call abort
   endif
!
   nflds=6+levs*14
   stid(8:8)='\0'
!
! allocate arrays
!
   lnt2=(jcap+1)*(jcap+2)
   allocate (ppnm(lnt2),hhnm(lnt2))
   allocate (si(levs+1),sl(levs))
   allocate (gz(lnt2))
   allocate (ps(lnt2))
   allocate (tv(lnt2,levs),tvpt(levs), dtvdxpt(levs), dtvdypt(levs))
   allocate (di(lnt2,levs),dipt(levs), ddidxpt(levs), ddidypt(levs))
   allocate (ze(lnt2,levs),zept(levs), dzedxpt(levs), dzedypt(levs))
   allocate (rq(lnt2,levs),rqpt(levs), drqdxpt(levs), drqdypt(levs))
   allocate (zz(lnt2,levs),zzpt(levs), dzzdxpt(levs), dzzdypt(levs))
   allocate (               tpt(levs),  dtdxpt(levs),  dtdypt(levs))
   allocate (               ppt(levs)                              )
   allocate (               upt(levs),  dudxpt(levs),  dudypt(levs))
   allocate (               vpt(levs),  dvdxpt(levs),  dvdypt(levs))
   allocate (                         dpsidxpt(levs),dpsidypt(levs))
   allocate (                         dchidxpt(levs),dchidypt(levs))
   allocate (              ompt(levs)                              )
   allocate (               zpt(levs)                              )
   allocate (              zipt(levs)                              )
   allocate (          zeflxdiv(levs)                              )
   allocate (           tflxdiv(levs)                              )
   allocate (          rqflxdiv(levs)                              )
   allocate (          zzflxdiv(levs)                              )
   allocate(ppt4(levs),zpt4(levs),upt4(levs),vpt4(levs),tvpt4(levs))
   allocate(tpt4(levs),rqpt4(levs),dipt4(levs))
   allocate(zept4(levs),ompt4(levs))
   allocate(zeflxdiv4(levs),tflxdiv4(levs))
   allocate(rqflxdiv4(levs),zzflxdiv4(levs))
!
   allocate (buf2(nflds))
!
   tim=0.
   nlev=levs+1
#ifdef STREAM
   call sfopen(idev,1,'stn.dat')
   if(idev.lt.0) then
     print *,'sfopen failed.'
     call abort
   endif
!     print *,'idev=',idev
#else
   open(unit=n3,file='stn.dat',status='unknown',form='unformatted')
#endif
   r4rdlat=rdlat
   r4rdlon=rdlon
   r4tim  =tim
!
   rlat=rdlat*pi/180.
   rlon=rdlon*pi/180.
   colrad=pi*0.5-rlat
!
!  compute legender polynomials
!
   call rpln2i(jcap,ppnm,hhnm,colrad)
!
!  loop for multiple files
!
   open(unit=n2,file='stnval.dat',status='unknown',                            &
        form='formatted')
   nc=0
   nfiles=0
   do while(nc.eq.0)
     read(5,'(A120)',end=1) filename
!
!  read binary sigma spectral coefficient file
!
     nfiles=nfiles+1
     open(unit=n1,file=filename,status='old',form='unformatted')
     call scnvrt(n1,jcap,levs,filefmt,                                         &
                fhour,idate,si,sl,gz,ps,tv,di,ze,rq)
     write( 6,'(" fhr=",f6.1," idate=",i2,2i3,i5," fnam=",a30)')               &
                 fhour,idate,filename
     write(n2,'(" fhr=",f6.1," idate=",i2,2i3,i5," fnam=",a120)')              &
                 fhour,idate,filename
     write(n2,'("si:",100f8.5)') si
     write(n2,'("sl:",100f8.5)') sl
!
!  compute spectral coeff of height
!
     call hydrosp(lnt2,levs,si,sl,gz,tv,zz)
!
!  compute gz at given lat/lon
!
     call sphpt1(jcap,1,gz,gzpt,rlon,ppnm)
     write(n2,'("gz=",f6.1)') gzpt
#ifdef DBG
     write(*,'("gz=",f6.1)') gzpt
#endif
!
!  compute log of ps and their horizontal derivatives on lat/lon
!  also computes ps in mb unit
!
     call sphpt1(jcap,1,ps,pspt,rlon,ppnm)
     call sphpt2(jcap,1,ps,dpsdxpt,dpsdypt,rlon,ppnm,hhnm)
     pspt=exp(pspt)*10.
     write(n2,'("ps=",f6.1)') pspt
#ifdef DBG
     write(*,'("ps=",f6.1)') pspt
#endif
!
!  compute pressure at model levels
!
     do k=1,levs
       ppt(k)=pspt*sl(k)
     enddo
     write(n2,'("p=",100f6.1)') (ppt(k),k=1,levs)
!
!  compute virtual temperature at given lat/lon
!
     call sphpt1(jcap,levs,tv,tvpt,rlon,ppnm)
     write(n2,'("tv=",100f6.1)') (tvpt(k),k=1,levs)
#ifdef DBG
     write(*,'("tv=",100f6.1)') (tvpt(k),k=1,levs)
#endif
!
!  compute divergence at given lat/lon
!
     call sphpt1(jcap,levs,di,dipt,rlon,ppnm)
     write(n2,'("div(10**5)=",5p100f7.3)') (dipt(k),k=1,levs)
#ifdef DBG
     write(*,'("div(10**5)=",5p100f7.3)') (dipt(k),k=1,levs)
#endif
!       call sphpt2(jcap,levs,di,ddidxpt,ddidypt,rlon,ppnm,hhnm)
!
!  compute vorticity at given lat/lon
!
     call sphpt1(jcap,levs,ze,zept,rlon,ppnm)
     write(n2,'("vot(10**5)=",5p100f7.3)') (zept(k),k=1,levs)
#ifdef DBG
     write(*,'("vot(10**5)=",5p100f7.3)') (zept(k),k=1,levs)
#endif
     call sphpt2(jcap,levs,ze,dzedxpt,dzedypt,rlon,ppnm,hhnm)
!
!  convert divergence/vorticity coeff to streamfunction, velocity
!  potential coefficients, and computes their horizontal derivatives
!  at given lat/lon
!
     call psichi(jcap,levs,di,ze)
     call sphpt2(jcap,levs,ze,dpsidxpt,dpsidypt,rlon,ppnm,hhnm)
     call sphpt2(jcap,levs,di,dchidxpt,dchidypt,rlon,ppnm,hhnm)
!
!  computes u and v as a sum of rotational and divergent parts
!
     do k=1,levs
       upt(k)=(-dpsidypt(k)+dchidxpt(k))/cos(rlat)
       vpt(k)=( dpsidxpt(k)+dchidypt(k))/cos(rlat)
     enddo
     write(n2,'("u=",100f6.1)') (upt(k),k=1,levs)
     write(n2,'("v=",100f6.1)') (vpt(k),k=1,levs)
#ifdef DBG
     write(*,'("u=",100f6.1)') (upt(k),k=1,levs)
     write(*,'("v=",100f6.1)') (vpt(k),k=1,levs)
#endif
!
!  compute spefic humidity at given lat/lon
!
     call sphpt1(jcap,levs,rq,rqpt,rlon,ppnm)
     write(n2,'("q(10**3)=",3p100f6.2)') (rqpt(k),k=1,levs)
#ifdef DBG
     write(*,'("q(10**3)=",3p100f6.2)') (rqpt(k),k=1,levs)
#endif
     call sphpt2(jcap,levs,rq,drqdxpt,drqdypt,rlon,ppnm,hhnm)
!
!  compute thermodynamic temperature and its horizontal derivatives
!
     call sphpt2(jcap,levs,tv,dtvdxpt,dtvdypt,rlon,ppnm,hhnm)
     do k=1,levs
       tpt(k)=tvpt(k)/(1.+fv_*rqpt(k))
       dtdxpt(k)=(dtvdxpt(k)-tpt(k)*fv_*drqdxpt(k))/(1+fv_*rqpt(k))
       dtdypt(k)=(dtvdypt(k)-tpt(k)*fv_*drqdypt(k))/(1+fv_*rqpt(k))
     enddo
     write(n2,'("t=",100f6.1)') (tpt(k),k=1,levs)
#ifdef DBG
     write(*,'("t=",100f6.1)') (tpt(k),k=1,levs)
#endif
!
!  compute geopotential height and its horizontal derivatives
!
     call sphpt1(jcap,levs,zz,zzpt,rlon,ppnm)
     call sphpt2(jcap,levs,zz,dzzdxpt,dzzdypt,rlon,ppnm,hhnm)
!
!  compute geopotential height using point values of t,q and gz
!
   call hydro(levs,si,sl,gzpt,tpt,rqpt,zpt,zipt)
   write(n2,'("z=",100f8.1)') (zpt(k),k=1,levs)
#ifdef DBG
   write(*,'("z=",100f8.1)') (zpt(k),k=1,levs)
#endif
!
!  compute vertical pressure velocity
!
   call omega(levs,si,sl,ps,dpsdxpt,dpsdypt,dipt,upt,vpt,ompt)
   do k=1,levs
     ompt(k)=ompt(k)*10.
   enddo
   write(n2,'("omega=",2p100f7.2)') (ompt(k),k=1,levs)
#ifdef DBG
   write(*,'("omega=",2p100f7.2)') (ompt(k),k=1,levs)
#endif
!
!  computes flux divergence of vorticity, temperature and specific humidity
!
     do k=1,levs
       zeflxdiv(k)=upt(k)*dzedxpt(k)+vpt(k)*dzedypt(k)+                        &
                   zept(k)*dipt(k)
       zeflxdiv(k)=zeflxdiv(k)*(si(k)-si(k+1))
       tflxdiv(k)=upt(k)* dtdxpt(k)+vpt(k)* dtdypt(k)+ tpt(k)*dipt(k)
       tflxdiv(k)=tflxdiv(k)*(si(k)-si(k+1))
       rqflxdiv(k)=upt(k)*drqdxpt(k)+vpt(k)*drqdypt(k)+                        &
                   rqpt(k)*dipt(k)
       rqflxdiv(k)=rqflxdiv(k)*(si(k)-si(k+1))
       zzflxdiv(k)=upt(k)*dzzdxpt(k)+vpt(k)*dzzdypt(k)+                        &
                   zzpt(k)*dipt(k)
        zzflxdiv(k)=zzflxdiv(k)*(si(k)-si(k+1))
     enddo
!
!  vertical integrals of flux divergences
!
     totzeflx=0.
      tottflx=0.
     totrqflx=0.
     totzzflx=0.
     do k=1,levs
       totzeflx=totzeflx+zeflxdiv(k)
        tottflx= tottflx+ tflxdiv(k)
       totrqflx=totrqflx+rqflxdiv(k)
       totzzflx=totrqflx+rqflxdiv(k)
     enddo
!
!  factor 100 is the conversion to watts/m**2
!
     totzeflx=totzeflx*pspt/g_
      tottflx= tottflx*pspt/g_*cp_*100.
     totrqflx=totrqflx*pspt/g_*hvap_*100.
     totzzflx=totzzflx*pspt
!
     write(n2,'("totzeflx(10**9)=",  9pf7.2)') totzeflx
     write(n2,'(" tottflx(10**-2)=",-2pf9.2)') tottflx
     write(n2,'("totrqflx(10**-2)=",-2pf8.2)') totrqflx
     write(n2,'("totrqflx(10**-2)=",-2pf8.2)') totzzflx
     write(n2,'(" ")')
     write(n2,'(" ")')
#ifdef DBG
     write(*,'("totzeflx(10**9)=",  9pf7.2)') totzeflx
     write(*,'(" tottflx(10**-2)=",-2pf9.2)') tottflx
     write(*,'("totrqflx(10**-2)=",-2pf8.2)') totrqflx
     write(*,'("totzzflx(10**-2)=",-2pf8.2)') totzzflx
#endif
!
!  write grads records
!
!  header
!
     nlev=levs+1
     nflag=1
!
#ifdef STATION
#ifdef STREAM
       nbytes=7*4
       call sfput(idev,nbytes,buf1,nwrit)
       if(nbytes.ne.nwrit) then
         print *,'sfput error. number of bytes written=',nwrit,                &
               ' mumber of bytes to be written=',nbytes
         call abort
       endif
!         print *,'nlev=',nlev,'  ',nwrit,' bytes written'
#else
       write(n3) stid,r4rdlat,r4rdlon,r4tim,nlev,nflag
#endif
#endif
!
!  body
!
     buf2(1)=gzpt
     buf2(2)=pspt
     buf2(3)=totzeflx
     buf2(4)=tottflx
     buf2(5)=totrqflx
     buf2(6)=totzzflx
     n=6
#ifdef STATION
     do k=1,levs
       buf2(n+1)=ppt(k)
       buf2(n+2)=zpt(k)
       buf2(n+3)=upt(k)
       buf2(n+4)=vpt(k)
       buf2(n+5)=tvpt(k)
       buf2(n+6)=tpt(k)
       buf2(n+7)=rqpt(k)
       buf2(n+8)=dipt(k)
       buf2(n+9)=zept(k)
       buf2(n+10)=ompt(k)
       buf2(n+11)=zeflxdiv(k)
       buf2(n+12)=tflxdiv(k)
       buf2(n+13)=rqflxdiv(k)
       buf2(n+14)=zzflxdiv(k)
       n=n+14
     enddo
#else
     n=n+1
     do k=1,levs
       buf2(n)=ppt(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=zpt(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=upt(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=vpt(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=tvpt(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=tpt(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=rqpt(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=dipt(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=zept(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=ompt(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=zeflxdiv(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=tflxdiv(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=rqflxdiv(k)
     enddo
     n=n+levs
     do k=1,levs
       buf2(n)=zzflxdiv(k)
     enddo
#endif
#ifdef STREAM
     nbytes=nflds*4
     call sfput(idev,nbytes,buf2,nwrit)
     if(nbytes.ne.nwrit) then
       print *,'sfput error. number of bytes written=',nwrit,                  &
               ' number of bytes to be written=',nbytes
       call abort
     endif
!       print *,'obs data. ',nwrit,' bytes written'
#else
#ifdef STATION
     write(n3) (buf2(i),i=1,nflds)
#else
     gzpt4=gzpt
     pspt4=pspt
     totzeflx4=totzeflx
     tottflx4=tottflx
     totrqflx4=totrqflx
     totzzflx4=totzzflx
     write(n3) gzpt4
     write(n3) pspt4
     write(n3) totzeflx4
     write(n3) tottflx4
     write(n3) totrqflx4
     write(n3) totzzflx4
     do k=1,levs
       ppt4(k)=ppt(k)
       zpt4(k)=zpt(k)
       upt4(k)=upt(k)
       vpt4(k)=vpt(k)
       tvpt4(k)=tvpt(k)
       tpt4(k)=tpt(k)
       rqpt4(k)=rqpt(k)
       dipt4(k)=dipt(k)
       zept4(k)=zept(k)
       ompt4(k)=ompt(k)
       zeflxdiv4(k)=zeflxdiv(k)
       tflxdiv4(k)=tflxdiv(k)
       rqflxdiv4(k)=rqflxdiv(k)
       zzflxdiv4(k)=zzflxdiv(k)
     enddo
     do k=1,levs
       write(n3) ppt4(k)
     enddo
     do k=1,levs
       write(n3) zpt4(k)
     enddo
     do k=1,levs
       write(n3) upt4(k)
     enddo
     do k=1,levs
       write(n3) vpt4(k)
     enddo
     do k=1,levs
       write(n3) tvpt4(k)
     enddo
     do k=1,levs
       write(n3) tpt4(k)
     enddo
     do k=1,levs
       write(n3) tpt4(k)
     enddo
     do k=1,levs
       write(n3) rqpt4(k)
     enddo
     do k=1,levs
       write(n3) dipt4(k)
     enddo
     do k=1,levs
       write(n3) zept4(k)
     enddo
     do k=1,levs
       write(n3) ompt4(k)
     enddo
     do k=1,levs
       write(n3) zeflxdiv4(k)
     enddo
     do k=1,levs
       write(n3) tflxdiv4(k)
     enddo
     do k=1,levs
       write(n3) rqflxdiv4(k)
     enddo
     do k=1,levs
       write(n3) zzflxdiv4(k)
     enddo
#endif
#endif
!
! tail end
!
     nlev=0
     nflag=0
#ifdef STATION
#ifdef STREAM
     nbytes=7*4
     call sfput(idev,nbytes,buf1,nwrit)
     if(nbytes.ne.nwrit) then
       print *,'sfput error. number of bytes written=',nwrit,                  &
               ' mumber of bytes to be written=',nbytes
       call abort
     endif
!       print *,'nlev=',nlev,'  ',nwrit,' bytes written'
#else
     write(n3) stid,r4rdlat,r4rdlon,r4tim,nlev,nflag
#endif
#endif
   enddo
    1 continue
#ifdef STATION
#ifdef STREAM
     nbytes=7*4
     call sfput(idev,nbytes,buf1,nwrit)
     if(nbytes.ne.nwrit) then
       print *,'sfput error. number of bytes written=',nwrit,                  &
               ' mumber of bytes to be written=',nbytes
       call abort
     endif
!       print *,'nlev=',nlev,'  ',nwrit,' bytes written'
     call sfclose(idev,ier)
     if(ier.ne.0) then
       print *,'sfclose error'
     endif
#else
     write(n3) stid,r4rdlat,r4rdlon,r4tim,nlev,nflag
#endif
#endif
!
#ifdef STREAM
     call sfclose(idev,ier)
     if(ier.ne.0) then
       print *,'sfclose error'
     endif
#endif
!
!  grads control file
!
   call mkctl(fhour,idate,nfiles,inchr,n4)
!
   stop
   end
   !
#ifdef STATION
   subroutine mkctl(fhour,idate,nfiles,inchr,nunit)
   character(len=12)    ::  hdmy
   character(len=3)     ::  mon(12)
   integer              ::  idate(4)
!
   data mon/'jan','feb','mar','apr','may','jun','jul','aug','sep',             &
            'oct','nov','dec'/
!
   if(idate(4).lt.100) idate(4)=idate(4)+1900
   if(idate(3).ge.10) then
     write(hdmy,'(i2,"z",i2,a3,i4)')                                           &
           idate(1),idate(3),mon(idate(2)),idate(4)
   else
     write(hdmy,'(i2,"z",i1,a3,i4)')                                           &
           idate(1),idate(3),mon(idate(2)),idate(4)
   endif
!
   nvars=19
   open(unit=nunit,file='stn.ctl',status='unknown',form='formatted')
   write(nunit,'("dset ^stn.dat")')
   write(nunit,'("dtype station")')
#ifndef STREAM
   write(nunit,'("options sequential big_endian")')
#endif
   write(nunit,'("stnmap ^stn.map")')
   write(nunit,'("undef -999.0")')
   write(nunit,'("title point values from global spectral coeffs")')
   write(nunit,'("tdef ",i6," linear ",a12,i4,"hr")')                          &
                  nfiles,hdmy,inchr
   write(nunit,'("vars ",i3)') nvars
   write(nunit,'("gz    0 99 surface geopotential height(m)")')
   write(nunit,'("ps    0 99 surface pressure(mb)")')
   write(nunit,'("zeflx 0 99 total column vorticity flux(N/m**3)")')
   write(nunit,'("tflx  0 99 total column heat flux(w/m**2)")')
   write(nunit,'("qflx  0 99 total column moisture flux(w/m**2)")')
   write(nunit,'("zflx  0 99 total column gz flux(w/m**2)")')
   write(nunit,'("z     1 99 easopotential height(m)")')
   write(nunit,'("u     1 99 east-west component of wind(m/s)")')
   write(nunit,'("v     1 99 north-south component of wind(m/s)")')
   write(nunit,'("tv    1 99 virtual temperature(K)")')
   write(nunit,'("t     1 99 temperature(K)")')
   write(nunit,'("q     1 99 specific humidity(gm/gm)")')
   write(nunit,'("div   1 99 divergence(1/s)")')
   write(nunit,'("vor   1 99 vorticity(1/s)")')
   write(nunit,'("omega 1 99 vertical pressure velocity(mb/s)")')
   write(nunit,'("votflx 1 99 layer weighted vorticity flux(/s2)")')
   write(nunit,'("tdflx 1 99 layer weighted temperature flx(K/s2)")')
   write(nunit,'("qdflx 1 99 layer weighted moisture flx (1/s2)")')
   write(nunit,'("zdflx 1 99 layer weighted gz flx (m/s3)")')
   write(nunit,'("endvars")')
!
   return
   end
#else
   subroutine mkctl(fhour,idate,nfiles,inchr,nunit)
   character(len=12)    ::  hdmy
   character(len=3)     ::  mon(12)
   integer              ::  idate(4)
!
   data mon/'jan','feb','mar','apr','may','jun','jul','aug','sep',             &
            'oct','nov','dec'/
!
   if(idate(4).lt.100) idate(4)=idate(4)+1900
   if(idate(3).ge.10) then
     write(hdmy,'(i2,"z",i2,a3,i4)')                                           &
           idate(1),idate(3),mon(idate(2)),idate(4)
   else
     write(hdmy,'(i2,"z",i1,a3,i4)')                                           &
           idate(1),idate(3),mon(idate(2)),idate(4)
   endif
!
   nvars=20
   open(unit=nunit,file='stn.ctl',status='unknown',form='formatted')
   write(nunit,'("dset ^stn.dat")')
#ifndef STREAM
   write(nunit,'("options sequential big_endian")')
#endif
   write(nunit,'("undef -999.0")')
   write(nunit,'("title point values from global spectral coeffs")')
   write(nunit,'("xdef 1 linear 0.000 1.000")')
   write(nunit,'("ydef 1 linear 0.000 1.000")')
   write(nunit,'("zdef 28 linear 1.000 1.000")')
   write(nunit,'("tdef ",i6," linear ",a12,i4,"hr")')nfiles,hdmy,inchr
   write(nunit,'("vars ",i3)') nvars
   write(nunit,'("gz    1 99 surface geopotential height(m)")')
   write(nunit,'("ps    1 99 surface pressure(mb)")')
   write(nunit,'("zeflx 1 99 total column vorticity flux(N/m**3)")')
   write(nunit,'("tflx  1 99 total column heat flux(w/m**2)")')
   write(nunit,'("qflx  1 99 total column moisture flux(w/m**2)")')
   write(nunit,'("zflx  1 99 total column gz flux(w/m**2)")')
   write(nunit,'("p     28 99 pressure (mb)")')
   write(nunit,'("z     28 99 easopotential height(m)")')
   write(nunit,'("u     28 99 east-west component of wind(m/s)")')
   write(nunit,'("v     28 99 north-south component of wind(m/s)")')
   write(nunit,'("tv    28 99 virtual temperature(K)")')
   write(nunit,'("t     28 99 temperature(K)")')
   write(nunit,'("q     28 99 specific humidity(gm/gm)")')
   write(nunit,'("div   28 99 divergence(1/s)")')
   write(nunit,'("vor   28 99 vorticity(1/s)")')
   write(nunit,'("omega 28 99 vertical pressure velocity(mb/s)")')
   write(nunit,'("votflx 28 99 layer weighted vorticity flux(/s2)")')
   write(nunit,'("tdflx 28 99 layer wghted temperature flx(K/s2)")')
   write(nunit,'("qdflx 28 99 layer weighted moisture flx (1/s2)")')
   write(nunit,'("zdflx 28 99 layer weighted gz flx (m/s3)")')
   write(nunit,'("endvars")')
!
   return
   end
#endif
   subroutine psichi(jcap,levs,di,ze)
!
   real                 ::  di((jcap+1)*(jcap+2),levs)
   real                 ::  ze((jcap+1)*(jcap+2),levs)
   real                 ::  cof((jcap+1)*(jcap+2))
   real,parameter       ::  er_=6.3712e06
!
   i=2
   cof(1)=0.
   cof(2)=0.
!     do nm=1,jcap
!       do m=0,jcap-nm
!         n=nm+m
!         cof(i+1)=-er_*er_/float(n*(n+1))
!         cof(i+2)=cof(i+1)
!         i=i+2
!       enddo
!     enddo
   i=0
   do m=0,jcap
     do n=m,jcap
       if(n.ne.0) then
         cof(i+1)=-er_*er_/float(n*(n+1))
         cof(i+2)=cof(i+1)
       else
         cof(i+1)=0.
         cof(i+2)=0.
       endif
       i=i+2
     enddo
   enddo
!
   do k=1,levs
     do i=1,(jcap+1)*(jcap+2)
       di(i,k)=di(i,k)*cof(i)
       ze(i,k)=ze(i,k)*cof(i)
     enddo
   enddo
!
   return
   end
   subroutine hydro(km,si,sl,zs,t,q,z,zi)
!
!
! usage:    call hydro(km,si,sl,zs,t,q,z,zi)
!
!   input argument list:
!     km       - integer number of levels
!     si       - real (km+1) sigma interface values
!     sl       - real (km) sigma values
!     zs       - real orography is m
!     t        - real (km) temperature in k
!     q        - real (km) specific humidity in kg/kg
!
!   output argument list:
!     z        - real (km) heights on the full levels in m
!     zi       - real (km) heights on the interfaces in m
!
! attributes:
!   language: cray fortran
!
!$$$
   real                 ::  si(km+1),sl(km),t(km),q(km)
   real                 ::  z(km),zi(km)
   real,parameter       ::  g= 9.8000e+0 ,rd= 2.8705e+2 ,rv= 4.6150e+2
   real,parameter       ::  rog=rd/g,fvirt=rv/rd-1.
!
   zi(1)=zs
!
   do k=1,km-1
     ca=rog*log(si(k)/sl(k))
     cb=rog*log(sl(k)/si(k+1))
     tv=t(k)*(1.+fvirt*q(k))
     z(k)=zi(k)+ca*tv
     zi(k+1)=z(k)+cb*tv
   enddo
!
   ca=rog*log(si(km)/sl(km))
   tv=t(km)*(1.+fvirt*q(km))
   z(km)=zi(km)+ca*tv
!
   return
   end
   subroutine omega(km,si,sl,ps,psx,psy,d,u,v,o)
!
! usage:    call omega(km,si,sl,ps,psx,psy,d,u,v,o)
!
!   input argument list:
!     km       - integer number of levels
!     si       - real (km+1) sigma interface values
!     sl       - real (km) sigma values
!     ps       - real surface pressure in kpa
!     psx      - real zonal gradient of log pressure in 1/m
!     psy      - real merid gradient of log pressure in 1/m
!     d        - real (km) divergence in 1/s
!     u        - real (km) zonal wind in m/s
!     v        - real (km) merid wind in m/s
!
!   output argument list:
!     o        - real (ix,km) pressure vertical velocity in pa/s
!
! attributes:
!   language: cray fortran
!
   real                 ::  si(km+1),sl(km)
   real                 ::  d(km),u(km),v(km),o(km)
!
   sum=0.
   do k=km,1,-1
     vgradp=u(k)*psx+v(k)*psy
     gradpv=vgradp+d(k)
     sum=sum+gradpv*(sl(k)-si(k+1))
     o(k)=(vgradp*sl(k)-sum)*ps*1.e3
     sum=sum+gradpv*(si(k)-sl(k))
   enddo
!
   return
   end
   !
   subroutine hydrosp(im,km,si,sl,zs,tv,z)
#ifdef CRAY_THREAD
!fpp$ noconcur r
#endif
!$$$  subprogram documentation block
!
! subprogram:    hydrosp     calculate geopotential heights
!
! abstract: calculates spectral coeffs of geopotential heights
! on the sigma full levels as a function of orography, virtual temperature
!     dz=rd/g*tv*dlnp
!
! program history log:
!
! usage:    call hydro(im,km,si,sl,zs,tv,q,z)
!
!   input argument list:
!     im       - integer number of coefficients
!     km       - integer number of levels
!     si       - real (km+1) sigma interface values
!     sl       - real (km) sigma values
!     zs       - real (im) orography in m
!     tv       - real (im,km) vertual temperature in k
!
!   output argument list:
!     z        - real (im,km) heights on the full levels in m
!
! attributes:
!   language: cray fortran
!
   real,parameter       ::  g_=9.8000e+0,rd_=2.8705e+2
   real,parameter       ::  rog_=rd_/g_
!$$$
   real                 ::  si(km+1),sl(km),zs(im),tv(im,km)
   real                 ::  z(im,km)
!
   real                 ::  zi(im,km)
!
   do i=1,im
     zi(i,1)=zs(i)
   enddo
!
   do k=1,km-1
     ca=rog_*log(si(k)/sl(k))
     cb=rog_*log(sl(k)/si(k+1))
     do i=1,im
       z(i,k)=zi(i,k)+ca*tv(i,k)
       zi(i,k+1)=z(i,k)+cb*tv(i,k)
     enddo
   enddo
!
   ca=rog_*log(si(km)/sl(km))
   do i=1,im
     z(i,km)=zi(i,km)+ca*tv(i,k)
   enddo
!
   return
   end

   subroutine scnvrt(nu,jcap,levs,flag,                                        &
                     fhour,idate,si,sl,gz,ps,tv,di,ze,rq)
!
!
! main program:  scnvrt    change data type to readable form
!   prgmmr: kanamitsu     org: w/np51     date: 03-05-01
!
! abstract: convert data
!
! input files:
!   unit   nu    file to read
!
! subprograms called:
!
! attributes:
!   language: fortran90
!
!$$$
!
   real                 ::  si(levs+1),sl(levs)
   real                 ::  gz((jcap+1)*(jcap+2)),ps((jcap+1)*(jcap+2))
   real                 ::  tv((jcap+1)*(jcap+2),levs)
   real                 ::  di((jcap+1)*(jcap+2),levs)
   real                 ::  ze((jcap+1)*(jcap+2),levs)
   real                 ::  rq((jcap+1)*(jcap+2),levs)
!
   character(len=8)     ::  on85lab(4)
   integer*4            ::  i4date(4)
   integer*4            ::  i8date(8)
   integer              ::  icdate(4)
   integer              ::  idate(4)
!
   real*4,allocatable   ::  r4si(:),r4sl(:)
   real*4,allocatable   ::  r4wave(:)
   real*4,allocatable   ::  r4dummy(:),r4dummy2(:),r4ensemble(:)
!
   real*8,allocatable   ::  r8si(:),r8sl(:)
   real*8,allocatable   ::  r8wave(:)
   real*8,allocatable   ::  r8dummy(:),r8dummy2(:),r8ensemble(:)
!
   real  ,allocatable   ::  rcsi(:),rcsl(:)
   real  ,allocatable   ::  rcwave(:)
   real  ,allocatable   ::  rcdummy(:),rcdummy2(:),rcensemble(:)
!
   real  ,allocatable   ::  wave(:)
   real  ,allocatable   ::  dummy(:),dummy2(:),ensemble(:)
!
   real*4               ::  r4fhour
   real*4               ::  r4waves,r4xlayers,r4trun
   real(4)              ::  r4order,r4realform,r4gencode
   real(4)              ::  r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4tracers
   real(4)              ::  r4subcen,r4ppid,r4slid,r4vcid,r4vmid,r4vtid
   real(4)              ::  r4runid,r4usrid,r4pdryini,r4clouds
!
   real*8               ::  r8fhour
   real*8               ::  r8waves,r8xlayers,r8trun
   real*8               ::  r8order,r8realform,r8gencode
   real*8               ::  r8rlond,r8rlatd,r8rlonp,r8rlatp,r8rlonr,r8rlatr,r8tracers
   real*8               ::  r8subcen,r8ppid,r8slid,r8vcid,r8vmid,r8vtid
   real*8               ::  r8runid,r8usrid,r8pdryini,r8clouds
!
   real                 ::  rcfhour
   real                 ::  rcwaves,rcxlayers,rctrun
   real                 ::  rcorder,rcrealform,rcgencode
   real                 ::  rcrlond,rcrlatd,rcrlonp,rcrlatp,rcrlonr,rcrlatr,rctracers
   real                 ::  rcsubcen,rcppid,rcslid,rcvcid,rcvmid,rcvtid
   real                 ::  rcrunid,rcusrid,rcpdryini,rcclouds
!
   character(len=8)     ::  flag
!
!  read flag
!   flag='ascii'     ...  ascii to binary conversion
!   flag='ieee'      ...  ieee f77 to binary conversion
!   flag='ieee_dp'   ...  ieee_dp f77 to binary conversion
!   flag='ieee_sgi'  ...  ieee_dp f77 for real but ieee for integer
!   flag='cray'      ...  format specified by the cray assign statement
!   flag=' '         ...  machine native binary (no conversion)
!
   write(6,*) jcap,ilatg,ilonf,levs,flag
!
   jcap=jcap
   kmax=levs
   nwave=(jcap+1)*(jcap+2)
   kdum=201-levs-1-levs
   kdum2=21
   kens=2
!
   if(flag(1:4).eq.'ieee'.and.flag(1:7).ne.'ieee_dp'.and.                      &
            flag(1:8).ne.'ieee_sgi') then
      flag(1:4)=flag(1:4)
      flag(5:8)='_reg'
   endif
!
   print *,'modified flag=',flag
!
   waves=jcap
   xlayers=levs
   trun=1.
   order=2.
   realform=1.
   rlond=ilonf
   rlatd=ilatg
   rlonp=ilonf
   rlatp=ilatg
   rlonr=ilonf
   rlatr=ilatg
   tracers=1.
   clouds=0.
   pdryini=0.
   subcen=0.
   ppid=0.
   slid=0.
   vcid=0.
   vmid=0.
   vtid=0.
!
#ifdef DEC
   close(nu)
   if(flag(1:4).eq.'ieee') then
     open(unit=nu,file='fort.11',form='unformatted',                           &
          convert='big_endian',status='old',err=920)
     go to 921
  920   continue
       print *,'error opening input sigma file'
       call abort
  921   continue
   endif
#endif
!
!  allocate necessary sigma arrays
!
   allocate(dummy(kdum),dummy2(kdum2),ensemble(kens))
   do i=1,kdum
     dummy(i)=0.
   enddo
   do i=1,kens
     ensemble(i)=0.
   enddo
   do k=1,kdum2
     dummy2(k)=0.
   enddo
   if(flag(1:5).eq.'ascii') then
     print *,'no allocation necessary'
   elseif(flag(1:1).eq.' ') then
     print *,'no allocation necessary'
   elseif(flag(1:8).eq.'ieee_reg') then
     allocate(r4si(kmax+1),r4sl(kmax))
     allocate(r4wave(nwave))
     allocate(r4dummy(kdum),r4dummy2(kdum2),r4ensemble(kens))
   elseif(flag(1:7).eq.'ieee_dp') then
     allocate(r8si(kmax+1),r8sl(kmax))
     allocate(r8wave(nwave))
     allocate(r8dummy(kdum),r8dummy2(kdum2),r8ensemble(kens))
   elseif(flag(1:4).eq.'cray') then
     allocate(rcsi(kmax+1),rcsl(kmax))
     allocate(rcwave(nwave))
     allocate(rcdummy(kdum),rcdummy2(kdum2),rcensemble(kens))
   elseif(flag(1:8).eq.'ieee_sgi') then
     allocate(r8si(kmax+1),r8sl(kmax))
     allocate(r8wave(nwave))
     allocate(r8dummy(kdum),r8dummy2(kdum2),r8ensemble(kens))
   endif
   allocate(wave(nwave))
!
!  first label record
!
   if(flag(1:5).eq.'ascii') then
     read(nu,100)
   else
     read(nu) on85lab
   endif
!
!  second fhour, idate, si, sl, and other record
!
! ascii
!
   if(flag(1:5).eq.'ascii') then
     read(nu,400) fhour,idate,si,sl
!
! native
!
   elseif(flag(1:1).eq.' ') then
     read(nu,err=201) fhour,idate,si,sl                                        &
          ,dummy,waves,xlayers,trun,order,realform,gencode                     &
          ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,tracers                         &
          ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid                &
          ,pdryini,dummy2,clouds
     print *,'read new format unit,fhour,idate=',n,fhour,idate
     if(tracers.eq.0.) then
       tracers=1.
       print *,'tracers reset to 1.'
     endif
     goto 202
201     continue
     rewind nu
     read(nu) on85lab
     read(nu,err=999) fhour,idate,si,sl
     print *,'read old format unit,fhour,idate=',n,fhour,idate
202     continue
!
! ieee_reg
!
   elseif(flag(1:8).eq.'ieee_reg') then
     read(nu,err=301) r4fhour,i4date,r4si,r4sl                                 &
          ,r4dummy,r4waves,r4xlayers,r4trun                                    &
          ,r4order,r4realform,r4gencode                                        &
          ,r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4tracers           &
          ,r4subcen,r4ensemble,r4ppid,r4slid,r4vcid,r4vmid,r4vtid              &
          ,r4runid,r4usrid,r4pdryini,r4dummy2,r4clouds
     print *,'read new format unit,fhour,idate=',n,fhour,idate
     if(r4tracers.eq.0.) then
       r4tracers=1.
       print *,'r4tracers reset to 1.'
     endif
     do i=1,kdum
       dummy(i)=r4dummy(i)
     enddo
     waves=r4waves
     xlayers=r4xlayers
     trun=r4trun
     order=r4order
     realform=r4realform
     gencode=r4gencode
     rlond=r4rlond
     rlatd=r4rlatd
     rlonp=r4rlonp
     rlatp=r4rlatp
     rlonr=r4rlonr
     rlatr=r4rlatr
     tracers=r4tracers
     subcen=r4subcen
     do i=1,kens
       ensemble(i)=r4ensemble(i)
     enddo
     ppid=r4ppid
     slid=r4slid
     vcid=r4vcid
     vmid=r4vmid
     vtid=r4vtid
     runid=r4runid
     usrid=r4usrid
     pdryini=r4pdryini
     do k=1,kdum2
       dummy2(k)=r4dummy2(i)
     enddo
     clouds=r4clouds
     goto 302
301     continue
     rewind nu
     read(nu) on85lab
     read(nu,err=999) r4fhour,i4date,r4si,r4sl
     print *,'read old format unit,fhour,idate=',n,fhour,idate
302     continue
     fhour=r4fhour
     do i=1,4
       idate(i)=i4date(i)
     enddo
     do k=1,kmax+1
       si(k)=r4si(k)
     enddo
     do k=1,kmax
       sl(k)=r4sl(k)
     enddo
!
! ieee_dep
!
   elseif(flag(1:7).eq.'ieee_dp') then
     read(nu,err=401) r8fhour,i8date,r8si,r8sl                                 &
          ,r8dummy,r8waves,r8xlayers,r8trun                                    &
          ,r8order,r8realform,r8gencode                                        &
          ,r8rlond,r8rlatd,r8rlonp,r8rlatp,r8rlonr,r8rlatr,r8tracers           &
          ,r8subcen,r8ensemble,r8ppid,r8slid,r8vcid,r8vmid,r8vtid              &
          ,r8runid,r8usrid,r8pdryini,r8dummy2,r8clouds
     print *,'read new format unit,fhour,idate=',n,fhour,idate
     if(r8tracers.eq.0.) then
       r8tracers=1.
       print *,'r8tracers reset to 1.'
     endif
     do i=1,kdum
       dummy(i)=r8dummy(i)
     enddo
     waves=r8waves
     xlayers=r8xlayers
     trun=r8trun
     order=r8order
     realform=r8realform
     gencode=r8gencode
     rlond=r8rlond
     rlatd=r8rlatd
     rlonp=r8rlonp
     rlatp=r8rlatp
     rlonr=r8rlonr
     rlatr=r8rlatr
     tracers=r8tracers
     subcen=r8subcen
     do i=1,kens
       ensemble(i)=r8ensemble(i)
     enddo
     ppid=r8ppid
     slid=r8slid
     vcid=r8vcid
     vmid=r8vmid
     vtid=r8vtid
     runid=r8runid
     usrid=r8usrid
     pdryini=r8pdryini
     do k=1,kdum2
       dummy2(k)=r8dummy2(i)
     enddo
     clouds=r8clouds
     goto 402
401     continue
     rewind nu
     read(nu) on85lab
     read(nu,err=999) r8fhour,i8date,r8si,r8sl
     print *,'read old format unit,fhour,idate=',n,fhour,idate
402     continue
     fhour=r8fhour
     do i=1,4
       idate(i)=i8date(i*2)
     enddo
     do k=1,kmax+1
       si(k)=r8si(k)
     enddo
     do k=1,kmax
       sl(k)=r8sl(k)
     enddo
!
! cray
!
   elseif(flag(1:4).eq.'cray') then
     read(nu,err=501) rcfhour,icdate,rcsi,rcsl                                 &
          ,rcdummy,rcwaves,rcxlayers,rctrun                                    &
          ,rcorder,rcrealform,rcgencode                                        &
          ,rcrlond,rcrlatd,rcrlonp,rcrlatp,rcrlonr,rcrlatr,rctracers           &
          ,rcsubcen,rcensemble,rcppid,rcslid,rcvcid,rcvmid,rcvtid              &
          ,rcrunid,rcusrid,rcpdryini,rcdummy2,rcclouds
     print *,'read new format unit,fhour,idate=',n,fhour,idate
     if(rctracers.eq.0.) then
       rctracers=1.
       print *,'rctracers reset to 1.'
     endif
     do i=1,kdum
       dummy(i)=rcdummy(i)
     enddo
     waves=rcwaves
     xlayers=rcxlayers
     trun=rctrun
     order=rcorder
     realform=rcrealform
     gencode=rcgencode
     rlond=rcrlond
     rlatd=rcrlatd
     rlonp=rcrlonp
     rlatp=rcrlatp
     rlonr=rcrlonr
     rlatr=rcrlatr
     tracers=rctracers
     subcen=rcsubcen
     do i=1,kens
       ensemble(i)=rcensemble(i)
     enddo
     ppid=rcppid
     slid=rcslid
     vcid=rcvcid
     vmid=rcvmid
     vtid=rcvtid
     runid=rcrunid
     usrid=rcusrid
     pdryini=rcpdryini
     do k=1,kdum2
       dummy2(k)=rcdummy2(i)
     enddo
     clouds=rcclouds
     goto 502
501     continue
     rewind nu
     read(nu) on85lab
     read(nu,err=999) rcfhour,icdate,rcsi,rcsl
     print *,'read old format unit,fhour,idate=',n,fhour,idate
502     continue
     fhour=rcfhour
     do i=1,4
       idate(i)=icdate(i)
     enddo
     do k=1,kmax+1
       si(k)=rcsi(k)
     enddo
     do k=1,kmax
       sl(k)=rcsl(k)
     enddo
!
! ieee_sgi
!
   elseif(flag(1:8).eq.'ieee_sgi') then
     read(nu,err=601) r8fhour,i4date,r8si,r8sl                                 &
          ,r8dummy,r8waves,r8xlayers,r8trun                                    &
          ,r8order,r8realform,r8gencode                                        &
          ,r8rlond,r8rlatd,r8rlonp,r8rlatp,r8rlonr,r8rlatr,r8tracers           &
          ,r8subcen,r8ensemble,r8ppid,r8slid,r8vcid,r8vmid,r8vtid              &
          ,r8runid,r8usrid,r8pdryini,r8dummy2,r8clouds
     print *,'read new format unit,fhour,idate=',n,fhour,idate
     if(r8tracers.eq.0.) then
       r8tracers=1.
       print *,'r8tracers reset to 1.'
     endif
     do i=1,kdum
       dummy(i)=r8dummy(i)
     enddo
     waves=r8waves
     xlayers=r8xlayers
     trun=r8trun
     order=r8order
     realform=r8realform
     gencode=r8gencode
     rlond=r8rlond
     rlatd=r8rlatd
     rlonp=r8rlonp
     rlatp=r8rlatp
     rlonr=r8rlonr
     rlatr=r8rlatr
     tracers=r8tracers
     subcen=r8subcen
     do i=1,kens
       ensemble(i)=r8ensemble(i)
     enddo
     ppid=r8ppid
     slid=r8slid
     vcid=r8vcid
     vmid=r8vmid
     vtid=r8vtid
     runid=r8runid
     usrid=r8usrid
     pdryini=r8pdryini
     do k=1,kdum2
       dummy2(k)=r8dummy2(i)
     enddo
     clouds=r8clouds
     goto 602
601     continue
     rewind nu
     read(nu) on85lab
     read(nu,err=999) r8fhour,i4date,r8si,r8sl
     print *,'read old format unit,fhour,idate=',n,fhour,idate
602     continue
     fhour=r8fhour
     do i=1,4
       idate(i)=i4date(i)
     enddo
     do k=1,kmax+1
       si(k)=r8si(k)
     enddo
     do k=1,kmax
       sl(k)=r8sl(k)
     enddo
   else
     print *,'illegal flag.  must be one of ascii/ieee/ieee_dp'
     print *,'given flag=',flag
     call abort
   endif
!
   print *,'fhour,idate=',fhour,idate
   print *,'number of tracers,clouds=',tracers,clouds
!
!     write(51) fhour,idate,si,sl
!    &      ,dummy,waves,xlayers,trun,order,realform,gencode
!    &      ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,tracers
!    &      ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid
!    &      ,pdryini,dummy2,clouds
!
!     read coefficients (only up to specific humidity)
!
   idv=0
   do k=1,2+4*kmax
     if(flag(1:5).eq.'ascii') then
       read(nu,300,end=900) wave
     elseif(flag(1:1).eq.' ') then
       read(nu,end=900) wave
     elseif(flag(1:8).eq.'ieee_reg') then
       read(nu,end=900) r4wave
       do n=1,nwave
         wave(n)=r4wave(n)
       enddo
     elseif(flag(1:4).eq.'cray') then
       read(nu,end=900) rcwave
       do n=1,nwave
         wave(n)=rcwave(n)
       enddo
     elseif(flag(1:7).eq.'ieee_dp'.or.flag(1:8).eq.'ieee_sgi') then
       read(nu,end=900) r8wave
       do n=1,nwave
         wave(n)=r8wave(n)
       enddo
     endif
     if(k.ge.3+levs.and.k.le.2+levs*3) then
       idv=idv+1
     endif
     do n=1,nwave
       if(k.eq.1) then
         gz(n)=wave(n)
       elseif(k.eq.2) then
         ps(n)=wave(n)
       elseif(k.ge.3.and.k.le.2+levs) then
         tv(n,k-3+1)=wave(n)
       elseif(k.ge.3+levs.and.k.le.2+levs*3) then
         if(mod(idv,2).eq.1) then
           di(n,(idv+1)/2)=wave(n)
         else
           ze(n,idv/2)=wave(n)
         endif
       elseif(k.ge.3+levs*3.and.k.le.2+levs*4) then
         rq(n,k-(3+levs*3)+1)=wave(n)
       endif
     enddo
   enddo
   go to 901
!
  900 continue
   print *,'hit eof while reading sigma file'
   call abort
!
  901 continue
   deallocate(wave)
   deallocate(dummy,dummy2,ensemble)
   if(flag(1:8).eq.'ieee_reg') then
     deallocate(r4si,r4sl)
     deallocate(r4wave)
     deallocate(r4dummy,r4dummy2,r4ensemble)
   elseif(flag(1:7).eq.'ieee_dp') then
     deallocate(r8si,r8sl)
     deallocate(r8wave)
     deallocate(r8dummy,r8dummy2,r8ensemble)
   elseif(flag(1:4).eq.'cray') then
     deallocate(rcsi,rcsl)
     deallocate(rcwave)
     deallocate(rcdummy,rcdummy2,rcensemble)
   elseif(flag(1:8).eq.'ieee_sgi') then
     deallocate(r8si,r8sl)
     deallocate(r8wave)
     deallocate(r8dummy,r8dummy2,r8ensemble)
   endif
   return
!
100   format(32a1)
300   format(1x,6e13.6)
400   format(1x,e13.6,4i13/(1x,e13.6))
999   continue
   print *,'sigma file read error'
   call abort
#endif
   end
