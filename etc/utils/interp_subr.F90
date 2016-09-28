!----------------------------------------------------------------------
   subroutine subinterp(a,ij,b,ijo,                                            &
                      n00,n10,n11,n01,d00,d10,d11,d01,lflag,dmiss)
!----------------------------------------------------------------------
!
   integer              ::  ij,ijo
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
!yh
!yh s .................................................... 021009 s
         if (a00(n).eq.dmiss .or. a10(n).eq.dmiss .or.                         &
               a01(n).eq.dmiss .or. a11(n).eq.dmiss ) then
            a00(n) = dmiss
            d00(n) = 1.0  
            a10(n) = 0.0
            a01(n) = 0.0
            a11(n) = 0.0
         endif
!yh e .................................................... 021009 e
!yh
      else
!        a00(n)=-9999.9999
         a00(n)=avg
         a10(n)=0.0
         a11(n)=0.0
         a01(n)=0.0
      endif
   enddo
   !
   do n=1,ijo
      b(n)   =    a00(n) * d00(n) + a10(n) * d10(n)                            &
                + a11(n) * d11(n) + a01(n) * d01(n)
   enddo
!
   return
   end
!
!----------------------------------------------------------------------
   subroutine averag(a,ij,avg)
!----------------------------------------------------------------------
   integer              ::  ij
   real                 ::  a(ij),avg
   !
   sum = 0.
   do n = 1,ij
   sum = a(n) + sum
   enddo
   avg = sum / ij
   return
   end
!
!----------------------------------------------------------------------
   subroutine i2oini(proj ,orient ,truth ,cotru ,                              &
                     delx ,dely ,rlat1 ,rlon1 ,rlat2,rlon2,ii, jj,             &
                     projo,oriento,trutho,cotruo,                              &
                     delxo,delyo,rlat1o,rlon1o,rlat2o,rlon2o,                  &
                     n00,n10,n11,n01,d00,d10,d11,d01,lflag,io,jo)
!----------------------------------------------------------------------
!
   integer              ::  io,jo,ii, jj
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
      print *, ' input grid rlat1 rlon1 x00 y00 ',rlat1,rlon1,x00,y00
   endif
!
   if(abs(projo).ne.3.) then
      call ll2xy(projo,oriento,trutho,cotruo,rlat1o,rlon1o,x00o,y00o)
      print *, ' output grid rlat1  rlon1  x00  y00 ',rlat1o,rlon1o,x00o,y00o
   endif
!
#ifdef DEBUG
   print*,' i j glat glon ',glat,glon
#endif
   n = 0
   do j=1,jo
      if(abs(projo).eq.3.) then
         rlato = rlat1o + delyo*float(j-1)/1000. 
      else
         yo=y00o+(j-1)*delyo
      endif
      do i=1,io
         if(abs(projo).eq.3.) then
            rlono = rlon1o + delxo*float(i-1)/1000. 
            if(rlono.lt.0.) rlono = rlono + 360.
            if(abs(proj).eq.3..or.abs(proj).eq.4.) then
               call ll2gg(proj,glat,glon,ii,jj,delx,dely,                      &
                          rlat1,rlon1,rlato,rlono,x,y)
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
         lon=xlon
         lat=xlat
#ifdef DEBUG
         print*,' i j rlato rlono x y lon lat'
         print 1967,i,j,rlato,rlono,x,y,lon,lat
1967     format(2i5,4f10.5,2i5)
#endif
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
            print *, y,' Out side of y dimension at i j ',i,j
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
         !
         if (lon.lt.1) then
            n00(n) = ii   + (lat   -1)*ii
            n01(n) = ii   + (lat+1 -1)*ii
         endif
         if (i.le.5 .and. j.le.5) then
            print 1964, i,j,n,lon,lat,dlon1,dlat1,n00(n),n10(n),n11(n),n01(n)
1964        format(5i5,2f8.3,4i5)
         endif
      enddo
   enddo
!
   return
   end
!
!----------------------------------------------------------------------
   subroutine ll2gg(proj,glat,glon,ii,jj,delx,dely,rlat1,rlon1,                &
                    rlat,rlon,xlon,ylat)
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
   subroutine ll2xy(cproj,corient,ctruth,ccotru,clat,clon,cx,cy)
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
   subroutine xy2ll(cproj,corient,ctruth,ccotru,cx,cy,clat,clon)
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
   subroutine gaulat(gaulb,k)
!----------------------------------------------------------------------
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
!----------------------------------------------------------------------
   subroutine bsslz1(bes,n)
!----------------------------------------------------------------------
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
   nn=n
   if(n.gt.50) then
      bes(50)=bz(50)
      do j=51,n
         bes(j)=bes(j-1)+pi
      enddo
      nn=49
   endif
   do j=1,nn
      bes(j)=bz(j)
   enddo
   return
   end
