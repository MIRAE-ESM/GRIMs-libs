   subroutine filtcof(nlenp,tinc,critfs,critfl,clancz,weight)
!
!  compute coefficients for time smoothing
!  
!  input parameters  
!  
!  nlenp  ... number of grid points used for time smoothing operation.
!           i=-nlen .. 0 ... +nlen  where nlen=nlenp-1
!  tinc   ... time increments between the data (use any unit)
!  critfs ... cutoff frequency, shorter side limit (in same unit as tinc)
!  critfl ... cutoff frequency, longer side limit (in same unit as tinc)
!     critfs=0.  high pass fileter
!     critfl=9.e10  low pass fileter
!  clancz ... lanczos filtering parameter (0. .. nofilter)
!
!  nlen should be chosen in such a way that 2*nlen*tinc >= critfs for
!  high frequency filter, 2*nlen*tinc >= critfl for low frequency filter
!  ane 2*nlen*tinc >= critfl for band-pass filter.
!
!  optional paramter
!
!  lresp ... array length required for response function calculation
!
   integer, parameter  ::  lresp=100
!
   real  ::  weight(1)
!
   real  ::  resp(lresp)
!
   pai=4.*atan(1.)
!
   nlen=nlenp-1
   print *,' --- time filtering coefficient calculation ---'
   print *,'nlen=',nlen
   print *,'clancz=',clancz
!
   if(critfs.eq.0.) then
      if(critfl.eq.0.) then
         print *,'error'
         print *,'critfl and critfs both equal zero'
         call abort
      endif
      thetacl=(2.*pai/critfl)*tinc
      weight(1)=1.-thetacl/pai
      print *,'high pass filter'
      print *,'critfl=',critfl
      print *,'thetacl=',thetacl
      lb=0
   elseif(critfl.eq.9.e10) then
      thetacs=(2.*pai/critfs)*tinc
      weight(1)=thetacs/pai
      print *,'low pass filter'
      print *,'critfs=',critfs
      print *,'thetacs=',thetacs
      lb=1
   elseif(critfs.lt.0..or.critfl.lt.0.) then
      print *,'error'
      print *,'critfs or critfl < 0'
      call abort
    else
      thetacs=(2.*pai/critfs)*tinc
      thetacl=(2.*pai/critfl)*tinc
      weight(1)=(thetacs-thetacl)/pai
      print *,'band pass filter'
      print *,'critfs=',critfs,' critfl=',critfl
      print *,'thetacs,thetacl=',thetacs,thetacl
      lb=2
   endif
   print *,' '
!
!  lb=0 .. high pass, lb=1 .. low pass, lb=2 .. band pass
!
!  weight calculation 
!
   do n=1,nlen
      if(critfs.eq.0.) then
         weight(n+1)=-sin(float(n)*thetacl)/(float(n)*pai)
      elseif(critfl.eq.9.e10) then
         weight(n+1)= sin(float(n)*thetacs)/(float(n)*pai)
      else
         weight(n+1)=(sin(float(n)*thetacs)-sin(float(n)*thetacl))/            &
         (float(n)*pai)
      endif
      if(clancz.gt.0.) then
        weight(n+1)=weight(n+1)*(sin(float(n)*pai/(float(nlen)*clancz+1.)))/   &
        ((float(n)*pai)/(float(nlen)*clancz+1.))
      endif
   enddo
!
   print *,' '
   print *,'unnormalized coefficients'
   do n=1,nlenp
     print *,n-1,weight(n)
   enddo
!
!  normalize weights
!  factor 2 for -n to n
!
   if(lb.eq.1) then
      critiv=0.
   elseif(lb.eq.0) then
      critiv=1./(2.*tinc)
   elseif(lb.eq.2) then
!       critiv=1./((critfl+critfs)*0.5)
      critiv=(1./critfl+1./critfs)*0.5
   endif
   ssum=weight(1)
   wsum=0.
   do n=1,nlen
      x=cos(2.*pai*tinc*float(n)*critiv)
      print *,'x=',x
      ssum=ssum+weight(n+1)*2.
      wsum=wsum+weight(n+1)*2.*x
   enddo
   print *,' '
   print *,'sum of weights without weighting   =',ssum
!
!  shift central point for high pass and band pass filter
!
   if(lb.eq.0.or.lb.eq.2) then
      weight(1)=weight(1)-ssum
   endif
   wsum=wsum+weight(1)
!
   usum=weight(1)
   do n=1,nlen
      usum=usum+weight(n+1)*2.
   enddo
   print *,'sum of shifted weights             =',usum
!
!  normalize
!
   weight(1)=weight(1)/wsum
   tsum=weight(1)
   do n=1,nlen
      weight(n+1)=weight(n+1)/wsum
      tsum=tsum+weight(n+1)*2.
   enddo
   print *,'sum of weights with weighting      =',wsum
   print *,'sum of weights after normalization =',tsum
   vsum=weight(1)
   do n=1,nlen
      x=cos(2.*pai*tinc*float(n)*critiv)
      vsum=vsum+weight(n+1)*2.*x
   enddo
   print *,'weighted sum of weights after normalization =',vsum
!
!  print final coefficients
!
   print *,'normalized and shifted coefficients'
   do n=1,nlenp
     print *,n-1,weight(n)
   enddo
!    
!  compute response function
!
   do i=1,lresp
      resp(i)=weight(1)
   enddo
   if(critfs.eq.0.) then
      xlen=thetacl*5.
   elseif(critfl.eq.9.e10) then
      xlen=thetacs*5.
   else
      xlen=thetacs*5.
   endif
   if(xlen.gt.pai) xlen=pai
   do i=1,lresp
      theta=xlen/float(lresp-1)*float(i-1)
      do n=1,nlen
         resp(i)=resp(i)+2.*weight(n+1)*cos(float(n)*theta)
      enddo
   enddo
!
   pi=4.*atan(1.)
   print *,' '
   print *,'response function'
   do i=1,lresp
      freq=xlen/float(lresp-1)*float(i-1)
      if(freq.ne.0.) then
         peri=2*pi/freq*tinc
      else
         peri=9.e30
      endif 
   write(6,1010) i,freq,peri,resp(i)
   1010  format(i3,3e14.5)
   enddo
!
   return
   end
