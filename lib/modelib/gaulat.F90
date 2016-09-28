   subroutine gaulat(gaul,k)
!
! hoon dimension a(2000)
   real, allocatable  ::  a(:)
   real  ::  gaul(1)
!
   allocate(a(k))
      esp=1.e-14
      c=(1.e0-(2.e0/3.14159265358979e0)**2)*0.25e0
      fk=k
      kk=k/2
      call bsslz1(a,kk)
   do is=1,kk
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
   enddo 
   if(k.eq.kk*2) go to 50
   a(kk+1)=0.e0
   pk=2.e0/fk**2
   do 40 n=2,k,2
   fn=n
   40 pk=pk*fn**2/(fn-1.e0)**2
   50 continue
   do n=1,kk
      l=k+1-n
      a(l)=-a(n)
   enddo 
!
   radi=180.e0/(4.e0*atan(1.e0))
   do n=1,k
      gaul(n)=acos(a(n))*radi
   enddo
!     print *,'gaussian lat (deg) by modlib gaulat.F for jmax=',k
!     print *,(gaul(n),n=1,k)
!
   deallocate(a)
   return
   70 print *,'error in gauaw'
   stop
   end
