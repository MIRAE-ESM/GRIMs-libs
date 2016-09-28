   subroutine gl2gl(gauin,imxin,jmxin,gauout,imxout,jmxout)
!
!  interpolation from gaussian grid to other gaussian grid
!
   dimension gauin (imxin ,jmxin )
!
   dimension gauout(imxout,jmxout)
   dimension gaulin(500),gaulou(500)
   dimension iindx1(1000)
   dimension iindx2(1000)
   dimension jindx1(500)
   dimension jindx2(500)
   dimension ddx(1000)
   dimension ddy(500)
!
   save iindx1,iindx2,jindx1,jindx2,ddx,ddy
!
   data ifp/0/
!
   if(imxin.eq.imxout.and.jmxin.eq.jmxout) then
      do j=1,jmxin
      do i=1,imxin
         gauout(i,j)=gauin(i,j)
      enddo 
      enddo 
      return
   endif
!
   if(ifp.ne.0) go to 111
   ifp=1
!
   call gaulat(gaulin,jmxin)
   do j=1,jmxin
      gaulin(j)=90.-gaulin(j)
   enddo 
!
   call gaulat(gaulou,jmxout)
   do j=1,jmxout
      gaulou(j)=90.-gaulou(j)
   enddo 
!
   dxin =360./float(imxin )
   dxout=360./float(imxout)
!
   do i=1,imxout
      alamd=float(i-1)*dxout
      i1=alamd/dxin+1.001
      iindx1(i)=i1
      i2=i1+1
      if(i2.gt.imxin) i2=1
      iindx2(i)=i2
!     print *,'i,i1,i2',i,i1,i2
      ddx(i)=(alamd-float(i1-1)*dxin)/dxin
   enddo 
!
   j2=1
   do 40 j=1,jmxout
      aphi=gaulou(j)
   do 50 jj=1,jmxin
   if(aphi.lt.gaulin(jj)) go to 50
      j2=jj
   go to 42
   50 continue
   42 continue
   if(j2.gt.2) go to 43
      j1=1
      j2=2
   go to 44
   43 continue
   if(j2.le.jmxin) go to 45
      j1=jmxin-1
      j2=jmxin
   go to 44
   45 continue
   j1=j2-1
   44 continue
!     print *,'j,j1,j2',j,j1,j2
   jindx1(j)=j1
   jindx2(j)=j2
   ddy(j)=(aphi-gaulin(j1))/(gaulin(j2)-gaulin(j1))
   40 continue
!
!     print *,'iindx1'
!     print *,(iindx1(n),n=1,imxout)
!     print *,'iindx2'
!     print *,(iindx2(n),n=1,imxout)
!     print *,'jindx1'
!     print *,(jindx1(n),n=1,jmxout)
!     print *,'jindx2'
!     print *,(jindx2(n),n=1,jmxout)
!     print *,'ddy'
!     print *,(ddy(n),n=1,jmxout)
!     print *,'ddx'
!     print *,(ddx(n),n=1,jmxout)
!
  111 continue
!
   do 60 j=1,jmxout
      y=ddy(j)
      j1=jindx1(j)
      j2=jindx2(j)
   do 60 i=1,imxout
      x=ddx(i)
      i1=iindx1(i)
      i2=iindx2(i)
      gauout(i,j)=(1.-x)*(1.-y)*gauin(i1,j1)+(1.-y)*x*gauin(i2,j1)+            &
      (1.-x)*y*gauin(i1,j2)+x*y*gauin(i2,j2)
   60 continue
!
   return
   end
