   subroutine glats(lgghaf,colrad,wgt,wgtcs,rcs2)
!
   dimension colrad(lgghaf),wgt(lgghaf),wgtcs(lgghaf),rcs2(lgghaf)
!
   eps=1.e-14
!
   si = 1.0
   l2=2*lgghaf
   rl2=l2
   scale = 2.0/(rl2*rl2)
   k1=l2-1
   pi = 4*atan(1.)
   dradz = pi / (4.*lgghaf)
!dradz = pi / 360.
   rad = 0.0
!
   print 202
   202 format(1h0,'print of k,phi,colrad,wgt,wgtcs,iter,p1 from glats')
!
   do k=1,lgghaf
      iter=0
      drad=dradz
      1 call poly(l2,rad,p2)
      2 p1 =p2
      iter=iter+1
      rad=rad+drad
      call poly(l2,rad,p2)
      if(sign(si,p1).eq.sign(si,p2)) go to 2
      if(drad.lt.eps)go to 3
      rad=rad-drad
!drad = drad * 0.25
      drad = drad * 0.1
      go to 1
      3 continue
      colrad(k)=rad
      phi = rad * 180 / pi
      call poly(k1,rad,p1)
      x = cos(rad)
      w = scale * (1.0 - x*x)/ (p1*p1)
      wgt(k) = w
      sn = sin(rad)
      w=w/(sn*sn)
      wgtcs(k) = w
      rc=1./(sn*sn)
      rcs2(k) = rc
      call poly(l2,rad,p1)
!
      print 102,k,phi,colrad(k),wgt(k),wgtcs(k),iter,p1
      102 format(1h ,i2,2x,f6.2,2x,f10.7,2x,e13.7,2x,e13.7,2x,i4,2x,d13.7)
!
   enddo
!
   return
   end
