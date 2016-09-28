   subroutine sphpt1(jcap,kmx,wave,physv,rlon,ppnm)
!                                                                       
!  this routine performs conversion of spectral coefficients
!  to physical space on arbitrary lat/lon points.
!
!  input:
!     jcap:  spherical coefficient resolution
!      kmx:  number of levels
!     wave:  spherical coefficents
!     rlon:  longituds of the points
!
!  output:
!    physv:  physical space value
!
   real  ::  wave((jcap+1)*(jcap+2),kmx)
   real  ::  physv(kmx)
!                                                                       
!  local dimension
!
   real, allocatable  ::  gfftc(:,:)
   real, allocatable  ::  sumc(:,:,:)
!
   real  ::  ppnm((jcap+1)*(jcap+2))
!
   allocate (sumc(2,0:jcap,kmx))
   allocate (gfftc(2,0:jcap))
!
   jcap1=jcap+1
!
   gfftc(1,0)=1.0
   gfftc(2,0)=0.0
   do mm=1,jcap
      waven=float(mm)
      gfftc(1,mm)= cos(waven*rlon)*2.0
      gfftc(2,mm)=-sin(waven*rlon)*2.0
   enddo
!                                                                       
!  do transform                                                 
!                                                                       
   do n=1,2
      do k=1,kmx
         do m=0,jcap
            sumc(n,m,k)=0.0
         enddo
      enddo
   enddo
   do l=0,jcap
      ls=l*((2*jcap+3)-l)
      do i=1,jcap1-l
         ii=(i-1)*2
         do k=1,kmx
            sumc(1,l,k)=sumc(1,l,k)+ppnm(ls+1+ii)*wave(ls+1+ii,k)
            sumc(2,l,k)=sumc(2,l,k)+ppnm(ls+2+ii)*wave(ls+2+ii,k)
         enddo
      enddo
   enddo
   do k=1,kmx
      physv(k)=0.0
   enddo
   do i=1,jcap1
      ii=i-1
      do k=1,kmx
         physv(k)=physv(k)+gfftc(1,ii)*sumc(1,ii,k)+gfftc(2,ii)*sumc(2,ii,k)
      enddo
   enddo
!
   deallocate (gfftc)
   deallocate (sumc)
!
   return                                                            
   end                                                               
   subroutine sphpt2(jcap,kmx,wave,physvx,physvy,rlon,ppnm,hhnm)
!
!    1                  rlon,rsnln,rcsln)
!
!  this routine performs conversion of spectral coefficients
!  to space derivative in physical space on arbitrary lat/lon points.
!
!  input:
!     jcap:  spherical coefficient resolution
!      kmx:  number of levels
!     wave:  spherical coefficents
!     rlon:  longituds of the points
!      pnm:  legendre polynomial
!      hnm:  legendre polynomial y-derivative
! not used
!    rsnln:  coordinate rotation sin factor
!    rcsln:  coordinate rotation cos factor
!
!  output:
!    physvx:  d/dx
!    physvy:  d/dy
!   
   real  ::  wave((jcap+1)*(jcap+2),kmx),physvx(kmx),physvy(kmx)
   real  ::  ppnm((jcap+1)*(jcap+2)),hhnm((jcap+1)*(jcap+2))
!
!  local variables
!
   real  :: qex(0:jcap)
   real  :: gfftc(2,0:jcap),gfftx(2,0:jcap)  
   real  :: sumc(2,0:jcap,kmx),sumy(2,0:jcap,kmx)
!
   real, parameter  ::  er_=6.3712e06
   real, parameter  ::  eriv_=1./er_
!
   jcap1=jcap+1
!                                                                       
!  get fer and qimer for spectral transfer
!
   do m=0,jcap
      qex(m)=eriv_*float(m)
   enddo
!
   gfftc(1,0)=1.0
   gfftc(2,0)=0.0
   do mm=1,jcap
      waven=float(mm)
      gfftc(1,mm)= cos(waven*rlon)*2.0
      gfftc(2,mm)=-sin(waven*rlon)*2.0
   enddo
   do mm=0,jcap
      gfftx(2,mm)=-gfftc(1,mm)*qex(mm)
      gfftx(1,mm)= gfftc(2,mm)*qex(mm)
   enddo
!                                                                      
!  do transform                                                 
!                                                                       
   do n=1,2
      do k=1,kmx
         do m=0,jcap
            sumc(n,m,k)=0.0
            sumy(n,m,k)=0.0
         enddo
      enddo
   enddo
   do l=0,jcap
      ls=l*((2*jcap+3)-l)
      do i=1,jcap1-l
         ii=(i-1)*2
         do k=1,kmx
            sumc(1,l,k)=sumc(1,l,k)+ppnm(ls+1+ii)*wave(ls+1+ii,k)
            sumc(2,l,k)=sumc(2,l,k)+ppnm(ls+2+ii)*wave(ls+2+ii,k)
            sumy(1,l,k)=sumy(1,l,k)+hhnm(ls+1+ii)*wave(ls+1+ii,k)
            sumy(2,l,k)=sumy(2,l,k)+hhnm(ls+2+ii)*wave(ls+2+ii,k)
         enddo
      enddo
   enddo
   do k=1,kmx
      physvx(k)=0.0
      physvy(k)=0.0
   enddo
   do i=1,jcap1
      ii=i-1
      do k=1,kmx
         physvx(k)=physvx(k)+gfftx(1,ii)*sumc(1,ii,k)+gfftx(2,ii)*sumc(2,ii,k)
         physvy(k)=physvy(k)+gfftc(1,ii)*sumy(1,ii,k)*eriv_+gfftc(2,ii)*       &
                   sumy(2,ii,k)*eriv_
      enddo
   enddo
!
!  rotation if different coordinate
!
!     do k=1,kmx
!       do n=1,lngrdb
!         qy=physvy(n,k)
!         qx=physvx(n,k)
!         physvx(n,k)=-qx*rsnln(n)-qy*rcsln(n)
!         physvy(n,k)= qx*rcsln(n)-qy*rsnln(n)
!       enddo
!     enddo
!
   return                                                            
   end
   subroutine rpln2i(jcap,qlnt,dqlnt,colrad)
! 
!  computes legendre polinomial and its y-derivatives 
! 
   real  ::  qlnt((jcap+1)*(jcap+2))
   real  ::  dqlnt((jcap+1)*(jcap+2))
!
! local arrays
!
   real  ::  qlnv((jcap+1)*(jcap+4))
   real  ::  x(jcap+1)
   real  ::  dpln((jcap+1)*(jcap+4))
!
   real  ::  indxmv((jcap+1)*(jcap+4))
   real  ::  deps((jcap+1)*(jcap+4)),rdeps((jcap+1)*(jcap+4)),dx(2*(jcap+1)),  &
             y(jcap+1)
   real  ::  dxab((jcap+1)*(jcap+2),2)
!
   call gpln2i(jcap,indxmv,deps,rdeps,dx,y)
   call ggozrm(jcap,dxab)
!
   jcap1=jcap+1
   jcap2=jcap+2
!
   colr=colrad
   sinlat=cos(colr)
   cos2=1.0-sinlat*sinlat
   prod=1.0
   do ll=1,jcap1
      x(ll)=0.5*prod
      prod=prod*cos2*y(ll)
   enddo
   do ll=1,jcap1
      x(ll)=sqrt(x(ll))
   enddo
   do ll=1,jcap1
      dpln(2*ll-1)=x(ll)
      dpln(2*ll  )=x(ll)
   enddo
   lplus=2*jcap1
   do ll=1,2*jcap1
      dpln(ll+lplus)=dx(ll)*sinlat*dpln(ll)
   enddo
   lp2=0
   lp1=2*jcap1
   lp0=2*2*jcap1
   len=2*jcap1-2
   do n=3,jcap2
      do ll=1,len
         dpln(ll+lp0)=(sinlat*dpln(ll+lp1)-deps(ll+lp1)*dpln(ll+lp2))*         &
                      rdeps(ll+lp0)
      enddo
      lp2=lp1
      lp1=lp0
      lp0=lp0+len
      len=len-2
   enddo
!
!  transpose vector dpln array from cra. order to ibm order.
!
   do i=1,jcap1*(jcap+4)
      qlnv(indxmv(i))=dpln(i)
   enddo
!
   lpv=0
   lpt=0
   len=2*jcap1
   do n=1,jcap1
      do ll=1,len
         qlnt(ll+lpt)=qlnv(ll+lpv)
      enddo
      lpv=lpv+len+2
      lpt=lpt+len
      len=len-2
   enddo
   lp0=0
   lp1=2
   len=2*jcap1
   do i=1,jcap1
      do ll=1,len
         dqlnt(ll+lp0)=+qlnv(ll+lp1)*dxab(ll+lp0,2)
      enddo
      lp1=lp1+len+2
      lp0=lp0+len
      len=len-2
   enddo
   lend=jcap1*jcap2-4
   do ll=1,lend
      dqlnt(ll+2)=dqlnt(ll+2)+qlnt(ll)*dxab(ll+2,1)
   enddo
!
   return
   end
   subroutine gpln2i(jcap,indxmv,deps,rdeps,dx,y)
!
   integer  ::  indxmv((jcap+1)*(jcap+4))
   real  ::  deps((jcap+1)*(jcap+4)),rdeps((jcap+1)*(jcap+4)),dx(2*(jcap+1)),  &
             y(jcap+1)
   real  ::  x(jcap+1)
!
   jcap1=jcap+1
   jcap2=jcap+2
!
   do ll=1,jcap1
      rdeps(ll) = 0.0
   enddo
!
   lplus=jcap1
   len  =jcap1
   do inde=2,jcap2
      do ll=1,len
         l=ll-1
         n=l+inde-1
         rdeps(ll+lplus)=(n*n-l*l)/(4.0*n*n-1.0)
      enddo
      lplus=lplus+len
      len=len-1
   enddo
   do i=jcap2,jcap1*jcap2/2+jcap1
      rdeps(i)=sqrt(rdeps(i))
   enddo
   do i=1,jcap1*jcap2/2+jcap1
      deps(2*i-1)=rdeps(i)
      deps(2*i  )=rdeps(i)
   enddo
   ibegin=2*jcap1+1
   do i=ibegin,jcap1*(jcap+4)
      rdeps(i)=1.0/deps(i)
   enddo
   do ll=1,jcap1
      x(ll)=ll*2+1
   enddo
   do ll=1,jcap1
      y(ll)=x(ll)/(x(ll)-1.)
   enddo
   do ll=1,jcap1
      x(ll)=sqrt(x(ll))
   enddo
   do ll=1,jcap1
      dx(2*ll-1)=x(ll)
      dx(2*ll  )=x(ll)
   enddo
!
!    set index array for transposing vector array
!   from cray order to ibm order.
!
   l=0
   do nn=1,jcap2
      lln=min0(jcap2-nn+1,jcap1)
      do ll=1,lln
         indx=((jcap+3)*(ll-1)-(ll-1)*ll/2+nn)*2
         l=l+2
         indxmv(l-1)=indx-1
         indxmv(l  )=indx
      enddo
   enddo
!
   return
   end
   subroutine ggozrm(jcap,dxab)
!
   real  ::  eps(jcap+2,jcap+1)
   real  ::  dxint((jcap+1)*(jcap+2))
   real  ::  dx((jcap+1)*2,jcap+2)
   real  ::  deps((jcap+1)*2,jcap+2)
   real  ::  dxab((jcap+1)*(jcap+2),2)
!
   jcap1=jcap+1
   jcap2=jcap+2
   do ll=1,jcap1
      l=ll-1
      do inde=2,jcap2
         n=l+inde-1
         a=(n*n-l*l)/(4.0*n*n-1.0)
         eps(inde,ll)=sqrt(a)
      enddo
   enddo
   do ll=1,jcap1
      eps(1,ll) = 0.0e0
   enddo
! 
   do ll=1,jcap1*2
      dxint(2*ll-1)=ll
      dxint(2*ll  )=ll
   enddo
   lp = 0
   do i=1,jcap2
      do ll=1,jcap1*2
         dx(ll,i)=dxint(ll+lp)
      enddo
      lp=lp+2
   enddo
   do i=1,jcap2
      do ll=1,jcap1
         deps(2*ll-1,i)=eps(i,ll)
         deps(2*ll  ,i)=eps(i,ll)
      enddo
   enddo
   do ll=1,jcap1*2
      dxab(ll,1)=0.0
   enddo
   lp1=jcap1*2
   len=jcap1*2-2
   do i=1,jcap
      do ll=1,len
         dxab(ll+lp1,1)= dx(ll,i+1)*deps(ll,i+1)
         dxab(ll+lp1,2)=-dx(ll,i  )*deps(ll,i+2)
      enddo
      lp1=lp1+len
      len=len-2
   enddo
   do i=1,jcap2
      do ll=1,jcap1*2
         dx(ll,i)=dx(ll,i)-1.e0
      enddo
   enddo
   do ll=1,jcap1*2
      dxab(ll,2)=-dx(ll,1)*deps(ll,2)
   enddo
!
!  transpose scalar arrays dxab from cray order to ibm order.
!
   call transo (dxab,jcap,2)
!
   return
   end
   subroutine transo(a,jcap,kmax)
!
   integer  ::  indxnn((jcap+1)*(jcap+2)),indxmm((jcap+1)*(jcap+2))
!
   real  ::  a((jcap+1)*(jcap+2),kmax)
   real  ::  b((jcap+1)*(jcap+2))
!
   jcap1=jcap+1
   jcap2=jcap+2
!
   call cmpind(jcap,indxnn,indxmm)
!
   do k=1,kmax
      do m=1,jcap1*jcap2
         b(indxmm(m))=a(m,k)
      enddo
      do m=1,jcap1*jcap2
         a(m,k)=b(m)
      enddo
   enddo
!
   return
   end
   subroutine cmpind(jcap,indxnn,indxmm)
!
   integer  ::  indxnn((jcap+1)*(jcap+2)),indxmm((jcap+1)*(jcap+2))
!
!  indxnn :  1-d index of converting input form spher coeff array
!                  to transposed form array
!  indxmm :  1-d index of converting transposed form spher coeff
!                  array to input form spherical coeff array
!
   jcap1=jcap+1
   l=0
   do m=1,jcap1
      nend=jcap1-m+1
      do nn=1,nend
         n=nn+m-1
         l=l+2
         indx=(jcap1*(n-m)-(n-m)*(n-m-1)/2+m)*2-1
         indxnn(l-1)=indx
         indxnn(l  )=indx+1
      enddo
   enddo
!
   l=0
   do nn=1,jcap1
      lln=jcap1-nn+1
      do ll=1,lln
         n=ll+nn-1
         m=ll
         indx=(m*jcap1-(jcap1-n)-(m-1)*m/2)*2-1
         l=l+2
         indxmm(l-1)=indx
         indxmm(l  )=indx+1
      enddo
   enddo
   return
!
   end
