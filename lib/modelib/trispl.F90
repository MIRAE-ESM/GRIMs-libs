   subroutine trispl(n,p,q,y)
!-----------------------------------------------------
!    subroutine calculates weights for cubic spline interpolation
!     n...number ofc     n...number of observations
!     y...function y(p) to be interpolated
!     p...coor (independent variables)
!     q,ovh,sh...contain weights
!----------------------------------------------------
!
   common/spl/ ovh(100),sh(100),iflag,jflag,theta
   real  ::  c(100),b(100),p(n),q(n),d(100),y(n),tchs(100),tb(100)

   nu=n-1
   if (iflag.ne.0) go to 99
      do i=2,n
         im=i-1
         him=p(i)-p(im)
         if(him .eq. 0.) him=0.001
         ovh(im)=1/him
         th=theta*him
         sh(im)=1/sinh(th)
         tchs(im)=(theta*cosh(th))*sh(im)-ovh(im)
      enddo    
      do i=2,nu
         im=i-1
         c(im)=ovh(im)-theta*sh(im)
         b(i)=tchs(im)+tchs(i)
      enddo    
         c(n-1)=ovh(n-1)-theta*sh(n-1)
         b(1)=tchs(1)
         b(n)=tchs(n-1)
         tb(1)=c(1)/b(1)
      do i=2,nu
         tb(i)=c(i)/(b(i)-c(i-1)*tb(i-1))
      enddo    
         d(1)=0.
         d(n)=0.
         iflag=1
      99   yp=(y(2)-y(1))*ovh(1)
      do i=2,nu
         ynow=(y(i+1)-y(i))*ovh(i)
         d(i)=ynow-yp
         yp=ynow
      enddo     
         q(1)=d(1)/b(1)
      do i=2,n
         im=i-1
         q(i)=(d(i)-c(im)*q(im))/(b(i)-c(im)*tb(im))
      enddo    
      do i=1,nu
         ii=n-i
         q(ii)=q(ii)-tb(ii)*q(ii+1)
      enddo    
   return
   end
