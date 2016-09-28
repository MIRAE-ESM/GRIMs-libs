   subroutine poly(n,rad,p)
   save
   x = cos(rad)
   y1 = 1.0
   y2=x
   do i=2,n
      g=x*y2
      y3=g-y1+g-(g-y1)/float(i)
      y1=y2
      y2=y3
   enddo 
   p=y3
   return
   end
