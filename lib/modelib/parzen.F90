   subroutine parzen(ipoint, weight)
!
! compute parzen window weight
!
! input
!   the number of points
   integer ipoint
!
! output
!   weight
   real weight (*)
!
   do i = 1, ipoint
      weight (i) = 1.-abs((float(i)-0.5*float(ipoint+1))/(0.5*float(ipoint+1)))
   enddo
   return
   end
