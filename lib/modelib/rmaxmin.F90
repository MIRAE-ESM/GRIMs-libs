   subroutine rmaxmin(a,len,k,k1,k2,ch)
!$$$  subprogram documentation block
!                .      .    .                                       .
! abstract:  do print maximum and minimum of a given array.
!
! program history log:
!
! usage:    call  maxmin(a,len,k,k1,k2,ch)
!   input argument list:
!     a 	- array for computing max and min (len,k)
!     len	- the first dimension of a
!     k 	- the second dimension of a
!     k1	- lower limit of second dimension to print
!     k2	- upper limit to print
!     ch 	- charcter string to print
!                 fpl and fml
!
!   output argument list:
!
!   input files: none
!
!   output files:
!     standard output
!
!   subprograms called:
!     intrinsic functions: amax1 amin1
!
!   remark: none
!
! attributes:
!   language: fortran 77.
!   machine:  cray ymp.
!
!$$$

   real  ::  a(len,k)
   character ch*(*)
!
   real, parameter  ::  crit=1.e20
!
   aamax=0.
   aamin=0.
   do j=k1,k2
      do m=1,len
         if(abs(a(m,j)).lt.crit.and.a(m,j).ne.0.) then
            aamax = a(m,j)
            aamin = a(m,j)
            go to 2
         endif
      enddo
      2   continue
      nmis=0
      nzero=0
      do m=1,len
         if(a(m,j).eq.0.) then
            nzero=nzero+1
            go to 1
         endif
         if(abs(a(m,j)).gt.crit) then
            nmis=nmis+1
            go to 1
         endif
         aamax = max( aamax, a(m,j) )
         aamin = min( aamin, a(m,j) )
         1  continue
      enddo
      print 100,ch,aamax,aamin,j,nzero,nmis
      100   format(a12,' has max=',e10.4,' min=',e10.4,' k=',i4,' nzero=',i7,  &
                   ' nmis=',i7)
   enddo
   return
   end
