   subroutine pder(m,enn1,elonn1,eon,eontop,pln,plntop,plndx,plndy)
!$$$  subprogram documentation block
!
! subprogram:    pder        compute legendre polynomials derivatives
!   prgmmr: iredell          org: w/nmc23     date: 92-10-31
!
! abstract: computes the zonal and meridional derivatives
!           of the normalized associated legendre polynomials
!           in the spectral triangle at a given latitude.
!           subprogram gspc and pleg should be called already.
!
! program history log:
!   91-10-31  mark iredell
!
! usage:    call pder(m,enn1,elonn1,eon,eontop,pln,plntop,
!    &                plndx,plndy)
!
!   input argument list:
!     m        - integer spectral truncation
!     enn1     - real ((m+1)*(m+2)/2) n*(n+1)/a**2
!     elonn1   - real ((m+1)*(m+2)/2) l/(n*(n+1))*a
!     eon      - real ((m+1)*(m+2)/2) epsilon/n*a
!     eontop   - real (m+1) epsilon/n*a over top
!     pln      - real ((m+1)*(m+2)/2) legendre polynomial
!     plntop   - real (m+1) legendre polynomial over top
!
!   output argument list:
!     plndx    - real ((m+1)*(m+2)/2) zonal derivatives (no i)
!     plndy    - real ((m+1)*(m+2)/2) meridional derivatives
!
! attributes:
!   language: cray fortran
!
!$$$
!fpp$ noconcur r
   real enn1((m+1)*(m+2)/2),elonn1((m+1)*(m+2)/2)
   real eon((m+1)*(m+2)/2),eontop(m+1)
   real pln((m+1)*(m+2)/2),plntop(m+1)
   real plndx((m+1)*(m+2)/2),plndy((m+1)*(m+2)/2)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute polynomial derivatives for spectral analysis
   i=1
   plndy(i)=eon(i+1)*pln(i+1)
   do i=2,(m+1)*(m+2)/2-1
      plndy(i)=eon(i+1)*pln(i+1)-eon(i)*pln(i-1)
   enddo
   i=(m+1)*(m+2)/2
   plndy(i)=-eon(i)*pln(i-1)
   do l=0,m
      nml=m-l
      i=l*(2*m+3-l)/2+(nml+1)
      plndy(i)=plndy(i)+eontop(l+1)*plntop(l+1)
   enddo
   do i=1,(m+1)*(m+2)/2
      plndx(i)=enn1(i)*elonn1(i)*pln(i)
      plndy(i)=enn1(i)*plndy(i)
   enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   return
   end
