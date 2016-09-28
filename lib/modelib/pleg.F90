   subroutine pleg(m,slat,clat,eps,epstop,pln,plntop)
!$$$  subprogram documentation block
!
! subprogram:    pleg        compute legendre polynomials
!   prgmmr: iredell          org: w/nmc23     date: 92-10-31
!
! abstract: evaluates the orthonormal associated legendre polynomials
!           in the spectral triangle at a given latitude.
!           subprogram gspc should be called already.
!           if l is the zonal wavenumber, n is the total wavenumber,
!           and eps(l,n)=sqrt((n**2-l**2)/(4*n**2-1)) then
!           the following bootstrapping formulas are used:
!           pln(0,0)=sqrt(0.5)
!           pln(l,l)=pln(l-1,l-1)*clat*sqrt(float(2*l+1)/float(2*l))
!           pln(l,n)=(slat*pln(l,n-1)-eps(l,n-1)*pln(l,n-2))/eps(l,n)
!
! program history log:
!   91-10-31  mark iredell
!
! usage:    call pleg(m,slat,clat,eps,epstop,pln,plntop)
!
!   input argument list:
!     m        - integer spectral truncation
!     slat     - real sine of latitude
!     clat     - real cosine of latitude
!     eps      - real ((m+1)*(m+2)/2) sqrt((n**2-l**2)/(4*n**2-1))
!     epstop   - real (m+1) sqrt((n**2-l**2)/(4*n**2-1)) over top
!
!   output argument list:
!     pln      - real ((m+1)*(m+2)/2) legendre polynomial
!     plntop   - real (m+1) legendre polynomial over top
!
! attributes:
!   language: cray fortran
!
!$$$
!fpp$ noconcur r
   real  ::  eps((m+1)*(m+2)/2),epstop(m+1)
   real  ::  pln((m+1)*(m+2)/2),plntop(m+1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  iteratively compute pln(l,l) (bottom hypotenuse of triangle)
   nml=0
   i=1
   pln(i)=sqrt(0.5)
   do l=1,m-nml
      plni=pln(i)
      i=l*(2*m+3-l)/2+(nml+1)
      pln(i)=plni*clat*sqrt(float(2*l+1)/float(2*l))
   enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute pln(l,l+1) (diagonal next to bottom hypotenuse of triangle)
   nml=1
!dir$ ivdep
   do l=0,m-nml
      i=l*(2*m+3-l)/2+(nml+1)
      pln(i)=slat*pln(i-1)/eps(i)
   enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute remaining pln in spectral triangle
   do nml=2,m
!dir$ ivdep
      do l=0,m-nml
         i=l*(2*m+3-l)/2+(nml+1)
         pln(i)=(slat*pln(i-1)-eps(i-1)*pln(i-2))/eps(i)
      enddo
   enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  compute polynomials over top of spectral triangle
   do l=0,m
      nml=m+1-l
      i=l*(2*m+3-l)/2+(nml+1)
      plntop(l+1)=(slat*pln(i-1)-eps(i-1)*pln(i-2))/epstop(l+1)
   enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   return
   end
