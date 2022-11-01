   subroutine nbaread(bbuf,isize,ib,nb,ka,a)
!$$$  subprogram documentation block
!
! subprogram: nbaread         byte-addressable read
!   prgmmr: kanamitsu         date: 04-02-06
!
! abstract: read a given number of bytes from an unblocked file,
!   skipping a given number of bytes.
!
! program history log:
!
! usage:    call baread(bbuf,isize,ib,nb,ka,a)
!   input arguments:
!     bbuf         grib record
!     isize        size of grib record
!     ib           integer number of bytes to skip
!     nb           integer number of bytes to read
!   output arguments:
!     ka           integer number of bytes actually read
!     a            character*1 (nb) buffer read
!
! attributes:
!   language: fortran 90
!   machine: workstations
!
!$$$
   character  ::  bbuf(*)
   character  ::  a(nb)

!   if(ib.gt.isize) then
   if(ib+8.gt.isize) then
      ka=0
      return
   endif
   ka=min(nb,isize)
   do i=1,ka
      a(i)=bbuf(i+ib-1)
      !  a(i)=bbuf(i+ib)
   enddo
!
!     print *,'nbaread: isize,ib,nb,ka=',isize,ib,nb,ka
!
   return
   end
