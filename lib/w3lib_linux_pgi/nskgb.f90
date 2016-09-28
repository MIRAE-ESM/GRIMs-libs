   subroutine nskgb(bbuf,isize,iseek,mseek,lskip,lgrib)
!$$$  subprogram documentation block
!
! subprogram: skgb           search for next grib message
!   prgmmr: iredell          org: w/nmc23     date: 93-11-22
!
! abstract: this subprogram searches a file for the next grib 1 message.
!   a grib 1 message is identified by its indicator section, i.e.
!   an 8-byte sequence with 'grib' in bytes 1-4 and 1 in byte 8.
!   if found, the length of the message is decoded from bytes 5-7.
!   the search is done over a given section of the file.
!   the search is terminated if an eof or i/o error is encountered.
!
! program history log:
!   93-11-22  iredell
!   95-10-31  iredell   add call to nbaread 
!
! usage:   call nskgb(bbuf,isize,iseek,mseek,lskip,lgrib) 
!   input arguments:
!     bbuf         grib record
!     isize        size of grib record
!     iseek        integer number of bytes to skip before search
!     mseek        integer maximum number of bytes to search
!   output arguments:
!     lskip        integer number of bytes to skip before message
!     lgrib        integer number of bytes in message (0 if not found)
!
! subprograms called:
!   nbaread       byte-addressable read
!   gbyte        get integer data from bytes
!
! attributes:
!   language: fortran
!
!$$$
   integer, parameter  ::  lseek=128
   character           ::  bbuf(isize)
   character           ::  z(lseek)

   lgrib=0
   ks=iseek
   kn=min(lseek,mseek)
   kz=lseek
   do while(lgrib.eq.0.and.kn.ge.8.and.kz.eq.lseek)
      call nbaread(bbuf,isize,ks,kn,kz,z)
      kz=min(isize-ks+1,kz)
      km=kz-8+1
      k=0
      do while(lgrib.eq.0.and.k.lt.km)
         call gbyte(z,i4,(k+0)*8,4*8)
         call gbyte(z,i1,(k+7)*8,1*8)
         if(i4.eq.1196575042.and.i1.eq.1) then
            lskip=ks+k
            call gbyte(z,lgrib,(k+4)*8,3*8)
         endif
         k=k+1
     enddo
     ks=ks+km
     kn=min(lseek,iseek+mseek-ks)
   enddo

   return
   end
