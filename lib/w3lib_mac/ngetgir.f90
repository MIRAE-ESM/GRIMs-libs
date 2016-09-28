   subroutine ngetgir(bbuf,isize,msk1,msk2,mnum,mbuf,cbuf,nlen,                &
                      nnum,iret)
!$$$  subprogram documentation block
!
! subprogram: getgir         reads a grib index file
!   prgmmr: iredell          org: w/nmc23     date: 95-10-31
!
! abstract: read a grib file and return its index contents.
!   see subprogram ixgb for documentation of the index buffer structure.
!
! program history log:
!   95-10-31  iredell
!
! usage:    call getgir(bbuf,isize,msk1,msk2,mbuf,cbuf,nlen,nnum,iret)
!   input arguments:
!     bbuf         grib record
!     isize        size of grib record
!     msk1         integer number of bytes to search for first message
!     msk2         integer number of bytes to search for other messages
!     mnum         integer number of index records to skip (usually 0)
!     mbuf         integer length of cbuf in bytes
!   output arguments:
!     cbuf         character*1 (mbuf) buffer to receive index data
!     nlen         integer length of each index record in bytes
!     nnum         integer number of index records
!                  (=0 if no grib messages are found)
!     iret         integer return code
!                    0      all ok
!                    1      cbuf too small to hold index data
!
! subprograms called:
!   skgb           seek next grib message
!   ixgb           make index record
!
! remarks: subprogram can be called from a multiprocessing environment.
!   do not engage the same logical unit from more than one processor.
!
! attributes:
!   language: fortran 77
!   machine:  cray, workstations
!
!$$$
   character           ::  bbuf(isize)
   character           ::  cbuf(mbuf)
   integer, parameter  ::  lindex=152
   
   !
   !  search for first grib message
   !
   iseek=0
   call nskgb(bbuf,isize,iseek,msk1,lskip,lgrib)
   do m=1,mnum
      if(lgrib.gt.0) then
         iseek=lskip+lgrib
         call nskgb(buf,isize,iseek,msk2,lskip,lgrib)
      endif
   enddo
   
   !
   !  make an index record for every grib record found
   !
   nlen=lindex
   nnum=0
   iret=0
   do while(iret.eq.0.and.lgrib.gt.0)
      if(nlen*(nnum+1).le.mbuf) then
         nnum=nnum+1
         call nixgb(bbuf,isize,lskip,lgrib,nlen,nnum,cbuf)
         iseek=lskip+lgrib
         call nskgb(bbuf,isize,iseek,msk2,lskip,lgrib)
      else
         iret=1
     endif
   enddo

   return
   end
