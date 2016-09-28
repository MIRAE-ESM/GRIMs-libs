   subroutine nainit(lu,cfn,isize,iret)
!$$$  subprogram documentation block
!
! subprogram: nainit         find grib file size
!   prgmmr: kanamistu        date: 04-02-06
!
! abstract: open a file  and get file size
!
! program history log:
!   04-02-06  kanamitsu     standard f90 version
!
! usage:    call naopen(lu,cfn,iret)
!   input arguments:
!     lu           integer unit to open
!     cfn          character (*) file name to open
!   output arguments:
!     isize        size of the file in byte
!     iret         integer return code (0 if successful)
!
! attributes:
!   language: fortran 90
!
!$$$
   implicit none
   character :: cfn*(*)
   integer   :: isize,lu,iret
   integer   :: ilen, jstat

   call pxfstructcreate('stat',jstat,iret)
   if (iret .ne. 0) then
      write(6,*)'Error in nainit for pxfstructcreate'
      call exit(1)
   endif
   ilen=0
   call pxfstat(cfn, ilen, jstat, iret)
   if (iret.ne.0) then
      print *,'FAIL: error from pxfstat = ',iret
      call exit(1)
   endif
   call pxfintget(jstat,'st_size',isize,iret)
   call pxfstructfree(jstat,iret)
   write(6,*)'file size is ',isize

   return
   end
