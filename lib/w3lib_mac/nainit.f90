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
   integer   :: lu, iret
   integer   :: isize
   integer   :: status(20)
   integer   :: stat ! service function to get file info. 
!
   open(lu,file=cfn,iostat=iret,access='direct',recl=1,                        &
        form='unformatted',err=999)
   print *,'file ',cfn,' opened. lu=',lu
!
   iret=stat(cfn//char(0),status)
!     print *,'status=',status
   if(iret.lt.0) then
      print *,'stat error'
      call abort
   endif
!
   isize=status(8)
   print *,'filesize=',isize
!
   close( lu )
   return
  999 continue
   print *,'file ',cfn,' open failed'
   call abort
   end
