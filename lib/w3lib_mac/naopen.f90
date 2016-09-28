   subroutine naopen(lu,cfn,bbuf,isize,iret)
!$$$  subprogram documentation block
!
! subprogram: naopen         byte-addressable open
!   prgmmr: kanamistu        date: 04-02-06
!
! abstract: open a file  and move entire grib record to an array
!           to be accessed by naread 
!
! program history log:
!   04-02-06  kanamitsu     standard f90 version
!
! usage:    call naopen(lu,cfn,iret)
!   input arguments:
!     lu           integer unit to open
!     cfn          character (*) file name to open
!   output arguments:
!     iret         integer return code (0 if successful)
!
! attributes:
!   language: fortran 90
!
!$$$
   implicit none

   integer    ::  lu,iret,isize
   character  ::  bbuf(isize)
   character  ::  cfn*(*)

   integer    ::  i
   integer    ::  ier
   integer    ::  iy
   integer    ::  status(15)
   integer    ::  fstat  ! service function to get file info. 
   !
   ! reopen with the correct record length (=file size)
   !
   open(lu,file=cfn,iostat=iret,access='direct',recl=isize,                    &
        form='unformatted')
   read(lu,rec=1) bbuf
   close(lu)
!
!     print *,'bbuf=',(bbuf(i),i=1,100)
!
   return
   end
