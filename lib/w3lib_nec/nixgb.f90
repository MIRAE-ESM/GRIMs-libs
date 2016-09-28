   subroutine nixgb(bbuf,isize,lskip,lgrib,nlen,nnum,cbuf)
!$$$  subprogram documentation block
!
! subprogram: ixgb           make index record
!   prgmmr: iredell          org: w/nmc23     date: 95-10-31
!
! abstract: this subprogram makes one index record.
!       byte 001-004: bytes to skip in data file before grib message
!       byte 005-008: bytes to skip in message before pds
!       byte 009-012: bytes to skip in message before gds (0 if no gds)
!       byte 013-016: bytes to skip in message before bms (0 if no bms)
!       byte 017-020: bytes to skip in message before bds
!       byte 021-024: bytes total in the message
!       byte 025-025: grib version number
!       byte 026-053: product definition section (pds)
!       byte 054-095: grid definition section (gds) (or nulls)
!       byte 096-101: first part of the bit map section (bms) (or nulls)
!       byte 102-112: first part of the binary data section (bds)
!       byte 113-152: (optional) bytes 41-80 of the pds
!
! program history log:
!   95-10-31  iredell
!
! usage:    call wrgi1r(lugb,lskip,lgrib,lugi)
!   input arguments:
!     lugb         integer logical unit of input grib file
!     lskip        integer number of bytes to skip before grib message
!     lgrib        integer number of bytes in grib message
!     nlen         integer length of each index record in bytes
!     nnum         integer index record number to make
!   output arguments:
!     cbuf         character*1 (mbuf) buffer to receive index data
!
! subprograms called:
!   gbyte        get integer data from bytes
!   sbyte        store integer data in bytes
!   baread       byte-addressable read
!
! attributes:
!   language: cray fortran
!
!$$$
   integer, parameter  ::  lindex=152
   
   character           ::  bbuf(isize)
   character           ::  cbuf(*)
   character           ::  cbread(lindex),cindex(lindex)
   
   integer, parameter  ::  ixskp=0,ixspd=4,ixsgd=8,ixsbm=12,ixsbd=16,ixlen=20, &
                           ixver=24,ixpds=25,ixgds=53,ixbms=95,ixbds=101,      &
                           ixpdx=112
   integer, parameter  ::  mxskp=4,mxspd=4,mxsgd=4,mxsbm=4,mxsbd=4,mxlen=4,    &
                           mxver=1,mxpds=28,mxgds=42,mxbms=6,mxbds=11,         &
                           mxpdx=40
   !
   !  initialize index record and read grib message
   !
   do i=1,lindex
      cindex(i)=char(0)
   enddo
   call sbyte(cindex,lskip,8*ixskp,8*mxskp)
   call sbyte(cindex,lgrib,8*ixlen,8*mxlen)
   !
   !  put pds in index record
   !
   iskpds=8
   ibskip=lskip
   ibread=iskpds+mxpds
   call nbaread(bbuf,isize,ibskip,ibread,lbread,cbread)
   if(lbread.ne.ibread) return
   cindex(ixver+1)=cbread(8)
   call sbyte(cindex,iskpds,8*ixspd,8*mxspd)
   call gbyte(cbread,lenpds,8*iskpds,8*3)
   call gbyte(cbread,incgds,8*iskpds+8*7+0,1)
   call gbyte(cbread,incbms,8*iskpds+8*7+1,1)
   ilnpds=min(lenpds,mxpds)
   cindex(1)(ixpds+1:ixpds+ilnpds)=cbread(1)(iskpds+1:iskpds+ilnpds)
   isktot=iskpds+lenpds
   !
   !  put pds extension in index record
   !
   if(lenpds.gt.40) then
      iskpdx=iskpds+40
      ibskip=lskip+iskpdx
      ibread=mxpdx
      call nbaread(bbuf,isize,ibskip,ibread,lbread,cbread)
      if(lbread.ne.ibread) return
      ilnpdx=min(lenpds-40,mxpdx)
      cindex(1)(ixpdx+1:ixpdx+ilnpdx)=cbread(1)(1:ilnpdx)
   endif
   !
   !  put gds in index record
   !
   if(incgds.ne.0) then
      iskgds=isktot
      ibskip=lskip+iskgds
      ibread=mxgds
      call nbaread(bbuf,isize,ibskip,ibread,lbread,cbread)
      if(lbread.ne.ibread) return
      call sbyte(cindex,iskgds,8*ixsgd,8*mxsgd)
      call gbyte(cbread,lengds,0,8*3)
      ilngds=min(lengds,mxgds)
      cindex(1)(ixgds+1:ixgds+ilngds)=cbread(1)(1:ilngds)
      isktot=iskgds+lengds
   endif
   !
   !  put bms in index record
   !
   if(incbms.ne.0) then
      iskbms=isktot
      ibskip=lskip+iskbms
      ibread=mxbms
      call nbaread(bbuf,isize,ibskip,ibread,lbread,cbread)
      if(lbread.ne.ibread) return
      call sbyte(cindex,iskbms,8*ixsbm,8*mxsbm)
      call gbyte(cbread,lenbms,0,8*3)
      ilnbms=min(lenbms,mxbms)
      cindex(1)(ixbms+1:ixbms+ilnbms)=cbread(1)(1:ilnbms)
      isktot=iskbms+lenbms
   endif
   !
   !  put bds in index record
   !
   iskbds=isktot
   ibskip=lskip+iskbds
   ibread=mxbds
   call nbaread(bbuf,isize,ibskip,ibread,lbread,cbread)
   if(lbread.ne.ibread) return
   call sbyte(cindex,iskbds,8*ixsbd,8*mxsbd)
   call gbyte(cbread,lenbds,0,8*3)
   ilnbds=min(lenbds,mxbds)
   cindex(1)(ixbds+1:ixbds+ilnbds)=cbread(1)(1:ilnbds)
   !
   !  store index record
   !
   nskip=nlen*(nnum-1)
   nstore=min(nlen,lindex)
   cbuf(1)(nskip+1:nskip+nstore)=cindex(1)(1:nstore)
   cbuf(1)(nskip+nstore+1:nskip+nlen)=char(0)

   return
   end
