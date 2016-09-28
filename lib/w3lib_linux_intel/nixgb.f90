   subroutine nixgb(bbuf,isize,lskip,lgrib,nlen,nnum,cbuf)
!$$$  subprogram documentation block
!
! subprogram: nixgb           make index record
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
!   04-02-06  kanamitsu  more efficient version
!
! usage:    call nixgb(bbuf,isize,lskip,lgrib,lugi)
!   input arguments:
!     bbuf         grib record
!     isize        size of grib record
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
!   nbaread       byte-addressable read
!
! attributes:
!   language: cray fortran
!
!$$$
   character bbuf(isize)
   character cbuf(*)
   parameter(lindex=152)
   parameter(ixskp=0,ixspd=4,ixsgd=8,ixsbm=12,ixsbd=16,ixlen=20,               &
             ixver=24,ixpds=25,ixgds=53,ixbms=95,ixbds=101,                    &
             ixpdx=112)
   parameter(mxskp=4,mxspd=4,mxsgd=4,mxsbm=4,mxsbd=4,mxlen=4,                  &
             mxver=1,mxpds=28,mxgds=42,mxbms=6,mxbds=11,                       &
             mxpdx=40)
   character cbread(lindex),cindex(lindex)
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
   if(lbread.ne.ibread) call abort
   cindex(ixver+1)=cbread(8)
   call sbyte(cindex,iskpds,8*ixspd,8*mxspd)
   call gbyte(cbread,lenpds,8*iskpds,8*3)
   call gbyte(cbread,incgds,8*iskpds+8*7+0,1)
   call gbyte(cbread,incbms,8*iskpds+8*7+1,1)
   ilnpds=min(lenpds,mxpds)
   do i=1,ilnpds
    cindex(ixpds+i)=cbread(iskpds+i)
   enddo

   isktot=iskpds+lenpds
   ! 
   !  put pds extension in index record
   !
   if(lenpds.gt.40) then
     iskpdx=iskpds+40
     ibskip=lskip+iskpdx
     ibread=mxpdx
     call nbaread(bbuf,isize,ibskip,ibread,lbread,cbread)
     if(lbread.ne.ibread) call abort
     ilnpdx=min(lenpds-40,mxpdx)
     do i=1,ilnpdx
      cindex(ixpdx+i)=cbread(i)
     enddo
   endif
   !
   !  put gds in index record
   !
   if(incgds.ne.0) then
     iskgds=isktot
     ibskip=lskip+iskgds
     ibread=mxgds
     call nbaread(bbuf,isize,ibskip,ibread,lbread,cbread)
     if(lbread.ne.ibread) call abort
     call sbyte(cindex,iskgds,8*ixsgd,8*mxsgd)
     call gbyte(cbread,lengds,0,8*3)
     ilngds=min(lengds,mxgds)
     do i=1,ilngds
       cindex(ixgds+i)=cbread(i)
     enddo
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
     if(lbread.ne.ibread) call abort
     call sbyte(cindex,iskbms,8*ixsbm,8*mxsbm)
     call gbyte(cbread,lenbms,0,8*3)
     ilnbms=min(lenbms,mxbms)
     do i=1,ilnbms
       cindex(ixbms+i)=cbread(i)
     enddo
     isktot=iskbms+lenbms
   endif
   !
   !  put bds in index record
   !
   iskbds=isktot
   ibskip=lskip+iskbds
   ibread=mxbds
   call nbaread(bbuf,isize,ibskip,ibread,lbread,cbread)
   if(lbread.ne.ibread) call abort
   call sbyte(cindex,iskbds,8*ixsbd,8*mxsbd)
   call gbyte(cbread,lenbds,0,8*3)
   ilnbds=min(lenbds,mxbds)
   do i=1,ilnbds
    cindex(ixbds+i)=cbread(i)
   enddo
   !
   !  store index record
   !
   nskip=nlen*(nnum-1)
   nstore=min(nlen,lindex)
   do i=1,nstore
    cbuf(nskip+i)=cindex(i)
   enddo
   do i=1,nlen-nstore
    cbuf(nskip+nstore+i)=char(0)
   enddo

   return
   end
