   subroutine nrdgb(bbuf,isize,lgrib,lskip,kpds,kgds,ndata,lbms,               &
                    data,luptr)
!
!  read grib file
!  input
!    bbuf  - total grib record in array
!    isize - size of grib record
!    lgrib - length of grib record
!    lskip - bytes to skip for grib record
!  output
!    kpds(22) - unpacked product definition section
!    kgds(22) - unpacked grid definition section
!    ndata    - number of data points
!    lbms(ndata) - logical bit map
!    data(ndata) - data unpacked
!
   character  ::  bbuf(isize)
   character  ::  grib(isize)*1
   integer    ::  kpds(25),kgds(22),kptr(16)
   logical*1  ::  lbms(*)
   real       ::  data(*)
   ndata=0
   call nbaread(bbuf,isize,lskip,lgrib,lread,grib)
   if(lread.lt.lgrib) then
      write(luptr,*) ' error in rdgb.  lread.lt.lgrib'
      call abort
   endif
   call w3fi63(grib,kpds,kgds,lbms,data,kptr,iret)
   if(iret.ne.0) then
      write(luptr,*) ' error in rdgb.  iret.ne.0 from w3fi63'
      write(luptr,*) ' iret=',iret
      call abort
   endif
   ndata=kptr(10)
   return
   end
