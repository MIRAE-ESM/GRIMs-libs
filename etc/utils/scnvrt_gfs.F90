   program scnvrt_gfs
!
!  main program documentation block
!
! main program:  scnvrt  convert gsm/rsm sigma and sfc files to 
!                        machine native form
!
! prgmmr: kanamitsu     org: w/np51     date: 01-03-31
!
! abstract: convert data and change header idate,fhour if needed
!
! program history log:
!   01-03-31  henry juang
!   03-08-08  masao Kanamitsu (generalized)
!   11-01-01  B.-K. Kim
!
! input files:
!   unit   11    sigma file to convert
!   unit   12    sfc   file to convert
!
! output files:
!   unit   51   sigma file converted
!   unit   52   sfc file converted
!
! subprograms called:
!   sfcfld
!
! attributes:
!   language: fortran
!
   character(len=8)     ::  on85lab(4)
!
!  gsm second header extra words
!
   integer,parameter    ::  kdum=201,kdum2=21,kens=2
   integer,parameter    ::  sfcio_intkind=4
   integer,parameter    ::  sfcio_realkind=4
   real                 ::  dummy(kdum),dummy2(kdum2),ensemble(kens)
!
!  rsm second header extra words
!
   integer,parameter    ::  levmax=100
   real                 ::  rdummy(2*levmax),ext(512-(6+2*levmax))
!
   integer, allocatable :: lev(:)
   character(len=8), allocatable :: gvar(:)
!
!  output array
!
   integer              ::  idate(4)
   real                 ::  akmdl(65),bkmdl(64)
   real, allocatable    ::  si(:),sl(:)
   real, allocatable    ::  array(:)
   real, allocatable    ::  grid(:,:)
!
   integer(sfcio_intkind)::  ivs,nhead,ndata,nresv(3)
   integer(sfcio_intkind)::  i4date(4)
   integer(sfcio_intkind)::  i4dim, j4dim, l4soil, l4realf
   integer(4),allocatable::  lpl(:)
   real*4                ::  r4dummy(kdum),r4dummy2(kdum2),r4ensemble(kens)
   real*4                ::  r4rdummy(2*levmax),r4ext(512-(6+2*levmax))
   real*4 ,allocatable  ::  r4si(:),r4sl(:)
   real*4 ,allocatable  ::  r4array(:)
   real*4 ,allocatable  ::  r4grid(:,:)
   real*4 ,allocatable  ::  zero(:,:)
   real*4 ,allocatable  ::  r4slmsk(:,:), r4orog(:,:),r4tsea(:,:),r4sheleg(:,:)
   real*4 ,allocatable  ::  r4tg3(:,:), r4zorl(:,:),r4alvsf(:,:),r4alvwf(:,:)
   real*4 ,allocatable  ::  r4alnsf(:,:), r4alnwf(:,:),r4vfrac(:,:),r4canopy(:,:)
   real*4 ,allocatable  ::  r4f10m(:,:), r4t2m(:,:),r4q2m(:,:),r4vtype(:,:)
   real*4 ,allocatable  ::  r4stype(:,:), r4facsf(:,:),r4facwf(:,:),r4uustar(:,:)
   real*4 ,allocatable  ::  r4ffmm(:,:), r4ffhh(:,:),r4hice(:,:),r4fice(:,:)
   real*4 ,allocatable  ::  r4tisfc(:,:), r4tprcp(:,:),r4srflag(:,:),r4snwdph(:,:)
   real*4 ,allocatable  ::  r4shdmin(:,:), r4shdmax(:,:),r4slope(:,:),r4snoalb(:,:)
   real*4 ,allocatable  ::  r4stc(:,:), r4smc(:,:),r4slc(:,:)
   real*4 ,allocatable  ::  r4cv(:,:), r4cvb(:,:),r4cvt(:,:)

   real(sfcio_realkind) ::  r4fhour
   real*4               ::  r4waves,r4xlayers,r4trun
   real*4               ::  r4order,r4realform,r4gencode
   real*4               ::  r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4water
   real*4               ::  r4subcen,r4ppid,r4slid,r4vcid,r4vmid,r4vtid
   real*4               ::  r4runid,r4usrid,r4pdryini,r4gases
!
   character(len=3)     ::  gsm0rsm
   character(len=4)     ::  sfcftyp
   character(len=4)     ::  cgfs,csfc
   character(len=8)     ::  infmt,ofmt
   data newyr,newmo,newdy,newhr,fhnew/-1,-1,-1,-1,-1./
   data infmt,ofmt/'bin','bin'/
   data jcap/0/
   data issfc,issig/1,1/
   data akmdl/0.000,0.000,0.575,5.741,21.516,55.712,116.899,214.015,356.223,552.720, &
              812.489,1143.988,1554.789,2051.150,2637.553,3316.217,4086.614,4945.029, &
              5884.206,6893.117,7956.908,9057.051,10171.712,11276.348,12344.490,13348.671, &
              14261.435,15056.342,15708.893,16197.315,16503.144,16611.603,16511.736,16197.967, &
              15683.489,14993.074,14154.316,13197.065,12152.937,11054.853,9936.614,8832.537, &
              7777.150,6804.874,5937.050,5167.146,4485.493,3883.052,3351.460,2883.038,2470.788, &
              2108.366,1790.051,1510.711,1265.752,1051.080,863.058,698.457,554.424,428.434, &
              318.266,221.958,137.790,64.247,0.000/
  data bkmdl/1.00000000,0.99467119,0.98862660,0.98174229,0.97386760,0.96482757,0.95443411, &
             0.94249106,0.92879731,0.91315100,0.89535499,0.87522360,0.85259067,0.82731884, &
             0.79930974,0.76851468,0.73494523,0.69868292,0.65988704,0.61879962,0.57574665, &
             0.53113482,0.48544331,0.43921080,0.39301826,0.34746849,0.30316412,0.26068545, &
             0.22057019,0.18329624,0.14926877,0.11881219,0.09216691,0.06947458,0.05064684, &
             0.03544162,0.02355588,0.01463712,0.00829402,0.00410671,0.00163591,0.00043106, &
             0.00003697,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000,0.00000000, &
             0.00000000/
!
   namelist/namcnv/ gsm0rsm,sfcftyp,jcap,idim,jdim,kdim,                       &
                    infmt,ofmt,                                                &
                    newyr,newmo,newdy,newhr,fhnew,issig,issfc
!
! gsm0rsm
!   'gsm'
!   'rsm'
!
! sfcftyp
!   'osu1'
!   'osu2'
!   'noa1'
!
! infmt  
!   'ascii' -o asc ...  ascii to binary conversion
!   'ieee'         ...  ieee f77 to binary conversion
!   'ieee_dp'      ...  ieee_dp f77 to binary conversion
!   'ieee_sgi'     ...  ieee_dp f77 for real but ieee for integer
!   'bin'          ...  machine native binary (see same option for ofmt)
! ofmt
!   'ascii' -o asc ... binary to ascii
!   'ieee '        ... binary to ieee
!   'bin'          ... binary (machine dependent)
!
!  output format dependent on compilation option
!  (i.e., no word length decleration for output variables and
!  arrays. normally double precision real and single precision
!  integer for sgi, sun but double precision real and integer
!  for hp and dec)
!
   read(5,namcnv)
   write(6,namcnv)
!
   ijdim=idim*jdim
   nwave=(jcap+1)*(jcap+2)
!
! allocate data
   allocate (si(kdim+1))
   allocate (sl(kdim  ))
   allocate (lpl(jdim/2))
   allocate (r4slmsk(ijdim,1))
   allocate (r4orog(ijdim,1))
   allocate (r4tsea(ijdim,1))
   allocate (r4sheleg(ijdim,1))
   allocate (r4tg3(ijdim,1))
   allocate (r4zorl(ijdim,1))
   allocate (r4alvsf(ijdim,1))
   allocate (r4alvwf(ijdim,1))
   allocate (r4alnsf(ijdim,1))
   allocate (r4alnwf(ijdim,1))
   allocate (r4vfrac(ijdim,1))
   allocate (r4canopy(ijdim,1))
   allocate (r4f10m(ijdim,1))
   allocate (r4t2m(ijdim,1))
   allocate (r4q2m(ijdim,1))
   allocate (r4vtype(ijdim,1))
   allocate (r4stype(ijdim,1))
   allocate (r4facsf(ijdim,1))
   allocate (r4facwf(ijdim,1))
   allocate (r4uustar(ijdim,1))
   allocate (r4ffmm(ijdim,1))
   allocate (r4ffhh(ijdim,1))
   allocate (r4hice(ijdim,1))
   allocate (r4fice(ijdim,1))
   allocate (r4tisfc(ijdim,1))
   allocate (r4tprcp(ijdim,1))
   allocate (r4srflag(ijdim,1))
   allocate (r4snwdph(ijdim,1))
   allocate (r4shdmin(ijdim,1))
   allocate (r4shdmax(ijdim,1))
   allocate (r4slope(ijdim,1))
   allocate (r4snoalb(ijdim,1))
   allocate (r4stc(ijdim,4))
   allocate (r4smc(ijdim,4))
   allocate (r4slc(ijdim,4))
   allocate (r4cv(ijdim,1))
   allocate (r4cvb(ijdim,1))
   allocate (r4cvt(ijdim,1))

!
   if(infmt(1:4).eq.'ieee'.and.infmt(1:7).ne.'ieee_dp'.and.                    &
         infmt(1:8).ne.'ieee_sgi') then
       infmt(5:8)='_reg'
   endif
!
   print *,'modified infmt=',infmt
   print *,'ofmt=',ofmt
!
!  global model second header record default
!
   do i=1,kdum
     dummy(i)=0.
   enddo
   waves=jcap
   xlayers=kdim
   trun=1.
   order=2.
   realform=1.
   rlond=idim
   rlatd=jdim
   rlonp=idim
   rlatp=jdim
   rlonr=idim
   rlatr=jdim
   water=1.
   gases=0.
   pdryini=0.
   subcen=0.
   do i=1,kens
     ensemble(i)=0.
   enddo
   ppid=0.
   slid=0.
   vcid=0.
   vmid=0.
   vtid=0.
   do k=1,kdum2
     dummy2(k)=0.
   enddo

!
!  1. sigma file
!
if (issig.eq.1) then
   !
   print *,' '
   print *,'sigma file'
   print *,' '
!
!  1.1 first label record
!
   read(11) 
   !read(11) on85lab
   if(ofmt(1:3).eq.'bin') then
      write(51) ' emc ncep sigma surface file    '
   elseif(ofmt(1:5).eq.'ascii'.or.ofmt(1:3).eq.'asc' ) then
      write(51,100) on85lab
   elseif(ofmt(1:4).eq.'ieee') then
      write(51) ' emc ncep sigma surface file    '
   endif
!
!  1.2 second record fhour, idate, si, sl, and others
!
   nheadtyp=0
!
! 1.2.3 ieee_reg
!
   if(infmt(1:8).eq.'ieee_reg') then
     allocate (r4si(kdim+1),r4sl(kdim))
     if(gsm0rsm(1:3).eq.'gsm') then
       read(11,err=301) r4fhour,i4date,r4si,r4sl                               &
          ,(r4dummy(k),k=1,kdum-(kdim+1)-kdim)                                 &
          ,r4waves,r4xlayers,r4trun                                            &
          ,r4order,r4realform,r4gencode                                        &
          ,r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4water           &
          ,r4subcen,r4ensemble,r4ppid,r4slid,r4vcid,r4vmid,r4vtid              &
          ,r4runid,r4usrid,r4pdryini,r4dummy2,r4gases
       nheadtyp=2
       print *,'read header rec with water>1 or gases>0'
       go to 302
  301     continue
       rewind 11
       read(11) on85lab 
       read(11,err=303) r4fhour,i4date,r4si,r4sl                               &
          ,(r4dummy(k),k=1,kdum-(kdim+1)-kdim)                                 &
          ,r4waves,r4xlayers,r4trun                                            &
          ,r4order,r4realform,r4gencode                                        &
          ,r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4water           &
          ,r4subcen,r4ensemble,r4ppid,r4slid,r4vcid,r4vmid,r4vtid              &
          ,r4runid,r4usrid,r4dummy2
       print *, r4fhour,i4date,r4si,r4sl                               &
          ,(r4dummy(k),k=1,kdum-(kdim+1)-kdim)                                 &
          ,r4waves,r4xlayers,r4trun                                            &
          ,r4order,r4realform,r4gencode                                        &
          ,r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4water           &
          ,r4subcen,r4ensemble,r4ppid,r4slid,r4vcid,r4vmid,r4vtid              &
          ,r4runid,r4usrid,r4dummy2
       nheadtyp=1
       print *,'read header rec with water<=1 and gases=0'
       r4gases=0.
       r4pdryini=0.
  302  continue
       print *,' fhour,idate=',r4fhour,i4date
       if(r4water.eq.0.) then
         r4water=1.
         print *,'r4water reset to 1.'
       endif
       do i=1,kdum
         dummy(i)=r4dummy(i)
       enddo
       waves=r4waves
       xlayers=r4xlayers
       trun=r4trun
       order=r4order
       realform=r4realform
       gencode=r4gencode
       rlond=r4rlond
       rlatd=r4rlatd
       rlonp=r4rlonp
       rlatp=r4rlatp
       rlonr=r4rlonr
       rlatr=r4rlatr
!bbk       water=r4water
       water=1
       subcen=r4subcen
       do i=1,kens
         ensemble(i)=r4ensemble(i)
       enddo
       ppid=r4ppid
       slid=r4slid
       vcid=r4vcid
       vmid=r4vmid
       vtid=r4vtid
       runid=r4runid
       usrid=r4usrid
       pdryini=r4pdryini
       do k=1,kdum2
         dummy2(k)=r4dummy2(i)
       enddo
       gases=r4gases
       goto 304
303    continue
       rewind 11
       read(11) on85lab
       read(11,err=999) r4fhour,i4date,r4si,r4sl
       print *,'read old format fhour,idate=',r4fhour,i4date
304    continue
       fhour=r4fhour
       do i=1,4
         idate(i)=i4date(i)
       enddo
       do k=1,kdim+1
         si(k)=akmdl(k)
       enddo
       do k=1,kdim
         sl(k)=bkmdl(k)
       enddo
     else
      print *,'unknown model'
      call abort
     endif
   else
     print *,'illegal infmt.  must be one of ascii/ieee/ieee_dp'
     print *,'given infmt=',infmt
     call abort
   endif
!
   print *,'fhour,idate=',fhour,idate
   print *,'number of water,gases=',water,gases
!
   if (fhour.le.1.) fhour=0.
   if(newyr.ge.0 ) idate(4)=newyr
   if(newmo.ge.0 ) idate(2)=newmo
   if(newdy.ge.0 ) idate(3)=newdy
   if(newhr.ge.0 ) idate(1)=newhr
   if(fhnew.ge.0.) fhour=fhnew
   if (idate(4).lt.100) then
     if(idate(4).lt.30) then
        idate(4)=idate(4)+2000
     else
        idate(4)=idate(4)+1900
     endif
   endif
!
!  write header 
!
   nwater=nint(water)
   ngases=nint(gases)
   if(ofmt(1:3).eq.'bin') then
     if(gsm0rsm(1:3).eq.'gsm') then
       if(nwater.eq.1.and.ngases.eq.0) then
         write(51) fhour,idate,si,sl                                           &
          ,(dummy(k),k=1,201-(kdim+1)-kdim)                                    &
          ,waves,xlayers,trun,order,realform,gencode                           &
          ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,water                         &
          ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid                &
          ,dummy2
         write(6,*) fhour,idate,si,sl                                           &
          ,(dummy(k),k=1,201-(kdim+1)-kdim)                                    &
          ,waves,xlayers,trun,order,realform,gencode                           &
          ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,water                         &
          ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid                &
          ,dummy2
         print *,'second header rec written'
       else
         write(51)fhour,idate,si,sl                                            &
            ,(dummy(k),k=1,201-(kdim+1)-kdim)                                  &
            ,waves,xlayers,trun,order,realform,gencode                         &
            ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,water                       &
            ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid              &
            ,pdryini,dummy2,gases
       endif
     endif
   endif
!
! 1.3 sigma variables
!
   iwater=nint(water)
   igases=nint(gases)
!
   if(gsm0rsm(1:3).eq.'gsm') then
     narray=nwave
     nrecs=2+3*kdim+iwater*kdim+igases*kdim
   endif
   allocate (array(narray))
   if(infmt(1:8).eq.'ieee_reg') then
     allocate (r4array(narray))
   endif
   do k=1,nrecs
     if(infmt(1:8).eq.'ieee_reg') then
       read(11,end=900) r4array(1:narray)
       do n=1,narray
         array(n)=r4array(n)
       enddo
     endif
     if(ofmt(1:3).eq.'bin') then
       write(51) array
     endif
   call maxmin(array,narray,1,1,'array     ')
   enddo

   go to 901
  900 continue
   print *,'hit eof while reading sigma file'
   call abort
  901 continue
   deallocate (array)
   if(infmt(1:8).eq.'ieee_reg') then
     deallocate (r4array)
   endif
   !
   ! issig .eq. 1
   !
endif

!
!  2. sfc file
!
if (issfc.eq.1) then
   !
   print *,' '
   print *,'surface file'
   print *,' '
!
! 2.1  label
!
     read(12)  cgfs,csfc,ivs,nhead,ndata,nresv
     print *, 'sfc.. on85lab ', on85lab
     print *, 'sfc.. cgfs, csfc ', cgfs, csfc
     print *, 'sfc.. ivs ', ivs
     print *, 'sfc.. nhead ', nhead
     print *, 'sfc.. ndata ', ndata
     print *, 'sfc.. nresv ', nresv

   if(ofmt(1:3).eq.'bin') then
     write(52) ' emc ncep surface file          '
   elseif(ofmt(1:4).eq.'ieee') then
     write(52) ' emc ncep surface file          '
   endif
!
! 2.2 second header
!
     read(12) 
     read(12) r4fhour,i4date,i4dim,j4dim,l4soil,i4realf
     write(6,*)  'sfc.. fhour ', r4fhour
     write(6,*)  'sfc.. idate ', i4date
     write(6,*)  'sfc.. idim ', i4dim
     write(6,*)  'sfc.. jdim ', j4dim
     write(6,*)  'sfc.. lsoil ', l4soil
     write(6,*)  'sfc.. irealf ', i4realf
     read(12) lpl 
     write(6,*)  'sfc.. lpl ', lpl
     read(12) 
     fhour=r4fhour
   if (fhour.le.1.) fhour=0.
     do i=1,4
       idate(i)=i4date(i)
     enddo
   if(newyr.ge.0 ) idate(4)=newyr
   if(newmo.ge.0 ) idate(2)=newmo
   if(newdy.ge.0 ) idate(3)=newdy
   if(newhr.ge.0 ) idate(1)=newhr
   if(fhnew.ge.0.) fhour=fhnew
!
!  fix for 2 digit year to 4 digit year
!
   if (idate(4).lt.100) then
     if(idate(4).lt.30) then
        idate(4)=idate(4)+2000
     else
        idate(4)=idate(4)+1900
     endif
   endif
   if(ofmt(1:3).eq.'bin') then
     write(52) fhour,idate
     print *,  fhour,idate
   endif
!
! 2.3 body
!
   call sfcfld(sfcftyp,0,nrecs,lev,gvar,mxlv)
   print *,'sfcftyp=',sfcftyp,',nrecs=',nrecs,' maxlev=',mxlv
   allocate (lev(nrecs),gvar(nrecs))
   call sfcfld(sfcftyp,1,nrecs,lev,gvar,mxlv)
!
   allocate (grid(ijdim,mxlv),stat=ierr)
   allocate (zero(ijdim,mxlv),stat=ierr)
   zero=0.0
   if (ierr /= 0) then
      print*,'allocate grid error:',ierr
      call abort
   endif

   if(infmt(1:8).eq.'ieee_reg') then
     allocate (r4grid(ijdim,mxlv))
   endif
!
   read(12,end=909) (r4slmsk(ij,1),ij=1,ijdim)
   read(12,end=909) (r4orog(ij,1),ij=1,ijdim)
   read(12,end=909) (r4tsea(ij,1),ij=1,ijdim)
   read(12,end=909) (r4sheleg(ij,1),ij=1,ijdim)
   read(12,end=909) (r4tg3(ij,1),ij=1,ijdim)
   read(12,end=909) (r4zorl(ij,1),ij=1,ijdim)
   read(12,end=909) (r4alvsf(ij,1),ij=1,ijdim)
   read(12,end=909) (r4alvwf(ij,1),ij=1,ijdim)
   read(12,end=909) (r4alnsf(ij,1),ij=1,ijdim)
   read(12,end=909) (r4alnwf(ij,1),ij=1,ijdim)
   read(12,end=909) (r4vfrac(ij,1),ij=1,ijdim)
   read(12,end=909) (r4canopy(ij,1),ij=1,ijdim)
   read(12,end=909) (r4f10m(ij,1),ij=1,ijdim)
   read(12,end=909) (r4t2m(ij,1),ij=1,ijdim)
   read(12,end=909) (r4q2m(ij,1),ij=1,ijdim)
   read(12,end=909) (r4vtype(ij,1),ij=1,ijdim)
   read(12,end=909) (r4stype(ij,1),ij=1,ijdim)
   read(12,end=909) (r4facsf(ij,1),ij=1,ijdim)
   read(12,end=909) (r4facwf(ij,1),ij=1,ijdim)
   read(12,end=909) (r4uustar(ij,1),ij=1,ijdim)
   read(12,end=909) (r4ffmm(ij,1),ij=1,ijdim)
   read(12,end=909) (r4ffhh(ij,1),ij=1,ijdim)
   read(12,end=909) (r4hice(ij,1),ij=1,ijdim)
   read(12,end=909) (r4fice(ij,1),ij=1,ijdim)
   read(12,end=909) (r4tisfc(ij,1),ij=1,ijdim)
   read(12,end=909) (r4tprcp(ij,1),ij=1,ijdim)
   read(12,end=909) (r4srflag(ij,1),ij=1,ijdim)
   read(12,end=909) (r4snwdph(ij,1),ij=1,ijdim)
   read(12,end=909) (r4shdmin(ij,1),ij=1,ijdim)
   read(12,end=909) (r4shdmax(ij,1),ij=1,ijdim)
   read(12,end=909) (r4slope(ij,1),ij=1,ijdim)
   read(12,end=909) (r4snoalb(ij,1),ij=1,ijdim)
   do l=1,4
     read(12,end=909) (r4stc(ij,l),ij=1,ijdim)
   enddo
   write (6,*) 'stc ...'
   do l=1,4
     read(12,end=909) (r4smc(ij,l),ij=1,ijdim)
   enddo
   write (6,*) 'smc ...'
   do l=1,4
     read(12,end=909) (r4slc(ij,l),ij=1,ijdim)
   enddo
   write (6,*) 'slc ...'

! ts
  grid(:,1)=r4tsea(:,1)  ; write(52) grid ; grid=zero
! smc
  grid(:,:)=r4smc(:,:) ; write(52) grid ; grid=zero
! sno
  grid(:,1)=r4sheleg(:,1) ; write(52) grid ; grid=zero
! stc
  grid(:,:)=r4stc(:,:) ; write(52) grid ; grid=zero
! tg3
  grid(:,1)=r4tg3(:,1) ; write(52) grid ; grid=zero
! z0
  grid(:,1)=r4zorl(:,1) ; write(52) grid ; grid=zero
! cv
  grid(:,1)=zero(:,1) ; write(52) grid ; grid=zero 
! cvb
  grid(:,1)=zero(:,1) ; write(52) grid ; grid=zero 
! cvt
  grid(:,1)=zero(:,1) ; write(52) grid ; grid=zero 
! alb
  grid(:,1)=r4alvsf(:,1) ; grid(:,2)=r4alvwf(:,1) 
  grid(:,3)=r4alnsf(:,1) ; grid(:,4)=r4alnwf(:,1) 
  write(52) grid ; grid=zero
! sli
  grid(:,1)=r4slmsk(:,1) ; write(52) grid ; grid=zero 
! vfrac
  grid(:,1)=r4vfrac(:,1) ; write(52) grid ; grid=zero 
! canop
  grid(:,1)=r4canopy(:,1) ; write(52) grid ; grid=zero 
! f10m
  grid(:,1)=r4f10m(:,1) ; write(52) grid ; grid=zero 
! vegtyp
  grid(:,1)=r4vtype(:,1) ; write(52) grid ; grid=zero
! soiltyp
  grid(:,1)=r4stype(:,1) ; write(52) grid ; grid=zero 
! albf
  grid(:,1)=r4facsf(:,1) ; grid(:,2)=r4facwf(:,1)
  write(52) grid ; grid=zero 
! ustar
  grid(:,1)=r4uustar(:,1) ; write(52) grid ; grid=zero
! fm
  grid(:,1)=r4ffmm(:,1) ; write(52) grid ; grid=zero
! fh
  grid(:,1)=r4ffhh(:,1) ; write(52) grid ; grid=zero
! prcp
  grid(:,1)=r4tprcp(:,1) ; write(52) grid ; grid=zero 
! srflag
  grid(:,1)=r4srflag(:,1) ; write(52) grid ; grid=zero 
! snodph
  grid(:,1)=r4snwdph(:,1) ; write(52) grid ; grid=zero 
! slc
  grid(:,:)=r4slc(:,:) ; write(52) grid ; grid=zero
! shdmin
  grid(:,1)=r4shdmin(:,1) ; write(52) grid ; grid=zero 
! shdmax
  grid(:,1)=r4shdmax(:,1) ; write(52) grid ; grid=zero 
! slope
  grid(:,1)=r4slope(:,1) ; write(52) grid ; grid=zero 
! snoalb
  grid(:,1)=r4snoalb(:,1) ; write(52) grid ; grid=zero
! omld
  grid(:,1)=50.           ; write(52) grid ; grid=zero
! paer(5)
  write(52) grid
! kprfi
  write(52) grid
! denni(2)
  write(52) grid
! idxci(5)
  write(52) grid
! cmixi(5)
  write(52) grid
!
! issfc.eq.1
!
endif
!
100   format(32a1)
909   stop
   !
999   continue
   print *,'sigma file read error'
   call abort
   end program scnvrt_gfs
!
   subroutine maxmin(f,imax,jmax,kmax,title)
!-------------------------------------------------------------------------------
!
! subprogram: chgr_maxmin
!          
!-------------------------------------------------------------------------------
   real               ::  f(imax,jmax,kmax)
   character(len=8)   ::  title
!------------------------------------------------------------------------------
!
!   print 99, title
   99 format(2x,'title=',a8)
!
   do k=1,kmax
!
      fmax=f(1,1,k)
      iimax=1
      jjmax=1
      fmin=f(1,1,k)
      iimin=1
      jjmin=1
!
      do j=1,jmax
         do i=1,imax
            if(fmax.lt.f(i,j,k)) then
               fmax=f(i,j,k)
               iimax=i
               jjmax=j
            endif
            if(fmin.gt.f(i,j,k)) then
               fmin=f(i,j,k)
               iimin=i
               jjmin=j
            endif
         enddo
      enddo
!
      print 100, k,fmax,iimax,jjmax,fmin,iimin,jjmin
100   format(2x,'level=',i3,' max=',e10.4,' at i=',i5,' j=',i5,                &
             ' min=',e10.4,' at i=',i5,' j=',i5)
!
   enddo
!
   return
   end subroutine maxmin
