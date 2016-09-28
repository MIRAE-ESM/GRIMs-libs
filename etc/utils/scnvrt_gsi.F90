   program scnvrt
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
!   13-04-25  B.B.K. (modified for GSI)
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
   integer,parameter    ::  ivs=200412
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
   integer              ::  lpl62(47)
   integer              ::  lpl254(192)
   real, allocatable    ::  si(:),sl(:)
   real, allocatable    ::  array(:)
   real, allocatable    ::  grid(:,:)
   real ,allocatable    ::  orog(:,:)
   real ,allocatable    ::  guess(:,:)
!
   integer*4            ::  i4date(4)
   integer*4,allocatable::  lpl(:)
   real*4               ::  r4dummy(kdum),r4dummy2(kdum2),r4ensemble(kens)
   real*4               ::  r4rdummy(2*levmax),r4ext(512-(6+2*levmax))
   real*4               ::  r4fhour
   real*4               ::  r4waves,r4xlayers,r4trun
   real*4               ::  r4order,r4realform,r4gencode
   real*4               ::  r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4water
   real*4               ::  r4subcen,r4ppid,r4slid,r4vcid,r4vmid,r4vtid
   real*4               ::  r4runid,r4usrid,r4pdryini,r4gases
   real*4 ,allocatable  ::  r4si(:),r4sl(:)
   real*4 ,allocatable  ::  r4array(:)
   real*4 ,allocatable  ::  r4grid(:,:)
   real*4 ,allocatable  ::  zero(:,:)
   real*4 ,allocatable  ::  r4slmsk (:,:), r4orog  (:,:), r4tsea  (:,:), r4sheleg(:,:)
   real*4 ,allocatable  ::  r4tg3   (:,:), r4zorl  (:,:), r4alvsf (:,:), r4alvwf (:,:)
   real*4 ,allocatable  ::  r4alnsf (:,:), r4alnwf (:,:), r4vfrac (:,:), r4canopy(:,:)
   real*4 ,allocatable  ::  r4f10m  (:,:), r4t2m   (:,:), r4q2m   (:,:), r4vtype (:,:)
   real*4 ,allocatable  ::  r4stype (:,:), r4facsf (:,:), r4facwf (:,:), r4uustar(:,:)
   real*4 ,allocatable  ::  r4ffmm  (:,:), r4ffhh  (:,:), r4hice  (:,:), r4fice  (:,:)
   real*4 ,allocatable  ::  r4tisfc (:,:), r4tprcp (:,:), r4srflag(:,:), r4snwdph(:,:)
   real*4 ,allocatable  ::  r4shdmin(:,:), r4shdmax(:,:), r4slope (:,:), r4snoalb(:,:)
   real*4 ,allocatable  ::  r4stc   (:,:), r4smc   (:,:), r4slc   (:,:)
   real*4 ,allocatable  ::  r4cv    (:,:), r4cvb   (:,:), r4cvt   (:,:)
!
   character(len=3)     ::  gsm0rsm
   character(len=4)     ::  sfcftyp
   character(len=8)     ::  infmt,ofmt
   data newyr,newmo,newdy,newhr,fhnew/-1,-1,-1,-1,-1./
   data infmt,ofmt/'bin','bin'/
   data jcap/0/
   data issfc,issig/1,1/
   data lpl62/ 30, 30, 30, 40, 48, 56, 60, 72, 72, 80, 90, 90, 96,110,110,120, &
              120,128,144,144,144,144,154,160,160,168,168,180,180,180,180,180, &
              180,192,192,192,192,192,192,192,192,192,192,192,192,192,192/
   data lpl254/  64,   64,   64,   64,   64,   64,   72,   72,   80,   90, &
                 96,  110,  110,  120,  120,  128,  144,  144,  154,  160, &
                168,  180,  180,  180,  192,  192,  210,  220,  220,  240, &
                240,  240,  240,  252,  256,  280,  280,  280,  288,  288, &
                288,  308,  308,  320,  320,  320,  330,  360,  360,  360, &
                360,  360,  360,  384,  384,  384,  384,  420,  420,  420, &
                440,  440,  440,  440,  440,  440,  462,  462,  462,  480, &
                480,  480,  480,  480,  480,  504,  504,  504,  504,  512, &
                512,  560,  560,  560,  560,  560,  560,  576,  576,  576, &
                576,  576,  576,  576,  576,  616,  616,  616,  616,  616, &
                616,  640,  640,  640,  640,  640,  640,  640,  640,  640, &
                640,  660,  660,  660,  720,  720,  720,  720,  720,  720, &
                720,  720,  720,  720,  720,  720,  720,  720,  720,  720, &
                720,  720,  720,  720,  720,  720,  720,  720,  768,  768, &
                768,  768,  768,  768,  768,  768,  768,  768,  768,  768, &
                768,  768,  768,  768,  768,  768,  768,  768,  768,  768, &
                768,  768,  768,  768,  768,  768,  768,  768,  768,  768, &
                768,  768,  768,  768,  768,  768,  768,  768,  768,  768, &
                768,  768,  768,  768,  768,  768,  768,  768,  768,  768, &
                768,  768/
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
   call sfcfld(sfcftyp,0,nrecs,lev,gvar,mxlv)
   allocate (lev(nrecs),gvar(nrecs))
   call sfcfld(sfcftyp,1,nrecs,lev,gvar,mxlv)
   allocate (orog(ijdim,1),stat=ierr)
   allocate (guess(ijdim,1),stat=ierr)
   allocate (grid(ijdim,mxlv),stat=ierr)
   allocate (r4grid(ijdim,mxlv))
   allocate (zero(ijdim,mxlv),stat=ierr)
   allocate (si(kdim+1),stat=ierr)
   allocate (sl(kdim  ),stat=ierr)
   allocate (lpl(jdim/2),stat=ierr)
   allocate (r4slmsk(ijdim,1),stat=ierr)
   allocate (r4orog(ijdim,1),stat=ierr)
   allocate (r4tsea(ijdim,1),stat=ierr)
   allocate (r4sheleg(ijdim,1),stat=ierr)
   allocate (r4tg3(ijdim,1),stat=ierr)
   allocate (r4zorl(ijdim,1),stat=ierr)
   allocate (r4alvsf(ijdim,1),stat=ierr)
   allocate (r4alvwf(ijdim,1),stat=ierr)
   allocate (r4alnsf(ijdim,1),stat=ierr)
   allocate (r4alnwf(ijdim,1),stat=ierr)
   allocate (r4vfrac(ijdim,1),stat=ierr)
   allocate (r4canopy(ijdim,1),stat=ierr)
   allocate (r4f10m(ijdim,1),stat=ierr)
   allocate (r4t2m(ijdim,1),stat=ierr)
   allocate (r4q2m(ijdim,1),stat=ierr)
   allocate (r4vtype(ijdim,1),stat=ierr)
   allocate (r4stype(ijdim,1),stat=ierr)
   allocate (r4facsf(ijdim,1),stat=ierr)
   allocate (r4facwf(ijdim,1),stat=ierr)
   allocate (r4uustar(ijdim,1),stat=ierr)
   allocate (r4ffmm(ijdim,1),stat=ierr)
   allocate (r4ffhh(ijdim,1),stat=ierr)
   allocate (r4hice(ijdim,1),stat=ierr)
   allocate (r4fice(ijdim,1),stat=ierr)
   allocate (r4tisfc(ijdim,1),stat=ierr)
   allocate (r4tprcp(ijdim,1),stat=ierr)
   allocate (r4srflag(ijdim,1),stat=ierr)
   allocate (r4snwdph(ijdim,1),stat=ierr)
   allocate (r4shdmin(ijdim,1),stat=ierr)
   allocate (r4shdmax(ijdim,1),stat=ierr)
   allocate (r4slope(ijdim,1),stat=ierr)
   allocate (r4snoalb(ijdim,1),stat=ierr)
   allocate (r4stc(ijdim,4),stat=ierr)
   allocate (r4smc(ijdim,4),stat=ierr)
   allocate (r4slc(ijdim,4),stat=ierr)
   allocate (r4cv(ijdim,1),stat=ierr)
   allocate (r4cvb(ijdim,1),stat=ierr)
   allocate (r4cvt(ijdim,1),stat=ierr)
   if (ierr /= 0) then
      print*,'allocate grid error:',ierr
      call abort
   endif
   if (jcap.eq.62 ) lpl(:)=lpl62(:)
   if (jcap.eq.254 ) lpl(:)=lpl254(:)
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
   if(infmt(1:5).eq.'ascii'.or.infmt(1:3).eq.'asc' ) then
     read(11,100)
     if(ofmt(1:3).eq.'bin') then
       write(51) ' emc ncep sigma surface file    '
     elseif(ofmt(1:5).eq.'ascii'.or.ofmt(1:3).eq.'asc' ) then
       write(51,100) ' emc ncep sigma surface file    '
     elseif(ofmt(1:4).eq.'ieee') then
       write(51) ' emc ncep sigma surface file    '
     endif
   else
     read(11) 
     if(ofmt(1:3).eq.'bin') then
        write(51) ' emc ncep sigma surface file    '
     elseif(ofmt(1:5).eq.'ascii'.or.ofmt(1:3).eq.'asc' ) then
        write(51,100) on85lab
     elseif(ofmt(1:4).eq.'ieee') then
        write(51) ' emc ncep sigma surface file    '
     endif
   endif
!
!  1.2 second record fhour, idate, si, sl, and others
!
!!!
!!! read GMP guess file for GSI
!!!
   if(infmt(1:3).eq.'bin') then
     if(gsm0rsm(1:3).eq.'gsm') then
       read(11,err=201) fhour,idate,si,sl                                      &
          ,(dummy(k),k=1,kdum-(kdim+1)-kdim)                                   &
          ,waves,xlayers,trun,order,realform,gencode                           &
          ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,water                           &
          ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid                &
          ,pdryini,dummy2,gases
       print *,'read header rec with water>1 or gases>0'
       goto 202
  201  continue
       rewind 11
       read(11) on85lab
       read(11,err=203) fhour,idate,si,sl                                      &
          ,(dummy(k),k=1,kdum-(kdim+1)-kdim)                                   &
          ,waves,xlayers,trun,order,realform,gencode                           &
          ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,water                         &
          ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid                &
          ,dummy2
       print *,'read header rec with water<=1 and gases=0'
  202  continue
       print *,'fhour, idate =',  fhour,idate                                  
       print *,'si, sl =',  si,sl         
       print *,'waves     =',  waves      
       print *,'xlayers   =',  xlayers    
       print *,'trun      =',  trun       
       print *,'order     =',  order      
       print *,'realform  =',  realform   
       print *,'gencode   =',  gencode    
       print *,'rlond     =',  rlond      
       print *,'rlatd     =',  rlatd      
       print *,'rlonp     =',  rlonp      
       print *,'rlatp     =',  rlatp      
       print *,'rlonr     =',  rlonr      
       print *,'rlatr     =',  rlatr      
       print *,'water     =',  water      
       print *,'subcen    =',  subcen     
       print *,'ensemble  =',  ensemble   
       print *,'ppid      =',  ppid       
       print *,'slid      =',  slid       
       print *,'vcid      =',  vcid       
       print *,'vmid      =',  vmid       
       print *,'vtid      =',  vtid       
       print *,'runid     =',  runid      
       print *,'usrid     =',  usrid      
       print *,'pdryini   =',  pdryini    
       print *,'gases     =',  gases      
       if(water.eq.0.) then
         water=1.
         print *,'water reset to 1.'
       endif
       if(gases.eq.1.) then
         water=2.
         gases=0.
       endif
       go to 204
  203  continue
       rewind 11
       read(11,err=999) on85lab
       read(11,err=999) fhour,idate,si,sl
       print *,'read short format'
  204  continue
     elseif(gsm0rsm(1:3).eq.'rsm') then
       read(11) fhour,idate,si,sl                                              &
               ,(rdummy(i),i=1,2*levmax+1-kdim-1-kdim),ext
     else
      print *,'unknown model'
      call abort
     endif
!!!
!!! read GSI analysis for GMP
!!!
   elseif(infmt(1:8).eq.'ieee_reg') then
     allocate (r4si(kdim+1),r4sl(kdim))
     if(gsm0rsm(1:3).eq.'gsm') then
       read(11,err=301) r4fhour,i4date,r4si,r4sl                               &
          ,(r4dummy(k),k=1,kdum-(kdim+1)-kdim)                                 &
          ,r4waves,r4xlayers,r4trun                                            &
          ,r4order,r4realform,r4gencode                                        &
          ,r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4water           &
          ,r4subcen,r4ensemble,r4ppid,r4slid,r4vcid,r4vmid,r4vtid              &
          ,r4runid,r4usrid,r4pdryini,r4dummy2,r4gases
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
       print *,'read header rec with water<=1 and gases=0'
       r4gases=0.
       r4pdryini=0.
  302  continue
       print *,' fhour,idate=',fhour,idate
       print *,'r4si, r4sl =',  r4si,r4sl         
       print *,'r4waves     =',  r4waves      
       print *,'r4xlayers   =',  r4xlayers    
       print *,'r4trun      =',  r4trun       
       print *,'r4order     =',  r4order      
       print *,'r4realform  =',  r4realform   
       print *,'r4gencode   =',  r4gencode    
       print *,'r4rlond     =',  r4rlond      
       print *,'r4rlatd     =',  r4rlatd      
       print *,'r4rlonp     =',  r4rlonp      
       print *,'r4rlatp     =',  r4rlatp      
       print *,'r4rlonr     =',  r4rlonr      
       print *,'r4rlatr     =',  r4rlatr      
       print *,'r4water     =',  r4water      
       print *,'r4subcen    =',  r4subcen     
       print *,'r4ensemble  =',  r4ensemble   
       print *,'r4ppid      =',  r4ppid       
       print *,'r4slid      =',  r4slid       
       print *,'r4vcid      =',  r4vcid       
       print *,'r4vmid      =',  r4vmid       
       print *,'r4vtid      =',  r4vtid       
       print *,'r4runid     =',  r4runid      
       print *,'r4usrid     =',  r4usrid      
       print *,'r4pdryini   =',  r4pdryini    
       print *,'r4gases     =',  r4gases      
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
       water=r4water
       subcen=r4subcen
       do i=1,kens
         ensemble(i)=r4ensemble(i)
       enddo
       ppid=r4ppid
       slid=r4slid
!       vcid=r4vcid
       vcid=2
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
         si(k)=r4si(k)
       enddo
       do k=1,kdim
         sl(k)=r4sl(k)
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
   fhourin=fhour
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
!!!
!!! write with binary format for GMP
!!!
   if(ofmt(1:3).eq.'bin') then
     if(nwater.eq.1.and.ngases.eq.0) then
       write(51) fhour,idate,si,sl                                       &
        ,(dummy(k),k=1,201-(kdim+1)-kdim)                                &
        ,waves,xlayers,trun,order,realform,gencode                       &
        ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,water                       &
        ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid            &
        ,dummy2
       print *,'write header rec with water=1 or gases=0'
     else
       write(51)fhour,idate,si,sl                                        &
          ,(dummy(k),k=1,201-(kdim+1)-kdim)                              &
          ,waves,xlayers,trun,order,realform,gencode                     &
          ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,water                     &
          ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid          &
          ,pdryini,dummy2,gases
     endif
       print *,'fhour, idate =',  fhour,idate                                  
       print *,'si, sl =',  si,sl         
       print *,'waves     =',  waves      
       print *,'xlayers   =',  xlayers    
       print *,'trun      =',  trun       
       print *,'order     =',  order      
       print *,'realform  =',  realform   
       print *,'gencode   =',  gencode    
       print *,'rlond     =',  rlond      
       print *,'rlatd     =',  rlatd      
       print *,'rlonp     =',  rlonp      
       print *,'rlatp     =',  rlatp      
       print *,'rlonr     =',  rlonr      
       print *,'rlatr     =',  rlatr      
       print *,'water     =',  water      
       print *,'subcen    =',  subcen     
       print *,'ensemble  =',  ensemble   
       print *,'ppid      =',  ppid       
       print *,'slid      =',  slid       
       print *,'vcid      =',  vcid       
       print *,'vmid      =',  vmid       
       print *,'vtid      =',  vtid       
       print *,'runid     =',  runid      
       print *,'usrid     =',  usrid      
       print *,'pdryini   =',  pdryini    
       print *,'gases     =',  gases      
!!!
!!! write with ieee format for GSI
!!!
   elseif(ofmt(1:4).eq.'ieee') then
     if(infmt(1:8).ne.'ieee_reg') then
       allocate (r4si(kdim+1),r4sl(kdim))
     endif
     r4fhour=fhour
     do i=1,4
       i4date(i)=idate(i)
     enddo
     do k=1,kdim+1
       r4si(k)=si(k)
     enddo
     do k=1,kdim
       r4sl(k)=sl(k)
     enddo
     do i=1,kdum
       r4dummy(i)=dummy(i)
     enddo
     r4waves=waves
     r4xlayers=xlayers
     r4trun=trun
     r4order=order
     r4realform=realform
     r4gencode=gencode
     r4rlond=rlond
     r4rlatd=rlatd
     r4rlonp=rlonp
     r4rlatp=rlatp
     r4rlonr=rlonr
     r4rlatr=rlatr
     r4water=3
     r4subcen=subcen
     do i=1,kens
       r4ensemble(i)=ensemble(i)
     enddo
     r4ppid=ppid
     r4slid=slid
     r4vcid=2
     r4vmid=vmid
     r4vtid=vtid
     r4runid=runid
     r4usrid=usrid
     r4pdryini=pdryini
     do k=1,kdum2
       r4dummy2(k)=dummy2(i)
     enddo
     r4gases=gases
     write(51) r4fhour,i4date,r4si,r4sl                                  &
      ,(r4dummy(k),k=1,201-(kdim+1)-kdim),r4waves,r4xlayers,r4trun       &
      ,r4order,r4realform,r4gencode                                      &
      ,r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4water           &
      ,r4subcen,r4ensemble,r4ppid,r4slid,r4vcid,r4vmid,r4vtid            &
      ,r4runid,r4usrid,r4dummy2
     print *,'r4fhour,i4date=',r4fhour,i4date
     print *,'r4si, r4sl =',  r4si,r4sl         
     print *,'r4waves     =',  r4waves      
     print *,'r4xlayers   =',  r4xlayers    
     print *,'r4trun      =',  r4trun       
     print *,'r4order     =',  r4order      
     print *,'r4realform  =',  r4realform   
     print *,'r4gencode   =',  r4gencode    
     print *,'r4rlond     =',  r4rlond      
     print *,'r4rlatd     =',  r4rlatd      
     print *,'r4rlonp     =',  r4rlonp      
     print *,'r4rlatp     =',  r4rlatp      
     print *,'r4rlonr     =',  r4rlonr      
     print *,'r4rlatr     =',  r4rlatr      
     print *,'r4water     =',  r4water      
     print *,'r4subcen    =',  r4subcen     
     print *,'r4ensemble  =',  r4ensemble   
     print *,'r4ppid      =',  r4ppid       
     print *,'r4slid      =',  r4slid       
     print *,'r4vcid      =',  r4vcid       
     print *,'r4vmid      =',  r4vmid       
     print *,'r4vtid      =',  r4vtid       
     print *,'r4runid     =',  r4runid      
     print *,'r4usrid     =',  r4usrid      
     print *,'r4pdryini   =',  r4pdryini    
     print *,'r4gases     =',  r4gases      
   endif
!
! 1.3 sigma variables
!
   iwater=nint(water)
   igases=nint(gases)
!
   narray=nwave
   nrecs=2+3*kdim+iwater*kdim+igases*kdim
   allocate (array(narray))
   if(infmt(1:8).eq.'ieee_reg') then
     allocate (r4array(narray))
   endif
   do k=1,nrecs
     if(infmt(1:3).eq.'bin') then
       read(11,end=900) array
     elseif(infmt(1:8).eq.'ieee_reg') then
       read(11,end=900) r4array(1:narray)
       do n=1,narray
         array(n)=r4array(n)
       enddo
     endif
     if(ofmt(1:3).eq.'bin') then
       write(51) array
     elseif(ofmt(1:4).eq.'ieee') then
       if(infmt(1:8).ne.'ieee_reg') then
         if (.not.allocated(r4array)) allocate (r4array(narray))
       endif
       do n=1,narray
         r4array(n)=array(n)
       enddo
       write(51) r4array
     endif
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
  read(12,iostat=ierr) on85lab
!
  if (ierr /= 0) then
    print*,'surface file not found'
    call abort
  endif
  write(52) ' emc ncep surface file          '
!
! 2.2 second header
!
  if(infmt(1:3).eq.'bin') then
    read(12) fhour,idate
  elseif(infmt(1:8).eq.'ieee_reg') then
    read(12) r4fhour,i4date
    fhour=r4fhour
    do i=1,4
      idate(i)=i4date(i)
    enddo
  endif
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
  elseif(ofmt(1:5).eq.'ascii'.or.ofmt(1:3).eq.'asc' ) then
    write(52,200) fhour,idate
  elseif(ofmt(1:4).eq.'ieee') then
    r4fhour=fhour
    do i=1,4
      i4date(i)=idate(i)
    enddo
    write(52) r4fhour,i4date, idim, jdim, ivs, lpl
    print *, r4fhour,i4date, idim, jdim, ivs, lpl
  endif
!
! 2.3 body
!
!
  zero=0.0
!
!!!
!!! grims2gsi
!!!
   if(infmt(1:3).eq.'bin' .and. ofmt(1:4).eq.'ieee') then
    open(54,file='sfc_f06.bin',form='unformatted',access='direct',recl=ijdim*4)
    print *, 'nrecs= ', nrecs
     do k=1,nrecs-1
       read(12,end=909) ((grid(ij,l),ij=1,ijdim),l=1,lev(k))
       call maxmn(grid,idim,jdim,lev(k),gvar(k))
       do n=1,lev(k)
         r4grid(:,n)=grid(:,n)
       write(54) r4grid(:,n)
       enddo
       write(52) ((r4grid(ij,l),ij=1,ijdim),l=1,lev(k))
       if (sfcftyp.eq.'noa1'.and.k.eq.20) then
         write(52) (zero(ij,1),ij=1,ijdim)
         write(52) (zero(ij,1),ij=1,ijdim)
       endif
     enddo
!!!
!!! gsi2grims
!!!
   elseif(infmt(1:8).eq.'ieee_reg'.and. ofmt(1:3).eq.'bin') then
! for grads 
    open(54,file='sfc_anl.bin',form='unformatted',access='direct',recl=ijdim*4)
!  tsea
     read(12,end=909) (r4tsea(ij,1) ,ij=1,ijdim)
!  smc
       read(12,end=909) ((r4smc(ij,k) ,ij=1,ijdim),k=1,4)
!  sheleg
     read(12,end=909) (r4sheleg(ij,1) ,ij=1,ijdim)
!  stc
       read(12,end=909) ((r4stc(ij,k) ,ij=1,ijdim),k=1,4)
!  tg3
     read(12,end=909) (r4tg3(ij,1) ,ij=1,ijdim)
!  zorl
     read(12,end=909) (r4zorl(ij,1) ,ij=1,ijdim)
!  cv
     read(12,end=909) (r4cv(ij,1) ,ij=1,ijdim)
!  cvb
     read(12,end=909) (r4cvb(ij,1) ,ij=1,ijdim)
!  cvt
     read(12,end=909) (r4cvt(ij,1) ,ij=1,ijdim)
!  alb
     read(12,end=909) (r4alvsf(ij,1) ,ij=1,ijdim),  &
                      (r4alvwf(ij,1) ,ij=1,ijdim),  &
                      (r4alnsf(ij,1) ,ij=1,ijdim),  &
                      (r4alnwf(ij,1) ,ij=1,ijdim)
!  slmsk
     read(12,end=909) (r4slmsk(ij,1) ,ij=1,ijdim)
!  vfrac
     read(12,end=909) (r4vfrac(ij,1) ,ij=1,ijdim)
! canop
     read(12,end=909) (r4canopy(ij,1) ,ij=1,ijdim)
! f10m
     read(12,end=909) (r4f10m(ij,1) ,ij=1,ijdim)
! vegtyp
     read(12,end=909) (r4vtype(ij,1) ,ij=1,ijdim)
! soiltyp
     read(12,end=909) (r4stype(ij,1) ,ij=1,ijdim)
! albf
     read(12,end=909) (r4facsf(ij,1) ,ij=1,ijdim), &
                      (r4facwf(ij,1) ,ij=1,ijdim)
! ustar
     read(12,end=909) (r4uustar(ij,1) ,ij=1,ijdim)
! fm
     read(12,end=909) (r4ffmm(ij,1) ,ij=1,ijdim)
! fh
     read(12,end=909) (r4ffhh(ij,1) ,ij=1,ijdim)
! prcp
     read(12,end=909) (r4tprcp(ij,1) ,ij=1,ijdim)
! srflag
     read(12,end=909) (r4srflag(ij,1) ,ij=1,ijdim)
! snodph
     read(12,end=909) (r4snwdph(ij,1) ,ij=1,ijdim)
! slc
       read(12,end=909) ((r4slc(ij,k) ,ij=1,ijdim),k=1,4)
! shdmin
     read(12,end=909) (r4shdmin(ij,1) ,ij=1,ijdim)
! shdmax
     read(12,end=909) (r4shdmax(ij,1) ,ij=1,ijdim)
! slope
     read(12,end=909) (r4slope(ij,1) ,ij=1,ijdim)
! snoalb
     read(12,end=909) (r4snoalb(ij,1) ,ij=1,ijdim)

     grid(:,1)=r4tsea(:,1); write(52) grid(:,1) ; grid=zero
     grid(:,:)=r4smc(:,:) ; write(52) grid ; grid=zero
     grid(:,1)=r4sheleg(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,:)=r4stc(:,:) ; write(52) grid ; grid=zero
     grid(:,1)=r4tg3(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4zorl(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=zero(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=zero(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=zero(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4alvsf(:,1) ; grid(:,2)=r4alvwf(:,1)
     grid(:,3)=r4alnsf(:,1) ; grid(:,4)=r4alnwf(:,1)
     write(52) grid ; grid=zero
     grid(:,1)=r4slmsk(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4vfrac(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4canopy(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4f10m(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4vtype(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4stype(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4facsf(:,1) ; grid(:,2)=r4facwf(:,1)
     write(52) (grid(:,k),k=1,2) ; grid=zero
     grid(:,1)=r4uustar(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4ffmm(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4ffhh(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4tprcp(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4srflag(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4snwdph(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,:)=r4slc(:,:) ; write(52) grid ; grid=zero
     grid(:,1)=r4shdmin(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4shdmax(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4slope(:,1) ; write(52) grid(:,1) ; grid=zero
     grid(:,1)=r4snoalb(:,1) ; write(52) grid(:,1) ; grid=zero
! omld
     grid(:,1)=50.0          ; write(52) grid(:,1) ; grid=zero
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

!for grads
     write(54) r4tsea
     write(54) r4smc(:,1)
     write(54) r4smc(:,2)
     write(54) r4smc(:,3)
     write(54) r4smc(:,4)
     write(54) r4sheleg
     write(54) r4stc(:,1)
     write(54) r4stc(:,2)
     write(54) r4stc(:,3)
     write(54) r4stc(:,4)
     write(54) r4tg3
     write(54) r4zorl
     write(54) r4alvsf
     write(54) r4alvwf
     write(54) r4alnsf
     write(54) r4alnwf
     write(54) r4slmsk
     write(54) r4vfrac
     write(54) r4canopy
     write(54) r4f10m
     write(54) r4vtype
     write(54) r4stype
     write(54) r4facsf
     write(54) r4uustar
     write(54) r4ffmm
     write(54) r4ffhh
     write(54) r4tprcp
     write(54) r4srflag
     write(54) r4snwdph
     write(54) r4slc(:,1)
     write(54) r4slc(:,2)
     write(54) r4slc(:,3)
     write(54) r4slc(:,4)
     write(54) r4shdmin
     write(54) r4shdmax
     write(54) r4slope
     write(54) r4snoalb
   endif
   !
   ! issfc.eq.1
   !
endif
!
100   format(32a1)
200   format(1x,e13.6,4i13)
300   format(1x,6e13.6)
400   format(1x,e13.6,4i13/(1x,e13.6))
909   stop
   !
999   continue
   print *,'sigma file read error'
   call abort
   end
 subroutine maxmn(f,imax,jmax,kmax,title)
  integer imax,jmax,kmax
  real f(imax,jmax,kmax),fmin,fmax
  character*8 title
  integer iimax,jjmax,iimin, jjmin, i,j,k
!
  print 90, title
  90 format (2x,'title=',a8)
!
      do k=1,kmax
!
      fmax=f(1,1,k)
      fmin=f(1,1,k)
!
      iimax=1
      jjmax=1
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
  100 format(2x,'level=',i5,' max=',e12.4,' at i=',i5,' j=',i5, &
                           ' min=',e12.4,' at i=',i5,' j=',i5)
!  
   enddo
! 
  return
  end subroutine maxmn
