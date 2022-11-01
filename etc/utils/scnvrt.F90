#include <machine.h>
   program scnvrt
   implicit none
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
!   firstlab
!
! attributes:
!   language: fortran
!
   character(len=8)     ::  on85lab(4)
!
!  gsm second header extra words
!
   integer,parameter    ::  kdum=201,kdum2=21,kens=2
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
   real, allocatable    ::  si(:),sl(:)
   real, allocatable    ::  array(:)
   real, allocatable    ::  grid(:,:)
!
   integer              ::  newyr,newmo,newdy,newhr
   integer              ::  jcap,issfc,issig,idim,jdim,kdim
   integer              ::  ijdim,nwave,i,k,nheadtyp
   integer              ::  ntrac,ncldg,itraces,iclouds,narray,nrecs,n,ierr,mxlv,ij,l
   real                 ::  fhnew,fhourin
!
   real                 ::  fhour
   real                 ::  waves,xlayers,trun
   real                 ::  order,realform,gencode
   real                 ::  rlond,rlatd,rlonp,rlatp,rlonr,rlatr,tracers
   real                 ::  subcen,ppid,slid,vcid,vmid,vtid
   real                 ::  runid,usrid,pdryini,clouds
!
   integer*4            ::  i4date(4)
   real*4               ::  r4dummy(kdum),r4dummy2(kdum2),r4ensemble(kens)
   real*4               ::  r4rdummy(2*levmax),r4ext(512-(6+2*levmax))
   real*4 ,allocatable  ::  r4si(:),r4sl(:)
   real*4 ,allocatable  ::  r4array(:)
   real*4 ,allocatable  ::  r4grid(:,:)
   real*4               ::  r4fhour
   real*4               ::  r4waves,r4xlayers,r4trun
   real*4               ::  r4order,r4realform,r4gencode
   real*4               ::  r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4tracers
   real*4               ::  r4subcen,r4ppid,r4slid,r4vcid,r4vmid,r4vtid
   real*4               ::  r4runid,r4usrid,r4pdryini,r4clouds
!
   integer*4            ::  i8date(8)
   real*8               ::  r8dummy(kdum),r8dummy2(kdum2),r8ensemble(kens)
   real*8               ::  r8rdummy(2*levmax),r8ext(512-(6+2*levmax))
   real*8 ,allocatable  ::  r8si(:),r8sl(:)
   real*8 ,allocatable  ::  r8array(:)
   real*8 ,allocatable  ::  r8grid(:,:)
   real*8               ::  r8fhour
   real*8               ::  r8waves,r8xlayers,r8trun
   real*8               ::  r8order,r8realform,r8gencode
   real*8               ::  r8rlond,r8rlatd,r8rlonp,r8rlatp,r8rlonr,r8rlatr,r8tracers
   real*8               ::  r8subcen,r8ppid,r8slid,r8vcid,r8vmid,r8vtid
   real*8               ::  r8runid,r8usrid,r8pdryini,r8clouds
!
   integer              ::  icdate(4)
   real                 ::  rcdummy(kdum),rcdummy2(kdum2),rcensemble(kens)
   real                 ::  rcrdummy(2*levmax),rcext(512-(6+2*levmax))
   real,allocatable     ::  rcsi(:),rcsl(:)
   real,allocatable     ::  rcarray(:)
   real,allocatable     ::  rcgrid(:,:)
   real                 ::  rcfhour
   real                 ::  rcwaves,rcxlayers,rctrun
   real                 ::  rcorder,rcrealform,rcgencode
   real                 ::  rcrlond,rcrlatd,rcrlonp,rcrlatp,rcrlonr,rcrlatr,rctracers
   real                 ::  rcsubcen,rcppid,rcslid,rcvcid,rcvmid,rcvtid
   real                 ::  rcrunid,rcusrid,rcpdryini,rcclouds
!
   character(len=3)     ::  gsm0rsm
   character(len=4)     ::  sfcftyp
   character(len=8)     ::  infmt,ofmt
   data newyr,newmo,newdy,newhr,fhnew/-1,-1,-1,-1,-1./
   data infmt,ofmt/'bin','bin'/
   data jcap/0/
   data issfc,issig/1,1/
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
!   'cray'         ...  format specified by the cray assign statement
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
   allocate (si(kdim+1))
   allocate (sl(kdim  ))
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
   tracers=1.
   clouds=0.
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
   runid=0.
   usrid=0.
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
#ifdef DEC
   if(infmt(1:4).eq.'ieee') then
     open(unit=11,file='fort.11',form='unformatted',                           &
          convert='big_endian',status='old',err=920)
     go to 921
920  continue
       print *,'error opening input sigma file'
       call abort
921  continue
   endif
   if(ofmt(1:4).eq.'ieee') then
     open(unit=51,file='fort.51',form='unformatted',                           &
          convert='big_endian',status='unknown',err=930)
     go to 931
930  continue
       print *,'error opening output sigma file'
       call abort
931  continue
   endif
#else
   if(infmt(1:4).eq.'ieee') then
     open(unit=11,form='unformatted',convert='big_endian',status='old')
   endif
#endif
!
!  1.1 first label record
!
   if(infmt(1:5).eq.'ascii'.or.infmt(1:3).eq.'asc' ) then
     !read(11,100)
     read(11,100) on85lab
     if(ofmt(1:3).eq.'bin') then
       write(51) ' emc ncep sigma surface file    '
     elseif(ofmt(1:5).eq.'ascii'.or.ofmt(1:3).eq.'asc' ) then
       write(51,100) ' emc ncep sigma surface file    '
     elseif(ofmt(1:4).eq.'ieee') then
       write(51) ' emc ncep sigma surface file    '
     endif
   else
     !read(11) 
     read(11) on85lab
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
   nheadtyp=0
!
!  1.2.1 ascii
!
   if(infmt(1:5).eq.'ascii'.or.infmt(1:3).eq.'asc' ) then
     read(11,400) fhour,idate,si,sl
!
!  1.2.2 native
!
   elseif(infmt(1:3).eq.'bin') then
     if(gsm0rsm(1:3).eq.'gsm') then
       read(11,err=201) fhour,idate,si,sl                                      &
          ,(dummy(k),k=1,kdum-(kdim+1)-kdim)                                   &
          ,waves,xlayers,trun,order,realform,gencode                           &
          ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,tracers                         &
          ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid                &
          ,pdryini,dummy2,clouds
       nheadtyp=2
       print *,'read header rec with tracer>1 or clouds>0'
       goto 202
  201  continue
       rewind 11
       read(11) on85lab
       read(11,err=203) fhour,idate,si,sl                                      &
          ,(dummy(k),k=1,kdum-(kdim+1)-kdim)                                   &
          ,waves,xlayers,trun,order,realform,gencode                           &
          ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,tracers                         &
          ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid                &
          ,dummy2
       nheadtyp=1
       print *,'read header rec with tracer<=1 and clouds=0'
  202  continue
       print *,' fhour,idate=',fhour,idate
       if(tracers.eq.0.) then
         tracers=1.
         print *,'tracers reset to 1.'
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
       tracers=ifix(ext(19)) ! iwater in rmp code
       clouds=ifix(ext(20))  ! igases in rmp code
     else
      print *,'unknown model'
      call abort
     endif
!
! 1.2.3 ieee_reg
!
   elseif(infmt(1:8).eq.'ieee_reg') then
     allocate (r4si(kdim+1),r4sl(kdim))
     if(gsm0rsm(1:3).eq.'gsm') then
       read(11,err=301) r4fhour,i4date,r4si,r4sl                               &
          ,(r4dummy(k),k=1,kdum-(kdim+1)-kdim)                                 &
          ,r4waves,r4xlayers,r4trun                                            &
          ,r4order,r4realform,r4gencode                                        &
          ,r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4tracers           &
          ,r4subcen,r4ensemble,r4ppid,r4slid,r4vcid,r4vmid,r4vtid              &
          ,r4runid,r4usrid,r4pdryini,r4dummy2,r4clouds
       nheadtyp=2
       print *,'read header rec with tracer>1 or clouds>0'
       go to 302
  301     continue
       rewind 11
       read(11) on85lab 
       read(11,err=303) r4fhour,i4date,r4si,r4sl                               &
          ,(r4dummy(k),k=1,kdum-(kdim+1)-kdim)                                 &
          ,r4waves,r4xlayers,r4trun                                            &
          ,r4order,r4realform,r4gencode                                        &
          ,r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4tracers           &
          ,r4subcen,r4ensemble,r4ppid,r4slid,r4vcid,r4vmid,r4vtid              &
          ,r4runid,r4usrid,r4dummy2
       nheadtyp=1
       print *,'read header rec with tracer<=1 and clouds=0'
       r4clouds=0.
       r4pdryini=0.
  302  continue
       print *,' fhour,idate=',fhour,idate
       if(r4tracers.eq.0.) then
         r4tracers=1.
         print *,'r4tracers reset to 1.'
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
       tracers=r4tracers
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
       clouds=r4clouds
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
     elseif(gsm0rsm(1:3).eq.'rsm') then
       read(11) r4fhour,i4date,r4si,r4sl                                       &
               ,(r4rdummy(i),i=1,2*levmax+1-kdim-1-kdim),r4ext
       do i=1,2*levmax
         rdummy(i)=r4rdummy(i)
       enddo
       do i=1,512-(6+2*levmax)
         ext(i)=r4ext(i)
       enddo
       tracers=ifix(ext(19)) ! iwater in rmp code
       clouds=ifix(ext(20))  ! igases in rmp code
     else
      print *,'unknown model'
      call abort
     endif
!
! 1.2.4 ieee_dep
!
   elseif(infmt(1:7).eq.'ieee_dp') then
     allocate (r8si(kdim+1),r8sl(kdim))
     if(gsm0rsm(1:3).eq.'gsm') then
       read(11,err=401) r8fhour,i8date,r8si,r8sl                               &
          ,(r8dummy(k),k=1,kdum-(kdim+1)-kdim)                                 &
          ,r8waves,r8xlayers,r8trun                                            &
          ,r8order,r8realform,r8gencode                                        &
          ,r8rlond,r8rlatd,r8rlonp,r8rlatp,r8rlonr,r8rlatr,r8tracers           &
          ,r8subcen,r8ensemble,r8ppid,r8slid,r8vcid,r8vmid,r8vtid              &
          ,r8runid,r8usrid,r8pdryini,r8dummy2,r8clouds
       print *,'read header rec with tracer>1 or clouds>0'
       go to 402
  401  continue
       rewind 11
       read on85lab
       read(11,err=403) r8fhour,i8date,r8si,r8sl                               &
          ,(r8dummy(k),k=1,kdum-(kdim+1)-kdim)                                 &
          ,r8waves,r8xlayers,r8trun                                            &
          ,r8order,r8realform,r8gencode                                        &
          ,r8rlond,r8rlatd,r8rlonp,r8rlatp,r8rlonr,r8rlatr,r8tracers           &
          ,r8subcen,r8ensemble,r8ppid,r8slid,r8vcid,r8vmid,r8vtid              &
          ,r8runid,r8usrid,r8dummy2
       nheadtyp=1
       print *,'read header rec with tracer<=1 and clouds=0'
       r8clouds=0.
       r8pdryini=0.
  402  continue
       print *,' fhour,idate=',fhour,idate
       if(r8tracers.eq.0.) then
         r8tracers=1.
         print *,'r8tracers reset to 1.'
       endif
       do i=1,kdum
         dummy(i)=r8dummy(i)
       enddo
       waves=r8waves
       xlayers=r8xlayers
       trun=r8trun
       order=r8order
       realform=r8realform
       gencode=r8gencode
       rlond=r8rlond
       rlatd=r8rlatd
       rlonp=r8rlonp
       rlatp=r8rlatp
       rlonr=r8rlonr
       rlatr=r8rlatr
       tracers=r8tracers
       subcen=r8subcen
       do i=1,kens
         ensemble(i)=r8ensemble(i)
       enddo
       ppid=r8ppid
       slid=r8slid
       vcid=r8vcid
       vmid=r8vmid
       vtid=r8vtid
       runid=r8runid
       usrid=r8usrid
       pdryini=r8pdryini
       do k=1,kdum2
         dummy2(k)=r8dummy2(i)
       enddo
       clouds=r8clouds
       goto 404
403    continue
       rewind 11
       read(11) on85lab
       read(11,err=999) r8fhour,i8date,r8si,r8sl
       print *,'read short format'
404    continue
       fhour=r8fhour
       do i=1,4
         idate(i)=i8date(i*2)
       enddo
       do k=1,kdim+1
         si(k)=r8si(k)
       enddo
       do k=1,kdim
         sl(k)=r8sl(k)
       enddo
     elseif(gsm0rsm(1:3).eq.'rsm') then
       read(11) r8fhour,i8date,r8si,r8sl                                       &
               ,(r8rdummy(i),i=1,2*levmax+1-kdim-1-kdim),r8ext
       do i=1,2*levmax
         rdummy(i)=r8rdummy(i)
       enddo
       do i=1,512-(6+2*levmax)
         ext(i)=r8ext(i)
       enddo
       tracers=ifix(ext(19)) ! iwater in rmp code
       clouds=ifix(ext(20))  ! igases in rmp code
     else
      print *,'unknown model'
      call abort
     endif
!
! 1.2.5 cray
!
   elseif(infmt(1:4).eq.'cray') then
     allocate (rcsi(kdim+1),rcsl(kdim))
     if(gsm0rsm(1:3).eq.'gsm') then
       read(11,err=501) rcfhour,icdate,rcsi,rcsl                               &
          ,(rcdummy(k),k=1,kdum-(kdim+1)-kdim)                                 &
          ,rcwaves,rcxlayers,rctrun                                            &
          ,rcorder,rcrealform,rcgencode                                        &
          ,rcrlond,rcrlatd,rcrlonp,rcrlatp,rcrlonr,rcrlatr,rctracers           &
          ,rcsubcen,rcensemble,rcppid,rcslid,rcvcid,rcvmid,rcvtid              &
          ,rcrunid,rcusrid,rcpdryini,rcdummy2,rcclouds
       nheadtyp=2
       print *,'read header rec with tracer>1 or clouds>0'
       go to 502
  501     continue
       rewind 11
       read(11) on85lab
       read(11,err=503) rcfhour,icdate,rcsi,rcsl                               &
          ,(rcdummy(k),k=1,kdum-(kdim+1)-kdim)                                 &
          ,rcwaves,rcxlayers,rctrun                                            &
          ,rcorder,rcrealform,rcgencode                                        &
          ,rcrlond,rcrlatd,rcrlonp,rcrlatp,rcrlonr,rcrlatr,rctracers           &
          ,rcsubcen,rcensemble,rcppid,rcslid,rcvcid,rcvmid,rcvtid              &
          ,rcrunid,rcusrid,rcdummy2
       nheadtyp=1
       print *,'read header rec with tracer<=1 and clouds=0'
       rcclouds=0.
       rcpdryini=0.
  502     continue
       if(rctracers.eq.0.) then
         rctracers=1.
         print *,'rctracers reset to 1.'
       endif
       do i=1,kdum
         dummy(i)=rcdummy(i)
       enddo
       waves=rcwaves
       xlayers=rcxlayers
       trun=rctrun
       order=rcorder
       realform=rcrealform
       gencode=rcgencode
       rlond=rcrlond
       rlatd=rcrlatd
       rlonp=rcrlonp
       rlatp=rcrlatp
       rlonr=rcrlonr
       rlatr=rcrlatr
       tracers=rctracers
       subcen=rcsubcen
       do i=1,kens
         ensemble(i)=rcensemble(i)
       enddo
       ppid=rcppid
       slid=rcslid
       vcid=rcvcid
       vmid=rcvmid
       vtid=rcvtid
       runid=rcrunid
       usrid=rcusrid
       pdryini=rcpdryini
       do k=1,kdum2
         dummy2(k)=rcdummy2(i)
       enddo
       clouds=rcclouds
       goto 504
503       continue
       rewind 11
       read(11) on85lab
       read(11,err=999) rcfhour,icdate,rcsi,rcsl
       print *,'read old format fhour,idate=',rcfhour,icdate
504       continue
       fhour=rcfhour
       do i=1,4
         idate(i)=icdate(i)
       enddo
       do k=1,kdim+1
         si(k)=rcsi(k)
       enddo
       do k=1,kdim
         sl(k)=rcsl(k)
       enddo
     elseif(gsm0rsm(1:3).eq.'rsm') then
       read(11) rcfhour,icdate,rcsi,rcsl                                       &
               ,(rcrdummy(i),i=1,2*levmax+1-kdim-1-kdim),rcext
       do i=1,2*levmax
         rdummy(i)=rcrdummy(i)
       enddo
       do i=1,512-(6+2*levmax)
         ext(i)=rcext(i)
       enddo
       tracers=ifix(ext(19)) ! iwater in rmp code
       clouds=ifix(ext(20))  ! igases in rmp code
     else
      print *,'unknown model'
      call abort
     endif
!
! 1.2.6 ieee_sgi
!
   elseif(infmt(1:8).eq.'ieee_sgi') then
     allocate (r8si(kdim+1),r8sl(kdim))
     if(gsm0rsm(1:3).eq.'gsm') then
       read(11,err=601) r8fhour,i4date,r8si,r8sl                               &
          ,r8dummy,r8waves,r8xlayers,r8trun                                    &
          ,r8order,r8realform,r8gencode                                        &
          ,r8rlond,r8rlatd,r8rlonp,r8rlatp,r8rlonr,r8rlatr,r8tracers           &
          ,r8subcen,r8ensemble,r8ppid,r8slid,r8vcid,r8vmid,r8vtid              &
          ,r8runid,r8usrid,r8pdryini,r8dummy2,r8clouds
       nheadtyp=2
       print *,'read header rec with tracer>1 or clouds>0'
       go to 602
  601     continue
       rewind 11
       read(11) on85lab
       read(11,err=603) r8fhour,i4date,r8si,r8sl                               &
          ,r8dummy,r8waves,r8xlayers,r8trun                                    &
          ,r8order,r8realform,r8gencode                                        &
          ,r8rlond,r8rlatd,r8rlonp,r8rlatp,r8rlonr,r8rlatr,r8tracers           &
          ,r8subcen,r8ensemble,r8ppid,r8slid,r8vcid,r8vmid,r8vtid              &
          ,r8runid,r8usrid,r8dummy2
       nheadtyp=1
       print *,'read header rec with tracer<=1 and clouds=0'
       r8clouds=0.
       r8pdryini=0.
  602     continue
       if(r8tracers.eq.0.) then
         r8tracers=1.
         print *,'r8tracers reset to 1.'
       endif
       do i=1,kdum
         dummy(i)=r8dummy(i)
       enddo
       waves=r8waves
       xlayers=r8xlayers
       trun=r8trun
       order=r8order
       realform=r8realform
       gencode=r8gencode
       rlond=r8rlond
       rlatd=r8rlatd
       rlonp=r8rlonp
       rlatp=r8rlatp
       rlonr=r8rlonr
       rlatr=r8rlatr
       tracers=r8tracers
       subcen=r8subcen
       do i=1,kens
         ensemble(i)=r8ensemble(i)
       enddo
       ppid=r8ppid
       slid=r8slid
       vcid=r8vcid
       vmid=r8vmid
       vtid=r8vtid
       runid=r8runid
       usrid=r8usrid
       pdryini=r8pdryini
       do k=1,kdum2
         dummy2(k)=r8dummy2(i)
       enddo
       clouds=r8clouds
       goto 604
603       continue
       rewind 11
       read(11) on85lab
       read(11,err=999) r8fhour,i4date,r8si,r8sl
       print *,'read old format fhour,idate=',r8fhour,i4date
604       continue
       fhour=r8fhour
       do i=1,4
         idate(i)=i4date(i)
       enddo
       do k=1,kdim+1
         si(k)=r8si(k)
       enddo
       do k=1,kdim
         sl(k)=r8sl(k)
       enddo
     elseif(gsm0rsm(1:3).eq.'rsm') then
       read(11) r8fhour,i4date,r8si,r8sl                                       &
               ,(r8rdummy(i),i=1,2*levmax+1-kdim-1-kdim),r8ext
       do i=1,2*levmax
         rdummy(i)=r8rdummy(i)
       enddo
       do i=1,512-(6+2*levmax)
         ext(i)=r8ext(i)
       enddo
       tracers=ifix(ext(19)) ! iwater in rmp code
       clouds=ifix(ext(20))  ! igases in rmp code
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
   print *,'number of tracers,clouds=',tracers,clouds
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
   ntrac=nint(tracers)
   ncldg=nint(clouds)
   if(ofmt(1:3).eq.'bin') then
     if(gsm0rsm(1:3).eq.'gsm') then
       if(ntrac.eq.1.and.ncldg.eq.0) then
         write(51) fhour,idate,si,sl                                           &
          ,(dummy(k),k=1,201-(kdim+1)-kdim)                                    &
          ,waves,xlayers,trun,order,realform,gencode                           &
          ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,tracers                         &
          ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid                &
          ,dummy2
         print *,'second header rec written'
       else
         write(51)fhour,idate,si,sl                                            &
            ,(dummy(k),k=1,201-(kdim+1)-kdim)                                  &
            ,waves,xlayers,trun,order,realform,gencode                         &
            ,rlond,rlatd,rlonp,rlatp,rlonr,rlatr,tracers                       &
            ,subcen,ensemble,ppid,slid,vcid,vmid,vtid,runid,usrid              &
            ,pdryini,dummy2,clouds
       endif
     elseif(gsm0rsm(1:3).eq.'rsm') then
       write(51) fhour,idate,si,sl                                             &
                ,(rdummy(i),i=1,2*levmax+1-kdim-1-kdim),ext
     endif
   elseif(ofmt(1:5).eq.'ascii'.or.ofmt(1:3).eq.'asc' ) then
     write(51,400) fhour,idate,si,sl
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
     if(gsm0rsm(1:3).eq.'gsm') then
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
       r4tracers=tracers
       r4subcen=subcen
       do i=1,kens
         r4ensemble(i)=ensemble(i)
       enddo
       r4ppid=ppid
       r4slid=slid
       r4vcid=vcid
       r4vmid=vmid
       r4vtid=vtid
       r4runid=runid
       r4usrid=usrid
       r4pdryini=pdryini
       do k=1,kdum2
         r4dummy2(k)=dummy2(i)
       enddo
       r4clouds=clouds
       if(ntrac.eq.1.and.ncldg.eq.0) then
         write(51) r4fhour,i4date,r4si,r4sl                                    &
          ,r4dummy,r4waves,r4xlayers,r4trun                                    &
          ,r4order,r4realform,r4gencode                                        &
          ,r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4tracers           &
          ,r4subcen,r4ensemble,r4ppid,r4slid,r4vcid,r4vmid,r4vtid              &
          ,r4runid,r4usrid,r4dummy2
       else
         write(51) r4fhour,i4date,r4si,r4sl                                    &
          ,r4dummy,r4waves,r4xlayers,r4trun                                    &
          ,r4order,r4realform,r4gencode                                        &
          ,r4rlond,r4rlatd,r4rlonp,r4rlatp,r4rlonr,r4rlatr,r4tracers           &
          ,r4subcen,r4ensemble,r4ppid,r4slid,r4vcid,r4vmid,r4vtid              &
          ,r4runid,r4usrid,r4pdryini,r4dummy2,r4clouds
       endif
     elseif(gsm0rsm(1:3).eq.'rsm') then
       do i=1,2*levmax
         r4rdummy(i)=rdummy(i)
       enddo
       do i=1,512-(6+2*levmax)
         r4ext(i)=ext(i)
       enddo
       write(51) r4fhour,i4date,r4si,r4sl                                      &
            ,(r4rdummy(i),i=1,2*levmax+1-kdim-1-kdim),r4ext
     endif
   endif
!
! 1.3 sigma variables
!
   itraces=nint(tracers)
   iclouds=nint(clouds)
!
   if(gsm0rsm(1:3).eq.'gsm') then
     narray=nwave
     nrecs=2+3*kdim+itraces*kdim+iclouds*kdim
   elseif(gsm0rsm(1:3).eq.'rsm') then
     narray=ijdim
     nrecs=2+(3+itraces+iclouds)*kdim+5
   endif
   allocate (array(narray))
   if(infmt(1:8).eq.'ieee_reg') then
     allocate (r4array(narray))
   elseif(infmt(1:4).eq.'cray') then
     allocate (rcarray(narray))
   elseif(infmt(1:7).eq.'ieee_dp'.or.infmt(1:8).eq.'ieee_sgi') then
     allocate (r8array(narray))
   endif
   do k=1,nrecs
     if(infmt(1:5).eq.'ascii'.or.infmt(1:3).eq.'asc' ) then
       read(11,300,end=900) array
     elseif(infmt(1:3).eq.'bin') then
       read(11,end=900) array
     elseif(infmt(1:8).eq.'ieee_reg') then
       read(11,end=900) r4array(1:narray)
       do n=1,narray
         array(n)=r4array(n)
       enddo
     elseif(infmt(1:4).eq.'cray') then
       read(11,end=900) rcarray
       do n=1,narray
         array(n)=rcarray(n)
       enddo
     elseif(infmt(1:7).eq.'ieee_dp'.or.infmt(1:8).eq.'ieee_sgi') then
       read(11,end=900) r8array
       do n=1,narray
         array(n)=r8array(n)
       enddo
     endif
     if(ofmt(1:3).eq.'bin') then
       write(51) array
     elseif(ofmt(1:5).eq.'ascii'.or.ofmt(1:3).eq.'asc' ) then
       write(51,300) array
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
   elseif(infmt(1:4).eq.'cray') then
     deallocate (rcarray)
   elseif(infmt(1:7).eq.'ieee_dp'.or.infmt(1:8).eq.'ieee_sgi') then
     deallocate (r8array)
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
#ifdef DEC
   if(infmt(1:4).eq.'ieee') then
     open(unit=12,file='fort.12',form='unformatted',                           &
          convert='big_endian',status='old',err=820)
     go to 821
  820   continue
     print *,'error opening input sfc file'
     call exit(1)
  821   continue
   endif
   if(ofmt(1:4).eq.'ieee') then
     open(unit=52,file='fort.52',form='unformatted',                           &
          convert='big_endian',status='unknown',err=830)
     go to 831
  830   continue
     print *,'error opening output sfc file'
     call abort
  831   continue
   endif
#else
   if(infmt(1:4).eq.'ieee') then
     open(unit=12,form='unformatted',convert='big_endian',status='old')
   endif
#endif
!
! 2.1  label
!
   if(infmt(1:5).eq.'ascii'.or.infmt(1:3).eq.'asc' ) then
     read(12,100,iostat=ierr)  on85lab
   else
     read(12,iostat=ierr) on85lab
   endif
   if (ierr /= 0) then
      print*,'surface file not found'
      call abort
   endif
   if(ofmt(1:3).eq.'bin') then
     write(52) ' emc ncep surface file          '
   elseif(ofmt(1:5).eq.'ascii'.or.ofmt(1:3).eq.'asc' ) then
     write(52,100) ' emc ncep surface file          '
   elseif(ofmt(1:4).eq.'ieee') then
     write(52) ' emc ncep surface file          '
   endif
!
! 2.2 second header
!
   if(infmt(1:5).eq.'ascii'.or.infmt(1:3).eq.'asc' ) then
     read(12,200) fhour,idate
   elseif(infmt(1:3).eq.'bin') then
     read(12) fhour,idate
     print *,'fhour idate readin'
   elseif(infmt(1:7).eq.'ieee_dp') then
     read(12) r8fhour,i8date
     fhour=r8fhour
     do i=1,4
       idate(i)=i8date(i*2)
     enddo
   elseif(infmt(1:4).eq.'cray') then
     read(12) rcfhour,icdate
     fhour=rcfhour
     do i=1,4
       idate(i)=icdate(i)
     enddo
   elseif(infmt(1:8).eq.'ieee_sgi') then
     read(12) r8fhour,i4date
     fhour=r8fhour
     do i=1,4
       idate(i)=i4date(i)
     enddo
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
   elseif(ofmt(1:5).eq.'ascii'.or.ofmt(1:3).eq.'asc' ) then
     write(52,200) fhour,idate
   elseif(ofmt(1:4).eq.'ieee') then
     r4fhour=fhour
     do i=1,4
       i4date(i)=idate(i)
     enddo
     write(52) r4fhour,i4date
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
   if (ierr /= 0) then
      print*,'allocate grid error:',ierr
      call abort
   endif
   if(infmt(1:8).eq.'ieee_reg') then
     allocate (r4grid(ijdim,mxlv))
   elseif(infmt(1:4).eq.'cray') then
     allocate (rcgrid(ijdim,mxlv))
   elseif(infmt(1:7).eq.'ieee_dp'.or.infmt(1:8).eq.'ieee_sgi') then
     allocate (r8grid(ijdim,mxlv))
   endif
   if(infmt(1:8).ne.'ieee_reg'.and.ofmt(1:4).eq.'ieee') then
     allocate (r4grid(ijdim,mxlv))
   endif
!
   do k=1,nrecs
     if(infmt(1:5).eq.'ascii'.or.infmt(1:3).eq.'asc' ) then
       read(12,300,end=909) ((grid(ij,l),ij=1,ijdim),l=1,lev(k))
     elseif(infmt(1:3).eq.'bin') then
       read(12,end=909) ((grid(ij,l),ij=1,ijdim),l=1,lev(k))
     elseif(infmt(1:8).eq.'ieee_reg') then
       read(12,end=909) ((r4grid(ij,l),ij=1,ijdim),l=1,lev(k))
#if defined(DEC)
       do n=1,lev(k)
         do ii=1,ijdim
           if (r4grid(ii,n).lt.1.e-38.and.r4grid(ii,n).gt.                     &
                 -1.e-38.and.r4grid(ii,n).ne.0.) then
             r4grid(ii,n)=0.
             print*, 'Setting r4grid1 to 0 at', ii
           endif
         enddo
       enddo
#endif
       do n=1,lev(k)
         do ij=1,ijdim
           grid(ij,n)=r4grid(ij,n)
         enddo
       enddo
     elseif(infmt(1:7).eq.'ieee_dp'.or.infmt(1:8).eq.'ieee_sgi') then
       read(12,end=909) ((r8grid(ij,l),ij=1,ijdim),l=1,lev(k))
       do n=1,lev(k)
         do ij=1,ijdim
           grid(ij,n)=r8grid(ij,n)
         enddo
       enddo
     elseif(infmt(1:4).eq.'cray') then
       read(12,end=909) ((rcgrid(ij,l),ij=1,ijdim),l=1,lev(k))
       do n=1,lev(k)
         do ij=1,ijdim
           grid(ij,n)=rcgrid(ij,n)
         enddo
       enddo
     endif
     if(ofmt(1:3).eq.'bin') then
       write(52) ((grid(ij,l),ij=1,ijdim),l=1,lev(k))
     elseif(ofmt(1:5).eq.'ascii'.or.ofmt(1:3).eq.'asc' ) then
       write(52,300) ((grid(ij,l),ij=1,ijdim),l=1,lev(k))
     elseif(ofmt(1:4).eq.'ieee') then
       do n=1,lev(k)
         do ij=1,ijdim
           r4grid(ij,n)=grid(ij,n)
         enddo
       enddo
       write(52) ((r4grid(ij,l),ij=1,ijdim),l=1,lev(k))
     endif
   enddo
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
