   program rmpiset
!$$$  main program documentation block
!
! main program:    rmpiset
!    prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: this program to get maxima dimension for all partitions
!
! program history log:
!    99-06-27  henry juang    finish entire test for gsm
!    00-03-09  henry juang    update with symmetry distribution
!                               prepare for reduce grid.
!
! input file lists:
!   unit 5   - standard input
!
! output file list:
!   unit 6   - standar output
! 
! subprograms called:
!   dimset   - to compute all maximal dimension, starting
!        point and length of each pe
!
!     include 'rdimset.F'
!     include 'equdiv.F'
!
! attributes:
!    language: fortran 90
!$$$
!
   implicit none
!
   integer              ::  npes,ncol,nrow,n,nc,igrd1,jgrd1,iwav1,jwav1
!
   integer              ::  levr,igrd,jgrd
   integer              ::  levmax,lonmax,latmax,lntmax,lnpmax,lwvmax
!
   real                 ::  pesx
   data ncol/0/, nrow/0/
!
   namelist /namset/ levr,igrd,jgrd,npes,ncol,nrow
!
   read(5,namset)
!
   igrd1=igrd+1
   jgrd1=jgrd+1
   iwav1=int((igrd-12)/3)*2+1
   jwav1=int((jgrd-12)/3)*2+1
!
   if( ncol.eq.0 ) then
     pesx=npes
     nc=sqrt(pesx)
     do n=nc,2,-1
       if( mod(npes,n).eq.0 ) then
         ncol=n
         go to 1234
       endif
     enddo
     ncol=1
   else
     if( mod(npes,ncol).ne.0 ) then
       print *,' user provided npes=',npes
       print *,' with an invalid ncol=',ncol
       call abort
       stop
     endif
   endif
1234  nrow=npes/ncol   
!
   call rdimset(iwav1,jwav1,levr,igrd1,jgrd1,                                  &
               npes,ncol,nrow,                                                 &
               levmax,                                                         &
               lonmax,                                                         &
               latmax,                                                         &
               lwvmax,                                                         &
               lntmax,                                                         &
               lnpmax)
!
   write(*,107) levmax
   write(*,108) lonmax
   write(*,109) latmax
   write(*,110) lwvmax
   write(*,111) lntmax
   write(*,112) lnpmax
!
 101  format('#define MP')
 102  format('#define mpi_real mpi_real8')
 103  format('#define _mpi_real_ 8')
 104  format('#define _npes_ ',i4)
 105  format('#define _ncol_ ',i4)
 106  format('#define _nrow_ ',i4)
 107  format('#define _levrp_ ',i3)
 108  format('#define _igrd1p_ ',i4)
 109  format('#define _jgrd1p_ ',i4)
 110  format('#define _iwav1p_ ',i3)
 111  format('#define _lnwavp_ ',i6)
 112  format('#define _llwavp_ ',i6)
 
   stop
   end

   subroutine rdimset(iwav1,jwav1,levr,igrd1,jgrd1,                            &
                     npes,ncol,nrow,                                           &
                     levmax,                                                   &
                     lonmax,                                                   &
                     latmax,                                                   &
                     lwvmax,                                                   &
                     lntmax,                                                   &
                     lnpmax)
!$$$  subprogram documentation block
! subprogram:    rdimset
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: preset all starting point and length for 
!           all pe for global spectral model.
!
! program history log:
!    99-06-27  henry juang    finish entire test for gsm
!
! usage:   call rdimset(levr,igrd1,jgrd1,
!    *                  npes,ncol,nrow,
!    *                  levstr,levlen,levmax,
!    *                  lonstr,lonlen,lonmax,
!    *                  latstr,latlen,latmax,
!    *                  lwvstr,lwvlen,lwvmax,
!    *                  lntstr,lntlen,lntmax,
!    *                  lnpstr,lnplen,lnpmax)
!
!    input argument lists:
!   iwav1   - integer spectral wavenumber
!   levr   - integer vertical layer number
!   igrd1   - integer gaussian grid for longitude
!   jgrd1   - integer gaussian grid for latitude
!   npes   - integer number of pe used: npes=ncol*nrow
!   ncol   - integer number of column
!   nrow   - integer number of nrow
!
!    output argument list:
!   lev*   - integer (npes) related to layers for each pe
!   lon*   - integer (npes) related to longitude for each pe
!   lat*   - integer (npes) related to latitude for each pe
!   lnt*   - integer (npes) related to npes cut of spectral
!   lnp*   - integer (npes) related to nrow cut of spectral
!   lwv*   - integer (npes) related to group of spectral in l
!   *str   - integer (npes) related to each starting point
!   *len   - integer (npes) related to each length
!   *max   - integer related to maximal length of the kind
! 
! subprograms called:
!   equdiv   - to compute about equal number of subgroup by division
!
! attributes:
!    language: fortran 90
!$$$
!
   implicit none
   integer              ::  levr,igrd1,jgrd1,npes,ncol,nrow,iwav1,jwav1
   integer              ::  latg2,lonf2,nr,nc,nn,jcaprm,nremain
   integer              ::  lnp,n2a,lx,ll,lh,n
   integer              ::  levmax,lonmax,latmax,lwvmax,lntmax,lnpmax
   integer              ::  levpnt,lonpnt,latpnt,lwvpnt,lntpnt,lnppnt
   integer,parameter    ::  npesx=10000
   integer              ::  levstr(0:npesx-1),levlen(0:npesx-1)
   integer              ::  lonstr(0:npesx-1),lonlen(0:npesx-1)
   integer              ::  latstr(0:npesx-1),latlen(0:npesx-1)
   integer              ::  lwvstr(0:npesx-1),lwvlen(0:npesx-1)
   integer              ::  lntstr(0:npesx-1),lntlen(0:npesx-1)
   integer              ::  lnpstr(0:npesx-1),lnplen(0:npesx-1)
   integer              ::  levdis(npesx),londis(npesx),lntdis(npesx)
   integer              ::  lwvdis(npesx),latdis(npesx),lnpdis(npesx)
   integer              ::  lwvdef(npesx)
!
   call equdiv(levr ,ncol,levdis)
   call equdiv(iwav1 ,nrow,lwvdis)
!
   do nr=1,nrow
     lnpdis(nr)=lwvdis(nr)*jwav1
   enddo                  

!
   latg2=jgrd1/2
   call equdiv(igrd1 ,ncol,londis)
   call equdiv(latg2 ,nrow,latdis)
!
   levmax=0
   lonmax=0
   latmax=0
   lwvmax=0
   lntmax=0
   lnpmax=0
!
   latpnt=1
   lwvpnt=1
   lntpnt=1
   lnppnt=1
   n=0
!
   do nr=1,nrow
!
     levpnt=1
     lonpnt=1
     call equdiv(lnpdis(nr),ncol,lntdis)
!
     do nc=1,ncol
!
       levstr(n)=levpnt
       levlen(n)=levdis(nc)
       levpnt=levpnt+levdis(nc)
       levmax=max(levmax,levlen(n))
!
       lonstr(n)=lonpnt
       lonlen(n)=londis(nc)
       lonpnt=lonpnt+londis(nc)
       lonmax=max(lonmax,lonlen(n))
!
       lntstr(n)=lntpnt
       lntlen(n)=lntdis(nc)
       lntpnt=lntpnt+lntdis(nc)
       lntmax=max(lntmax,lntlen(n))
!
       latstr(n)=latpnt
       latlen(n)=latdis(nr)
       latmax=max(latmax,latlen(n))
!
       lwvstr(n)=lwvpnt
       lwvlen(n)=lwvdis(nr)
       lwvmax=max(lwvmax,lwvlen(n))
!
       lnpstr(n)=lnppnt
       lnplen(n)=lnpdis(nr)
       lnpmax=max(lnpmax,lnplen(n))
!
       n=n+1
!
     enddo
!
     if( nr.lt.nrow ) then
       latpnt=latpnt+latdis(nr)
       lwvpnt=lwvpnt+lwvdis(nr)
       lnppnt=lnppnt+lnpdis(nr)
     endif
!
   enddo
!
   do n=0,npes-1
     lwvstr(n)=lwvstr(n)-1
     lnpstr(n)=lnpstr(n)-1
     lntstr(n)=lntstr(n)-1
   enddo
!
   levmax=levmax
   lonmax=lonmax
   latmax=latmax*2
   lwvmax=lwvmax   ! consider as iwv1p
   lntmax=lntmax
   lnpmax=lnpmax
!
   return
   end

   subroutine equdiv(len,ncut,lenarr)
!$$$  subprogram documentation block
!
! subprogram:    equdiv
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: cut len into ncut pieces with load balancing
!
! program history log:
!    99-06-27  henry juang    finish entire test for gsm
!
! usage:   equdiv(len,ncut,lenarr)
!
!    input argument lists:
!   len   - integer total length 
!   ncut   - integer number of subgroup
!
!    output argument list:
!   lenarr   - integer (ncut) length of each subgroup
! 
! subprograms called: none
!
! attributes:
!    language: fortran 90
!$$$
!
   implicit none
   integer              ::  len,ncut
   integer              ::  n0,n1,n
   integer              ::  lenarr(ncut)
   !
   n0=len/ncut
   n1=mod(len,ncut)
   do n=1,n1
     lenarr(n)=n0+1
   enddo
   do n=n1+1,ncut
     lenarr(n)=n0
   enddo
   return
   end

