   program mpiset
!$$$  main program documentation block
!
! main program:    mainset
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
!     include 'dimset.F'
!     include 'equdiv.F'
!     include 'equdis.F'
!
! attributes:
!    language: fortran 90
!$$$
!
   implicit none
!
   integer              ::  npes,ncol,nrow,n,nc
!
   integer              ::  jcap,levs,lonf,latg,lermax
   integer              ::  levmax,lonmax,latmax,lntmax,lnpmax,lwvmax
!
   logical              ::  lreal4
   data lreal4/.false./
!
   real                 ::  pesx
   data ncol/0/, nrow/0/
!
   namelist /namset/ jcap,levs,lonf,latg,npes,ncol,nrow,lreal4
!
   read(5,namset)
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
     if(mod(npes,ncol).ne.0 ) then
       print *,' user provided npes=',npes
       print *,' with an invalid ncol=',ncol
       call abort
       stop
     endif
   endif
1234  nrow=npes/ncol      
   call dimset(jcap,levs,lonf,latg,                                            &
               npes,ncol,nrow,                                                 &
               levmax,                                                         &
               lermax,                                                         &
               lonmax,                                                         &
               latmax,                                                         &
               lwvmax,                                                         &
               lntmax,                                                         &
               lnpmax)
!
   write(*,101)
   if(.not.lreal4) then
     write(*,102)
     write(*,103)
   else
     write(*,202)
     write(*,203)
   endif
   write(*,104) npes
   write(*,105) ncol
   write(*,106) nrow
   write(*,107) levmax
   write(*,117) lermax
   write(*,108) lonmax
   write(*,109) latmax
   write(*,110) lwvmax
   write(*,111) lntmax
   write(*,112) lnpmax
!
 101  format('#define MP')
 102  format('#define MPI_REAL mpi_real8')
 103  format('#define _mpi_real_ 8')
 202  format('#define MPI_REAL mpi_real')
 203  format('#define _mpi_real_ 4')
 104  format('#define _npes_ ',i4)
 105  format('#define _ncol_ ',i4)
 106  format('#define _nrow_ ',i4)
 107  format('#define _levsp_ ',i3)
 117  format('#define _levsr_ ',i3)
 108  format('#define _lonfp_ ',i4)
 109  format('#define _latgp_ ',i4)
 110  format('#define _jcapp_ ',i3)
 111  format('#define _lntp_ ',i6)
 112  format('#define _llnp_ ',i6)
 
   stop
   end
   !
   subroutine dimset(jcap,levs,lonf,latg,                                      &
                     npes,ncol,nrow,                                           &
                     levmax,                                                   &
                     lermax,                                                   &
                     lonmax,                                                   &
                     latmax,                                                   &
                     lwvmax,                                                   &
                     lntmax,                                                   &
                     lnpmax)
!$$$  subprogram documentation block
!
! subprogram:    dimset
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: preset all starting point and length for 
!           all pe for global spectral model.
!
! program history log:
!    99-06-27  henry juang    finish entire test for gsm
!    00-03-09  henry juang    update with symmetry distribution
!                               prepare for reduce grid.
!
! usage:   call dimset(jcap,levs,lonf,latg,
!    *                  npes,ncol,nrow,
!    *                  levstr,levlen,levmax,
!    *                  lonstr,lonlen,lonmax,
!    *                  latstr,latlen,latmax,
!    *                  lwvstr,lwvlen,lwvmax,
!    *                  lntstr,lntlen,lntmax,
!    *                  lnpstr,lnplen,lnpmax)
!
!    input argument lists:
!   jcap   - integer spectral wavenumber
!   levs   - integer vertical layer number
!   lonf   - integer gaussian grid for longitude
!   latg   - integer gaussian grid for latitude
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
!   equdiv   - to compute about equal number of subgroup 
!   equdis   - to compute about equal number of subgroup by spread
!
! attributes:
!    language: fortran 90
!$$$
!
   implicit none
   integer              ::  jcap,levs,lonf,latg,npes,ncol,nrow
   integer              ::  latg2,lonf2,nr,nc,nn,jcaprm,nremain
   integer              ::  lnp,n2a,lx,ll,lh,n
   integer              ::  levmax,lonmax,latmax,lwvmax,lntmax,lnpmax,lermax
   integer              ::  levpnt,lonpnt,latpnt,lwvpnt,lntpnt,lnppnt,lerpnt
   integer,parameter    ::  npesx=10000
   integer              ::  levstr(0:npesx-1),levlen(0:npesx-1)
   integer              ::  lerstr(0:npesx-1),lerlen(0:npesx-1)
   integer              ::  lonstr(0:npesx-1),lonlen(0:npesx-1)
   integer              ::  latstr(0:npesx-1),latlen(0:npesx-1)
   integer              ::  lwvstr(0:npesx-1),lwvlen(0:npesx-1)
   integer              ::  lntstr(0:npesx-1),lntlen(0:npesx-1)
   integer              ::  lnpstr(0:npesx-1),lnplen(0:npesx-1)
   integer              ::  levdis(npesx),londis(npesx),lntdis(npesx)
   integer              ::  lwvdis(npesx),latdis(npesx),lnpdis(npesx)
   integer              ::  lwvdef(npesx),lerdis(npesx)
!
   call equdiv(levs ,ncol,levdis)
   call equdiv(levs ,nrow,lerdis)
   call equdiv(lonf ,ncol,londis)
   latg2=latg/2
   call equdis(-1,latg2 ,nrow,latdis,lwvdef)
   call equdis( 1,jcap+1,nrow,lwvdis,lwvdef)
   lh=0
   do nr=1,nrow
     lnpdis(nr)=0
     lx=lh+1
     lh=lh+lwvdis(nr)
     do n=lx,lh
       lnpdis(nr)=lnpdis(nr)+jcap+2-lwvdef(n)
     enddo
   enddo
!
   levmax=0
   lermax=0
   lonmax=0
   latmax=0
   lwvmax=0
   lntmax=0
   lnpmax=0
!
   lerpnt=1
   latpnt=1
   lwvpnt=1
   lntpnt=1
   lnppnt=1
   n=0
!
   do nr=1,nrow
!
     nremain=0
!       if(nr.eq.nrow) nremain=jcaprm
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
       lerstr(n)=lerpnt
       lerlen(n)=lerdis(nr)
       lermax=max(lermax,lerlen(n))
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
!         print*,' n nc lntlen lntmax ',n,nc,lntlen(n),lntmax
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
       lnppnt=lnppnt+lnpdis(nr)
       lerpnt=lerpnt+lerdis(nr)
       latpnt=latpnt+latdis(nr)
       lwvpnt=lwvpnt+lwvdis(nr)
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
   lwvmax=lwvmax
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

   subroutine equdis(ind,len,ncut,lenarr,lendef)
!$$$  subprogram documentation block
!
! subprogram:    equdis
!            
! prgmmr: hann-ming henry juang    org:w/np51   date:99-05-01
!
! abstract: cut len into ncut pieces with load balancing by 
!           symmetric distribution
!
! program history log:
!    00-03-03  henry juang    for symmetry dirstribution
!
! usage:   equdiv(len,ncut,lenarr)
!
!    input argument lists:
!   ind   - integer spread direction: 1 for regular,
!                                          -1 for reverse
!   len   - integer total length 
!   ncut   - integer number of subgroup
!
!    output argument list:
!   lenarr   - integer (ncut) length of each subgroup
!   lendef   - integer (len) redefine the index 
! 
! subprograms called: none
!
! attributes:
!    language: fortran 90
!$$$
!
   implicit none
   integer              ::  ind,len,ncut
   integer              ::  nn,n,i,lens,lene
   integer              ::  lenarr(ncut),lendef(len)
   integer, allocatable :: lentmp(:)
   allocate(lentmp(len))
!
   do i=1,ncut
     lenarr(i)=0
   enddo
   if( ind.eq.1 ) then
     lens=1
     lene=len
   else
     lens=len
     lene=1
   endif
   i=1
   n=1
   do nn=lens,lene,ind
     lenarr(n)=lenarr(n)+1
     lentmp(nn)=n
     n=n+i
     if(n.eq.ncut+1) then
       i=-1
       n=n+i
     endif
     if(n.eq.0) then
       i=1
       n=n+i
     endif
   enddo
!
   n=0
   do i=1,ncut
     do nn=1,len
       if(lentmp(nn).eq.i) then
         n=n+1
         lendef(n)=nn
       endif
     enddo
   enddo
!
   deallocate(lentmp)
!
   return
   end
