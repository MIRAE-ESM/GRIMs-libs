#include <machine.h>
   program incdte
!$$$  main program documentation block
!
! main program:  incdte    add increment date
!   prgmmr: kanamitsu          org: w/np51     date: 01-03-31
!
! abstract: compute the increment of date 
!
! program history log:
!   01-03-31  hann-ming juang  add w3tag calls for nco implementation
!
! namelists:
!   namin:      parameters determining new date
!
! input files:
!   unit   11  sigma file(s)
!
! output files:
!   unit   51  sigma file
!
! subprograms called:
!
! attributes:
!   language: fortran
!
!$$$
!
#ifdef NCO_TAG
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   call w3tagb('clim_incdte',2001,0000,0000,'np51')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#endif
!
   read (5,*) iyv,imv,idv,ihv,inc
!
   if(inc.ge.0) then
     incdy=inc/24
     ihv=ihv+mod(inc,24)
     incdy=incdy+ihv/24
     ihv=mod(ihv,24)
     n=1
     do while (n.le.incdy)
       idv=idv+1
       if(imv.eq.4.or.imv.eq.6.or.imv.eq.9.or.imv.eq.11) then
         mondy=30
       elseif(imv.eq.2) then
         if(mod(iyv,4).eq.0) then
           if (mod(iyv,100).eq.0.and.mod(iyv,400).ne.0) then
              mondy=28
           else
              mondy=29
           endif
         else
           mondy=28
         endif
       else
         mondy=31
       endif
       if(idv.gt.mondy) then
         imv=imv+1
         idv=1
         if(imv.gt.12) then
           iyv=iyv+1
           imv=1
         endif
       endif
       n=n+1
     enddo
!
   else
     if(inc+ihv.lt.0) then
       incdy=(ihv+inc+1)/24-1
       ihv=ihv+inc-24*incdy
     else
       incdy=0
       ihv=ihv+inc
     endif
     n=incdy
     do while(n.lt.0)
       idv=idv-1
       if(idv.le.0) then
         imv=imv-1
         if(imv.le.0) then
           iyv=iyv-1
           imv=12
         endif
         if(imv.eq.4.or.imv.eq.6.or.imv.eq.9.or.imv.eq.11) then
           mondy=30
         elseif(imv.eq.2) then
           if(mod(iyv,4).eq.0) then
             if (mod(iyv,100).eq.0.and.mod(iyv,400).ne.0) then
                mondy=28
             else
                mondy=29
             endif
           else
              mondy=28
           endif
         else
           mondy=31
         endif
         idv=mondy
       endif
       n=n+1
     enddo
   endif
!
#ifdef NCO_TAG
   write(51,100) iyv,imv,idv,ihv
#endif
   write(6,100) iyv,imv,idv,ihv
  100 format(4i5)
#ifdef NCO_TAG
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   CALL w3tage('clim_incdte')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#endif
!
   stop
   end
