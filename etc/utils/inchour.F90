#include <machine.h>
   program inchour
!$$$  main program documentation block
!
! main program:  inchour    get increment hour
!   prgmmr: kanamitsu          org: w/np51     date: 01-03-31
!
! abstract:  compute increment hour
!
! program history log:
!   01-03-31  hann-ming juang  add w3tag calls for nco implementation
!
! namelists:
!   namin:      parameters determining new date
!
! input files:
!
! output files:
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
   call w3tagb('clim_inchour',2001,0000,0000,'np51')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#endif
!
!  given two dates, compute hour increment
!
   read(5,*) iys,ims,ids,ihs,iye,ime,ide,ihe
   call compjd(iye,ime,ide,ihe,0,jde,fjde)
   call compjd(iys,ims,ids,ihs,0,jds,fjds)
   inc=(float(jde-jds)+fjde-fjds)*24
#ifdef NCO_TAG
   write(51,*) inc
#endif
   print *,inc
#ifdef NCO_TAG
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   CALL w3tage('clim_inchour')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#endif
   stop
   end
!
   subroutine compjd(jyr,jmnth,jday,jhr,jmn,jd,fjd)
!
   integer              ::  ndm(12)
   data jdor/2415019/,jyr19/1900/
   data ndm/0,31,59,90,120,151,181,212,243,273,304,334/
!     
      jd=jdor
      jyrm9=jyr-jyr19
      lp=jyrm9/4
      if(lp.gt.0) then
        jd=jd+1461*lp-1
      endif
      ny=jyrm9-4*lp
      ic=0
      if(ny.gt.0) then
        jd=jd+365*ny+1
!       Start of new lines
        if(lp.eq.0) then
           jd=jd-1
        else
           if(mod(jyr-ny,400).ne.0.and.mod(jyr-ny,100).eq.0) then
              jd=jd-1
           endif
        endif
!       End of new lines
      else
        if(jmnth.gt.2) ic=1
!       Start of new lines
        if(lp.eq.0) ic=0
        if(mod(jyr,400).ne.0.and.mod(jyr,100).eq.0) then
           ic=0
        endif
!       End of new lines
      endif
      jd=jd+ndm(jmnth)+jday+ic
      if(jhr.ge.12) then
        fjd=.041666667e0*float(jhr-12)+.00069444444e0*float(jmn)
      else
        jd=jd-1
        fjd=.5e0+.041666667e0*float(jhr)+.00069444444e0*float(jmn)
      endif
      return
      end
