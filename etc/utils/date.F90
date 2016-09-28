#include <machine.h>
   program date
!$$$  main program documentation block
!
! main program:  date    get date from file
!   prgmmr: kanamitsu          org: w/np51     date: 01-03-31
!
! abstract: get date from model files
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
   integer              ::  idate(4)
!
   character(len=120)   ::  fnam
#ifdef NCO_TAG
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   call w3tagb('clim_date',2001,0000,0000,'np51')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#endif
!   
   read(5,'(A120)') fnam
!
   open(unit=11,file=fnam,form='unformatted',err=900)
!
   read(11) 
!
   read(11) fhour,idate
!
   ihour=nint(fhour)
!
#ifdef NCO_TAG
   write(51,100) idate(1),idate(2),idate(3),idate(4),ihour
#endif
   write(6,100) idate(1),idate(2),idate(3),idate(4),ihour
100   format(4i6,i10)
#ifdef NCO_TAG
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   CALL w3tage('clim_date')
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#endif
   stop
!
900 continue
   write(6,*) 'file open error in program date'
   call abort
   end
