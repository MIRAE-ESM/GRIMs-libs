!
   program cray2ie3
!
!-------------------------------------------------------------------------------
!
!  Converts sigma or surface files written on cray
!  (cos blocked but cray binary) to ieee format
!
!  Written by M. Kanamitsu 10/26/98
!
!  This program uses ncaru routine
!
!-------------------------------------------------------------------------------
!
   integer, parameter   ::  lh = 1000,la = 80000
   character(len = 80)  ::  fname
   character(len = 8 )  ::  label(4)
   real                 ::  rhead(lh)
   real                 ::  array(la)
   integer              ::  ihead(lh)
   integer              ::  crayopen, crayread, crayclose, crayback
!
   namelist/namfl/ fname
!
   iunit=11
   read(5,namfl)
   write(6,namfl)
   print *,'iunit=',iunit
!
   nrec=1
   icf=crayopen(fname, 0, o'660')
   if(icf .le. 0) then
      print *,'Unable to open file=',fname,' icf=',icf
      call abort
   endif
!
   nwds = crayread(icf, label, 4, 0)
   if (nwds.ne.4) then
      print *, "Unable to read label character record"
      print *, "nwds=",nwds
      call abort
   endif
   print *,label
   write(51) label
!
   nwds = crayread(icf, rhead, lh, 1)
   print *,nwds
   if (nwds.le.0) then
      print *, "Unable to read 2nd header record as real"
      call abort
   endif
   ier = crayback(icf)
   if (ier.ne.0) then
      print *, "FATAL - Unable to backspace, ier = ", ier
      call abort
   endif
   print *,"  Backspace done for reading integer header"
   nwds = crayread(icf, ihead, lh, 3)
   print *,nwds
   if (nwds.le.0) then
      print *, "Unable to read 2nd header record as integer"
      call abort
   endif
   print *,'fhour=',rhead(1),' idate=',(ihead(i),i=2,5)
   print *,'si,sl=',(rhead(i),i=6,100)
   write(51) rhead(1),(ihead(i),i=2,5),(rhead(i),i=6,nwds)
   nrec=2
   !
   100 continue
   !
!
   nrec=nrec+1
   nwds = crayread(icf,  array, la, 1)
   if (nwds.le.0) then
      print*, "Unable to read history array or eof/eod"
      stop
   endif
   print *,'nwds=',nwds,' array(1)=',array(1)
   write(51)(array(i),i=1,nwds)
   !
   go to 100
   !
!
   end
