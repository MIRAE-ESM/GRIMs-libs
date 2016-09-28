      program cray2ie3
c
c  Converts sigma or surface files written on cray
c  (cos blocked but cray binary) to ieee format
c
c  Written by M. Kanamitsu 10/26/98
c
c  This program uses ncaru routine
c
      character*80 fname
c
      character*8 label(4)
c
      parameter(lh=1000,la=80000)
      dimension rhead(lh)
      dimension ihead(lh)
      dimension array(la)
c
      integer crayopen, crayread, crayclose, crayback
c
      namelist/namfl/ fname
c
      iunit=11
      read(5,namfl)
      write(6,namfl)
      print *,'iunit=',iunit
c
      nrec=1
      icf=crayopen(fname, 0, o'660')
      if(icf .le. 0) then
        print *,'Unable to open file=',fname,' icf=',icf
        call abort
      endif
c
      nwds = crayread(icf, label, 4, 0)
      if (nwds.ne.4) then
        print *, "Unable to read label character record"
        print *, "nwds=",nwds
        call abort
      endif
      print *,label
      write(51) label
c
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
  100 continue
c
      nrec=nrec+1
      nwds = crayread(icf,  array, la, 1)
      if (nwds.le.0) then
        print*, "Unable to read history array or eof/eod"
        stop
      endif
      print *,'nwds=',nwds,' array(1)=',array(1)
      write(51)(array(i),i=1,nwds)
      go to 100
c
      end
