      program msgjb
      character*8 subset,subtab(1000)
      dimension   nsub(1000)
      data ntab   /0/

      call openbf(8,'IN',8)

1     call readmg(8,subset,idate,iret)
      if(iret.ne.0) goto 100               

      do n=1,ntab
      if(subset.eq.subtab(n)) then
         nsub(n) = nsub(n)+1
         goto 1
      endif
      enddo

      ntab = ntab+1
      subtab(ntab) = subset
      nsub(ntab) = 1
      goto 1
      
100   do n=1,ntab
      print*,subtab(n),' occuring ',nsub(n),' times'
      enddo

      print*,'processing ended normally'
      stop
      end
