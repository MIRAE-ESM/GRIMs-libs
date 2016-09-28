   subroutine rdsfc(gsm0rsm,sfcftyp,grid,ijdim,ksoil)
!
!  read surface file of various kinds, including rsm and gsm
!
!       osu1        osu2        noa1        osu1-rsm        osu2-rsm      noa1-rsm
!
!   1   ts          ts          ts          ts              ts            ts
!   2   smc(ksoil)  smc(ksoil)  smc(ksoil)  smc(ksoil)      smc(ksoil)    smc(ksoil)
!   3   sno         sno         sno         sno             sno           sno
!   4   stc(ksoil)  stc(ksoil)  stc(ksoil)  stc(ksoil)      stc(ksoil)    stc(ksoil)
!   5   tg3         tg3         tg3         tg3             tg3           tg3
!   6   z0          z0          z0          z0              z0            z0
!   7   cv          cv          cv          cv              cv            cv
!   8   cvb         cvb         cvb         cvb             cvb           cvb
!   9   cvt         cvt         cvt         cvt             cvt           cvt
!   10  alb(1)      alb(4)      alb(4)      alb(1)          alb(4)        alb(4)
!   11  sli         sli         sli         sli             sli           sli
!   12  plantr      vfrac       vfrac       vfrac           vfrac         vfrac
!   13  canop       canop       canop       hprime          hprime        hprime
!   14  f10m        f10m        f10m        canop           canop         canop
!   15              vegtyp      vegtyp      f10m            f10m          f10m
!   16              soiltyp     soiltyp                     vegtyp        vegtyp
!   17              albf(2)     albf(2)                     soiltyp       soiltyp
!   18              ustar       ustar                       albf(2)       albf(2)
!   19              fm          fm                          ustar         ustar
!   20              fh          fh                          fm            fm
!   21                          prcp                        fh            fh
!   22                          srflag                                    prcp
!   23                          snodph                                    srflag
!   24                          slc(ksoil)                                sbidph
!   25                          shdmin                                    slc(ksoil)
!   26                          shdmax                                    shdmin
!   27                          slope                                     shdmax
!   28                          snoalb                                    slope
!   29                                                                    snoalb
   real              ::  grid(ijdim,*)
!
   character(LEN=3)  ::  gsm0rsm
   character(LEN=4)  ::  sfcftyp
!
   integer           ::  lev(500)
!
   do i=1,500
      lev(i)=0
   enddo
!
   if(gsm0rsm(1:3).eq.'gsm') then
      if(sfcftyp(1:4).eq.'osu1') then
         kend=14 
         lev( 1)=1
         lev( 2)=ksoil
         lev( 3)=1
         lev( 4)=ksoil
         lev( 5)=1
         lev( 6)=1
         lev( 7)=1
         lev( 8)=1
         lev( 9)=1
         lev(10)=1
         lev(11)=1
         lev(12)=1
         lev(13)=1
         lev(14)=1
      elseif(sfcftyp(1:4).eq.'osu2') then
         kend=20 
         lev( 1)=1
         lev( 2)=ksoil
         lev( 3)=1
         lev( 4)=ksoil
         lev( 5)=1
         lev( 6)=1
         lev( 7)=1
         lev( 8)=1
         lev( 9)=1
         lev(10)=4
         lev(11)=1
         lev(12)=1
         lev(13)=1
         lev(14)=1
         lev(15)=1
         lev(16)=1
         lev(17)=2
         lev(18)=1
         lev(19)=1
         lev(20)=1
      elseif(sfcftyp(1:4).eq.'noa1') then
         kend=28 
         lev( 1)=1
         lev( 2)=ksoil
         lev( 3)=1
         lev( 4)=ksoil
         lev( 5)=1
         lev( 6)=1
         lev( 7)=1
         lev( 8)=1
         lev( 9)=1
         lev(10)=4
         lev(11)=1
         lev(12)=1
         lev(13)=1
         lev(14)=1
         lev(15)=1
         lev(16)=1
         lev(17)=2
         lev(18)=1
         lev(19)=1
         lev(20)=1
         lev(21)=1
         lev(22)=1
         lev(23)=1
         lev(24)=ksoil
         lev(25)=1
         lev(26)=1
         lev(27)=1
         lev(28)=1
      else
         print *,'no such sfc file type'
         call abort
      endif
   elseif(gsm0rsm(1:3).eq.'rsm') then
      if(sfcftyp(1:4).eq.'osu1') then
         kend=15 
         lev( 1)=1
         lev( 2)=ksoil
         lev( 3)=1
         lev( 4)=ksoil
         lev( 5)=1
         lev( 6)=1
         lev( 7)=1
         lev( 8)=1
         lev( 9)=1
         lev(10)=1
         lev(11)=1
         lev(12)=1
         lev(13)=1
         lev(14)=1
         lev(15)=1
      elseif(sfcftyp(1:4).eq.'osu2') then
         kend=21 
         lev( 1)=1
         lev( 2)=ksoil
         lev( 3)=1
         lev( 4)=ksoil
         lev( 5)=1
         lev( 6)=1
         lev( 7)=1
         lev( 8)=1
         lev( 9)=1
         lev(10)=4
         lev(11)=1
         lev(12)=1
         lev(13)=1
         lev(14)=1
         lev(15)=1
         lev(16)=1
         lev(17)=1
         lev(18)=2
         lev(19)=1
         lev(20)=1
         lev(21)=1
      elseif(sfcftyp(1:4).eq.'noa1') then
         kend=29 
         lev( 1)=1
         lev( 2)=ksoil
         lev( 3)=1
         lev( 4)=ksoil
         lev( 5)=1
         lev( 6)=1
         lev( 7)=1
         lev( 8)=1
         lev( 9)=1
         lev(10)=4
         lev(11)=1
         lev(12)=1
         lev(13)=1
         lev(14)=1
         lev(15)=1
         lev(16)=1
         lev(17)=1
         lev(18)=2
         lev(19)=1
         lev(20)=1
         lev(21)=1
         lev(22)=1
         lev(23)=1
         lev(24)=1
         lev(25)=ksoil
         lev(26)=1
         lev(27)=1
         lev(28)=1
      else
         print *,'no such sfc file type'
         call abort
      endif
   else
      print *,'no such model choices'
      call abort
   endif
!
   do k=1,kend
      read(12,end=909) ((grid(ij,l),ij=1,ijdim),l=1,lev(k))
      write(6,*) ' write k=',k
      write(52) ((grid(ij,l),ij=1,ijdim),l=1,lev(k))
   enddo
   stop
!
909 continue
   print *,'hit eof reading unit 12'
   call abort
!
   return
   end
