   subroutine sfcfld(sfcftyp,iflag,                                            &
                     nrecs,lev,svar,maxlev)
!
!  returns number of 2-D fields per record in the surface file.
!
!  input:
!   sfcftyp: character ['osu1','osu2','noa1']
!   iflag  : integer.  =0 only returns nrecs and maxlev (for allocate)
!                      =l regular return
!      call with iflag=0, allocate the array and call with iflag=1
!  output:
!   nrecs:  integer  .. number of records
!   lev  :  int  array (nrecs)  .. number of 2-D fields in the record
!   svar :  char array (nrecs)  .. short variable name
!   maxlev: int      .. max level/type in this file
!   
!  note that mountain variance is removed from regional model sfc file
!  to make it consistent with global model   
!   
! 
!      osu1       osu2       noa1         vic1  
!-----------------------------------------------
!  1   ts         ts         ts            ts
!  2   smc(ksoil) smc(ksoil) smc(ksoil)    smc(ksoil)
!  3   sno        sno        sno           sno
!  4   stc(ksoil) stc(ksoil) stc(ksoil)    stc(nsoil)
!  5   tg3        tg3        tg3           tg3
!  6   z0         z0         z0            z0
!  7   cv         cv         cv            cv   
!  8   cvb        cvb        cvb           cvb
!  9   cvt        cvt        cvt           cvt
! 10   alb(kalbd) alb(kalbd) alb(kalbd)    alb(kalbd)
! 11   sli        sli        sli           sli
! 12   plantr     vfrac      vfrac         vfrac
! 13   canop      canop      canop         canop
! 14   f10m       f10m       f10m          f10m
! 15   paer(5)    vegtyp     vegtyp        vegtyo
! 16   kprfi      soiltyp    soiltyp       rt(ksoil)
! 17   denni(2)   albf(2)    albf(2)       albf(2)
! 18   idxci(5)   ustar      ustar         ustar
! 19   cmixi(5)   fm         fm            fm
! 20              fh         fh            fh
! 21              omld       prcp          prcp
! 22              paer(5)    srflag        srflag
! 23              kprfi      snodph        binf
! 24              denni(2)   slc(ksoil)    ds
! 25              idxci(5)   shdmin        dsm
! 26              cmixi(5)   shdmax        ws
! 27                         slope         cef
! 28                         snoalb        expt(ksoil)
! 29                         omld          kst(ksoil)
! 30                         paer(5)       dph(ksoil)
! 31                         kprfi         bub(ksoil)
! 32                         denni(2)      qrt(ksoil)
! 33                         idxci(5)      bkd(ksoil)
! 34                         cmixi(5)      sld(ksoil)
! 35                                       wcr(ksoil)
! 36                                       wpw(ksoil)
! 37                                       smr(ksoil)
! 38                                       rmx(ksoil)
! 39                                       dphn(ksoil)
! 40                                       smxn(ksoil)
! 41                                       expn(ksoil)
! 42                                       bubn(ksoil)
! 43                                       alpn(ksoil)
! 44                                       betn(ksoil)
! 45                                       gamn(ksoil)
! 46                                       flai
! 47                                       silz
! 48                                       snwz
! 49                                       sic(ksoil)
! 50                                       csno
! 51                                       rsno
! 52                                       tsf
! 53                                       tpk
! 54                                       sfw
! 55                                       pkw
! 56                                       lstsn
! 57                                       omld
! 58                                       paer(5)
! 59                                       kprfi   
! 60                                       denni(2)
! 61                                       idxci(5)
! 62                                       cmixi(5)

   implicit none
!
   character(LEN=4)  ::  sfcftyp
   integer           ::  iflag
   integer           ::  ksoil(100)
!
   integer           ::  lev(*)
   character(LEN=8)  ::  svar(*)
!
   integer           ::  nrecs,kalbd,maxlev
!
   if(sfcftyp(1:4).eq.'osu1') then
      nrecs=14 
      ksoil(1)=2
      kalbd=1
      maxlev=2
      if(iflag.eq.0) return
      lev( 1)=1
      lev( 2)=ksoil(1)
      lev( 3)=1
      lev( 4)=ksoil(1)
      lev( 5)=1
      lev( 6)=1
      lev( 7)=1
      lev( 8)=1
      lev( 9)=1
      lev(10)=kalbd
      lev(11)=1
      lev(12)=1
      lev(13)=1
      lev(14)=1
      svar( 1)='ts'
      svar( 2)='smc'
      svar( 3)='sno'
      svar( 4)='stc'
      svar( 5)='tg3'
      svar( 6)='z0'
      svar( 7)='cv'
      svar( 8)='cvb'
      svar( 9)='cvt'
      svar(10)='alb'
      svar(11)='sli'
      svar(12)='plantr'
      svar(13)='canop'
      svar(14)='f10m'
   elseif(sfcftyp(1:4).eq.'osu2') then
      nrecs=26 
      ksoil(1)=2
      kalbd=4
      maxlev=5
      if(iflag.eq.0) return
      lev( 1)=1
      lev( 2)=ksoil(1)
      lev( 3)=1
      lev( 4)=ksoil(1)
      lev( 5)=1
      lev( 6)=1
      lev( 7)=1
      lev( 8)=1
      lev( 9)=1
      lev(10)=kalbd
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
      lev(22)=5
      lev(23)=1
      lev(24)=2
      lev(25)=5
      lev(26)=5
      svar( 1)='ts'
      svar( 2)='smc'
      svar( 3)='sno'
      svar( 4)='stc'
      svar( 5)='tg3'
      svar( 6)='z0'
      svar( 7)='cv'
      svar( 8)='cvb'
      svar( 9)='cvt'
      svar(10)='alb'
      svar(11)='sli'
      svar(12)='vegcov'
      svar(13)='canop'
      svar(14)='f10m'
      svar(15)='vegtyp'
      svar(16)='soiltyp'
      svar(17)='albf'
      svar(18)='ustar'
      svar(19)='fm'
      svar(20)='fh'
      svar(21)='omld'
      svar(22)='paer'
      svar(23)='kprfi'
      svar(24)='denni'
      svar(25)='idxci'
      svar(26)='cmixi'
   elseif(sfcftyp(1:4).eq.'noa1') then
      nrecs=34 
      ksoil(1)=4
      kalbd=4
      maxlev=5
      if(iflag.eq.0) return
      lev( 1)=1
      lev( 2)=ksoil(1)
      lev( 3)=1
      lev( 4)=ksoil(1)
      lev( 5)=1
      lev( 6)=1
      lev( 7)=1
      lev( 8)=1
      lev( 9)=1
      lev(10)=kalbd
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
      lev(24)=ksoil(1)
      lev(25)=1
      lev(26)=1
      lev(27)=1
      lev(28)=1
      lev(29)=1
      lev(30)=5
      lev(31)=1
      lev(32)=2
      lev(33)=5
      lev(34)=5
      svar( 1)='ts'
      svar( 2)='smc'
      svar( 3)='sno'
      svar( 4)='stc'
      svar( 5)='tg3'
      svar( 6)='z0'
      svar( 7)='cv'
      svar( 8)='cvb'
      svar( 9)='cvt'
      svar(10)='alb'
      svar(11)='sli'
      svar(12)='vegcov'
      svar(13)='canop'
      svar(14)='f10m'
      svar(15)='vegtyp'
      svar(16)='soiltyp'
      svar(17)='albf'
      svar(18)='ustar'
      svar(19)='fm'
      svar(20)='fh'
      svar(21)='prcp'
      svar(22)='srflag'
      svar(23)='snodph'
      svar(24)='slc'
      svar(25)='shdmin'
      svar(26)='shdmax'
      svar(27)='slope'
      svar(28)='snoalb'
      svar(29)='omld'
      svar(30)='paer'
      svar(31)='kprfi'
      svar(32)='denni'
      svar(33)='idxci'
      svar(34)='cmixi'
   elseif(sfcftyp(1:4).eq.'vic1') then
      nrecs=62
      ksoil(1)=3
      ksoil(2)=5
      kalbd=4
      maxlev=5
      if(iflag.eq.0) return
      lev( 1)=1
      lev( 2)=ksoil(1)
      lev( 3)=1
      lev( 4)=ksoil(2)
      lev( 5)=1
      lev( 6)=1
      lev( 7)=1
      lev( 8)=1
      lev( 9)=1
      lev(10)=kalbd
      lev(11)=1
      lev(12)=1
      lev(13)=1
      lev(14)=1
      lev(15)=1
      lev(16)=ksoil(1)
      lev(17)=2
      lev(18)=1
      lev(19)=1
      lev(20)=1
      lev(21)=1
      lev(22)=1
      lev(23)=1
      lev(24)=1
      lev(25)=1
      lev(26)=1
      lev(27)=1
      lev(28)=ksoil(1)
      lev(29)=ksoil(1)
      lev(30)=ksoil(1)
      lev(31)=ksoil(1)
      lev(32)=ksoil(1)
      lev(33)=ksoil(1)
      lev(34)=ksoil(1)
      lev(35)=ksoil(1)
      lev(36)=ksoil(1)
      lev(37)=ksoil(1)
      lev(38)=ksoil(1)
      lev(39)=ksoil(2)
      lev(40)=ksoil(2)
      lev(41)=ksoil(2)
      lev(42)=ksoil(2)
      lev(43)=ksoil(2)
      lev(44)=ksoil(2)
      lev(45)=ksoil(2)
      lev(46)=1
      lev(47)=1
      lev(48)=1
      lev(49)=ksoil(1)
      lev(50)=1
      lev(51)=1
      lev(52)=1
      lev(53)=1
      lev(54)=1
      lev(55)=1
      lev(56)=1
      lev(57)=1
      lev(58)=5
      lev(59)=1
      lev(60)=2
      lev(61)=5
      lev(62)=5
      svar( 1)='ts'
      svar( 2)='smc'
      svar( 3)='sno'
      svar( 4)='stc'
      svar( 5)='tg3'
      svar( 6)='z0'
      svar( 7)='cv'
      svar( 8)='cvb'
      svar( 9)='cvt'
      svar(10)='alb'
      svar(11)='sli'
      svar(12)='vegcov'
      svar(13)='canop'
      svar(14)='f10m'
      svar(15)='vegtyp'
      svar(16)='root'
      svar(17)='albf'
      svar(18)='ustar'
      svar(19)='fm'
      svar(20)='fh'
      svar(21)='prcp'
      svar(22)='srflag'
      svar(23)='binf'
      svar(24)='ds'
      svar(25)='dsm'
      svar(26)='ws'
      svar(27)='cef'
      svar(28)='expt'
      svar(29)='kst'
      svar(30)='dph'
      svar(31)='bub'
      svar(32)='qrt'
      svar(33)='bkd'
      svar(34)='sld'
      svar(35)='wcr'
      svar(36)='wpw'
      svar(37)='smr'
      svar(38)='smx'
      svar(39)='dphn'
      svar(40)='smxn'
      svar(41)='expn'
      svar(42)='bubn'
      svar(43)='alpn'
      svar(44)='betn'
      svar(45)='gamn'
      svar(46)='flai'
      svar(47)='silz'
      svar(48)='snwz'
      svar(49)='sic'
      svar(50)='csno'
      svar(51)='rsno'
      svar(52)='tsf'
      svar(53)='tpk'
      svar(54)='sfw'
      svar(55)='pkw'
      svar(56)='lstsn'
      svar(57)='omld'
      svar(58)='paer'
      svar(59)='kprfi'
      svar(60)='denni'
      svar(61)='idxci'
      svar(62)='cmixi'
   else
      print *,'no such sfc file type'
      call abort
   endif
!
   return
   end
