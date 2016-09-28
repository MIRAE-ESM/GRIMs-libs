   SUBROUTINE GBYTES(IN,IOUT,ISKIP,NBYTE,NSKIP,N)
!          Get bytes - unpack bits:  Extract arbitrary size values from a
!          packed bit string, right justifying each value in the unpacked
!          array.
!            IN    = character*1 array input
!            IOUT  = unpacked array output
!            ISKIP = initial number of bits to skip
!            NBYTE = number of bits to take
!            NSKIP = additional number of bits to skip on each iteration
!            N     = number of iterations
! v1.1
!

   integer, dimension(8) ::  ones
   character(len=1)      ::  in(*)
   integer               ::  iout(*)
   integer               ::  tbit, bitcnt
   save ones
   data ones /1,3,7,15,31,63,127,255/

   ! nbit is the start position of the field in bits
   nbit = iskip
   do i = 1, n
      bitcnt = nbyte
      index=nbit/8+1
      ibit=mod(nbit,8)
      nbit = nbit + nbyte + nskip

      ! first byte
      tbit = min(bitcnt,8-ibit)
      itmp = and(ichar(in(index)),ones(8-ibit))
      if (tbit.ne.8-ibit) itmp = ishft(itmp,tbit-8+ibit)
      index = index + 1
      bitcnt = bitcnt - tbit

      ! now transfer whole bytes
      do while (bitcnt.ge.8)
         itmp = or(ishft(itmp,8),ichar(in(index)))
         bitcnt = bitcnt - 8
         index = index + 1
      enddo

      ! get data from last byte
      if (bitcnt.gt.0) then
          itmp = or(ishft(itmp,bitcnt), and(ishft(ichar(in(index)),            &
             -(8-bitcnt)),ones(bitcnt)))
      endif

      iout(i) = itmp
   enddo

   RETURN
   END                                                                  
