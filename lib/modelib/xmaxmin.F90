   subroutine xmaxmin(a,idim,ilen,klen,ch)

   dimension a(idim,*)
   character ch*(*)
!
   do k=1,klen
      aamax = a(1,k)
      aamin = a(1,k)
         do i=1,ilen
            aamax = max( aamax, a(i,k) )
            aamin = min( aamin, a(i,k) )
         enddo
      print 100,ch,aamax,aamin,k
      100 format(a12,' has max=',e10.4,' min=',e10.4,' k=',i4)
   enddo
   return
   end
