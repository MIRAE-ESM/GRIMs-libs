   subroutine incdte(iy,im,id,ih,jy,jm,jd,jh,inchr)
!
!  compute jy,jm,jd,jh of forecast
!
   jy=iy
   jm=im
   jd=id
   incdy=inchr/24
   jh=ih+mod(inchr,24)
   incdy=incdy+jh/24
   jh=mod(jh,24)
   do incd=1,incdy
      jd=jd+1
   if(jm.eq.4.or.jm.eq.6.or.jm.eq.9.or.jm.eq.11) then
      mondy=30
   elseif(jm.eq.2) then
      if(mod(jy,4).eq.0) then
         mondy=29
      else
         mondy=28
      endif
   else
      mondy=31
   endif
   if(jd.gt.mondy) then
      jm=jm+1
      jd=1
      if(jm.gt.12) then
         jy=jy+1
         jm=1
      endif
   endif
   enddo
!
   return
   end
