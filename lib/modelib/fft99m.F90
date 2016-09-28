   subroutine fax(ifax,n,mode)
   integer  ::  ifax(*)
!
   nn=n
   if (iabs(mode).eq.1) go to 10
   if (iabs(mode).eq.8) go to 10
   nn=n/2
   if ((nn+nn).eq.n) go to 10
   ifax(1)=-99
   return
10 k=1
!     test for factors of 4
20 if (mod(nn,4).ne.0) go to 30
   k=k+1
   ifax(k)=4
   nn=nn/4
   if (nn.eq.1) go to 80
   go to 20
!     test for extra factor of 2
30 if (mod(nn,2).ne.0) go to 40
   k=k+1
   ifax(k)=2
   nn=nn/2
   if (nn.eq.1) go to 80
!     test for factors of 3
40 if (mod(nn,3).ne.0) go to 50
   k=k+1
   ifax(k)=3
   nn=nn/3
   if (nn.eq.1) go to 80
   go to 40
!     now find remaining factors
50 l=5
   inc=2
!     inc alternately takes on values 2 and 4
60 if (mod(nn,l).ne.0) go to 70
   k=k+1
   ifax(k)=l
   nn=nn/l
   if (nn.eq.1) go to 80
   go to 60
70 l=l+inc
   inc=6-inc
   go to 60
80 ifax(1)=k-1
!     ifax(1) contains number of factors
!     ifax(1) contains number of factors
   nfax=ifax(1)
!     sort factors into ascending order
   if (nfax.eq.1) go to 110
   do ii=2,nfax
      istop=nfax+2-ii
      do 90 i=2,istop
         if (ifax(i+1).ge.ifax(i)) go to 90
         item=ifax(i)
         ifax(i)=ifax(i+1)
         ifax(i+1)=item
90 continue
   enddo 
110 continue
   return
   end
   subroutine fftrig(trigs,n,mode)
!cafa  save
   real  ::  trigs(1)
!
   pi=2.0D0*asin(1.0)
   imode=iabs(mode)
   nn=n
   if (imode.gt.1.and.imode.lt.6) nn=n/2
   del=(pi+pi)/float(nn)
   l=nn+nn
   do i=1,l,2
      angle=0.5D0*float(i-1)*del
      trigs(i)=cos(angle)
      trigs(i+1)=sin(angle)
   enddo 
   if (imode.eq.1) return
   if (imode.eq.8) return
   del=0.5D0*del
   nh=(nn+1)/2
   l=nh+nh
   la=nn+nn
   do i=1,l,2
      angle=0.5D0*float(i-1)*del
      trigs(la+i)=cos(angle)
      trigs(la+i+1)=sin(angle)
   enddo 
   if (imode.le.3) return
   del=0.5D0*del
   la=la+nn
   if (mode.eq.5) go to 40
   do i=2,nn
      angle=float(i-1)*del
      trigs(la+i)=2.0D0*sin(angle)
   enddo 
   return
40 continue
   del=0.5D0*del
   do i=2,n
      angle=float(i-1)*del
      trigs(la+i)=sin(angle)
   enddo
   return
   end
   subroutine vpassmf(a,b,c,d,trigs,inc1,inc2,inc3,inc4,lot,n,ifac,la)
!cafa  save
   real  ::  a(n),b(n),c(n),d(n),trigs(n)
!
   common/comvpa/ sin36,cos36,sin72,cos72,sin60
!
   sin36=0.587785252292473
   cos36=0.809016994374947
   sin72=0.951056516295154
   cos72=0.309016994374947
   sin60=0.866025403784437
!
   m=n/ifac
   iink=m*inc1
   jink=la*inc2
   jump=(ifac-1)*jink
   ibase=0
   jbase=0
   igo=ifac-1
   if (igo.gt.4) return
   go to (10,50,90,130),igo
!
!     coding for factor 2
!
10 ia=1
   ja=1
   ib=ia+iink
   jb=ja+jink
   do l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do ijk=1,lot
         c(ja+j)=a(ia+i)+a(ib+i)
         d(ja+j)=b(ia+i)+b(ib+i)
         c(jb+j)=a(ia+i)-a(ib+i)
         d(jb+j)=b(ia+i)-b(ib+i)
         i=i+inc3
         j=j+inc4
      enddo 
      ibase=ibase+inc1
      jbase=jbase+inc2
   enddo
   if (la.eq.m) return
   la1=la+1
   jbase=jbase+jump
   do k=la1,m,la
      kb=k+k-2
      c1=trigs(kb+1)
      s1=trigs(kb+2)
         do l=1,la
            i=ibase
            j=jbase
!dir$ ivdep
            do ijk=1,lot
               c(ja+j)=a(ia+i)+a(ib+i)
               d(ja+j)=b(ia+i)+b(ib+i)
               c(jb+j)=c1*(a(ia+i)-a(ib+i))-s1*(b(ia+i)-b(ib+i))
               d(jb+j)=s1*(a(ia+i)-a(ib+i))+c1*(b(ia+i)-b(ib+i))
               i=i+inc3
               j=j+inc4
            enddo
         ibase=ibase+inc1
         jbase=jbase+inc2
      enddo 
      jbase=jbase+jump
   enddo
   return
!
!     coding for factor 3
!
50 ia=1
   ja=1
   ib=ia+iink
   jb=ja+jink
   ic=ib+iink
   jc=jb+jink
   do l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do ijk=1,lot
         c(ja+j)=a(ia+i)+(a(ib+i)+a(ic+i))
         d(ja+j)=b(ia+i)+(b(ib+i)+b(ic+i))
         c(jb+j)=(a(ia+i)-0.5e0*(a(ib+i)+a(ic+i)))-(sin60*(b(ib+i)-b(ic+i)))
         c(jc+j)=(a(ia+i)-0.5e0*(a(ib+i)+a(ic+i)))+(sin60*(b(ib+i)-b(ic+i)))
         d(jb+j)=(b(ia+i)-0.5e0*(b(ib+i)+b(ic+i)))+(sin60*(a(ib+i)-a(ic+i)))
         d(jc+j)=(b(ia+i)-0.5e0*(b(ib+i)+b(ic+i)))-(sin60*(a(ib+i)-a(ic+i)))
         i=i+inc3
         j=j+inc4
      enddo 
      ibase=ibase+inc1
      jbase=jbase+inc2
   enddo 
   if (la.eq.m) return
   la1=la+1
   jbase=jbase+jump
   do k=la1,m,la
      kb=k+k-2
      kc=kb+kb
      c1=trigs(kb+1)
      s1=trigs(kb+2)
      c2=trigs(kc+1)
      s2=trigs(kc+2)
      do l=1,la
         i=ibase
         j=jbase
!dir$ ivdep
         do ijk=1,lot
            c(ja+j)=a(ia+i)+(a(ib+i)+a(ic+i))
            d(ja+j)=b(ia+i)+(b(ib+i)+b(ic+i))
            c(jb+j)=c1*((a(ia+i)-0.5e0*(a(ib+i)+a(ic+i)))                      &
                    -(sin60*(b(ib+i)-b(ic+i))))                                &
                    -s1*((b(ia+i)-0.5e0*(b(ib+i)+b(ic+i)))                     &
                    +(sin60*(a(ib+i)-a(ic+i))))
            d(jb+j)=s1*((a(ia+i)-0.5e0*(a(ib+i)+a(ic+i)))                      &
                    -(sin60*(b(ib+i)-b(ic+i))))                                &
                    +c1*((b(ia+i)-0.5e0*(b(ib+i)+b(ic+i)))                     &
                    +(sin60*(a(ib+i)-a(ic+i))))
            c(jc+j)=c2*((a(ia+i)-0.5e0*(a(ib+i)+a(ic+i)))                      &
                    +(sin60*(b(ib+i)-b(ic+i))))                                &
                    -s2*((b(ia+i)-0.5e0*(b(ib+i)+b(ic+i)))                     &
                    -(sin60*(a(ib+i)-a(ic+i))))
            d(jc+j)=s2*((a(ia+i)-0.5e0*(a(ib+i)+a(ic+i)))                      &
                    +(sin60*(b(ib+i)-b(ic+i))))                                &
                    +c2*((b(ia+i)-0.5e0*(b(ib+i)+b(ic+i)))                     &
                    -(sin60*(a(ib+i)-a(ic+i))))
            i=i+inc3
            j=j+inc4
         enddo 
         ibase=ibase+inc1
         jbase=jbase+inc2
      enddo 
      jbase=jbase+jump
   enddo 
   return
!
!     coding for factor 4
!
90 ia=1
   ja=1
   ib=ia+iink
   jb=ja+jink
   ic=ib+iink
   jc=jb+jink
   id=ic+iink
   jd=jc+jink
   do l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do ijk=1,lot
         c(ja+j)=(a(ia+i)+a(ic+i))+(a(ib+i)+a(id+i))
         c(jc+j)=(a(ia+i)+a(ic+i))-(a(ib+i)+a(id+i))
         d(ja+j)=(b(ia+i)+b(ic+i))+(b(ib+i)+b(id+i))
         d(jc+j)=(b(ia+i)+b(ic+i))-(b(ib+i)+b(id+i))
         c(jb+j)=(a(ia+i)-a(ic+i))-(b(ib+i)-b(id+i))
         c(jd+j)=(a(ia+i)-a(ic+i))+(b(ib+i)-b(id+i))
         d(jb+j)=(b(ia+i)-b(ic+i))+(a(ib+i)-a(id+i))
         d(jd+j)=(b(ia+i)-b(ic+i))-(a(ib+i)-a(id+i))
         i=i+inc3
         j=j+inc4
      enddo 
      ibase=ibase+inc1
      jbase=jbase+inc2
   enddo 
   if (la.eq.m) return
   la1=la+1
   jbase=jbase+jump
   do k=la1,m,la
      kb=k+k-2
      kc=kb+kb
      kd=kc+kb
      c1=trigs(kb+1)
      s1=trigs(kb+2)
      c2=trigs(kc+1)
      s2=trigs(kc+2)
      c3=trigs(kd+1)
      s3=trigs(kd+2)
      do l=1,la
         i=ibase
         j=jbase
!dir$ ivdep
         do ijk=1,lot
            c(ja+j)=(a(ia+i)+a(ic+i))+(a(ib+i)+a(id+i))
            d(ja+j)=(b(ia+i)+b(ic+i))+(b(ib+i)+b(id+i))
            c(jc+j)=c2*((a(ia+i)+a(ic+i))-(a(ib+i)+a(id+i)))                   &
                   -s2*((b(ia+i)+b(ic+i))-(b(ib+i)+b(id+i)))
            d(jc+j)=s2*((a(ia+i)+a(ic+i))-(a(ib+i)+a(id+i)))                   &
                    +c2*((b(ia+i)+b(ic+i))-(b(ib+i)+b(id+i)))
            c(jb+j)=c1*((a(ia+i)-a(ic+i))-(b(ib+i)-b(id+i)))                   &
                    -s1*((b(ia+i)-b(ic+i))+(a(ib+i)-a(id+i)))
            d(jb+j)=s1*((a(ia+i)-a(ic+i))-(b(ib+i)-b(id+i)))                   &
                    +c1*((b(ia+i)-b(ic+i))+(a(ib+i)-a(id+i)))
            c(jd+j)=c3*((a(ia+i)-a(ic+i))+(b(ib+i)-b(id+i)))                   &
                    -s3*((b(ia+i)-b(ic+i))-(a(ib+i)-a(id+i)))
            d(jd+j)=s3*((a(ia+i)-a(ic+i))+(b(ib+i)-b(id+i)))                   &
                    +c3*((b(ia+i)-b(ic+i))-(a(ib+i)-a(id+i)))
            i=i+inc3
            j=j+inc4
         enddo 
         ibase=ibase+inc1
         jbase=jbase+inc2
      enddo 
      jbase=jbase+jump
   enddo 
   return
!
!     coding for factor 5
!
130 ia=1
   ja=1
   ib=ia+iink
   jb=ja+jink
   ic=ib+iink
   jc=jb+jink
   id=ic+iink
   jd=jc+jink
   ie=id+iink
   je=jd+jink
   do l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do ijk=1,lot
         c(ja+j)=a(ia+i)+(a(ib+i)+a(ie+i))+(a(ic+i)+a(id+i))
         d(ja+j)=b(ia+i)+(b(ib+i)+b(ie+i))+(b(ic+i)+b(id+i))
         c(jb+j)=(a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i)))     &
                 -(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i)))
         c(je+j)=(a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i)))     &
                 +(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i)))
         d(jb+j)=(b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i)))     &
                 +(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i)))
         d(je+j)=(b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i)))     &
                 -(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i)))
         c(jc+j)=(a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i)))     &
                 -(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i)))
         c(jd+j)=(a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i)))     &
                 +(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i)))
         d(jc+j)=(b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i)))     &
                 +(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i)))
         d(jd+j)=(b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i)))     &
                 -(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i)))
         i=i+inc3
         j=j+inc4
      enddo 
      ibase=ibase+inc1
      jbase=jbase+inc2
   enddo
   if (la.eq.m) return
   la1=la+1
   jbase=jbase+jump
   do k=la1,m,la
      kb=k+k-2
      kc=kb+kb
      kd=kc+kb
      ke=kd+kb
      c1=trigs(kb+1)
      s1=trigs(kb+2)
      c2=trigs(kc+1)
      s2=trigs(kc+2)
      c3=trigs(kd+1)
      s3=trigs(kd+2)
      c4=trigs(ke+1)
      s4=trigs(ke+2)
      do l=1,la
         i=ibase
         j=jbase
!dir$ ivdep
         do ijk=1,lot
            c(ja+j)=a(ia+i)+(a(ib+i)+a(ie+i))+(a(ic+i)+a(id+i))
            d(ja+j)=b(ia+i)+(b(ib+i)+b(ie+i))+(b(ic+i)+b(id+i))
            c(jb+j)=                                                           &
                 c1*((a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i))) &
                 -(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i))))           &
                 -s1*((b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i)))&
                 +(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i))))
            d(jb+j)=                                                           &
                 s1*((a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i))) &
                 -(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i))))           &
                 +c1*((b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i)))&
                 +(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i))))
            c(je+j)=                                                           &
                 c4*((a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i))) &
                 +(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i))))           &
                 -s4*((b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i)))&
                 -(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i))))
            d(je+j)=                                                           &
                 s4*((a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i))) &
                 +(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i))))           &
                 +c4*((b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i)))&
                 -(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i))))
            c(jc+j)=                                                           &
                 c2*((a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i))) &
                 -(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i))))           &
                 -s2*((b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i)))&
                 +(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i))))
            d(jc+j)=                                                           &
                 s2*((a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i))) &
                 -(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i))))           &
                 +c2*((b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i)))&
                 +(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i))))
            c(jd+j)=                                                           &
                 c3*((a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i))) &
                 +(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i))))           &
                 -s3*((b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i)))&
                 -(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i))))
            d(jd+j)=                                                           &
                 s3*((a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i))) &
                 +(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i))))           &
                 +c3*((b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i)))&
                 -(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i))))
            i=i+inc3
            j=j+inc4
         enddo 
         ibase=ibase+inc1
         jbase=jbase+inc2
      enddo 
      jbase=jbase+jump
   enddo 
   return
   end
   subroutine fft99m(a,work,trigs,ifax,inc,jump,n,lot,isign)
!cafa  save
   real     ::  a(n),work(n),trigs(n)
   integer  ::  ifax(*)
!
   nfax=ifax(1)
   nx=n
   nh=n/2
   ink=inc+inc
   if (isign.eq.+1) go to 30
!
!     if necessary, transfer data to work area
   igo=50
   if (mod(nfax,2).eq.1) goto 40
   ibase=1
   jbase=1
   do l=1,lot
      i=ibase
      j=jbase
!dir$ ivdep
      do m=1,n
         work(j)=a(i)
         i=i+inc
         j=j+1
      enddo
      ibase=ibase+jump
      jbase=jbase+nx
   enddo
!
   igo=60
   go to 40
!
!     preprocessing (isign=+1)
!     ------------------------
!
30 continue
   call fft99af(a,work,trigs,inc,jump,n,lot)
   igo=60
!
!     complex transform
!     -----------------
!
40 continue
   ia=1
   la=1
   do k=1,nfax
      if (igo.eq.60) go to 60
50 continue
      call vpassmf(a(ia),a(ia+inc),work(1),work(2),trigs,                      &
           ink,2,jump,nx,lot,nh,ifax(k+1),la)
      igo=60
      go to 70
60 continue
      call vpassmf(work(1),work(2),a(ia),a(ia+inc),trigs,                      &
           2,ink,nx,jump,lot,nh,ifax(k+1),la)
      igo=50
70 continue
      la=la*ifax(k+1)
   enddo 
!
   if (isign.eq.-1) go to 130
!
!     if necessary, transfer data from work area
   if (mod(nfax,2).eq.1) go to 110
   ibase=1
   jbase=1
   do l=1,lot
      i=ibase
      j=jbase
!dir$ ivdep
      do m=1,n
         a(j)=work(i)
         i=i+1
         j=j+inc
      enddo 
      ibase=ibase+nx
      jbase=jbase+jump
   enddo
!
!     fill in zeros at end
110 continue
   go to 140
!
!     postprocessing (isign=-1):
!     --------------------------
!
130 continue
   call fft99bf(work,a,trigs,inc,jump,n,lot)
!
140 continue
   return
   end
   subroutine fft99af(a,work,trigs,inc,jump,n,lot)
!cafa  save
!     subroutine fft99af - preprocessing step for fft99, isign=+1
!     (spectral to gridpoint transform)
!
   real  ::  a(n),work(n),trigs(n)
!
   nh=n/2
   nx=n
   ink=inc+inc
!
!     a(0)   a(n/2)
   ia=1
   ib=n*inc+1
   ja=1
   jb=2
!dir$ ivdep
   do l=1,lot
      work(ja)=a(ia)
      work(jb)=a(ia)
      ia=ia+jump
      ib=ib+jump
      ja=ja+nx
      jb=jb+nx
   enddo 
!
!     remaining wavenumbers
   iabase=2*inc+1
   ibbase=(n-2)*inc+1
   jabase=3
   jbbase=n-1
!
   do k=3,nh,2
      ia=iabase
      ib=ibbase
      ja=jabase
      jb=jbbase
      c=trigs(n+k)
      s=trigs(n+k+1)
!dir$ ivdep
      do l=1,lot
         work(ja)=(a(ia)+a(ib))-                                               &
                  (s*(a(ia)-a(ib))+c*(a(ia+inc)+a(ib+inc)))
         work(jb)=(a(ia)+a(ib))+                                               &
                  (s*(a(ia)-a(ib))+c*(a(ia+inc)+a(ib+inc)))
         work(ja+1)=(c*(a(ia)-a(ib))-s*(a(ia+inc)+a(ib+inc)))+                 &
                    (a(ia+inc)-a(ib+inc))
         work(jb+1)=(c*(a(ia)-a(ib))-s*(a(ia+inc)+a(ib+inc)))-                 &
                    (a(ia+inc)-a(ib+inc))
         ia=ia+jump
         ib=ib+jump
         ja=ja+nx
         jb=jb+nx
      enddo 
      iabase=iabase+ink
      ibbase=ibbase-ink
      jabase=jabase+2
      jbbase=jbbase-2
   enddo 
!
   if (iabase.ne.ibbase) go to 50
!     wavenumber n/4 (if it exists)
   ia=iabase
   ja=jabase
!dir$ ivdep
   do l=1,lot
      work(ja)=2.0e0*a(ia)
      work(ja+1)=-2.0e0*a(ia+inc)
      ia=ia+jump
      ja=ja+nx
   enddo 
!
   50 continue
   return
   end
   subroutine fft99bf(work,a,trigs,inc,jump,n,lot)
!cafa  save
!     subroutine fft99bf - postprocessing step for fft99, isign=-1
!     (gridpoint to spectral transform)
!
   real  ::  work(n),a(n),trigs(n)
!
   nh=n/2
   nx=n
   ink=inc+inc
!
!     a(0)   a(n/2)
   scale=1.0e0/float(n)
   ia=1
   ib=2
   ja=1
   jb=n*inc+1
!dir$ ivdep
   do l=1,lot
      a(ja)=scale*(work(ia)+work(ib))
      a(ja+inc)=0.0e0
      ia=ia+nx
      ib=ib+nx
      ja=ja+jump
      jb=jb+jump
   enddo 
!
!     remaining wavenumbers
   scale=0.5e0*scale
   iabase=3
   ibbase=n-1
   jabase=2*inc+1
   jbbase=(n-2)*inc+1
!
   do k=3,nh,2
      ia=iabase
      ib=ibbase
      ja=jabase
      jb=jbbase
      c=trigs(n+k)
      s=trigs(n+k+1)
!dir$ ivdep
      do l=1,lot
         a(ja)=scale*((work(ia)+work(ib))                                      &
               +(c*(work(ia+1)+work(ib+1))+s*(work(ia)-work(ib))))
         a(jb)=scale*((work(ia)+work(ib))                                      &
               -(c*(work(ia+1)+work(ib+1))+s*(work(ia)-work(ib))))
         a(ja+inc)=scale*((c*(work(ia)-work(ib))-s*(work(ia+1)+work(ib+1)))    &
                   +(work(ib+1)-work(ia+1)))
         a(jb+inc)=scale*((c*(work(ia)-work(ib))-s*(work(ia+1)+work(ib+1)))    &
                   -(work(ib+1)-work(ia+1)))
         ia=ia+nx
         ib=ib+nx
         ja=ja+jump
         jb=jb+jump
      enddo 
      iabase=iabase+2
      ibbase=ibbase-2
      jabase=jabase+ink
      jbbase=jbbase-ink
   enddo 
!
   if (iabase.ne.ibbase) go to 50
!     wavenumber n/4 (if it exists)
   ia=iabase
   ja=jabase
   scale=2.0e0*scale
!dir$ ivdep
   do l=1,lot
      a(ja)=scale*work(ia)
      a(ja+inc)=-scale*work(ia+1)
      ia=ia+nx
      ja=ja+jump
   enddo 
!
   50 continue
   return
   end
