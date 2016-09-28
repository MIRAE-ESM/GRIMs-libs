!
      program sigdif
!-------------------------------------------------------------------------------
!
!     average two sigma-level spectrum files
!     usage: sigdif inputsig1 inputsig2 outputsig
!
!-------------------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------------------
      integer                            ::  ncpus_,jcap_,levs_,ilonf_,&
                                             ilatg_,ijcap_,ilevs_,ko_,&
                                             npes_,ncol_
      integer                            ::  jcap,kdim
      integer                            ::  i,j,k,idum(4),idum2(4)
      integer                            ::  iargc
      real*8 ,allocatable, dimension(:)  ::  var,var2
      real*8                             ::  dummy1,dummy2(245)
      real*8                             ::  dummy3,dummy4(245)
      character(len=8)                   ::  head(4)
      character(len=8)                   ::  head2(4)
      character                          ::  infile1*80,infile2*80,outfile*80
      namelist/paralist/ ncpus_,jcap_,levs_,ilonf_,ilatg_,ijcap_,ilevs_,&
                         ko_,npes_,ncol_
!
      read(1,paralist)
      jcap=jcap_
      kdim=levs_
      allocate(var((jcap+1)*(jcap+2)))
      allocate(var2((jcap+1)*(jcap+2)))
!
      print *, iargc(),jcap,kdim
      if (iargc().ne.3) then
         write(6,*) 'Usage: sigdif input1 input2 output'
         stop
      endif
!
      call getarg(1,infile1)
      call getarg(2,infile2)
      call getarg(3,outfile)
!
      open(1,file=trim(infile1),form='unformatted')
      open(2,file=trim(infile2),form='unformatted')
      open(10,file=trim(outfile),form='unformatted')

      read(1) head
      read(1) dummy1,idum,dummy2
!
      read(2) head2
      read(2) dummy3,idum2,dummy4
!
!      write(6,'(i4,3e15.7)') 
!     $     (i,dummy2(i),dummy4(i),dummy2(i)-dummy4(i),i=1,245)
!
      write(10) head
      write(10) dummy3,idum2,dummy4
!
      do k=1,2
         read(1) var
         read(2) var2
!         write(6,*) k,k,var(1),var2(1)
         write(10) ((var(j)-var2(j)),j=1,(jcap+1)*(jcap+2))
!         write(10,rec=k) (real(var(j)),j=1,(jcap+1)*(jcap+2))
      enddo
!
      do k=1,kdim
         read(1) var
         read(2) var2
!         write(6,*) k+3,k+3,var(1),var2(1)
         write(10) ((var(j)-var2(j)),j=1,(jcap+1)*(jcap+2))
!         write(10,rec=k+2) (real(var(j)),j=1,(jcap+1)*(jcap+2))
      enddo
!
      do k=1,kdim*2
         read(1) var
         read(2) var2
!         write(6,*) k+2+kdim,2+kdim+mod(k+1+kdim,2)*kdim+int((k+1)/2),
!     $        var(1),var2(1)
         write(10) ((var(j)-var2(j)),j=1,(jcap+1)*(jcap+2))
!         write(10,rec=2+kdim+mod(k+1+kdim,2)*kdim+int((k+1)/2))
!     $        (real(var(j)),j=1,(jcap+1)*(jcap+2))
      enddo
!
      do k=1,kdim
         read(1) var
         read(2) var2
!         write(6,*) k+2+kdim*3,k+2+kdim*3,var(1),var2(1)
         write(10) ((var(j)-var2(j)),j=1,(jcap+1)*(jcap+2))
!         write(6,*) k+2+kdim*3,k+2+kdim*3,var(1)
!         write(10,rec=k+2+kdim*3) (real(var(j)),j=1,(jcap+1)*(jcap+2))
      enddo
!
      deallocate(var,var2)
      stop
      end program sigdif
