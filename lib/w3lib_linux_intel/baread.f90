!-----------------------------------------------------------------------
   SUBROUTINE BAREAD(LU,IB,NB,KA,A)
          USE IFPOSIX                                         
          USE IFPORT        
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM: BAREAD         BYTE-ADDRESSABLE READ
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 94-04-01
!
! ABSTRACT: READ A GIVEN NUMBER OF BYTES FROM AN UNBLOCKED FILE,
!   SKIPPING A GIVEN NUMBER OF BYTES.
!
! PROGRAM HISTORY LOG:
!   94-04-01  IREDELL
!   95-10-16  IREDELL     WORKSTATION VERSION
!
! USAGE:    CALL BAREAD(LU,IB,NB,KA,A)
!   INPUT ARGUMENTS:
!     LU           INTEGER UNIT TO READ
!     IB           INTEGER NUMBER OF BYTES TO SKIP
!     NB           INTEGER NUMBER OF BYTES TO READ
!   OUTPUT ARGUMENTS:
!     KA           INTEGER NUMBER OF BYTES ACTUALLY READ
!     A            CHARACTER*1 (NB) BUFFER READ
!
! SUBPROGRAMS CALLED:
!   FSEEK        SET FILE POSITION
!   FGETC        READ BYTE FROM FILE
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 77
!   MACHINE: WORKSTATIONS
!
!$$$
   CHARACTER(len=1)  ::  A(NB)
   integer           ::  is,ierr,npos,ifd

   is = FSEEK(LU,IB,0)
   IF(is.EQ.0) THEN
      CALL PXFFILENO (lu, ifd, ierr)
      CALL PXFREAD (ifd, a, nb, ka, ierr)
      if (ierr .ne. 0) then
         print*,'PXFREAD error in baread ',ier
         call abort
      endif
   else
      print*,'FSEEK ',ib,' in baread error:',is
      call exit(5)
   ENDIF

   RETURN
   END
