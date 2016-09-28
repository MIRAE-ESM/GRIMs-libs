   SUBROUTINE W3FI83 (DATA,NPTS,FVAL1,FDIFF1,ISCAL2,                           &
                                   ISC10,KPDS,KGDS)
!$$$  SUBPROGRAM DOCUMENTATION  BLOCK
!                .      .    .                                       .
! SUBPROGRAM:  W3FI83        RESTORE DELTA PACKED DATA TO ORIGINAL
!   PRGMMR: CAVANAUGH        ORG: NMC421      DATE:93-08-18
!
! ABSTRACT: RESTORE DELTA PACKED DATA TO ORIGINAL VALUES
!           RESTORE FROM BOUSTREPHEDONIC ALIGNMENT
!
! PROGRAM HISTORY LOG:
!   93-07-14  CAVANAUGH
!   93-07-22  STACKPOLE      ADDITIONS TO FIX SCALING
!   94-01-27  CAVANAUGH   ADDED REVERSAL OF EVEN NUMBERED ROWS
!                         (BOUSTROPHEDONIC PROCESSING) TO RESTORE
!                         DATA TO ORIGINAL SEQUENCE.
!   94-03-02  CAVANAUGH   CORRECTED REVERSAL OF EVEN NUMBERED ROWS
!
! USAGE:    CALL W3FI83(DATA,NPTS,FVAL1,FDIFF1,ISCAL2,
!    *                                ISC10,KPDS,KGDS)
!   INPUT ARGUMENT LIST:
!     DATA     - SECOND ORDER DIFFERENCES
!     NPTS     - NUMBER OF POINTS IN ARRAY
!     FVAL1    - ORIGINAL FIRST ENTRY IN ARRAY
!     FDIFF1   - ORIGINAL FIRST FIRST-DIFFERENCE
!     ISCAL2   - POWER-OF-TWO EXPONENT FOR UNSCALING
!     ISC10    - POWER-OF-TEN EXPONENT FOR UNSCALING
!     KPDS     - ARRAY OF INFORMATION FOR PDS
!     KGDS     - ARRAY OF INFORMATION FOR GDS
!
!   OUTPUT ARGUMENT LIST:
!     DATA     - EXPANDED ORIGINAL DATA VALUES
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: SiliconGraphics 3.5 FORTRAN 77
!   MACHINE:  SiliconGraphics IRIS-4D/25, 25, INDIGO, indy
!
!$$$
!
   REAL       ::   FVAL1,FDIFF1
   REAL       ::   DATA(*),BOUST(200)
   INTEGER    ::   NPTS,NROW,NCOL,KPDS(*),KGDS(*),ISC10
!  ---------------------------------------
   SAVE
   !
   ! REMOVE DECIMAL UN-SCALING INTRODUCED DURING UNPACKING
   !
   DSCAL = 10.0 ** ISC10
   IF (DSCAL.EQ.0.0) THEN
       DO 50 I=1,NPTS
           DATA(I) = 1.0
   50     CONTINUE
   ELSE IF (DSCAL.EQ.1.0) THEN
   ELSE
       DO 51 I=1,NPTS
           DATA(I) = DATA(I) * DSCAL
   51     CONTINUE
   END IF

   DATA(1)  = FVAL1
   DATA(2)  = FDIFF1
   DO 200 J = 3,2,-1
       DO 100 K = J, NPTS
           DATA(K)  = DATA(K) + DATA(K-1)
  100     CONTINUE
  200 CONTINUE
   !
   !     NOW REMOVE THE BINARY SCALING FROM THE RECONSTRUCTED FIELD
   !     AND THE DECIMAL SCALING TOO
   !
   IF (DSCAL.EQ.0) THEN
       SCALE  = 0.0
   ELSE
       SCALE =(2.0**ISCAL2)/DSCAL
   END IF
   DO 300 I=1,NPTS
     DATA(I) = DATA(I) * SCALE
  300 CONTINUE
!  ==========================================================
   IF (IAND(KPDS(4),128).NE.0) THEN
       NROW  = KGDS(3)
       NCOL  = KGDS(2)
      !
      !      DATA LAID OUT BOUSTROPHEDONIC STYLE
      !
      !  
!         PRINT*, '  REVERSE BOUSTROPHEDON'
       DO 210 I = 2, NROW, 2
           !
           ! REVERSE THE EVEN NUMBERED ROWS
           !
           DO 201 J = 1, NCOL
               NPOS  = I * NCOL - J + 1
               BOUST(J) = DATA(NPOS)
  201         CONTINUE
           DO 202 J = 1, NCOL
               NPOS  = NCOL * (I-1) + J
               DATA(NPOS)  = BOUST(J)
  202         CONTINUE
  210     CONTINUE


   END IF
!  =================================================================
   RETURN
   END
