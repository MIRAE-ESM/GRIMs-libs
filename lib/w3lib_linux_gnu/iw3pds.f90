   LOGICAL FUNCTION IW3PDS(L1, L2, KEY)
!$$$   SUBPROGRAM  DOCUMENTATION  BLOCK
!
! FUNCT  IW3PDS              TEST FOR MATCH TWO CHARACTER ARRAYS
!   PRGMMR: REJONES          ORG: NMC421      DATE: 90-06-04
!
! ABSTACT: TEST TWO PDS (GRIB PRODUCT DEFINITION SECTION) TO SEE
!     IF ALL EQUAL; OTHERWISE .FALSE. IF KEY = 1, ALL 24 CHARACTERS
!     ARE TESTED, IF KEY = 0 , THE DATE (CHARACTERS 13-17) ARE NOT
!     TESTED. IF KEY = 2, TEST 11 OF 1ST 12 BYTES OF PDS.
!
! PROGRAM HISTORY LOG:
!   88-02-22  R.E.JONES
!   89-01-20  R.E.JONES  CONNVERT TO MICROSOFT FORTRAN 4.10
!   89-06-19  R.E.JONES  CHANGE IW3PDB TO IW3PDS
!   90-06-04  R E.JONES  CHANGE TO SUN FORTRAN 1.3
!   91-03-29  R.E.JONES  CONVERT TO SiliconGraphics FORTRAN
!   93-03-29  R.E.JONES  ADD SAVE STATEMENT
!   93-11-03  R.E.JONES  ADD KEY=2, TEST 1ST 12 BYTES OF PDS
!                        ADD CHANGES SO BYTE 4 OF PDS IS NOT
!                        TESTED.
!   94-04-04  R.E.JONES  ADD KEY=3, TEST BYTES 1-3, 7-12
!
! USAGE:  II = IW3PDS(L1,L2,KEY)
!
!   INPUT VARIABLES:
!     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
!     ------ --------- -----------------------------------------------
!     L1     ARG LIST  CHARACTER ARRAY TO MATCH WITH L2,
!                      L1 CAN ALSO BE A 6 WORD INTEGER*4 ARRAY
!     L2     ARG LIST  CHARACTER ARRAY TO MATCH WITH L1,
!                      L2 CAN ALSO BE A INTEGER*4 ARRAY
!     KEY    ARG LIST  1, MATCH 24 BYTES OF PDS
!                      0, DO NOT INCLUDE THE DATE (BYTES 13-17) IN
!                         MATCH.
!                      2, MATCH 11 OF 1ST 12 BYTES OF PDS, BYTE
!                         4 (TABLE VER. NO.) IS NOT TESTED.
!                      3, MATCH 1-3, 7-12 BYTES OF PDS
!
!   OUTPUT VARIABLES:
!     NAMES  INTERFACE DESCRIPTION OF VARIABLES AND TYPES
!     ------ --------- -----------------------------------------------
!     IW3PDB FUNCTION  LOGICAL .TRUE. IF L1 AND L2 MATCH ON ALL CHAR.,
!                      LOGICAL .FALSE. IF NOT MATCH ON ANY CHAR.
!
! EXAMPLE:  SEARCH IDTBL FOR MATCH WITH GIVEN (PDS), USE RBA IN 7TH
!           ID WORD TO READ RECORD BY RBA.
!
!           INTEGER*4 IDTBL(1794), IPDS(6), RBA
!           LOGICAL*4 IW3PDS
!
!           KEY = 0
!           DO 400 I = 9,1793,7
!             IF (IDTBL(I).EQ.0) GO TO 500
!               IF (IW3PDS(IPDS,IDTBL(I),KEY)) THEN
!                  RBA = IDTBL(I+6)
!                  GO TO 600
!               END IF
!   400     CONTINUE
!
!   500     CONTINUE
!           GO TO XXXX ... ERROR EXIT , CAN NOT FIND RECORD
!
!   600     ..  READ RECORD WITH RBA
!
! ATTRIBUTES:
!   LANGUAGE: SiliconGraphics 3.3. FORTRAN 77
!   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO
!
!$$$
!
   CHARACTER(len=1)  ::  L1(24)
   CHARACTER(len=1)  ::  L2(24)
!
   SAVE
!
   IW3PDS = .TRUE.
!
   IF (KEY.EQ.1) THEN
      DO I = 1,3
         IF (L1(I).NE.L2(I))  GO TO 70
      END DO 
!
      DO I = 5,24
         IF (L1(I).NE.L2(I))  GO TO 70
      END DO
!
   ELSE
!
      DO I = 1,3
         IF (L1(I).NE.L2(I))  GO TO 70
      END DO
      !
      ! DO NOT TEST BYTE 4, 5, 6 PDS VER. NO, COUNTRY,
      ! MODEL NUMBER. U.S. OR U.K. WAFS DATA WILL WORK. 
      !
      IF (KEY.EQ.3) THEN
         DO I = 7,12
            IF (L1(I).NE.L2(I))  GO TO 70
         END DO
         GO TO 60
       END IF
       !
       ! DO NOT TEST PDS VERSION NUMBER, IT MAY BE 1 OR 2
       !
       DO I = 5,12
          IF (L1(I).NE.L2(I))  GO TO 70
       END DO 
       
       IF (KEY.EQ.2) GO TO 60

       DO I = 18,24
          IF (L1(I).NE.L2(I))  GO TO 70
       END DO  
!
   ENDIF
!
   60     CONTINUE
   RETURN
!
   70     CONTINUE
   IW3PDS = .FALSE.
   RETURN
   END
