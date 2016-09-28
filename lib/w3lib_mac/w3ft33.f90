   SUBROUTINE W3FT33(AIN,OUT,NSFLAG)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    W3FT33      THICKEN THINNED WAFS GRIB GRID 37-44
!   PRGMMR: RALPH PETTERSON  ORG: W/NMCXX    DATE: 94-11-13
!
! ABSTRACT: SUBROUTINE THICKENS ONE THINNED WAFS GRIB GRID TO A
!   REAL ARRAY OF 5329 NUMBERS (73,73) 1.25 DEGREE GRID.
!
! PROGRAM HISTORY LOG:
!   94-??-??  RALPH PETERSON
!   94-11-07  R.E.JONES        ADD DOC BLOCK, CHANGE CALL TO 3
!                              PARAMETERS. REPLACE COS WITH TABLE
!                              LOOKUP.
!   95-06-02  RALPH PETERSON   CHANGES TO CORRECT MISS-POSITION
!                              BETWEEN + OR - 8.75 N/S.
!   95-06-03  R.E.JONES        CHANGES SO 8 ROWS WITH 73 VALUES
!                              ARE NOT THICKENED, 10% FASTER.  
!
! USAGE:    CALL W3FT33(AIN, OUT, NSFLAG)
!   INPUT ARGUMENT LIST:
!     AIN      - REAL 3447 WORD ARRAY WITH UNPACKED THINNED WAFS
!                GRIB TYPE 37-44.
!     NSFLAG   - INTEGER =  1  AIN IS WAFS GRIB GRID 37-40  N. HEMI.
!                        = -1  AIN IS WAFS GRIB GRID 41-44  S. HEMI.   
!
!   OUTPUT ARGUMENT LIST:  
!     OUT      - REAL (73,73) WORD ARRAY WITH THICKENED WAFS GRIB
!                GRID 37-44.
!
! REMARKS: THE POLE POINT FOR U AND V WIND COMPONENTS WILL HAVE ONLY
!   ONE POINT. IF YOU NEED THE POLE ROW CORRECTED SEE PAGE 9 SECTION
!   1 IN OFFICE NOTE 388. YOU NEED BOTH U AND V TO MAKE THE 
!   CORRECTION.
! 
! ATTRIBUTES:
!   LANGUAGE: SiliconGraphics 5.2 FORTRAN 77
!   MACHINE:  SiliconGraphics IRIS-4D/25, 35, INDIGO, Indy
!
!$$$
!         
   INTEGER, PARAMETER  ::  NX=73,NY=73
   INTEGER, PARAMETER  ::  NIN=3447
!
   REAL                ::  AIN(*)
   REAL                ::  OUT(NX,NY)
!
   INTEGER             ::  IPOINT(NX)
!
   SAVE
!
   DATA  IPOINT/                                                               &
    73, 73, 73, 73, 73, 73, 73, 73, 72, 72, 72, 71, 71, 71, 70,                &
    70, 69, 69, 68, 67, 67, 66, 65, 65, 64, 63, 62, 61, 60, 60,                &
    59, 58, 57, 56, 55, 54, 52, 51, 50, 49, 48, 47, 45, 44, 43,                &
    42, 40, 39, 38, 36, 35, 33, 32, 30, 29, 28, 26, 25, 23, 22,                &
    20, 19, 17, 16, 14, 12, 11,  9,  8,  6,  5,  3,  2/
!
   NXM   = NX - 1
   FNXM  = FLOAT(NXM)
   !
   !        TEST FOR GRIDS (37-40)
   !
   IF (NSFLAG.GT.0) THEN
      !
      !          DO NOT THICKEN 8 ROWS WITH 73 VALUES, MOVE DATA 
      !          TO OUT ARRAY. GRIDS (37-40) N. 
      !
      IS = 0
      DO J = 1,8
         DO I = 1,NX
            IS       = IS + 1
            OUT(I,J) = AIN(IS)
         END DO
      END DO
!
      IE = NX * 8
      DO J = 9,NY
         NPOINT   = IPOINT(J)
         IS       = IE + 1
         IE       = IS + NPOINT - 1
         DPTS     = (FLOAT(NPOINT)-1.) / FNXM
         PW       = 1.0
         PE       = PW + DPTS
         OUT(1,J) = AIN(IS)
         VALW     = AIN(IS)
         VALE     = AIN(IS+1)
         DVAL     = (VALE-VALW)
         DO I = 2,NXM
            WGHT     = PE -FLOAT(IFIX(PE))
            OUT(I,J) = VALW + WGHT * DVAL
            PW       = PE
            PE       = PE + DPTS
            IF (IFIX(PW).NE.IFIX(PE)) THEN
               IS   = IS + 1
               VALW = VALE
               VALE = AIN(IS+1)
               DVAL = (VALE - VALW)
            END IF
         END DO
         OUT(NX,J) = AIN(IE)
      END DO
!
   ELSE
      !
      !         DO NOT THICKEN 8 ROWS WITH 73 VALUES, MOVE DATA  
      !         TO OUT ARRAY. GRIDS (41-44) S.
      !
      IS = NIN - (8 * NX)
      DO J = 66,NY
         DO I = 1,NX
            IS       = IS + 1
            OUT(I,J) = AIN(IS)
         END DO
      END DO
!
      IE = 0
      DO J = 1,65
         NPOINT   = IPOINT(74-J)
         IS       = IE + 1
         IE       = IS + NPOINT - 1
         DPTS     = (FLOAT(NPOINT)-1.) / FNXM
         PW       = 1.0
         PE       = PW + DPTS
         OUT(1,J) = AIN(IS)
         VALW     = AIN(IS)
         VALE     = AIN(IS+1)
         DVAL     = (VALE-VALW)
         DO I = 2,NXM
            WGHT     = PE -FLOAT(IFIX(PE))
            OUT(I,J) = VALW + WGHT * DVAL
            PW       = PE
            PE       = PE + DPTS
            IF (IFIX(PW).NE.IFIX(PE)) THEN
               IS   = IS + 1
               VALW = VALE
               VALE = AIN(IS+1)
               DVAL = (VALE - VALW)
            END IF
         END DO
         OUT(NX,J) = AIN(IE)
      END DO
   END IF
!
   RETURN
   END
