     SUBROUTINE AEA (IA, IE, NC )
!$$$    SUBPROGRAM DOCUMENTATION  BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    AEA         ASCII TO EBCDIC, OR EBCDIC TO ASCII
!   PRGMMR: DESMARAIS        ORG: W342       DATE: 82-11-29
!
! ABSTRACT: CONVERT ASCII TO EBCDIC, OR EBCDIC TO ASCII BY CHARACTER.
!
! PROGRAM HISTORY LOG:
!   82-11-29  DESMARAIS
!   88-03-31  R.E.JONES  CHANGE LOGIC SO IT WORKS LIKE A
!                        IBM370 TRANSLATE INSTRUCTION.
!   88-08-22  R.E.JONES  CHANGES FOR MICROSOFT FORTRAN 4.10
!   88-09-04  R.E.JONES  CHANGE TABLES TO 128 CHARACTER SET
!   89-08-16  R.E.JONES  CHANGE TO IBM370 VS FORTRAN
!   90-06-11  R.E.JONES  CHANGE TO SUN FORTRAN 1.3
!   91-03-28  R.E.JONES  CHANGE TO SiliconGraphics FORTRAN
!
! USAGE:    CALL AEA (IA, IE, NC)
!   INPUT ARGUMENT LIST:
!     IA       - CHARACTER*1 ARRAY OF ASCII DATA IF NC < 0
!     IE       - CHARACTER*1 ARRAY OF EBCDIC DATA IF NC > 0
!     NC       - INTEGER,  CONTAINS CHARACTER COUNT TO CONVERT....
!                IF NC .LT. 0,  CONVERT ASCII TO EBCDIC
!                IF NC .GT. 0,  CONVERT EBCDIC TO ASCII
!
!   OUTPUT ARGUMENT LIST:
!     IA       - CHARACTER*1 ARRAY OF ASCII DATA IF NC > 0
!     IE       - CHARACTER*1 ARRAY OF EBCDIC DATA IF NC < 0
!
!
! REMARKS: SOFTWARE VERSION OF IBM370 TRANSLATE INSTRUCTION, BY
!   CHANGING THE TWO TABLES WE COULD DO A  64, 96, 128  ASCII
!   CHARACTER SET, CHANGE LOWER CASE TO UPPER, ETC.
!
! ATTRIBUTES:
!   LANGUAGE: HP FORTRAN 77
!   MACHINE:  Hewlett-Packard 9000/705, 715, 735, 750, 755, 712
!
!$$$
!***   ASCII  CONTAINS ASCII CHARACTERS, AS PUNCHED ON IBM029
!
   CHARACTER(len=1)  ::  IA(*)
   CHARACTER(len=1)  ::  IE(*)
   CHARACTER(len=1)  ::  ASCII(0:255)
   CHARACTER(len=1)  ::  ASCII1(0:127)
   CHARACTER(len=1)  ::  ASCII2(0:127)
   CHARACTER(len=1)  ::  EBCDIC(0:255)
!
   EQUIVALENCE    (ASCII(0),ASCII1(0))
   EQUIVALENCE    (ASCII(128),ASCII2(0))
!
    DATA  ASCII1 /                                                             &
    Z'00',Z'01',Z'02',Z'03',Z'00',Z'09',Z'00',Z'7F',                           &
    Z'00',Z'00',Z'00',Z'0B',Z'0C',Z'0D',Z'0E',Z'0F',                           &
    Z'10',Z'11',Z'12',Z'00',Z'00',Z'00',Z'00',Z'00',                           &
    Z'18',Z'19',Z'00',Z'00',Z'00',Z'00',Z'00',Z'00',                           &
    Z'00',Z'00',Z'1C',Z'00',Z'0A',Z'00',Z'17',Z'00',                           &
    Z'00',Z'00',Z'00',Z'00',Z'00',Z'05',Z'06',Z'07',                           &
    Z'00',Z'00',Z'16',Z'00',Z'00',Z'1E',Z'00',Z'04',                           &
    Z'00',Z'00',Z'00',Z'00',Z'14',Z'15',Z'00',Z'1A',                           &
    Z'20',Z'00',Z'60',Z'00',Z'00',Z'00',Z'00',Z'00',                           &
    Z'00',Z'00',Z'60',Z'2E',Z'3C',Z'28',Z'2B',Z'00',                           &
    Z'26',Z'00',Z'00',Z'00',Z'00',Z'00',Z'00',Z'00',                           &
    Z'00',Z'00',Z'21',Z'24',Z'2A',Z'29',Z'3B',Z'5E',                           &
    Z'2D',Z'2F',Z'00',Z'00',Z'00',Z'00',Z'00',Z'00',                           &
    Z'00',Z'00',Z'7C',Z'2C',Z'25',Z'5F',Z'3E',Z'3F',                           &
    Z'00',Z'00',Z'00',Z'00',Z'00',Z'00',Z'00',Z'00',                           &
    Z'00',Z'60',Z'3A',Z'23',Z'40',Z'27',Z'3D',Z'22'/
    DATA  ASCII2 /                                                             &
    Z'20',Z'61',Z'62',Z'63',Z'64',Z'65',Z'66',Z'67',                           &
    Z'68',Z'69',Z'20',Z'20',Z'20',Z'20',Z'20',Z'20',                           &
    Z'20',Z'6A',Z'6B',Z'6C',Z'6D',Z'6E',Z'6F',Z'70',                           &
    Z'71',Z'72',Z'20',Z'20',Z'20',Z'20',Z'20',Z'20',                           &
    Z'20',Z'7E',Z'73',Z'74',Z'75',Z'76',Z'77',Z'78',                           &
    Z'79',Z'7A',Z'20',Z'20',Z'20',Z'5B',Z'20',Z'20',                           &
    Z'00',Z'00',Z'00',Z'00',Z'00',Z'00',Z'00',Z'00',                           &
    Z'00',Z'00',Z'00',Z'00',Z'00',Z'5D',Z'00',Z'00',                           &
    Z'7B',Z'41',Z'42',Z'43',Z'44',Z'45',Z'46',Z'47',                           &
    Z'48',Z'49',Z'20',Z'20',Z'20',Z'20',Z'20',Z'20',                           &
    Z'7D',Z'4A',Z'4B',Z'4C',Z'4D',Z'4E',Z'4F',Z'50',                           &
    Z'51',Z'52',Z'20',Z'20',Z'20',Z'20',Z'20',Z'20',                           &
    Z'5C',Z'20',Z'53',Z'54',Z'55',Z'56',Z'57',Z'58',                           &
    Z'59',Z'5A',Z'20',Z'20',Z'20',Z'20',Z'20',Z'20',                           &
    Z'30',Z'31',Z'32',Z'33',Z'34',Z'35',Z'36',Z'37',                           &
    Z'38',Z'39',Z'20',Z'20',Z'20',Z'20',Z'20',Z'20'/
!
!***  EBCDIC CONTAINS HEX. REPRESENTATION OF EBCDIC CHARACTERS
!
    DATA  EBCDIC /                                                             &
    Z'00',Z'01',Z'02',Z'03',Z'37',Z'2D',Z'2E',Z'2F',                           &
    Z'16',Z'05',Z'25',Z'0B',Z'0C',Z'0D',Z'0E',Z'0F',                           &
    Z'10',Z'11',Z'12',Z'00',Z'3C',Z'3D',Z'32',Z'26',                           &
    Z'18',Z'19',Z'3F',Z'27',Z'22',Z'00',Z'35',Z'00',                           &
    Z'40',Z'5A',Z'7F',Z'7B',Z'5B',Z'6C',Z'50',Z'7D',                           &
    Z'4D',Z'5D',Z'5C',Z'4E',Z'6B',Z'60',Z'4B',Z'61',                           &
    Z'F0',Z'F1',Z'F2',Z'F3',Z'F4',Z'F5',Z'F6',Z'F7',                           &
    Z'F8',Z'F9',Z'7A',Z'5E',Z'4C',Z'7E',Z'6E',Z'6F',                           &
    Z'7C',Z'C1',Z'C2',Z'C3',Z'C4',Z'C5',Z'C6',Z'C7',                           &
    Z'C8',Z'C9',Z'D1',Z'D2',Z'D3',Z'D4',Z'D5',Z'D6',                           &
    Z'D7',Z'D8',Z'D9',Z'E2',Z'E3',Z'E4',Z'E5',Z'E6',                           &
    Z'E7',Z'E8',Z'E9',Z'AD',Z'E0',Z'BD',Z'5F',Z'6D',                           &
    Z'79',Z'81',Z'82',Z'83',Z'84',Z'85',Z'86',Z'87',                           &
    Z'88',Z'89',Z'91',Z'92',Z'93',Z'94',Z'95',Z'96',                           &
    Z'97',Z'98',Z'99',Z'A2',Z'A3',Z'A4',Z'A5',Z'A6',                           &
    Z'A7',Z'A8',Z'A9',Z'C0',Z'6A',Z'D0',Z'A1',Z'07',                           &
    128*Z'40'/

   NUM = IABS(NC)

   IF (NC .EQ. 0)   RETURN

   IF (NC .GT. 0)   THEN
   
      !
      !***  CONVERT STRING ...  EBCDIC TO ASCII,   NUM CHARACTERS
      !
      DO J = 1, NUM
         IA(J) = ASCII(ICHAR(IE(J)))
      END DO 

   ELSE
      !
      !***  CONVERT STRING ...  ASCII TO EBCDIC,   NUM CHARACTERS
      !
      DO J = 1, NUM
         IE(J) = EBCDIC(ICHAR(IA(J)))
      END DO  
   END IF

   RETURN
   END
