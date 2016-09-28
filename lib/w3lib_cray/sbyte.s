* PACKAGE BYTES          DESCRIPTION OF INDIVIDUAL USER ENTRIES
*                        GBYTES, SBYTES, GBYTE, SBYTE FOLLOWS
*                        THIS PACKAGE DESCRIPTION.
*
* LATEST REVISION        JANUARY 1985
*
* PURPOSE                ON THE CRAY MACHINES, TO PACK/UNPACK BYTES
*                        (FIXED-LENGTH GROUPS OF BITS, IN THE RANGE
*                        OF 1 TO 64 BITS EACH) IN MEMORY.  BYTES
*                        MAY CROSS WORD BOUNDARIES.
*
* SPECIAL CONDITIONS     SEE 'TIMING' REMARKS BELOW.
*
* I/O                    NONE
*
* PRECISION              SINGLE
*
* REQUIRED LIBRARY       NONE
* FILES
*
* LANGUAGE               CRAY ASSEMBLY LANGUAGE
*
* HISTORY                THESE ROUTINES HAVE BEEN AVAILABLE ON NCAR'S
*                        BATCH MACHINES SINCE THE EARLY 1970'S.
*
*                        THE PRESENT VERSION OF GBYTES AND GBYTE WAS
*                        WRITTEN BY JAY W. CHALMERS OF NCAR'S HIGH
*                        ALTITUDE OBSERVATORY IN JUNE 1984.
*
*                        THE PRESENT VERSION OF SBYTES AND SBYTE WAS
*                        WRITTEN BY DAVE KITTS OF NCAR'S SCIENTIFIC
*                        COMPUTING DIVISION IN MAY 1982.
*
* PORTABILITY            SPECIFIC TO CRAY MACHINES.
*
* TIMING                 GBYTES IS FASTER THAN SBYTES.  THE TIMING
*                        FOR BOTH DEPENDS ON NBITS AND ITER.
*
*                        INITIAL TESTS INDICATE THAT THESE ROUTINES
*                        ARE THE FASTEST PACKERS/UNPACKERS ON THE
*                        CRAY MACHINES.  USERS SHOULD AVOID OVERLAP
*                        OF INPUT ARRAY WITH OUTPUT ARRAY, AS THIS
*                        CAN SLOW THE PERFORMANCE SIGNIFICANTLY.
************************************************************************
*
* SUBROUTINE SBYTE (PACKED,UNPACKED,NOFF,NBITS)
*
* PURPOSE                GIVEN A BYTE, RIGHT-JUSTIFIED IN A WORD, TO
*                        PACK THE BYTE INTO A TARGET WORD OR ARRAY.
*                        BITS SURROUNDING THE BYTE IN THE TARGET
*                        AREA ARE UNCHANGED.
*
* USAGE                  CALL SBYTE (PACKED,UNPACKED,NOFF,NBITS)
*
* ARGUMENTS
* ON INPUT               PACKED
*                          THE WORD OR ARRAY WHICH WILL CONTAIN THE
*                          PACKED BYTE.  BYTE MAY CROSS WORD BOUNDARIES.
*
*                        UNPACKED
*                          THE WORD CONTAINING THE RIGHT-JUSTIFIED BYTE
*                          TO BE PACKED.
*
*                        NOFF
*                          THE NUMBER OF BITS TO SKIP, LEFT TO RIGHT,
*                          IN 'PACKED' IN ORDER TO LOCATE WHERE THE
*                          BYTE IS TO BE PACKED.
*
*                        NBITS
*                          THE NUMBER OF BITS IN THE BYTE TO BE PACKED.
*                          MAXIMUM OF 64 BITS.
*
* ON OUTPUT              PACKED
*                          WORD OR CONSECUTIVE WORDS CONTAINING THE
*                          REQUESTED BYTE.
************************************************************************
         IDENT     SBYTE
ARY         =           1                  ARRAY POINTER
DST         =           2                  DESTINATION POINTER
POS         =           3                  BIT POSITION POINTER
SIZ         =           4                  SIZE POINTER
OFST        =           5                  OFSET VALUE
SHFT        =           6                  SHIFT VALUE
* H1         CON           A'CRAYLIB'          Q8QST4 ARGUMENT 1
* H2         CON           A'BYTES'            Q8QST4 ARGUMENT 2
* H3         CON           A'SBYTE'            Q8QST4 ARGUMENT 3
* H4         CON           A'VERSION ',A'01'   Q8QST4 ARGUMENT 4
SBYTE         ENTER           NP=4
*         CALL           Q8QST4,(H1,H2,H3,H4)
         ARGADD    A.POS,POS          FETCH LOCATION OF BYTE POSITION
         ARGADD    A.DST,DST          FETCH THE LOCATION SOURCE BYTE
         ARGADD    A.SIZ,SIZ          FETCH LOCATION OF BYTE SIZE
         ARGADD    A.ARY,ARY          FETCH LOCATION OF DESTINATION
         S1           0,A.POS         FETCH THE POSITION VALUE
         S2           0,A.DST         FETCH THE SOURCE BYTE RIGHT JUSTI FIED
         A.SIZ        0,A.SIZ         FETCH THE BYTE SIZE
         S4           <6              MAKE MASK FOR SHIFT COUNT
         S5           <64
         S3           S4&S1          ISOLATE THE SHIFT COUNT
         S1           S1>6           DIVIDE THE POSITION BY 64
         A.OFST       S1             SET UP THE OFFSET
         A.SHFT       S3             SET SHIFT COUNT
         A.ARY        A.ARY+A.OFST   POINT TO ARRAY ELEMENT
         S6           0,A.ARY        FETCH DESTINATION WORD
         S7           1,A.ARY        FETCH SECOND DESTINATION WORD
         A.OFST       64             SET UP BASE FOR ADDRESS CALCULATI  ON
         A7           A.OFST-A.SIZ   SET UP SHIFT COUNT TO LEFT JUST BYTE
         S5           S5<A7          LEFT JUSTIFY MASK
         S2           S2<A7          LEFT JUSTIFY BYTE
         A7           A.OFST-A.SHFT  SET UP SHIFT COUNT TO POSITION BYTE
         S3           0              CLEAR MASK BUFFER
         S4           0              CLEAR BYTE BUFFER
         S3           S3,S5<A7       POSITION MASK
         S4           S4,S2<A7       POSITION BYTE
         S6           S4!S6&S3       SLIP IN UPPER PORTION OF BYTE
         S5           S5<A7          POSITION MASK IF NECESSARY
         S2           S2<A7          POSITION BYTE IF NECESSARY
         S7           S2!S7&S5       SLIP IN LOWER PORTION OF BYTE
         0,A.ARY      S6             RESTORE THE FIRST AND SECOND WORD
         1,A.ARY      S7             WITH THE BYTE ADDED
         EXIT           NAME=SBYTE
         END
