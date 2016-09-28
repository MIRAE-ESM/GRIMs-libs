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
* SUBROUTINE SBYTES (PACKED,UNPACKED,NOFF,NBITS,ISKIP,ITER)
*
* PURPOSE                GIVEN AN ARRAY OF RIGHT-JUSTIFIED BYTES,
*                        TO PACK THE SERIES OF BYTES INTO A TARGET
*                        WORD OR ARRAY. IN THE TARGET AREA, BITS
*                        SURROUNDING THE PACKED BYTES ARE UNCHANGED.
*
* USAGE                  CALL SBYTES (PACKED,UNPACKED,NOFF,NBITS,
*                                     ISKIP,ITER)
*
* ARGUMENTS
* ON INPUT               PACKED
*                          THE WORD OR ARRAY WHICH WILL CONTAIN THE
*                          PACKED INFORMATION.
*
*                        UNPACKED
*                          THE ARRAY OF RIGHT-JUSTIFIED BYTES.
*
*                        NOFF
*                          THE INITIAL NUMBER OF BITS TO SKIP, LEFT
*                          TO RIGHT, IN 'PACKED' IN ORDER TO LOCATE
*                          WHERE THE FIRST BYTE IS TO BE PACKED.
*
*                        NBITS
*                          THE NUMBER OF BITS IN EACH BYTE TO BE PACKED.
*                          MAXIMUM OF 64 BITS.
*
*                        ISKIP
*                          THE NUMBER OF BITS TO REMAIN UNCHANGED
*                          BETWEEN EACH BYTE PACKED INTO 'PACKED'.
*
*                        ITER
*                          THE NUMBER OF BYTES TO PACKED.
*
* ON OUTPUT              PACKED
*                          CONTAINS THE REQUESTED PACKED BYTES.
************************************************************************
         IDENT     SBYTES
ARY         =           1                  ARRAY POINTER
DST         =           2                  DESTINATION POINTER
POS         =           3                  BIT POSITION POINTER
SIZ         =           4                  SIZE POINTER
OFST        =           5                  OFSET VALUE
SHFT        =           6                  SHIFT VALUE
SKP         =           5                  SKIP ARGUMENT POSITION
RPT         =           6                  REPEAT ARGUMENT POSITION
* H1         CON           A'CRAYLIB'               Q8QST4 ARGUMENT 1
* H2         CON           A'BYTES'               Q8QST4 ARGUMENT 2
* H3         CON           A'SBYTES'               Q8QST4 ARGUMENT 3
* H4         CON           A'VERSION ',A'01'   Q8QST4 ARGUMENT 4
TPOS     DEFT
TSKP     DEFT
TRPT     DEFT
SBYTES         ENTER           NP=6,NT=3
*         CALL           Q8QST4,(H1,H2,H3,H4)
         ARGADD    A.SIZ,SIZ
         ARGADD    A.SKP,SKP
         ARGADD    A.POS,POS
         ARGADD    A.RPT,RPT
         ARGADD    A.ARY,ARY
         ARGADD    A.DST,DST
         A.SIZ        0,A.SIZ         FETCH SIZE OF BYTE
         S3           0,A.SKP         FETCH NUMBER OF BITS TO SKIP
         S1           0,A.POS         FETCH INITIAL POSITION
         S4           0,A.RPT         FETCH NUMBER OF TIMES TO REPEAT
         S5           A.SIZ           ISOLATE SIZE FOR SKIP CALCULATION
         S3           S3+S5           CALCULATE ACTUAL SKIP SIZE
         S0           S4              TRANSFER COUNT TO S0 FOR TESTI    NG
         T.TSKP       S3              STORE SKIP SIZE
         T.TPOS       S1              STORE INITIAL POSITION
         T.TRPT       S4              STORE REPETITION COUNT
         JSZ          SBRET           DON'T DO ANYTHING ON A ZERO COUNT
SB100    S4           <6              MAKE MASK FOR SHIFT COUNT
         S1           T.TPOS          FETCH THE POSITION VALUE
         S2           0,A.DST         FETCH THE SOURCE BYTE RIGHT JUSTI FIED
         S3           S4&S1           ISOLATE THE SHIFT COUNT
         S1           S1>6            DIVIDE THE POSITION BY 64
         A.OFST       S1              SET UP THE OFFSET
         A.SHFT       S3              SET SHIFT COUNT
         A.OFST       A.ARY+A.OFST    POINT TO ARRAY ELEMENT
         A.POS        64              SET UP BASE FOR ADDRESS CAL       CULATION
         A7           A.POS-A.SIZ     SET UP SHIFT COUNT TO LEFT JU     ST BYTE
         S1           T.TRPT          FETCH REPEAT COUNT
         S3           T.TPOS          FETCH CURRENT POSITION
         S4           T.TSKP          FETCH SKIP COUNT
         S5           1
         S1           S1-S5           COUNT A REPITION
         S6           0,A.OFST        FETCH DESTINATION WORD
         S7           1,A.OFST        FETCH SECOND DESTINATION WORD
         S3           S3+S4           INCREMENT POSITION
         S0           S1              SET UP LOOP TEST
         S5           <64
         T.TPOS       S3              SAVE CURRENT POSITION
         T.TRPT       S1              SAVE REPITION COUNT
         S5           S5<A7           LEFT JUSTIFY MASK
         S2           S2<A7           LEFT JUSTIFY BYTE
         A7           A.POS-A.SHFT   SET UP SHIFT COUNT TO POSITION BYTE
         S3           0               CLEAR MASK BUFFER
         S4           0               CLEAR BYTE BUFFER
         S3           S3,S5<A7        POSITION MASK
         S4           S4,S2<A7        POSITION BYTE
         S6           S4!S6&S3        SLIP IN UPPER PORTION OF BYTE
         S5           S5<A7           POSITION MASK IF NECESSARY
         S2           S2<A7           POSITION BYTE IF NECESSARY
         S7           S2!S7&S5        SLIP IN LOWER PORTION OF BYTE
         0,A.OFST     S6              RESTORE THE 1ST AND 2ND WORD
         1,A.OFST     S7              WITH THE BYTE ADDED
         A.DST        A.DST+1         INCREMENT THE DESTINATION
         JSN          SB100           LOOP FOR ALL BYTES
SBRET         EXIT           NAME=SBYTES,NT=3
         END
