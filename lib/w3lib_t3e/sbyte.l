1CAL Version 2 - 11.1.0.0 (09/17/96)     SBYTE                                                                      Page 1
 Date: 02/05/98    Time: 14:31:35                                                                                     (1)
                                         V---+----1----+----2----+----3----+----4----+----5----+----6----+----7-V  Cray T.Series IEEE
                                       1.* PACKAGE BYTES          DESCRIPTION OF INDIVIDUAL USER ENTRIES
                                       2.*                        GBYTES, SBYTES, GBYTE, SBYTE FOLLOWS
                                       3.*                        THIS PACKAGE DESCRIPTION.
                                       4.*
                                       5.* LATEST REVISION        JANUARY 1985
                                       6.*
                                       7.* PURPOSE                ON THE CRAY MACHINES, TO PACK/UNPACK BYTES
                                       8.*                        (FIXED-LENGTH GROUPS OF BITS, IN THE RANGE
                                       9.*                        OF 1 TO 64 BITS EACH) IN MEMORY.  BYTES
                                      10.*                        MAY CROSS WORD BOUNDARIES.
                                      11.*
                                      12.* SPECIAL CONDITIONS     SEE 'TIMING' REMARKS BELOW.
                                      13.*
                                      14.* I/O                    NONE
                                      15.*
                                      16.* PRECISION              SINGLE
                                      17.*
                                      18.* REQUIRED LIBRARY       NONE
                                      19.* FILES
                                      20.*
                                      21.* LANGUAGE               CRAY ASSEMBLY LANGUAGE
                                      22.*
                                      23.* HISTORY                THESE ROUTINES HAVE BEEN AVAILABLE ON NCAR'S
                                      24.*                        BATCH MACHINES SINCE THE EARLY 1970'S.
                                      25.*
                                      26.*                        THE PRESENT VERSION OF GBYTES AND GBYTE WAS
                                      27.*                        WRITTEN BY JAY W. CHALMERS OF NCAR'S HIGH
                                      28.*                        ALTITUDE OBSERVATORY IN JUNE 1984.
                                      29.*
                                      30.*                        THE PRESENT VERSION OF SBYTES AND SBYTE WAS
                                      31.*                        WRITTEN BY DAVE KITTS OF NCAR'S SCIENTIFIC
                                      32.*                        COMPUTING DIVISION IN MAY 1982.
                                      33.*
                                      34.* PORTABILITY            SPECIFIC TO CRAY MACHINES.
                                      35.*
                                      36.* TIMING                 GBYTES IS FASTER THAN SBYTES.  THE TIMING
                                      37.*                        FOR BOTH DEPENDS ON NBITS AND ITER.
                                      38.*
                                      39.*                        INITIAL TESTS INDICATE THAT THESE ROUTINES
                                      40.*                        ARE THE FASTEST PACKERS/UNPACKERS ON THE
                                      41.*                        CRAY MACHINES.  USERS SHOULD AVOID OVERLAP
                                      42.*                        OF INPUT ARRAY WITH OUTPUT ARRAY, AS THIS
                                      43.*                        CAN SLOW THE PERFORMANCE SIGNIFICANTLY.
                                      44.************************************************************************
                                      45.*
                                      46.* SUBROUTINE SBYTE (PACKED,UNPACKED,NOFF,NBITS)
                                      47.*
                                      48.* PURPOSE                GIVEN A BYTE, RIGHT-JUSTIFIED IN A WORD, TO
                                      49.*                        PACK THE BYTE INTO A TARGET WORD OR ARRAY.
                                      50.*                        BITS SURROUNDING THE BYTE IN THE TARGET
                                      51.*                        AREA ARE UNCHANGED.
                                      52.*
1CAL Version 2 - 11.1.0.0 (09/17/96)     SBYTE                                                                      Page 2
 Date: 02/05/98    Time: 14:31:35                                                                                     (2)
                                         V---+----1----+----2----+----3----+----4----+----5----+----6----+----7-V  Cray T.Series IEEE
                                       1.* USAGE                  CALL SBYTE (PACKED,UNPACKED,NOFF,NBITS)
                                       2.*
                                       3.* ARGUMENTS
                                       4.* ON INPUT               PACKED
                                       5.*                          THE WORD OR ARRAY WHICH WILL CONTAIN THE
                                       6.*                          PACKED BYTE.  BYTE MAY CROSS WORD BOUNDARIES.
                                       7.*
                                       8.*                        UNPACKED
                                       9.*                          THE WORD CONTAINING THE RIGHT-JUSTIFIED BYTE
                                      10.*                          TO BE PACKED.
                                      11.*
                                      12.*                        NOFF
                                      13.*                          THE NUMBER OF BITS TO SKIP, LEFT TO RIGHT,
                                      14.*                          IN 'PACKED' IN ORDER TO LOCATE WHERE THE
                                      15.*                          BYTE IS TO BE PACKED.
                                      16.*
                                      17.*                        NBITS
                                      18.*                          THE NUMBER OF BITS IN THE BYTE TO BE PACKED.
                                      19.*                          MAXIMUM OF 64 BITS.
                                      20.*
                                      21.* ON OUTPUT              PACKED
                                      22.*                          WORD OR CONSECUTIVE WORDS CONTAINING THE
                                      23.*                          REQUESTED BYTE.
                                      24.************************************************************************
                                      25.         IDENT     SBYTE
                              1       26.ARY         =           1                  ARRAY POINTER
                              2       27.DST         =           2                  DESTINATION POINTER
                              3       28.POS         =           3                  BIT POSITION POINTER
                              4       29.SIZ         =           4                  SIZE POINTER
                              5       30.OFST        =           5                  OFSET VALUE
                              6       31.SHFT        =           6                  SHIFT VALUE
                                      32.* H1         CON           A'CRAYLIB'          Q8QST4 ARGUMENT 1
                                      33.* H2         CON           A'BYTES'            Q8QST4 ARGUMENT 2
                                      34.* H3         CON           A'SBYTE'            Q8QST4 ARGUMENT 3
                                      35.* H4         CON           A'VERSION ',A'01'   Q8QST4 ARGUMENT 4
        0a  <macro>                   36.SBYTE         ENTER           NP=4
                                      37.*        The current word is now D'1 (O'1)                              <Edited Line>   :5
                                      38.                                                                        ** ENTER        :5
                                      39.*        The current word is now D'2 (O'2)                              <Edited Line>   :5
                                      40.                                                                        ** ENTER        :5
                                      41.*        The current word is now D'3 (O'3)                              <Edited Line>   :5
                                      42.                                                                        ** ENTER        :5
                                      43.*        The current word is now D'35 (O'43)                            <Edited Line>   :5
                                      44.                                                                        ** ENTER        :5
                                      45.*        The current word is now D'99 (O'143)                           <Edited Line>   :5
                                      46.                                                                        ** ENTER        :5
                                      47.*        The current word is now D'163 (O'243)                          <Edited Line>   :5
                                      48.                                                                        ** ENTER        :5
                                      49.*        The current word is now D'164 (O'244)                          <Edited Line>   :5
                                      50.                                                                        ** ENTER        :5
                                      51.*        The current word is now D'1188 (O'2244)                        <Edited Line>   :5
                                      52.                                                                        ** ENTER        :5
1CAL Version 2 - 11.1.0.0 (09/17/96)     SBYTE                                                                      Page 3
 Date: 02/05/98    Time: 14:31:35                                                                                     (3)
 Mixed                                   V---+----1----+----2----+----3----+----4----+----5----+----6----+----7-V  Cray T.Series IEEE
                                       1.*        The current word is now D'1189 (O'2245)                        <Edited Line>   :5
                                       2.                                                                        ** ENTER        :5
                                       3.*        The current word is now D'1190 (O'2246)                        <Edited Line>   :5
                                       4.                                                                        ** ENTER        :5
                                       5.*        The current word is now D'1191 (O'2247)                        <Edited Line>   :5
                                       6.                                                                        ** ENTER        :5
                                       7.*        The current word is now D'1192 (O'2250)                        <Edited Line>   :5
                                       8.                                                                        ** ENTER        :5
                                       9.*        The current word is now D'1193 (O'2251)                        <Edited Line>   :5
                                      10.                                                                        ** ENTER        :5
                                      11.*        The current word is now D'1194 (O'2252)                        <Edited Line>   :5
                                      12.                                                                        ** ENTER        :5
                                      13.*        The current word is now D'1322 (O'2452)                        <Edited Line>   :5
                                      14.                                                                        ** ENTER        :5
                                      15.*        The current word is now D'1323 (O'2453)                        <Edited Line>   :5
                                      16.                                                                        ** ENTER        :5
                           2454       17.LE@SA =         %%LOC$2                                                 <Edited Line>   :5
                                      18.                                                                        ** ENTER        :5
                                      19.*        The current word is now D'0 (O'0)                              <Edited Line>   :5
                                      20.                                                                        ** ENTER        :5
                                      21.*        The current word is now D'1 (O'1)                              <Edited Line>   :5
                                      22.                                                                        ** ENTER        :5
                                      23.*        The current word is now D'2 (O'2)                              <Edited Line>   :5
                                      24.                                                                        ** ENTER        :5
                                      25.*        The current word is now D'3 (O'3)                              <Edited Line>   :5
                                      26.                                                                        ** ENTER        :5
                                      27.*        The current word is now D'0 (O'0)                              <Edited Line>   :5
                                      28.                                                                        ** ENTER        :5
                                      29.*        D'4 Words defined above, LE@TBLK = D'4.                        <Edited Line>   :5
                                      30.                                                                        ** ENTER        :5
                                      31.*        The current word is now D'0 (O'0)                              <Edited Line>   :4
                                      32.                                                                        ** ENTER        :4
                                      33.*        The current word is now D'1 (O'1)                              <Edited Line>   :4
                                      34.                                                                        ** ENTER        :4
                              2       35.LE@ARLST =         %%LOC$2                                              <Edited Line>   :4
                                      36.                                                                        ** ENTER        :4
                                      37.*         CALL           Q8QST4,(H1,H2,H3,H4)
        4d  <macro>                   38.         ARGADD    A.POS,POS          FETCH LOCATION OF BYTE POSITION
        5d  <macro>                   39.         ARGADD    A.DST,DST          FETCH THE LOCATION SOURCE BYTE
        6d  <macro>                   40.         ARGADD    A.SIZ,SIZ          FETCH LOCATION OF BYTE SIZE
        7d  <macro>                   41.         ARGADD    A.ARY,ARY          FETCH LOCATION OF DESTINATION
       10d  123100 00000000000        42.         S1           0,A.POS         FETCH THE POSITION VALUE
       11c  122200 00000000000        43.         S2           0,A.DST         FETCH THE SOURCE BYTE RIGHT JUSTI FIED
       12b  104400 00000000000        44.         A.SIZ        0,A.SIZ         FETCH THE BYTE SIZE
       13a  042472                    45.         S4           <6              MAKE MASK FOR SHIFT COUNT
         b  042500                    46.         S5           <64
         c  044341                    47.         S3           S4&S1          ISOLATE THE SHIFT COUNT
         d  055172                    48.         S1           S1>6           DIVIDE THE POSITION BY 64
       14a  023510                    49.         A.OFST       S1             SET UP THE OFFSET
         b  023630                    50.         A.SHFT       S3             SET SHIFT COUNT
         c  030115                    51.         A.ARY        A.ARY+A.OFST   POINT TO ARRAY ELEMENT
         d  121600 00000000000        52.         S6           0,A.ARY        FETCH DESTINATION WORD
1CAL Version 2 - 11.1.0.0 (09/17/96)     SBYTE                                                                      Page 4
 Date: 02/05/98    Time: 14:31:35                                                                                     (4)
 Mixed                                   V---+----1----+----2----+----3----+----4----+----5----+----6----+----7-V  Cray T.Series IEEE
       15c  121700 00000000001         1.         S7           1,A.ARY        FETCH SECOND DESTINATION WORD
       16b  020500 00000000100         2.         A.OFST       64             SET UP BASE FOR ADDRESS CALCULATI  ON
       17a  031754                     3.         A7           A.OFST-A.SIZ   SET UP SHIFT COUNT TO LEFT JUST BYTE
         b  056507                     4.         S5           S5<A7          LEFT JUSTIFY MASK
         c  056207                     5.         S2           S2<A7          LEFT JUSTIFY BYTE
         d  031756                     6.         A7           A.OFST-A.SHFT  SET UP SHIFT COUNT TO POSITION BYTE
       20a  043300                     7.         S3           0              CLEAR MASK BUFFER
         b  043400                     8.         S4           0              CLEAR BYTE BUFFER
         c  056357                     9.         S3           S3,S5<A7       POSITION MASK
         d  056427                    10.         S4           S4,S2<A7       POSITION BYTE
       21a  050643                    11.         S6           S4!S6&S3       SLIP IN UPPER PORTION OF BYTE
         b  056507                    12.         S5           S5<A7          POSITION MASK IF NECESSARY
         c  056207                    13.         S2           S2<A7          POSITION BYTE IF NECESSARY
         d  050725                    14.         S7           S2!S7&S5       SLIP IN LOWER PORTION OF BYTE
       22a  131600 00000000000        15.         0,A.ARY      S6             RESTORE THE FIRST AND SECOND WORD
         d  131700 00000000001        16.         1,A.ARY      S7             WITH THE BYTE ADDED
       23c  <macro>                   17.         EXIT           NAME=SBYTE
                                      18.         END
1CAL Version 2 - 11.1.0.0 (09/17/96)     SBYTE                                                                      Page 5
 Date: 02/05/98    Time: 14:31:35                                                                                     (5)
                                              ------------------  Symbol Cross Reference  ------------------       Cray T.Series IEEE
 $$CPUTYPE                          . 2:36 D 
                               0    .
 $$SCONST                      0    . 3:30 D 
 $STKOFEN                      0X   . 3:36 D  3:36   
 %$MICRO                       1    . 3:36 F  3:36   
         File: /lib/asdef           .
 %$MULTI                       1    . 3:36 F  3:36   
         File: /lib/asdef           .
 %$NEWSEQ                      1    . 3:36 F  3:36   
         File: /lib/asdef           .
 %$STACK                       1    . 3:36 F  3:36   
         File: /lib/asdef           .
 %ARPTR                        6    . 3:36 D  3:36    3:36 F 
 %BCALLSQ                      3    . 3:36 D  3:36   
 %MSKFPR                       6    . 3:18 D 
 %MSKFPS                  374000    . 3:18 D 
 %MSKIFP                    1760    . 3:18 D 
 %NUMREG                      10    . 3:36 D 
 %SCRREG                       5    . 3:36 D  3:36    3:36 F 
 %SR0BDM                      22    . 3:18 D 
 %SR0DVS                      14    . 3:18 D 
 %SR0IDV                       5    . 3:18 D 
 %SR0INV                       4    . 3:18 D 
 %SR0INX                      10    . 3:18 D 
 %SR0IOR                      23    . 3:18 D 
 %SR0IOV                       6    . 3:18 D 
 %SR0IUN                       7    . 3:18 D 
 %SR0IXI                      11    . 3:18 D 
 %SR0NVS                      13    . 3:18 D 
 %SR0NXS                      17    . 3:18 D 
 %SR0OVS                      15    . 3:18 D 
 %SR0RM0                       1    . 3:18 D 
 %SR0RM1                       2    . 3:18 D 
 %SR0UNS                      16    . 3:18 D 
 %SR0XIS                      20    . 3:18 D 
 %STKATP                      67    . 3:36 R 
         File: /lib/asdef           .
 %STKCBP                       2    . 3:36 S  3:36 R  4:17 R 
         File: /lib/asdef           .
 %STKCTP                      66    . 3:36 R  3:36 S  4:17 S 
         File: /lib/asdef           .
 %STKPTR                       7    . 3:36 D  3:36    3:36 F 
 %TCALLSQ                      0    . 3:36 D  3:36   
 @SBYTE                        0    . 3:36 D 
 ADDR32$ON                          . 2:36 D  3:30 F 
                               0    .
 ADDR64$ON                          . 2:36 D  3:18 F  3:20 F  3:30 F  3:32 F 
                               1    .
 ARY                           1    . 2:26 D  3:41 F  3:41    3:41 R  3:51    3:52 R  4: 1 R  4:15 S  4:16 S 
 AVL$ON                        1    . 2:36 D 
 AVPOP$ON                      1    . 2:36 D 
 BDM$ON                        1    . 2:36 D 
1CAL Version 2 - 11.1.0.0 (09/17/96)     SBYTE                                                                      Page 6
 Date: 02/05/98    Time: 14:31:35                                                                                     (6)
                                              ------------------  Symbol Cross Reference  ------------------       Cray T.Series IEEE
 BMM$ON                        1    . 2:36 D 
 C$PARCELS                          . 2:36 D 
                               3    .
 CIGS$ON                       1    . 2:36 D 
 CORI$ON                       1    . 2:36 D 
 DST                           2    . 2:27 D  3:39 F  3:39    3:39 R  3:43 R 
 EMA$ON                        0    . 2:36 D 
 FCDSIZE                       2    . 3:36 D 
 HPM$ON                        1    . 2:36 D 
 IEEE$ON                       1    . 2:36 D  3:18 F  3:30 F 
 J$PARCELS                          . 2:36 D 
                               3    .
 L2$MAX$VL                          . 2:36 D 
                               7    .
 LE@ARLST                      2    . 3:35 D  3:36   
 LE@SA                      2454    . 3:17 D 
 LE@TBLK                       4    . 3:18 D  3:28 F  3:28   
 LH@ARLST                      1    . 3:30 D 
 LH@SA                         0    . 2:36 D 
 LH@TBLK                       1    . 3:18 D 
 MAX$BMM                       0    . 2:36 D 
 MAX$VL                      200    . 2:36 D 
 MAXCHL    400000000000000000000    . 3:34 D 
 N@ARADD                      60    . 3:32 D 
 N@ARCHCK                      1    . 3:30 D 
 N@ARCHR                       1    . 3:30 D 
 N@ARFLAG                      1    . 3:30 D 
 N@ARGU                       14    . 3:30 D 
 N@ARGW                       24    . 3:30 D 
 N@ARLEN                     100    . 3:34 D 
 N@ARLENB                     75    . 3:34 D 
 N@ARLINE                     30    . 3:30 D 
 N@ARN                        40    . 3:30 D  3:36   
 N@AROFF                       6    . 3:32 D 
 N@ARVAL                       1    . 3:30 D  3:36   
 N@SACNTX                    100    . 3:16 D 
 N@SAMASK                    100    . 2:36 D 
 N@SANUM                     100    . 3: 6 D 
 N@SAOCTP                    100    . 3:10 D 
 N@SAOFF                     100    . 2:40 D 
 N@SAPADDR                          . 3: 4 D 
                             100    .
 N@SAPEND                    100    . 2:38 D 
 N@SASMSK                    100    . 3: 8 D 
 N@SASVVL                    100    . 3: 2 D 
 N@SASVVM                    100    . 2:48 D 
 N@SASVVM1                          . 2:52 D 
                             100    .
 N@SAVEC                     100    . 3:14 D 
 N@TBLKBA                    100    . 3:20 D 
 N@TBLKBL                    100    . 3:26 D 
 N@TBLKBN                    100    . 3:28 D 
1CAL Version 2 - 11.1.0.0 (09/17/96)     SBYTE                                                                      Page 7
 Date: 02/05/98    Time: 14:31:35                                                                                     (7)
                                              ------------------  Symbol Cross Reference  ------------------       Cray T.Series IEEE
 N@TBLKIF                      1    . 3:22 D 
 N@TBLKIV                    100    . 3:24 D 
 N@TBLKNB                     20    . 3:18 D 
 N@TBLKNN                     10    . 3:22 D 
 N@TBLKNP                     40    . 3:22 D 
 N@TBLKVN                      7    . 3:18 D 
 N@TBLKWC                     40    . 3:18 D 
 N@TNB0                       40    . 3:36   
         File: /lib/asdef           .
 N@TNBBL                       1    . 3:36   
         File: /lib/asdef           .
 N@TNBCR                      14    . 3:36   
         File: /lib/asdef           .
 N@TNBL                       20    . 3:36   
         File: /lib/asdef           .
 N@TNBLN                      20    . 3:36   
         File: /lib/asdef           .
 N@TNBLT                       7    . 3:36   
         File: /lib/asdef           .
 N@TNBNB                       7    . 3:36   
         File: /lib/asdef           .
 N@TNBNT                       7    . 3:36   
         File: /lib/asdef           .
 N@TNBSCS                     40    . 3:36   
         File: /lib/asdef           .
 N@TNBSVS                     40    . 3:36   
         File: /lib/asdef           .
 N@TNBTVS                     40    . 3:36   
         File: /lib/asdef           .
 NE@SA                         1    . 2:36 D 
 OFST                          5    . 2:30 D  3:49    3:51    4: 2    4: 3    4: 6   
 PC$ON                         1    . 2:36 D 
 POS                           3    . 2:28 D  3:38 F  3:38    3:38 R  3:42 R 
 R$PARCELS                          . 2:36 D 
                               1    .
 READVL$ON                          . 2:36 D 
                               1    .
 S@ARADD                      20    . 3:32 D 
 S@ARCHCK                      3    . 3:30 D 
 S@ARCHR                       2    . 3:30 D 
 S@ARFLAG                      0    . 3:30 D 
 S@ARGU                       40    . 3:30 D 
 S@ARGW                       54    . 3:30 D 
 S@ARLEN                       0    . 3:34 D 
 S@ARLENB                      0    . 3:34 D 
 S@ARLINE                     10    . 3:30 D 
 S@ARN                        40    . 3:30 D  3:36   
 S@AROFF                       0    . 3:32 D 
 S@ARVAL                       1    . 3:30 D  3:36   
 S@SACNTX                      0    . 3:16 D 
 S@SAMASK                      0    . 2:36 D 
 S@SANUM                       0    . 3: 6 D 
1CAL Version 2 - 11.1.0.0 (09/17/96)     SBYTE                                                                      Page 8
 Date: 02/05/98    Time: 14:31:35                                                                                     (8)
                                              ------------------  Symbol Cross Reference  ------------------       Cray T.Series IEEE
 S@SAOCTP                      0    . 3:10 D 
 S@SAOFF                       0    . 2:40 D 
 S@SAPADDR                          . 3: 4 D 
                               0    .
 S@SAPEND                      0    . 2:38 D 
 S@SASMSK                      0    . 3: 8 D 
 S@SASVVL                      0    . 3: 2 D 
 S@SASVVM                      0    . 2:48 D 
 S@SASVVM1                          . 2:52 D 
                               0    .
 S@SAVEC                       0    . 3:14 D 
 S@TBLKBA                      0    . 3:20 D 
 S@TBLKBL                      0    . 3:26 D 
 S@TBLKBN                      0    . 3:28 D 
 S@TBLKIF                      0    . 3:22 D 
 S@TBLKIV                      0    . 3:24 D 
 S@TBLKNB                     20    . 3:18 D 
 S@TBLKNN                     30    . 3:22 D 
 S@TBLKNP                     40    . 3:22 D 
 S@TBLKVN                      0    . 3:18 D 
 S@TBLKWC                     40    . 3:18 D 
 S@TNBNT                      62    . 3:36   
         File: /lib/asdef           .
 SAOFFA                       20    . 2:42 D 
 SAOFFS                       30    . 2:42 D 
 SBYTE                         0a+  . 3:36    3:36 E  3:36 D 
 SHFT                          6    . 2:31 D  3:50    4: 6   
 SIZ                           4    . 2:29 D  3:40 F  3:40    3:40 R  3:44 R  3:44    4: 3   
 STATRG$ON                          . 2:36 D 
                               1    .
 VPOP$ON                       1    . 2:36 D 
 VRECUR$ON                          . 2:36 D 
                               0    .
 VSHIFT$ON                          . 2:36 D 
                               1    .
 W@ARADD                       0    . 3:32 D 
 W@ARCHCK                      0    . 3:30 D 
 W@ARCHR                       0    . 3:30 D 
 W@ARFLAG                      0    . 3:30 D 
 W@ARGU                        0    . 3:30 D 
 W@ARGW                        0    . 3:30 D 
 W@ARLEN                       1    . 3:34 D 
 W@ARLENB                      1    . 3:34 D 
 W@ARLINE                      0    . 3:30 D 
 W@ARN                         0    . 3:30 D 
 W@AROFF                       0    . 3:32 D 
 W@ARVAL                       0    . 3:30 D 
 W@SACNTX                   2453    . 3:16 D 
 W@SAMASK                      0    . 2:36 D 
 W@SANUM                    2247    . 3: 6 D 
 W@SAOCTP                   2251    . 3:10 D 
 W@SAOFF                       2    . 2:40 D 
1CAL Version 2 - 11.1.0.0 (09/17/96)     SBYTE                                                                      Page 9
 Date: 02/05/98    Time: 14:31:35                                                                                     (9)
                                              ------------------  Symbol Cross Reference  ------------------       Cray T.Series IEEE
 W@SAPADDR                          . 3: 4 D 
                            2246    .
 W@SAPEND                      1    . 2:38 D 
 W@SASMSK                   2250    . 3: 8 D 
 W@SASTK                    2252    . 3:10 D 
 W@SASVB                      43    . 2:42 D 
 W@SASVT                     143    . 2:44 D 
 W@SASVV                     244    . 2:48 D 
 W@SASVVL                   2245    . 3: 2 D 
 W@SASVVM                    243    . 2:48 D 
 W@SASVVM1                          . 2:52 D 
                            2244    .
 W@SAVEC                    2452    . 3:14 D 
 W@SAXP                        3    . 2:40 D 
 W@TBLKBA                      0    . 3:20 D 
 W@TBLKBL                      3    . 3:26 D 
 W@TBLKBN                      0    . 3:28 D 
 W@TBLKIF                      1    . 3:22 D 
 W@TBLKIV                      2    . 3:24 D 
 W@TBLKNB                      0    . 3:18 D 
 W@TBLKNN                      1    . 3:22 D 
 W@TBLKNP                      1    . 3:22 D 
 W@TBLKVN                      0    . 3:18 D 
 W@TBLKWC                      0    . 3:18 D 
 XASET$ON                      1    . 2:36 D 
 XEA$ON                        0    . 2:36 D 
 XSSET$ON                      1    . 2:36 D 
1
