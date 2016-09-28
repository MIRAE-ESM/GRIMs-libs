C-----------------------------------------------------------------------
C  MAIN PROGRAM PREPDATE.  MODIFIED BY MK TO SIMPLY OUTPUT BUFR FILE DATES
C-----------------------------------------------------------------------
C
      COMMON/COUNT/KTS(52),KTF(52)                                      
C                                                                       
      CHARACTER*50 HEADR,OBLVL,QMLVL,FCLVL,ERLVL,OBSNG,QMSNG,FCSNG,     
     $ ERSNG                                                            
      CHARACTER*8  SUBSET                                               
      CHARACTER*6  STNID                                                
C                                                                       
      DIMENSION  HDR(7),OBS(9,255),FCS(8,255),QMS(7,255),ERS(7,255)     
C                                                                       
      INTEGER  IRTYP(289),NBUF(1280),IQ(6)                              
C                                                                       
      EQUIVALENCE(STNID,HDR(1))                                         
C                                                                       
      DATA HEADR /'SID XOB YOB DHR TYP T29 ITP                       '/ 
      DATA OBLVL /'POB QOB TOB ZOB UOB VOB RHO PWO CAT               '/ 
      DATA OBSNG /'PMO                                               '/ 
      DATA QMLVL /'PQM QQM TQM ZQM WQM RHQ PWQ                       '/ 
      DATA QMSNG /'PMQ                                               '/ 
      DATA FCLVL /'PFC QFC TFC ZFC UFC VFC RHF PWF                   '/ 
      DATA FCSNG /'PMF                                               '/ 
      DATA ERLVL /'PER QER TER ZER WER RHE PWE                       '/ 
      DATA ERSNG /'PME                                               '/ 
C                                                                       
      DATA  LUBFR/11/,IREC/0/,KOUNT/0/                                   
C                                                                       
  	  JDATE = IHR + 100*IDY + 10000*IMN + 1000000*IYR
C                                                                       
C  OPEN BUFR FILE - READ IN FIRST MESSAGE (TABLE A ENTRIES)             
C                                                                       
      CALL OPENBF(LUBFR,'IN',LUBFR)                                     
      WRITE(6,100) LUBFR                                                  
  100 FORMAT(/5X,'BUFR/PREPDA DATA SET IN UNIT',I3,' SUCCESSFULLY',
     $ ' OPENED -- FIRST MESSAGE CONTAINING TABLE A ENTRIES READ'/)     
C                                                                       
C  READ IN FIRST DATA MESSAGE AND GET DATE                              
C                                                                       
      CALL READMG(LUBFR,SUBSET,IDATE,IRET)                              
      IF(IRET.EQ.0)  THEN
  		  PRINT '('' BUFR/PREPDA FILE HAS DATE: '',I8//)',IDATE  
        IYR=IDATE/1000000
        IMN=(IDATE-IYR*1000000)/10000
        IDY=(IDATE-IYR*1000000-IMN*10000)/100
        IHR=IDATE-IYR*1000000-IMN*10000-IDY*100
        PRINT *,IYR,IMN,IDY,IHR
      ELSE
        PRINT *,9999,99,99,99
		  ENDIF
C
      END
