       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. PROJECT.                                             
       AUTHOR. GIRI.                                                
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           EXEC SQL                                                     
              INCLUDE SQLCA                                             
           END-EXEC. 
           EXEC SQL                                                     
              INCLUDE DB2TAB0                                             
           END-EXEC.   
           EXEC SQL                                                     
              INCLUDE DB2TAB1                                             
           END-EXEC.                                          
       01 WS-TIME.                                                      
            05 ASK-TIME            PIC S9(15) COMP-3.                   
            05 ASK-TIME1           PIC X(80) VALUE SPACES.              
            05 ASK-DATE1           PIC X(80) VALUE SPACES.          
       01 WS-IN.                                                        
           10 WS-SNO               PIC X(3).                            
           10 WS-FILLER            PIC X(3).                            
           10 WS-ENO               PIC X(5).                            
           10 WS-FILLER            PIC X(3).                            
           10 WS-ENAME             PIC X(10).                           
           10 WS-FILLER            PIC X(3).                            
           10 WS-MNAME             PIC X(10).                           
           10 WS-FILLER            PIC X(3).                            
           10 WS-PNAME             PIC X(10).                           
           10 WS-FILLER            PIC X(3).                            
           10 WS-SALARY            PIC ZZ9,999.99. 
           10 WS-FILLER            PIC X(3).                            
           10 WS-PCOST             PIC ZZ9,999.99.           
       01 REC-KEY                  PIC X(5).                            
       01 WS-RESP                  PIC S9(8) USAGE COMP.                
       01 WS-USER                  PIC X(5).                           
       01 WS-PWD                   PIC X(8).                            
       01 WS-COUNT                 PIC 9(1) VALUE 0.                    
       01 WS-TOTAL                 PIC 9(10).                           
       01 WS-SAL                   PIC 9(10).                           
       01 I                        PIC 9.    
           COPY VSAMREC.                                       
           COPY TEC105M.                                                
       PROCEDURE DIVISION.                                              
       000-EMP-MAIN.                                                    
           EXEC CICS                                                    
                HANDLE AID                                              
                  PF1 (002-REFRESH-PARA)                                
                  PF2 (003-QUIT-PARA)                                   
            END-EXEC.                                                   
           MOVE LOW-VALUES TO LOGINI LOGINO.                            
           MOVE LOW-VALUES TO EMPDI EMPDO.                              
           MOVE SPACES TO WS-REC1.                                      
           MOVE SPACES TO WS-IN.                                        
           MOVE 'ENTER LOGIN DETAILS' TO MSGO.                          
           MOVE 'LOGIN PAGE' TO INFO1O.                                 
       001-PERF-MAP1.                                                   
            EXEC CICS SEND                                              
                 MAP('LOGIN') MAPSET('TEC105M')                         
                  ERASE                                                 
            END-EXEC.                                                   
            EXEC CICS RECEIVE                                           
                 MAP('LOGIN') MAPSET('TEC105M')                         
            END-EXEC.                                                   
           MOVE UIDI TO WS-USER.                                        
           MOVE PSWRDI TO WS-PWD. 
           EXEC SQL
           SELECT USER,PASS
           INTO :HUSER, :HPASS
           FROM TRNGGRP.DB2TAB0
           WHERE USER = :WS-USER ANS PASS = :WS-PWD
           END-EXEC. 
           IF SQLCODE = O THEN                                           
               PERFORM 100-EMP-DETAILS                                  
               PERFORM 300-SEND-MAP                                     
               PERFORM 003-QUIT-PARA                                    
           ELSE                                                         
                PERFORM 200-ERROR-MSG                                   
           END-IF.                                                      
       200-ERROR-MSG.                                                   
            COMPUTE WS-COUNT = WS-COUNT + 1.                            
             IF ( WS-COUNT < 4 ) THEN                                   
              MOVE LOW-VALUES TO LOGINO                                 
              MOVE 'ENTER VALID LOGIN DETAILS' TO MSGO                  
              MOVE 'ATHENTICATION FAILURE PAGE' TO INFO1O               
              PERFORM 001-PERF-MAP1                                     
            ELSE                                                        
               MOVE 'REVOKED PAGE' TO INFO1O                            
               MOVE 'YOUR PREVILAGE IS REVOKED' TO MSGO                 
             EXEC CICS                                                  
                 SEND MAP('LOGIN') MAPSET('TEC105M')                    
                  ERASE                                                 
             END-EXEC                                                   
            PERFORM 003-QUIT-PARA                                       
            END-IF.                                                     
       002-REFRESH-PARA.                                                
             MOVE LOW-VALUES TO LOGINO.                                 
             MOVE 'REFRESH PAGE' TO INFO1O.                             
             PERFORM 001-PERF-MAP1.                                     
       100-EMP-DETAILS.                                                 
              EXEC CICS ASKTIME                                         
                 ABSTIME(ASK-TIME)                                      
              END-EXEC.                                                 
              EXEC CICS                                                 
                FORMATTIME ABSTIME(ASK-TIME)                            
                 YYYYMMDD(ASK-DATE1)                                    
                  DATESEP('/')                                          
                   TIME(ASK-TIME1)                                      
                    TIMESEP                                             
               END-EXEC.                                                
               MOVE ASK-DATE1 TO DATEO.                                 
               MOVE ASK-TIME1 TO TIMEO.                                 
               EXEC SQL                                                 
                DECLARE EMPCUR CURSOR FOR                               
                  SELECT ENO,DNO,ENAME,SALARY                           
                    FROM TRNGGRP.DB2TAB1                             
               END-EXEC.                                                
               EXEC SQL                                                 
                 OPEN EMPCUR                                            
               END-EXEC.                                                
              PERFORM VARYING I FROM 1 BY 1 UNTIL SQLCODE NOT = 0       
               EXEC SQL                                                 
                FETCH EMPCUR                                            
                  INTO :HENO,:HDNO,:HENAME,:HSALARY                     
               END-EXEC                                                 
               MOVE HENO TO WS-ENO                                      
               MOVE HENAME TO WS-ENAME                                  
               MOVE HSALARY TO WS-SALARY WS-SAL                         
               COMPUTE WS-TOTAL = WS-TOTAL + WS-SAL                     
               MOVE HDNO TO REC-KEY                                     
               EXEC CICS                                                
                 READ                                                  
                  FILE('VSAB13')                                       
                   INTO(WS-REC1)                                        
                    RIDFLD(REC-KEY)                                     
                     RESP(WS-RESP)                                      
                      LENGTH(LENGTH OF WS-REC1)                         
               END-EXEC                                                 
               IF WS-RESP = DFHRESP(NORMAL) THEN                        
                 MOVE WS-PNAME1 TO WS-PNAME 
                 MOVE WS-PCOST1 TO WS-PCOST                            
                 MOVE WS-MNAME1 TO WS-MNAME                             
               END-IF                                                   
               IF WS-MNAME1 IS EQUAL TO SPACES THEN                     
                  MOVE 'XYZ' TO WS-MNAME1                               
                  MOVE WS-MNAME1 TO WS-MNAME                            
               END-IF                                                   
                 MOVE I TO WS-SNO                                       
               IF SQLCODE = 0 THEN                                      
                IF WS-PCOST > 5000000.00 THEN                               
                 MOVE '2' TO RECC(I)                                    
                END-IF                                                  
                MOVE WS-IN TO RECO(I)                                   
                END-IF                                                  
               END-PERFORM.                                             
        300-SEND-MAP.                                                   
               MOVE WS-TOTAL TO TOTALO.                                 
               MOVE 'EMPLOYEE DETAILS' TO INFO2O                        
               EXEC CICS                                                
                   SEND MAP('EMPD') MAPSET('TEC105M')                   
                    ERASE                                               
               END-EXEC.                                                
       003-QUIT-PARA.                                                   
               EXEC CICS                                                
                  RETURN                                                
               END-EXEC.                                                
           STOP RUN.                                                    
