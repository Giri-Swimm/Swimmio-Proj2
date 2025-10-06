       01  LOGINI.                                   
           02  FILLER PIC X(12).                     
           02  UIDL    COMP  PIC  S9(4).             
           02  UIDF    PICTURE X.                    
           02  FILLER REDEFINES UIDF.                
             03 UIDA    PICTURE X.                   
           02  FILLER   PICTURE X(2).                
           02  UIDI  PIC X(10).                      
           02  PSWRDL    COMP  PIC  S9(4).           
           02  PSWRDF    PICTURE X.                  
           02  FILLER REDEFINES PSWRDF.              
             03 PSWRDA    PICTURE X.                 
           02  FILLER   PICTURE X(2).                
           02  PSWRDI  PIC X(6).                     
           02  MSGL    COMP  PIC  S9(4).             
           02  MSGF    PICTURE X.                    
           02  FILLER REDEFINES MSGF.                
             03 MSGA    PICTURE X.                   
           02  FILLER   PICTURE X(2).                
           02  MSGI  PIC X(27).                      
           02  INFO1L    COMP  PIC  S9(4).           
           02  INFO1F    PICTURE X.                  
           02  FILLER REDEFINES INFO1F.              
             03 INFO1A    PICTURE X.                 
           02  FILLER   PICTURE X(2). 
            02  INFO1I  PIC X(40).                     
        01  LOGINO REDEFINES LOGINI.                   
            02  FILLER PIC X(12).                      
            02  FILLER PICTURE X(3).                   
            02  UIDC    PICTURE X.                     
            02  UIDH    PICTURE X.                     
            02  UIDO  PIC X(10).                       
            02  FILLER PICTURE X(3).                   
            02  PSWRDC    PICTURE X.                   
            02  PSWRDH    PICTURE X.                   
            02  PSWRDO  PIC X(6).                      
            02  FILLER PICTURE X(3).                   
            02  MSGC    PICTURE X.                     
            02  MSGH    PICTURE X.                     
            02  MSGO  PIC X(27).                       
            02  FILLER PICTURE X(3).                   
            02  INFO1C    PICTURE X.                   
            02  INFO1H    PICTURE X.                   
            02  INFO1O  PIC X(40).                     
        01  EMPDI.                                     
            02  FILLER PIC X(12).                      
            02  DATEL    COMP  PIC  S9(4).             
            02  DATEF    PICTURE X.                    
            02  FILLER REDEFINES DATEF.                
              03 DATEA    PICTURE X.                   
            02  FILLER   PICTURE X(2). 
            02  DATEI  PIC X(10).                              
            02  TIMEL    COMP  PIC  S9(4).                     
            02  TIMEF    PICTURE X.                            
            02  FILLER REDEFINES TIMEF.                        
              03 TIMEA    PICTURE X.                           
            02  FILLER   PICTURE X(2).                         
            02  TIMEI  PIC X(8).                               
            02  RECD OCCURS 7 TIMES.                           
              03  RECL    COMP  PIC  S9(4).                    
              03  RECF    PICTURE X.                           
              03  FILLER   PICTURE X(2).                       
              03  RECI  PIC X(79).                             
            02  TOTALL    COMP  PIC  S9(4).                    
            02  TOTALF    PICTURE X.                           
            02  FILLER REDEFINES TOTALF.                       
              03 TOTALA    PICTURE X.                          
            02  FILLER   PICTURE X(2).                         
            02  TOTALI  PIC 9(12).                             
            02  INFO2L    COMP  PIC  S9(4).                    
            02  INFO2F    PICTURE X.                           
            02  FILLER REDEFINES INFO2F.                       
              03 INFO2A    PICTURE X.                          
            02  FILLER   PICTURE X(2).                         
            02  INFO2I  PIC X(40).                             
        01  EMPDO REDEFINES EMPDI.                             
            02  FILLER PIC X(12).                              
            02  FILLER PICTURE X(3).              
            02  DATEC    PICTURE X.               
            02  DATEH    PICTURE X.               
            02  DATEO  PIC X(10).                 
            02  FILLER PICTURE X(3).              
            02  TIMEC    PICTURE X.               
            02  TIMEH    PICTURE X.               
            02  TIMEO  PIC X(8).                  
            02  DFHMS1 OCCURS 7 TIMES.            
              03  FILLER PICTURE X(2).            
              03  RECA    PICTURE X.              
              03  RECC    PICTURE X.              
              03  RECH    PICTURE X.              
              03  RECO  PIC X(79).                
            02  FILLER PICTURE X(3).              
            02  TOTALC    PICTURE X.              
            02  TOTALH    PICTURE X.              
            02  TOTALO PIC ZZ,ZZ,ZZ9.99.          
            02  FILLER PICTURE X(3).              
            02  INFO2C    PICTURE X.              
            02  INFO2H    PICTURE X.              
            02  INFO2O  PIC X(40).                                               