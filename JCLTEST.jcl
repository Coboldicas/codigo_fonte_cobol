//COBUCL  PROC CPARM1='LOAD,SUPMAP',                                   
//             CPARM2='SIZE=2048K,BUF=1024K',                          
//             LKEDPGM='IEWL'                                          
//COB  EXEC  PGM=IKFCBL00,REGION=4096K,                                
//           PARM='&CPARM1,&CPARM2'                                    
//STEPLIB  DD DSN=SYSC.LINKLIB,DISP=SHR                                
//SYSPRINT  DD SYSOUT=*                                                
//SYSUT1 DD UNIT=SYSDA,SPACE=(460,(700,100))                           
//SYSUT2 DD UNIT=SYSDA,SPACE=(460,(700,100))                           
//SYSUT3 DD UNIT=SYSDA,SPACE=(460,(700,100))                           
//SYSUT4 DD UNIT=SYSDA,SPACE=(460,(700,100))                           
//SYSLIN DD DSN=&LOADSET,DISP=(MOD,PASS),UNIT=SYSDA,                   
//             SPACE=(80,(500,100))                                    
//LKED EXEC PGM=&LKEDPGM,                                              
//          PARM='LIST,XREF,LET',COND=(5,LT,COB),REGION=96K            
//SYSLIN  DD DSN=&LOADSET,DISP=(OLD,DELETE)                            
//  DD  DDNAME=SYSIN                                                   
//SYSLMOD DD DDNAME=SYSLMOD                                            
//SYSLIB DD   DSN=SYSC.COBLIB,DISP=SHR                                 
//SYSUT1 DD UNIT=SYSDA,SPACE=(1024,(50,20))                            
//SYSPRINT DD SYSOUT=*                                                 




//LKED EXEC PGM=IEWL,PARM='LIST,XREF,LET'                   
//SYSLIN       DD DSNAME=&LOADSET,DISP=(OLD,DELETE)         
***             DD DDNAME=SYSIN                             
//SYSLIN       DD *                                         
***SYSLIB       DD DSNAME=SYS1.COBLIB,DISP=SHR              
//SYSLIB       DD DSN=CDICAS.TEST.LOADLIB,DISP=SHR          
//LKED.SYSLMOD DD DSN=CDICAS.TEST.LOADLIB(PROGTEST),DISP=SHR
//LKED.SYSLIB  DD DSN=SYS1.COBLIB,DISP=SHR                  
//             DD DSN=SYS1.LINKLIB,DISP=SHR                 
//SYSPRINT     DD SYSOUT=*                                  