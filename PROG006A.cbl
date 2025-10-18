       IDENTIFICATION DIVISION.
       PROGRAM-ID. ExemploBMS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-MAPA PIC X(40).
       77 WS-NOME PIC X(20).
       
       EXEC CICS DFLD RECEIVE INTO(WS-MAPA) LENGTH(40)
            END-EXEC.
       
       PROCEDURE DIVISION.
           EXEC CICS SEND MAP('EXEMPLO') MAPSET('EXEMPLO')
                END-EXEC.
       
           EXEC CICS RECEIVE MAP('EXEMPLO') MAPSET('EXEMPLO')
                INTO(WS-NOME) END-EXEC.
       
           DISPLAY 'Nome digitado: ' WS-NOME.
       
           EXEC CICS RETURN END-EXEC.
       