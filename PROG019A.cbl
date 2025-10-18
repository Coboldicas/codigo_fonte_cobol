      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 12/05/2025
      * DESCRICAO..: PROGRAMA COBOL - COMANDO SEARCH
      * NOME.......: PROG019A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG019A.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       working-storage section.

       01  WRK-IND1            PIC 9(2) VALUE ZEROS.
       
       01  WRK-PROCURADO       PIC 9(3) VALUE ZEROS.
       01  WRK-ENCONTRADO      PIC X(1) VALUE 'N'.


       01  TABELA-DADOS.
        05 TABELA-CLIENTE OCCURS 5 TIMES 
            INDEXED BY IDX.
         10 CLI-ID     PIC 9(03). 
         10 CLI-NOME   PIC X(20). 


       PROCEDURE DIVISION.

       100-INICIO.
      *> cobol-lint CL002 100-INICIO

           DISPLAY 'DIGITE O ID PARA PROCURAR'
           ACCEPT WRK-PROCURADO

           MOVE 101 TO CLI-ID(1)
           MOVE 'ANA PAULA'   TO CLI-NOME(1)       
           MOVE 102 TO CLI-ID(2)
           MOVE 'JOAO SILVA'  TO CLI-NOME(2)       
           MOVE 103 TO CLI-ID(3)
           MOVE 'MARIA LIMA'  TO CLI-NOME(3)       
           MOVE 104 TO CLI-ID(4)
           MOVE 'PEDRO ALVES' TO CLI-NOME(4)       
           MOVE 105 TO CLI-ID(5)
           MOVE 'LUCAS COSTA' TO CLI-NOME(5)       

           SET IDX TO 1

           SEARCH TABELA-CLIENTE
               AT END 
                  DISPLAY 'ID NAO ENCONTRADO'
               WHEN CLI-ID (IDX) = WRK-PROCURADO
                  MOVE 'S'  TO WRK-ENCONTRADO
                  DISPLAY 'CLIENTE ENCONTRADO - ID: '
                   CLI-ID(IDX) ' - NOME: '
                  CLI-NOME (IDX)
           END-SEARCH  
         
           IF  WRK-ENCONTRADO EQUAL 'N'
               DISPLAY 'ID: ' WRK-PROCURADO ' NAO ENCONTRADO!'
           END-IF

           STOP RUN.
      *> cobol-lint CL002 999-end
       999-END.
