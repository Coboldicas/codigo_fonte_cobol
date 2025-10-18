       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGTEST4.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ID-PROCURADO         PIC 9(3) VALUE 102.
       01 WS-ID-ENCONTRADO        PIC X VALUE 'N'.
       01 INDICE                  PIC 9(3) COMP-3 VALUE 1.

       01 TABELA-DADOS.
          05 TABELA-CLIENTES OCCURS 5 TIMES
             INDEXED BY IDX.
             10 CLI-ID         PIC 9(3).
             10 CLI-NOME       PIC X(20).

       PROCEDURE DIVISION.
       INICIO.
           MOVE 101 TO CLI-ID (1)
           MOVE "ANA PAULA" TO CLI-NOME (1)
           MOVE 102 TO CLI-ID (2)
           MOVE "JOÃO SILVA" TO CLI-NOME (2)
           MOVE 103 TO CLI-ID (3)
           MOVE "MARIA LIMA" TO CLI-NOME (3)
           MOVE 104 TO CLI-ID (4)
           MOVE "PEDRO ALVES" TO CLI-NOME (4)
           MOVE 105 TO CLI-ID (5)
           MOVE "LUCAS COSTA" TO CLI-NOME (5)

           SET IDX TO 1
           SEARCH TABELA-CLIENTES
               AT END
                   DISPLAY "ID NÃO ENCONTRADO."
               WHEN CLI-ID (IDX) = WS-ID-PROCURADO
                   MOVE "S" TO WS-ID-ENCONTRADO
                   DISPLAY "CLIENTE ENCONTRADO: " CLI-NOME (IDX)
           END-SEARCH

           IF WS-ID-ENCONTRADO = 'N'
               DISPLAY "CLIENTE COM ID " 
               WS-ID-PROCURADO " NÃO ENCONTRADO."
           END-IF

           STOP RUN.
