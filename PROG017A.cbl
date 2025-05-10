       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG017A.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       working-storage section.

       01 WRK-VALOR    PIC 9(01) VALUES ZEROS.
       01 STATUS-PEDIDO PIC X(1).
         88 PEDIDO-APROVADO VALUE "A".
         88 PEDIDO-CANCELADO VALUE "C".
      *01  WRK-IND1 PIC 9(2).

       01  WRK-TABELA-CODIGOS.
         05 WRK-CODIGO   PIC X(5) OCCURS 10 TIMES INDEXED BY WRK-IND1.





       PROCEDURE DIVISION.

           DISPLAY 'DIGITE O VALOR'
           ACCEPT WRK-VALOR

           IF WRK-VALOR EQUAL 1
              SET PEDIDO-APROVADO TO TRUE
           ELSE 
              IF WRK-VALOR EQUAL 2
                 SET PEDIDO-CANCELADO TO TRUE
              ELSE
                 DISPLAY 'ERRO NO PEDIDO!'
              END-IF 
           END-IF

           IF STATUS-PEDIDO EQUAL 'A' 
                DISPLAY 'PEDIDO APROVADO!'
                DISPLAY 'STATUS-PEDIDO: ' STATUS-PEDIDO
           ELSE
              IF PEDIDO-CANCELADO                 
                DISPLAY 'PEDIDO CANCELADO!'
                DISPLAY 'STATUS-PEDIDO: ' STATUS-PEDIDO
              END-IF
           END-IF

           MOVE 'ABCDE' TO WRK-CODIGO(1)
           MOVE 'EFGHI' TO WRK-CODIGO(2)
           MOVE 'JKLMN' TO WRK-CODIGO(3)
           MOVE 'OPQRS' TO WRK-CODIGO(4)
           MOVE 'TUVXZ' TO WRK-CODIGO(5)

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 
                                    UNTIL WRK-IND1 EQUAL 10
                   DISPLAY 'WRK-CODIGO :' WRK-CODIGO(WRK-IND1) 
                  
           END-PERFORM

           IF STATUS-PEDIDO EQUAL 'A' 
              SET WRK-IND1 TO 1
              DISPLAY 'WRK-CODIGO :' WRK-CODIGO(WRK-IND1) 
           ELSE
              SET WRK-IND1 TO 5
              DISPLAY 'WRK-CODIGO :' WRK-CODIGO(WRK-IND1) 
           END-IF 

           EVALUATE STATUS-PEDIDO
             WHEN 'A' 
                DISPLAY 'PEDIDO APROVADO!'
             WHEN 'B'
                DISPLAY 'PEDIDO CANCELADO!'
           END-EVALUATE

           STOP RUN.