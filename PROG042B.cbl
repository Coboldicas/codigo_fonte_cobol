      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 24/05/2026
      * DESCRICAO..: CONSULTA ULTIMO CODIGO DO PEDIDO CADASTRADO
      * MODULO.....: CADASTRO DE PEDIDOS
      * NOME.......:   PROG042B
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG042B. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT ASSIGN TO 'CADASTRO3.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT.
       01  FD-ARQ-ENT.
         05  FDE-REG-ID-PEDIDO            PIC 9(05) VALUE ZEROS.
         05  FDE-REG-ID-CLIENTE           PIC 9(05) VALUE ZEROS.
         05  FDE-REG-NOME-CLIENTE         PIC X(30) VALUE SPACES.
         05  FDE-REG-ID-PRODUTO           PIC 9(05) VALUE ZEROS.
         05  FDE-REG-NOME-PRODUTO         PIC X(30) VALUE SPACES.
         05  FDE-REG-QTDE-PRODUTO         PIC 9(03) VALUE ZEROS.
         05  FDE-REG-VALOR-UNIT           PIC 9(05)V99 VALUE ZEROS.
         05  FDE-REG-VALOR-TOTAL          PIC 9(05)V99 VALUE ZEROS.

       WORKING-STORAGE SECTION.
       01  WRK-FIM-ARQUIVO                PIC X(01) VALUE 'N'.
       01  WRK-QTDE-REG                   PIC 9(05) VALUE ZEROS.           
       01  WRK-IND1                       PIC 9(05) VALUE ZEROS.           
       01  WRK-IND2                       PIC 9(05) VALUE ZEROS.           
       01  WRK-ID-PEDIDO-AUX              PIC 9(05) VALUE ZEROS.

       01 TABELA-PEDIDOS.
           05 DADOS-PEDIDOS     OCCURS 50 TIMES.
               10 TAB-ID-PEDIDO         PIC 9(05).
               10 TAB-ID-CLIENTE        PIC 9(05).
               10 TAB-NOME-CLIENTE      PIC X(30).
               10 TAB-ID-PRODUTO        PIC 9(05).
               10 TAB-NOME-PRODUTO      PIC X(30).
               10 TAB-QTDE-PRODUTO      PIC 9(03).
               10 TAB-VALOR-UNIT        PIC 9(05)V99.
               10 TAB-VALOR-TOTAL       PIC 9(05)V99.

       01  INDICE-LEITURA              PIC 9(02) VALUE 1.

       LINKAGE SECTION.
       01  WRK-ID-PEDIDO               PIC 9(05) VALUE ZEROS.

      *================================================================*
       PROCEDURE DIVISION              USING WRK-ID-PEDIDO.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQUIVO-ENT

           PERFORM 0001-LER-ARQSEQ  UNTIL WRK-FIM-ARQUIVO = "S"

           CLOSE ARQUIVO-ENT

           PERFORM 0002-CLASS-REG-CODIGO

           PERFORM 9999-FINALIZAR  
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FAZER CADASTRO USUARIO
      *----------------------------------------------------------------*
       0001-LER-ARQSEQ                 SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENT
               AT END MOVE "S"         TO WRK-FIM-ARQUIVO 
           END-READ
           IF WRK-FIM-ARQUIVO EQUAL 'N'
               MOVE FDE-REG-ID-PEDIDO
                                    TO TAB-ID-PEDIDO   (INDICE-LEITURA)   
               MOVE FDE-REG-ID-CLIENTE
                                    TO TAB-ID-CLIENTE  (INDICE-LEITURA)
               MOVE FDE-REG-NOME-CLIENTE
                                    TO TAB-NOME-CLIENTE (INDICE-LEITURA)
               MOVE FDE-REG-ID-PRODUTO
                                    TO TAB-ID-PRODUTO   (INDICE-LEITURA)   
               MOVE FDE-REG-NOME-PRODUTO
                                    TO TAB-NOME-PRODUTO (INDICE-LEITURA)
               MOVE FDE-REG-VALOR-UNIT
                                    TO TAB-VALOR-UNIT   (INDICE-LEITURA)
               MOVE FDE-REG-VALOR-TOTAL
                                    TO TAB-VALOR-TOTAL  (INDICE-LEITURA)

               ADD 1                   TO INDICE-LEITURA
               ADD 1                   TO WRK-QTDE-REG
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CLASSIFICAÇÃO DE REGISTROS - CODIGO DO PRODUTO
      *----------------------------------------------------------------*
       0002-CLASS-REG-CODIGO           SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 
                                       UNTIL WRK-IND1 > WRK-QTDE-REG
               PERFORM VARYING WRK-IND2 FROM 1 BY 1 UNTIL 
                                    WRK-IND2 > WRK-QTDE-REG - WRK-IND1
                   IF TAB-ID-PEDIDO (WRK-IND2) 
                                       > TAB-ID-PEDIDO (WRK-IND2 + 1)
                       MOVE TAB-ID-PEDIDO (WRK-IND2)
                                       TO WRK-ID-PEDIDO-AUX 
                       MOVE TAB-ID-PEDIDO  (WRK-IND2 + 1)
                                       TO TAB-ID-PEDIDO (WRK-IND2)
                       MOVE WRK-ID-PEDIDO-AUX 
                                       TO TAB-ID-PEDIDO(WRK-IND2 + 1)
                   END-IF
               END-PERFORM

           MOVE TAB-ID-PEDIDO(WRK-IND1)
                                       TO WRK-ID-PEDIDO
           ADD 1                       TO WRK-ID-PEDIDO
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-finalizar
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'FIM DE PROGRAMA'

           GOBACK
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
      