      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 16/06/2026
      * DESCRICAO..: DETALHAR PEDIDOS 
      * NOME.......: PROG045A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG045A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT1 ASSIGN TO 'PEDDET01.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQUIVO-ENT2 ASSIGN TO 'CADASTRO3.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQUIVO-SAI  ASSIGN TO 'CADASTRO4.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT1.
       01  FD-ARQ-ENT1.
         05  FDE1-REG-ID-PEDIDO           PIC 9(05) VALUE ZEROS.

       FD  ARQUIVO-ENT2.
       01  FD-ARQ-ENT2.
         05  FDE2-REG-ID-PEDIDO           PIC 9(05) VALUE ZEROS.
         05  FDE2-REG-ID-CLIENTE          PIC 9(05) VALUE ZEROS.
         05  FDE2-REG-NOME-CLIENTE        PIC X(30) VALUE SPACES.
         05  FDE2-REG-ID-PRODUTO          PIC 9(05) VALUE ZEROS.
         05  FDE2-REG-NOME-PRODUTO        PIC X(30) VALUE SPACES.
         05  FDE2-REG-QTDE-PRODUTO        PIC 9(03) VALUE ZEROS.
         05  FDE2-REG-VALOR-UNIT          PIC 9(05)V99 VALUE ZEROS.
         05  FDE2-REG-VALOR-TOTAL         PIC 9(05)V99 VALUE ZEROS.

       FD  ARQUIVO-SAI.
       01  FD-ARQ-SAI.
         05  FDS-REG-ID-PEDIDO            PIC 9(05) VALUE ZEROS.
         05  FDS-REG-ID-CLIENTE           PIC 9(05) VALUE ZEROS.
         05  FDS-REG-NOME-CLIENTE         PIC X(30) VALUE SPACES.
         05  FDS-REG-ID-PRODUTO           PIC 9(05) VALUE ZEROS.
         05  FDS-REG-NOME-PRODUTO         PIC X(30) VALUE SPACES.
         05  FDS-REG-QTDE-PRODUTO         PIC 9(03) VALUE ZEROS.
         05  FDS-REG-VALOR-UNIT           PIC 9(05)V99 VALUE ZEROS.
         05  FDS-REG-VALOR-TOTAL          PIC 9(05)V99 VALUE ZEROS.

       WORKING-STORAGE SECTION.
       01  WRK-FIM-ARQUIVO1               PIC X(01) VALUE 'N'.

       01  WRK-FIM-ARQUIVO2               PIC X(01) VALUE 'N'.
       01  WRK-IND1                       PIC 9(05) VALUE ZEROS.           
       01  INDICE-LEITURA                 PIC 9(02) VALUE 1.
       01  WRK-QTDE-REG                   PIC 9(05) VALUE ZEROS.           
       01  WRK-ENCONTRADO                 PIC X(01) VALUE 'N'.           

       01  WRK-ID-PEDIDO                  PIC 9(05) VALUE ZEROS.

       01 TABELA-PEDIDOS.
           05 DADOS-PEDIDOS     OCCURS 50 TIMES.
               10 TAB-REG-ID-PEDIDO       PIC 9(05).
               10 TAB-REG-ID-CLIENTE      PIC 9(05).
               10 TAB-REG-NOME-CLIENTE    PIC X(30).
               10 TAB-REG-ID-PRODUTO      PIC 9(05).
               10 TAB-REG-NOME-PRODUTO    PIC X(30).
               10 TAB-REG-QTDE-PRODUTO    PIC 9(03).
               10 TAB-REG-VALOR-UNIT      PIC 9(05)V99.
               10 TAB-REG-VALOR-TOTAL     PIC 9(05)V99.


      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQUIVO-ENT1
                      ARQUIVO-ENT2

           PERFORM 0001-LER-ARQSEQ UNTIL WRK-FIM-ARQUIVO1 EQUAL 'S'
           PERFORM 0002-LER-ARQSEQ UNTIL WRK-FIM-ARQUIVO2 EQUAL 'S'

           CLOSE ARQUIVO-ENT1
                 ARQUIVO-ENT2

           PERFORM 0003-PESQUISAR-PEDIDO

           PERFORM 9999-FINALIZAR  
           . 
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LER ARQUIVO ID DO PEDIDO
      *----------------------------------------------------------------*
       0001-LER-ARQSEQ                 SECTION.
      *----------------------------------------------------------------*

           READ ARQUIVO-ENT1
             AT END 
                  MOVE "S"             TO WRK-FIM-ARQUIVO1
             NOT AT END  
               MOVE FDE1-REG-ID-PEDIDO TO WRK-ID-PEDIDO
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA E CARREGAMENTO DE TABELA INTERNA 
      *----------------------------------------------------------------*
       0002-LER-ARQSEQ                 SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENT2
             AT END 
                  MOVE "S"             TO WRK-FIM-ARQUIVO2
             NOT AT END  
               MOVE FDE2-REG-ID-PEDIDO TO TAB-REG-ID-PEDIDO  
                                                       (INDICE-LEITURA)
               MOVE FDE2-REG-ID-CLIENTE
                                       TO TAB-REG-ID-CLIENTE
                                                       (INDICE-LEITURA)
               MOVE FDE2-REG-NOME-CLIENTE
                                       TO TAB-REG-NOME-CLIENTE
                                                       (INDICE-LEITURA)
               MOVE FDE2-REG-ID-PRODUTO
                                       TO TAB-REG-ID-PRODUTO
                                                       (INDICE-LEITURA)
               MOVE FDE2-REG-NOME-PRODUTO
                                       TO TAB-REG-NOME-PRODUTO
                                                       (INDICE-LEITURA)
               MOVE FDE2-REG-QTDE-PRODUTO
                                       TO TAB-REG-QTDE-PRODUTO
                                                       (INDICE-LEITURA)
               MOVE FDE2-REG-VALOR-UNIT
                                       TO TAB-REG-VALOR-UNIT
                                                       (INDICE-LEITURA)
               MOVE FDE2-REG-VALOR-TOTAL
                                       TO TAB-REG-VALOR-TOTAL
                                                       (INDICE-LEITURA)
               ADD 1                   TO INDICE-LEITURA
               ADD 1                   TO WRK-QTDE-REG
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSULTAR PEDIDO NA TABELA INTERNA
      *----------------------------------------------------------------*
       0003-PESQUISAR-PEDIDO           SECTION.
      *----------------------------------------------------------------*
      *    BUSCAR CODIGO DO PEDIDO
           PERFORM VARYING WRK-IND1    FROM 1 BY 1 
                                       UNTIL WRK-IND1 > WRK-QTDE-REG
                                       OR WRK-ENCONTRADO EQUAL 'S'
              IF WRK-ID-PEDIDO         EQUAL TAB-REG-ID-PEDIDO(WRK-IND1)
                 PERFORM 0004-GRAVAR-ARQUIVO
                 MOVE 'S'              TO WRK-ENCONTRADO 
               END-IF 
           END-PERFORM 

           SUBTRACT 1                  FROM WRK-IND1
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO DE SAIDA COM DETALHES DO PEDIDO
      *----------------------------------------------------------------*
       0004-GRAVAR-ARQUIVO             SECTION.
      *----------------------------------------------------------------*

           OPEN OUTPUT ARQUIVO-SAI

                MOVE WRK-ID-PEDIDO     TO FDS-REG-ID-PEDIDO
                MOVE TAB-REG-ID-CLIENTE  (WRK-IND1) 
                                       TO FDS-REG-ID-CLIENTE
                MOVE TAB-REG-NOME-CLIENTE(WRK-IND1)
                                       TO FDS-REG-NOME-CLIENTE
                MOVE TAB-REG-ID-PRODUTO  (WRK-IND1)
                                       TO FDS-REG-ID-PRODUTO
                MOVE TAB-REG-NOME-PRODUTO(WRK-IND1)
                                       TO FDS-REG-NOME-PRODUTO
                MOVE TAB-REG-QTDE-PRODUTO(WRK-IND1)
                                       TO FDS-REG-QTDE-PRODUTO
                MOVE TAB-REG-VALOR-UNIT  (WRK-IND1)
                                       TO FDS-REG-VALOR-UNIT
                MOVE TAB-REG-VALOR-TOTAL (WRK-IND1)
                                       TO FDS-REG-VALOR-TOTAL
             WRITE FD-ARQ-SAI

           CLOSE ARQUIVO-SAI
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-finalizar
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'FIM DE PROGRAMA'
           STOP RUN
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
