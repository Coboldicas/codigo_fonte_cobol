      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 18/05/2026
      * DESCRICAO..: CADASTRO DE PEDIDOS 
      * NOME.......: PROG042A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG042A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  
                                         
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT1 ASSIGN TO 'CADASTRO1.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQUIVO-ENT2 ASSIGN TO 'CADASTRO2.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQUIVO-ENT3 ASSIGN TO 'PEDENT01.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQUIVO-SAI ASSIGN TO 'CADASTRO3.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT1.
       01  FD-ARQ-ENT1.
         05  FDE1-REG-ID-CLIENTE          PIC 9(05) VALUE ZEROS.
         05  FDE1-REG-NOME-CLIENTE        PIC X(30) VALUE SPACES.
         05  FDE1-REG-IDADE               PIC 9(03) VALUE ZEROS.

       FD  ARQUIVO-ENT2.
       01  FD-ARQ-ENT2.
         05  FDE2-REG-ID-PRODUTO           PIC 9(05) VALUE ZEROS.
         05  FDE2-REG-NOME-PRODUTO         PIC X(30) VALUE SPACES.
         05  FDE2-REG-VALOR                PIC 9(05)V99 VALUE ZEROS.

       FD  ARQUIVO-ENT3.
       01  FD-ARQ-ENT3.
         05  FDE3-REG-ID-PEDIDO            PIC 9(05) VALUE ZEROS.
         05  FDE3-REG-ID-CLIENTE           PIC 9(05) VALUE ZEROS.
         05  FDE3-REG-ID-PRODUTO           PIC 9(05) VALUE ZEROS.
         05  FDE3-REG-QUANTIDADE           PIC 9(03) VALUE ZEROS.

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
       01  WRK-FIM-ARQUIVO3               PIC X(01) VALUE 'N'.
       01  WRK-IND1                       PIC 9(05) VALUE ZEROS.           
       01  WRK-IND2                       PIC 9(05) VALUE ZEROS.           
       01  WRK-IND3                       PIC 9(05) VALUE ZEROS.                  
       01  INDICE-LEITURA1                PIC 9(02) VALUE 1.
       01  INDICE-LEITURA2                PIC 9(02) VALUE 1.
       01  WRK-ACHOU-CLIENTE              PIC X(01) VALUE 'N'.
       01  WRK-ACHOU-PRODUTO              PIC X(01) VALUE 'N'.

       01  WRK-ID-PEDIDO                  PIC 9(05) VALUE ZEROS.
       01  WRK-ID-CLIENTE                 PIC 9(05) VALUE ZEROS.
       01  WRK-NOME-CLIENTE               PIC X(30) VALUE SPACES.
       01  WRK-QTDE-CLIENTE               PIC 9(05) VALUE ZEROS.                 
       01  WRK-ID-PRODUTO                 PIC 9(05) VALUE ZEROS.
       01  WRK-NOME-PRODUTO               PIC X(30) VALUE SPACES.
       01  WRK-QTDE-PRODUTO               PIC 9(03) VALUE ZEROS.
       01  WRK-VALOR-UNIT                 PIC 9(05)V99 VALUE ZEROS.
       01  WRK-VALOR-TOTAL                PIC 9(05)V99 VALUE ZEROS.
       01  WRK-QUANTIDADE                 PIC 9(05) VALUE ZEROS.                 

      *  Variável para armazenar o código de retorno das chamadas
       01  WRK-RETURN-CODE             PIC S9(4) COMP VALUE ZERO.


       01 TABELA-CLIENTES.
           05 DADOS-CLIENTES     OCCURS 50 TIMES.
               10 TAB-ID-CLIENTE          PIC 9(05).
               10 TAB-NOME-CLIENTE        PIC X(30).
               10 TAB-IDADE               PIC 9(03).

       01 TABELA-PRODUTOS.
           05 DADOS-PRODUTOS     OCCURS 50 TIMES.
               10 TAB-ID-PRODUTO          PIC 9(05).
               10 TAB-NOME-PRODUTO        PIC X(30).
               10 TAB-VALOR               PIC 9(05)V99.

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

           PERFORM 0001-LER-ARQSEQ-ENT1 UNTIL WRK-FIM-ARQUIVO1 EQUAL 'S'

           CLOSE ARQUIVO-ENT1

           OPEN INPUT ARQUIVO-ENT2

           PERFORM 0002-LER-ARQSEQ-ENT2 UNTIL WRK-FIM-ARQUIVO2 EQUAL 'S'

           CLOSE ARQUIVO-ENT2

           OPEN INPUT ARQUIVO-ENT3

           PERFORM 0003-LER-ARQSEQ-ENT3 UNTIL WRK-FIM-ARQUIVO3 EQUAL 'S'

           CLOSE ARQUIVO-ENT3

           PERFORM 0004-CADASTRAR-PEDIDOS

           PERFORM 9999-FINALIZAR  
           . 
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA ARQUIVO DE CLIENTES
      *----------------------------------------------------------------*
       0001-LER-ARQSEQ-ENT1            SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENT1
             AT END 
                  MOVE "S"             TO WRK-FIM-ARQUIVO1 
             NOT AT END  
               MOVE FDE1-REG-ID-CLIENTE
                                    TO TAB-ID-CLIENTE  (INDICE-LEITURA1)   
               MOVE FDE1-REG-NOME-CLIENTE
                                    TO TAB-NOME-CLIENTE(INDICE-LEITURA1)
               MOVE FDE1-REG-IDADE  TO TAB-IDADE       (INDICE-LEITURA1)

               ADD 1                   TO INDICE-LEITURA1
               ADD 1                   TO WRK-QTDE-CLIENTE
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA ARQUIVO DE PRODUTOS
      *----------------------------------------------------------------*
       0002-LER-ARQSEQ-ENT2            SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENT2
             AT END 
                  MOVE "S"             TO WRK-FIM-ARQUIVO2 
             NOT AT END  
               MOVE FDE2-REG-ID-PRODUTO
                                    TO TAB-ID-PRODUTO  (INDICE-LEITURA2)   
               MOVE FDE2-REG-NOME-PRODUTO
                                    TO TAB-NOME-PRODUTO(INDICE-LEITURA2)
               MOVE FDE2-REG-VALOR  TO TAB-VALOR       (INDICE-LEITURA2)

               ADD 1                   TO INDICE-LEITURA2
               ADD 1                   TO WRK-QTDE-PRODUTO
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA ARQUIVO DE PEDIDOS
       0003-LER-ARQSEQ-ENT3            SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENT3
             AT END 
                  MOVE "S"             TO WRK-FIM-ARQUIVO3 
             NOT AT END  
               MOVE FDE3-REG-ID-CLIENTE
                                       TO WRK-ID-CLIENTE
               MOVE FDE3-REG-ID-PRODUTO
                                       TO WRK-ID-PRODUTO
               MOVE FDE3-REG-QUANTIDADE
                                       TO WRK-QUANTIDADE
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CADASTRAR PEDIDOS
      *----------------------------------------------------------------*
       0004-CADASTRAR-PEDIDOS          SECTION.
      *----------------------------------------------------------------*
          
           MOVE FDE3-REG-ID-PEDIDO     TO WRK-ID-PEDIDO
           PERFORM 0005-INCLUIR-ID-CLIENTE
           PERFORM 0006-INCLUIR-ID-PRODUTO
           PERFORM 0007-INCLUIR-QUANTIDADE-PRODUTO

           IF  WRK-ACHOU-CLIENTE EQUAL 'S'
           AND WRK-ACHOU-PRODUTO EQUAL 'S'
           AND WRK-QTDE-PRODUTO GREATER ZERO
              COMPUTE WRK-VALOR-TOTAL =
                      WRK-QTDE-PRODUTO * WRK-VALOR-UNIT
              PERFORM 0010-OBTER-ULTIMO-CODIGO 
              PERFORM 0011-GRAVAR-ARQUIVO 
           ELSE
              DISPLAY 'NAO FOI POSSIVEL INCLUIR O PEDIDO.'
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------*
      *    INCLUIR ID DO CLIENTE
      *----------------------------------------------------------------*
       0005-INCLUIR-ID-CLIENTE         SECTION.
      *----------------------------------------------------------------*

           MOVE FDE3-REG-ID-CLIENTE    TO WRK-ID-CLIENTE
           PERFORM 0008-CONSULTAR-CLIENTE
           IF WRK-ACHOU-CLIENTE        EQUAL 'S'
              CONTINUE
           ELSE
              DISPLAY 'CLIENTE NAO ENCONTRADO.'
              PERFORM 9999-FINALIZAR
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------*
      *    INCLUIR ID DO PRODUTO
      *----------------------------------------------------------------*
       0006-INCLUIR-ID-PRODUTO          SECTION.
      *----------------------------------------------------------------*

           MOVE FDE3-REG-ID-PRODUTO     TO WRK-ID-PRODUTO
           PERFORM 0009-CONSULTAR-PRODUTO
           IF WRK-ACHOU-PRODUTO        EQUAL 'S'
              CONTINUE
           ELSE
              DISPLAY 'PRODUTO NAO ENCONTRADO.'
              PERFORM 9999-FINALIZAR
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-end
       0006-END.                       EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------*
      *    INCLUIR QUANTIDADE DO PRODUTO
      *----------------------------------------------------------------*
       0007-INCLUIR-QUANTIDADE-PRODUTO SECTION.
      *----------------------------------------------------------------*

           MOVE FDE3-REG-QUANTIDADE    TO WRK-QTDE-PRODUTO

           IF WRK-QTDE-PRODUTO         EQUAL ZERO
              DISPLAY 'QUANTIDADE INVALIDA.'
              PERFORM 9999-FINALIZAR
           ELSE
              CONTINUE 
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-end
       0007-END.                       EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------*
      *    CONSULTAR CLIENTE CADASTRADO
      *----------------------------------------------------------------*
       0008-CONSULTAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

      *    BUSCAR CODIGO DO CLIENTE
           PERFORM VARYING WRK-IND1    FROM 1 BY 1 
                                       UNTIL WRK-IND1 > WRK-QTDE-CLIENTE
                                       OR WRK-ACHOU-CLIENTE EQUAL 'S'
              IF WRK-ID-CLIENTE        EQUAL TAB-ID-CLIENTE (WRK-IND1)
                 MOVE TAB-NOME-CLIENTE (WRK-IND1)
                                       TO WRK-NOME-CLIENTE
                 MOVE 'S'              TO WRK-ACHOU-CLIENTE 
              END-IF 
           END-PERFORM 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0008-end
       0008-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSULTAR PRODUTO CADASTRADO
      *----------------------------------------------------------------*
       0009-CONSULTAR-PRODUTO          SECTION.
      *----------------------------------------------------------------*

      *    BUSCAR CODIGO DO PRODUTO
           PERFORM VARYING WRK-IND2    FROM 1 BY 1 
                                       UNTIL WRK-IND2 > WRK-QTDE-PRODUTO
                                       OR WRK-ACHOU-PRODUTO EQUAL 'S'
              IF WRK-ID-PRODUTO        EQUAL TAB-ID-PRODUTO (WRK-IND2)
                 MOVE TAB-NOME-PRODUTO (WRK-IND2)
                                       TO WRK-NOME-PRODUTO
                 MOVE TAB-VALOR(WRK-IND2)
                                       TO WRK-VALOR-UNIT
                 MOVE 'S'              TO WRK-ACHOU-PRODUTO 
              END-IF 
           END-PERFORM 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0009-end
       0009-END.                       EXIT.
      *----------------------------------------------------------------

      *----------------------------------------------------------------*
      *    BUSCAR ULTIMO CODIGO DO PEDIDO
      *----------------------------------------------------------------*
       0010-OBTER-ULTIMO-CODIGO        SECTION.
      *----------------------------------------------------------------*

            CALL 'PROG042B' USING WRK-ID-PEDIDO

            MOVE RETURN-CODE TO WRK-RETURN-CODE

            IF WRK-RETURN-CODE NOT = 0
               DISPLAY 'ERRO NA CHAMADA PROG042B. RETURN-CODE: '
                WRK-RETURN-CODE
               STOP RUN
            END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0010-end
       0010-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO DE PEDIDOS
      *----------------------------------------------------------------*
       0011-GRAVAR-ARQUIVO             SECTION.
      *----------------------------------------------------------------*

           OPEN EXTEND ARQUIVO-SAI
             MOVE WRK-ID-CLIENTE       TO FDS-REG-ID-CLIENTE
             MOVE WRK-NOME-CLIENTE     TO FDS-REG-NOME-CLIENTE 
             MOVE WRK-ID-PEDIDO        TO FDS-REG-ID-PEDIDO   
             MOVE WRK-ID-PRODUTO       TO FDS-REG-ID-PRODUTO
             MOVE WRK-NOME-PRODUTO     TO FDS-REG-NOME-PRODUTO
             MOVE WRK-QTDE-PRODUTO     TO FDS-REG-QTDE-PRODUTO
             MOVE WRK-VALOR-UNIT       TO FDS-REG-VALOR-UNIT  
             MOVE WRK-VALOR-TOTAL      TO FDS-REG-VALOR-TOTAL 
             ADD 1                     TO WRK-IND3

             WRITE FD-ARQ-SAI

           CLOSE ARQUIVO-SAI
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0011-end
       0011-END.                       EXIT.
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
