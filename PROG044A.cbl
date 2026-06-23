      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 25/05/2026
      * DESCRICAO..: LISTAR PEDIDOS - MODULO LEITURA DE ARQUIVO
      * NOME.......: PROG044A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG044A. 
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
       01  WRK-IND1                       PIC 9(05) VALUE ZEROS.           

       01  WRK-ID-PEDIDO                  PIC 9(05) VALUE ZEROS.
       01  WRK-NOME-CLIENTE               PIC X(30) VALUE SPACES.
       01  WRK-NOME-PRODUTO               PIC X(30) VALUE SPACES.
       01  WRK-QTDE-PRODUTO               PIC 9(03) VALUE ZEROS.
       01  WRK-VALOR-TOTAL                PIC ZZ.ZZZ,99 VALUE ZEROS.
       
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQUIVO-ENT

           PERFORM 0001-LER-ARQSEQ 

           PERFORM 0002-MONTAR-CABECALHO

           PERFORM 0003-LISTAR-PEDIDOS UNTIL WRK-FIM-ARQUIVO EQUAL 'S'

           IF WRK-FIM-ARQUIVO          EQUAL 'S' AND
              WRK-IND1                 EQUAL ZEROS
              DISPLAY  '     ARQUIVO VAZIO     '
           ELSE
              DISPLAY 'QTDE. REGISTROS: ' WRK-IND1
           END-IF 

           PERFORM 9999-FINALIZAR  
           . 
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DE ARQUIVO SEQUENCIAL
      *----------------------------------------------------------------*
       0001-LER-ARQSEQ                 SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENT
               AT END 
               MOVE "S"                TO WRK-FIM-ARQUIVO 
               NOT AT END

               MOVE FDE-REG-ID-PEDIDO  TO WRK-ID-PEDIDO    
               MOVE FDE-REG-NOME-CLIENTE
                                       TO WRK-NOME-CLIENTE 
               MOVE FDE-REG-NOME-PRODUTO
                                       TO WRK-NOME-PRODUTO 
               MOVE FDE-REG-QTDE-PRODUTO 
                                       TO WRK-QTDE-PRODUTO 
               MOVE FDE-REG-VALOR-TOTAL
                                       TO WRK-VALOR-TOTAL  
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CABECALHO DE LISTAGEM
      *----------------------------------------------------------------*
       0002-MONTAR-CABECALHO           SECTION.
      *----------------------------------------------------------------*

           DISPLAY '****************************************************
      -    '*****************************' 
           DISPLAY '*                                  LISTAR PEDIDOS   
      -    '                            *'  
           DISPLAY '*---------------------------------------------------
      -    '----------------------------*'
           DISPLAY 'COD.  CLIENTE                        PRODUTO       
      -    '                QTD VLR TOTAL' 
           DISPLAY '----- ------------------------------ ---------------
      -    '--------------- --- ---------'
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LISTAR PEDIDOS
      *----------------------------------------------------------------*
       0003-LISTAR-PEDIDOS             SECTION.
      *----------------------------------------------------------------*

           DISPLAY WRK-ID-PEDIDO ' ' WRK-NOME-CLIENTE ' '
            WRK-NOME-PRODUTO ' ' WRK-QTDE-PRODUTO ' ' WRK-VALOR-TOTAL

           ADD 1                       TO WRK-IND1
           PERFORM 0001-LER-ARQSEQ 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-finalizar
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQUIVO-ENT

           DISPLAY 'FIM DE PROGRAMA'
           
           STOP RUN
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
