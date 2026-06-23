      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 24/05/2026
      * DESCRICAO..: CONSULTA DE PRODUTOS
      * NOME.......: PROG043A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG043A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT1 ASSIGN TO 'PRODCONS01.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQUIVO-ENT2 ASSIGN TO 'CADASTRO2.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQUIVO-SAI  ASSIGN TO 'PRODRET01.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT1.
       01  FD-ARQ-ENT1.
         05  FDE1-REG-ID-PRODUTO          PIC 9(05) VALUE ZEROS.

       FD  ARQUIVO-ENT2.
       01  FD-ARQ-ENT2.
         05  FDE2-REG-ID-PRODUTO          PIC 9(05) VALUE ZEROS.
         05  FDE2-REG-NOME-PRODUTO        PIC X(30) VALUE SPACES.
         05  FDE2-REG-VALOR               PIC 9(05)V99 VALUE ZEROS.

       FD  ARQUIVO-SAI.
       01  FD-ARQ-SAI.
         05  FDS-REG-ID-PRODUTO           PIC 9(05) VALUE ZEROS.
         05  FDS-REG-NOME-PRODUTO         PIC X(30) VALUE SPACES.
         05  FDS-REG-VALOR                PIC 9(05)V99 VALUE ZEROS.

       WORKING-STORAGE SECTION.
       01 TABELA-PRODUTOS.
           05 DADOS-PRODUTOS     OCCURS 50 TIMES.
               10 TAB-ID-PRODUTO          PIC 9(05).
               10 TAB-NOME-PRODUTO        PIC X(30).
               10 TAB-VALOR               PIC 9(05)V99.
       
       01  WRK-FIM-ARQUIVO1               PIC X(01) VALUE 'N'.
       01  WRK-FIM-ARQUIVO2               PIC X(01) VALUE 'N'.
       01  WRK-QTDE-REG                   PIC 9(05) VALUE ZEROS.           
       01  WRK-IND1                       PIC 9(05) VALUE ZEROS.           
       01  WRK-ENCONTRADO                 PIC X(01) VALUE 'N'.           
       01  INDICE-LEITURA                 PIC 9(02) VALUE 1.

       01  WRK-ID-PRODUTO                 PIC 9(05) VALUE ZEROS.
       01  WRK-NOME-PRODUTO               PIC X(30) VALUE SPACES.
       01  WRK-VALOR                      PIC 9(05)V99 VALUE ZEROS.

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

           PERFORM 0011-LER-ARQSEQ UNTIL WRK-FIM-ARQUIVO1 EQUAL 'S'

           CLOSE ARQUIVO-ENT1

           OPEN INPUT ARQUIVO-ENT2

           PERFORM 0001-LER-ARQSEQ UNTIL WRK-FIM-ARQUIVO2 EQUAL 'S'

           CLOSE ARQUIVO-ENT2

           PERFORM 0002-CONSULTAR-REGISTRO 

           PERFORM 0005-GRAVAR-ARQUIVO 

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
 
           READ ARQUIVO-ENT2
             AT END 
                  MOVE "S"             TO WRK-FIM-ARQUIVO2 
             NOT AT END  
               MOVE FDE2-REG-ID-PRODUTO
                                    TO TAB-ID-PRODUTO   (INDICE-LEITURA)   
               MOVE FDE2-REG-NOME-PRODUTO
                                    TO TAB-NOME-PRODUTO (INDICE-LEITURA)
               MOVE FDE2-REG-VALOR  TO TAB-VALOR        (INDICE-LEITURA)

               ADD 1                   TO INDICE-LEITURA
               ADD 1                   TO WRK-QTDE-REG
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LER ARQUIVO ENTRADA1
      *----------------------------------------------------------------*
       0011-LER-ARQSEQ                 SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENT1
             AT END 
                  MOVE "S"             TO WRK-FIM-ARQUIVO1
             NOT AT END  
               MOVE FDE1-REG-ID-PRODUTO
                                       TO WRK-ID-PRODUTO
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0011-end
       0011-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSULTAR REGISTROS PARA ALTERAÇÃO 
      *----------------------------------------------------------------*
       0002-CONSULTAR-REGISTRO         SECTION.
      *----------------------------------------------------------------*

      *    BUSCAR CODIGO DO PRODUTO
           PERFORM VARYING WRK-IND1    FROM 1 BY 1 
                                       UNTIL WRK-IND1 > WRK-QTDE-REG
                                       OR WRK-ENCONTRADO EQUAL 'S'
              IF WRK-ID-PRODUTO        EQUAL TAB-ID-PRODUTO (WRK-IND1)
                 MOVE TAB-NOME-PRODUTO (WRK-IND1)
                                       TO WRK-NOME-PRODUTO
                 MOVE TAB-VALOR (WRK-IND1) 
                                       TO WRK-VALOR                                    
                 MOVE 'S'              TO WRK-ENCONTRADO 
               END-IF 
           END-PERFORM 

           SUBTRACT 1                  FROM WRK-IND1
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    DESCARREGAR TABELA INTERNA E REGRAVAR ARQUIVO DE ENTRADA
      *----------------------------------------------------------------*
       0005-GRAVAR-ARQUIVO             SECTION.
      *----------------------------------------------------------------*

           OPEN OUTPUT ARQUIVO-SAI

             MOVE WRK-ID-PRODUTO       TO FDS-REG-ID-PRODUTO
             MOVE WRK-NOME-PRODUTO     TO FDS-REG-NOME-PRODUTO 
             MOVE WRK-VALOR            TO FDS-REG-VALOR 

             WRITE FD-ARQ-SAI

           CLOSE ARQUIVO-SAI
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
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
