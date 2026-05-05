      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 04/05/2026
      * DESCRICAO..: CADASTRAR PRODUTOS - MODULO CADASTRO DE PRODUTOS
      * NOME.......: PROG040A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG040A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-SAI ASSIGN TO 'CADASTRO2.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-SAI.
       01  FD-ARQ-SAI.
         05  FDS-REG-ID-PRODUTO           PIC 9(05) VALUE ZEROS.
         05  FDS-REG-NOME-PRODUTO         PIC X(30) VALUE SPACES.
         05  FDS-REG-VALOR                PIC 9(05)V99 VALUE ZEROS.

       WORKING-STORAGE SECTION.
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

           OPEN EXTEND ARQUIVO-SAI

           PERFORM 0001-TELA-CADASTRO
           PERFORM 0002-CADASTRAR-PRODUTO
           PERFORM 9999-FINALIZAR  
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    RECEBER DADOS DO PRODUTO - DA TELA
      *----------------------------------------------------------------*
       0001-TELA-CADASTRO              SECTION.
      *----------------------------------------------------------------*
 
           DISPLAY '**------------------------------------**' 
           DISPLAY '**     CADASTRO DE PRODUTOS - TELA    **'
           DISPLAY '**------------------------------------**'
           PERFORM 0003-VALIDAR-ID-PRODUTO 
           PERFORM 0004-VALIDAR-NOME-PRODUTO
           DISPLAY 'VALOR DO PRODUTO: ' WITH NO ADVANCING
           ACCEPT WRK-VALOR           
           DISPLAY '**------------------------------------**'
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CADASTRO DE PRODUTOS
      *----------------------------------------------------------------*
       0002-CADASTRAR-PRODUTO          SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-ID-PRODUTO         TO FDS-REG-ID-PRODUTO

           MOVE WRK-NOME-PRODUTO       TO FDS-REG-NOME-PRODUTO 
           
           MOVE WRK-VALOR              TO FDS-REG-VALOR 

           WRITE FD-ARQ-SAI
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR ID DO PRODUTO
      *----------------------------------------------------------------*
       0003-VALIDAR-ID-PRODUTO         SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'ID DO PRODUTO: ' WITH NO ADVANCING
           ACCEPT WRK-ID-PRODUTO

              IF WRK-ID-PRODUTO EQUAL ZERO
                  DISPLAY 'ID DO PRODUTO NAO PODE SER ZERO'
                  GO TO 0003-VALIDAR-ID-PRODUTO
              END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR NOME DO PRODUTO
      *----------------------------------------------------------------*
       0004-VALIDAR-NOME-PRODUTO       SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'NOME DO PRODUTO: ' WITH NO ADVANCING
           ACCEPT WRK-NOME-PRODUTO

              IF WRK-NOME-PRODUTO EQUAL SPACES
                  DISPLAY 'NOME DO PRODUTO NAO PODE SER VAZIO'
                  GO TO 0004-VALIDAR-NOME-PRODUTO
              END-IF
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

           CLOSE ARQUIVO-SAI
                 
           DISPLAY 'FIM DE PROGRAMA'

           STOP RUN
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
      