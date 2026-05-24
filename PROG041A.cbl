      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 11/05/2026
      * DESCRICAO..: LISTAR PRODUTOS - MODULO LEITURA DE ARQUIVO
      * NOME.......: PROG041A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG041A. 
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
       01  WRK-FIM-ARQUIVO                PIC X(01) VALUE 'N'.
       01  WRK-IND1                       PIC 9(05) VALUE ZEROS.           
       01  WRK-ID-PRODUTO                 PIC 9(05) VALUE ZEROS.
       01  WRK-NOME-PRODUTO               PIC X(30) VALUE SPACES.
       01  WRK-VALOR                      PIC ZZ.ZZZ,99 VALUE ZEROS.
       
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQUIVO-SAI

           PERFORM 0001-LER-ARQSEQ 

           PERFORM 0002-LISTAR-PRODUTO UNTIL WRK-FIM-ARQUIVO EQUAL 'S'

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
 
           READ ARQUIVO-SAI
               AT END 
               MOVE "S"                TO WRK-FIM-ARQUIVO 
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LISTAR PRODUTOS
      *----------------------------------------------------------------*
       0002-LISTAR-PRODUTO             SECTION.
      *----------------------------------------------------------------*

           MOVE FDS-REG-ID-PRODUTO     TO WRK-ID-PRODUTO  
           MOVE FDS-REG-NOME-PRODUTO   TO WRK-NOME-PRODUTO
           MOVE FDS-REG-VALOR          TO WRK-VALOR        
           
      *    DISPLAY 'ID: ' WRK-ID-PRODUTO
      *    DISPLAY 'NOME: ' WRK-NOME-PRODUTO
      *    DISPLAY 'VALOR: ' WRK-VALOR

           ADD 1                       TO WRK-IND1
           PERFORM 0001-LER-ARQSEQ 
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

           CLOSE ARQUIVO-SAI

           DISPLAY 'FIM DE PROGRAMA'
           
           STOP RUN
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
