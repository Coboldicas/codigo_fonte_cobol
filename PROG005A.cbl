       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG005A.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENTRADA ASSIGN TO "SEQENT02.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.

       FD  ARQUIVO-ENTRADA.
       01 REGISTRO.
          05 COD          PIC 9(05).
          05 FILLER       PIC X(01).
          05 NOME         PIC X(15).
          05 FILLER       PIC X(01).
          05 IDADE        PIC 9(02).
          05 FILLER       PIC X(01).
          05 PROFISSAO    PIC X(15).
          05 FILLER       PIC X(01).
          05 CIDADE       PIC X(15).
          05 FILLER       PIC X(01).
          05 ESTADO       PIC X(02).

       WORKING-STORAGE SECTION.
       01 FIM-ARQUIVO    PIC X VALUE "N".
       01 WRK-NOME           PIC X(15).
      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0001-ABRIR-ARQUIVO
           PERFORM 0002-LEITURA-REGISTRO UNTIL FIM-ARQUIVO = "S"
           PERFORM 0008-FECHAR-ARQUIVO
           PERFORM 9999-FINALIZAR
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ABRE ARQUIVO SEQUENCIAL
      *----------------------------------------------------------------*
       0001-ABRIR-ARQUIVO              SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQUIVO-ENTRADA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA ARQUIVO SEQUENCIAL
      *----------------------------------------------------------------*
       0002-LEITURA-REGISTRO           SECTION.
      *----------------------------------------------------------------*

           READ ARQUIVO-ENTRADA INTO REGISTRO
               AT END MOVE "S" TO FIM-ARQUIVO
           END-READ.
           
           MOVE NOME TO WRK-NOME
           DISPLAY 'WRK-NOME: ' WRK-NOME

           initialize WRK-NOME

           DISPLAY 'WRK-NOME: ' WRK-NOME
           IF FIM-ARQUIVO = "N"
              DISPLAY "COD: " COD ", NOME: " NOME ", IDADE: " IDADE ", P
      -    "ROFISSAO: " PROFISSAO ", CIDADE: " CIDADE ", ESTADO: " 
      -    ESTADO           
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FECHAR ARQUIVO SEQUENCIAL
      *----------------------------------------------------------------*
       0008-FECHAR-ARQUIVO             SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQUIVO-ENTRADA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0008-end
       0008-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

            DISPLAY "FIM DO PROGRAMA"
            STOP RUN
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*