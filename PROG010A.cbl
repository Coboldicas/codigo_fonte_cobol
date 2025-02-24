      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOLDICAS
      * DATA: 17/02/2025
      * OBJETIVO: LEITURA E GRAVACAO DE ARQUIVOS SEQUENCIAS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG010A.
      *================================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT ARQUIVO-ENTRADA ASSIGN TO 'SEQENT02.dat'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQUIVO-SAIDA   ASSIGN TO 'SEQSAI03.dat'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO-ENTRADA.
       01   WRK-SEQENT02-REGISTRO.
         05 WRK-SEQENT02-CODIGO    PIC 9(05).
         05 FILLER                 PIC X(01).
         05 WRK-SEQENT02-NOME      PIC X(15).
         05 FILLER                 PIC X(01).

         05 WRK-SEQENT02-IDADE     PIC 9(02).
         05 FILLER                 PIC X(01).
         05 WRK-SEQENT02-PROFISSAO PIC X(15).
         05 FILLER                 PIC X(01).
         05 WRK-SEQENT02-CIDADE    PIC X(15).
         05 FILLER                 PIC X(01).
         05 WRK-SEQENT02-ESTADO    PIC X(02).

       FD ARQUIVO-SAIDA.
       01   WRK-SEQSAI03-REGISTRO  PIC X(100).

       WORKING-STORAGE SECTION.
       01  WRK-IND1                PIC 9(02).
       01  WRK-FIM-ARQUIVO         PIC X(01) VALUE 'N'.

      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
            PERFORM 0001-ABERTURA-ARQUIVO 
            PERFORM 0002-LEITURA-ARQUIVO
            PERFORM 0003-LEITURA-REGISTRO UNTIL WRK-FIM-ARQUIVO = 'S'
            PERFORM 0099-FECHAMENTO-ARQUIVO
            PERFORM 9999-FINALIZAR
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ABERTURA DE ARQUIVO
      *----------------------------------------------------------------*
       0001-ABERTURA-ARQUIVO           SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT  ARQUIVO-ENTRADA
                OUTPUT ARQUIVO-SAIDA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DE ARQUIVO
      *----------------------------------------------------------------*
       0002-LEITURA-ARQUIVO            SECTION.
      *----------------------------------------------------------------*

           READ ARQUIVO-ENTRADA INTO WRK-SEQENT02-REGISTRO
              AT END MOVE 'S' TO WRK-FIM-ARQUIVO
           END-READ 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DE REGISTROS
      *----------------------------------------------------------------*
       0003-LEITURA-REGISTRO            SECTION.
      *----------------------------------------------------------------*

           ADD 1                        TO WRK-IND1
           DISPLAY 'CODIGO: '   WRK-SEQENT02-CODIGO ','
      -           'NOME  : '    WRK-SEQENT02-NOME   ','
      -           'IDADE : '    WRK-SEQENT02-IDADE  ','
      -           'PROFISSAO: ' WRK-SEQENT02-PROFISSAO ','
      -           'CIDADE: '    WRK-SEQENT02-CIDADE ','
      -           'ESTADO: '    WRK-SEQENT02-ESTADO '.'

           PERFORM 0004-GRAVAR-ARQUIVO 
           PERFORM 0002-LEITURA-ARQUIVO
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVACAO DE ARQUIVO
      *----------------------------------------------------------------*
       0004-GRAVAR-ARQUIVO              SECTION.
      *----------------------------------------------------------------*
           
           MOVE WRK-SEQENT02-REGISTRO   TO WRK-SEQSAI03-REGISTRO 
           WRITE WRK-SEQSAI03-REGISTRO 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    FECHAMENTO DE ARQUIVO
      *----------------------------------------------------------------*
       0099-FECHAMENTO-ARQUIVO         SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQUIVO-ENTRADA
                 ARQUIVO-SAIDA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0099-end
       0099-END.                       EXIT.
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
