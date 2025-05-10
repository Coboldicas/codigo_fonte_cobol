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

<<<<<<< HEAD
=======
           SELECT ARQ-SAIDA-CLASS ASSIGN TO 'SEQSAI05.dat'.


>>>>>>> 718268a (Primeiro commit)
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

<<<<<<< HEAD
=======
       SD ARQ-SAIDA-CLASS.
       01   WRK-SEQSAICLS-REGISTRO PIC X(100).

>>>>>>> 718268a (Primeiro commit)
       WORKING-STORAGE SECTION.
       01  WRK-IND1                PIC 9(02).
       01  WRK-FIM-ARQUIVO         PIC X(01) VALUE 'N'.

<<<<<<< HEAD
=======

       01   WRK-TABELA-INTERNA.
         05 WRK-TABELA-REGISTROS  OCCURS 20 TIMES.
           10  WRK-TABELA-CODIGO      PIC 9(05).
           10 FILLER                  PIC X(01).
           10 WRK-TABELA-NOME         PIC X(15).
           10 FILLER                  PIC X(01).
           10 WRK-TABELA-IDADE        PIC 9(02).
           10 FILLER                  PIC X(01).
           10 WRK-TABELA-PROFISSAO    PIC X(15).
           10 FILLER                  PIC X(01).
           10 WRK-TABELA-CIDADE       PIC X(15).
           10 FILLER                  PIC X(01).
           10 WRK-TABELA-ESTADO       PIC X(02).



>>>>>>> 718268a (Primeiro commit)
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
<<<<<<< HEAD
            PERFORM 0002-LEITURA-ARQUIVO
            PERFORM 0003-LEITURA-REGISTRO UNTIL WRK-FIM-ARQUIVO = 'S'
            PERFORM 0099-FECHAMENTO-ARQUIVO
=======
      *     PERFORM 0002-LEITURA-ARQUIVO
      *     PERFORM 0005-CLASSIFICAR-REG UNTIL WRK-FIM-ARQUIVO = 'S'
            PERFORM 0005-CLASSIFICAR-REG
      *     PERFORM 0003-LEITURA-REGISTRO
      *     PERFORM 0003-LEITURA-REGISTRO UNTIL WRK-FIM-ARQUIVO = 'S'
      *     PERFORM 0099-FECHAMENTO-ARQUIVO
>>>>>>> 718268a (Primeiro commit)
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

<<<<<<< HEAD
=======
           display '0001-ABERTURA-ARQUIVO'
>>>>>>> 718268a (Primeiro commit)
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

<<<<<<< HEAD
=======
           DISPLAY '0002-LEITURA-ARQUIVO'
>>>>>>> 718268a (Primeiro commit)
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

<<<<<<< HEAD
=======
      *    PERFORM 0005-CLASSIFICAR-REG 
>>>>>>> 718268a (Primeiro commit)
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
<<<<<<< HEAD
           
=======
           DISPLAY '0004-GRAVAR-ARQUIVO'
      *    MOVE WRK-SEQSAICLS-REGISTRO TO WRK-TABELA-INTERNA
      *    DISPLAY WRK-TABELA-INTERNA

>>>>>>> 718268a (Primeiro commit)
           MOVE WRK-SEQENT02-REGISTRO   TO WRK-SEQSAI03-REGISTRO 
           WRITE WRK-SEQSAI03-REGISTRO 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

<<<<<<< HEAD
=======
      *----------------------------------------------------------------*
      *    CLASSIFICACAO DE REGISTRO
      *----------------------------------------------------------------*
       0005-CLASSIFICAR-REG             SECTION.
      *----------------------------------------------------------------*

           display '0005-CLASSIFICAR-REG'

           DISPLAY 'WRK-FIM-ARQUIVO: ' WRK-FIM-ARQUIVO      
           SORT ARQ-SAIDA-CLASS ON ASCENDING KEY WRK-SEQENT02-NOME
      *    INPUT PROCEDURE 0002-LEITURA-ARQUIVO
      *    OUTPUT PROCEDURE 0004-GRAVAR-ARQUIVO
           USING ARQUIVO-ENTRADA
           GIVING ARQUIVO-SAIDA
      *    OUTPUT PROCEDURE 0004-GRAVAR-ARQUIVO

      *    DISPLAY 'REGISTRO TEMP: ' WRK-SEQSAICLS-REGISTRO
      *     MOVE WRK-SEQSAI03-REGISTRO TO WRK-TABELA-INTERNA
      *     DISPLAY WRK-TABELA-INTERNA
      *    PERFORM 0002-LEITURA-ARQUIVO
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

>>>>>>> 718268a (Primeiro commit)

      *----------------------------------------------------------------*
      *    FECHAMENTO DE ARQUIVO
      *----------------------------------------------------------------*
       0099-FECHAMENTO-ARQUIVO         SECTION.
      *----------------------------------------------------------------*

<<<<<<< HEAD
           CLOSE ARQUIVO-ENTRADA
                 ARQUIVO-SAIDA
=======
           display '0099-FECHAMENTO-ARQUIVO'

           CLOSE ARQUIVO-ENTRADA
           close ARQUIVO-SAIDA
           
>>>>>>> 718268a (Primeiro commit)
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
