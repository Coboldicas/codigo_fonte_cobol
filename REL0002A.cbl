      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 09/06/2025
      * DESCRICAO..: RELATORIO COM TOTALIZADOR
      * NOME.......: REL0002A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REL0002A.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.                
           DECIMAL-POINT IS COMMA.  
          
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENTRADA ASSIGN TO 'CADASTRO3.dat'
           ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REL0002A-SAIDA ASSIGN TO 'REL0002A.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD ARQUIVO-ENTRADA.
       01 REG-ENTRADA.
           05 NOME                    PIC X(30).
           05 IDADE                   PIC 9(02).
           05 SALARIO                 PIC 9(05)V99.

       FD  REL0002A-SAIDA.
       01  WRK-REL0002A-REGISTRO.
        05 WRK-REL0002A-STRING        PIC X(100).

       WORKING-STORAGE SECTION.
       77 FIM-ARQ         PIC X VALUE 'N'.
       77 TOTAL-SALARIO   PIC 9(7)V99 VALUE ZEROS.
       77 CONTADOR        PIC 9(4) VALUE ZEROS.

       01  WRK-IND-LINHA           PIC 9(02) VALUE ZEROS.
       01  WRK-IND-PAGINACAO       PIC 9(02) VALUE 1.
       01  WRK-IND1                PIC 9(02) VALUE 1.

       01  WRK-MASCARA-SALARIO     PIC ZZZ.ZZZ.ZZZ,ZZ.
      *  MASCARA FORMATO DA DATA - DD/MM/AAAA
       01  WRK-MASC-DATA.
         05 WRK-MASC-DATA-DIA          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-MES          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-ANO          PIC 9(004) VALUE ZEROS.

      *  MASCARA FORMATO DA HORA - HH:MM
       01  WRK-MASC-TIME.
         05 WRK-MASC-HORA              PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE ':'.
         05 WRK-MASC-MIN               PIC 9(002) VALUE ZEROS.


       01  WS-LINHA-TRACO          PIC X(30) VALUE ALL "-".

       01  WRK-REL0002A-LINHA.
        03 FILLER                      PIC X(80) VALUE 
            '**---------------------------------------------------------
      -     '-------------------**'.
       01  WRK-REL0002A-LINHA-BRANCO   PIC X(80) VALUE SPACES.

       01  WRK-REL0002A-CAB1.
        03 FILLER                      PIC X(67) VALUE
            '** REL0002A                    COBOL DICAS                 
      -     '        '.
        03 WRK-REL0002A-CAB1-DATA      PIC X(10) VALUE SPACES.
        03 FILLER                      PIC X(03) VALUE ' **'. 

       01  WRK-REL0002A-CAB2.
        03 FILLER                      PIC X(08) VALUE
            '** PAG: '.
        03 WRK-REL0002A-PAG            PIC 9(02) VALUE ZEROS.
        03 FILLER                      PIC X(62) VALUE
            '                RELATORIO CADASTRO CLIENTE                 
      -     '   '.
        03 WRK-REL0002A-HORA           PIC X(05) VALUE SPACES.
        03 FILLER                      PIC X(03) VALUE ' **'.

       01  WRK-REL0002A-CAB3.
        03 FILLER                      PIC X(06) VALUE 'NOME  '.
        03 FILLER                      PIC X(25) VALUE SPACES.
        03 FILLER                      PIC X(10) VALUE '   IDADE  '.
        03 FILLER                      PIC X(12) VALUE '     SALARIO'.

       01  WRK-REL0002A-DET1.
        03 WRK-REL0002A-NOME           PIC X(30) VALUE SPACES.
        03 FILLER                      PIC X(07) VALUE SPACES.
        03 WRK-REL0002A-IDADE          PIC 9(02) VALUE ZEROS.
        03 FILLER                      PIC X(06) VALUE SPACES.
        03 WRK-REL0002A-SALARIO        PIC ZZ.ZZZ,ZZ.

       01  WRK-REL0002A-TOT1.
        03 FILLER                  PIC X(02) VALUE SPACES.
        03 FILLER                  PIC X(12) VALUE 'QTDE. REG.: '.
        03 WRK-REL0002A-QTD        PIC 9(02) VALUE ZEROS.
        03 FILLER                  PIC X(07) VALUE SPACES.
        03 FILLER                  PIC X(17) VALUE 'TOTAL SALARIO: R$'.
        03 WRK-REL0002A-TOT-SAL        PIC ZZZ.ZZZ.ZZZ,ZZ.

      *    DEFINICAO DE DATA E HORA DO SISTEMA. 
       COPY COD001A.

      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

            PERFORM 0001-ABRE-ARQUIVO 
            PERFORM 0002-OBTER-DATA 
            PERFORM 0003-OBTER-TIME   
            PERFORM 0004-GERAR-CABECALHO
            PERFORM 0007-LER-ARQUIVO
            PERFORM 0005-GERAR-DETALHE UNTIL FIM-ARQ = 'S'
            PERFORM 0008-GRAVAR-TOTAIS
            PERFORM 0006-FECHA-ARQUIVO 
            PERFORM 9999-FINALIZAR
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ABERTURA DE ARQUIVO
      *----------------------------------------------------------------*
       0001-ABRE-ARQUIVO               SECTION.
      *----------------------------------------------------------------*

           OPEN  INPUT ARQUIVO-ENTRADA
                OUTPUT REL0002A-SAIDA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DATA SISTEMA
      *----------------------------------------------------------------*
       0002-OBTER-DATA                 SECTION.
      *----------------------------------------------------------------*

            CALL 'PROGDATA' USING COD001A-REGISTRO 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER HORA SISTEMA
      *----------------------------------------------------------------*
       0003-OBTER-TIME                 SECTION.
      *----------------------------------------------------------------*

            CALL 'PROGTIME' USING COD001A-REGISTRO 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GERAR CABECALHO RELATORIO
      *----------------------------------------------------------------*
       0004-GERAR-CABECALHO            SECTION.
      *----------------------------------------------------------------*

            MOVE COD001A-DATA-ANO      TO WRK-MASC-DATA-ANO
            MOVE COD001A-DATA-MES      TO WRK-MASC-DATA-MES
            MOVE COD001A-DATA-DIA      TO WRK-MASC-DATA-DIA

            MOVE COD001A-HORA          TO WRK-MASC-HORA
            MOVE COD001A-MINUTO        TO WRK-MASC-MIN

            DISPLAY WRK-REL0002A-LINHA
            MOVE WRK-REL0002A-LINHA    TO WRK-REL0002A-STRING
            WRITE WRK-REL0002A-REGISTRO
            MOVE WRK-IND-PAGINACAO     TO WRK-REL0002A-PAG
            MOVE WRK-MASC-DATA         TO WRK-REL0002A-CAB1-DATA

            DISPLAY WRK-REL0002A-CAB1
            MOVE WRK-REL0002A-CAB1     TO WRK-REL0002A-STRING
            WRITE WRK-REL0002A-REGISTRO

            MOVE WRK-MASC-TIME         TO WRK-REL0002A-HORA

            DISPLAY WRK-REL0002A-CAB2
            MOVE WRK-REL0002A-CAB2     TO WRK-REL0002A-STRING
            WRITE WRK-REL0002A-REGISTRO

            DISPLAY WRK-REL0002A-LINHA
            MOVE WRK-REL0002A-LINHA    TO WRK-REL0002A-STRING
            WRITE WRK-REL0002A-REGISTRO

            DISPLAY WRK-REL0002A-CAB3
            MOVE WRK-REL0002A-CAB3     TO WRK-REL0002A-STRING
            WRITE WRK-REL0002A-REGISTRO
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GERAR DETALHE DO RELATORIO
      *----------------------------------------------------------------*
       0005-GERAR-DETALHE              SECTION.
      *----------------------------------------------------------------*

            IF WRK-IND-LINHA           GREATER  9
               MOVE ZEROS              TO WRK-IND-LINHA
               ADD 1                   TO WRK-IND-PAGINACAO
               DISPLAY WRK-REL0002A-LINHA-BRANCO
               MOVE WRK-REL0002A-LINHA-BRANCO
                                       TO WRK-REL0002A-STRING
               WRITE WRK-REL0002A-REGISTRO
               PERFORM 0004-GERAR-CABECALHO
            END-IF

            MOVE NOME                  TO WRK-REL0002A-NOME 
            MOVE IDADE                 TO WRK-REL0002A-IDADE
            MOVE SALARIO               TO WRK-REL0002A-SALARIO

            ADD SALARIO                TO TOTAL-SALARIO
            ADD 1                      TO CONTADOR

            DISPLAY WRK-REL0002A-DET1
            MOVE WRK-REL0002A-DET1     TO WRK-REL0002A-STRING
            WRITE WRK-REL0002A-REGISTRO

            ADD 1                      TO WRK-IND1
            ADD 1                      TO WRK-IND-LINHA

            PERFORM 0007-LER-ARQUIVO
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       0006-FECHA-ARQUIVO              SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQUIVO-ENTRADA
                 REL0002A-SAIDA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-end
       0006-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA ARQUIVO DE ENTRADA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-ler-arquivo
       0007-LER-ARQUIVO                SECTION.
      *----------------------------------------------------------------*

           READ ARQUIVO-ENTRADA
               AT END
                   MOVE 'S' TO FIM-ARQ
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-end
       0007-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR TOTALIZADOR
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0008-GRAVAR-TOTAIS 
       0008-GRAVAR-TOTAIS                SECTION.
      *----------------------------------------------------------------*

            DISPLAY WRK-REL0002A-LINHA-BRANCO
            MOVE WRK-REL0002A-LINHA-BRANCO
                                       TO WRK-REL0002A-STRING
            WRITE WRK-REL0002A-REGISTRO

            MOVE CONTADOR              TO WRK-REL0002A-QTD 
            MOVE TOTAL-SALARIO         TO WRK-MASCARA-SALARIO 
                                          WRK-REL0002A-TOT-SAL
            MOVE WRK-REL0002A-TOT1     TO WRK-REL0002A-STRING

            WRITE WRK-REL0002A-REGISTRO

            DISPLAY 'QTDE. REG.: ' CONTADOR
            DISPLAY "Salário: R$" WRK-MASCARA-SALARIO 
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

            DISPLAY 'RELATORIO GERADO COM SUCESSO!'
            STOP RUN 
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*








