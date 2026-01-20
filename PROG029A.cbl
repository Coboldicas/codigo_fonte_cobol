      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 19/01/2026
      * DESCRICAO..: CONVERSOR DE MOEDAS
      * NOME.......: PROG029A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG029A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * VARIAVEIS DE ENTRADA
      *----------------------------------------------------------------*
       01 WRK-VALOR-REAL               PIC 9(7)V9(02) VALUE ZEROS.

      *----------------------------------------------------------------*
      * TAXAS DE CONVERSAO 
      *----------------------------------------------------------------*
       01 WRK-TAXA-DOLAR               PIC 9(3)V99 VALUE 5,37.
       01 WRK-TAXA-EURO                PIC 9(3)V99 VALUE 6,25.
       01 WRK-TAXA-IENE                PIC 9(3)V999 VALUE 0,034.

      *----------------------------------------------------------------*
      * VALORES CONVERTIDOS
      *----------------------------------------------------------------*
       01 WRK-VALOR-DOLAR              PIC 9(7)V99 VALUE ZEROS.
       01 WRK-VALOR-EURO               PIC 9(7)V99 VALUE ZEROS.
       01 WRK-VALOR-IENE               PIC 9(7)V99 VALUE ZEROS.

      *----------------------------------------------------------------*
      * MASCARA EXIBICAO
      *----------------------------------------------------------------*
       01 WRK-MASC-REAL                PIC Z.ZZZ.ZZZ,ZZ.
       01 WRK-MASC-DOLAR               PIC Z.ZZZ.ZZZ,ZZ.
       01 WRK-MASC-EURO                PIC Z.ZZZ.ZZZ,ZZ.
       01 WRK-MASC-IENE                PIC Z.ZZZ.ZZZ,ZZ.

      *----------------------------------------------------------------*
      * CONTROLE
      *----------------------------------------------------------------*
       01 WRK-MENSAGEM                 PIC X(50) VALUE SPACES.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0001-INFORMAR-VALOR
           PERFORM 0002-VALIDAR-VALOR
           PERFORM 0003-CALCULAR-VALOR
           PERFORM 0004-EXIBIR-RESULTADO
           PERFORM 9999-FINALIZAR
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INFORMAR O VALOR DE ENTRADA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-informar-valor
       0001-INFORMAR-VALOR             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '**---------------------------------------**'
           DISPLAY '**           CONVERSOR DE MOEDAS         **'
           DISPLAY '**---------------------------------------**'
           DISPLAY "INFORME O VALOR EM REAL: " 
                    WITH NO ADVANCING
           ACCEPT WRK-VALOR-REAL
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR VALOR DE ENTRADA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-validar-valor
       0002-VALIDAR-VALOR             SECTION.
      *----------------------------------------------------------------*

           IF WRK-VALOR-REAL <= 0
              MOVE 'VALOR INVALIDO INFORMADO.'
                                       TO WRK-MENSAGEM 
              DISPLAY WRK-MENSAGEM 
              PERFORM 9999-FINALIZAR
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CALCULAR VALOR DA MOEDA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-calcular-valor
       0003-CALCULAR-VALOR             SECTION.
      *----------------------------------------------------------------*

           COMPUTE WRK-VALOR-DOLAR = WRK-VALOR-REAL / WRK-TAXA-DOLAR  
           COMPUTE WRK-VALOR-EURO  = WRK-VALOR-REAL / WRK-TAXA-EURO 
           COMPUTE WRK-VALOR-IENE  = WRK-VALOR-REAL / WRK-TAXA-IENE  

           MOVE WRK-VALOR-REAL         TO WRK-MASC-REAL 
           MOVE WRK-VALOR-DOLAR        TO WRK-MASC-DOLAR
           MOVE WRK-VALOR-EURO         TO WRK-MASC-EURO 
           MOVE WRK-VALOR-IENE         TO WRK-MASC-IENE 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INFORMAR O VALOR DE ENTRADA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-exibir-resultado
       0004-EXIBIR-RESULTADO           SECTION.
      *----------------------------------------------------------------*

           DISPLAY '**---------------------------------------**'
           DISPLAY '**         RESULTADO DA CONVERSAO        **'
           DISPLAY '**---------------------------------------**'
           DISPLAY "REAL..: "  WRK-MASC-REAL
           DISPLAY "DOLAR.: "  WRK-MASC-DOLAR
           DISPLAY "EURO..: "  WRK-MASC-EURO 
           DISPLAY "IENE..: "  WRK-MASC-IENE 
           DISPLAY '**---------------------------------------**'
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

           DISPLAY '********************************'
           DISPLAY '*        FIM DE PROGRAMA       *'
           DISPLAY '********************************'
           STOP RUN 
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*

