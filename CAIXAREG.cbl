      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 21/07/2025
      * DESCRICAO..: SIMULADOR DE CAIXA REGISTRADORA
      * NOME.......: CAIXAREG
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAIXAREG.
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WRK-VALOR-ITEM1              PIC 9(05)V99 VALUE ZEROS.
       01  WRK-VALOR-ITEM2              PIC 9(05)V99 VALUE ZEROS.   
       01  WRK-VALOR-ITEM3              PIC 9(05)V99 VALUE ZEROS.
       01  WRK-DESCONTO                 PIC 9(05)V99 VALUE ZEROS.
       01  WRK-TOTAL-COMPRA             PIC 9(06)V99 VALUE ZEROS.
       01  WRK-TOTAL-FINAL              PIC 9(06)V99 VALUE ZEROS.

       01  WRK-VALIDA-DESC              PIC X(01)    VALUE 'N'.
       01  WRK-VALIDA-ITEM              PIC X(01)    VALUE 'N'.
       01  WRK-VALIDA-VALDESC           PIC X(01)    VALUE 'N'.
       01  WRK-MASCARA                  PIC ZZZ.ZZZ.ZZZ,ZZ.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
           PERFORM 0001-INSERIR-VALOR
           PERFORM 0002-CALCULA-COMPRA
           PERFORM 0003-CALCULA-DESCONTO
           PERFORM 0004-TOTALIZADOR
           PERFORM 9999-FINALIZAR
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERE DADOS ATRAVES DA TELA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-inserir-valor
       0001-INSERIR-VALOR              SECTION.
      *----------------------------------------------------------------*

           PERFORM 0011-INSERIR-VALOR1 UNTIL WRK-VALIDA-ITEM EQUAL 'S'
           MOVE 'N'                    TO WRK-VALIDA-ITEM
           PERFORM 0021-INSERIR-VALOR2 UNTIL WRK-VALIDA-ITEM EQUAL 'S'
           MOVE 'N'                    TO WRK-VALIDA-ITEM
           PERFORM 0031-INSERIR-VALOR3 UNTIL WRK-VALIDA-ITEM EQUAL 'S'
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERE DADOS ATRAVES DA TELA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0011-inserir-valor1
       0011-INSERIR-VALOR1             SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE O VALOR DO ITEM 1'.
           ACCEPT WRK-VALOR-ITEM1

           IF WRK-VALOR-ITEM1          EQUAL ZEROS 
              DISPLAY 'VALOR DO ITEM 1 ZERADO'
              MOVE 'N'                 TO WRK-VALIDA-ITEM
           ELSE
              MOVE 'S'                 TO WRK-VALIDA-ITEM
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0011-end
       0011-END.                       EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    INSERE DADOS ATRAVES DA TELA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0021-inserir-valor2
       0021-INSERIR-VALOR2             SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE O VALOR DO ITEM 2'.
           ACCEPT WRK-VALOR-ITEM2

           IF WRK-VALOR-ITEM2          EQUAL ZEROS 
              DISPLAY 'VALOR DO ITEM 2 ZERADO'
              MOVE 'N'                 TO WRK-VALIDA-ITEM
           ELSE
              MOVE 'S'                 TO WRK-VALIDA-ITEM
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0021-end
       0021-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERE DADOS ATRAVES DA TELA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0031-inserir-valor3
       0031-INSERIR-VALOR3              SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE O VALOR DO ITEM 3'.
           ACCEPT WRK-VALOR-ITEM3

           IF WRK-VALOR-ITEM3          EQUAL ZEROS 
              DISPLAY 'VALOR DO ITEM 3 ZERADO'
              MOVE 'N'                 TO WRK-VALIDA-ITEM
           ELSE
              MOVE 'S'                 TO WRK-VALIDA-ITEM
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0031-end
       0031-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CALCULA TOTAL COMPRA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-calcula-compra
       0002-CALCULA-COMPRA             SECTION.
      *----------------------------------------------------------------*

           ADD WRK-VALOR-ITEM1         TO WRK-TOTAL-COMPRA
           ADD WRK-VALOR-ITEM2         TO WRK-TOTAL-COMPRA
           ADD WRK-VALOR-ITEM3         TO WRK-TOTAL-COMPRA

           DISPLAY 'VALOR TOTAL DA COMPRA: ' WRK-TOTAL-COMPRA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDA E CALCULA DESCONTO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-calcula-desconto
       0003-CALCULA-DESCONTO           SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DESEJA DAR DESCONTO (S)SIM OU (N)NAO'
           ACCEPT WRK-VALIDA-DESC
           
           IF WRK-VALIDA-DESC         EQUAL 'S'
              PERFORM 0033-VALIDA-DESCONTO UNTIL 
                                       WRK-VALIDA-VALDESC EQUAL 'S'
              COMPUTE WRK-TOTAL-FINAL = 
                                   (WRK-TOTAL-COMPRA - WRK-DESCONTO)
           ELSE
              MOVE WRK-TOTAL-COMPRA    TO WRK-TOTAL-FINAL
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDA E CALCULA DESCONTO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0033-valida-desconto
       0033-VALIDA-DESCONTO           SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE O VALOR DO DESCONTO'
           ACCEPT  WRK-DESCONTO
           IF WRK-DESCONTO > ZEROS
              MOVE 'S'               TO WRK-VALIDA-VALDESC
           ELSE
              MOVE 'N'               TO WRK-VALIDA-VALDESC
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0033-end
       0033-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EXIBIR VALORES TOTAIS
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-TOTALIZADOR
       0004-TOTALIZADOR                SECTION.
      *----------------------------------------------------------------*

           DISPLAY '**----------------------------------**'
           MOVE WRK-VALOR-ITEM1        TO WRK-MASCARA
           DISPLAY '** VALOR DO ITEM 1.......: ' WRK-MASCARA
           MOVE WRK-VALOR-ITEM2        TO WRK-MASCARA
           DISPLAY '** VALOR DO ITEM 2.......: ' WRK-MASCARA
           MOVE WRK-VALOR-ITEM3        TO WRK-MASCARA
           DISPLAY '** VALOR DO ITEM 3.......: ' WRK-MASCARA
           DISPLAY '**-------------------------'
           MOVE WRK-TOTAL-COMPRA       TO WRK-MASCARA
           DISPLAY '** VALOR TOTAL DA COMPRA.: ' WRK-MASCARA
           MOVE WRK-DESCONTO           TO WRK-MASCARA
           DISPLAY '** VALOR DO DESCONTO.....:-' WRK-MASCARA
           DISPLAY '**-------------------------'
           MOVE WRK-TOTAL-FINAL        TO WRK-MASCARA
           DISPLAY '** VALOR TOTAL FINAL.....: ' WRK-MASCARA
           DISPLAY '**----------------------------------**'
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
           DISPLAY 'FIM DE PROGRAMA'
           STOP RUN 
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*