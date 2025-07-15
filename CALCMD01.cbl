      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 14/07/2025
      * DESCRICAO..: CALCULO MEDIO COM VALIDACAO
      * NOME.......: CALCMD01
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCMD01.
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WRK-VALOR01         PIC 9(06)V99 VALUE ZEROS.
       01 WRK-VALOR02         PIC 9(06)V99 VALUE ZEROS.
       01 WRK-VALOR03         PIC 9(06)V99 VALUE ZEROS.
       01 WRK-DIVISOR         PIC 9(02) VALUE ZEROS.
       01 WRK-VALOR-MEDIO     PIC 9(08)V9999 VALUE ZEROS.

       01 WRK-VALIDA01        PIC X(01) VALUE 'N'.
       01 WRK-VALIDA02        PIC X(01) VALUE 'N'.
       01 WRK-VALIDA03        PIC X(01) VALUE 'N'.

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
           PERFORM 0021-INSERIR-VALOR
           PERFORM 0031-INSERIR-VALOR
           PERFORM 0002-CALCULAR-MEDIA
           PERFORM 9999-FINALIZAR 

           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERE DADOS ATRAVES DA TELA
      *----------------------------------------------------------------*
       0001-INSERIR-VALOR              SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE O PRIMEIRO VALOR (0 A 99)'.
           ACCEPT WRK-VALOR01

           IF WRK-VALOR01 > 0
              MOVE 'S'                 TO WRK-VALIDA01 
           ELSE
              DISPLAY 'VALOR01 INVALIDO'
              PERFORM 0001-INSERIR-VALOR
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERE DADOS ATRAVES DA TELA
      *----------------------------------------------------------------*
       0021-INSERIR-VALOR              SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE O SEGUNDO VALOR (0 A 99)'.
           ACCEPT WRK-VALOR02

           IF WRK-VALOR02 > 0
              MOVE 'S'                 TO WRK-VALIDA02 
           ELSE
              DISPLAY 'VALOR02 INVALIDO'
              PERFORM 0021-INSERIR-VALOR
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0021-end
       0021-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERE DADOS ATRAVES DA TELA
      *----------------------------------------------------------------*
       0031-INSERIR-VALOR              SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE O TERCEIRO VALOR (0 A 99)'.
           ACCEPT WRK-VALOR03

           IF WRK-VALOR03 > 0
              MOVE 'S'                 TO WRK-VALIDA03 
           ELSE
              DISPLAY 'VALOR03 INVALIDO'
              PERFORM 0031-INSERIR-VALOR
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0031-end
       0031-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PESQUISAR CPF PROCURADO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-CALCULAR-MEDIA
       0002-CALCULAR-MEDIA             SECTION.
      *----------------------------------------------------------------*

           IF WRK-VALIDA01 EQUAL 'S' AND  
              WRK-VALIDA02 EQUAL 'S' AND 
              WRK-VALIDA03 EQUAL 'S'
              DISPLAY 'POR QUANTO OS VALORES SERAO DIVIDOS: '
              ACCEPT WRK-DIVISOR 

              COMPUTE WRK-VALOR-MEDIO =  (WRK-VALOR01 + WRK-VALOR02 +
                                          WRK-VALOR03)
              DISPLAY '**------------------------------------------**'
              DISPLAY 'VALOR 01: '    WRK-VALOR01         
              DISPLAY 'VALOR 02: '    WRK-VALOR02
              DISPLAY 'VALOR 03: '    WRK-VALOR03
              DISPLAY '-------------'
              DISPLAY 'TOTAL   : '    WRK-VALOR-MEDIO            
              COMPUTE WRK-VALOR-MEDIO =  WRK-VALOR-MEDIO / WRK-DIVISOR
              DISPLAY '-------------'
              DISPLAY 'VALOR MEDIO: ' WRK-VALOR-MEDIO
           ELSE
              DISPLAY 'NAO FOI POSSIVEL EFETUAR O CALCULO.'
              DISPLAY 'VERIFIQUE OS VALORES.'

              IF WRK-VALIDA01 EQUAL 'N'
                 PERFORM 0001-INSERIR-VALOR
              ELSE  
                 IF WRK-VALIDA02 EQUAL 'N'
                    PERFORM 0021-INSERIR-VALOR
                 ELSE  
                    IF WRK-VALIDA03 EQUAL 'N'
                       PERFORM 0031-INSERIR-VALOR
                    END-IF
                 END-IF
              END-IF 
           END-IF
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
           DISPLAY 'FIM DE PROGRAMA'
           STOP RUN 
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*