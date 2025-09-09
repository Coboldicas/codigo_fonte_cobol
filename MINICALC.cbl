      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 08/09/2025
      * DESCRICAO..: MINI CALCULADORA - COBOL
      * NOME.......: MINICALC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINICALC. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WRK-OPERACAO                PIC X(01) VALUE SPACES.
       01  WRK-RAW1                    PIC X(30) VALUE SPACES.
       01  WRK-RAW2                    PIC X(30) VALUE SPACES.

       01  WRK-NUM1                    PIC S9(07)V9(4) VALUE ZEROS.
       01  WRK-NUM2                    PIC S9(07)V9(4) VALUE ZEROS.
       01  WRK-RESULTADO               PIC S9(11)V9(6) VALUE ZEROS.

       01  WRK-RESULT-EDIT             PIC -ZZ.ZZZ.ZZZ,999999.
       01  WRK-KEEP                    PIC X(01) VALUE 'S'.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0001-MENU-PRINCIPAL
           PERFORM 0002-EFETUAR-CALC
           PERFORM 0007-ANALISA-CALCULO 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    MENU PRINCIPAL
      *----------------------------------------------------------------*
       0001-MENU-PRINCIPAL             SECTION.
      *----------------------------------------------------------------*

           DISPLAY "==============================="
           DISPLAY "  MINI CALCULADORA (COBOL)     "
           DISPLAY "==============================="

      *>   1) LER PRIMEIRO NUMERO COMO TEXTO (ACEITA VIRGULA E PONTO)
           DISPLAY 'INFORME O 1o NUMERO: ' WITH NO ADVANCING 
           ACCEPT WRK-RAW1
           INSPECT WRK-RAW1 REPLACING ALL '.' BY ','.
           MOVE WRK-RAW1               TO WRK-NUM1

      *>   2) LER OPERAÇÃO
           DISPLAY 'ESCOLHA A OPERAÇÃO ( + , - , * , / ): '
                WITH NO ADVANCING 
           ACCEPT WRK-OPERACAO
           IF  ((WRK-OPERACAO          EQUAL '+')
             OR (WRK-OPERACAO          EQUAL '-')
             OR (WRK-OPERACAO          EQUAL '*')
             OR (WRK-OPERACAO          EQUAL '/'))
              CONTINUE
           ELSE
              DISPLAY 'OPERAÇÃO INVALIDA!'
              PERFORM 9999-FINALIZAR
           END-IF 

      *>   3) LER SEGUNDO NUMERO COMO TEXTO (ACEITA VIRGULA E PONTO)
           DISPLAY 'INFORME O 2o NUMERO: ' WITH NO ADVANCING 
           ACCEPT WRK-RAW2
           INSPECT WRK-RAW2 REPLACING ALL '.' BY ','.
           MOVE WRK-RAW2               TO WRK-NUM2
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    TELA MENU
      *----------------------------------------------------------------*
       0002-EFETUAR-CALC               SECTION.
      *----------------------------------------------------------------*

           EVALUATE WRK-OPERACAO
               WHEN '+'
                   PERFORM 0003-CALCULAR-ADICAO
               WHEN '-'
                   PERFORM 0004-CALCULAR-SUBTRACAO
               WHEN '*'
                   PERFORM 0005-CALCULAR-MULTIPLICACAO
               WHEN '/'
                   PERFORM 0006-CALCULAR-DIVISAO
               WHEN OTHER
                    DISPLAY 'OPÇÃO INVÁLIDA!' 
                    PERFORM 9999-FINALIZAR
           END-EVALUATE
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EFETUA CALCULO DE ADIÇÃO
      *----------------------------------------------------------------*
       0003-CALCULAR-ADICAO            SECTION.
      *----------------------------------------------------------------*

           COMPUTE WRK-RESULTADO = WRK-NUM1 + WRK-NUM2
           MOVE WRK-RESULTADO          TO WRK-RESULT-EDIT
           DISPLAY 'RESULTADO: ' WRK-RESULT-EDIT
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EFETUA CALCULO DE ADIÇÃO
      *----------------------------------------------------------------*
       0004-CALCULAR-SUBTRACAO         SECTION.
      *----------------------------------------------------------------*

           COMPUTE WRK-RESULTADO = WRK-NUM1 - WRK-NUM2
           MOVE WRK-RESULTADO          TO WRK-RESULT-EDIT
           DISPLAY 'RESULTADO: ' WRK-RESULT-EDIT
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EFETUA CALCULO DE MULTIPLIÇÃO
      *----------------------------------------------------------------*
       0005-CALCULAR-MULTIPLICACAO     SECTION.
      *----------------------------------------------------------------*

           COMPUTE WRK-RESULTADO = WRK-NUM1 * WRK-NUM2
           MOVE WRK-RESULTADO          TO WRK-RESULT-EDIT
           DISPLAY 'RESULTADO: ' WRK-RESULT-EDIT
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EFETUA CALCULO DE DIVISÃO
      *----------------------------------------------------------------*
       0006-CALCULAR-DIVISAO           SECTION.
      *----------------------------------------------------------------*

           IF WRK-NUM2 = 0
              DISPLAY 'ERRO: DIVISÃO POR ZERO NÃO PERMITIDA'
              PERFORM 9999-FINALIZAR
           ELSE 
              COMPUTE WRK-RESULTADO = WRK-NUM1 / WRK-NUM2
              MOVE WRK-RESULTADO      TO WRK-RESULT-EDIT
              DISPLAY 'RESULTADO: ' WRK-RESULT-EDIT
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-end
       0006-END.                       EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    ANALISAR NOVO CALCULO
      *----------------------------------------------------------------*
       0007-ANALISA-CALCULO            SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DESEJA EFETUAR UM NOVO CALCULO?: (S/N)'
                   WITH NO ADVANCING 
           ACCEPT WRK-KEEP
           IF ((WRK-KEEP EQUAL 'S')
           OR  (WRK-KEEP EQUAL 'N'))
              CONTINUE
           ELSE 
              PERFORM 0007-ANALISA-CALCULO 
           END-IF 

           IF WRK-KEEP                 EQUAL 'S'
              PERFORM 0000-PROCESSAR
           ELSE
              PERFORM 9999-FINALIZAR
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-end
       0007-END.                       EXIT.
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
