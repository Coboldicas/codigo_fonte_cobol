      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 11/08/2025
      * DESCRICAO..: VALIDAÇÃO DE CNPJ - MODULO 11 
      * NOME.......: VALIDA1B
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALIDA1B. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * -------- Interface de entrada/saida para demo ----------
       01  WRK-CNPJ-ENTRADA            PIC X(14) VALUE SPACES.
       01  WRK-RESP                    PIC X(03).

      * -------- Áreas de trabalho da validação ----------------
       01  WRK-CNPJ.
           05 WRK-RAW                  PIC X(14) VALUE SPACES.
           05 WRK-DIGITS               PIC 9(28).
           05 WRK-NUM REDEFINES WRK-DIGITS.
              10 WRK-N                 PIC 9(02) OCCURS 14 TIMES.
           05 WRK-CHAR                 pic X(01).
           05 WRK-CODIGO               PIC 9(02).
           05 WRK-LEN                  PIC 9(02).
           05 WRK-LEN2                 PIC 9(02) VALUE ZEROS.
           05 WRK-I                    PIC 9(02).
           05 WRK-J                    PIC 9(02).
           05 WRK-L                    PIC 9(02).
           05 WRK-SOMA                 PIC 9(06).
           05 WRK-MULTIPLICACAO        PIC 9(06).           
           05 WRK-RESTO                PIC 9(02).
           05 WRK-DV1                  PIC 9(01).
           05 WRK-DV2                  PIC 9(01).
           05 WRK-ALL-EQUAL            PIC X(01) VALUE 'N'.
           05 ALL-DIGITS-EQUAL         PIC X(01) VALUE 'S'.
           05 WRK-IS-VALID             PIC X(01) VALUE 'N'.
           05 CNPJ-VALIDO              PIC X(01) VALUE 'N'.

       01  WRK-PESO1                   PIC 9 OCCURS 12 TIMES.
       01  WRK-PESO2                   PIC 9 OCCURS 13 TIMES.

      *     DEPOIS - CNPJ: XX.XXX.XXX/XXXX-00
       01 WRK-CNPJ-IDENTIFICADO        PIC X(14) VALUE SPACES.

       01 WRK-MASCARA-CNPJ.
         05 WRK-MASC-CNPJ-PARTE1       PIC X(02) VALUE SPACES.
         05 FILLER                     PIC X(01) VALUE '.'.
         05 WRK-MASC-CNPJ-PARTE2       PIC X(03) VALUE SPACES.
         05 FILLER                     PIC X(01) VALUE '.'.
         05 WRK-MASC-CNPJ-PARTE3       PIC X(03) VALUE SPACES.       
         05 FILLER                     PIC X(01) VALUE '/'.
         05 WRK-MASC-CNPJ-FILIAL       PIC X(04) VALUE SPACES.
         05 FILLER                     PIC X(01) VALUE '-'.
         05 WRK-MASC-CNPJ-DV           PIC 9(02) VALUE ZEROS.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0001-INSERIR-CNPJ
           PERFORM 0002-INICIALIZAR 
           PERFORM 0003-INSERIR-TABINT
           PERFORM 0004-EXTRAIR-DIGITO
           PERFORM 0008-VALIDAR-DIGITO
           PERFORM 9999-FINALIZAR
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERE NUMERO DO CPF ATRAVES DA TELA
      *----------------------------------------------------------------*
       0001-INSERIR-CNPJ                SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE O CNPJ (SEM CARACTERES ESPECIAIS)'  
           ACCEPT WRK-CNPJ-ENTRADA 

           IF WRK-CNPJ-ENTRADA         EQUAL SPACES
              PERFORM 9999-FINALIZAR
           ELSE 
              MOVE WRK-CNPJ-ENTRADA    TO WRK-RAW
                                          WRK-CNPJ-IDENTIFICADO
              MOVE WRK-CNPJ-IDENTIFICADO(1:2)  
                                       TO WRK-MASC-CNPJ-PARTE1
              MOVE WRK-CNPJ-IDENTIFICADO(3:3) 
                                       TO WRK-MASC-CNPJ-PARTE2
              MOVE WRK-CNPJ-IDENTIFICADO(6:3)
                                       TO WRK-MASC-CNPJ-PARTE3
              MOVE WRK-CNPJ-IDENTIFICADO(9:4)
                                       TO WRK-MASC-CNPJ-FILIAL
              MOVE WRK-CNPJ-IDENTIFICADO(13:2) 
                                       TO WRK-MASC-CNPJ-DV
           END-IF
           .
      *----------------------------------------------------------------*
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INICIALIZAR VARIAVEIS
      *----------------------------------------------------------------*
       0002-INICIALIZAR                SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-IS-VALID
           MOVE 0                      TO WRK-LEN
           MOVE 0                      TO WRK-I WRK-J 
                                          WRK-SOMA 
                                          WRK-RESTO
           MOVE 'N'                    TO WRK-ALL-EQUAL
           MOVE ZEROES                 TO WRK-DIGITS
           .
      *----------------------------------------------------------------*
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERE VALOR NA TABELA INTERNA
      *----------------------------------------------------------------*
       0003-INSERIR-TABINT             SECTION.
      *----------------------------------------------------------------*

           MOVE 5 TO WRK-PESO1(1)
           MOVE 4 TO WRK-PESO1(2)
           MOVE 3 TO WRK-PESO1(3)
           MOVE 2 TO WRK-PESO1(4)
           MOVE 9 TO WRK-PESO1(5)
           MOVE 8 TO WRK-PESO1(6)
           MOVE 7 TO WRK-PESO1(7)
           MOVE 6 TO WRK-PESO1(8)
           MOVE 5 TO WRK-PESO1(9)
           MOVE 4 TO WRK-PESO1(10)
           MOVE 3 TO WRK-PESO1(11)
           MOVE 2 TO WRK-PESO1(12)

           MOVE 6 TO WRK-PESO2(1)
           MOVE 5 TO WRK-PESO2(2)
           MOVE 4 TO WRK-PESO2(3)
           MOVE 3 TO WRK-PESO2(4)
           MOVE 2 TO WRK-PESO2(5)
           MOVE 9 TO WRK-PESO2(6)
           MOVE 8 TO WRK-PESO2(7)
           MOVE 7 TO WRK-PESO2(8)
           MOVE 6 TO WRK-PESO2(9)
           MOVE 5 TO WRK-PESO2(10)
           MOVE 4 TO WRK-PESO2(11)
           MOVE 3 TO WRK-PESO2(12)
           MOVE 2 TO WRK-PESO2(13)
           .
      *----------------------------------------------------------------*
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EXTRAIR APENAS DIGITOS E IDENTIFICAR ALPHANUMERICO
      *    MUDANDO O CARACTER DE ACRODO COM A ORDEM DA TABELA ASCII
      *----------------------------------------------------------------*
       0004-EXTRAIR-DIGITO             SECTION.
      *----------------------------------------------------------------*

      *    VERIFICAR TAMANHO EXATO
           INSPECT WRK-CNPJ-ENTRADA    TALLYING WRK-LEN2
                                       FOR ALL " "
           COMPUTE WRK-LEN2 = 14 - WRK-LEN2
           IF WRK-LEN2 NOT EQUAL 14
              DISPLAY 'TAMANHO DO CNPJ INVALIDO'
      *       PERFORM 9999-FINALIZAR
           END-IF
      *    PERFORM 0005-VALIDAR-IGUAIS
           PERFORM 0006-CALCULAR-DIGITO1 
           PERFORM 0007-CALCULAR-DIGITO2 
           .
      *----------------------------------------------------------------*
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    REJEITAR SEQUENCIAIS COM TODOS OS 14 DIGITOS IGUAIS
      *----------------------------------------------------------------*
       0005-VALIDAR-IGUAIS             SECTION.
      *----------------------------------------------------------------*

           MOVE 'S'                    TO WRK-ALL-EQUAL
           PERFORM VARYING WRK-I FROM 2 BY 1 UNTIL WRK-I > 14
              IF WRK-DIGITS(WRK-I:1)   NOT EQUAL WRK-DIGITS(1:1)
                 MOVE 'N'              TO WRK-ALL-EQUAL
              END-IF
           END-PERFORM

           IF WRK-ALL-EQUAL            EQUAL 'S'
              DISPLAY 'DIGITOS NAO PODEM SER REPETITIVOS'
              PERFORM 9999-FINALIZAR
           END-IF
           .
      *----------------------------------------------------------------*
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CALCULO DO DIGITO 1
      *----------------------------------------------------------------*
       0006-CALCULAR-DIGITO1           SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-I FROM 1 BY 1 UNTIL WRK-I > WRK-LEN2
              ADD 1                    TO WRK-LEN
              IF WRK-LEN               <= 13
                 MOVE WRK-RAW(WRK-I:1) TO WRK-DIGITS(WRK-LEN:1)
                 MOVE  WRK-DIGITS(WRK-LEN:1) 
                                       TO WRK-L 
                 IF  WRK-DIGITS(WRK-LEN:1)
                                       IS NUMERIC
                     IF WRK-LEN        <= 12
                        COMPUTE WRK-MULTIPLICACAO =
                                WRK-L * WRK-PESO1(WRK-I)
                        COMPUTE WRK-SOMA =
                                WRK-SOMA + WRK-MULTIPLICACAO  
                     ELSE
      *                 DISPLAY 'WRK-SOMA:' WRK-SOMA
                        COMPUTE WRK-RESTO = FUNCTION MOD(WRK-SOMA, 11)
      *                 DISPLAY 'WRK-RESTO: ' WRK-RESTO
                        IF WRK-RESTO    < 2
                           MOVE 0      TO WRK-DV1
                        ELSE
                           COMPUTE WRK-DV1 = 11 - WRK-RESTO
                        END-IF
      *                 DISPLAY 'DIGITO 1.: ' WRK-DV1
                     END-IF 
                 ELSE 
                     MOVE WRK-DIGITS(WRK-LEN:1)
                                       TO WRK-CHAR
                     COMPUTE WRK-CODIGO = FUNCTION ORD(WRK-CHAR)
                     SUBTRACT 1      FROM WRK-CODIGO 
                     SUBTRACT 48     FROM WRK-CODIGO 
                     MOVE  WRK-CODIGO  TO WRK-L
                     IF WRK-LEN        <= 12
                        COMPUTE WRK-MULTIPLICACAO =
                                WRK-L * WRK-PESO1(WRK-I)
                        COMPUTE WRK-SOMA = 
                                WRK-SOMA + WRK-MULTIPLICACAO  
                     ELSE
      *                 DISPLAY 'WRK-SOMA:' WRK-SOMA
                        COMPUTE WRK-RESTO = FUNCTION MOD(WRK-SOMA, 11)
      *                 DISPLAY 'WRK-RESTO: ' WRK-RESTO
                        IF WRK-RESTO < 2
                           MOVE 0 TO WRK-DV1
                        ELSE
                           COMPUTE WRK-DV1 = 11 - WRK-RESTO
                        END-IF
      *                 DISPLAY 'DIGITO 1: ' WRK-DV1
                     END-IF 
                 END-IF
              END-IF
           END-PERFORM
           .
      *----------------------------------------------------------------*
       0006-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CALCULO DO DIGITO 2
      *----------------------------------------------------------------*
       0007-CALCULAR-DIGITO2           SECTION.
      *----------------------------------------------------------------*

           PERFORM 0002-INICIALIZAR 

           PERFORM VARYING WRK-I FROM 1 BY 1 UNTIL WRK-I > WRK-LEN2
              ADD 1                    TO WRK-LEN
              IF WRK-LEN               <= 14
                 MOVE WRK-RAW(WRK-I:1) TO WRK-DIGITS(WRK-LEN:1)
                 MOVE  WRK-DIGITS(WRK-LEN:1) 
                                       TO WRK-L 
                 IF  WRK-DIGITS(WRK-LEN:1)
                                       IS NUMERIC
                     IF WRK-LEN        <= 13
                        COMPUTE WRK-MULTIPLICACAO =
                                WRK-L * WRK-PESO2(WRK-I)
                        COMPUTE WRK-SOMA =
                                WRK-SOMA + WRK-MULTIPLICACAO  
                     ELSE
      *                 DISPLAY 'WRK-SOMA:' WRK-SOMA
                        COMPUTE WRK-RESTO = FUNCTION MOD(WRK-SOMA, 11)
      *                 DISPLAY 'WRK-RESTO: ' WRK-RESTO
                        IF WRK-RESTO    < 2
                           MOVE 0      TO WRK-DV2
                        ELSE
                           COMPUTE WRK-DV2 = 11 - WRK-RESTO
                        END-IF
      *                 DISPLAY 'DIGITO 1.: ' WRK-DV2
                     END-IF 
                 ELSE 
                     MOVE WRK-DIGITS(WRK-LEN:1)
                                       TO WRK-CHAR
                     COMPUTE WRK-CODIGO = FUNCTION ORD(WRK-CHAR)
                     SUBTRACT 1      FROM WRK-CODIGO 
                     SUBTRACT 48     FROM WRK-CODIGO 
                     MOVE  WRK-CODIGO  TO WRK-L
                     IF WRK-LEN        <= 13
                        COMPUTE WRK-MULTIPLICACAO =
                                WRK-L * WRK-PESO2(WRK-I)
                        COMPUTE WRK-SOMA = 
                                WRK-SOMA + WRK-MULTIPLICACAO  
                     ELSE
      *                 DISPLAY 'WRK-SOMA:' WRK-SOMA
                        COMPUTE WRK-RESTO = FUNCTION MOD(WRK-SOMA, 11)
      *                 DISPLAY 'WRK-RESTO: ' WRK-RESTO
                        IF WRK-RESTO < 2
                           MOVE 0 TO WRK-DV2
                        ELSE
                           COMPUTE WRK-DV2 = 11 - WRK-RESTO
                        END-IF
      *                 DISPLAY 'DIGITO 1: ' WRK-DV2
                     END-IF 
                 END-IF
              END-IF
           END-PERFORM
           .
      *----------------------------------------------------------------*
       0007-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR DIGITO VERIFICADOR
      *----------------------------------------------------------------*
       0008-VALIDAR-DIGITO             SECTION.
      *----------------------------------------------------------------*


           IF (WRK-DV1 = WRK-DIGITS(13:1)) AND
              (WRK-DV2 = WRK-DIGITS(14:1))
      *       DISPLAY 'CNPJ VALIDO'
               DISPLAY "CNPJ VALIDO: " WRK-MASCARA-CNPJ
           ELSE
               DISPLAY "CNPJ INVALIDO."
           END-IF
           .
      *----------------------------------------------------------------*
       0008-END.                       EXIT.
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
