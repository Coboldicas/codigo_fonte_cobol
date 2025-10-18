       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALCNPJ.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * -------- Interface de entrada/saida para demo ----------
       01  WS-ENTRADA           PIC X(40).
       01  WS-RESP              PIC X(03).

      * -------- Áreas de trabalho da validação ----------------
       01  WS-CNPJ.
           05 WS-RAW              PIC X(40).
           05 WS-DIGITS           PIC 9(14).
           05 WS-NUM REDEFINES WS-DIGITS.
              10 WS-N             PIC 9 OCCURS 14 TIMES.
           05  WS-CHAR            pic X(1).
           05  WS-CODIGO          PIC 9(2).
           05 WS-LEN              PIC 9(02).
           05 WS-I                PIC 9(02).
           05 WS-J                PIC 9(02).
           05 WS-SOMA             PIC 9(06).
           05 WS-RESTO            PIC 9(02).
           05 WS-DV1              PIC 9(01).
           05 WS-DV2              PIC 9(01).
           05 WS-ALL-EQUAL        PIC X(01) VALUE 'N'.
           05 ALL-DIGITS-EQUAL    PIC X(1) VALUE 'S'.
           05 WS-IS-VALID         PIC X(1) VALUE 'N'.
           05 CNPJ-VALIDO      PIC X(1) VALUE 'N'.

       01  WS-PESO1   PIC 9 OCCURS 12 TIMES.
       01  WS-PESO2   PIC 9 OCCURS 13 TIMES.

      * =======================================================
       PROCEDURE DIVISION.
      *> cobol-lint CL002 0000-main-loop
       0000-MAIN-LOOP.

           MOVE 5 TO WS-PESO1(1)
           MOVE 4 TO WS-PESO1(2)
           MOVE 3 TO WS-PESO1(3)
           MOVE 2 TO WS-PESO1(4)
           MOVE 9 TO WS-PESO1(5)
           MOVE 8 TO WS-PESO1(6)
           MOVE 7 TO WS-PESO1(7)
           MOVE 6 TO WS-PESO1(8)
           MOVE 5 TO WS-PESO1(9)
           MOVE 4 TO WS-PESO1(10)
           MOVE 3 TO WS-PESO1(11)
           MOVE 2 TO WS-PESO1(12)

           MOVE 6 TO WS-PESO2(1)
           MOVE 5 TO WS-PESO2(2)
           MOVE 4 TO WS-PESO2(3)
           MOVE 3 TO WS-PESO2(4)
           MOVE 2 TO WS-PESO2(5)
           MOVE 9 TO WS-PESO2(6)
           MOVE 8 TO WS-PESO2(7)
           MOVE 7 TO WS-PESO2(8)
           MOVE 6 TO WS-PESO2(9)
           MOVE 5 TO WS-PESO2(10)
           MOVE 4 TO WS-PESO2(11)
           MOVE 3 TO WS-PESO2(12)
           MOVE 2 TO WS-PESO2(13)

           DISPLAY "=== Validador de CNPJ (mod 11) ===".
           DISPLAY "Digite um CNPJ (vazio para sair):".
           ACCEPT WS-ENTRADA.
           IF WS-ENTRADA = SPACES OR WS-ENTRADA = " "
               STOP RUN
           END-IF

           MOVE WS-ENTRADA TO WS-RAW
           PERFORM VALIDA-CNPJ

           IF CNPJ-VALIDO EQUAL 'S'
               DISPLAY "CNPJ valido."
           ELSE
               DISPLAY "CNPJ invalido."
           END-IF

           GO TO 0000-MAIN-LOOP.
      *> cobol-lint CL002 0000-end
       0000-END.
      * =======================================================
      *  Parágrafo reutilizável: VALIDA-CNPJ
      *  Entrada:  WS-RAW (X até 40) com ou sem máscara
      *  Saída:    CNPJ-VALIDO (88), WS-DIGITS (14 dígitos)
      * =======================================================
       VALIDA-CNPJ.
           MOVE 'N' TO WS-IS-VALID
           MOVE 0   TO WS-LEN
           MOVE 0   TO WS-I WS-J WS-SOMA WS-RESTO
           MOVE 0   TO WS-DV1 WS-DV2
           MOVE 'N' TO WS-ALL-EQUAL
           MOVE ZEROES TO WS-DIGITS

           *> 1) Extrair apenas dígitos
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 
                                                      LENGTH OF WS-RAW
      *       IF WS-RAW(WS-I:1) >= "0" AND WS-RAW(WS-I:1) <= "9"
                 ADD 1 TO WS-LEN
                 IF WS-LEN <= 14
                    MOVE WS-RAW(WS-I:1) TO WS-DIGITS(WS-LEN:1)
                    DISPLAY 'DIGITO: ' WS-DIGITS(WS-LEN:1)
                    if  WS-DIGITS(WS-LEN:1) is not numeric 
                    DISPLAY 'DIGITO antes: ' WS-DIGITS(WS-LEN:1)
                        move WS-DIGITS(WS-LEN:1) to WS-CHAR
                        display 'não É NUMERICO'
                        COMPUTE WS-CODIGO = FUNCTION ORD(WS-CHAR)
                        display 'WS-CODIGO :' WS-CODIGO 
                        move ws-codigo to WS-DIGITS(WS-LEN:1)
                    end-if
                    DISPLAY 'DIGITO depois: ' WS-DIGITS(WS-LEN:1)
                 END-IF
      *       END-IF
           END-PERFORM

           *> 2) Verificar tamanho exato
             DISPLAY 'WS-LEN : ' WS-LEN 
           IF WS-LEN NOT = 14
              EXIT PARAGRAPH
           END-IF

           *> 3) Rejeitar sequencias com TODOS os 14 dígitos iguais
           MOVE 'S' TO WS-ALL-EQUAL
           PERFORM VARYING WS-I FROM 2 BY 1 UNTIL WS-I > 14
              IF WS-DIGITS(WS-I:1) NOT = WS-DIGITS(1:1)
                 MOVE 'N' TO WS-ALL-EQUAL
                EXIT PERFORM
              END-IF
           END-PERFORM
           IF WS-ALL-EQUAL EQUAL 'S'
              EXIT PARAGRAPH
           END-IF

           *> 4) Calcular DV1 (sobre os 12 primeiros dígitos)
           MOVE 0 TO WS-SOMA
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 12
      *       ADD (WS-N(WS-I) * PESO1(WS-I)) TO WS-SOMA
              COMPUTE WS-SOMA = WS-SOMA + (WS-N(WS-I) * WS-PESO1(WS-I))   
           END-PERFORM
           DISPLAY 'WS-SOMA:' WS-SOMA
           COMPUTE WS-RESTO = FUNCTION MOD(WS-SOMA, 11)
           DISPLAY 'WS-RESTO: ' WS-RESTO
           IF WS-RESTO < 2
              MOVE 0 TO WS-DV1
           ELSE
              COMPUTE WS-DV1 = 11 - WS-RESTO
           END-IF
           DISPLAY 'WS-DV1: ' WS-DV1

           *> 5) Calcular DV2 (sobre os 12 + DV1)
           MOVE 0 TO WS-SOMA
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 13
              IF WS-I = 13
      *          ADD (WS-DV1 * PESO2(WS-I)) TO WS-SOMA
                 COMPUTE WS-SOMA = WS-SOMA + (WS-DV1 * WS-PESO2(WS-I))
              ELSE
      *          ADD (WS-N(WS-I) * PESO2(WS-I)) TO WS-SOMA
                 COMPUTE WS-SOMA = WS-SOMA + (WS-N(WS-I) *
                                                       WS-PESO2(WS-I))
              END-IF
           END-PERFORM
           DISPLAY 'SOMA: ' WS-SOMA
           COMPUTE WS-RESTO = FUNCTION MOD(WS-SOMA, 11)
           DISPLAY 'WS-RESTO' WS-RESTO
           IF WS-RESTO < 2
              MOVE 0 TO WS-DV2
           ELSE
              COMPUTE WS-DV2 = 11 - WS-RESTO
           END-IF
           DISPLAY 'WS-DV2: ' WS-DV2
           *> 6) Comparar com os dois dígitos informados
           MOVE 'N' TO CNPJ-VALIDO
           DISPLAY 'WS-N(13): '  WS-N(13)
           DISPLAY 'WS-N(14): '  WS-N(14)
           DISPLAY 'WS-DV1: ' WS-DV1
           DISPLAY 'WS-DV2: ' WS-DV2
           IF (WS-DV1 = WS-N(13)) AND
              (WS-DV2 = WS-N(14))
              DISPLAY 'VALIDO'
              MOVE 'S' TO CNPJ-VALIDO 
           END-IF.

     
