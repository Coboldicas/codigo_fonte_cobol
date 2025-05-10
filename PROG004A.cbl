       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG004A.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Definição do tamanho máximo de cadastros
       01 MAX-REGISTROS       PIC 9(02) VALUE 10.
       01 INDICE              PIC 9(02) VALUE 1.

      * Definição da estrutura do cadastro
       01 CADASTROS.
           05 USUARIO OCCURS 10 TIMES.
               10 NOME      PIC X(30).
               10 IDADE     PIC 9(02).
               10 CARGO     PIC X(20).

      * Variáveis auxiliares
       01 RESP          PIC X(03).
       01 TEMP-NOME     PIC X(30).
       01 TEMP-IDADE    PIC 9(02).
       01 TEMP-CARGO    PIC X(20).

       PROCEDURE DIVISION.

      *MAIN-LOGIC.

           DISPLAY "SIMULACAO DE CADASTRO - DIGITE 'FIM' PARA SAIR".

           PERFORM UNTIL INDICE > MAX-REGISTROS
               DISPLAY "Nome: "
               ACCEPT TEMP-NOME

               IF TEMP-NOME = "FIM" THEN
                   EXIT PERFORM
               END-IF

               DISPLAY "Idade: "
               ACCEPT TEMP-IDADE

               DISPLAY "Cargo: "
               ACCEPT TEMP-CARGO

               MOVE TEMP-NOME TO NOME(INDICE)
               MOVE TEMP-IDADE TO IDADE(INDICE)
               MOVE TEMP-CARGO TO CARGO(INDICE)

               ADD 1 TO INDICE
           END-PERFORM.

           DISPLAY "LISTA DE CADASTROS:"
           PERFORM VARYING INDICE FROM 1 BY 1 UNTIL
                                INDICE > MAX-REGISTROS
               IF NOME(INDICE) NOT = SPACES THEN
                   DISPLAY "Nome: " NOME(INDICE)
                   DISPLAY "Idade: " IDADE(INDICE)
                   DISPLAY "Cargo: " CARGO(INDICE)
                   DISPLAY "----------------------"
               END-IF
           END-PERFORM.
           STOP RUN.