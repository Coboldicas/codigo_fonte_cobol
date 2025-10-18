       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTE002.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS TECLA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TECLA PIC 9(4).
         01 I     PIC 9(4).
       PROCEDURE DIVISION.
           DISPLAY "Primeira mensagem"
           ACCEPT TECLA
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 50
            DISPLAY " "
           END-PERFORM
           DISPLAY "Tela limpa!" AT LINE 5 COLUMN 10
           STOP RUN.
