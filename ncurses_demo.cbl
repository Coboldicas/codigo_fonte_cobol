       IDENTIFICATION DIVISION.
       PROGRAM-ID. NCURSES-DEMO.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MSG        PIC X(50) VALUE "Olá, NCurses no COBOL!".
       
       PROCEDURE DIVISION.
      *    CALL "iniciar_ncurses"
           CALL "ncurses_wrapper.c"

           CALL "mostrar_mensagem" USING MSG
       
           CALL "esperar_tecla"
           
           CALL "finalizar_ncurses"
           
           STOP RUN.