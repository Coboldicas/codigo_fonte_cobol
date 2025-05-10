       identification division.
       program-id. PROGTEST3.

       data division.
       working-storage section.
       01 WS-NOME        PIC X(30) VALUE SPACES.
       01 WS-IDADE       PIC 9(2) VALUE ZEROS.
       01 WS-KEY         PIC 9(2).
       01 WS-IDADE-STR   PIC X(3). *> String para exibir WS-IDADE

       procedure division.
       main-section.
           DISPLAY 'COMEÇA'
           CALL "initscr" *> Inicia o ncurses
           CALL "clear"

           CALL "mvprintw" USING BY VALUE 2 BY VALUE 10 BY
            REFERENCE "=== CADASTRO ==="
           CALL "mvprintw" USING BY VALUE 4 BY VALUE 10 BY
            REFERENCE "Nome: "
           CALL "refresh"
           CALL "scanw" USING BY REFERENCE WS-NOME

           CALL "mvprintw" USING BY VALUE 6 BY VALUE 10 BY
            REFERENCE "Idade: "
           CALL "refresh"
           CALL "scanw" USING BY REFERENCE WS-IDADE

            DISPLAY "WS-IDADE "WS-IDADE 
            DISPLAY "WS-IDADE-STR" WS-IDADE-STR
            DISPLAY "NOME" WS-NOME


      * Converter WS-IDADE para string
           MOVE WS-IDADE TO WS-IDADE-STR

           CALL "clear"
           CALL "mvprintw" USING BY VALUE 10 BY VALUE 10 BY
            REFERENCE "Cadastro concluído!"
           CALL "mvprintw" USING BY VALUE 12 BY VALUE 10 BY
            REFERENCE "Nome: "
           CALL "mvprintw" USING BY VALUE 12 BY VALUE 16 BY 
            REFERENCE WS-NOME
           CALL "mvprintw" USING BY VALUE 14 BY VALUE 10 BY
            REFERENCE "Idade: "
           CALL "mvprintw" USING BY VALUE 14 BY VALUE 17 BY
            REFERENCE WS-IDADE-STR

            DISPLAY "WS-IDADE "WS-IDADE 
            DISPLAY "WS-IDADE-STR" WS-IDADE-STR
            DISPLAY "NOME" WS-NOME

           CALL "refresh"
            DISPLAY "WS-IDADE "WS-IDADE 
            DISPLAY "WS-IDADE-STR" WS-IDADE-STR
            DISPLAY "NOME" WS-NOME
           CALL "getch" USING BY REFERENCE WS-KEY

           CALL "endwin" *> Encerra ncurses
           STOP RUN.
