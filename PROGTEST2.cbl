       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGTEST2.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-NOME  PIC X(30).
       01 WS-IDADE PIC 9(2).
       01 WS-KEY   PIC 9(2).

       PROCEDURE DIVISION.
      *> cobol-lint CL002 main-process
       MAIN-PROCESS.

           *> Inicia ncurses
           CALL "initscr"
           CALL "keypad" USING BY VALUE 1 BY VALUE 1
           CALL "noecho"
           CALL "clear"

           *> Exibe a tela inicial
           CALL "mvprintw" USING BY VALUE 2 BY VALUE 10 BY
            REFERENCE "=== CADASTRO DE USUÁRIO ==="
           CALL "mvprintw" USING BY VALUE 4 BY VALUE 10 BY
            REFERENCE "Nome: "
           CALL "mvprintw" USING BY VALUE 6 BY VALUE 10 BY 
            REFERENCE "Idade: "

           *> Pede a entrada do usuário
           CALL "refresh"
           CALL "mvscanw" USING BY VALUE 4 BY VALUE 17 BY 
            REFERENCE WS-NOME
           CALL "mvscanw" USING BY VALUE 6 BY VALUE 17 BY
            REFERENCE WS-IDADE

           *> Exibe os dados informados
           CALL "clear"
           CALL "mvprintw" USING BY VALUE  8 BY VALUE 10 BY
            REFERENCE "Cadastro concluído!"
           CALL "mvprintw" USING BY VALUE 10 BY VALUE 10 BY
            REFERENCE "Nome digitado: "
           CALL "mvprintw" USING BY VALUE 10 BY VALUE 27 BY
            REFERENCE WS-NOME
           CALL "mvprintw" USING BY VALUE 12 BY VALUE 10 BY
            REFERENCE "Idade digitada: "
           CALL "mvprintw" USING BY VALUE 12 BY VALUE 27 BY
            REFERENCE WS-IDADE

           CALL "refresh"
           CALL "getch" USING BY REFERENCE WS-KEY

           *> Finaliza ncurses
           CALL "endwin"
           STOP RUN.
