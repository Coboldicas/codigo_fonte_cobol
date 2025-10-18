       identification division.
       program-id. NCURSESDEMO.

       data division.
       working-storage section.
       01 WS-NOME        PIC X(30).
       01 WS-IDADE       PIC 9(2).
       01 WS-KEY         PIC 9(2).

       procedure division.
      *main-section.
           CALL "initscr"         *> Inicia o ncurses
           CALL "cbreak"          *> Permite entrada sem precisar pressionar ENTER
           CALL "noecho"          *> Desativa a exibição automática de entrada
           CALL "clear"

           *> Exibe o título
           CALL "mvprintw" USING BY VALUE 2 BY VALUE 10 BY
            REFERENCE "=== CADASTRO ==="

           *> Solicita o nome
           CALL "mvprintw" USING BY VALUE 4 BY VALUE 10 BY
            REFERENCE "Nome: "
           CALL "refresh"
           CALL "scanw" USING BY REFERENCE WS-NOME

           *> Solicita a idade
           CALL "mvprintw" USING BY VALUE 6 BY VALUE 10 BY
            REFERENCE "Idade: "
           CALL "refresh"
           CALL "scanw" USING BY REFERENCE WS-IDADE

           *> Limpa a tela e exibe os dados cadastrados
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
            REFERENCE WS-IDADE

           *> Atualiza a tela
           CALL "refresh"

           *> Aguarda uma tecla antes de sair
           CALL "getch" USING BY REFERENCE WS-KEY
           CALL "endwin" *> Encerra o ncurses

           STOP RUN.
