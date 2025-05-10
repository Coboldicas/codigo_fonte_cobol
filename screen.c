#include <ncurses.h>
#include <string.h>

void __attribute__((visibility("default"))) exibir_tela(char *nome, int *idade) {    
    initscr();
    noecho();
    cbreak();

    mvprintw(2, 10, "=== CADASTRO ===");

    mvprintw(4, 10, "Nome: ");
    refresh();
    getnstr(nome, 30);

    mvprintw(6, 10, "Idade: ");
    refresh();
    scanw("%d", idade);


    clear();
    mvprintw(10, 10, "Cadastro concluído!");
    mvprintw(12, 10, "Nome: %s", nome);
    mvprintw(14, 10, "Idade: %d", *idade);

    refresh();
    getch();
    endwin();
}
