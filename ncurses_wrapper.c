#include <ncurses.h>

void iniciar_ncurses() {
    initscr();            // Inicia o ncurses
    cbreak();             // Habilita entrada sem buffer
    noecho();             // Não mostra caracteres digitados
    keypad(stdscr, TRUE); // Habilita teclas especiais
    curs_set(0);          // Esconde o cursor
}

void finalizar_ncurses() {
    endwin(); // Encerra o ncurses
}

void mostrar_mensagem(const char *mensagem) {
    mvprintw(5, 10, "%s", mensagem); // Mostra a mensagem na tela
    refresh(); // Atualiza a tela
}

void esperar_tecla() {
    getch(); // Aguarda a entrada do usuário
}
