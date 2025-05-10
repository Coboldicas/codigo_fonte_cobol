       identification division.
       program-id. MAINCOBOL.

       data division.
       working-storage section.
       01 WS-NOME        PIC X(30).
       01 WS-IDADE       PIC 9(2).

       procedure division.
      * main-section.

           *> Carregar a biblioteca compartilhada
           CALL "exibir_tela" USING BY REFERENCE WS-NOME, WS-IDADE.

           *> Exibe os dados retornados pelo C
           DISPLAY "Nome digitado: " WS-NOME.
           DISPLAY "Idade digitada: " WS-IDADE.

      *scanw("%d", idade);
           STOP RUN.
