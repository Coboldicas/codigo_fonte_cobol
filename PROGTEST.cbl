       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGTEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NOME          PIC X(30).
       01 WS-IDADE         PIC 9(2).
      *01 WS-MENSAGEM      PIC X(24) VALUE  "Digite e pressione Enter".

       SCREEN SECTION.
       01 TELA-CADASTRO.
          05 BLANK SCREEN.
          05 LINE 02 COLUMN 10 VALUE "==== CADASTRO DE USUARIO ====".
          05 LINE 04 COLUMN 10 VALUE "Nome: ".
          05 LINE 04 COLUMN 17 PIC X(30) USING WS-NOME.
          05 LINE 06 COLUMN 10 VALUE "Idade: ".
          05 LINE 06 COLUMN 18 PIC 9(2) USING WS-IDADE.
          05 LINE 08 COLUMN 10 VALUE "Digite e pressione Enter".

       PROCEDURE DIVISION.
      *INICIO.
           DISPLAY TELA-CADASTRO.
           ACCEPT TELA-CADASTRO.

           *> Exibindo os dados inseridos
           DISPLAY "Usuario cadastrado sucesso!" AT LINE 12 COLUMN 10.
      *    AT LINE 12 COLUMN 10.
           DISPLAY "Nome: " AT LINE 13 COLUMN 10 WS-NOME.
           DISPLAY "Idade: " AT LINE 14 COLUMN 10 WS-IDADE.

           STOP RUN.
