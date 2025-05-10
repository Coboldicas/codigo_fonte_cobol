       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTES-SQL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 WS-NOME      PIC X(50).
       01 WS-IDADE     PIC 99.
       EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE DIVISION.
       DISPLAY "Conectando ao banco..."
       EXEC SQL
           CONNECT TO "meubanco"
       END-EXEC

       IF SQLCODE NOT = 0
           DISPLAY "Erro ao conectar: " SQLCODE
           STOP RUN
       END-IF

       DISPLAY "Conectado. Lendo dados..."
       EXEC SQL
           SELECT nome, idade INTO :WS-NOME, :WS-IDADE FROM
            clientes WHERE id = 1
       END-EXEC

       DISPLAY "Nome: " WS-NOME
       DISPLAY "Idade: " WS-IDADE

       EXEC SQL DISCONNECT END-EXEC
       STOP RUN.
