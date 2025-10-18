       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG021A.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-USUARIOS ASSIGN TO 'USUARIO.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-USUARIO.

       DATA DIVISION.
       FILE SECTION.
       FD ARQ-USUARIOS.
       01 REG-USUARIO.
           05 NOME-USUARIO  PIC X(20).
           05 SENHA-USUARIO PIC X(10).

       WORKING-STORAGE SECTION.
       01 FS-USUARIO         PIC XX.
       01 FIM-ARQUIVO        PIC X     VALUE 'N'.
       01 LOGIN-VALIDO       PIC X     VALUE 'N'.

       01 ENTRADA-USUARIO.
           05 USER-INFORMADO  PIC X(20) VALUE SPACES.
           05 SENHA-INFORMADA PIC X(10) VALUE SPACES.

       01 MSG-LOGIN-OK       PIC X(50) VALUE
                                   "Login realizado com sucesso!".
       01 MSG-LOGIN-ERRO     PIC X(50) VALUE
                                   "Usuário ou senha inválidos.".

       PROCEDURE DIVISION.
       INICIO-PROGRAMA.

           DISPLAY 'DIGITE USUARIO: '
           ACCEPT USER-INFORMADO
           DISPLAY 'DIGITE SENHA: '
           ACCEPT SENHA-INFORMADA

           OPEN INPUT ARQ-USUARIOS

           IF FS-USUARIO NOT = "00"
               DISPLAY "Erro ao abrir o arquivo. STATUS: " FS-USUARIO
               STOP RUN
           END-IF

           PERFORM ACHAR-USUARIO UNTIL FIM-ARQUIVO = 'S' 
                                    OR LOGIN-VALIDO = 'S'

           IF LOGIN-VALIDO = 'S'
               DISPLAY MSG-LOGIN-OK
           ELSE
               DISPLAY MSG-LOGIN-ERRO
           END-IF

           CLOSE ARQ-USUARIOS
           STOP RUN.

       ACHAR-USUARIO.
           READ ARQ-USUARIOS
               AT END
                   MOVE 'S' TO FIM-ARQUIVO
               NOT AT END
                   IF NOME-USUARIO = USER-INFORMADO
                      AND SENHA-USUARIO = SENHA-INFORMADA
                       MOVE 'S' TO LOGIN-VALIDO
                   END-IF
           END-READ.
