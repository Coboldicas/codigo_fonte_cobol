      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 16/06/2025
      * DESCRICAO..: SIMULACAO DE LOGIN (USUARIO, SENHA)
      * NOME.......: LOGIN01A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGIN01A.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.                
           DECIMAL-POINT IS COMMA.  
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT ARQ-USUARIOS ASSIGN TO 'USUARIO.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS FS-USUARIO.

       DATA DIVISION.
       FILE SECTION.

       FD ARQ-USUARIOS.
       01 REG-USUARIO.
           05 NOME-USUARIO            PIC X(20).
           05 SENHA-USUARIO           PIC X(10).

       WORKING-STORAGE SECTION.
       01 FS-USUARIO          PIC XX.
       01 FIM-ARQUIVO         PIC X VALUE 'N'.
       01 LOGIN-VALIDO        PIC X VALUE 'N'.
       01 WRK-IND             PIC 9(03).

       01  ENTRADA-USUARIO.
         05 USER-INFORMADO           PIC X(20).
         05 SENHA-INFORMADA          PIC X(10).

       01  MSG-LOGIN-OK      PIC X(50) VALUE 
            'LOGIN REALIZADO COM SUCESSSO!'.

       01  MSG-LOGIN-ERRO    PIC X(50) VALUE  
            ' USUARIO E SENHA INVALIDOS!'.

      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

            PERFORM 0001-ABRE-ARQUIVO 
            PERFORM 0002-OBTER-DADOS
            PERFORM 0007-LER-ARQUIVO
            PERFORM 0005-VALIDAR-USUARIO UNTIL FIM-ARQUIVO = 'S'
                                         OR LOGIN-VALIDO EQUAL 'S'
            PERFORM 0006-FECHA-ARQUIVO 
            PERFORM 9999-FINALIZAR
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ABERTURA DE ARQUIVO
      *----------------------------------------------------------------*
       0001-ABRE-ARQUIVO               SECTION.
      *----------------------------------------------------------------*

           OPEN  INPUT ARQ-USUARIOS

           IF FS-USUARIO NOT = "00"
              DISPLAY 'ERRO NA ABERTURA DE ARQUIVO.' 
              DISPLAY 'FILE STATUS: ' FS-USUARIO 
              STOP RUN
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DADOS DO USUARIO
      *----------------------------------------------------------------*
       0002-OBTER-DADOS                SECTION.
      *----------------------------------------------------------------*

            DISPLAY 'DIGITE USUARIO: '
            ACCEPT USER-INFORMADO
            DISPLAY 'DIGITE A SENHA: '
            ACCEPT SENHA-INFORMADA  
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR DADOS DO USUARIO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-VALIDAR-USUARIO
       0005-VALIDAR-USUARIO             SECTION.
      *----------------------------------------------------------------*

           IF NOME-USUARIO EQUAL USER-INFORMADO
           AND SENHA-USUARIO EQUAL SENHA-INFORMADA
               MOVE 'S' TO LOGIN-VALIDO 
           ELSE
              PERFORM 0007-LER-ARQUIVO
           END-IF  
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       0006-FECHA-ARQUIVO              SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQ-USUARIOS 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-end
       0006-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA ARQUIVO DE ENTRADA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-ler-arquivo
       0007-LER-ARQUIVO                SECTION.
      *----------------------------------------------------------------*

           READ ARQ-USUARIOS
               AT END
                   MOVE 'S' TO FIM-ARQUIVO
           END-READ

           IF FS-USUARIO EQUAL "00"
              ADD 1 TO WRK-IND
           ELSE  
               IF WRK-IND EQUAL 0
                  IF FS-USUARIO EQUAL "10"
                     DISPLAY 'ARQUIVO VAZIO.' 
                  ELSE
                     DISPLAY 'ERRO NA LEITURA DE ARQUIVO.' 
                     DISPLAY 'FILE STATUS: ' FS-USUARIO 
                  END-IF
              END-IF
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-end
       0007-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           IF WRK-IND GREATER 0 
              IF LOGIN-VALIDO = 'S'
                 DISPLAY MSG-LOGIN-OK
               ELSE
                DISPLAY MSG-LOGIN-ERRO
               END-IF
           END-IF  
 
           STOP RUN 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*

