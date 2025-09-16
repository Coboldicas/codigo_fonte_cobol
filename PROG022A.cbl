      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 15/09/2025
      * DESCRICAO..: VALIDACAO DE CAMPOS OBRIGATORIOS 
      * NOME.......: PROG022A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG022A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WRK-NOME                    PIC X(30) VALUE SPACES.
       01  WRK-IDADE                   PIC 9(02) VALUE ZEROS. 

       01  WRK-VAL-NOME                PIC X(01) VALUE SPACES.
       01  WRK-VAL-IDADE               PIC X(01) VALUE SPACES. 
       01  WRK-VAL-NUMERICO            PIC X(01) VALUE SPACES. 
       01  WRK-IND1                    PIC 9(02) VALUE ZEROS.
       01  WRK-QTDE                    PIC 9(02) VALUE ZEROS.
       01  WRK-QTDE2                   PIC 9(02) VALUE ZEROS.
       01  WRK-CARACTERE               PIC X(01) VALUE SPACES.
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0001-VALIDACAO-NOME  
                                       UNTIL WRK-VAL-NOME
                                       EQUAL 'S'
           PERFORM 0002-VALIDACAO-IDADE 
                                       UNTIL WRK-VAL-IDADE
                                       EQUAL 'S'
           PERFORM 0003-EMITIR-DADOS
           PERFORM 9999-FINALIZAR
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR DADOS DE ENTRADA - CAMPO NOME
      *----------------------------------------------------------------*
       0001-VALIDACAO-NOME             SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-VAL-NUMERICO
           MOVE ZEROS                  TO WRK-QTDE2
                                          WRK-QTDE

           DISPLAY 'DIGITE O NOME: ' 
                    WITH NO ADVANCING
           ACCEPT WRK-NOME

           INSPECT FUNCTION REVERSE (WRK-NOME) 
                   TALLYING WRK-QTDE FOR LEADING SPACES
           COMPUTE WRK-QTDE2 = WRK-QTDE - 30

           IF WRK-NOME                 EQUAL SPACES 
              DISPLAY '>>> ERRO: CAMPO NOME OBRIGATORIO'
              MOVE 'N'                 TO WRK-VAL-NOME
           ELSE
              PERFORM VARYING WRK-IND1 FROM 1 BY 1 
                                       UNTIL WRK-IND1 > 
                                       WRK-QTDE2
              MOVE WRK-NOME(WRK-IND1:1) 
                                       TO WRK-CARACTERE
              IF  WRK-CARACTERE        >= "0" 
              AND WRK-CARACTERE        <= "9"
                  MOVE 'S'             TO WRK-VAL-NUMERICO
              ELSE 
                  MOVE 'N'             TO WRK-VAL-NUMERICO
              END-IF 
              END-PERFORM
              IF WRK-VAL-NUMERICO      EQUAL 'S'
                 MOVE 'N'              TO WRK-VAL-NOME
                 DISPLAY '>>> ERRO: CAMPO NOME NAO PODE CONTER NUMERAL'
              ELSE 
                 MOVE 'S'              TO WRK-VAL-NOME
              END-IF 
            END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR DADOS DE ENTRADA - CAMPO IDADE
      *----------------------------------------------------------------*
       0002-VALIDACAO-IDADE            SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE A IDADE (ANOS): ' 
                    WITH NO ADVANCING
           ACCEPT WRK-IDADE

           IF WRK-IDADE                EQUAL ZEROS 
              DISPLAY '>>> ERRO: A IDADE NAO PODE SER ZERO'
              MOVE 'N'                 TO WRK-VAL-IDADE
           ELSE
              MOVE 'S'                 TO WRK-VAL-IDADE
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EXIBIR DADOS DIGITADOS NA TELA
      *----------------------------------------------------------------*
       0003-EMITIR-DADOS              SECTION.
      *----------------------------------------------------------------*

           DISPLAY ' CADASTRO EFETUADO COM SUCESSO!'
           DISPLAY 'NOME INFORMADO: ' WRK-NOME
           DISPLAY 'IDADE INFORMADA: ' WRK-IDADE ' ANOS'
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-finalizar
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
           DISPLAY 'FIM DE PROGRAMA'
           STOP RUN 
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*

