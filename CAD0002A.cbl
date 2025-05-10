      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 06/02/2025
      * DESCRICAO..: MODULO DE CADASTRO DE USUARIO - POR TELA
      * NOME.......: CAD0002A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAD0002A.
      *================================================================*
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

      * Definição do tamanho máximo de cadastros
       01  WRK-MAX-TAB-INT             PIC 9(02) VALUE 50.
       01  WRK-IND1                    PIC 9(02) VALUE ZEROS.
       01  WRK-IND2                    PIC 9(02) VALUE ZEROS.
       01  WRK-CONTEM-REG              PIC X(01) VALUE 'N'.
       01  WRK-FIM-CAD                 PIC X(01) VALUE 'N'.
       01  WRK-CHAR-ATUAL              PIC X(01) VALUE SPACES.
       01  WRK-NOME-VALIDO             PIC X(01) VALUE 'N'.
       01  WRK-IDADE-VALIDA            PIC X(01) VALUE 'N'.
       01  WRK-DATA-NASC-VALIDA        PIC X(01) VALUE 'N'.
       01  WRK-CONTADOR                PIC 9(03) VALUE ZEROS.
       01  WRK-TAMANHO                 PIC 9(02) VALUE ZEROS.

       01  WRK-DATA-NASC-AUX.
         05 WRK-DATA-NASC-DD                       PIC 9(02).
         05 FILLER                                 PIC X(01).
         05 WRK-DATA-NASC-MM                       PIC 9(02).
         05 FILLER                                 PIC X(01).
         05 WRK-DATA-NASC-AAAA                     PIC 9(04).

      * Variáveis auxiliares
       01  WRK-AUXILIAR.
        05 WRK-COD                     PIC 9(02).
        05 WRK-NOME                    PIC X(30).
        05 WRK-IDADE                   PIC 9(02).
        05 WRK-DATA-NASC               PIC 9(08).
        05 WRK-CARGO                   PIC X(20).    
        05 WRK-EMAIL                   PIC X(50).
        05 WRK-TELEFONE                PIC 9(09).
        05 WRK-RUA                     PIC X(50).
        05 WRK-CIDADE                  PIC X(30).
        05 WRK-ESTADO                  PIC X(02).
        05 WRK-CEP                     PIC 9(08).

       LINKAGE SECTION.
      * Definição da estrutura do cadastro
       COPY COPY002A.

      *SCREEN SECTION.
      *01 TELA-CADASTRO.
      * 05 BLANK SCREEN.
      * 05 LINE 02 COLUMN 10 VALUE "==== CADASTRO DE USUARIO ====".
      * 05 LINE 04 COLUMN 02 VALUE "Nome...........: ".
      * 05 LINE 04 COLUMN 20 PIC X(30) USING WRK-NOME.
      * 05 LINE 05 COLUMN 02 VALUE "Idade..........: ".
      * 05 LINE 05 COLUMN 20 PIC 9(02) USING WRK-IDADE.
      * 05 LINE 06 COLUMN 02 VALUE "Data nasc......: ".
      * 05 LINE 06 COLUMN 20 PIC 9(08) USING WRK-DATA-NASC.
      * 05 LINE 07 COLUMN 02 VALUE "Cargo..........: ".
      * 05 LINE 07 COLUMN 20 PIC X(20) USING WRK-CARGO.
      * 05 LINE 08 COLUMN 02 VALUE "E-mail.........: ".
      * 05 LINE 08 COLUMN 20 PIC X(50) USING WRK-EMAIL.
      * 05 LINE 09 COLUMN 02 VALUE "Telefone.......: ".
      * 05 LINE 09 COLUMN 20 PIC 9(09) USING WRK-TELEFONE.
      * 05 LINE 10 COLUMN 02 VALUE "Rua............: ".
      * 05 LINE 10 COLUMN 20 PIC X(50) USING WRK-RUA.
      * 05 LINE 11 COLUMN 02 VALUE "Cidade.........: ".
      * 05 LINE 11 COLUMN 20 PIC X(30) USING WRK-CIDADE.
      * 05 LINE 12 COLUMN 02 VALUE "Estado.........: ".
      * 05 LINE 12 COLUMN 20 PIC X(02) USING WRK-ESTADO.
      * 05 LINE 13 COLUMN 02 VALUE "CEP............: ".
      * 05 LINE 13 COLUMN 20 PIC 9(08) USING WRK-CEP.
      * 05 LINE 15 COLUMN 10 VALUE "Digite e pressione Enter".


      *================================================================*
       PROCEDURE DIVISION USING COPY002A-REGISTRO.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF COPY002A-QUANT-REG     LESS 50
              MOVE COPY002A-QUANT-REG  TO WRK-IND1
              PERFORM 0001-OBTER-DADOS-TELA 
                                     UNTIL WRK-FIM-CAD
                                     EQUAL 'S'
           ELSE 
              DISPLAY "QUANTIDADE DE LIDOS REGISTROS MAIOR QUE 50"
           END-IF

           MOVE WRK-IND1 TO COPY002A-QUANT-REG

           PERFORM 9999-FINALIZAR
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DADOS DA TELA
      *----------------------------------------------------------------*
       0001-OBTER-DADOS-TELA           SECTION.
      *----------------------------------------------------------------*
              INITIALIZE WRK-AUXILIAR
                      REPLACING ALPHANUMERIC BY SPACES
                                     NUMERIC BY ZEROES
              MOVE 'N'                 TO WRK-NOME-VALIDO
           IF WRK-CONTEM-REG      EQUAL 'N'
              DISPLAY SPACE
              DISPLAY "============================================="
              DISPLAY "         CADASTRO DE USUÁRIO                 "
              DISPLAY "============================================="
              DISPLAY SPACE
              DISPLAY "Digite 'FIM' no campo Nome para sair."
           ELSE
              DISPLAY SPACE
              DISPLAY "Deseja inserir mais registros?"
              DISPLAY "Digite o Nome ou digite 'FIM' para sair"
           END-IF
           DISPLAY "Nome...........: " 
           PERFORM 0011-VALIDAR-NOME UNTIL WRK-NOME-VALIDO EQUAL 'S'
           IF  WRK-NOME       NOT EQUAL SPACES
           AND WRK-NOME       NOT EQUAL "FIM"
               ADD 1                 TO WRK-IND1

               DISPLAY "Idade..........: " 
               PERFORM 0012-VALIDAR-IDADE UNTIL WRK-IDADE-VALIDA
                                          EQUAL 'S'
               DISPLAY "Data nasc......: " 
               PERFORM 0014-VALIDAR-DATA-NASC 
                                          UNTIL WRK-DATA-NASC-VALIDA
                                          EQUAL 'S'
               DISPLAY "Cargo..........: " 
               PERFORM 0015-VALIDAR-CARGO
    
               DISPLAY "E-mail.........: " 
               PERFORM 0016-VALIDAR-EMAIL
    
               DISPLAY "Telefone.......: " 
               ACCEPT WRK-TELEFONE
    
               DISPLAY "Rua............: " 
               ACCEPT WRK-RUA
    
               DISPLAY "Cidade.........: " 
               ACCEPT WRK-CIDADE
    
               DISPLAY "Estado.........: " 
               ACCEPT WRK-ESTADO
    
               DISPLAY "CEP............: " 
               ACCEPT WRK-CEP
               MOVE 'S'                TO WRK-CONTEM-REG 
           ELSE
               MOVE 'S'                TO WRK-FIM-CAD
               MOVE 'N'                TO WRK-CONTEM-REG
           END-IF

           IF WRK-CONTEM-REG   EQUAL 'S'
              PERFORM 0002-MOVER-DADOS
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    MOVIMENTAR DADOS DA TELA PARA BOOK 
      *----------------------------------------------------------------*
       0002-MOVER-DADOS                SECTION.
      *----------------------------------------------------------------*

            MOVE WRK-IND1              TO COPY002A-COD      (WRK-IND1)
            MOVE WRK-NOME              TO COPY002A-NOME     (WRK-IND1)
            MOVE WRK-IDADE             TO COPY002A-IDADE    (WRK-IND1)
            MOVE WRK-DATA-NASC         TO COPY002A-DATA-NASC(WRK-IND1)
            MOVE WRK-CARGO             TO COPY002A-CARGO    (WRK-IND1)  
            MOVE WRK-EMAIL             TO COPY002A-EMAIL    (WRK-IND1)
            MOVE WRK-TELEFONE          TO COPY002A-TELEFONE (WRK-IND1)
            MOVE WRK-RUA               TO COPY002A-RUA      (WRK-IND1)
            MOVE WRK-CIDADE            TO COPY002A-CIDADE   (WRK-IND1)
            MOVE WRK-ESTADO            TO COPY002A-ESTADO   (WRK-IND1)
            MOVE WRK-CEP               TO COPY002A-CEP      (WRK-IND1)
            .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR DADOS DE ENTRADA - NOME
      *----------------------------------------------------------------*
       0011-VALIDAR-NOME               SECTION.
      *----------------------------------------------------------------*

           MOVE ZEROS                  TO WRK-CONTADOR

           ACCEPT WRK-NOME
          
           PERFORM VARYING WRK-IND2 FROM 1 BY 1 UNTIL
               WRK-IND2 > LENGTH OF WRK-NOME
               MOVE WRK-NOME(WRK-IND2:1) TO WRK-CHAR-ATUAL 
               IF WRK-CHAR-ATUAL  IS NUMERIC
                   ADD 1 TO WRK-CONTADOR
               END-IF
           END-PERFORM
           IF WRK-CONTADOR     EQUAL ZEROS 
                   MOVE 'S'       TO WRK-NOME-VALIDO
           ELSE
                   MOVE 'N'       TO WRK-NOME-VALIDO
           END-IF
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0011-end
       0011-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR DADOS DE ENTRADA - IDADE
      *----------------------------------------------------------------*
       0012-VALIDAR-IDADE              SECTION.
      *----------------------------------------------------------------*

               ACCEPT WRK-IDADE

               EVALUATE WRK-IDADE 
               WHEN ZEROS 
                  DISPLAY 'FAVOR INFORMAR IDADE CORRETA'
                  MOVE 'N'             TO WRK-IDADE-VALIDA
               WHEN LESS 18
                  DISPLAY 'CADASTRO NAO PERMITIDO PARA MENORES DE 18'
                  MOVE 'N'             TO WRK-IDADE-VALIDA
               WHEN OTHER 
                  MOVE 'S'             TO WRK-IDADE-VALIDA
               END-EVALUATE
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0012-end
       0012-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR DADOS DE ENTRADA - DATA DE NASCIMENTO
      *----------------------------------------------------------------*
       0014-VALIDAR-DATA-NASC          SECTION.
      *----------------------------------------------------------------*

              ACCEPT WRK-DATA-NASC-AUX

              EVALUATE WRK-DATA-NASC-DD
              WHEN ZEROS 
                   DISPLAY 'DIA NAO INFORMADO'
                   MOVE 'N' TO WRK-DATA-NASC-VALIDA
                   GO TO 0014-END
              WHEN GREATER 31
                   DISPLAY 'DIA INVALIDO'
                   MOVE 'N' TO WRK-DATA-NASC-VALIDA
                   GO TO 0014-END
              WHEN IS NOT NUMERIC
                   DISPLAY 'DIA INVALIDO'
                   MOVE 'N' TO WRK-DATA-NASC-VALIDA
                   GO TO 0014-END
              WHEN OTHER
                   MOVE 'S' TO WRK-DATA-NASC-VALIDA                 
              END-EVALUATE

              EVALUATE WRK-DATA-NASC-MM
              WHEN ZEROS 
                   DISPLAY 'MES NAO INFORMADO'
                   MOVE 'N' TO WRK-DATA-NASC-VALIDA
                   GO TO 0014-END
              WHEN GREATER 12
                   DISPLAY 'MES INVALIDO'
                   MOVE 'N' TO WRK-DATA-NASC-VALIDA
                   GO TO 0014-END
              WHEN 02
                   IF WRK-DATA-NASC-DD GREATER 28
                      DISPLAY 'DIA INVALIDO'                   
                      MOVE 'N' TO WRK-DATA-NASC-VALIDA
                      GO TO 0014-END
                   ELSE 
                      MOVE 'S' TO WRK-DATA-NASC-VALIDA
                   END-IF
              WHEN IS NOT NUMERIC
                   DISPLAY 'MES INVALIDO'
                   MOVE 'N' TO WRK-DATA-NASC-VALIDA
                   GO TO 0014-END
              WHEN OTHER
                   MOVE 'S' TO WRK-DATA-NASC-VALIDA                 
              END-EVALUATE

              EVALUATE WRK-DATA-NASC-AAAA
              WHEN ZEROS
                  DISPLAY 'ANO INVALIDO'
                  MOVE 'N' TO WRK-DATA-NASC-VALIDA
                  GO TO 0014-END
              WHEN IS NOT NUMERIC
                   DISPLAY 'ANO INVALIDO'
                   MOVE 'N' TO WRK-DATA-NASC-VALIDA
                   GO TO 0014-END
              WHEN OTHER
                  MOVE 'S' TO WRK-DATA-NASC-VALIDA
              END-EVALUATE

              MOVE WRK-DATA-NASC-DD   TO WRK-DATA-NASC(1:2)
              MOVE WRK-DATA-NASC-MM   TO WRK-DATA-NASC(3:2)
              MOVE WRK-DATA-NASC-AAAA TO WRK-DATA-NASC(5:4)
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0014-end
       0014-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR DADOS DE ENTRADA - CARGO
      *----------------------------------------------------------------*
       0015-VALIDAR-CARGO              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-CARGO
                      WRK-TAMANHO

           ACCEPT WRK-CARGO

           INSPECT FUNCTION REVERSE(WRK-CARGO) TALLYING  WRK-TAMANHO 
                   FOR LEADING SPACES 

           SUBTRACT LENGTH OF WRK-CARGO FROM WRK-TAMANHO

           IF WRK-TAMANHO GREATER 20
              DISPLAY 'TAMANHO DO CAMPO CARGO EXCEDENTE'
              GO TO 0015-VALIDAR-CARGO
           END-IF
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0015-end
       0015-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR DADOS DE ENTRADA - EMAIL
      *----------------------------------------------------------------*
       0016-VALIDAR-EMAIL              SECTION.
      *----------------------------------------------------------------*

           INITIALIZE WRK-EMAIL
                      WRK-TAMANHO

           ACCEPT WRK-EMAIL

           IF WRK-EMAIL NOT EQUAL SPACES
      *     VALIDAR @ OBRIGATORIO
              INSPECT WRK-EMAIL TALLYING  WRK-TAMANHO 
                      FOR ALL '@' 
              IF WRK-TAMANHO EQUAL ZEROS
                 DISPLAY 'EMAIL INVALIDO'
                 GO TO 0016-VALIDAR-EMAIL
              END-IF

              INITIALIZE WRK-TAMANHO

      *    VALIDAR ESPACOS ANTES DO @
              INSPECT WRK-EMAIL TALLYING  WRK-TAMANHO 
                      FOR ALL ' @' 
              IF WRK-TAMANHO GREATER 0
                 DISPLAY 'EMAIL INVALIDO'
                 GO TO 0016-VALIDAR-EMAIL
              END-IF

              INITIALIZE WRK-TAMANHO

      *    VALIDAR ESPACOS APOS DO @
              INSPECT WRK-EMAIL TALLYING  WRK-TAMANHO 
                      FOR ALL '@ ' 
              IF WRK-TAMANHO GREATER 0
                 DISPLAY 'EMAIL INVALIDO'
                 GO TO 0016-VALIDAR-EMAIL
              END-IF

              INITIALIZE WRK-TAMANHO

      *    VALIDAR EMAIL COMECA COM ESPACOS 
              INSPECT WRK-EMAIL TALLYING wrk-tamanho FOR
                      LEADING SPACES 
              IF WRK-TAMANHO GREATER 0
                 DISPLAY 'EMAIL INVALIDO'
                 GO TO 0016-VALIDAR-EMAIL
              END-IF
           END-IF 
           .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0016-end
       0016-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

            IF COPY002A-QUANT-REG  NOT EQUAL ZEROS
               DISPLAY "DADOS CADASTRADOS COM SUCESSO!"
            END-IF 

            GOBACK.
            
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*