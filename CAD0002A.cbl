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

       SCREEN SECTION.
       01 TELA-CADASTRO.
        05 BLANK SCREEN.
        05 LINE 02 COLUMN 10 VALUE "==== CADASTRO DE USUARIO ====".
        05 LINE 04 COLUMN 02 VALUE "Nome...........: ".
        05 LINE 04 COLUMN 20 PIC X(30) USING WRK-NOME.
        05 LINE 05 COLUMN 02 VALUE "Idade..........: ".
        05 LINE 05 COLUMN 20 PIC 9(02) USING WRK-IDADE.
        05 LINE 06 COLUMN 02 VALUE "Data nasc......: ".
        05 LINE 06 COLUMN 20 PIC 9(08) USING WRK-DATA-NASC.
        05 LINE 07 COLUMN 02 VALUE "Cargo..........: ".
        05 LINE 07 COLUMN 20 PIC X(20) USING WRK-CARGO.
        05 LINE 08 COLUMN 02 VALUE "E-mail.........: ".
        05 LINE 08 COLUMN 20 PIC X(50) USING WRK-EMAIL.
        05 LINE 09 COLUMN 02 VALUE "Telefone.......: ".
        05 LINE 09 COLUMN 20 PIC 9(09) USING WRK-TELEFONE.
        05 LINE 10 COLUMN 02 VALUE "Rua............: ".
        05 LINE 10 COLUMN 20 PIC X(50) USING WRK-RUA.
        05 LINE 11 COLUMN 02 VALUE "Cidade.........: ".
        05 LINE 11 COLUMN 20 PIC X(30) USING WRK-CIDADE.
        05 LINE 12 COLUMN 02 VALUE "Estado.........: ".
        05 LINE 12 COLUMN 20 PIC X(02) USING WRK-ESTADO.
        05 LINE 13 COLUMN 02 VALUE "CEP............: ".
        05 LINE 13 COLUMN 20 PIC 9(08) USING WRK-CEP.
        05 LINE 15 COLUMN 10 VALUE "Digite e pressione Enter".

      *================================================================*
       PROCEDURE DIVISION USING COPY002A-REGISTRO.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           IF COPY002A-QUANT-REG       LESS  50
              PERFORM 0001-OBTER-DADOS-TELA
           ELSE 
              DISPLAY "QUANTIDADE DE LIDOS REGISTROS MAIOR QUE 50"
           END-IF

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


           DISPLAY "          CADASTRO DE USUARIO          "
           DISPLAY "        PARA SAIR - DIGITE 'FIM'       "
           MOVE COPY002A-QUANT-REG     TO WRK-IND1 
           ADD 1                       TO WRK-IND1
      
      *    PERFORM UNTIL WRK-IND1 > WRK-MAX-TAB-INT
               DISPLAY TELA-CADASTRO
               ACCEPT TELA-CADASTRO
      *        DISPLAY "Nome...........: " ACCEPT WRK-NOME 
               IF WRK-NOME          EQUAL SPACES
                                       OR "FIM" 
                  SUBTRACT 1         FROM WRK-IND1
                  IF WRK-IND1     GREATER ZEROS
                     MOVE WRK-IND1     TO COPY002A-QUANT-REG
                  END-IF
                  PERFORM 9999-FINALIZAR
               END-IF

      *        DISPLAY "Idade..........: "
      *        ACCEPT WRK-IDADE

      *        DISPLAY "Data nasc......: "
      *        ACCEPT WRK-DATA-NASC

      *        DISPLAY "Cargo..........: "
      *        ACCEPT WRK-CARGO

      *        DISPLAY "E-mail.........: "
      *        ACCEPT WRK-EMAIL    

      *        DISPLAY "Telefone.......: "               
      *        ACCEPT WRK-TELEFONE 

      *        DISPLAY "Rua............: "
      *        ACCEPT WRK-RUA      

      *        DISPLAY "Cidade.........: "
      *        ACCEPT WRK-CIDADE   

      *        DISPLAY "Estado.........: "
      *        ACCEPT WRK-ESTADO   

      *        DISPLAY "CEP............: "
      *        ACCEPT WRK-CEP      

               PERFORM 0002-MOVER-DADOS

               ADD 1                   TO WRK-IND1
      *    END-PERFORM

           SUBTRACT 1                FROM WRK-IND1
           MOVE WRK-IND1               TO COPY002A-QUANT-REG
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