      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 06/02/2025
      * DESCRICAO..: MODULO DE CADASRO DE USUARIO
      * NOME.......: CAD0002A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAD0002A.
      *================================================================*
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

      * Definição do tamanho máximo de cadastros
       01  WRK-MAX-TAB-INT             PIC 9(02) VALUE 10.
       01  WRK-IND1                    PIC 9(02) VALUE 1.

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

      *================================================================*
       PROCEDURE DIVISION USING COPY002A-REGISTRO.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

            PERFORM 0001-OBTER-DADOS-TELA
            PERFORM 9999-FINALIZAR
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DATA SISTEMA
      *----------------------------------------------------------------*
       0001-OBTER-DADOS-TELA           SECTION.
      *----------------------------------------------------------------*

           DISPLAY "SIMULACAO DE CADASTRO - DIGITE 'FIM' PARA SAIR".

           PERFORM UNTIL WRK-IND1 > WRK-MAX-TAB-INT
               DISPLAY "Nome...........: " ACCEPT WRK-NOME

               IF WRK-NOME EQUAL SPACES OR "FIM" 
                  IF WRK-IND1 EQUAL 1 
                     MOVE ZEROS TO WRK-IND1
                  ELSE 
                     SUBTRACT 1 FROM  WRK-IND1
                  END-IF
                  PERFORM 9999-FINALIZAR
               END-IF

               DISPLAY "Idade..........: "
               ACCEPT WRK-IDADE

               DISPLAY "Data nasc......: "
               ACCEPT WRK-DATA-NASC

               DISPLAY "Cargo..........: "
               ACCEPT WRK-CARGO

               DISPLAY "E-mail.........: "
               ACCEPT WRK-EMAIL    

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

               PERFORM 0002-MOVER-DADOS

               ADD 1 TO WRK-IND1
           END-PERFORM
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

            MOVE WRK-IND1 TO COPY002A-QUANT-REG

            IF COPY002A-QUANT-REG  NOT EQUAL ZEROS
               DISPLAY "DADOS CADASTRADOS COM SUCESSO!"
            END-IF 

            GOBACK.
            
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*