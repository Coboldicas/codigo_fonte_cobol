      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 07/03/2025
      * DESCRICAO..: MODULO DE CÇASSIFICACAO DE REGISTRO
      * NOME.......: SORT001A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT001A.
      *================================================================*
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 WRK-QTD-REG                  PIC 9(2) VALUE ZEROS.
       01 WRK-IND1                     PIC 9(2) VALUE 1.
       01 WRK-IND2                     PIC 9(2) VALUE 1.
       01 WRK-TEMP                     PIC X(220).
       01 WRK-FLAG-CLASS               PIC X(1).

       01  WRK-TABELA.
        05  WRK-CADUSUAR OCCURS 50 TIMES.
         10 WRK-CADASTRO.
         15 WRK-COD                PIC 9(02).
         15 WRK-NOME               PIC X(30).
         15 WRK-IDADE              PIC 9(02).
         15 WRK-DATA-NASC          PIC 9(08).
         15 WRK-CARGO              PIC X(20).    
         15 WRK-EMAIL              PIC X(50).
         15 WRK-TELEFONE           PIC 9(09).
         15 WRK-ENDERECO.
          20 WRK-RUA               PIC X(50).
          20 WRK-CIDADE            PIC X(30).
          20 WRK-ESTADO            PIC X(02).
          20 WRK-CEP               PIC 9(08).

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

           DISPLAY "DESEJA CLASSICAR OS REGISTROS POR NOME?"
           DISPLAY "       DIGITE [S]SIM  OU  [N]NAO       ".

           ACCEPT WRK-FLAG-CLASS 

           IF WRK-FLAG-CLASS    NOT EQUAL 'S' AND 'N' AND SPACES
              DISPLAY "FAVOR DIGITAR 'S' OU 'N'"
              GO TO 0000-PROCESSAR
           ELSE  
              IF WRK-FLAG-CLASS           EQUAL 'S'
                 PERFORM 0001-CARREGAR-TAB-INT 
                 PERFORM 0002-CLASSIFICAR-REG
                 PERFORM 0003-DESCARREGAR-TAB-INT 
                 DISPLAY "DADOS CLASSIFICADOS COM SUCESSO!"
              END-IF 
           END-IF
           PERFORM 9999-FINALIZAR
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CARREGAR TABELA INTERNA
      *----------------------------------------------------------------*
       0001-CARREGAR-TAB-INT           SECTION.
      *----------------------------------------------------------------*

           MOVE COPY002A-QUANT-REG     TO WRK-QTD-REG
           PERFORM VARYING WRK-IND1 FROM 1 BY 1 
                     UNTIL WRK-IND1 > WRK-QTD-REG 
                      MOVE COPY002A-COD       (WRK-IND1)
                        TO WRK-COD            (WRK-IND1)
                      MOVE COPY002A-NOME      (WRK-IND1)
                        TO WRK-NOME           (WRK-IND1)
                      MOVE COPY002A-IDADE     (WRK-IND1)
                        TO WRK-IDADE          (WRK-IND1)
                      MOVE COPY002A-DATA-NASC (WRK-IND1)
                        TO WRK-DATA-NASC      (WRK-IND1)
                      MOVE COPY002A-CARGO     (WRK-IND1)
                        TO WRK-CARGO          (WRK-IND1)
                      MOVE COPY002A-EMAIL     (WRK-IND1)
                        TO WRK-EMAIL          (WRK-IND1)
                      MOVE COPY002A-TELEFONE  (WRK-IND1)
                        TO WRK-TELEFONE       (WRK-IND1)
                      MOVE COPY002A-RUA       (WRK-IND1)
                        TO WRK-RUA            (WRK-IND1)
                      MOVE COPY002A-CIDADE    (WRK-IND1)
                        TO WRK-CIDADE         (WRK-IND1)
                      MOVE COPY002A-ESTADO    (WRK-IND1)
                        TO WRK-ESTADO         (WRK-IND1)
                      MOVE COPY002A-CEP       (WRK-IND1)
                        TO WRK-CEP            (WRK-IND1)
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CLASSIFICAR REGISTROS POR NOME
      *----------------------------------------------------------------*
       0002-CLASSIFICAR-REG            SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 
                     UNTIL WRK-IND1 >= WRK-QTD-REG
                   PERFORM VARYING WRK-IND2 FROM 1 BY 1 
                     UNTIL WRK-IND2 > WRK-QTD-REG - WRK-IND1
                        IF WRK-NOME(WRK-IND2) > WRK-NOME(WRK-IND2 + 1)
                           MOVE WRK-CADASTRO(WRK-IND2) TO WRK-TEMP
                           MOVE WRK-CADASTRO(WRK-IND2 + 1) 
                                         TO WRK-CADASTRO(WRK-IND2)
                           MOVE WRK-TEMP TO WRK-CADASTRO(WRK-IND2 + 1)
                        END-IF
                   END-PERFORM
           END-PERFORM 
            .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    DESCARREGAR DADOS TABELA INTERNA NO BOOK
      *----------------------------------------------------------------*
       0003-DESCARREGAR-TAB-INT        SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 
                     UNTIL WRK-IND1 > WRK-QTD-REG
                      MOVE WRK-COD            (WRK-IND1)
                        TO COPY002A-COD       (WRK-IND1)
                      MOVE WRK-NOME           (WRK-IND1)
                        TO COPY002A-NOME      (WRK-IND1)
                      MOVE WRK-IDADE          (WRK-IND1)
                        TO COPY002A-IDADE     (WRK-IND1)
                      MOVE WRK-DATA-NASC      (WRK-IND1)
                        TO COPY002A-DATA-NASC (WRK-IND1)
                      MOVE WRK-CARGO          (WRK-IND1)
                        TO COPY002A-CARGO     (WRK-IND1)
                      MOVE WRK-EMAIL          (WRK-IND1)
                        TO COPY002A-EMAIL     (WRK-IND1)
                      MOVE WRK-TELEFONE       (WRK-IND1)
                        TO COPY002A-TELEFONE  (WRK-IND1)
                      MOVE WRK-RUA            (WRK-IND1)
                        TO COPY002A-RUA       (WRK-IND1)
                      MOVE WRK-CIDADE         (WRK-IND1)
                        TO COPY002A-CIDADE    (WRK-IND1)
                      MOVE WRK-ESTADO         (WRK-IND1)
                        TO COPY002A-ESTADO    (WRK-IND1)
                      MOVE WRK-CEP            (WRK-IND1)
                        TO COPY002A-CEP       (WRK-IND1)
            END-PERFORM
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
           GOBACK
            .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*