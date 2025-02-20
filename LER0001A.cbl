      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOLDICAS
      * DATA: 13/02/2025
      * OBJETIVO: MODULO DE LEITURA DE ARQUIVO SEQUENCIAL
      * OBS.: 
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LER0001A.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENTRADA ASSIGN TO "SEQENT01.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENTRADA.
       01  WRK-COPY002A-REGISTRO.
        05  WRK-COPY002A-CADUSUAR.
         10 WRK-COPY002A-COD                PIC 9(02).
         10 WRK-COPY002A-NOME               PIC X(30).
         10 WRK-COPY002A-IDADE              PIC 9(02).
         10 WRK-COPY002A-DATA-NASC          PIC 9(08).
         10 WRK-COPY002A-CARGO              PIC X(20).    
         10 WRK-COPY002A-EMAIL              PIC X(50).
         10 WRK-COPY002A-TELEFONE           PIC 9(09).
         10 WRK-COPY002A-ENDERECO.
          15 WRK-COPY002A-RUA               PIC X(50).
          15 WRK-COPY002A-CIDADE            PIC X(30).
          15 WRK-COPY002A-ESTADO            PIC X(02).
          15 WRK-COPY002A-CEP               PIC 9(08).


       WORKING-STORAGE SECTION.
       01 FIM-ARQUIVO    PIC X VALUE "N".
       01  WRK-IND1                PIC 9(02) VALUE ZEROS.

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

            OPEN INPUT ARQUIVO-ENTRADA.
            PERFORM 0001-LER-ARQSEQ
            MOVE COPY002A-QUANT-REG TO WRK-IND1
            PERFORM 0002-LER-REGISTRO  UNTIL FIM-ARQUIVO = "S"
            PERFORM 9999-FINALIZAR
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FAZER CADASTRO USUARIO
      *----------------------------------------------------------------*
       0001-LER-ARQSEQ                 SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENTRADA INTO WRK-COPY002A-REGISTRO
               AT END MOVE "S" TO FIM-ARQUIVO
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA E MOVIMENTACAO DE REGISTROS
      *----------------------------------------------------------------*
       0002-LER-REGISTRO               SECTION.
      *----------------------------------------------------------------*

            ADD 1                      TO WRK-IND1
            MOVE WRK-COPY002A-REGISTRO TO COPY002A-CADUSUAR(WRK-IND1)

            IF FIM-ARQUIVO = "N"
               DISPLAY "ID: " COPY002A-COD(WRK-IND1)  ",
      -                "Nome: " COPY002A-NOME(WRK-IND1)
            END-IF

            PERFORM 0001-LER-ARQSEQ            
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

            MOVE WRK-IND1              TO COPY002A-QUANT-REG 
            CLOSE ARQUIVO-ENTRADA
            GOBACK
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
