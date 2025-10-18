      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOLDICAS
      * DATA: 13/02/2025
      * OBJETIVO: MODULO DE GRAVACAO EM ARQUIVO SEQUENCIAL
      * OBS.: 
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRAV001A.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-OUT ASSIGN TO "SEQSAI02.dat"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-OUT.
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
       01  WRK-IND1                PIC 9(02) VALUE ZEROS.
       01  WRK-IND2                PIC 9(02) VALUE ZEROS.

       LINKAGE SECTION.
      * DEFINICAO DA ESTRUTURA DO ARQUIVO
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
            PERFORM 0001-ABRE-ARQUIVO 
            PERFORM 0002-ESCREVE-REGISTROS
            PERFORM 0003-FECHA-ARQUIVO
            PERFORM 9999-FINALIZAR 
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ABRE ARQUIVO DE SAIDA
      *----------------------------------------------------------------*
       0001-ABRE-ARQUIVO               SECTION.
      *----------------------------------------------------------------*

           OPEN OUTPUT ARQUIVO-OUT
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVA REGISTROS NO ARQUIVO SEQUENCIAL 
      *----------------------------------------------------------------*
       0002-ESCREVE-REGISTROS          SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL
                           WRK-IND1 GREATER COPY002A-QUANT-REG 
                      MOVE COPY002A-CADUSUAR(WRK-IND1)
                                       TO WRK-COPY002A-REGISTRO
                     WRITE WRK-COPY002A-REGISTRO
           END-PERFORM
            .
      *----------------------------------------------------------------*      
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ABRE ARQUIVO DE SAIDA
      *----------------------------------------------------------------*
       0003-FECHA-ARQUIVO              SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQUIVO-OUT
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

            DISPLAY "ARQUIVO GRAVADO COM SUCESSO!"

            GOBACK.
            
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*