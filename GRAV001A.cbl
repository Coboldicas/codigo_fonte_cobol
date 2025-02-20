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

       LINKAGE SECTION.
      * Definição da estrutura do cadastro
       COPY COPY002A.

      *================================================================*
       PROCEDURE DIVISION USING COPY002A-REGISTRO.
      *================================================================*


      *> cobol-lint CL002 0000-principal
       0000-PRINCIPAL SECTION.
            PERFORM 0001-ABRE-ARQUIVO 
            PERFORM 0002-ESCREVE-REGISTROS
            PERFORM 0003-FECHA-ARQUIVO
            . 
      *> cobol-lint CL002 0000-fim
       0000-FIM. EXIT.

       0001-ABRE-ARQUIVO SECTION.
           OPEN OUTPUT ARQUIVO-OUT.
      *> cobol-lint CL002 0001-fim
       0001-FIM. EXIT.

       0002-ESCREVE-REGISTROS SECTION.

           DISPLAY "GRAVAR SEQUENCIAL:"
           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL
                                 WRK-IND1 GREATER COPY002A-QUANT-REG 
                   MOVE COPY002A-CADUSUAR(WRK-IND1) TO 
                                            WRK-COPY002A-REGISTRO
                   WRITE WRK-COPY002A-REGISTRO

           END-PERFORM
            .
      *> cobol-lint CL002 0002-fim
       0002-FIM. EXIT.

       0003-FECHA-ARQUIVO SECTION.
           CLOSE ARQUIVO-OUT.
           DISPLAY "Arquivo Gravado com Sucesso!"
           GOBACK.
      *> cobol-lint CL002 0003-fim
       0003-FIM. EXIT.