       IDENTIFICATION DIVISION.
       PROGRAM-ID. REL0001A.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REL0001A-OUT ASSIGN TO "REL0001A.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  REL0001A-OUT.
       01  WRK-REL0001A-REGISTRO.
        05 WRK-REL0001A-STRING         PIC X(100).


       WORKING-STORAGE SECTION.
      * Definição do tamanho máximo de cadastros
       01  WRK-IND-LINHA           PIC 9(02) VALUE ZEROS.
       01  WRK-IND-PAGINACAO       PIC 9(02) VALUE 1.
       01  WRK-IND1                PIC 9(02) VALUE 1.

       01  WRK-DATA-COMUM          PIC 9(008) VALUE ZEROS.
      *  MASCARA FORMATO DA DATA - DD/MM/AAAA
       01  WRK-MASC-DATA.
         05 WRK-MASC-DATA-DIA          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-MES          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-ANO          PIC 9(004) VALUE ZEROS.

      *  MASCARA FORMATO DA HORA - HH:MM
       01  WRK-MASC-TIME.
         05 WRK-MASC-HORA              PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE ':'.
         05 WRK-MASC-MIN               PIC 9(002) VALUE ZEROS.

       01  WRK-REL0001A-LINHA.
        03 FILLER                      PIC X(80) VALUE 
            "**---------------------------------------------------------
      -     "-------------------**".
       01  WRK-REL0001A-LINHA-BRANCO   PIC X(80) VALUE SPACES.

       01  WRK-REL0001A-CAB1.
        03 FILLER                      PIC X(67) VALUE
            "** REL0001A                    COBOL DICAS                 
      -     "        ".
        03 WRK-REL0001A-CAB1-DATA      PIC X(10) VALUE SPACES.
        03 FILLER                      PIC X(03) VALUE " **". 

       01  WRK-REL0001A-CAB2.
        03 FILLER                      PIC X(08) VALUE
            "** PAG: ".
        03 WRK-REL0001A-PAG            PIC 9(02) VALUE ZEROS.
        03 FILLER                      PIC X(62) VALUE
            "                RELATORIO CADASTRO USUARIO                 
      -     "   ".
        03 WRK-REL0001A-HORA           PIC X(05) VALUE SPACES.
        03 FILLER                      PIC X(03) VALUE " **".

       01  WRK-REL0001A-DET1.
        03 FILLER                      PIC X(06) VALUE "NOME: ".
        03 WRK-REL0001A-NOME           PIC X(30) VALUE SPACES.
        03 FILLER                      PIC X(11) VALUE " - CODIGO: ".
        03 WRK-REL0001A-COD            PIC 9(02) VALUE ZEROS.

       01  WRK-REL0001A-DET2.
        03 FILLER                      PIC X(08) VALUE "IDADE: ".
        03 WRK-REL0001A-IDADE          PIC 9(02) VALUE ZEROS.
        03 FILLER                      PIC X(15) VALUE 
            " - DATA NASC.: ".
        03 WRK-REL0001A-DATA-NASC      PIC X(10) VALUE SPACES.
        03 FILLER                      PIC X(13) VALUE " - TELEFONE: ".
        03 WRK-REL0001A-TELEFONE       PIC 9(09) VALUE ZEROS.

       01  WRK-REL0001A-DET3.
        03 FILLER                      PIC X(07) VALUE "CARGO: ".
        03 WRK-REL0001A-CARGO          PIC X(20) VALUE SPACES.
        03 FILLER                      PIC X(11) VALUE " - E-MAIL: ".
        03 WRK-REL0001A-EMAIL          PIC X(30) VALUE SPACES.


       01  WRK-REL0001A-DET4.
        03 FILLER                      PIC X(10) VALUE "ENDERECO: ".
        03 WRK-REL0001A-RUA            PIC X(50) VALUE SPACES.

       01  WRK-REL0001A-DET5.
        03 FILLER                      PIC X(14) VALUE "CIDADE: ".
        03 WRK-REL0001A-CIDADE         PIC X(30) VALUE SPACES.
        03 FILLER                      PIC X(11) VALUE " - ESTADO: ".
        03 WRK-REL0001A-ESTADO         PIC X(02) VALUE SPACES.
        03 FILLER                      PIC X(08) VALUE " - CEP: ".
        03 WRK-REL0001A-CEP            PIC 9(08) VALUE ZEROS.

      *    DEFINICAO DE DATA E HORA DO SISTEMA. 
       COPY COD001A.

       LINKAGE SECTION.
      *    Definição da estrutura do cadastro
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
            PERFORM 0002-OBTER-DATA 
            PERFORM 0003-OBTER-TIME   
            PERFORM 0004-GERAR-CABECALHO
            PERFORM 0005-GERAR-DETALHE UNTIL WRK-IND1 GREATER 
                                             COPY002A-QUANT-REG 
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

           OPEN OUTPUT REL0001A-OUT
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DATA SISTEMA
      *----------------------------------------------------------------*
       0002-OBTER-DATA                 SECTION.
      *----------------------------------------------------------------*

            CALL 'PROGDATA' USING COD001A-REGISTRO 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DATA SISTEMA
      *----------------------------------------------------------------*
       0003-OBTER-TIME                 SECTION.
      *----------------------------------------------------------------*

            CALL 'PROGTIME' USING COD001A-REGISTRO 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GERAR CABECALHO RELATORIO
      *----------------------------------------------------------------*
       0004-GERAR-CABECALHO            SECTION.
      *----------------------------------------------------------------*

            MOVE COD001A-DATA-ANO      TO WRK-MASC-DATA-ANO
            MOVE COD001A-DATA-MES      TO WRK-MASC-DATA-MES
            MOVE COD001A-DATA-DIA      TO WRK-MASC-DATA-DIA

            MOVE COD001A-HORA          TO WRK-MASC-HORA
            MOVE COD001A-MINUTO        TO WRK-MASC-MIN

      *     DISPLAY WRK-REL0001A-LINHA
            MOVE WRK-REL0001A-LINHA    TO WRK-REL0001A-STRING
            WRITE WRK-REL0001A-REGISTRO
            MOVE WRK-IND-PAGINACAO     TO WRK-REL0001A-PAG
            MOVE WRK-MASC-DATA         TO WRK-REL0001A-CAB1-DATA
      *     DISPLAY WRK-REL0001A-CAB1
            MOVE WRK-REL0001A-CAB1     TO WRK-REL0001A-STRING
            WRITE WRK-REL0001A-REGISTRO

            MOVE WRK-MASC-TIME         TO WRK-REL0001A-HORA
      *     DISPLAY WRK-REL0001A-CAB2
            MOVE WRK-REL0001A-CAB2     TO WRK-REL0001A-STRING
            WRITE WRK-REL0001A-REGISTRO
      *     DISPLAY WRK-REL0001A-LINHA
            MOVE WRK-REL0001A-LINHA    TO WRK-REL0001A-STRING
            WRITE WRK-REL0001A-REGISTRO

            ADD 04                     TO WRK-IND-LINHA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GERAR DETALHE DO RELATORIO
      *----------------------------------------------------------------*
       0005-GERAR-DETALHE              SECTION.
      *----------------------------------------------------------------*

            IF WRK-IND-LINHA           GREATER  10
               MOVE ZEROS              TO WRK-IND-LINHA
               ADD 1                   TO WRK-IND-PAGINACAO
      *        DISPLAY WRK-REL0001A-LINHA-BRANCO
               MOVE WRK-REL0001A-LINHA-BRANCO
                                       TO WRK-REL0001A-STRING
               WRITE WRK-REL0001A-REGISTRO
               ADD 1                   TO WRK-IND-LINHA
               PERFORM 0004-GERAR-CABECALHO
            END-IF

            MOVE COPY002A-NOME(WRK-IND1)
                                       TO WRK-REL0001A-NOME
            MOVE COPY002A-COD(WRK-IND1)
                                       TO WRK-REL0001A-COD
      *     DISPLAY WRK-REL0001A-DET1
            MOVE WRK-REL0001A-DET1     TO WRK-REL0001A-STRING
            WRITE WRK-REL0001A-REGISTRO

            MOVE COPY002A-DATA-NASC(WRK-IND1)
                                       TO WRK-DATA-COMUM
            MOVE WRK-DATA-COMUM(1:2)   TO WRK-MASC-DATA-DIA
            MOVE WRK-DATA-COMUM(3:2)   TO WRK-MASC-DATA-MES
            MOVE WRK-DATA-COMUM(5:4)   TO WRK-MASC-DATA-ANO
            MOVE WRK-MASC-DATA         TO WRK-REL0001A-DATA-NASC
            MOVE COPY002A-IDADE(WRK-IND1)
                                       TO WRK-REL0001A-IDADE
      *     DISPLAY WRK-REL0001A-DET2
            MOVE WRK-REL0001A-DET2     TO WRK-REL0001A-STRING
            WRITE WRK-REL0001A-REGISTRO

            MOVE COPY002A-CARGO(WRK-IND1)
                                       TO WRK-REL0001A-CARGO
            MOVE COPY002A-TELEFONE(WRK-IND1)
                                       TO WRK-REL0001A-TELEFONE
            MOVE COPY002A-EMAIL(WRK-IND1)
                                       TO WRK-REL0001A-EMAIL
      *     DISPLAY WRK-REL0001A-DET3
            MOVE WRK-REL0001A-DET3     TO WRK-REL0001A-STRING
            WRITE WRK-REL0001A-REGISTRO


            MOVE COPY002A-RUA(WRK-IND1)
                                       TO WRK-REL0001A-RUA
      *     DISPLAY WRK-REL0001A-DET4
            MOVE WRK-REL0001A-DET4     TO WRK-REL0001A-STRING
            WRITE WRK-REL0001A-REGISTRO

            MOVE COPY002A-CIDADE(WRK-IND1)
                                       TO WRK-REL0001A-CIDADE
            MOVE COPY002A-ESTADO(WRK-IND1)
                                       TO WRK-REL0001A-ESTADO
            MOVE COPY002A-CEP(WRK-IND1)
                                       TO WRK-REL0001A-CEP
      *     DISPLAY WRK-REL0001A-DET5
            MOVE WRK-REL0001A-DET5     TO WRK-REL0001A-STRING
            WRITE WRK-REL0001A-REGISTRO

      *     DISPLAY WRK-REL0001A-LINHA
            MOVE WRK-REL0001A-LINHA    TO WRK-REL0001A-STRING
            WRITE WRK-REL0001A-REGISTRO

            ADD 1                      TO WRK-IND1
            ADD 05                     TO WRK-IND-LINHA
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

           CLOSE REL0001A-OUT
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-end
       0006-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

            DISPLAY "RELATORIO GERADO COM SUCESSO!"
            GOBACK 
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*