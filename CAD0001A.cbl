      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOLDICAS
      * DATA: 06/02/2025
      * OBJETIVO: PROGRAMA DE CADASTRO DE USUARIO
      * OBS.: 
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAD0001A.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Definição do tamanho máximo de cadastros
       01  WRK-MAX-TAB-INT         PIC 9(02) VALUE 10.
       01  WRK-IND1                PIC 9(02) VALUE ZEROS.

      *  MASCARA FORMATO DA DATA - DD/MM/AAAA
       01  WRK-MASC-DATA.
         05 WRK-MASC-DATA-DIA          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-MES          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-ANO          PIC 9(004) VALUE ZEROS.

      *     DEFINICAO DE DATA E HORA DO SISTEMA. 
            COPY COD001A.

      *     Definição da estrutura do cadastro
            COPY COPY002A.
 
       PROCEDURE DIVISION.

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*
            PERFORM 0001-OBTER-DATA 
            PERFORM 0002-CAD-USUAR
            PERFORM 1002-LER-ARQSEQ
            PERFORM 0003-GRAVA-ARQSEQ
            PERFORM 0004-REL-USUAR
            PERFORM 9999-FINALIZAR
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DATA SISTEMA
      *----------------------------------------------------------------*
       0001-OBTER-DATA                  SECTION.
      *----------------------------------------------------------------*

            CALL 'PROGDATA' USING COD001A-REGISTRO. 

            MOVE COD001A-DATA-ANO      TO WRK-MASC-DATA-ANO
            MOVE COD001A-DATA-MES      TO WRK-MASC-DATA-MES
            MOVE COD001A-DATA-DIA      TO WRK-MASC-DATA-DIA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FAZER CADASTRO USUARIO
      *----------------------------------------------------------------*
       0002-CAD-USUAR                  SECTION.
      *----------------------------------------------------------------*
 
            CALL 'CAD0002A' USING COPY002A-REGISTRO
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FAZER CADASTRO USUARIO
      *----------------------------------------------------------------*
       1002-LER-ARQSEQ                 SECTION.
      *----------------------------------------------------------------*
 
            CALL 'LER0001A' USING COPY002A-REGISTRO
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 1002-end
       1002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FAZER CADASTRO USUARIO
      *----------------------------------------------------------------*
       0003-GRAVA-ARQSEQ               SECTION.
      *----------------------------------------------------------------*

            CALL 'GRAV001A' USING COPY002A-REGISTRO
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
       0004-REL-USUAR                  SECTION.
      *----------------------------------------------------------------*

            IF COPY002A-QUANT-REG NOT EQUAL ZEROS
               CALL 'REL0001A' USING COPY002A-REGISTRO
            ELSE 
               DISPLAY 'NAO HÁ DADOS INFORMADOS NA TELA'
            END-IF 
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

            DISPLAY "DATA.........: " WRK-MASC-DATA
            STOP RUN
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
