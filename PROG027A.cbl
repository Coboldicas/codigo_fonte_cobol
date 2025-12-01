      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 24/11/2025
      * DESCRICAO..: SIMULADOR DE NOTAS FISCAIS - SIMPLIFICADA
      * NOME.......: PROG027A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG027A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT ASSIGN TO 'PRODUTOS.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
          
           SELECT ARQUIVO-SAI ASSIGN TO 'NOTAFISCAL.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT.
       01  FD-LIN-PRODUTOS.
         05 FD-COD-PRODUTO                PIC 9(05).
         05 FD-NOME-PRODUTO               PIC X(30).
         05 FD-VALOR-PRODUTO              PIC 9(06)V99.

       FD  ARQUIVO-SAI.
       01  FD-LIN-NOTA-FISCAL             PIC X(80).

       WORKING-STORAGE SECTION.
       01  WRK-FIM-ARQ                    PIC X(01) VALUE 'N'.      
       01  WRK-LINHA                      PIC 9(02) VALUE ZEROS.
       01  WRK-IND1                       PIC 9(02) VALUE ZEROS.
       01  WRK-RETURN-CODE                PIC S9(4) COMP VALUE ZERO.

      *-- Valores calculados por item
       01  VALORES-ITEM.
         05 WRK-VLR-ICMS                  PIC 9(6)V99.
         05 WRK-VLR-ISS                   PIC 9(6)V99.
         05 WRK-VLR-IMPOSTO               PIC 9(7)V99.
         05 WRK-VLR-TOTAL-ITEM            PIC 9(7)V99.

      *-- Totais da Nota
       01  VALORES-TOTAIS.
         05 WRK-TOT-BRUTO                 PIC 9(9)V99 VALUE 0.
         05 WRK-TOT-ICMS                  PIC 9(9)V99 VALUE 0.
         05 WRK-TOT-ISS                   PIC 9(9)V99 VALUE 0.
         05 WRK-TOT-NOTA                  PIC 9(9)V99 VALUE 0.

      *-- Constantes de imposto (18% e 5%)
       01  CONSTANTES.
         05 WRK-TX-ICMS                   PIC V999 VALUE ZEROS.
         05 WRK-TX-ISS                    PIC V999 VALUE ZEROS.

      *    LAYOUT NOTA FISCAL 
       01  WRK-BOLETO-LINHA.
        03 FILLER                      PIC X(80) VALUE 
            '*----------------------------------------------------------
      -     '--------------------*'.
       01  WRK-BOLETO-LINHA-BRANCO     PIC X(80) VALUE SPACES.

       01  WRK-BOLETO-CAB1.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(04) VALUE SPACES. 
        03 FILLER                      PIC X(42) VALUE
            'SIMULADOR DE NOTAS FISCAIS - SIMPLIFICADAS'.
        03 FILLER                      PIC X(06) VALUE SPACES. 
        03 FILLER                      PIC X(14) VALUE
            'DATA EMISSAO: '.
      *    MASCARA FORMATO DA DATA - DD/MM/AAAA
        03 WRK-MASC-DATA.
         05 WRK-MASC-DATA-DIA          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-MES          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-ANO          PIC 9(004) VALUE ZEROS.
        03 FILLER                      PIC X(002) VALUE ' *'.

       01  WRK-BOLETO-CAB2.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(04) VALUE
            'COD.'.
        03 FILLER                      PIC X(02) VALUE SPACES. 
        03 FILLER                      PIC X(09) VALUE
            'DESCRICAO'.
        03 FILLER                      PIC X(28) VALUE SPACES. 
        03 FILLER                      PIC X(05) VALUE
            'VALOR'.
        03 FILLER                      PIC X(05) VALUE SPACES.         
        03 FILLER                      PIC X(08) VALUE
            'IMPOSTOS'.
        03 FILLER                      PIC X(08) VALUE SPACES.         
        03 FILLER                      PIC X(05) VALUE
            'TOTAL'.
        03 FILLER                      PIC X(02) VALUE SPACES. 
        03 FILLER                      PIC X(02) VALUE ' *'.

       01  WRK-BOLETO-DET1.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 WRK-DET1-COD-PRODUTO        PIC 9(05) VALUE ZEROS.
        03 FILLER                      PIC X(01) VALUE SPACES.
        03 WRK-DET1-NOME-PRODUTO       PIC X(30) VALUE SPACES.
        03 FILLER                      PIC X(02) VALUE SPACES.
        03 WRK-DET1-VALOR-PRODUTO      PIC ZZZ.ZZZ,ZZ.
        03 FILLER                      PIC X(03) VALUE SPACES.
        03 WRK-DET1-VALOR-IMPOSTOS     PIC ZZZ.ZZZ,ZZ.
        03 FILLER                      PIC X(03) VALUE SPACES.
        03 WRK-DET1-VALOR-TOTAL        PIC ZZZ.ZZZ,ZZ.
        03 FILLER                      PIC X(02) VALUE SPACES. 
        03 FILLER                      PIC X(02) VALUE ' *'.

       01  WRK-BOLETO-TOT1.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(20) VALUE
            'VALOR TOTAL DA NOTA:'.
        03 FILLER                      PIC X(44) VALUE SPACES.  
        03 WRK-TOT1-VALOR-TOTAL        PIC ZZZ.ZZZ,ZZ.
        03 FILLER                      PIC X(02) VALUE SPACES. 
        03 FILLER                      PIC X(02) VALUE ' *'.

      *    DEFINICAO DE DATA E HORA DO SISTEMA. 
       COPY COD001A.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQUIVO-ENT
               OUTPUT ARQUIVO-SAI

           PERFORM 0001-OBTER-DATA 
           PERFORM 0004-GRAVAR-CABECALHO
           PERFORM 0002-LER-ARQUIVO-PROD
           PERFORM 0003-PROCESSAR-NOTA UNTIL WRK-FIM-ARQ EQUAL 'S'
           PERFORM 0006-GRAVAR-TOTAL
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

            CALL 'PROGDATA' USING COD001A-REGISTRO

            MOVE RETURN-CODE TO WRK-RETURN-CODE

            IF WRK-RETURN-CODE NOT = 0
               DISPLAY 'ERRO NA CHAMADA PROGDATA. RETURN-CODE: '
                WRK-RETURN-CODE
               STOP RUN
            END-IF

            MOVE COD001A-DATA-ANO      TO WRK-MASC-DATA-ANO
            MOVE COD001A-DATA-MES      TO WRK-MASC-DATA-MES
            MOVE COD001A-DATA-DIA      TO WRK-MASC-DATA-DIA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DE ARQUIVO DE ENTRADA 
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-ler-arquivo-ent
       0002-LER-ARQUIVO-PROD           SECTION.
      *----------------------------------------------------------------*

           READ ARQUIVO-ENT        INTO FD-LIN-PRODUTOS
             AT END MOVE "S"         TO WRK-FIM-ARQ 
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------* 
      *    PROCESSAR TITULOS
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-processar-nota
       0003-PROCESSAR-NOTA             SECTION.
      *----------------------------------------------------------------*

      *    Calcula impostos

           MOVE 0,180 TO WRK-TX-ICMS 
           MOVE 0,050 TO WRK-TX-ISS

           COMPUTE WRK-VLR-ICMS = FD-VALOR-PRODUTO * WRK-TX-ICMS
           COMPUTE WRK-VLR-ISS  = FD-VALOR-PRODUTO * WRK-TX-ISS 
           COMPUTE WRK-VLR-IMPOSTO = WRK-VLR-ICMS + WRK-VLR-ISS 
           COMPUTE WRK-VLR-TOTAL-ITEM =
                   FD-VALOR-PRODUTO + WRK-VLR-ICMS + WRK-VLR-ISS

      *    Acumula totais
           ADD FD-VALOR-PRODUTO        TO WRK-TOT-BRUTO
           ADD WRK-VLR-ICMS            TO WRK-TOT-ICMS
           ADD WRK-VLR-ISS             TO WRK-TOT-ISS
           ADD WRK-VLR-TOTAL-ITEM      TO WRK-TOT-NOTA
           
           PERFORM 0005-GRAVAR-DETALHE 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR CABECALHO NO ARQUIVO SEQUENCIAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-gravar-cabecalho
       0004-GRAVAR-CABECALHO           SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-BOLETO-LINHA       TO FD-LIN-NOTA-FISCAL
           WRITE FD-LIN-NOTA-FISCAL

           MOVE WRK-BOLETO-CAB1        TO FD-LIN-NOTA-FISCAL
           WRITE FD-LIN-NOTA-FISCAL

           MOVE WRK-BOLETO-LINHA       TO FD-LIN-NOTA-FISCAL
           WRITE FD-LIN-NOTA-FISCAL

           MOVE WRK-BOLETO-CAB2        TO FD-LIN-NOTA-FISCAL
           WRITE FD-LIN-NOTA-FISCAL

           ADD 4                       TO WRK-LINHA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR DETALHES DO PRODUTO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-gravar-detalhe
       0005-GRAVAR-DETALHE             SECTION.
      *----------------------------------------------------------------*

           MOVE FD-COD-PRODUTO         TO WRK-DET1-COD-PRODUTO
           MOVE FD-NOME-PRODUTO        TO WRK-DET1-NOME-PRODUTO
           MOVE FD-VALOR-PRODUTO       TO WRK-DET1-VALOR-PRODUTO
           MOVE WRK-VLR-IMPOSTO        TO WRK-DET1-VALOR-IMPOSTOS
           MOVE WRK-VLR-TOTAL-ITEM     TO WRK-DET1-VALOR-TOTAL

           MOVE WRK-BOLETO-DET1        TO FD-LIN-NOTA-FISCAL
           WRITE FD-LIN-NOTA-FISCAL

           ADD 1                       TO WRK-LINHA
           PERFORM 0002-LER-ARQUIVO-PROD
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR DETALHES DO PRODUTO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-gravar-total
       0006-GRAVAR-TOTAL               SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-BOLETO-LINHA       TO FD-LIN-NOTA-FISCAL
           WRITE FD-LIN-NOTA-FISCAL

           MOVE WRK-TOT-NOTA           TO WRK-TOT1-VALOR-TOTAL
           MOVE WRK-BOLETO-TOT1        TO FD-LIN-NOTA-FISCAL
           WRITE FD-LIN-NOTA-FISCAL

           MOVE WRK-BOLETO-LINHA       TO FD-LIN-NOTA-FISCAL
           WRITE FD-LIN-NOTA-FISCAL

           ADD 3                       TO WRK-LINHA
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-end
       0006-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-finalizar
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQUIVO-ENT
                 ARQUIVO-SAI
           
           DISPLAY '********************************'
           DISPLAY '*        FIM DE PROGRAMA       *'
           DISPLAY '********************************'
           STOP RUN 
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
