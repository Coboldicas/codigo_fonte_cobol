      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 10/11/2025
      * DESCRICAO..: GERADOR DE BOLETOS FICTICIOS
      * NOME.......: PROG025A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG025A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT ASSIGN TO 'TITULOS.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQUIVO-PARM ASSIGN TO 'PARAMETROS.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQUIVO-SAI ASSIGN TO 'BOLETOS.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT.
       01  FD-LIN-TITULOS                 PIC X(256).

       FD  ARQUIVO-PARM.
       01  FD-LIN-PARAM                   PIC X(064).

       FD  ARQUIVO-SAI.
       01  FD-LIN-BOLETOS                 PIC X(80).

       WORKING-STORAGE SECTION.
       01  SEP                            PIC X VALUE '|'.
       01  WRK-HOJE-AAAAMMDD              PIC 9(8).
       01  WRK-HOJE-INT                   PIC 9(9).
       01  WRK-FIM-ARQ                    PIC X(01) VALUE 'N'.
       01  WRK-RETURN-CODE                PIC S9(4) COMP VALUE ZERO.
       01  WRK-IND1                       PIC 9(02) VALUE ZEROS.
       01  WRK-IND2                       PIC 9(02) VALUE ZEROS.
       01  WRK-IND3                       PIC 9(02) VALUE ZEROS.

      *--- Parametrização
       01  WRK-MULTA-PCT                  PIC 9(3)V99 VALUE ZEROS.
       01  WRK-JUROS-DIA-PCT              PIC 9(3)V999 VALUE ZEROS.

      *--- Campos do título
       01  TITULOS.
         05  TITULOS-NOSSONUM             PIC X(20).
         05  TITULOS-SACADO               PIC X(60).
         05  TITULOS-CEDENTE              PIC X(60).
         05  TITULOS-VALOR-CENTS          PIC 9(10).
         05  TITULOS-VENC-AAAAMMDD        PIC 9(8).
         05  TITULOS-PAGTO-AAAAMMDD       PIC 9(8).

      *--- Cálculo
       01  VALOR-VALOR                    PIC 9(7)V99 VALUE 0.
       01  VALOR-MULTA                    PIC 9(7)V99 VALUE 0.
       01  VALOR-JUROS                    PIC 9(7)V99 VALUE 0.
       01  VALOR-TOTAL                    PIC 9(7)V99 VALUE 0.
       01  VALOR-DIAS-ATRASO              PIC 9(5)    VALUE 0.

      *--- Datas inteiras para diferença
       01  VENC-INT                       PIC 9(9).
       01  BASE-INT                       PIC 9(9).

      *    MASCARA FORMATO DA DATA - DD/MM/AAAA
       01  WRK-MASC-DATA.
         05 WRK-MASC-DATA-DIA          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-MES          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-ANO          PIC 9(004) VALUE ZEROS.


      *    LAYOUT BOLETO 
       01  WRK-BOLETO-LINHA.
        03 FILLER                      PIC X(80) VALUE 
            '*----------------------------------------------------------
      -     '--------------------*'.
       01  WRK-BOLETO-LINHA-BRANCO     PIC X(80) VALUE SPACES.

       01  WRK-BOLETO-CAB1.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(16) VALUE SPACES. 
        03 FILLER                      PIC X(45) VALUE
            'BOLETO FICTICIO - NAO UTILIZAR PARA PAGAMENTO'.
        03 FILLER                      PIC X(15) VALUE SPACES. 
        03 FILLER                      PIC X(02) VALUE ' *'.

       01  WRK-BOLETO-DET1.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(09) VALUE
            'Cedente: '.
        03 WRK-BOLETO-CEDENTE          PIC X(43) VALUE SPACES. 
        03 FILLER                      PIC X(14) VALUE
            '| Vencimento: '.
        03 WRK-BOLETO-VENC.
         05 WRK-VENC-DATA-DIA          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-VENC-DATA-MES          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-VENC-DATA-ANO          PIC 9(004) VALUE ZEROS.
        03 FILLER                      PIC X(02) VALUE ' *'.

       01  WRK-BOLETO-DET2.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(14) VALUE
            'Nosso Numero: '.
        03 WRK-BOLETO-NOSSONUM         PIC X(20) VALUE SPACES.
        03 FILLER                      PIC X(18) VALUE SPACES.
        03 FILLER                      PIC X(25) VALUE 
            '|------------------------'.
        03 FILLER                      PIC X(01) VALUE '*'.

       01  WRK-BOLETO-DET3.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(24) VALUE
            'Base de calculo (hoje): '.
        03 WRK-BOLETO-HOJE             PIC X(10) VALUE SPACES. 
        03 FILLER                      PIC X(18) VALUE SPACES.
        03 FILLER                      PIC X(12) VALUE
            '| Valor:    '.
        03 WRK-BOLETO-VALOR            PIC Z.ZZZ.ZZZ,ZZ.
        03 FILLER                      PIC X(02) VALUE ' *'.

       01  WRK-BOLETO-DET4.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(41) VALUE SPACES.
        03 FILLER                      PIC X(09) VALUE
            '| Multa ('.
        03 WRK-BOLETO-MULTA            PIC ZZZ,ZZ.
        03 FILLER                      PIC X(08) VALUE 
            '%):     '.
        03 WRK-BOLETO-VALOR-MULTA      PIC Z.ZZZ.ZZZ,ZZ.
        03 FILLER                      PIC X(02) VALUE ' *'.

       01  WRK-BOLETO-DET5.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(16) VALUE
            'Dias de atraso: '.
        03 WRK-BOLETO-DIAS             PIC X(06) VALUE ZEROS. 
        03 FILLER                      PIC X(19) VALUE SPACES.
        03 FILLER                      PIC X(09) VALUE
            '| Juros ('.
        03 WRK-BOLETO-JUROS            PIC ZZZ,ZZZ.
        03 FILLER                      PIC X(07) VALUE
            '%/dia):'.
        03 WRK-BOLETO-VALOR-JUROS      PIC Z.ZZZ.ZZZ,ZZ.
        03 FILLER                      PIC X(02) VALUE ' *'.

       01  WRK-BOLETO-DET6.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(41) VALUE SPACES.
        03 FILLER                      PIC X(36) VALUE
            '|-----------------------------------'.
        03 FILLER                      PIC X(01) VALUE '*'.

       01  WRK-BOLETO-DET7.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(41) VALUE SPACES.
        03 FILLER                      PIC X(23) VALUE
            '| Total a pagar:       '.
        03 WRK-BOLETO-TOTAL            PIC Z.ZZZ.ZZZ,ZZ.
        03 FILLER                      PIC X(02) VALUE ' *'.

       01  WRK-BOLETO-DET8.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(09) VALUE
            'Sacado : '.
        03 WRK-BOLETO-SACADO           PIC X(60) VALUE SPACES.
        03 FILLER                      PIC X(07) VALUE SPACES.
        03 FILLER                      PIC X(02) VALUE ' *'.

       01  WRK-BOLETO-DET9.
        03 FILLER                      PIC X(02) VALUE '* '.
        03 FILLER                      PIC X(24) VALUE
            'Linha digitavel:       '.
        03 WRK-BOLETO-DIGITAVEL        PIC X(52) VALUE SPACES.
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
                INPUT ARQUIVO-PARM
               OUTPUT ARQUIVO-SAI

           PERFORM 0001-OBTER-DATA 
           PERFORM 0002-LER-ARQUIVO-PARM
           PERFORM 0003-PROCESSAR-TITULOS
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

            MOVE COD001A-DATA-ANO      TO WRK-HOJE-AAAAMMDD(1:4)
            MOVE COD001A-DATA-MES      TO WRK-HOJE-AAAAMMDD(5:2)
            MOVE COD001A-DATA-DIA      TO WRK-HOJE-AAAAMMDD(7:2)

            COMPUTE WRK-HOJE-INT = 
                            FUNCTION INTEGER-OF-DATE(WRK-HOJE-AAAAMMDD)
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DE ARQUIVO DE ENTRADA 
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-ler-arquivo-ent
       0002-LER-ARQUIVO-PARM            SECTION.
      *----------------------------------------------------------------*

           READ ARQUIVO-PARM INTO FD-LIN-PARAM
               AT END MOVE "S" TO WRK-FIM-ARQ 
           END-READ

           *> Pega MULTA e JUROS
           UNSTRING FD-LIN-PARAM DELIMITED BY ALL SEP
                    INTO WRK-MULTA-PCT, WRK-JUROS-DIA-PCT
           END-UNSTRING
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------* 
      *    PROCESSAR TITULOS
      *----------------------------------------------------------------*
       0003-PROCESSAR-TITULOS            SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                WRK-FIM-ARQ  EQUAL 'S' 
             READ ARQUIVO-ENT        INTO FD-LIN-TITULOS
               AT END MOVE "S"         TO WRK-FIM-ARQ 
               NOT AT END
                 IF FD-LIN-TITULOS(1:1) IS NUMERIC
                    ADD 01             TO WRK-IND2
                    PERFORM 0004-TRATAR-LINHA
                 END-IF
             END-READ
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*
     
      *----------------------------------------------------------------*
      *    TRATAR LINHA
      *----------------------------------------------------------------*
       0004-TRATAR-LINHA               SECTION.
      *----------------------------------------------------------------*

           MOVE SPACES                 TO TITULOS

           UNSTRING FD-LIN-TITULOS DELIMITED BY SEP
                    INTO TITULOS-NOSSONUM
                         TITULOS-SACADO
                         TITULOS-CEDENTE
                         TITULOS-VALOR-CENTS
                         TITULOS-VENC-AAAAMMDD
                         TITULOS-PAGTO-AAAAMMDD
           END-UNSTRING
           
           *> Converte valor em centavos p/ decimal 2 casas
           COMPUTE VALOR-VALOR = TITULOS-VALOR-CENTS / 100,00

           *> Converte datas para inteiro
           COMPUTE VENC-INT = 
                        FUNCTION INTEGER-OF-DATE(TITULOS-VENC-AAAAMMDD)

           IF TITULOS-PAGTO-AAAAMMDD   EQUAL ZEROS 
                                       OR TITULOS-PAGTO-AAAAMMDD = " "
              MOVE WRK-HOJE-INT        TO BASE-INT
           ELSE
              COMPUTE BASE-INT = 
                       FUNCTION INTEGER-OF-DATE(TITULOS-PAGTO-AAAAMMDD)
           END-IF

           COMPUTE VALOR-DIAS-ATRASO = BASE-INT - VENC-INT
           IF VALOR-DIAS-ATRASO        < 0 
              MOVE 0                   TO VALOR-DIAS-ATRASO
           ELSE 
              IF VALOR-DIAS-ATRASO        > 0
                 COMPUTE VALOR-MULTA = VALOR-VALOR
                                            * (WRK-MULTA-PCT / 100)
                 COMPUTE VALOR-JUROS = VALOR-VALOR 
                                            * (WRK-JUROS-DIA-PCT / 100) 
                                            * VALOR-DIAS-ATRASO
              ELSE
                 MOVE 0                TO VALOR-MULTA VALOR-JUROS
              END-IF
           END-IF

           COMPUTE VALOR-TOTAL = VALOR-VALOR + VALOR-MULTA + VALOR-JUROS

           PERFORM 0005-GRAVAR-ARQUIVO
            .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO SEQUENCIAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-gravar-arquivo
       0005-GRAVAR-ARQUIVO             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-BOLETO-LINHA       TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE WRK-BOLETO-CAB1        TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE WRK-BOLETO-LINHA       TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE TITULOS-CEDENTE        TO WRK-BOLETO-CEDENTE
           MOVE TITULOS-VENC-AAAAMMDD(7:2)
                                       TO WRK-VENC-DATA-DIA
           MOVE TITULOS-VENC-AAAAMMDD(5:2)
                                       TO WRK-VENC-DATA-MES
           MOVE TITULOS-VENC-AAAAMMDD(1:4)
                                       TO WRK-VENC-DATA-ANO
           MOVE WRK-BOLETO-DET1        TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE TITULOS-NOSSONUM       TO WRK-BOLETO-NOSSONUM
           MOVE WRK-BOLETO-DET2        TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE WRK-MASC-DATA          TO WRK-BOLETO-HOJE
           MOVE VALOR-VALOR            TO WRK-BOLETO-VALOR
           MOVE WRK-BOLETO-DET3        TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE WRK-BOLETO-LINHA       TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE WRK-MULTA-PCT          TO WRK-BOLETO-MULTA
           MOVE VALOR-MULTA            TO WRK-BOLETO-VALOR-MULTA
           MOVE WRK-BOLETO-DET4        TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE VALOR-DIAS-ATRASO      TO WRK-BOLETO-DIAS
           MOVE WRK-JUROS-DIA-PCT      TO WRK-BOLETO-JUROS
           MOVE VALOR-JUROS            TO WRK-BOLETO-VALOR-JUROS
           MOVE WRK-BOLETO-DET5        TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE WRK-BOLETO-DET6        TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE VALOR-TOTAL            TO WRK-BOLETO-TOTAL
           MOVE WRK-BOLETO-DET7        TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE WRK-BOLETO-LINHA       TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE TITULOS-SACADO         TO WRK-BOLETO-SACADO
           MOVE WRK-BOLETO-DET8        TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE WRK-BOLETO-LINHA       TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           *> Linha digitavel totalmente ficticia
           MOVE 
             '00190.00009 01234.5678900 00000.000000 0 00000000000000' 
                                       TO WRK-BOLETO-DIGITAVEL
           MOVE WRK-BOLETO-DET9        TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE WRK-BOLETO-LINHA       TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           MOVE WRK-BOLETO-LINHA-BRANCO
                                       TO FD-LIN-BOLETOS
           WRITE FD-LIN-BOLETOS

           ADD 1                       TO WRK-IND3
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-finalizar
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQUIVO-ENT
                 ARQUIVO-PARM
                 ARQUIVO-SAI
           
           DISPLAY '********************************'
           DISPLAY '*        FIM DE PROGRAMA       *'
           DISPLAY '*------------------------------*'
           DISPLAY '* TITULOS PROCESSADOS:      ' WRK-IND2 ' *'
           DISPLAY '* REGISTROS GRAVADOS :      ' WRK-IND3 ' *'
           DISPLAY '********************************'
           STOP RUN 
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
