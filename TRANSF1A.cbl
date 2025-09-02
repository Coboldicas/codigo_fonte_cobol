      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 18/08/2025
      * DESCRICAO..: SIMULACAO TRANSFERENCIA BANCARIA - COM LOG 
      * NOME.......: TRANSF1A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSF1A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTA-ENT ASSIGN TO 'CONTAENT.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CONTA-SAI ASSIGN TO 'CONTASAI.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TXT-LOG   ASSIGN TO 'TXTLOG.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CONTA-ENT.
       01  FD-ENT-LINHA.
         05  FDE-NUM-CONTA                PIC 9(10) VALUE ZEROS.
         05  FDE-NOME                     PIC X(20) VALUE SPACES.
         05  FDE-SALDO-STR                PIC X(12) VALUE SPACES.

       FD  CONTA-SAI.
       01  FDS-SAI-LINHA.
         05  FDS-NUM-CONTA                PIC 9(10) VALUE ZEROS.
         05  FDS-NOME                     PIC X(20) VALUE SPACES.
         05  FDS-SALDO-STR                PIC X(12) VALUE ZEROS.

       FD  TXT-LOG.
       01  WRK-LOG-REGISTRO.
         05  FD-LOG-LINHA                PIC X(200).

       WORKING-STORAGE SECTION.
      *> ---------------- Entradas interativas ----------------
       01  WS-CONTA-A                  PIC 9(10) VALUE ZEROS.
       01  WS-CONTA-B                  PIC 9(10) VALUE ZEROS.
       01  WS-VALOR                    PIC 9(09)V99 VALUE ZEROS.

       01  WRK-VALOR-STR               PIC X(12) VALUE ZEROS.
       01  WRK-VALOR-NUM REDEFINES WRK-VALOR-STR.
           05 WRK-VALOR-V99            PIC 9(9)V99.

      *> ---------------- Registro parser (arquivo) -----------
       01  F-SALDO                     PIC 9(9)V99 VALUE 0.

      *> ---------------- Contas em memória -------------------
       01  A-NUM-CONTA                 PIC X(10) VALUE SPACES.
       01  A-NOME                      PIC X(20) VALUE SPACES.
       01  A-SALDO                     PIC 9(9)V99 VALUE 0.

       01  B-NUM-CONTA                 PIC X(10) VALUE SPACES.
       01  B-NOME                      PIC X(20) VALUE SPACES.
       01  B-SALDO                     PIC 9(9)V99 VALUE 0.

      *> ---------------- Flags de controle -------------------
       01  WS-STATUS                   PIC 9 VALUE 0.
           88  OK                      VALUE 0.
           88  ERRO                    VALUE 1.
       01  ENCONTROU-A                 PIC X VALUE 'N'.
       01  ENCONTROU-B                 PIC X VALUE 'N'.

      *> ---------------- Datas e formatos --------------------
       01  WS-DATE                     PIC 9(8).   *> AAAAMMDD
       01  WS-TIME                     PIC 9(6).   *> HHMMSS
       01  A-SALDO-EDIT                PIC ZZZ.ZZZ.ZZZ,99.
       01  B-SALDO-EDIT                PIC ZZZ.ZZZ.ZZZ,99.
       01  F-SALDO-EDIT                PIC ZZZ.ZZZ.ZZZ,99.

      *> ---------------- Mensagens ---------------------------
       01  WS-MOTIVO                   PIC X(60) VALUE SPACES.

      *  Variável para armazenar o código de retorno das chamadas
       01  WRK-RETURN-CODE             PIC S9(4) COMP VALUE ZERO.

       01  FIM-ARQUIVO                 PIC X(01) VALUE "N".
       01  WRK-IND1                    PIC 9(02) VALUE ZEROS.
       01  WRK-INDA                    PIC 9(02) VALUE ZEROS.
       01  WRK-INDB                    PIC 9(02) VALUE ZEROS.
       01  WRK-QTDE-REG                PIC 9(02) VALUE ZEROS.

       01  TAB-ENT-LINHA.
           05  TAB-ENT-LINHA OCCURS 10 TIMES.
               10  TAB-NUM-CONTA     PIC 9(10) VALUE ZEROS.
               10  TAB-NOME          PIC X(20) VALUE SPACES.
               10  TAB-SALDO-STR     PIC 9(09)V99 VALUE ZEROS.

      *     DEFINICAO DE DATA E HORA DO SISTEMA. 
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

           PERFORM 0001-OBTER-DATA
           PERFORM 0002-OBTER-HORA
           PERFORM 0004-CARREGAR-CONTAS
           PERFORM 0005-INSERIR-TRANSF
           PERFORM 0006-VALIDAR-VALOR
           IF ENCONTROU-A EQUAL 'S' AND
              ENCONTROU-B EQUAL 'S'
              PERFORM 0009-PROCESSAR-TRANSF
           END-IF
           PERFORM 0010-ATUALIZAR-ARQUIVO
           PERFORM 0011-GRAVAR-LOG
           IF OK
               MOVE A-SALDO TO A-SALDO-EDIT
               MOVE B-SALDO TO B-SALDO-EDIT
               DISPLAY 'Transferência realizada com sucesso.'
               DISPLAY 'Origem:  ' A-NUM-CONTA ' Novo saldo: ' 
                                  A-SALDO-EDIT
               DISPLAY 'Destino: ' B-NUM-CONTA ' Novo saldo: '
                                  B-SALDO-EDIT
           ELSE
               DISPLAY 'Falha: ' WS-MOTIVO
           END-IF

           PERFORM 9999-FINALIZAR
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DATA SISTEMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-obter-data
       0001-OBTER-DATA                  SECTION.
      *----------------------------------------------------------------*

            CALL 'PROGDATA' USING COD001A-REGISTRO

            MOVE RETURN-CODE TO WRK-RETURN-CODE

            IF WRK-RETURN-CODE NOT = 0
               DISPLAY 'ERRO NA CHAMADA PROGDATA. RETURN-CODE: '
                WRK-RETURN-CODE
               STOP RUN
            END-IF

            MOVE COD001A-DATA-ANO      TO WS-DATE(1:4) 
            MOVE COD001A-DATA-MES      TO WS-DATE(5:2) 
            MOVE COD001A-DATA-DIA      TO WS-DATE(7:2) 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER HORA SISTEMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-obter-hora
       0002-OBTER-HORA                 SECTION.
      *----------------------------------------------------------------*

            CALL 'PROGTIME' USING COD001A-REGISTRO 

            MOVE RETURN-CODE TO WRK-RETURN-CODE

            IF WRK-RETURN-CODE NOT = 0
               DISPLAY 'ERRO NA CHAMADA PROGDATA. RETURN-CODE: '
                WRK-RETURN-CODE
               STOP RUN
            END-IF

            MOVE COD001A-HORA          TO WS-TIME(1:2)
            MOVE COD001A-MINUTO        TO WS-TIME(3:2)
            MOVE COD001A-SEGUNDO       TO WS-TIME(5:2)
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DE ARQUIVO DE ENTRADA - CONTA-ENT
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-ler-conta-ent
       0003-LER-CONTA-ENT              SECTION.
      *----------------------------------------------------------------*

           READ CONTA-ENT INTO FD-ENT-LINHA
               AT END MOVE "S" TO FIM-ARQUIVO
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CARREGADOS DADOS DAS CONTAS PARA TRANSFERENCIA:
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-carregar-contas
       0004-CARREGAR-CONTAS            SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT CONTA-ENT

      *    LÊ O ARQUIVO E CARREGA NA TABELA INTERNA
           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                FIM-ARQUIVO  EQUAL 'S' 
               PERFORM 0003-LER-CONTA-ENT
               IF FIM-ARQUIVO          EQUAL 'N'
                  MOVE FDE-NUM-CONTA   TO TAB-NUM-CONTA(WRK-IND1)
                  MOVE FDE-NOME        TO TAB-NOME(WRK-IND1)
                  MOVE FDE-SALDO-STR   TO WRK-VALOR-STR
                  MOVE WRK-VALOR-V99   TO TAB-SALDO-STR(WRK-IND1)
                  ADD 1                TO WRK-QTDE-REG
               END-IF 
           END-PERFORM 

           CLOSE CONTA-ENT
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERE INFORMACOES DE TRANSFERENCIA ATRAVES DA TELA
      *----------------------------------------------------------------*
       0005-INSERIR-TRANSF             SECTION.
      *----------------------------------------------------------------*

           PERFORM 0051-INSERIR-ORIGEM 
           IF ENCONTROU-A EQUAL 'S'
              PERFORM 0052-INSERIR-DESTINO
              IF ENCONTROU-B EQUAL 'S'
      *     1. Conta A ≠ Conta B. 
                 IF WS-CONTA-A = WS-CONTA-B
                    DISPLAY 
                        'Conta origem deve ser diferente Conta destino'
                    PERFORM 0005-INSERIR-TRANSF
                 END-IF
              END-IF 
           END-IF 
           
           IF ENCONTROU-A EQUAL 'S' AND
              ENCONTROU-B EQUAL 'S'
              CONTINUE
           ELSE 
              PERFORM 0005-INSERIR-TRANSF
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERIR E VALIDAR CONTA ORIGEM
      *----------------------------------------------------------------*
       0051-INSERIR-ORIGEM             SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE NUMERO DA CONTA DE ORIGEM: '
                    WITH NO ADVANCING
           ACCEPT WS-CONTA-A

           MOVE WS-CONTA-A             TO A-NUM-CONTA 

           PERFORM 0007-PESQUISAR-ORIGEM 

           IF ENCONTROU-A EQUAL 'S'
              CONTINUE
           ELSE 
              DISPLAY 'NUMERO DA CONTA DE ORIGEM NÃO LOCALIZADO'              
              PERFORM 0051-INSERIR-ORIGEM 
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0051-end
       0051-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERIR E VALIDAR CONTA DESTINO
      *----------------------------------------------------------------*
       0052-INSERIR-DESTINO             SECTION.
      *----------------------------------------------------------------*

           DISPLAY 'DIGITE NUMERO DA CONTA DESTINO:   '
                    WITH NO ADVANCING
           ACCEPT WS-CONTA-B
           MOVE WS-CONTA-B              TO B-NUM-CONTA 

           PERFORM 0008-PESQUISAR-DESTINO 

           IF ENCONTROU-B EQUAL 'S'
              CONTINUE
           ELSE 
              DISPLAY 'NUMERO DA CONTA DE DESTINO NÃO LOCALIZADO'
              PERFORM 0052-INSERIR-DESTINO 
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0052-end
       0052-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDAR DADOS DE ENTRADA:
      *----------------------------------------------------------------*
       0006-VALIDAR-VALOR              SECTION.
      *----------------------------------------------------------------*
 
           DISPLAY 'INFORME O VALOR DE TRANSFERENCIA [ex: 100,00]:    '
                    WITH NO ADVANCING
           ACCEPT WS-VALOR 

      *     2. Valor > 0 e com 2 casas decimais.
           IF WS-VALOR > 0
              CONTINUE
           ELSE 
              DISPLAY 'Valor deve ser maior que 0.' 
              PERFORM 9999-FINALIZAR
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-end
       0006-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PESQUISAR CONTA ORIGEM
      *----------------------------------------------------------------*
       0007-PESQUISAR-ORIGEM           SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO ENCONTROU-A
           MOVE ZEROS                  TO WRK-IND1

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                   (WRK-IND1 > WRK-QTDE-REG) OR
                                   (ENCONTROU-A EQUAL 'S')
              IF A-NUM-CONTA           EQUAL TAB-NUM-CONTA(WRK-IND1)
                 MOVE 'S'              TO ENCONTROU-A
                 MOVE WRK-IND1         TO WRK-INDA
              ELSE 
                 MOVE 'N'              TO ENCONTROU-A
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-end
       0007-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PESQUISAR CONTA DESTINO
      *----------------------------------------------------------------*
       0008-PESQUISAR-DESTINO          SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO ENCONTROU-B
           MOVE ZEROS                  TO WRK-IND1

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                   (WRK-IND1 > WRK-QTDE-REG) OR
                                   (ENCONTROU-B EQUAL 'S')
              IF B-NUM-CONTA           EQUAL TAB-NUM-CONTA(WRK-IND1)
                 MOVE 'S'              TO ENCONTROU-B
                 MOVE WRK-IND1         TO WRK-INDB
              ELSE 
                 MOVE 'N'              TO ENCONTROU-B
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0008-end
       0008-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR REALIZAR A TRANSFERENCIA DE CONTAS
      *----------------------------------------------------------------*
       0009-PROCESSAR-TRANSF            SECTION.
      *----------------------------------------------------------------*
      *> Debita A
           SUBTRACT WS-VALOR FROM TAB-SALDO-STR(WRK-INDA)
               ON SIZE ERROR
                    SET ERRO TO TRUE
                    MOVE 'Erro aritmético ao debitar A' TO WS-MOTIVO
                    EXIT PARAGRAPH
                    PERFORM 9999-FINALIZAR
           END-SUBTRACT

           MOVE TAB-SALDO-STR(WRK-INDA) TO A-SALDO

      *> Credita B
           ADD WS-VALOR TO TAB-SALDO-STR(WRK-INDB)
               ON SIZE ERROR
      *> rollback simples em memória
                   ADD WS-VALOR TO TAB-SALDO-STR(WRK-INDA) 
                   SET ERRO TO TRUE
                   MOVE 'Erro aritmético ao creditar B' TO WS-MOTIVO
                   EXIT PARAGRAPH
           END-ADD

           MOVE TAB-SALDO-STR(WRK-INDB) TO B-SALDO
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0009-end
       0009-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR ATUALIZAR ARQUIVO DE CONTAS
      *    Regrava arquivo com novos saldos de A e B
      *----------------------------------------------------------------*
       0010-ATUALIZAR-ARQUIVO           SECTION.
      *----------------------------------------------------------------*
 
           OPEN OUTPUT CONTA-SAI

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                    WRK-IND1 > WRK-QTDE-REG
                  MOVE TAB-NUM-CONTA(WRK-IND1)
                                       TO FDS-NUM-CONTA
                  MOVE TAB-NOME(WRK-IND1)
                                       TO FDS-NOME
                  MOVE TAB-SALDO-STR(WRK-IND1)
                                       TO WRK-VALOR-V99
                  MOVE WRK-VALOR-STR   TO FDS-SALDO-STR
                  WRITE FDS-SAI-LINHA
           END-PERFORM

           CLOSE CONTA-SAI

           SET OK TO TRUE
           MOVE 'OK' TO WS-MOTIVO
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0010-end
       0010-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR GRAVAR LOG PARA AUDITORIA
      *----------------------------------------------------------------*
       0011-GRAVAR-LOG                SECTION.
      *----------------------------------------------------------------*

           OPEN OUTPUT TXT-LOG
           MOVE A-SALDO TO A-SALDO-EDIT
           MOVE B-SALDO TO B-SALDO-EDIT
           move WS-VALOR TO F-SALDO-EDIT

           MOVE 'LOG' TO WRK-LOG-REGISTRO 
           STRING
               WS-DATE '-' WS-TIME ' | '
               WS-CONTA-A ' => ' WS-CONTA-B ' | '
               'VALOR=' DELIMITED BY SIZE
               F-SALDO-EDIT DELIMITED BY SIZE
               ' | A=' A-SALDO-EDIT ' B=' B-SALDO-EDIT ' | '
               'STATUS=' WS-MOTIVO
               DELIMITED BY SIZE
               INTO WRK-LOG-REGISTRO
           END-STRING
 
           WRITE WRK-LOG-REGISTRO
           CLOSE TXT-LOG
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0011-end
       0011-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-finalizar
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*
           DISPLAY 'FIM DE PROGRAMA'
           STOP RUN 
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
