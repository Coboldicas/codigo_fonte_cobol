      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 29/09/2025
      * DESCRICAO..: CONTROLE DE ESTOQUE AVANÇADO
      * OBS........: ESSE PROGRAMA É UM PROGRAMA CLONE DO PROG023A
      * NOME.......: PROG023B
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG023B. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT ASSIGN TO 'ESTOQUE.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT.
       01  FD-ARQ-ENT.
         05  FDE-COD-PRODUTO              PIC 9(05) VALUE ZEROS.
         05  FDE-NOME-PRODUTO             PIC X(20) VALUE SPACES.
         05  FDE-QTDE-PRODUTO             PIC 9(05) VALUE ZEROS.
         05  FDE-VALOR-PRODUTO            PIC 9(15)V99 VALUE ZEROS.

       WORKING-STORAGE SECTION.
       01  WRK-FIM-ARQ                    PIC X(01) VALUE 'N'.
       01  WRK-IND1                       PIC 9(02) VALUE ZEROS.
       01  WRK-IND2                       PIC 9(02) VALUE ZEROS.
       01  WRK-QTDE-REG                   PIC 9(02) VALUE ZEROS.
       01  WRK-ENCONTROU                  PIC X(01) VALUE 'N'.
       01  WRK-EXCLUIR                    PIC X(01) VALUE 'N'.

       01  WRK-COD-PRODUTO                PIC 9(05) VALUE ZEROS.
       01  WRK-NOME-PRODUTO               PIC X(20) VALUE SPACES.
       01  WRK-QTDE-PRODUTO               PIC 9(05) VALUE ZEROS.
       01  WRK-VALOR-PRODUTO              PIC 9(15)V99 VALUE ZEROS.

       01  WRK-VAL-CODIGO                 PIC X(01) VALUE 'N'.
       01  WRK-VAL-NOME                   PIC X(01) VALUE 'N'. 
       01  WRK-VAL-QTDE                   PIC X(01) VALUE 'N'. 
       01  WRK-VAL-VALOR                  PIC X(01) VALUE 'N'. 

       01  WRK-OPCAO                      PIC 9(01) VALUE ZEROS.

       01  TAB-ENT-LINHA.
           05  TAB-ENT-LINHA OCCURS 50 TIMES.
               10  TAB-CODIGO         PIC 9(05)   VALUE ZEROS. 
               10  TAB-NOME           PIC X(20)   VALUE SPACES.
               10  TAB-QTDE           PIC 9(05)   VALUE ZEROS.
               10  TAB-VALOR          PIC 9(15)V99 VALUE ZEROS.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0011-CARREGAR-TABELA-INT
           PERFORM 0002-TELA-MENU 
           PERFORM 0007-GRAVAR-ARQUIVO
           PERFORM 9999-FINALIZAR
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DE ARQUIVO DE ENTRADA 
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-ler-arquivo-ent
       0001-LER-ARQUIVO-ENT            SECTION.
      *----------------------------------------------------------------*

           READ ARQUIVO-ENT INTO FD-ARQ-ENT
               AT END MOVE "S" TO WRK-FIM-ARQ 
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DE ARQUIVO DE ENTRADA 
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-ler-arquivo-ent
       0011-CARREGAR-TABELA-INT        SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQUIVO-ENT

      *    LÊ O ARQUIVO E CARREGA NA TABELA INTERNA
           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                WRK-FIM-ARQ  EQUAL 'S' 
               PERFORM 0001-LER-ARQUIVO-ENT 
               IF WRK-FIM-ARQ          EQUAL 'N'
                  MOVE FDE-COD-PRODUTO TO TAB-CODIGO(WRK-IND1)
                  MOVE FDE-NOME-PRODUTO
                                       TO TAB-NOME(WRK-IND1)
                  MOVE FDE-QTDE-PRODUTO
                                       TO TAB-QTDE(WRK-IND1)
                  MOVE FDE-VALOR-PRODUTO
                                       TO TAB-VALOR(WRK-IND1)
                  ADD 1                TO WRK-QTDE-REG
               END-IF 
           END-PERFORM 

           IF WRK-QTDE-REG >= 50
              DISPLAY 'AGENDA CHEIA! - MAXIMO 50 REGISTROS'
              DISPLAY ' FOI ATINGIDO A QUANTIDADE MAXIMA DE REGISTROS'
              CLOSE ARQUIVO-ENT
              PERFORM 9999-FINALIZAR
           ELSE
              CLOSE ARQUIVO-ENT
           END-IF

           MOVE ZEROS                  TO WRK-IND1
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0011-end
       0011-END.                       EXIT.
      *----------------------------------------------------------------*


      *----------------------------------------------------------------*
      *    TELA MENU
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-tela-menu
       0002-TELA-MENU                  SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-VAL-CODIGO
           MOVE 'N'                    TO WRK-VAL-NOME
           MOVE 'N'                    TO WRK-VAL-QTDE
           MOVE 'N'                    TO WRK-VAL-VALOR

           DISPLAY '*==========================================*'
           DISPLAY '*=          CONTROLE DE ESTOQUE           =*'
           DISPLAY '*==========================================*'
           DISPLAY '* 1 - CADASTRAR PRODUTO                    *'
           DISPLAY '* 2 - ENTRADA DE PRODUTO                   *'
           DISPLAY '* 3 - SAIDA DE PRODUTO                     *'
           DISPLAY '* 4 - LISTAR PRODUTO                       *'                      
           DISPLAY '* 5 - EXCLUIR PRODUTO                      *'                      
           DISPLAY '* 9 - SAIR                                 *'
           DISPLAY '*==========================================*'
           DISPLAY 'FAVOR INFORMAR A OPÇÃO DESEJADA: '
                    WITH NO ADVANCING
           ACCEPT WRK-OPCAO

           EVALUATE WRK-OPCAO
               WHEN 1 PERFORM 0003-CADASTRAR-PROD
               WHEN 2 PERFORM 0004-ENTRADA-PROD 
               WHEN 3 PERFORM 0005-SAIDA-PROD 
               WHEN 4 PERFORM 0006-LISTAR-PROD                               
               WHEN 5 PERFORM 0008-EXCLUIR-PROD                               
               WHEN 9 CONTINUE
               WHEN OTHER
                    DISPLAY 'OPÇÃO INVÁLIDA!' 
           END-EVALUATE
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CADASTRO DE CONTROLE DE ESTOQUE - ARQUIVO SEQUENCIAL
      *----------------------------------------------------------------*
       0003-CADASTRAR-PROD             SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*========== CADASTRAR PRODUTO =============*'
           PERFORM 0031-VALIDAR-PROD UNTIL WRK-VAL-CODIGO EQUAL 'S'

           PERFORM 0032-VALIDAR-NOME UNTIL WRK-VAL-NOME EQUAL 'S'

           PERFORM 0033-VALIDAR-QTDE UNTIL WRK-VAL-QTDE EQUAL 'S'

           PERFORM 0034-VALIDAR-VALOR 
                                     UNTIL WRK-VAL-VALOR EQUAL 'S'

           ADD 1                       TO WRK-QTDE-REG

           MOVE WRK-COD-PRODUTO        TO TAB-CODIGO(WRK-QTDE-REG)
           MOVE WRK-NOME-PRODUTO       TO TAB-NOME(WRK-QTDE-REG)
           MOVE WRK-QTDE-PRODUTO       TO TAB-QTDE(WRK-QTDE-REG)
           MOVE WRK-VALOR-PRODUTO      TO TAB-VALOR(WRK-QTDE-REG)           

           PERFORM 0002-TELA-MENU 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDACAO CAMPO CODIGO PRODUTO
      *----------------------------------------------------------------*
       0031-VALIDAR-PROD               SECTION.
      *----------------------------------------------------------------*

           DISPLAY '* CODIGO: 'WITH NO ADVANCING
           ACCEPT WRK-COD-PRODUTO
           IF WRK-COD-PRODUTO          EQUAL ZEROS
              DISPLAY 'CODIGO DO PRODUTO NAO PODE SER ZERADO'
           ELSE
              PERFORM 0041-CONSULTAR-PROD
              IF WRK-ENCONTROU         EQUAL 'S'
                 IF WRK-OPCAO          EQUAL 1
                    DISPLAY 'CODIGO DO PRODUTO JA CADASTRADO'           
                    MOVE 'N'              TO WRK-VAL-CODIGO
                 ELSE
                    MOVE 'S'              TO WRK-VAL-CODIGO
                 END-IF
              ELSE 
                 MOVE 'S'              TO WRK-VAL-CODIGO
              END-IF
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0031-end
       0031-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDACAO CAMPO NOME PRODUTO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0032-validar-nome
       0032-VALIDAR-NOME               SECTION.
      *----------------------------------------------------------------*

           DISPLAY '* NOME:   'WITH NO ADVANCING
           ACCEPT WRK-NOME-PRODUTO

           IF WRK-NOME-PRODUTO         EQUAL SPACES
              DISPLAY 'NOME DO PRODUTO NAO PODE SER EM BRANCO'
           ELSE
              MOVE 'S'                 TO WRK-VAL-NOME
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0032-end
       0032-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDACAO CAMPO NOME PRODUTO
      *----------------------------------------------------------------*
       0033-VALIDAR-QTDE               SECTION.
      *----------------------------------------------------------------*

           DISPLAY '* QTDE.:  'WITH NO ADVANCING
           ACCEPT WRK-QTDE-PRODUTO

           IF WRK-QTDE-PRODUTO         EQUAL ZEROS
              DISPLAY 'QUANTIDADE DE PRODUTO NAO PODE SER ZERADO'
           ELSE
              IF WRK-OPCAO             EQUAL 3 
                 IF WRK-QTDE-PRODUTO   GREATER TAB-QTDE(WRK-IND1)
                    DISPLAY 'ESTOQUE INSUFICIENTE'
                    MOVE 'N'           TO WRK-VAL-QTDE
                 ELSE 
                    MOVE 'S'           TO WRK-VAL-QTDE
                 END-IF
              ELSE
                 MOVE 'S'              TO WRK-VAL-QTDE
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0033-end
       0033-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDACAO CAMPO VALOR PRODUTO
      *----------------------------------------------------------------*
       0034-VALIDAR-VALOR              SECTION.
      *----------------------------------------------------------------*

           DISPLAY '* VALOR:   'WITH NO ADVANCING
           ACCEPT WRK-VALOR-PRODUTO

           IF WRK-VALOR-PRODUTO        EQUAL ZEROS 
              DISPLAY 'VALOR DO PRODUTO NAO PODE SER ZERADO'
           ELSE
              MOVE 'S'                 TO WRK-VAL-VALOR
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0034-end
       0034-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ENTRADA DE CONTROLE DE ESTOQUE - TABELA INTERNA
      *----------------------------------------------------------------*
       0004-ENTRADA-PROD               SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*========== ENTRADA DE PRODUTO =============*'
           PERFORM 0031-VALIDAR-PROD UNTIL WRK-VAL-CODIGO EQUAL 'S'
           PERFORM 0041-CONSULTAR-PROD

           DISPLAY '* 'TAB-CODIGO(WRK-IND1) ' | '
                       TAB-NOME(WRK-IND1)   ' | ' 
                       TAB-QTDE(WRK-IND1)   ' | '
                       TAB-VALOR(WRK-IND1)  '     *'

           PERFORM 0033-VALIDAR-QTDE UNTIL WRK-VAL-QTDE EQUAL 'S'
           ADD WRK-QTDE-PRODUTO      TO TAB-QTDE(WRK-IND1)

           PERFORM 0034-VALIDAR-VALOR UNTIL WRK-VAL-VALOR EQUAL 'S'
           MOVE WRK-VALOR-PRODUTO    TO TAB-VALOR(WRK-IND1)         

           PERFORM 0002-TELA-MENU 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PESQUISAR CODIGO PRODUTO - TABELA INTERNA
      *----------------------------------------------------------------*
       0041-CONSULTAR-PROD             SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ENCONTROU
           MOVE ZEROS                  TO WRK-IND1

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                   (WRK-IND1 > WRK-QTDE-REG) OR
                                   (WRK-ENCONTROU EQUAL 'S')
              IF WRK-COD-PRODUTO    EQUAL TAB-CODIGO(WRK-IND1)
                 MOVE 'S'           TO WRK-ENCONTROU
                 SUBTRACT 1 FROM WRK-IND1
              ELSE 
                 CONTINUE 
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0041-end
       0041-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    SAIDA DE CONTROLE DE ESTOQUE - TABELA INTERNA
      *----------------------------------------------------------------*
       0005-SAIDA-PROD                 SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*==========  SAIDA DE PRODUTO  =============*'
           PERFORM 0031-VALIDAR-PROD UNTIL WRK-VAL-CODIGO EQUAL 'S'
           PERFORM 0041-CONSULTAR-PROD

           DISPLAY '* 'TAB-CODIGO(WRK-IND1) ' | '
                       TAB-NOME(WRK-IND1)   ' | ' 
                       TAB-QTDE(WRK-IND1)   ' | '
                       TAB-VALOR(WRK-IND1)  '     *'

           PERFORM 0033-VALIDAR-QTDE UNTIL WRK-VAL-QTDE EQUAL 'S'

           SUBTRACT WRK-QTDE-PRODUTO FROM TAB-QTDE(WRK-IND1)
           
           PERFORM 0002-TELA-MENU 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LISTAR CONTROLE DE ESTOQUE - TABELA INTERNA
      *----------------------------------------------------------------*
       0006-LISTAR-PROD                 SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*========== LISTA DE PRODUTOS =============*'
           DISPLAY '* CODIGO| NOME                 | QTDE.     *'
           DISPLAY '*------------------------------------------*'
           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                         (WRK-IND1 > WRK-QTDE-REG) 
              DISPLAY '* 'TAB-CODIGO(WRK-IND1) ' | '
                          TAB-NOME(WRK-IND1)   ' | ' 
                          TAB-QTDE(WRK-IND1)   ' | '
                          TAB-VALOR(WRK-IND1)  '     *'
           END-PERFORM
           DISPLAY '*==========================================*'
           
           PERFORM 0002-TELA-MENU 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-end
       0006-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR ARQUIVO SEQUENCIAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-gravar-arquivo
       0007-GRAVAR-ARQUIVO             SECTION.
      *----------------------------------------------------------------*
 
           OPEN OUTPUT ARQUIVO-ENT

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                    WRK-IND1 > WRK-QTDE-REG
                  MOVE TAB-CODIGO(WRK-IND1)
                                       TO FDE-COD-PRODUTO
                  MOVE TAB-NOME(WRK-IND1)
                                       TO FDE-NOME-PRODUTO
                  MOVE TAB-QTDE(WRK-IND1)
                                       TO FDE-QTDE-PRODUTO
                  MOVE TAB-VALOR(WRK-IND1)
                                       TO FDE-VALOR-PRODUTO
                  WRITE FD-ARQ-ENT
           END-PERFORM

           CLOSE ARQUIVO-ENT
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-end
       0007-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EXCLUIR CONTROLE DE ESTOQUE - TABELA INTERNA
      *----------------------------------------------------------------*
       0008-EXCLUIR-PROD                 SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*=========== EXCLUIR PRODUTOS =============*'
           DISPLAY '* CODIGO| NOME                 | QTDE.     *'
           DISPLAY '*------------------------------------------*'
           PERFORM 0031-VALIDAR-PROD UNTIL WRK-VAL-CODIGO EQUAL 'S'
           PERFORM 0041-CONSULTAR-PROD

           DISPLAY '* 'TAB-CODIGO(WRK-IND1) ' | '
                       TAB-NOME(WRK-IND1)   ' | ' 
                       TAB-QTDE(WRK-IND1)   ' | '
                       TAB-VALOR(WRK-IND1)  '     *'


           DISPLAY 'CONFIRMA EXCLUSAO DO PRODUTO? (S/N)'
                    WITH NO ADVANCING
           ACCEPT WRK-EXCLUIR

           PERFORM 0081-EXCLUIR-PROD
           PERFORM 0002-TELA-MENU 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0008-end
       0008-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    EXCLUIR PRODUTO - TABELA INTERNA
      *----------------------------------------------------------------*
       0081-EXCLUIR-PROD             SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ENCONTROU
           MOVE ZEROS                  TO WRK-IND1

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                   (WRK-IND1 > WRK-QTDE-REG)
              IF WRK-COD-PRODUTO    EQUAL TAB-CODIGO(WRK-IND1)
                 MOVE 'S'           TO WRK-ENCONTROU
                
                 SUBTRACT 1 FROM WRK-QTDE-REG

                 MOVE TAB-CODIGO(WRK-IND1 + 1)
                                       TO TAB-CODIGO(WRK-IND1)
                 MOVE TAB-NOME(WRK-IND1 + 1)
                                       TO TAB-NOME(WRK-IND1)
                 MOVE TAB-QTDE(WRK-IND1 + 1 )
                                       TO TAB-QTDE(WRK-IND1)
                 MOVE TAB-VALOR(WRK-IND1 + 1)
                                       TO TAB-VALOR(WRK-IND1)
              ELSE 
                 IF WRK-ENCONTROU EQUAL 'S'
                    
                    MOVE TAB-CODIGO(WRK-IND1 + 1)
                                       TO TAB-CODIGO(WRK-IND1)
                    MOVE TAB-NOME(WRK-IND1 + 1)
                                       TO TAB-NOME(WRK-IND1)
                    MOVE TAB-QTDE(WRK-IND1 + 1 )
                                       TO TAB-QTDE(WRK-IND1)
                    MOVE TAB-VALOR(WRK-IND1 + 1)
                                       TO TAB-VALOR(WRK-IND1)
                 END-IF
                 CONTINUE 
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0081-end
       0081-END.                       EXIT.
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
