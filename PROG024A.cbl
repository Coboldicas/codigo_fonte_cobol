      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 03/11/2025
      * DESCRICAO..: SIMULACAO DE SISTEMAS DE BIBLIOTECA EM COBOL
      * NOME.......: PROG024A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG024A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT ASSIGN TO 'BIBLIOTECA.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ARQUIVO-SAI ASSIGN TO 'BIBLIOTECA.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT.
       01  FD-ARQ-ENT.
         05  FDE-COD-LIVRO                PIC 9(05) VALUE ZEROS.
         05  FDE-NOME-LIVRO               PIC X(40) VALUE SPACES.
         05  FDE-AUTOR-LIVRO              PIC X(20) VALUE SPACES.
         05  FDE-DISPONIVEL               PIC X(01) VALUE SPACES.
         05  FDE-DATA-EMPRESTIMO          PIC X(10) VALUE SPACES.

       FD  ARQUIVO-SAI.
       01  FD-ARQ-SAI.
         05  FDS-TRANSACAO                PIC X(10) VALUE SPACES.
         05  FDS-BARRA1                   PIC X(01) VALUE SPACES.         
         05  FDS-DATA-TRANSACAO           PIC X(10) VALUE SPACES.
         05  FDS-BARRA2                   PIC X(01) VALUE SPACES.
         05  FDS-COD-LIVRO                PIC 9(05) VALUE ZEROS.
         05  FDS-BARRA3                   PIC X(01) VALUE SPACES.
         05  FDS-NOME-LIVRO               PIC X(40) VALUE SPACES.
         05  FDS-BARRA4                   PIC X(01) VALUE SPACES.
         05  FDS-AUTOR-LIVRO              PIC X(20) VALUE SPACES.
         05  FDS-BARRA5                   PIC X(01) VALUE SPACES.
         05  FDS-DISPONIVEL               PIC X(01) VALUE SPACES.
         05  FDS-BARRA6                   PIC X(01) VALUE SPACES.
         05  FDS-DATA-EMPRESTIMO          PIC X(10) VALUE SPACES.
         05  FDS-BARRA7                   PIC X(01) VALUE SPACES.
         05  FDS-DATA-DEVOLUCAO           PIC X(10) VALUE SPACES.


       WORKING-STORAGE SECTION.
     
      *  CAMPOS DE ENTRADA DE DADOS
       01  WRK-COD-LIVRO                PIC 9(05) VALUE ZEROS.
       01  WRK-NOME-LIVRO               PIC X(40) VALUE SPACES.
       01  WRK-AUTOR-LIVRO              PIC X(20) VALUE SPACES.
       01  WRK-DISPONIVEL               PIC X(01) VALUE SPACES.
       01  WRK-DATA-EMPRESTIMO          PIC X(10) VALUE SPACES.

      *  VARIAVEIS AUXILIARES
       01  WRK-FIM-ARQ                    PIC X(01) VALUE 'N'.
       01  WRK-IND1                       PIC 9(02) VALUE ZEROS.
       01  WRK-QTDE-REG                   PIC 9(02) VALUE ZEROS.
       01  WRK-ENCONTROU                  PIC X(01) VALUE 'N'.
       01  WRK-OPCAO                      PIC 9(01) VALUE ZEROS.
       01  WRK-RETURN-CODE                PIC S9(4) COMP VALUE ZERO.

      *  CAMPOS DE VALIDACAO
       01  WRK-VAL-CODIGO                 PIC X(01) VALUE 'N'.
       01  WRK-VAL-NOME                   PIC X(01) VALUE 'N'. 
       01  WRK-VAL-AUTOR                  PIC X(01) VALUE 'N'. 
       01  WRK-VAL-DATA                   PIC X(01) VALUE 'N'. 

      *  MASCARA FORMATO DA DATA - DD/MM/AAAA
       01  WRK-MASC-DATA.
         05 WRK-MASC-DATA-DIA          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-MES          PIC 9(002) VALUE ZEROS.
         05 FILLER                     PIC X(001) VALUE '/'.
         05 WRK-MASC-DATA-ANO          PIC 9(004) VALUE ZEROS.

      *    TABELA INTERNA COM 50 OCOCRRENCIAS 
       01  TAB-ENT-LINHA.
           05  TAB-ENT-LINHA OCCURS 50 TIMES.
               10  TAB-CODIGO         PIC 9(05)   VALUE ZEROS. 
               10  TAB-NOME           PIC X(40)   VALUE SPACES.
               10  TAB-AUTOR          PIC X(20)   VALUE SPACES.
               10  TAB-DISPONIVEL     PIC X(01)   VALUE SPACES.
               10  TAB-DATA           PIC X(10)   VALUE SPACES.

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

           PERFORM 0100-OBTER-DATA 
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
               EXTEND ARQUIVO-SAI

      *    LÊ O ARQUIVO E CARREGA NA TABELA INTERNA
           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                WRK-FIM-ARQ  EQUAL 'S' 
               PERFORM 0001-LER-ARQUIVO-ENT 
               IF WRK-FIM-ARQ          EQUAL 'N'
                  MOVE FDE-COD-LIVRO   TO TAB-CODIGO(WRK-IND1)
                  MOVE FDE-NOME-LIVRO  TO TAB-NOME(WRK-IND1)
                  MOVE FDE-AUTOR-LIVRO TO TAB-AUTOR(WRK-IND1)
                  MOVE FDE-DISPONIVEL  TO TAB-DISPONIVEL(WRK-IND1)
                  MOVE FDE-DATA-EMPRESTIMO
                                       TO TAB-DATA(WRK-IND1)
                  ADD 1                TO WRK-QTDE-REG
               END-IF 
           END-PERFORM 

           IF WRK-QTDE-REG >= 50
              DISPLAY 'TABELA CHEIA! - MAXIMO 50 REGISTROS'
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
           MOVE 'N'                    TO WRK-VAL-AUTOR   
           MOVE 'N'                    TO WRK-VAL-DATA

           DISPLAY '*==========================================*'
           DISPLAY '*=          SISTEMA DE BIBLIOTECA         =*'
           DISPLAY '*==========================================*'
           DISPLAY '* 1 - CADASTRAR LIVRO                      *'
           DISPLAY '* 2 - EMPRESTAR LIVRO                      *'
           DISPLAY '* 3 - DEVOLVER LIVRO                       *'
           DISPLAY '* 4 - LISTAR LIVRO                         *'                      
           DISPLAY '* 5 - BUSCAR POR CODIGO                    *'                      
           DISPLAY '* 9 - SAIR                                 *'
           DISPLAY '*==========================================*'
           DISPLAY 'FAVOR INFORMAR A OPÇÃO DESEJADA: '
                    WITH NO ADVANCING
           ACCEPT WRK-OPCAO

           EVALUATE WRK-OPCAO
               WHEN 1 PERFORM 0003-CADASTRAR-LIVRO
               WHEN 2 PERFORM 0004-EMPRESTAR-LIVRO 
               WHEN 3 PERFORM 0005-DEVOLVER-LIVRO 
               WHEN 4 PERFORM 0006-LISTAR-LIVRO                               
               WHEN 5 PERFORM 0008-BUSCAR-CODIGO                               
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
      *    CADASTRO DE LIVROS
      *----------------------------------------------------------------*
       0003-CADASTRAR-LIVRO            SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*========== CADASTRAR LIVRO =============*'
           PERFORM 0031-VALIDAR-COD   UNTIL WRK-VAL-CODIGO EQUAL 'S'

           PERFORM 0032-VALIDAR-NOME  UNTIL WRK-VAL-NOME   EQUAL 'S'

           PERFORM 0033-VALIDAR-AUTOR UNTIL WRK-VAL-AUTOR  EQUAL 'S'

           ADD 1                       TO WRK-QTDE-REG

           MOVE WRK-COD-LIVRO          TO TAB-CODIGO(WRK-QTDE-REG)
           MOVE WRK-NOME-LIVRO         TO TAB-NOME(WRK-QTDE-REG)
           MOVE WRK-AUTOR-LIVRO        TO TAB-AUTOR(WRK-QTDE-REG)
           MOVE 'S'                    TO TAB-DISPONIVEL(WRK-QTDE-REG)
           MOVE SPACES                 TO TAB-DATA(WRK-QTDE-REG)           

           PERFORM 0009-GRAVAR-LOG
           PERFORM 0002-TELA-MENU 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDACAO CAMPO CODIGO LIVRO
      *----------------------------------------------------------------*
       0031-VALIDAR-COD                SECTION.
      *----------------------------------------------------------------*

           DISPLAY '* CODIGO: 'WITH NO ADVANCING
           ACCEPT WRK-COD-LIVRO
           IF WRK-COD-LIVRO            EQUAL ZEROS
              DISPLAY 'CODIGO DO LIVRO NAO PODE SER ZERADO'
           ELSE
              PERFORM 0041-CONSULTAR-LIVRO
              IF WRK-ENCONTROU         EQUAL 'S'
                 IF WRK-OPCAO          EQUAL 1
                    DISPLAY 'CODIGO DO LIVRO JA CADASTRADO'           
                    MOVE 'N'           TO WRK-VAL-CODIGO
                 ELSE
                    MOVE 'S'           TO WRK-VAL-CODIGO
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
      *    VALIDACAO CAMPO NOME LIVRO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0032-validar-nome
       0032-VALIDAR-NOME               SECTION.
      *----------------------------------------------------------------*

           DISPLAY '* NOME:   'WITH NO ADVANCING
           ACCEPT WRK-NOME-LIVRO

           IF WRK-NOME-LIVRO         EQUAL SPACES
              DISPLAY 'NOME DO LIVRO NAO PODE SER EM BRANCO'
           ELSE
              MOVE 'S'                 TO WRK-VAL-NOME
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0032-end
       0032-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDACAO CAMPO NOME LIVRO
      *----------------------------------------------------------------*
       0033-VALIDAR-AUTOR              SECTION.
      *----------------------------------------------------------------*

           DISPLAY '* NOME:   'WITH NO ADVANCING
           ACCEPT WRK-AUTOR-LIVRO

           IF WRK-AUTOR-LIVRO        EQUAL SPACES
              DISPLAY 'NOME DO AUTOR NAO PODE SER EM BRANCO'
           ELSE
              MOVE 'S'                 TO WRK-VAL-AUTOR
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0033-end
       0033-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ENTRADA DE CONTROLE DE ESTOQUE - TABELA INTERNA
      *----------------------------------------------------------------*
       0004-EMPRESTAR-LIVRO               SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*========== ENTRADA DE LIVRO =============*'
           PERFORM 0031-VALIDAR-COD UNTIL WRK-VAL-CODIGO EQUAL 'S'
           PERFORM 0041-CONSULTAR-LIVRO
           
           IF WRK-ENCONTROU            EQUAL 'S'
              DISPLAY '* 'TAB-CODIGO(WRK-IND1) ' | '
                          TAB-NOME(WRK-IND1)   ' | ' 
                          TAB-AUTOR(WRK-IND1)   ' | '
                          TAB-DISPONIVEL(WRK-IND1) ' | '
                          TAB-DATA(WRK-IND1)  '     *'

              IF TAB-DISPONIVEL(WRK-IND1) 
                                       EQUAL 'N'      
                 DISPLAY 'LIVRO NAO DISPONIVEL PARA EMPRESTIMO' 
              ELSE
                 PERFORM 0042-EFETIVAR-EMPR
              END-IF
           ELSE 
              DISPLAY 'LIVRO NAO CADASTRADO'
           END-IF
 
           PERFORM 0002-TELA-MENU 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PESQUISAR CODIGO LIVRO - TABELA INTERNA
      *----------------------------------------------------------------*
       0041-CONSULTAR-LIVRO             SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ENCONTROU
           MOVE ZEROS                  TO WRK-IND1

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                   (WRK-IND1 > WRK-QTDE-REG) OR
                                   (WRK-ENCONTROU EQUAL 'S')
              IF WRK-COD-LIVRO    EQUAL TAB-CODIGO(WRK-IND1)
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
      *    EFETIVAR ENTRADA DE LIVROS
      *----------------------------------------------------------------*
       0042-EFETIVAR-EMPR               SECTION.
      *----------------------------------------------------------------*

                 MOVE 'N'              TO TAB-DISPONIVEL(WRK-IND1)
                 MOVE WRK-MASC-DATA    TO TAB-DATA(WRK-IND1)         

                 PERFORM 0009-GRAVAR-LOG
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0042-end
       0042-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    SAIDA DE CONTROLE DE ESTOQUE - TABELA INTERNA
      *----------------------------------------------------------------*
       0005-DEVOLVER-LIVRO                 SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*==========  DEVOLUCAO DE LIVRO  =============*'
           PERFORM 0031-VALIDAR-COD UNTIL WRK-VAL-CODIGO EQUAL 'S'
           PERFORM 0041-CONSULTAR-LIVRO
           
           IF WRK-ENCONTROU            EQUAL 'S'
              DISPLAY '* 'TAB-CODIGO(WRK-IND1) ' | '
                          TAB-NOME(WRK-IND1)   ' | ' 
                          TAB-AUTOR(WRK-IND1)   ' | '
                          TAB-DISPONIVEL(WRK-IND1) ' | '
                          TAB-DATA(WRK-IND1)  '     *'

              IF TAB-DISPONIVEL(WRK-IND1) 
                                       EQUAL 'S'
                 DISPLAY 'FAVOR VERIFICAR O CODIGO CORRETO DO LIVRO'
              ELSE 
                 MOVE 'S'              TO TAB-DISPONIVEL(WRK-IND1)
                 MOVE WRK-MASC-DATA    TO TAB-DATA(WRK-IND1)         
                 PERFORM 0009-GRAVAR-LOG
              END-IF
           ELSE 
              DISPLAY 'LIVRO NAO CADASTRADO'
           END-IF

           PERFORM 0002-TELA-MENU 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LISTAR CONTROLE DE ESTOQUE - TABELA INTERNA
      *----------------------------------------------------------------*
       0006-LISTAR-LIVRO                 SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*========== LISTA DE LIVROS =============*'
           DISPLAY '* CODIGO| NOME                 | AUTOR     *'
           DISPLAY '*------------------------------------------*'
           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                         (WRK-IND1 > WRK-QTDE-REG) 
              DISPLAY '* 'TAB-CODIGO(WRK-IND1) ' | '
                          TAB-NOME(WRK-IND1)   ' | ' 
                          TAB-AUTOR(WRK-IND1)   ' | '
                          TAB-DISPONIVEL(WRK-IND1) ' | '
                          TAB-DATA(WRK-IND1)  '     *'
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
                                       TO FDE-COD-LIVRO
                  MOVE TAB-NOME(WRK-IND1)
                                       TO FDE-NOME-LIVRO
                  MOVE TAB-AUTOR(WRK-IND1)
                                       TO FDE-AUTOR-LIVRO
                  MOVE TAB-DISPONIVEL(WRK-IND1)
                                       TO FDE-DISPONIVEL
                  MOVE TAB-DATA(WRK-IND1)
                                       TO FDE-DATA-EMPRESTIMO
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
       0008-BUSCAR-CODIGO                 SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*=========== CONSULTAR CODIGO =============*'
           DISPLAY '* CODIGO| NOME                 | AUTOR     *'
           DISPLAY '*------------------------------------------*'
           PERFORM 0031-VALIDAR-COD UNTIL WRK-VAL-CODIGO EQUAL 'S'
           PERFORM 0041-CONSULTAR-LIVRO
           IF WRK-ENCONTROU          EQUAL 'S'
              DISPLAY '* 'TAB-CODIGO(WRK-IND1) ' | '
                          TAB-NOME(WRK-IND1)   ' | ' 
                          TAB-AUTOR(WRK-IND1)   ' | '
                          TAB-DISPONIVEL(WRK-IND1) ' | '
                          TAB-DATA(WRK-IND1)  '     *'
           ELSE 
              DISPLAY 'LIVRO NAO CADASTRADO'
           END-IF

           PERFORM 0002-TELA-MENU 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0008-end
       0008-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVACAO DO ARQUIVO DE LOG
      *----------------------------------------------------------------*
       0009-GRAVAR-LOG                 SECTION.
      *----------------------------------------------------------------*

           MOVE '|'                    TO FDS-BARRA1
                                          FDS-BARRA2
                                          FDS-BARRA3
                                          FDS-BARRA4
                                          FDS-BARRA5
                                          FDS-BARRA6
                                          FDS-BARRA7

           MOVE WRK-MASC-DATA          TO FDS-DATA-TRANSACAO

           IF WRK-OPCAO                EQUAL 1 
              MOVE 'CADASTRAR'         TO FDS-TRANSACAO
              MOVE WRK-COD-LIVRO       TO FDS-COD-LIVRO      
              MOVE WRK-NOME-LIVRO      TO FDS-NOME-LIVRO     
              MOVE WRK-AUTOR-LIVRO     TO FDS-AUTOR-LIVRO    
              MOVE 'S'                 TO FDS-DISPONIVEL     
              MOVE SPACES              TO FDS-DATA-EMPRESTIMO
              MOVE SPACES              TO FDS-DATA-DEVOLUCAO
           END-IF

           IF WRK-OPCAO                EQUAL 2 
              MOVE 'EMPRESTAR'         TO FDS-TRANSACAO
              MOVE TAB-CODIGO(WRK-IND1)
                                      TO FDS-COD-LIVRO      
              MOVE TAB-NOME(WRK-IND1)  TO FDS-NOME-LIVRO     
              MOVE TAB-AUTOR(WRK-IND1) TO FDS-AUTOR-LIVRO    
              MOVE TAB-DISPONIVEL(WRK-IND1)
                                       TO FDS-DISPONIVEL     
              MOVE TAB-DATA(WRK-IND1)  TO FDS-DATA-EMPRESTIMO
              MOVE SPACES              TO FDS-DATA-DEVOLUCAO
           END-IF

           IF WRK-OPCAO                EQUAL 3 
              MOVE 'DEVOLVER'          TO FDS-TRANSACAO
              MOVE TAB-CODIGO(WRK-IND1)
                                      TO FDS-COD-LIVRO      
              MOVE TAB-NOME(WRK-IND1)  TO FDS-NOME-LIVRO     
              MOVE TAB-AUTOR(WRK-IND1) TO FDS-AUTOR-LIVRO    
              MOVE TAB-DISPONIVEL(WRK-IND1)
                                       TO FDS-DISPONIVEL     
              MOVE TAB-DATA(WRK-IND1)  TO FDS-DATA-EMPRESTIMO
              MOVE WRK-MASC-DATA       TO FDS-DATA-DEVOLUCAO
           END-IF

           WRITE FD-ARQ-SAI
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0009-end
       0009-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER DATA SISTEMA
      *----------------------------------------------------------------*
       0100-OBTER-DATA                  SECTION.
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
      *> cobol-lint CL002 0100-end
       0100-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-finalizar
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQUIVO-SAI
           DISPLAY 'FIM DE PROGRAMA'
           STOP RUN 
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*
