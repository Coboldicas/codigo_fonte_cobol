      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 01/09/2025
      * DESCRICAO..: AGENDA TELEFONICA - CADASTRAR E LISTAR NOME E
      *              TELEFONE EM ARQUIVO SEQUENCIAL 
      * NOME.......: CAD0005A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAD0005A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT ASSIGN TO 'CADAGENDA.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT.
       01  FD-ARQ-ENT.
         05  FDE-REG-NOME                 PIC X(30) VALUE SPACES.
         05  FDE-REG-TELEFONE             PIC X(15) VALUE SPACES.

       WORKING-STORAGE SECTION.
       01  WRK-OPCAO                      PIC 9(01) VALUE ZEROS.
       01  WRK-FS-ARQENT                  PIC X(02) VALUE SPACES.
       01  WRK-REG-NOME                   PIC X(30) VALUE SPACES.
       01  WRK-REG-TELEFONE               PIC X(15) VALUE SPACES.
       01  WRK-FIM-ARQ                    PIC X(01) VALUE 'N'.
       01  WRK-IND1                       PIC 9(02) VALUE ZEROS.
       01  WRK-ENCONTROU                  PIC X(01) VALUE 'N'.
       01  WRK-QTDE-REG                PIC 9(02) VALUE ZEROS.

       01  TAB-ENT-LINHA.
           05  TAB-ENT-LINHA OCCURS 10 TIMES.
               10  TAB-NOME          PIC X(30) VALUE SPACES.
               10  TAB-TELEFONE      PIC X(15) VALUE SPACES.

      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*

      *----------------------------------------------------------------*
      *    PROCESSAMENTO PRINCIPAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-processar
       0000-PROCESSAR                  SECTION.
      *----------------------------------------------------------------*

           PERFORM 0002-TELA-MENU
           PERFORM 9999-FINALIZAR
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LEITURA DE ARQUIVO DE ENTRADA 
      *----------------------------------------------------------------*
       0001-LER-ARQUIVO-ENT              SECTION.
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
      *    TELA MENU
      *----------------------------------------------------------------*
       0002-TELA-MENU                  SECTION.
      *----------------------------------------------------------------*

           DISPLAY '*========== AGENDA TELEFONICA =============*'
           DISPLAY '* 1 - CADASTRAR                            *'
           DISPLAY '* 2 - LISTAR                               *'
           DISPLAY '* 0 - SAIR                                 *'
           DISPLAY '*==========================================*'
           DISPLAY 'FAVOR INFORMAR A OPÇÃO DESEJADA: '
                    WITH NO ADVANCING
           ACCEPT WRK-OPCAO

           EVALUATE WRK-OPCAO
               WHEN 1 PERFORM 0003-CADASTRAR-TEL
               WHEN 2 PERFORM 0007-LISTAR-AGENDA 
               WHEN 0 CONTINUE
               WHEN OTHER
<<<<<<< HEAD
                    DISPLAY 'OPÇÃO INVÁLIDA!'
=======
                    DISPLAY 'OPÇÃO INVÁLIDA!' 
>>>>>>> cb85f5b (Adiciona estrutura do projeto COBOL Dicas)
           END-EVALUATE
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CADASTRO DE NOPME E TELEFONE - ARQUIVO SEQUENCIAL
      *----------------------------------------------------------------*
       0003-CADASTRAR-TEL              SECTION.
      *----------------------------------------------------------------*

           PERFORM 0004-CARREGAR-TELEFONE
 
           DISPLAY '*========== AGENDA TELEFONICA =============*'
           DISPLAY '*               CADASTRAR                  *'
           DISPLAY '*==========================================*'
           DISPLAY '* NOME: 'WITH NO ADVANCING
           ACCEPT WRK-REG-NOME

           IF WRK-REG-NOME EQUAL SPACES OR WRK-REG-NOME EQUAL 'FIM'
              PERFORM 9999-FINALIZAR
           ELSE
              DISPLAY '* TELEFONE: 'WITH NO ADVANCING
              ACCEPT WRK-REG-TELEFONE
              IF WRK-REG-TELEFONE EQUAL SPACES
                 DISPLAY '* NUMERO DO TELEFONE NAO INFORMADO'
                 PERFORM 9999-FINALIZAR
              ELSE
                 PERFORM 0005-PESQUISAR-TEL
                 PERFORM 0006-GRAVAR-AGENDA
              END-IF
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0003-end
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CARREGAR AGENDA TELEFONICA NA TABELA INTERNA PARA PESQUISA
      *----------------------------------------------------------------*
       0004-CARREGAR-TELEFONE          SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQUIVO-ENT

      *    LÊ O ARQUIVO E CARREGA NA TABELA INTERNA
           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                WRK-FIM-ARQ  EQUAL 'S' 
               PERFORM 0001-LER-ARQUIVO-ENT 
               IF WRK-FIM-ARQ          EQUAL 'N'
                  MOVE FDE-REG-NOME    TO TAB-NOME(WRK-IND1)
                  MOVE FDE-REG-TELEFONE
                                       TO TAB-TELEFONE(WRK-IND1)
                  ADD 1                TO WRK-QTDE-REG
               END-IF 
           END-PERFORM 

           IF WRK-QTDE-REG >= 10
              DISPLAY 'AGENDA CHEIA! - MAXIMO 10 REGISTROS'
              DISPLAY ' FOI ATINGIDO A QUANTIDADE MAXIMA DE REGISTROS'
              CLOSE ARQUIVO-ENT
              PERFORM 9999-FINALIZAR
           ELSE
              CLOSE ARQUIVO-ENT
           END-IF
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PESQUISAR CONTA ORIGEM
      *----------------------------------------------------------------*
       0005-PESQUISAR-TEL              SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ENCONTROU
           MOVE ZEROS                  TO WRK-IND1

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                   (WRK-IND1 > WRK-QTDE-REG) OR
                                   (WRK-ENCONTROU EQUAL 'S')
              IF WRK-REG-TELEFONE   EQUAL TAB-TELEFONE(WRK-IND1)
                 MOVE 'S'           TO WRK-ENCONTROU
                 DISPLAY 'NUMERO DO TELEFONE JA ESTA CADASTRADO'
                 PERFORM 9999-FINALIZAR
              ELSE 
                 CONTINUE 
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR ATUALIZAR AGENDA TELEFONICA
      *----------------------------------------------------------------*
       0006-GRAVAR-AGENDA              SECTION.
      *----------------------------------------------------------------*
 
           OPEN OUTPUT ARQUIVO-ENT

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                    WRK-IND1 > WRK-QTDE-REG
                  MOVE TAB-NOME(WRK-IND1)
                                       TO FDE-REG-NOME
                  MOVE TAB-TELEFONE(WRK-IND1)
                                       TO FDE-REG-TELEFONE
                  WRITE FD-ARQ-ENT
           END-PERFORM

           MOVE WRK-REG-NOME           TO FDE-REG-NOME
           MOVE WRK-REG-TELEFONE       TO FDE-REG-TELEFONE
           WRITE FD-ARQ-ENT

           CLOSE ARQUIVO-ENT
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-end
       0006-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    ROTINA RESPONSAVEL POR LISTAR AGENDA TELEFONICA
      *----------------------------------------------------------------*
       0007-LISTAR-AGENDA              SECTION.
      *----------------------------------------------------------------*
 
           OPEN INPUT ARQUIVO-ENT

           DISPLAY '*========== AGENDA TELEFONICA =============*'
           DISPLAY '*               LISTAR                     *'

      *    LÊ O ARQUIVO E CARREGA NA TABELA INTERNA
           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                WRK-FIM-ARQ  EQUAL 'S' 
               PERFORM 0001-LER-ARQUIVO-ENT 
               IF WRK-FIM-ARQ          EQUAL 'N'
               DISPLAY '*  NOME: ' FDE-REG-NOME                        '
      -    '      -  TELEFONE: ' FDE-REG-TELEFONE 
                  ADD 1                TO WRK-QTDE-REG
               ELSE
                  IF WRK-QTDE-REG = 0 
                     DISPLAY 'NÃO FORAM ENCONTRADOS REGISTROS'
                  ELSE
                     DISPLAY '-------------------------------'
                     DISPLAY 'QTDE. REGISTROS: ' WRK-QTDE-REG  
                     CONTINUE 
               END-IF 
           END-PERFORM 
           DISPLAY '*==========================================*'

           CLOSE ARQUIVO-ENT
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-end
       0007-END.                       EXIT.
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
