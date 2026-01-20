      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 01/12/2025
      * DESCRICAO..: CONTROLE DE PONTO - SIMPLIFICADO
      * NOME.......: PROG028A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG028A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT ASSIGN TO 'FUNCIONARIO.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
          
           SELECT ARQUIVO-SAI ASSIGN TO 'PONTO.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT.
       01  FD-ARQ-ENT.
         05 FDE-COD-FUNCIONARIO            PIC 9(05).
         05 FDE-NOME-FUNCIONARIO           PIC X(30).

       FD  ARQUIVO-SAI.
       01  FD-LIN-PONTO.
         05 FDS-COD-FUNCIONARIO            PIC 9(05).
         05 FDS-NOME-FUNCIONARIO           PIC X(30).
         05 FDS-DATA-REGISTRO              PIC X(10).
         05 FDS-HORA-REGISTRO              PIC X(08).         
         05 FDS-TIPO-REGISTRO              PIC X(01).                  

       WORKING-STORAGE SECTION.
       01  WRK-FIM-ARQ                    PIC X(01) VALUE 'N'.      
       01  WRK-IND1                       PIC 9(02) VALUE ZEROS.
       01  WRK-IND2                       PIC 9(02) VALUE ZEROS.
       01  WRK-RETURN-CODE                PIC S9(4) COMP VALUE ZERO.
       01  WRK-QTDE-REG                   PIC 9(02) VALUE ZEROS.
       01  WRK-QTDE-REG-S                 PIC 9(02) VALUE ZEROS.
       01  WRK-ENCONTROU                  PIC X(01) VALUE 'N'.
       01  WRK-COD-FUNCIONARIO            PIC 9(05) VALUE ZEROS.
       01  WRK-COD-TIPO                   PIC X(01) VALUE SPACES.
       01  WRK-VAL-FUNCIONARIO            PIC X(01) VALUE 'N'.
       01  WRK-VAL-TIPO                   PIC X(01) VALUE 'N'. 
       01  WRK-TIPO-ANT                   PIC X(01) VALUE SPACES. 

       01  TAB-ENT-LINHA.
           05  TAB-ENT-LINHA OCCURS 50 TIMES.
               10  TAB-COD-FUNCIONARIO PIC 9(05)   VALUE ZEROS. 
               10  TAB-NOME-FUNCIONARIO
                                       PIC X(30)   VALUE SPACES.

       01  TAB-SAI-LINHA.
           05  TAB-SAI-LINHA OCCURS 100 TIMES.
               10  TABS-COD-FUNCIONARIO PIC 9(05)   VALUE ZEROS. 
               10  TABS-DATA-REGISTRO   PIC X(10)   VALUE SPACES.
               10  TABS-TIPO-REGISTRO   PIC X(01)   VALUE SPACES.                  

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

           PERFORM 0011-CARREGAR-TABELA-INT
           PERFORM 0012-CARREGAR-TABELA-SAI
           PERFORM 0002-OBTER-DATA 
           PERFORM 0003-OBTER-TIME
           PERFORM 0004-TELA-MENU
           OPEN EXTEND ARQUIVO-SAI
           PERFORM 0006-REGISTRAR-PONTO
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
                  MOVE FDE-COD-FUNCIONARIO
                                       TO TAB-COD-FUNCIONARIO(WRK-IND1)
                  MOVE FDE-NOME-FUNCIONARIO
                                       TO TAB-NOME-FUNCIONARIO(WRK-IND1)
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
      *    CARREGAR TABELA INTERNA - REGISTROS DE SAIDA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0012-CARREGAR-TABELA-SAI
       0012-CARREGAR-TABELA-SAI        SECTION.
      *----------------------------------------------------------------*

           OPEN INPUT ARQUIVO-SAI
           MOVE 'N' TO WRK-FIM-ARQ

      *    LÊ O ARQUIVO E CARREGA NA TABELA INTERNA
           PERFORM VARYING WRK-IND2 FROM 1 BY 1 UNTIL 
                                WRK-FIM-ARQ  EQUAL 'S' 
               PERFORM 0021-LER-ARQUIVO-SAI
               IF WRK-FIM-ARQ          EQUAL 'N'
                  MOVE FDS-COD-FUNCIONARIO
                                       TO TABS-COD-FUNCIONARIO(WRK-IND2)
                  MOVE FDS-DATA-REGISTRO
                                       TO TABS-DATA-REGISTRO(WRK-IND2)
                  MOVE FDS-TIPO-REGISTRO
                                       TO TABS-TIPO-REGISTRO(WRK-IND2)
                  ADD 1                TO WRK-QTDE-REG-S
               END-IF 
           END-PERFORM 

           IF WRK-QTDE-REG-S >= 100
              DISPLAY 'TABELA CHEIA! - MAXIMO 100 REGISTROS'
              DISPLAY ' FOI ATINGIDO A QUANTIDADE MAXIMA DE REGISTROS'
              PERFORM 9999-FINALIZAR
           END-IF

           CLOSE ARQUIVO-SAI
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0012-end
       0012-END.                       EXIT.
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
      *    LEITURA DE ARQUIVO DE SAIDA 
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0021-ler-arquivo-sai
       0021-LER-ARQUIVO-SAI            SECTION.
      *----------------------------------------------------------------*

           READ ARQUIVO-SAI INTO FD-LIN-PONTO
               AT END MOVE "S" TO WRK-FIM-ARQ 
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0021-end
       0021-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    OBTER HORA SISTEMA
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
      *    TELA MENU
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-tela-menu
       0004-TELA-MENU                  SECTION.
      *----------------------------------------------------------------*

           DISPLAY "==============================================="
           DISPLAY "   CONTROLE DE PONTO SIMPLIFICADO - ETAPA 1    "
           DISPLAY "        REGISTRO DE PONTO EM ARQUIVO           "
           DISPLAY "==============================================="
           PERFORM 0005-VALIDAR-FUNCIONARIO 
                                  UNTIL WRK-VAL-FUNCIONARIO EQUAL 'S'
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0004-end
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    VALIDACAO CADATRO DE FUNCIONARIO
      *----------------------------------------------------------------*
       0005-VALIDAR-FUNCIONARIO        SECTION.
      *----------------------------------------------------------------*

           DISPLAY "Informe o codigo do funcionario: " 
                    WITH NO ADVANCING
           ACCEPT WRK-COD-FUNCIONARIO

           IF WRK-COD-FUNCIONARIO      EQUAL ZEROS
              DISPLAY 'CODIGO DO FUNCIONARIO NAO PODE SER ZERADO'
           ELSE
              PERFORM 0051-CONSULTAR-FUNCIONARIO
              IF WRK-ENCONTROU         EQUAL 'S'
                 MOVE 'S'              TO WRK-VAL-FUNCIONARIO
              ELSE 
                 DISPLAY 'CODIGO DO FUNCIONARIO NAO CADASTRADO'
              END-IF
           END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0005-end
       0005-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PESQUISAR CODIGO FUNCIONARIO - TABELA INTERNA
      *----------------------------------------------------------------*
       0051-CONSULTAR-FUNCIONARIO     SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ENCONTROU
           MOVE ZEROS                  TO WRK-IND1

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 UNTIL 
                                   (WRK-IND1 > WRK-QTDE-REG) OR
                                   (WRK-ENCONTROU EQUAL 'S')
              IF WRK-COD-FUNCIONARIO
                                    EQUAL TAB-COD-FUNCIONARIO(WRK-IND1)
                 MOVE 'S'              TO WRK-ENCONTROU
                 SUBTRACT 1          FROM WRK-IND1
              ELSE 
                 CONTINUE 
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0051-end
       0051-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------* 
      *    REGISTRAR PONTO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-REGISTRAR-PONTO
       0006-REGISTRAR-PONTO            SECTION.
      *----------------------------------------------------------------*

           DISPLAY "Tipo de ponto (E=Entrada, S=Saida): " 
                    WITH NO ADVANCING
           ACCEPT WRK-COD-TIPO

           IF WRK-COD-TIPO  NOT EQUAL 'E' AND
              WRK-COD-TIPO  NOT EQUAL 'S' 
              DISPLAY "Tipo de ponto Invalido!" 
           ELSE
              PERFORM 0007-VALIDAR-TIPO
           END-IF            
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0006-end
       0006-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------* 
      *    VALIDAR TIPO DE REGISTRO
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-VALIDAR-TIPO
       0007-VALIDAR-TIPO               SECTION.
      *----------------------------------------------------------------*

              PERFORM 0071-CONSULTAR-TIPO

              IF WRK-COD-TIPO          EQUAL WRK-TIPO-ANT
                 DISPLAY 'REGISTRO NAO PERMITIDO!'
              ELSE 
                 IF WRK-TIPO-ANT       EQUAL SPACES
                    IF WRK-COD-TIPO    EQUAL 'S'
                       DISPLAY 'REGISTRO NAO PERMITIDO!!'
                    ELSE 
                       PERFORM 0008-GRAVAR-ARQUIVO                 
                 ELSE
                    PERFORM 0008-GRAVAR-ARQUIVO
                 END-IF 
              END-IF 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0007-end
       0007-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    PESQUISAR TIPO DE PONTO - TABELA INTERNA
      *----------------------------------------------------------------*
       0071-CONSULTAR-TIPO     SECTION.
      *----------------------------------------------------------------*

           MOVE 'N'                    TO WRK-ENCONTROU
           MOVE ZEROS                  TO WRK-IND2
           PERFORM VARYING WRK-IND2 FROM 1 BY 1 UNTIL 
                                   (WRK-IND2 > WRK-QTDE-REG-S) 
              IF COD001A-DATA       EQUAL
                                    TABS-DATA-REGISTRO(WRK-IND2)
                 IF WRK-COD-FUNCIONARIO 
                                    EQUAL
                                    TABS-COD-FUNCIONARIO(WRK-IND2)
                    MOVE TABS-TIPO-REGISTRO(WRK-IND2)
                                    TO WRK-TIPO-ANT
                 END-IF
              ELSE
                 CONTINUE
              END-IF 
           END-PERFORM
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0071-end
       0071-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    GRAVAR PONTO NO ARQUIVO SEQUENCIAL
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0008-gravar-arquivo
       0008-GRAVAR-ARQUIVO             SECTION.
      *----------------------------------------------------------------*

           MOVE WRK-COD-FUNCIONARIO    TO FDS-COD-FUNCIONARIO 
           MOVE TAB-NOME-FUNCIONARIO(WRK-IND1)
                                       TO FDS-NOME-FUNCIONARIO
           MOVE COD001A-DATA           TO FDS-DATA-REGISTRO   
           MOVE COD001A-TIME           TO FDS-HORA-REGISTRO   
           MOVE WRK-COD-TIPO           TO FDS-TIPO-REGISTRO   

           WRITE FD-LIN-PONTO
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0008-end
       0008-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-finalizar
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQUIVO-SAI
           
           DISPLAY '********************************'
           DISPLAY '*        FIM DE PROGRAMA       *'
           DISPLAY '********************************'
           STOP RUN 
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*

