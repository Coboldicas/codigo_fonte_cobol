      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 27/04/2026
      * DESCRICAO..: EXCLUSÃO DE REGISTROS
      * NOME.......: PROG039A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG039A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT ASSIGN TO 'CADASTRO1.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT.
       01  FD-ARQ-ENT.
         05  FDE-REG-ID-CLIENTE          PIC 9(05) VALUE ZEROS.
         05  FDE-REG-NOME-CLIENTE        PIC X(30) VALUE SPACES.
         05  FDE-REG-IDADE               PIC 9(03) VALUE ZEROS.

       WORKING-STORAGE SECTION.
       01 TABELA-CLIENTES.
           05 DADOS-CLIENTES     OCCURS 50 TIMES.
               10 TAB-ID-CLIENTE          PIC 9(05).
               10 TAB-NOME-CLIENTE        PIC X(30).
               10 TAB-IDADE               PIC 9(03).
       
       01  WRK-FIM-ARQUIVO                PIC X(01) VALUE 'N'.
       01  WRK-QTDE-REG                   PIC 9(05) VALUE ZEROS.           
       01  WRK-IND1                       PIC 9(05) VALUE ZEROS.           
       01  WRK-EXCLUIDO                   PIC X(01) VALUE 'N'.           
       01  INDICE-LEITURA                 PIC 9(02) VALUE 1.

       01  WRK-ID-CLIENTE                 PIC 9(05) VALUE ZEROS.
       01  WRK-NOME-CLIENTE               PIC X(30) VALUE SPACES.
       01  WRK-IDADE                      PIC 9(03) VALUE ZEROS.

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

           PERFORM 0001-LER-ARQSEQ UNTIL WRK-FIM-ARQUIVO  EQUAL 'S'

           CLOSE ARQUIVO-ENT

           PERFORM 0002-EXCLUIR-REGISTRO 

           PERFORM 0005-GRAVAR-ARQUIVO 

           PERFORM 9999-FINALIZAR  
           .
      *----------------------------------------------------------------*
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FAZER CADASTRO USUARIO
      *----------------------------------------------------------------*
       0001-LER-ARQSEQ                 SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENT
             AT END 
                  MOVE "S" TO WRK-FIM-ARQUIVO 
             NOT AT END  
               MOVE FDE-REG-ID-CLIENTE
                                    TO TAB-ID-CLIENTE(INDICE-LEITURA)   
               MOVE FDE-REG-NOME-CLIENTE
                                    TO TAB-NOME-CLIENTE (INDICE-LEITURA)
               MOVE FDE-REG-IDADE   TO TAB-IDADE        (INDICE-LEITURA)

               ADD 1                   TO INDICE-LEITURA
               ADD 1                   TO WRK-QTDE-REG
           END-READ
           .
      *----------------------------------------------------------------*
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    INSERIR REGISTRO PARA EXCLUSÃO
      *----------------------------------------------------------------*
       0002-EXCLUIR-REGISTRO            SECTION.
      *----------------------------------------------------------------*
           
           DISPLAY 'FAVOR INFORMAR O ID DO CLIENTE: '
           WITH NO ADVANCING
           ACCEPT WRK-ID-CLIENTE
           .
      *----------------------------------------------------------------*
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    DESCARREGAR TABELA INTERNA E REGRAVAR ARQUIVO DE ENTRADA
      *----------------------------------------------------------------*
       0005-GRAVAR-ARQUIVO             SECTION.
      *----------------------------------------------------------------*

           OPEN OUTPUT ARQUIVO-ENT

           PERFORM VARYING WRK-IND1 FROM 1 BY 1
                                UNTIL WRK-IND1 > WRK-QTDE-REG

             IF WRK-ID-CLIENTE EQUAL TAB-ID-CLIENTE(WRK-IND1)
                MOVE 'S'               TO WRK-EXCLUIDO
             ELSE 
                MOVE TAB-ID-CLIENTE(WRK-IND1)
                                       TO FDE-REG-ID-CLIENTE
                MOVE TAB-NOME-CLIENTE(WRK-IND1)
                                       TO FDE-REG-NOME-CLIENTE 
                MOVE TAB-IDADE(WRK-IND1)  
                                       TO FDE-REG-IDADE 
                WRITE FD-ARQ-ENT
             END-IF

           END-PERFORM

           CLOSE ARQUIVO-ENT
           .
      *----------------------------------------------------------------*
       0005-END.                       EXIT.
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