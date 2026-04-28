      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 20/04/2026
      * DESCRICAO..: CONSULTA DE REGISTROS
      * NOME.......: PROG038A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG038A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT1 ASSIGN TO 'ALTCONS01.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQUIVO-ENT2 ASSIGN TO 'CADASTRO1.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQUIVO-SAI ASSIGN TO 'ALTRET01.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT1.
       01  FD-ARQ-ENT1.
         05  FDE1-REG-ID-CLIENTE          PIC 9(05) VALUE ZEROS.

       FD  ARQUIVO-ENT2.
       01  FD-ARQ-ENT2.
         05  FDE2-REG-ID-CLIENTE          PIC 9(05) VALUE ZEROS.
         05  FDE2-REG-NOME-CLIENTE        PIC X(30) VALUE SPACES.
         05  FDE2-REG-IDADE               PIC 9(03) VALUE ZEROS.

       FD  ARQUIVO-SAI.
       01  FD-ARQ-SAI.
         05  FDS-REG-ID-CLIENTE           PIC 9(05) VALUE ZEROS.
         05  FDS-REG-NOME-CLIENTE         PIC X(30) VALUE SPACES.
         05  FDS-REG-IDADE                PIC 9(03) VALUE ZEROS.

       WORKING-STORAGE SECTION.
       01 TABELA-CLIENTES.
           05 DADOS-CLIENTES     OCCURS 50 TIMES.
               10 TAB-ID-CLIENTE         PIC 9(05).
               10 TAB-NOME-CLIENTE       PIC X(30).
               10 TAB-IDADE              PIC 9(03).
       
       01  WRK-FIM-ARQUIVO1               PIC X(01) VALUE 'N'.
       01  WRK-FIM-ARQUIVO2               PIC X(01) VALUE 'N'.
       01  WRK-QTDE-REG                   PIC 9(05) VALUE ZEROS.           
       01  WRK-IND1                       PIC 9(05) VALUE ZEROS.           
       01  WRK-ENCONTRADO                 PIC X(01) VALUE 'N'.           
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

           OPEN INPUT ARQUIVO-ENT1

           PERFORM 0011-LER-ARQSEQ UNTIL WRK-FIM-ARQUIVO1 EQUAL 'S'

           CLOSE ARQUIVO-ENT1

           OPEN INPUT ARQUIVO-ENT2

           PERFORM 0001-LER-ARQSEQ UNTIL WRK-FIM-ARQUIVO2 EQUAL 'S'

           CLOSE ARQUIVO-ENT2

           PERFORM 0002-CONSULTAR-REGISTRO 

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
 
           READ ARQUIVO-ENT2
             AT END 
                  MOVE "S" TO WRK-FIM-ARQUIVO2 
             NOT AT END  
               MOVE FDE2-REG-ID-CLIENTE
                                    TO TAB-ID-CLIENTE(INDICE-LEITURA)   
               MOVE FDE2-REG-NOME-CLIENTE
                                    TO TAB-NOME-CLIENTE (INDICE-LEITURA)
               MOVE FDE2-REG-IDADE  TO TAB-IDADE        (INDICE-LEITURA)

               ADD 1                   TO INDICE-LEITURA
               ADD 1                   TO WRK-QTDE-REG
           END-READ
           .
      *----------------------------------------------------------------*
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LER ARQUIVO ENTRADA1
      *----------------------------------------------------------------*
       0011-LER-ARQSEQ                 SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENT1
             AT END 
                  MOVE "S" TO WRK-FIM-ARQUIVO1
             NOT AT END  
               MOVE FDE1-REG-ID-CLIENTE TO WRK-ID-CLIENTE
           END-READ
           .
      *----------------------------------------------------------------*
       0011-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CONSULTAR REGISTROS PARA ALTERAÇÃO 
      *----------------------------------------------------------------*
       0002-CONSULTAR-REGISTRO         SECTION.
      *----------------------------------------------------------------*

      *    BUSCAR CODIGO DO CLIENTE
           PERFORM VARYING WRK-IND1  FROM 1 BY 1 
                                    UNTIL WRK-IND1 > WRK-QTDE-REG
                                    OR WRK-ENCONTRADO EQUAL 'S'
              IF WRK-ID-CLIENTE     EQUAL TAB-ID-CLIENTE (WRK-IND1)
                 MOVE TAB-NOME-CLIENTE (WRK-IND1)
                                       TO WRK-NOME-CLIENTE
                 MOVE TAB-IDADE (WRK-IND1) 
                                       TO WRK-IDADE                                    
                 MOVE 'S'              TO WRK-ENCONTRADO 
               END-IF 
           END-PERFORM 

           SUBTRACT 1  FROM WRK-IND1
           .
      *----------------------------------------------------------------*
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    DESCARREGAR TABELA INTERNA E REGRAVAR ARQUIVO DE ENTRADA
      *----------------------------------------------------------------*
       0005-GRAVAR-ARQUIVO             SECTION.
      *----------------------------------------------------------------*

           OPEN OUTPUT ARQUIVO-SAI

             MOVE WRK-ID-CLIENTE       TO FDS-REG-ID-CLIENTE
             MOVE WRK-NOME-CLIENTE     TO FDS-REG-NOME-CLIENTE 
             MOVE WRK-IDADE            TO FDS-REG-IDADE 

             WRITE FD-ARQ-SAI

           CLOSE ARQUIVO-SAI
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