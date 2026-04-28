      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 13/04/2026
      * DESCRICAO..: CLASSIFICAÇÃO DE REGISTROS
      * NOME.......: PROG036A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG036A. 
      *================================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.  

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQUIVO-ENT1 ASSIGN TO 'CLASSENT.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARQUIVO-ENT2 ASSIGN TO 'CADASTRO1.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQUIVO-ENT1.
       01  FD-ARQ-ENT1.
         05  FDE1-TIPO-CLASSIFICACAO       PIC 9(01) VALUE ZEROS.

       FD  ARQUIVO-ENT2.
       01  FD-ARQ-ENT2.
         05  FDE2-REG-ID-CLIENTE           PIC 9(05) VALUE ZEROS.
         05  FDE2-REG-NOME-CLIENTE         PIC X(30) VALUE SPACES.
         05  FDE2-REG-IDADE                PIC 9(03) VALUE ZEROS.

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
       01  WRK-IND2                       PIC 9(05) VALUE ZEROS.           
       01  WRK-CTPO-CASS                  PIC 9(01) VALUE ZEROS.
       01  WRK-DADOS-CLIENTES-AUX         PIC X(38).
       01  INDICE-LEITURA                 PIC 9(02) VALUE 1.

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

           EVALUATE WRK-CTPO-CASS 
            WHEN 1
                 PERFORM 0002-CLASS-REG-CODIGO
            WHEN 2
                 PERFORM 0003-CLASS-REG-NOME
            WHEN 3
                 PERFORM 0004-CLASS-REG-IDADE
            WHEN OTHER
                 DISPLAY 'OPCAO INCORRETA'
                 PERFORM 9999-FINALIZAR
           END-EVALUATE

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
      *    LER ARQUIVO ENTRADA 1
      *----------------------------------------------------------------*
       0011-LER-ARQSEQ                 SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENT1
             AT END 
                  MOVE "S" TO WRK-FIM-ARQUIVO1 
             NOT AT END  
                  MOVE FDE1-TIPO-CLASSIFICACAO
                                       TO WRK-CTPO-CASS
           END-READ
           .
      *----------------------------------------------------------------*
       0011-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CLASSIFICAÇÃO DE REGISTROS - CODIGO DO CLIENTE
      *----------------------------------------------------------------*
       0002-CLASS-REG-CODIGO           SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 
                                     UNTIL WRK-IND1 > WRK-QTDE-REG
               PERFORM VARYING WRK-IND2 FROM 1 BY 1 UNTIL 
                                    WRK-IND2 > WRK-QTDE-REG - WRK-IND1
                   IF TAB-ID-CLIENTE (WRK-IND2) 
                                        > TAB-ID-CLIENTE (WRK-IND2 + 1)
                       MOVE DADOS-CLIENTES (WRK-IND2)
                                       TO WRK-DADOS-CLIENTES-AUX
                       MOVE DADOS-CLIENTES (WRK-IND2 + 1)
                                       TO DADOS-CLIENTES(WRK-IND2)
                       MOVE WRK-DADOS-CLIENTES-AUX 
                                       TO DADOS-CLIENTES(WRK-IND2 + 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
      *----------------------------------------------------------------*
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CLASSIFICAÇÃO DE REGISTROS - NOME DO CLIENTE
      *----------------------------------------------------------------*
       0003-CLASS-REG-NOME            SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 
                                   UNTIL WRK-IND1 > WRK-QTDE-REG
               PERFORM VARYING WRK-IND2 FROM 1 BY 1 UNTIL 
                                    WRK-IND2 > WRK-QTDE-REG - WRK-IND1
                   IF TAB-NOME-CLIENTE (WRK-IND2) 
                                      > TAB-NOME-CLIENTE (WRK-IND2 + 1)
                       MOVE DADOS-CLIENTES (WRK-IND2)
                                       TO WRK-DADOS-CLIENTES-AUX
                       MOVE DADOS-CLIENTES (WRK-IND2 + 1)
                                       TO DADOS-CLIENTES(WRK-IND2)
                       MOVE WRK-DADOS-CLIENTES-AUX 
                                       TO DADOS-CLIENTES(WRK-IND2 + 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
      *----------------------------------------------------------------*
       0003-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    CLASSIFICAÇÃO DE REGISTROS - IDADE DO CLIENTE
      *----------------------------------------------------------------*
       0004-CLASS-REG-IDADE           SECTION.
      *----------------------------------------------------------------*

           PERFORM VARYING WRK-IND1 FROM 1 BY 1 
                                       UNTIL WRK-IND1 > WRK-QTDE-REG
               PERFORM VARYING WRK-IND2 FROM 1 BY 1 UNTIL 
                                    WRK-IND2 > WRK-QTDE-REG - WRK-IND1
                   IF TAB-IDADE (WRK-IND2) 
                                      > TAB-IDADE (WRK-IND2 + 1)
                       MOVE DADOS-CLIENTES (WRK-IND2)
                                       TO WRK-DADOS-CLIENTES-AUX
                       MOVE DADOS-CLIENTES (WRK-IND2 + 1)
                                       TO DADOS-CLIENTES(WRK-IND2)
                       MOVE WRK-DADOS-CLIENTES-AUX 
                                       TO DADOS-CLIENTES(WRK-IND2 + 1)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
      *----------------------------------------------------------------*
       0004-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    DESCARREGAR TABELA INTERNA E REGRAVAR ARQUIVO DE ENTRADA
      *----------------------------------------------------------------*
       0005-GRAVAR-ARQUIVO             SECTION.
      *----------------------------------------------------------------*

           OPEN OUTPUT ARQUIVO-ENT2

           PERFORM VARYING WRK-IND1 FROM 1 BY 1
                                UNTIL WRK-IND1 > WRK-QTDE-REG

             MOVE TAB-ID-CLIENTE(WRK-IND1)
                                       TO FDE2-REG-ID-CLIENTE
             MOVE TAB-NOME-CLIENTE(WRK-IND1)
                                       TO FDE2-REG-NOME-CLIENTE 
             MOVE TAB-IDADE(WRK-IND1)  TO FDE2-REG-IDADE 

             WRITE FD-ARQ-ENT2
           END-PERFORM

           CLOSE ARQUIVO-ENT2
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