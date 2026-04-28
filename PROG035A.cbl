      ******************************************************************
      * PROGRAMADOR: JOSE ROBERTO - COBOL DICAS
      * DATA.......: 06/04/2026
      * DESCRICAO..: LISTAR CLIENTES - MODULO LEITURA DE ARQUIVO
      * NOME.......: PROG035A
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG035A. 
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
         05  FDE-REG-ID-CLIENTE           PIC 9(05) VALUE ZEROS.
         05  FDE-REG-NOME-CLIENTE         PIC X(30) VALUE SPACES.
         05  FDE-REG-IDADE                PIC 9(03) VALUE ZEROS.

       WORKING-STORAGE SECTION.
       01  WRK-FIM-ARQUIVO                PIC X(01) VALUE 'N'.
       01  WRK-IND1                       PIC 9(05) VALUE ZEROS.           
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

           PERFORM 0001-LER-ARQSEQ 

           PERFORM 0002-LISTAR-CLIENTE UNTIL WRK-FIM-ARQUIVO EQUAL 'S'

           IF WRK-FIM-ARQUIVO EQUAL 'S' AND
              WRK-IND1 EQUAL ZEROS
              DISPLAY  '     ARQUIVO VAZIO     '
           END-IF 

           PERFORM 9999-FINALIZAR  
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0000-end
       0000-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FAZER CADASTRO USUARIO
      *----------------------------------------------------------------*
       0001-LER-ARQSEQ                 SECTION.
      *----------------------------------------------------------------*
 
           READ ARQUIVO-ENT
               AT END MOVE "S" TO WRK-FIM-ARQUIVO 
           END-READ
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0001-end
       0001-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    LISTAR CLIENTES
      *----------------------------------------------------------------*
       0002-LISTAR-CLIENTE          SECTION.
      *----------------------------------------------------------------*

           MOVE FDE-REG-ID-CLIENTE     TO WRK-ID-CLIENTE  
           MOVE FDE-REG-NOME-CLIENTE   TO WRK-NOME-CLIENTE
           MOVE FDE-REG-IDADE          TO WRK-IDADE       
           ADD 1                       TO WRK-IND1
           PERFORM 0001-LER-ARQSEQ 
           .
      *----------------------------------------------------------------*
      *> cobol-lint CL002 0002-end
       0002-END.                       EXIT.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *    FINALIZAR PROGRAMA
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-finalizar
       9999-FINALIZAR                  SECTION.
      *----------------------------------------------------------------*

           CLOSE ARQUIVO-ENT

           DISPLAY 'FIM DE PROGRAMA'
           STOP RUN
           .           
      *----------------------------------------------------------------*
      *> cobol-lint CL002 9999-end
       9999-END.                       EXIT.
      *----------------------------------------------------------------*